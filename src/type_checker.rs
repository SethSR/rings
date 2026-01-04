
use std::collections::hash_map::Entry;

use crate::ast::{Block as AstBlock, Id as AstId, Kind};
use crate::discovery::{ProcMap, Region, RegionMap};
use crate::error;
use crate::identifier::{Id as IdentId, Identifier, Map as IdentMap};
use crate::operators::{BinaryOp, UnaryOp};
use crate::rings_type::{Meet, Type};
use crate::token::{self, Id as TokenId};
use crate::{text, token_source};
use crate::{ProcData, Span, SrcPos};

enum Error {
	InternalValue,
	AlreadyDefined(IdentId),
	MismatchedTypes(Type, Type),
	InvalidBinOp(BinaryOp, Type, Type),
	InvalidUnOp(UnaryOp, Type),
	TooManyLoopVariables,
	NegativeLoopRange,
	MissingLoopBounds,
	// Compiler Errors
	NoType { msg: &'static str, ast_id: AstId },
	MissingAstNode(IdentId, AstId),
}

impl Error {
	fn to_string(&self, source: &str, identifiers: &IdentMap<Span<SrcPos>>) -> String {
		match self {
			Self::InternalValue => {
				"Cannot define internal values, assign instead".to_string()
			}
			Self::AlreadyDefined(ident_id) => {
				format!("'{}' already defined", text(source, identifiers, ident_id))
			}
			Self::MismatchedTypes(expected, found) => {
				format!("Variable has type {expected}, but expression has type {found}")
			}
			Self::InvalidBinOp(op, lhs, rhs) => {
				format!("Unable to apply '{op}' to '{lhs}' and '{rhs}'")
			}
			Self::InvalidUnOp(op, rhs) => {
				format!("Unable to apply '{op}' to '{rhs}'")
			}
			Self::TooManyLoopVariables => {
				"simple for-loops require a single loop variable".to_string()
			}
			Self::NegativeLoopRange => {
				"start value must be less than or equal to end value".to_string()
			}
			Self::MissingLoopBounds => {
				"simple for-loops require a fully specified range '[start..end]'".to_string()
			}
			Self::NoType { msg, ast_id } => {
				format!("{msg} has no type: {ast_id:?}")
			}
			Self::MissingAstNode(proc_id, ast_id) => {
				format!("{} not found in procedure '{}'", ast_id, text(source, identifiers, &proc_id))
			}
		}
	}
}

pub fn eval(
	source: &str,
	identifiers: &IdentMap<Span<SrcPos>>,
	tok_list: &token::KindList,
	tok_pos: &token::PosList,
	procedures: &ProcMap,
	regions: &RegionMap,
	proc_db: &mut IdentMap<ProcData>,
) -> Result<(), error::Error> {
	// Check for 'main' procedure
	if !proc_db.contains_key(&"main".id()) {
		let tok_src = token_source(source, identifiers, tok_list, tok_pos, TokenId::default());
		let message = "missing 'main' procedure";
		let err = error::Error::new(tok_src, message);
		return Err(err.with_kind(error::Kind::Checker)
			//.display(&data.source_file, &data.source, &data.line_pos)
		);
	}

	// Check for stack regions
	let has_call_stack = regions.contains_key(&"CallStack".id());
	let has_data_stack = regions.contains_key(&"DataStack".id());
	if let Some(stack_src_loc) = regions.get(&"Stack".id()) {
		// call and data stack are combined, reject explicitly named regions
		if has_call_stack || has_data_stack {
			let tok_src = token_source(source, identifiers, tok_list, tok_pos, (stack_src_loc.span.start as usize).into());
			let message = "Combined Call/Data stack already defined";
			let err = error::Error::new(tok_src, message);
			return Err(err.with_kind(error::Kind::Checker)
				//.display(&data.source_file, &data.source, &data.line_pos)
			);
		}
	} else {
		// no combined stack region, call and data stack are required
		if !has_call_stack {
			let tok_src = token_source(source, identifiers, tok_list, tok_pos, TokenId::default());
			let message = "No combined Call/Data stack defined, declare a dedicated 'CallStack' region.";
			let err = error::Error::new(tok_src, message);
			return Err(err.with_kind(error::Kind::Checker)
				//.display(&data.source_file, &data.source, &data.line_pos)
			);
		} else if !has_data_stack {
			let tok_src = token_source(source, identifiers, tok_list, tok_pos, TokenId::default());
			let message = "No combined Call/Data stack defined, declare a dedicated 'DataStack' region.";
			let err = error::Error::new(tok_src, message);
			return Err(err.with_kind(error::Kind::Checker)
				//.display(&data.source_file, &data.source, &data.line_pos)
			);
		}
	}

	// Check for region overlap
	let regions_vec: Vec<(&IdentId, &Region)> = regions.iter().collect();

	for i in 0..regions_vec.len() {
		for j in i+1..regions_vec.len() {
			let (i_name, i_span) = regions_vec[i];
			let (j_name, j_span) = regions_vec[j];
			if !(i_span.span.start >= j_span.span.end || i_span.span.end <= j_span.span.start) {
				let i_src_loc = identifiers[i_name];
				let j_src_loc = identifiers[j_name];
				let i_text = text(source, identifiers, i_name);
				let j_text = text(source, identifiers, j_name);
				let msg = format!("regions {i_text} and {j_text} overlap");
				let i_msg = format!("{i_text} has a memory range of {i_span:?}");
				let j_msg = format!("{j_text} has a memory range of {j_span:?}");
				let err = error::Error::new(i_src_loc, msg)
					.with_note_at(i_src_loc, &i_msg)
					.with_note_at(j_src_loc, &j_msg)
					.with_kind(error::Kind::Checker);
					//.display(&data.source_file, &data.source, &data.line_pos);
				return Err(err);
			}
		}
	}

	// Check Record placement
	// TODO - srenshaw - Ensure records fit within their respective regions.

	// Check procedures
	let completed_procs = proc_db.clone();
	for (proc_id, mut proc_data) in completed_procs {
		let proc_type = &procedures[&proc_id];

		for (param_name, param_type) in &proc_type.params {
			proc_data.ident_to_type.insert(*param_name, *param_type);
		}

		let proc_start = proc_data.ast_start;
		let range = proc_data.ast_pos_tok[proc_start].clone();

		let ret_type = proc_type.ret_type;
		if let Err(err) = check_stmt(&mut proc_data, proc_id, proc_start, ret_type) {
			let tok_src = token_source(
				source, identifiers,
				tok_list, tok_pos,
				range.start);
			let message = err.to_string(source, identifiers);
			let err = error::Error::new(tok_src, message)
				.with_kind(error::Kind::Checker);
				//.display(&data.source_file, &data.source, &data.line_pos);
			return Err(err);
		}
		proc_db.entry(proc_id)
			.and_modify(|data| *data = proc_data);
	}

	Ok(())
}

fn check_stmt(proc_data: &mut ProcData,
	proc_id: IdentId, ast_id: AstId, ret_type: Type,
) -> Result<(), Error> {
	match proc_data.ast_nodes[ast_id].clone() {
		Kind::Int(_) => {
			proc_data.ast_to_type.insert(ast_id, Type::int());
			Ok(())
		}

		Kind::Dec(_) => {
			proc_data.ast_to_type.insert(ast_id, Type::Top);
			Ok(())
		}

		Kind::Ident(ident_id) => {
			check_ident(proc_data, &ident_id, ast_id);
			Ok(())
		}

		Kind::Define(lvalue_id, var_type) => {
			check_define(proc_data, lvalue_id, var_type)
		}

		Kind::Assign(lvalue_id, expr_id) => {
			if proc_data.ast_nodes.get(expr_id).is_none() {
				return Err(Error::MissingAstNode(proc_id, expr_id));
			}
			check_stmt(proc_data, proc_id, lvalue_id, ret_type)?;
			check_stmt(proc_data, proc_id, expr_id, ret_type)?;
			check_assign(proc_data, lvalue_id, expr_id)
		}

		Kind::BinOp(op, left, right) => {
			check_binop(proc_data, proc_id, ast_id, op, &left, &right, ret_type)
		}

		Kind::UnOp(op, right) => {
			check_unop(proc_data, ast_id, op, &right)
		}

		Kind::Return(expr_id) => {
			check_return(proc_data, proc_id, expr_id, ret_type)
		}

		Kind::Block(block) => {
			check_block(proc_data, proc_id, &block, ret_type)
		}

		Kind::If(cond_id, then_block, else_block) => {
			check_if(proc_data, proc_id, cond_id, &then_block, &else_block, ret_type)
		}

		Kind::While(cond_id, block) => {
			check_condition(proc_data, proc_id, cond_id, ret_type)?;
			check_block(proc_data, proc_id, &block, ret_type)
		}

		Kind::For(_vars, Some(_), _, _) => { todo!() }
		#[cfg(feature="forloop")]
		Kind::For(vars, Some(table_id), range, block) => {
			let table = &proc_data.tables[table_id];
			debug_assert!(vars.len() <= table.column_spec.len());

			let (start, end) = match range {
				Some(range) => (range.get_start(), range.get_end(table.row_count)),
				None => (0, table.row_count),
			};

			if start >= table.row_count {
				return Err("'start' value in bounds must be less than table row-count".to_string());
			}
			if end > table.row_count {
				return Err("'end' value in bounds must be less than or equal to table row-count".to_string());
			}
			if start > end {
				return Err("start value must be less than or equal to end value".to_string());
			}

			for i in 0..vars.len() {
				let var = &vars[i];
				if vars[i+1..].contains(var) {
					return Some(format!("duplicate field name '{}' in table", proc_data.text(var)));
				}

				if !table.column_spec.iter().any(|(a,_)| var == a) {
					return Err(format!("field '{}' not found in table '{}'", proc_data.text(var), data.text(table_id)));
				}
			}
			self.check_block(proc_data, block, ret_type)
		}

		Kind::For(vars, None, Some(range), block) => {
			if vars.len() != 1 {
				return Err(Error::TooManyLoopVariables);
			}
			match range {
				crate::Bounds::Full { start, end } => {
					debug_assert!(start <= end);
					if start > end {
						return Err(Error::NegativeLoopRange);
					}
				}
				crate::Bounds::From {..} | crate::Bounds::To {..} => {
					return Err(Error::MissingLoopBounds);
				}
			}
			check_block(proc_data, proc_id, &block, ret_type)
		}

		Kind::For(vars, None, None, block) => {
			todo!("infinite for-loop: {vars:?} {block:?}")
		}

		Kind::Call(proc_id, exprs) => {
			todo!("procedure-call: {proc_id} {exprs:?}")
		}

		#[cfg(feature="access")]
		Kind::Access(base_id, segments) => {
			let mut curr_id = base_id;
			for segment in segments {
				match segment {
					PathSegment::Field(field_id) => {
						let Some(record) = proc_data.records.get(curr_id) else {
							return Err(format!("no record named '{}' found", proc_data.text(curr_id)));
						};
						if !record.fields.iter().any(|(f_id,_)| field_id == f_id) {
							return Err(format!("no field '{}' in record '{}'",
								proc_data.text(&field_id), data.text(&curr_id),
							));
						}
						curr_id = field_id;
					}
					PathSegment::Index(expr_id, field_id) => {
						todo!("table-index-2: [{expr_id}].{field_id}")
					}
				}
			}
			Ok(())
		}
	}
}

fn check_ident(proc_data: &mut ProcData,
	ident_id: &IdentId, ast_id: AstId,
) {
	let new_type = proc_data.ident_to_type.get(ident_id)
		.unwrap_or(&Type::Top);
	proc_data.ast_to_type.insert(ast_id, *new_type);
}

fn check_define(proc_data: &mut ProcData,
	lvalue_id: AstId, var_type: Type,
) -> Result<(), Error> {
	let Kind::Ident(ident_id) = proc_data.ast_nodes[lvalue_id] else {
		return Err(Error::InternalValue);
	};

	let Entry::Vacant(e) = proc_data.ident_to_type.entry(ident_id) else {
		return Err(Error::AlreadyDefined(ident_id));
	};

	e.insert(var_type);
	proc_data.ast_to_type.insert(lvalue_id, var_type);
	Ok(())
}

fn check_assign(proc_data: &ProcData,
	lvalue_id: AstId, ast_id: AstId,
) -> Result<(), Error> {
	let ident_id = match proc_data.ast_nodes[lvalue_id] {
		Kind::Ident(id) => id,
		Kind::Define(def_id, _) => match proc_data.ast_nodes[def_id] {
			Kind::Ident(id) => id,
			_ => todo!("missing ident for lvalue: {lvalue_id:?}"),
		}
		_ => todo!("missing ident for lvalue: {lvalue_id:?}"),
	};
	let Some(lvalue_type) = proc_data.ident_to_type.get(&ident_id) else {
		return Err(Error::NoType { msg: "lvalue", ast_id: lvalue_id });
	};
	let Some(expr_type) = proc_data.ast_to_type.get(&ast_id) else {
		return Err(Error::NoType { msg: "assign expression", ast_id });
	};
	if expr_type.meet(lvalue_type) != *lvalue_type {
		Err(Error::MismatchedTypes(*lvalue_type, *expr_type))
	} else {
		// The types match, so we're okay.
		Ok(())
	}
}

fn check_binop(proc_data: &mut ProcData,
	proc_id: IdentId, ast_id: AstId,
	op: BinaryOp, left_id: &AstId, right_id: &AstId, proc_type: Type,
) -> Result<(), Error> {
	check_stmt(proc_data, proc_id, *left_id, proc_type)?;
	check_stmt(proc_data, proc_id, *right_id, proc_type)?;
	match op {
		BinaryOp::Add |
		BinaryOp::Sub |
		BinaryOp::Mul |
		BinaryOp::Div |
		BinaryOp::Mod |
		BinaryOp::ShL |
		BinaryOp::ShR |
		BinaryOp::BinAnd |
		BinaryOp::BinOr |
		BinaryOp::BinXor |
		BinaryOp::LogAnd |
		BinaryOp::LogOr |
		BinaryOp::LogXor |
		BinaryOp::CmpEQ |
		BinaryOp::CmpNE |
		BinaryOp::CmpGE |
		BinaryOp::CmpGT |
		BinaryOp::CmpLE |
		BinaryOp::CmpLT => {
			let left_type = proc_data.ast_to_type[left_id];
			let right_type = proc_data.ast_to_type[right_id];
			match left_type.meet(&right_type) {
				Type::Bot => Err(Error::InvalidBinOp(op, left_type, right_type)),
				new_type => {
					// Types are able to meet
					proc_data.ast_to_type.insert(ast_id, new_type);
					Ok(())
				}
			}
		}
	}
}

fn check_unop(proc_data: &mut ProcData,
	ast_id: AstId,
	op: UnaryOp, right: &AstId,
) -> Result<(), Error> {
	let right_type = proc_data.ast_to_type[right];
	if !matches!(right_type, Type::S8(_)) {
		return Err(Error::InvalidUnOp(op, right_type));
	};

	#[cfg(feature="types")]
	if !matches!(rtype, T::Bool | T::U8 | T::S8 | T::U16 | T::S16 | T::U32 | T::S32) {
		return Some(format!("TC - unable to apply '{op}' to type '{}'", right_type.display(proc_data)));
	}
	proc_data.ast_to_type.insert(ast_id, right_type);
	Ok(())
}

fn check_return(proc_data: &mut ProcData,
	proc_id: IdentId, ast_id: Option<AstId>, proc_type: Type,
) -> Result<(), Error> {
	let ret_type = match ast_id {
		Some(ast_id) => {
			check_stmt(proc_data, proc_id, ast_id, proc_type)?;
			match proc_data.ast_to_type.get(&ast_id) {
				Some(ret_type) => *ret_type,
				None => return Err(Error::NoType { msg: "return expression", ast_id }),
			}
		}
		None => Type::Unit,
	};

	let meet_type = ret_type.meet(&proc_type);
	if meet_type == Type::Bot {
		return Err(Error::MismatchedTypes(proc_type, ret_type));
	}
	Ok(())
}

fn check_block(proc_data: &mut ProcData,
	proc_id: IdentId, block: &AstBlock, proc_type: Type,
) -> Result<(), Error> {
	for stmt_id in &block.0 {
		check_stmt(proc_data, proc_id, *stmt_id, proc_type)?;
	}
	Ok(())
}

fn check_condition(proc_data: &mut ProcData,
	proc_id: IdentId, cond_id: AstId, proc_type: Type,
) -> Result<(), Error> {
	check_stmt(proc_data, proc_id, cond_id, proc_type)?;
	let cond_type = proc_data.ast_to_type[&cond_id];
	if cond_type.meet(&Type::s8_top()) == Type::Bot
	//&& cond_type.meet(Type::Rings(crate::Type::U32)) == Type::Bot
	{
		return Err(Error::MismatchedTypes(Type::Int, cond_type));
	}
	Ok(())
}

fn check_if(proc_data: &mut ProcData,
	proc_id: IdentId,
	cond_id: AstId, then_block: &AstBlock, else_block: &AstBlock,
	proc_type: Type,
) -> Result<(), Error> {
	check_condition(proc_data, proc_id, cond_id, proc_type)?;
	check_block(proc_data, proc_id, then_block, proc_type)?;
	check_block(proc_data, proc_id, else_block, proc_type)
}

