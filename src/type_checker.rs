
use std::collections::HashSet;

use crate::error;
use crate::identifier::{IdentId, Identifier, Map as IdentMap};
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::operators::{BinaryOp, UnaryOp};
use crate::parser::{Ast, AstId, AstKind, AstList, Data as PrsData, PathSegment, Type, Value};
use crate::token::Id as TokenId;
use crate::{token_source, SrcPos};

pub type TypedList = AstList<TypedKind, SrcPos>;

#[derive(Debug, Clone)]
pub struct TypedKind(AstKind, Type);

type TypedAst = Ast<TypedKind, SrcPos>;

impl TypedAst {
	fn new(value: Ast<AstKind, SrcPos>, typ: Type) -> Self {
		Self {
			kind: TypedKind(value.kind, typ),
			location: value.location,
		}
	}
}

enum Error {
	AlreadyDefined(IdentId),
	TypeMismatch(Type, Type),
	ParamCountMismatch {
		proc_id: IdentId,
		param_count: usize,
		arg_count: usize,
	},
	FieldCountMismatch {
		table_id: IdentId,
		field_count: usize,
		index_count: usize,
	},
	InvalidBinOp(BinaryOp, Type, Type),
	InvalidUnOp(UnaryOp, Type),
	TooManyLoopVariables,
	NegativeLoopRange,
	MissingLoopBounds,
	NonIdentifierField(AstKind),
	UnknownField { table_id: IdentId, field_id: IdentId },
	UnknownRegion(IdentId),
	UnknownMark { region_id: IdentId, mark_id: IdentId },
}

impl Error {
	fn to_string(&self, input: &InputData, lex_data: &LexData) -> String {
		match self {
			Self::AlreadyDefined(ident_id) => {
				format!("'{}' already defined", lex_data.text(input, ident_id))
			}
			Self::TypeMismatch(expected, found) => {
				format!("Variable has type {expected:?}, but expression has type {found:?}")
			}
			Self::ParamCountMismatch { proc_id, param_count, arg_count } => {
				format!("'{}' has {param_count} parameters, found {arg_count} arguments",
					lex_data.text(input, proc_id),
				)
			}
			Self::FieldCountMismatch { table_id, field_count, index_count } => {
				format!("'{}' has {field_count} fields, found {index_count} indexes",
					lex_data.text(input, table_id),
				)
			}
			Self::InvalidBinOp(op, lhs, rhs) => {
				format!("Unable to apply '{op}' to '{lhs:?}' and '{rhs:?}'")
			}
			Self::InvalidUnOp(op, rhs) => {
				format!("Unable to apply '{op}' to '{rhs:?}'")
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
			Self::NonIdentifierField(field_kind) => {
				format!("Expected an identifier, found '{field_kind}'")
			}
			Self::UnknownField { table_id, field_id } => {
				format!("Unknown field '{}' in table '{}'",
					lex_data.text(input, field_id),
					lex_data.text(input, table_id),
				)
			}
			Self::UnknownRegion(region_id) => {
				format!("Unknown region '{}'", lex_data.text(input, region_id))
			}
			Self::UnknownMark { region_id, mark_id } => {
				format!("Unknown mark '{}' for region '{}'",
					lex_data.text(input, mark_id),
					lex_data.text(input, region_id),
				)
			}
		}
	}
}

pub fn eval(
	input: &InputData,
	lex_data: &LexData,
	prs_data: &PrsData<SrcPos>,
) -> Result<IdentMap<TypedList>, error::Error> {
	// Check for 'main' procedure
	if !prs_data.procedures.contains_key(&"main".id()) {
		let tok_src = token_source(input, lex_data, TokenId::default());
		let message = "missing 'main' procedure";
		let err = error::Error::new(tok_src, message);
		return Err(err.with_kind(error::Kind::Checker));
	}

	// Check for stack regions
	let has_call_stack = prs_data.regions.contains_key(&"CallStack".id());
	let has_data_stack = prs_data.regions.contains_key(&"DataStack".id());
	if let Some(stack_src_loc) = prs_data.regions.get(&"Stack".id()) {
		// call and data stack are combined, reject explicitly named regions
		if has_call_stack || has_data_stack {
			let tok_src = token_source(input, lex_data, (stack_src_loc.span.start as usize).into());
			let message = "Combined Call/Data stack already defined";
			let err = error::Error::new(tok_src, message);
			return Err(err.with_kind(error::Kind::Checker));
		}
	} else {
		// no combined stack region, call and data stack are required
		if !has_call_stack {
			let tok_src = token_source(input, lex_data, TokenId::default());
			let message = "No combined Call/Data stack defined, declare a dedicated 'CallStack' region.";
			let err = error::Error::new(tok_src, message);
			return Err(err.with_kind(error::Kind::Checker));
		} else if !has_data_stack {
			let tok_src = token_source(input, lex_data, TokenId::default());
			let message = "No combined Call/Data stack defined, declare a dedicated 'DataStack' region.";
			let err = error::Error::new(tok_src, message);
			return Err(err.with_kind(error::Kind::Checker));
		}
	}

	// Check for region overlap
	let regions_vec: Vec<_> = prs_data.regions.iter().collect();

	for i in 0..regions_vec.len() {
		for j in i+1..regions_vec.len() {
			let (i_name, i_span) = regions_vec[i];
			let (j_name, j_span) = regions_vec[j];
			if !(i_span.span.start >= j_span.span.end || i_span.span.end <= j_span.span.start) {
				let i_src_loc = lex_data.location(i_name);
				let j_src_loc = lex_data.location(j_name);
				let i_text = lex_data.text(input, i_name);
				let j_text = lex_data.text(input, j_name);
				let msg = format!("regions {i_text} and {j_text} overlap");
				let i_msg = format!("{i_text} has a memory range of {i_span:?}");
				let j_msg = format!("{j_text} has a memory range of {j_span:?}");
				let err = error::Error::new(i_src_loc, msg)
					.with_note_at(i_src_loc, &i_msg)
					.with_note_at(j_src_loc, &j_msg)
					.with_kind(error::Kind::Checker);
				return Err(err);
			}
		}
	}

	// Check Record placement
	// TODO - srenshaw - Ensure records fit within their respective regions.

	// Check procedures
	let mut typed_procedures = IdentMap::with_capacity(prs_data.procedures.len());
	for (proc_id, proc_data) in &prs_data.procedures {
		let proc_start: AstId = 0.into();

		match check_proc(&prs_data, *proc_id) {
			Ok(new_ast) => {
				typed_procedures.insert(*proc_id, new_ast);
			}

			Err(err) => {
				let tok_src = proc_data.body[proc_start].location;
				let message = err.to_string(input, lex_data);
				let err = error::Error::new(tok_src, message)
						.with_kind(error::Kind::Checker);
				return Err(err);
			}
		}
	}

	Ok(typed_procedures)
}

fn meet_ast(
	lhs: &TypedAst,
	rhs: &TypedAst,
) -> Result<Type, Error> {
	meet(lhs.kind.1, rhs.kind.1)
}

fn meet(lhs: Type, rhs: Type) -> Result<Type, Error> {
	match (lhs, rhs) {
		(Type::Unknown, typ) |
		(typ, Type::Unknown) => Ok(typ),
		(Type::Int, Type::Dec) |
		(Type::Dec, Type::Int) => Ok(Type::Dec),
		(Type::Int, typ) |
		(typ, Type::Int) if typ.is_integer() => Ok(typ),
		(Type::U8, Type::S16) |
		(Type::S16, Type::U8) => Ok(Type::S16),
		(Type::U8, Type::S32) |
		(Type::S32, Type::U8) => Ok(Type::S32),
		(Type::U16, Type::S32) |
		(Type::S32, Type::U16) => Ok(Type::S32),
		(Type::S8, Type::S16) |
		(Type::S16, Type::S8) => Ok(Type::S16),
		(Type::S8, Type::S32) |
		(Type::S32, Type::S8) => Ok(Type::S32),
		(Type::S16, Type::S32) |
		(Type::S32, Type::S16) => Ok(Type::S32),
		(a, b) if a == b => Ok(a),
		_ => Err(Error::TypeMismatch(lhs, rhs)),
	}
}

fn check_proc(
	prs_data: &PrsData<SrcPos>,
	proc_id: IdentId,
) -> Result<AstList<TypedKind, SrcPos>, Error> {
	let mut out = AstList::default();
	let mut scope_depth = 0;
	let mut mark_set = HashSet::<IdentId>::default();

	let proc_data = &prs_data.procedures[&proc_id];

	for ast_id in 0..proc_data.body.len() {
		let ast = proc_data.body[ast_id].clone();
		match ast.kind {
			AstKind::Int(_) => {
				out.push(TypedAst::new(ast, Type::Int));
			}

			AstKind::Dec(_) => {
				out.push(TypedAst::new(ast, Type::Dec));
			}

			AstKind::Ident(ident_id) => {
				let ex_type = match prs_data.values.get(&ident_id) {
					Some(Value::Integer(_)) => Type::Int,
					Some(Value::Decimal(_)) => Type::Dec,
					None => *prs_data.types.get(&(proc_id, scope_depth, ident_id))
							.unwrap_or_else(|| panic!("missing type for {ident_id}: ({proc_id}, {scope_depth}, {ident_id})")),
				};
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::Assign { lhs, rhs } => {
				let lhs = &out[lhs];
				let rhs = &out[rhs];
				let ex_type = meet_ast(lhs, rhs)?;
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::BinOp { op, lhs, rhs } => {
				let lhs = &out[lhs];
				let rhs = &out[rhs];
				let typ = meet_ast(lhs, rhs)?;

				let valid_op = match op {
					BinaryOp::Add |
					BinaryOp::Div |
					BinaryOp::Mul |
					BinaryOp::Mod |
					BinaryOp::ShL |
					BinaryOp::ShR |
					BinaryOp::Sub => {
						// u8, u16, u32, s8, s16, s32, int, dec
						typ.is_integer() || typ.is_decimal()
					}

					BinaryOp::BinAnd |
					BinaryOp::BinOr |
					BinaryOp::BinXor |
					BinaryOp::CmpEQ |
					BinaryOp::CmpGE |
					BinaryOp::CmpGT |
					BinaryOp::CmpLE |
					BinaryOp::CmpLT |
					BinaryOp::CmpNE => {
						// u8, u16, u32, s8, s16, s32
						typ.is_integer()
					}

					BinaryOp::LogAnd |
					BinaryOp::LogOr |
					BinaryOp::LogXor => {
						typ == Type::Bool
					}
				};

				if !valid_op {
					return Err(Error::InvalidBinOp(op, lhs.kind.1, rhs.kind.1));
				}

				out.push(TypedAst::new(ast, typ));
			}

			AstKind::UnOp { op, rhs } => {
				let rhs_type = out[rhs].kind.1;

				let valid_op = match op {
					UnaryOp::Neg => {
						rhs_type.is_signed_integer()
					}
					UnaryOp::Not => {
						rhs_type.is_integer() || rhs_type == Type::Bool
					}
				};

				if !valid_op {
					return Err(Error::InvalidUnOp(op, rhs_type));
				}

				out.push(TypedAst::new(ast, rhs_type));
			}

			AstKind::Return(Some(ret)) => {
				let ret = &out[ret];
				let ex_type = meet(ret.kind.1, proc_data.ret_type)?;
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::Return(None) => {
				let ex_type = meet(Type::Void, proc_data.ret_type)?;
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::ScopeBegin => {
				scope_depth += 1;
				// NOTE - srenshaw - We convert scope nodes to keep index references correct.
				out.push(TypedAst::new(ast, Type::Void));
			}

			AstKind::ScopeEnd => {
				scope_depth -= 1;
				// NOTE - srenshaw - We convert scope nodes to keep index references correct.
				out.push(TypedAst::new(ast, Type::Void));
			}

			AstKind::Block(ref block) => {
				let ex_type = block.last()
						.map(|&id| out[id].kind.1)
						.unwrap_or(Type::Void);
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::If { cond, ref then_block, ref else_block } => {
				let cond_type = out[cond].kind.1;
				if !cond_type.is_integer() {
					return Err(Error::TypeMismatch(Type::Int, cond_type));
				}

				let then_type = then_block.last()
						.map(|&id| out[id].kind.1)
						.unwrap_or(Type::Void);
				let else_type = else_block.last()
						.map(|&id| out[id].kind.1)
						.unwrap_or(Type::Void);
				let ex_type = meet(then_type, else_type)?;

				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::While { cond, ..} => {
				let cond_type = out[cond].kind.1;
				if !cond_type.is_integer() {
					return Err(Error::TypeMismatch(Type::Int, cond_type));
				}
				out.push(TypedAst::new(ast, Type::Void));
			}

			AstKind::For { ref indexes, table, range_start, range_end, ..} => {
				fn get_bounds_type(list: &TypedList, ast_id_opt: Option<AstId>) -> Result<Type, Error> {
					let Some(ast_id) = ast_id_opt else {
						return Err(Error::MissingLoopBounds);
					};
					let ast_type = list[ast_id].kind.1;

					// TODO - srenshaw - Check for constant values

					Ok(ast_type)
				}

				if let Some((table_id, table)) = table
						.map(|id| (id, &prs_data.tables[&id]))
				{
					if table.fields.len() < indexes.len() {
						return Err(Error::FieldCountMismatch {
							table_id,
							field_count: table.fields.len(),
							index_count: indexes.len(),
						});
					}

					for idx_id in indexes.iter() {
						let idx_ast = &out[*idx_id].kind;
						let AstKind::Ident(idx_ident) = idx_ast.0 else {
							return Err(Error::NonIdentifierField(idx_ast.0.clone()));
						};

						// TODO - srenshaw - We may want to add destructuring for Records eventually.

						let Type::Unknown = idx_ast.1 else {
							return Err(Error::TypeMismatch(Type::Unknown, idx_ast.1));
						};

						let Some((_, f_type)) = table.fields.iter()
								.find(|field| field.0 == idx_ident) else {
							return Err(Error::UnknownField { table_id, field_id: idx_ident });
						};

						out[*idx_id].kind.1 = *f_type;
					}

					// TODO - srenshaw - Handle Table special cases

					let start_type = get_bounds_type(&out, range_start)?;
					let end_type = get_bounds_type(&out, range_end)?;

					let bound_type = meet(start_type, end_type)?;
					if !bound_type.is_integer() {
						return Err(Error::TypeMismatch(Type::Int, bound_type));
					}
				} else {
					if indexes.len() > 1 {
						return Err(Error::TooManyLoopVariables);
					}

					// TODO - srenshaw - Handle non-table special cases

					let start_type = get_bounds_type(&out, range_start)?;
					let end_type = get_bounds_type(&out, range_end)?;

					let bound_type = meet(start_type, end_type)?;
					let index_type = out[indexes[0]].kind.1;

					let loop_type = meet(index_type, bound_type)?;
					if !loop_type.is_integer() {
						return Err(Error::TypeMismatch(Type::Int, loop_type));
					}
				}

				out.push(TypedAst::new(ast, Type::Void));
			}

			#[cfg(feature = "forloop")]
			AstKind::For(vars, Some(table_id), range, block) => {
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
					if vars[i + 1..].contains(var) {
						return Some(format!("duplicate field name '{}' in table", proc_data.text(var)));
					}

					if !table.column_spec.iter().any(|(a, _)| var == a) {
						return Err(format!("field '{}' not found in table '{}'", proc_data.text(var), data.text(table_id)));
					}
				}
				self.check_block(proc_data, block, ret_type)
			}

			AstKind::Call { proc_id, ref block } => {
				let proc_data = &prs_data.procedures[&proc_id];

				if proc_data.params.len() != block.len() {
					return Err(Error::ParamCountMismatch {
						proc_id,
						param_count: proc_data.params.len(),
						arg_count: block.len(),
					});
				}

				for ((_, p_type), arg_id) in proc_data.params.iter().zip(block.iter()) {
					let arg_type = out[*arg_id].kind.1;
					meet(*p_type, arg_type)?;
				}

				out.push(TypedAst::new(ast, proc_data.ret_type));
			}

			AstKind::Access { base_id, ref path } => {
				let mut curr_id = base_id;
				let mut typ = None;

				for segment in path {
					match segment {
						PathSegment::Field(field_id) => {
							let Some(record) = prs_data.records.get(&curr_id) else {
								panic!("no record named '{curr_id}' found")
							};
							let Some((_, field_type)) = record.fields.iter().find(|(f_id, _)| field_id == f_id) else {
								panic!("no field '{field_id}' in record '{curr_id}'")
							};
							curr_id = *field_id;
							typ = Some(*field_type);
						}
						PathSegment::Index(expr_id, field_id) => {
							todo!("table-index-2: [{expr_id}].{field_id}")
						}
					}
				}

				if typ.is_none() {
					panic!("no type for access {ast:?}")
				}

				out.push(TypedAst::new(ast, typ.unwrap()));
			}

			AstKind::Mark { region_id, mark_id } => {
				if !prs_data.regions.contains_key(&region_id) {
					return Err(Error::UnknownRegion(region_id));
				};

				if mark_set.contains(&mark_id) {
					return Err(Error::AlreadyDefined(mark_id));
				}
				mark_set.insert(mark_id);

				out.push(TypedAst::new(ast, Type::Void));
			}

			AstKind::Free { region_id, mark_id } => {
				if !prs_data.regions.contains_key(&region_id) {
					return Err(Error::UnknownRegion(region_id));
				}

				if let Some(mark_id) = mark_id {
					if !mark_set.contains(&mark_id) {
						return Err(Error::UnknownMark { region_id, mark_id });
					}
					mark_set.remove(&mark_id);
				}

				out.push(TypedAst::new(ast, Type::Void));
			}

			AstKind::Use { region_id, ident } => {
				todo!("Checking overlay activation: {region_id} {ident}")
			}
		}
	}

	Ok(out)
}

// TODO - srenshaw - Add type-checker tests
