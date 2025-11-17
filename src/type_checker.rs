
use std::collections::hash_map::Entry;

use crate::ast::{
	Block as AstBlock,
	Id as AstId,
	Kind,
	PathSegment,
};
use crate::identifier;
use crate::error;
use crate::rings_type::Meet;
use crate::{Data, Type};

// NOTE - srenshaw - CE <== Compiler Error: This shouldn't happen and needs to be fixed

// NOTE - srenshaw - TC <== Type-Checker Error: Turn this into its own error category

pub fn eval(data: &mut Data) {
	let completed_procs = data.completed_procs.clone();
	for (proc_id, proc_start) in completed_procs {
		let proc_type = &data.procedures[&proc_id];

		for (param_name, param_type) in &proc_type.params {
			data.ident_to_type.insert(*param_name, *param_type);
		}

		let node = data.ast_nodes[proc_start].clone();
		let range = data.ast_pos_tok[proc_start].clone();

		//eprintln!("{} AST:", data.text(&proc_id));
		//for ast in &data.ast_nodes {
		//	eprintln!("  {ast:?}");
		//}

		let ret_type = proc_type.ret_type;
		if let Err(err_msg) = check_stmt(data, &node, proc_start, ret_type) {
			let mut err = error::error(data, &err_msg, range.start);
			err.set_kind(error::Kind::Checker);
			data.errors.push(err);
			return;
		}

		// Clear helper maps for next run
		data.ast_to_type.clear();
		data.ident_to_type.clear();
	}
}

fn check_stmt(data: &mut Data,
	node: &Kind, ast_id: AstId, ret_type: Type,
) -> Result<(), String> {
	match node {
		Kind::Int(num) => {
			//println!("  Int({num})");
			data.ast_to_type.insert(ast_id, Type::Top);
			Ok(())
		}

		Kind::Dec(num) => {
			//println!("  Dec({num})");
			data.ast_to_type.insert(ast_id, Type::Top);
			Ok(())
		}

		Kind::Ident(ident_id) => {
			//println!("  Ident({})", data.text(ident_id));
			check_ident(data, ident_id, ast_id);
			Ok(())
		}

		Kind::Define(lvalue_id, var_type, expr_id) => {
			//println!("  Define({} : {var_type:?} = {})", lvalue_id.index(), ast_id.index());
			let Some(expr_kind) = data.ast_nodes.get(*expr_id) else {
				todo!("expression with no ast node: {expr_id:?}")
			};
			check_stmt(data, &expr_kind.clone(), *expr_id, ret_type)?;
			check_define(data, *lvalue_id, *expr_id, *var_type)
		}

		Kind::Assign(lvalue_id, expr_id) => {
			//println!("  Assign({} = {})", lvalue_id.index(), ast_id.index());
			let Some(expr_kind) = data.ast_nodes.get(*expr_id) else {
				todo!("expression with no ast node: {expr_id:?}")
			};
			check_stmt(data, &expr_kind.clone(), *expr_id, ret_type)?;
			check_assign(data, *lvalue_id, *expr_id)
		}

		Kind::BinOp(op, left, right) => {
			//println!("  BinOp({} {op} {})", left.index(), right.index());
			check_binop(data, ast_id, *op, left, right, ret_type)
		}

		Kind::UnOp(op, right) => {
			//println!("  UnOp({op}{})", right.index());
			check_unop(data, ast_id, *op, right)
		}

		Kind::Return(expr_id) => {
			//println!("  Return({})", expr_id
			//	.map(|id| id.index().to_string())
			//	.unwrap_or("-".to_string()));
			check_return(data, *expr_id, ret_type)
		}

		Kind::Block(block) => {
			//println!("  Block({} nodes)", block.0.len());
			check_block(data, block, ret_type)
		}

		Kind::If(cond_id, then_block, else_block) => {
			//println!("  If({} -> {} nodes <> {} nodes)", cond_id.index(), then_block.0.len(), else_block.0.len());
			check_if(data, *cond_id, then_block, else_block, ret_type)
		}

		Kind::While(cond_id, block) => {
			//println!("  While({} -> {} nodes)", cond_id.index(), block.0.len());
			check_condition(data, *cond_id, ret_type)?;
			check_block(data, block, ret_type)
		}

		Kind::For(_, Some(_), _, _) => { todo!() }
		#[cfg(feature="ready")]
		Kind::For(vars, Some(table_id), range, block) => {
			//println!("  For({} in {}{} -> {} nodes)",
			//	vars.iter()
			//		.map(|var| data.text(var))
			//		.collect::<Vec<_>>()
			//		.join(","),
			//	data.text(table_id),
			//	range.map(|r| format!("[{r}]")).unwrap_or(String::new()),
			//	block.0.len());

			let table = &data.tables[table_id];
			debug_assert!(vars.len() <= table.column_spec.len());

			let (start, end) = match range {
				Some(range) => (range.get_start(), range.get_end(table.row_count)),
				None => (0, table.row_count),
			};

			if start >= table.row_count {
				return Some("'start' value in bounds must be less than table row-count".to_string());
			}
			if end > table.row_count {
				return Some("'end' value in bounds must be less than or equal to table row-count".to_string());
			}
			if start > end {
				return Some("start value must be less than or equal to end value".to_string());
			}

			for i in 0..vars.len() {
				let var = &vars[i];
				if vars[i+1..].contains(var) {
					return Some(format!("duplicate field name '{}' in table", data.text(var)));
				}

				if !table.column_spec.iter().any(|(a,_)| var == a) {
					return Some(format!("field '{}' not found in table '{}'", data.text(var), data.text(table_id)));
				}
			}
			self.check_block(data, block, ret_type)
		}

		Kind::For(vars, None, Some(range), block) => {
			//println!("  For({} in {} -> {} nodes)",
			//	vars.iter().map(|var| data.text(var)).collect::<Vec<_>>().join(","),
			//	range, block.0.len());
			if vars.len() != 1 {
				return Err("simple for-loops require a single loop variable".to_string());
			}
			match range {
				crate::Bounds::Full { start, end } => {
					debug_assert!(start <= end);
					if start > end {
						return Err("start value must be less than or equal to end value".to_string());
					}
				}
				crate::Bounds::From {..} | crate::Bounds::To {..} => {
					return Err("simple for-loops require a fully specified range (start..end)".to_string());
				}
			}
			check_block(data, block, ret_type)
		}

		Kind::For(vars, None, None, block) => {
			//println!("  For({} -> {} nodes)",
			//	vars.iter().map(|var| data.text(var)).collect::<Vec<_>>().join(","),
			//	block.0.len());
			todo!("infinite for-loop")
		}

		Kind::Call(proc_id, exprs) => {
			//println!("  Call({}({}))", data.text(proc_id), exprs.iter()
			//	.map(|id| id.to_string())
			//	.collect::<Vec<_>>()
			//	.join(","));
			todo!("procedure-call")
		}

		#[cfg(feature="ready")]
		Kind::Access(base_id, segments) => {
			//println!("  Access({}{})", data.text(base_id), segments.iter()
			//	.map(|segment| match segment {
			//		PathSegment::Field(field_id) => format!(".{}", data.text(field_id)),
			//		PathSegment::Index(expr_id, field_id) => format!("[{expr_id}].{}", data.text(field_id)),
			//	})
			//	.collect::<Vec<_>>()
			//	.join(""));

			let mut curr_id = base_id;
			for segment in segments {
				match segment {
					PathSegment::Field(field_id) => {
						let Some(record) = data.records.get(curr_id) else {
							return Some(format!("no record named '{}' found", data.text(curr_id)));
						};
						if !record.fields.iter().any(|(f_id,_)| field_id == f_id) {
							return Some(format!("no field '{}' in record '{}'",
								data.text(field_id), data.text(curr_id),
							));
						}
						curr_id = field_id;
					}
					PathSegment::Index(expr_id, field_id) => {
						todo!("table-index-2: [{expr_id}].{field_id}")
					}
				}
			}
			None
		}
	}
}

fn check_ident(data: &mut Data,
	ident_id: &identifier::Id, ast_id: AstId,
) {
	let new_type = data.ident_to_type.get(ident_id)
		.unwrap_or(&Type::Top);
	data.ast_to_type.insert(ast_id, *new_type);
}

fn check_define(data: &mut Data,
	lvalue_id: AstId, expr_id: AstId,
	var_type: Type,
) -> Result<(), String> {
	let Kind::Ident(ident_id) = data.ast_nodes[lvalue_id] else {
		return Err("TC - Cannot define internal values, assign instead".to_string());
	};

	match data.ident_to_type.entry(ident_id) {
		Entry::Occupied(_) => {
			Err(format!("TC - '{}' has already been defined", data.text(&ident_id)))
		}
		Entry::Vacant(e) => {
			let Some(expr_type) = data.ast_to_type.get(&expr_id) else {
					return Err(format!("CE - define expression has no type: {expr_id:?}"));
			};
			match expr_type.meet(&var_type) {
				Type::Bot => {
					Err(format!("TC - variable has type '{}', but the expression has type '{}'",
							var_type,
							expr_type,
						))
				}
				new_type => {
					data.ast_to_type.insert(lvalue_id, new_type);
					e.insert(new_type);
					Ok(())
				}
			}
		}
	}
}

fn check_assign(data: &Data,
	lvalue_id: AstId, ast_id: AstId,
) -> Result<(), String> {
	let Kind::Ident(ident_id) = data.ast_nodes[lvalue_id] else {
		todo!("missing ident for lvalue: {lvalue_id:?}")
	};
	let Some(lvalue_type) = data.ident_to_type.get(&ident_id) else {
		return Err(format!("CE - lvalue has no type: {lvalue_id:?}"));
	};
	let Some(expr_type) = data.ast_to_type.get(&ast_id) else {
		return Err(format!("CE - assign expression has no type: {ast_id:?}"));
	};
	if expr_type.meet(lvalue_type) != *lvalue_type {
		Err(format!("TC - variable has type '{}', but expression has type '{}'",
			lvalue_type,
			expr_type,
		))
	} else {
		// The types match, so we're okay.
		Ok(())
	}
}

fn check_binop(data: &mut Data,
	ast_id: AstId,
	op: crate::BinaryOp, left_id: &AstId, right_id: &AstId, proc_type: Type,
) -> Result<(), String> {
	let left = data.ast_nodes[*left_id].clone();
	check_stmt(data, &left, *left_id, proc_type)?;
	let right = data.ast_nodes[*right_id].clone();
	check_stmt(data, &right, *right_id, proc_type)?;
	match op {
		crate::BinaryOp::Add |
		crate::BinaryOp::Sub |
		crate::BinaryOp::Mul |
		crate::BinaryOp::Div |
		crate::BinaryOp::Mod |
		crate::BinaryOp::ShL |
		crate::BinaryOp::ShR |
		crate::BinaryOp::BinAnd |
		crate::BinaryOp::BinOr |
		crate::BinaryOp::BinXor |
		crate::BinaryOp::LogAnd |
		crate::BinaryOp::LogOr |
		crate::BinaryOp::LogXor |
		crate::BinaryOp::CmpEQ |
		crate::BinaryOp::CmpNE |
		crate::BinaryOp::CmpGE |
		crate::BinaryOp::CmpGT |
		crate::BinaryOp::CmpLE |
		crate::BinaryOp::CmpLT => {
			let left_type = data.ast_to_type[left_id];
			let right_type = data.ast_to_type[right_id];
			match left_type.meet(&right_type) {
				Type::Bot => Err(format!("TC - unable to apply '{op}' to types '{}' and '{}'",
					left_type,
					right_type,
				)),
				new_type => {
					// Types are able to meet
					data.ast_to_type.insert(ast_id, new_type);
					Ok(())
				}
			}
		}
		_ => todo!("add the rest of the binary operators"),
	}
}

fn check_unop(data: &mut Data,
	ast_id: AstId,
	op: crate::UnaryOp, right: &AstId,
) -> Result<(), String> {
	let right_type = data.ast_to_type[right];
	if !matches!(right_type, Type::S8(_)) {
		return Err(format!("TC - unable to apply '{op}' to type '{}'", right_type));
	};

	#[cfg(feature="ready")]
	if !matches!(rtype, T::Bool | T::U8 | T::S8 | T::U16 | T::S16 | T::U32 | T::S32) {
		return Some(format!("TC - unable to apply '{op}' to type '{}'", right_type.display(data)));
	}
	data.ast_to_type.insert(ast_id, right_type);
	Ok(())
}

fn check_return(data: &mut Data,
	ast_id: Option<AstId>, proc_type: Type,
) -> Result<(), String> {
	let ret_type = match ast_id {
		Some(ast_id) => {
			let kind = data.ast_nodes[ast_id].clone();
			check_stmt(data, &kind, ast_id, proc_type)?;
			match data.ast_to_type.get(&ast_id) {
				Some(ret_type) => *ret_type,
				None => return Err(format!("CE - return expression has no type: {ast_id:?}")),
			}
		}
		None => Type::Unit,
	};

	let meet_type = ret_type.meet(&proc_type);
	if meet_type == Type::Bot {
		return Err(format!("TC - Expected return type {proc_type}, found {ret_type}"));
	}
	Ok(())
}

fn check_block(data: &mut Data,
	block: &AstBlock, proc_type: Type,
) -> Result<(), String> {
	for stmt_id in &block.0 {
		let stmt = data.ast_nodes[*stmt_id].clone();
		check_stmt(data, &stmt, *stmt_id, proc_type)?;
	}
	Ok(())
}

fn check_condition(data: &mut Data,
	cond_id: AstId, proc_type: Type,
) -> Result<(), String> {
	let cond = data.ast_nodes[cond_id].clone();
	check_stmt(data, &cond, cond_id, proc_type)?;
	let cond_type = data.ast_to_type[&cond_id];
	if cond_type.meet(&Type::s8_top()) == Type::Bot
	//&& cond_type.meet(Type::Rings(crate::Type::U32)) == Type::Bot
	{
		todo!("TC - cond node must have integer type");
	}
	Ok(())
}

fn check_if(data: &mut Data,
	cond_id: AstId, then_block: &AstBlock, else_block: &AstBlock, proc_type: Type,
) -> Result<(), String> {
	check_condition(data, cond_id, proc_type)?;
	check_block(data, then_block, proc_type)?;
	check_block(data, else_block, proc_type)
}

