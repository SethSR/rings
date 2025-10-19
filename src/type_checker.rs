
use std::collections::HashMap;

use crate::ast;
use crate::error;
use crate::Data;

use ast::Kind;

pub fn eval(data: &mut Data) {
	for (proc_id, proc_range) in &data.completed_procs {
		let proc_type = &data.procedures[proc_id];
		let proc_start = data.proc_start[proc_id];
		let mut type_info = HashMap::new();
		for (param_name, param_type) in &proc_type.params {
			type_info.insert(*param_name, *param_type);
		}
		for (ast_offset, node) in data.ast_nodes[proc_range.clone()]
			.iter()
			.enumerate()
		{
			print!("[{ast_offset:3}] ");
			match node {
				Kind::Ident(ident_id) => {
					println!("AST-Ident({})", data.text(*ident_id));
				}
				Kind::Int(num) => {
					println!("AST-Int({num})");
				}
				Kind::Dec(num) => {
					println!("AST-Dec({num})");
				}
				Kind::Define(ident_id, var_type, ast_id) => {
					println!("AST-Define({} : {var_type:?} = {})", data.text(*ident_id), ast_id.index());
				}
				Kind::Assign(ident_id, ast_id) => {
					println!("AST-Assign({} = {})", data.text(*ident_id), ast_id.index());
				}
				Kind::BinOp(op, left, right) => {
					println!("AST-BinOp({} {op} {})", left.index(), right.index());
				}
				Kind::UnOp(op, right) => {
					println!("AST-UnOp({op}{})", right.index());
				}
				Kind::Return(Some(ast_id)) => {
					println!("AST-Return({})", ast_id.index());

					// let kind = &data.ast_nodes[*ast_id];
					// let ret_type = synthesize(data, kind);
					// if proc_type.ret_type != ret_type {
					// 	error::error(data,
					// 		&format!("TC - Expected return type {:?}, found '{ret_type:?}'",
					// 			proc_type.ret_type,
					// 		),
					// 		proc_start + ast_offset,
					// 	);
					// 	return;
					// }
				}
				Kind::Return(None) => {
					println!("AST-Return(-)");

					// if proc_type.ret_type != crate::Type::Unit {
					// 	error::error(data,
					// 		&format!("TC - no return value for procedure with '{:?}' return type",
					// 			proc_type.ret_type),
					// 		proc_start + ast_offset,
					// 	);
					// 	return;
					// }
				}
				Kind::If(cond, then_block, else_block) => {
					println!("AST-If({} -> {} <> {})", cond.index(), then_block.0.len(), else_block.0.len());
				}
				Kind::While(cond, block) => {
					println!("AST-While({} -> {})", cond.index(), block.0.len());
				}
				Kind::For(vars, Some(table_id), Some(range), block) => {
					println!("AST-For({vars:?} in {}[{}] -> {})", data.text(*table_id), range, block.0.len());
				}
				Kind::For(vars, Some(table_id), None, block) => {
					println!("AST-For({vars:?} in {} -> {})", data.text(*table_id), block.0.len());
				}
				Kind::For(vars, None, Some(range), block) => {
					println!("AST-For({vars:?} in [{}] -> {})", range, block.0.len());
				}
				Kind::For(vars, None, None, block) => {
					println!("AST-For({vars:?} -> {})", block.0.len());
				}
				Kind::Block(block) => {
					println!("AST-Block({})", block.0.len());
				}
			}
		}
	}
}

fn synthesize(_data: &Data, _ast_kind: &Kind) -> crate::Type {
	crate::Type::Unit
}

