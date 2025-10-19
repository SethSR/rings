
use std::collections::HashMap;
use std::collections::hash_map::Entry;

use crate::ast;
use crate::identifier;
use crate::error;
use crate::Data;

use ast::Kind;

// NOTE - srenshaw - CE <== Compiler Error: This shouldn't happen and needs to be fixed

// NOTE - srenshaw - TC <== Type-Checker Error: Turn this into it's own error category

#[derive(Debug, Default)]
struct Checker {
	ast_to_type: HashMap<ast::Id, Type>,
	ident_to_type: HashMap<identifier::Id, Type>,
}

pub fn eval(data: &mut Data) {
	for (proc_id, proc_range) in &data.completed_procs {
		let proc_type = &data.procedures[proc_id];

		let mut checker = Checker::default();
		for (param_name, param_type) in &proc_type.params {
			checker.ident_to_type.insert(*param_name, Type::Rings(*param_type));
		}

		let node_iter = data.ast_nodes[proc_range.clone()].iter();
		let range_iter = data.ast_pos_tok[proc_range.clone()].iter();
		for (offset, (node, range)) in node_iter.zip(range_iter).enumerate() {
			print!("[{offset:3}][{:3}..{:3}] ", range.start, range.end);

			let ast_id = proc_range.start + offset;
			match node {
				Kind::Int(num) => {
					println!("AST-Int({num})");
					checker.ast_to_type.insert(ast_id, Type::Top);
				}

				Kind::Dec(num) => {
					println!("AST-Dec({num})");
					checker.ast_to_type.insert(ast_id, Type::Top);
				}

				Kind::Ident(ident_id) => {
					checker.check_ident(data, ident_id, ast_id);
				}

				Kind::Define(ident_id, var_type, ast_id) => {
					if let Some(err_msg) = checker.check_define(data, ident_id, *ast_id, *var_type) {
						println!("{checker:?}");
						error::error(data, &err_msg, range.start);
						return;
					}
				}

				Kind::Assign(ident_id, ast_id) => {
					if let Some(err_msg) = checker.check_assign(data, ident_id, *ast_id) {
						println!("{checker:?}");
						error::error(data, &err_msg, range.start);
						return;
					}
				}

				Kind::BinOp(op, left, right) => {
					println!("AST-BinOp({} {op} {})", left.index(), right.index());
					if let Some(err_msg) = checker.check_binop(data, ast_id, *op, left, right) {
						error::error(data, &err_msg, range.start);
						return;
					}
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

impl Checker {
	fn check_ident(&mut self, data: &Data, ident_id: &identifier::Id, ast_id: ast::Id) {
		println!("AST-Ident({})", data.text(*ident_id));
		let new_type = self.ident_to_type.get(ident_id)
			.unwrap_or(&Type::Top);
		self.ast_to_type.insert(ast_id, *new_type);
	}

	fn check_define(&mut self, data: &Data, ident_id: &identifier::Id, ast_id: ast::Id,
		var_type: crate::Type,
	) -> Option<String> {
		println!("AST-Define({} : {var_type:?} = {})", data.text(*ident_id), ast_id.index());
		match self.ident_to_type.entry(*ident_id) {
			Entry::Occupied(_) => {
				Some(format!("TC - '{}' has already been defined", data.text(*ident_id)))
			}
			Entry::Vacant(e) => {
				let Some(expr_type) = self.ast_to_type.get(&ast_id) else {
						return Some(format!("CE - expression has no type: {ast_id:?}"));
				};
				match expr_type.meet(Type::Rings(var_type)) {
					Type::Bot => {
						Some(format!("TC - variable has type '{}', but the expression has type '{}'",
								var_type.display(data),
								expr_type.display(data),
							))
					}
					new_type => {
						e.insert(new_type);
						None
					}
				}
			}
		}
	}

	fn check_assign(&mut self, data: &Data,
		ident_id: &identifier::Id, ast_id: ast::Id,
	) -> Option<String> {
		println!("AST-Assign({} = {})", data.text(*ident_id), ast_id.index());
		let Some(var_type) = self.ident_to_type.get(ident_id) else {
			return Some(format!("TC - found unknown identifier '{}'", data.text(*ident_id)));
		};
		let Some(expr_type) = self.ast_to_type.get(&ast_id) else {
			return Some(format!("CE - expression has no type: {ast_id:?}"));
		};
		if &expr_type.meet(*var_type) != var_type {
			Some(format!("TC - variable has type '{}', but expression has type '{}'",
				var_type.display(data),
				expr_type.display(data),
			))
		} else {
			// The types match, so we're okay.
			None
		}
	}

	fn check_binop(&mut self, data: &Data,
		ast_id: ast::Id,
		op: crate::BinaryOp, left: &ast::Id, right: &ast::Id,
	) -> Option<String> {
		if op == crate::BinaryOp::Add {
			let left_type = self.ast_to_type[left];
			let right_type = self.ast_to_type[right];
			match left_type.meet(right_type) {
				Type::Bot => {
					Some(format!("TC - unable to apply '{op}' to types '{}' and '{}'",
						left_type.display(data),
						right_type.display(data),
					))
				}
				new_type => {
					// Types are able to meet
					self.ast_to_type.insert(ast_id, new_type);
					None
				}
			}
		} else {
			todo!("add the rest of the binary operators")
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Type {
	Top, // Any/unknown type
	Rings(crate::Type), // specific compiler type
	Bot, // All/invalid type
}

impl Type {
	fn display<'a>(&self, data: &'a Data) -> &'a str {
		match self {
			Self::Top => "Unknown type",
			Self::Bot => "Invalid type",
			Self::Rings(ty) => ty.display(data),
		}
	}
}

impl Type {
	fn meet(self, rhs: Type) -> Type {
		use crate::Type as Rings;

		match (self, rhs) {
			(Self::Top, _) => rhs,
			(_, Self::Top) => self,
			(Self::Bot, _) | (_, Self::Bot) => Self::Bot,

			(Self::Rings(Rings::Record(a)), Self::Rings(Rings::Record(b))) => if a == b {
				self
			} else {
				Self::Bot
			}
			(Self::Rings(Rings::Record(_)), _) | (_, Self::Rings(Rings::Record(_))) => Self::Bot,

			(Self::Rings(Rings::Table(a)), Self::Rings(Rings::Table(b))) => if a == b {
				self
			} else {
				Self::Bot
			}
			(Self::Rings(Rings::Table(_)), _) | (_, Self::Rings(Rings::Table(_))) => Self::Bot,

			(Self::Rings(Rings::Unit), Self::Rings(Rings::Unit)) => Self::Rings(Rings::Unit),
			(Self::Rings(Rings::Unit), _) | (_, Self::Rings(Rings::Unit)) => Self::Bot,

			(Self::Rings(Rings::U32), Self::Rings(Rings::S32)) => Self::Bot,
			(Self::Rings(Rings::S32), Self::Rings(Rings::U32)) => Self::Bot,

			(Self::Rings(Rings::S16), Self::Rings(Rings::U32)) => Self::Bot,
			(Self::Rings(Rings::S8), Self::Rings(Rings::U32)) => Self::Bot,
			(Self::Rings(Rings::U32), Self::Rings(Rings::S16)) => Self::Bot,
			(Self::Rings(Rings::U32), Self::Rings(Rings::S8)) => Self::Bot,

			(Self::Rings(Rings::U16), Self::Rings(Rings::S16)) => Self::Bot,
			(Self::Rings(Rings::S16), Self::Rings(Rings::U16)) => Self::Bot,

			(Self::Rings(Rings::S8), Self::Rings(Rings::U16)) => Self::Bot,
			(Self::Rings(Rings::U16), Self::Rings(Rings::S8)) => Self::Bot,

			(Self::Rings(Rings::S8), Self::Rings(Rings::U8)) => Self::Bot,
			(Self::Rings(Rings::U8), Self::Rings(Rings::S8)) => Self::Bot,

			(Self::Rings(Rings::Bool), _) | (_, Self::Rings(Rings::Bool)) => Self::Rings(Rings::Bool),
			(Self::Rings(Rings::S32), _) | (_, Self::Rings(Rings::S32)) => Self::Rings(Rings::S32),
			(Self::Rings(Rings::U32), _) | (_, Self::Rings(Rings::U32)) => Self::Rings(Rings::U32),
			(Self::Rings(Rings::S16), _) | (_, Self::Rings(Rings::S16)) => Self::Rings(Rings::S16),
			(Self::Rings(Rings::U16), _) | (_, Self::Rings(Rings::U16)) => Self::Rings(Rings::U16),
			(Self::Rings(Rings::S8), Self::Rings(Rings::S8)) => Self::Rings(Rings::S8),
			(Self::Rings(Rings::U8), Self::Rings(Rings::U8)) => Self::Rings(Rings::U8),
		}
	}
}

