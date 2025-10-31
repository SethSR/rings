
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
	for (proc_id, &proc_start) in &data.completed_procs {
		let proc_type = &data.procedures[proc_id];

		let mut checker = Checker::default();
		for (param_name, param_type) in &proc_type.params {
			checker.ident_to_type.insert(*param_name, Type::Rings(*param_type));
		}

		let node = &data.ast_nodes[proc_start];
		let range = &data.ast_pos_tok[proc_start];
		let ret_type = proc_type.ret_type;
		if let Some(err_msg) = checker.check_stmt(data, node, proc_start, ret_type) {
			println!("{checker:?}");
			let mut err =error::error(data, &err_msg, range.start);
			err.set_kind(error::Kind::Checker);
			data.errors.push(err);
			return;
		}
	}
}

impl Checker {
	fn check_stmt(&mut self, data: &Data,
		node: &ast::Kind, ast_id: ast::Id, ret_type: crate::Type,
	) -> Option<String> {
		match node {
			Kind::Int(num) => {
				println!("AST-Int({num})");
				self.ast_to_type.insert(ast_id, Type::Top);
				None
			}

			Kind::Dec(num) => {
				println!("AST-Dec({num})");
				self.ast_to_type.insert(ast_id, Type::Top);
				None
			}

			Kind::Ident(ident_id) => {
				println!("AST-Ident({})", data.text(ident_id));
				self.check_ident(ident_id, ast_id);
				None
			}

			Kind::Define(ident_id, var_type, expr_id) => {
				println!("AST-Define({} : {var_type:?} = {})", data.text(ident_id), ast_id.index());
				self.check_define(data, ident_id, *expr_id, *var_type)
			}

			Kind::Assign(ident_id, expr_id) => {
				println!("AST-Assign({} = {})", data.text(ident_id), ast_id.index());
				self.check_assign(data, ident_id, *expr_id)
			}

			Kind::BinOp(op, left, right) => {
				println!("AST-BinOp({} {op} {})", left.index(), right.index());
				self.check_binop(data, ast_id, *op, left, right)
			}

			Kind::UnOp(op, right) => {
				println!("AST-UnOp({op}{})", right.index());
				self.check_unop(data, ast_id, *op, right)
			}

			Kind::Return(expr_id) => {
				println!("AST-Return({})", expr_id
					.map(|id| id.index().to_string())
					.unwrap_or("-".to_string()));
				self.check_return(*expr_id, ret_type)
			}

			Kind::Block(block) => {
				println!("AST-Block({})", block.0.len());
				for stmt_id in &block.0 {
					let stmt = &data.ast_nodes[*stmt_id];
					self.check_stmt(data, stmt, *stmt_id, ret_type)?;
				}
				None
			}

			Kind::If(cond, then_block, else_block) => {
				println!("AST-If({} -> {} <> {})", cond.index(), then_block.0.len(), else_block.0.len());
				let cond_type = self.ast_to_type[cond];
				if cond_type.meet(Type::Rings(crate::Type::S32)) == Type::Bot
				&& cond_type.meet(Type::Rings(crate::Type::U32)) == Type::Bot
				{
					todo!("TC - cond node must have integer type");
				}
				for stmt_id in &then_block.0 {
					let stmt = &data.ast_nodes[*stmt_id];
					self.check_stmt(data, stmt, *stmt_id, ret_type)?;
				}
				for stmt_id in &else_block.0 {
					let stmt = &data.ast_nodes[*stmt_id];
					self.check_stmt(data, stmt, *stmt_id, ret_type)?;
				}
				todo!()
			}

			Kind::While(cond, block) => {
				println!("AST-While({} -> {})", cond.index(), block.0.len());
				todo!()
			}

			Kind::For(vars, Some(table_id), Some(range), block) => {
				println!("AST-For({} in {}[{}] -> {})",
					vars.iter().map(|var| data.text(var)).collect::<Vec<_>>().join(","),
					data.text(table_id), range, block.0.len());
				todo!("ranged-table for-loop")
			}

			Kind::For(vars, Some(table_id), None, block) => {
				println!("AST-For({} in {} -> {})",
					vars.iter().map(|var| data.text(var)).collect::<Vec<_>>().join(","),
					data.text(table_id), block.0.len());
				todo!("indexed-table for-loop")
			}

			Kind::For(vars, None, Some(range), block) => {
				println!("AST-For({} in {} -> {})",
					vars.iter().map(|var| data.text(var)).collect::<Vec<_>>().join(","),
					range, block.0.len());
				if vars.len() != 1 {
					return Some("simple for-loops require a single loop variable".to_string());
				}
				match range {
					crate::Bounds::Full { start, end } => {
						debug_assert!(start <= end);
						if start > end {
							return Some("start value must be less than or equal to end value".to_string());
						}
					}
					crate::Bounds::From {..} | crate::Bounds::To {..} => {
						return Some("simple for-loops require a fully specified range (start..end)".to_string());
					}
				}
				for stmt_id in &block.0 {
					let stmt = &data.ast_nodes[*stmt_id];
					self.check_stmt(data, stmt, *stmt_id, ret_type);
				}
				None
			}

			Kind::For(vars, None, None, block) => {
				println!("AST-For({} -> {})",
					vars.iter().map(|var| data.text(var)).collect::<Vec<_>>().join(","),
					block.0.len());
				todo!("infinite for-loop")
			}

			Kind::TableIndex(table_id, expr_id) => {
				println!("AST-TableIndex({}[{}])", data.text(table_id), expr_id);
				todo!("table-index")
			}

			Kind::Call(proc_id, exprs) => {
				println!("AST-Call({}({}))", data.text(proc_id), exprs.iter()
					.map(|id| id.to_string())
					.collect::<Vec<_>>()
					.join(","));
				todo!("procedure-call")
			}
		}
	}

	fn check_ident(&mut self,
		ident_id: &identifier::Id, ast_id: ast::Id,
	) {
		let new_type = self.ident_to_type.get(ident_id)
			.unwrap_or(&Type::Top);
		self.ast_to_type.insert(ast_id, *new_type);
	}

	fn check_define(&mut self, data: &Data,
		ident_id: &identifier::Id, ast_id: ast::Id,
		var_type: crate::Type,
	) -> Option<String> {
		match self.ident_to_type.entry(*ident_id) {
			Entry::Occupied(_) => {
				Some(format!("TC - '{}' has already been defined", data.text(ident_id)))
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
		let Some(var_type) = self.ident_to_type.get(ident_id) else {
			return Some(format!("TC - found unknown identifier '{}'", data.text(ident_id)));
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
				Type::Bot => Some(format!("TC - unable to apply '{op}' to types '{}' and '{}'",
					left_type.display(data),
					right_type.display(data),
				)),
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

	fn check_unop(&mut self, data: &Data,
		ast_id: ast::Id,
		op: crate::UnaryOp, right: &ast::Id,
	) -> Option<String> {
		let right_type = self.ast_to_type[right];
		let Type::Rings(rtype) = right_type else {
			return Some(format!("TC - unable to apply '{op}' to type '{}'", right_type.display(data)));
		};

		use crate::Type as T;
		if !matches!(rtype, T::Bool | T::U8 | T::S8 | T::U16 | T::S16 | T::U32 | T::S32) {
			return Some(format!("TC - unable to apply '{op}' to type '{}'", right_type.display(data)));
		}
		self.ast_to_type.insert(ast_id, right_type);
		None
	}

	fn check_return(&mut self,
		ast_id: Option<ast::Id>, proc_type: crate::Type,
	) -> Option<String> {
		let ret_type = match ast_id {
			Some(ast_id) => match self.ast_to_type.get(&ast_id) {
				Some(ret_type) => *ret_type,
				None => return Some(format!("CE - expression has no type: {ast_id:?}")),
			}
			None => Type::Rings(crate::Type::Unit),
		};
		if ret_type.meet(Type::Rings(proc_type)) == Type::Bot {
			return Some(format!("TC - Expected return type {proc_type:?}, found '{ret_type:?}'"));
		}
		None
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
		match (self, rhs) {
			(Self::Top, _) => rhs,
			(_, Self::Top) => self,
			(Self::Bot, _) | (_, Self::Bot) => Self::Bot,

			(Self::Rings(a), Self::Rings(b)) => meet_rings(a, b)
				.map(Self::Rings)
				.unwrap_or(Self::Bot),
		}
	}
}

fn meet_rings(a: crate::Type, b: crate::Type) -> Option<crate::Type> {
	use crate::Type as T;

	match (a, b) {
		(type_a, type_b) if type_a == type_b => Some(type_a),

		(T::Record(_), _) | (_, T::Record(_)) => None,

		(T::Table(_), _) | (_, T::Table(_)) => None,

		(T::Unit, _) | (_, T::Unit) => None,

		(T::U32, T::S32) | (T::S32, T::U32) => None,
		(T::U32, T::S16) | (T::S16, T::U32) => None,
		(T::U32, T::S8 ) | (T::S8 , T::U32) => None,

		(T::U16, T::S16) | (T::S16, T::U16) => None,
		(T::U16, T::S8)  | (T::S8 , T::U16) => None,
		(T::U8 , T::S8)  | (T::S8 , T::U8 ) => None,

		(T::S8, T::S8) => Some(T::S8),
		(T::U8, T::U8) => Some(T::U8),

		(T::Bool, _) | (_, T::Bool) => Some(T::Bool),
		(T::S32, _) | (_, T::S32) => Some(T::S32),
		(T::U32, _) | (_, T::U32) => Some(T::U32),
		(T::S16, _) | (_, T::S16) => Some(T::S16),
		(T::U16, _) | (_, T::U16) => Some(T::U16),
	}
}

