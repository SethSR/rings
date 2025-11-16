
use crate::ast;
use crate::error;
use crate::identifier::Id as IdentId;
use crate::{BinaryOp, Data, Bounds, UnaryOp};


type TempId = u32; // Temporary variable ID

#[derive(Debug, Clone, PartialEq)]
pub enum Tac {
	// Basic operations
	Copy { src: Location,  dst: Location },

	// Arithmetic
	BinOp { op: BinaryOp, left: Location, right: Location, dst: Location },
	UnOp { op: UnaryOp, src: Location, dst: Location },

	// Memory access (for table/variable access)
	#[cfg(feature="ready")]
	Load { address: Location, offset: i32, dst: TempId },
	#[cfg(feature="ready")]
	Store { src: Location, address: Location, offset: i32 },

	// Control Flow
	Label(LabelId),
	Jump(LabelId),
	JumpIf { cond: Location, target: LabelId },
	JumpIfNot { cond: Location, target: LabelId },

	// Procedure related
	#[cfg(feature="ready")]
	Call { name: IdentId, args: Vec<Location>, dst: Option<TempId> },
	Return(Option<Location>),

	// Comments/debug
	#[cfg(feature="ready")]
	Comment(String),
}

impl Tac {
	pub fn to_text(&self, data: &Data) -> String {
		match self {
			Tac::Copy { src, dst } => {
				format!("COPY  {} -> {}", src.to_text(data), dst.to_text(data))
			}
			Tac::BinOp { op, left, right, dst } => {
				format!("BINOP {} {op} {} -> {}",
					left.to_text(data), right.to_text(data), dst.to_text(data))
			}
			Tac::UnOp { op, src, dst } => {
				format!("UNOP  {op}{} -> {}", src.to_text(data), dst.to_text(data))
			}
			#[cfg(feature="ready")]
			Tac::Load { address, offset, dst } => {
				format!("LOAD  ({} + {offset}) -> ?{dst}", address.to_text(data))
			}
			#[cfg(feature="ready")]
			Tac::Store { src, address, offset } => {
				format!("STORE {} -> ({} + {offset})", src.to_text(data), address.to_text(data))
			}
			Tac::Label(label_id) => {
				format!("label_{label_id}:")
			}
			Tac::Jump(label_id) => {
				format!("JUMP  label_{label_id}")
			}
			Tac::JumpIf { cond, target } => {
				format!("JIF   {} => label_{target}", cond.to_text(data))
			}
			Tac::JumpIfNot { cond, target } => {
				format!("JIF  !{} => label_{target}", cond.to_text(data))
			}
			#[cfg(feature="ready")]
			Tac::Call { name, args, dst: Some(dst) } => {
				format!("CALL  {}({}) -> {dst}", data.text(name),
					args.iter().map(|loc| loc.to_text(data)).collect::<Vec<_>>().join(","))
			}
			#[cfg(feature="ready")]
			Tac::Call { name, args, ..} => {
				format!("CALL  {}({})", data.text(name),
					args.iter().map(|loc| loc.to_text(data)).collect::<Vec<_>>().join(","))
			}
			Tac::Return(Some(address)) => {
				format!("RET   {}", address.to_text(data))
			}
			Tac::Return(None) => {
				"RET".to_string()
			}
			#[cfg(feature="ready")]
			Tac::Comment(msg) => {
				format!("; {msg}")
			}
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Location {
	Temp(TempId),      // Temporary variable
	Variable(IdentId), // Named variable
	Constant(u32),     // Immediate constant
}

impl Location {
	fn to_text(&self, data: &Data) -> String {
		match self {
			Self::Temp(temp_id) => format!("?{temp_id}"),
			Self::Variable(ident_id) => data.text(ident_id).to_string(),
			Self::Constant(num) => num.to_string(),
		}
	}
}

pub type LabelId = u32;

pub fn eval(data: &mut Data) {
	let mut tac_functions = data.completed_procs.keys()
		.map(|proc_id| TacSection {
			name: *proc_id,
			locals: vec![],
			instructions: vec![],
			next_temp: 0,
			next_label: 0,
		})
		.collect::<Vec<_>>();

	for mut tac_function in tac_functions.drain(..) {
		let name = data.text(&tac_function.name).to_string();

		if !tac_function.lower(data) {
			eprintln!("failed to lower '{name}'");
		}
		data.tac_sections.insert(tac_function.name, tac_function);
	}
}

#[derive(Debug, PartialEq)]
pub struct TacSection {
	pub name: IdentId,
	// pub params: Vec<IdentId>,
	pub locals: Vec<(IdentId, crate::Type)>,
	pub instructions: Vec<Tac>,
	pub next_temp: TempId,
	pub next_label: LabelId,
}

impl TacSection {
	pub fn lower(&mut self, data: &mut Data) -> bool {
		let proc_start = data.completed_procs[&self.name];

		let kind = &data.ast_nodes[proc_start];
		let range = &data.ast_pos_tok[proc_start];
		if let Err(err_msg) = self.lower_node(kind, data) {
			error::error(data, &err_msg, range.start);
			return false;
		}

		true
	}

	fn lower_node(&mut self, kind: &ast::Kind, data: &Data) -> Result<Option<Location>, String> {
		match kind {
			ast::Kind::Int(value) => {
				// TODO - srenshaw - Ensure value is within u32
				Ok(Some(Location::Constant(*value as u32)))
			}

			ast::Kind::Dec(_value) => {
				todo!()
			}

			ast::Kind::Ident(ident_id) => {
				Ok(Some(Location::Variable(*ident_id)))
			}

			ast::Kind::Define(var_id, type_kind, expr_id) => {
				let ast::Kind::Ident(ident_id) = &data.ast_nodes[*var_id] else {
					return Err("cannot initialize internal variables, assign instead".to_string());
				};

				// Get initializer value
				let Some(init_value) = self.lower_node(&data.ast_nodes[*expr_id], data)? else {
					return Ok(None);
				};

				// Add to locals
				self.locals.push((*ident_id, *type_kind));

				// Emit assignment
				self.emit(Tac::Copy {
					src: init_value,
					dst: Location::Variable(*ident_id),
				});

				Ok(None)
			}

			ast::Kind::Assign(var_id, expr_id) => {
				let Some(lvalue) = self.lower_node(&data.ast_nodes[*var_id], data)? else {
					todo!("unknown assign L-Value '{:?}'", data.ast_nodes.get(*var_id));
				};

				let Some(expr_node) = data.ast_nodes.get(*expr_id) else {
					todo!("no expression AST node {expr_id}");
				};
				let Some(rvalue) = self.lower_node(expr_node, data)? else {
					return Ok(None);
				};

				self.emit(Tac::Copy {
					src: rvalue,
					dst: lvalue,
				});

				Ok(None)
			}

			ast::Kind::BinOp(op, left_id, right_id) => {
				let Some(left) = self.lower_node(&data.ast_nodes[*left_id], data)? else {
					return Ok(None);
				};
				let Some(right) = self.lower_node(&data.ast_nodes[*right_id], data)? else {
					return Ok(None);
				};

				// Allocation temporary for result
				let temp = self.alloc_temp();

				self.emit(Tac::BinOp {
					op: *op,
					left,
					right,
					dst: Location::Temp(temp),
				});

				Ok(Some(Location::Temp(temp)))
			}

			ast::Kind::UnOp(op, right_id) => {
				let Some(node) = self.lower_node(&data.ast_nodes[*right_id], data)? else {
					return Ok(None);
				};

				let temp = self.alloc_temp();

				self.emit(Tac::UnOp {
					op: *op,
					src: node,
					dst: Location::Temp(temp),
				});

				Ok(Some(Location::Temp(temp)))
			}

			ast::Kind::Return(maybe_expr) => {
				let value = if let Some(expr_id) = maybe_expr {
					self.lower_node(&data.ast_nodes[*expr_id], data)?
				} else {
					None
				};

				self.emit(Tac::Return(value));
				Ok(None)
			}

			ast::Kind::Block(stmts) => {
				for stmt_id in &stmts.0 {
					self.lower_node(&data.ast_nodes[*stmt_id], data)?;
				}
				Ok(None)
			}

			ast::Kind::If(cond_id, then_block, else_block) => {
				let Some(cond) = self.lower_node(&data.ast_nodes[*cond_id], data)? else {
					return Ok(None);
				};

				let else_label = self.alloc_label();
				let end_label = self.alloc_label();

				// if !cond goto else
				self.emit(Tac::JumpIfNot { cond, target: else_label });

				// then block
				for stmt_id in &then_block.0 {
					self.lower_node(&data.ast_nodes[*stmt_id], data)?;
				}
				self.emit(Tac::Jump(end_label));

				// else block
				self.emit(Tac::Label(else_label));
				for stmt_id in &else_block.0 {
					self.lower_node(&data.ast_nodes[*stmt_id], data)?;
				}

				// end
				self.emit(Tac::Label(end_label));
				Ok(None)
			}

			ast::Kind::While(cond_id, body_block) => {
				let start_label = self.alloc_label();
				let end_label = self.alloc_label();

				// start:
				self.emit(Tac::Label(start_label));

				// if !cond goto end
				let Some(cond) = self.lower_node(&data.ast_nodes[*cond_id], data)? else {
					return Ok(None);
				};
				self.emit(Tac::JumpIfNot { cond, target: end_label });

				// body
				for stmt_id in &body_block.0 {
					self.lower_node(&data.ast_nodes[*stmt_id], data)?;
				}

				// goto start
				self.emit(Tac::Jump(start_label));

				// end:
				self.emit(Tac::Label(end_label));
				Ok(None)
			}

			ast::Kind::For(
				iter_vars,
				table_id,
				bounds,
				body_block,
			) => {
				// For now, simple version: lower to while loop
				// for i in 0..N becomes:
				// i = 0
				// while i < N:
				//   body
				//   i = i + 1

				self.lower_for_loop(data, iter_vars, table_id, bounds, body_block)?;

				Ok(None)
			}

			ast::Kind::Call(_proc_id, _exprs) => {
				todo!("lower proc-call")
			}

			#[cfg(feature="ready")]
			ast::Kind::Access(base_id, segments) => {
				let temp = Location::Temp(self.alloc_temp());
				let address = if let Some(record) = data.records.get(base_id) {
					record.address.unwrap_or_else(|| panic!("no address for record '{}'", data.text(base_id)))
				} else if let Some(table) = data.tables.get(base_id) {
					table.address.unwrap_or_else(|| panic!("no address for table '{}'", data.text(base_id)))
				} else {
					panic!("'{}' is not a Record or Table", data.text(base_id));
				};

				self.emit(Tac::Copy {
					src: Location::Constant(address),
					dst: temp.clone(),
				});

				let mut curr_ident = *base_id;
				let mut i = 0;
				while i < segments.len() {
					match segments[i] {
						ast::PathSegment::Field(field_id) => {
							let Some(record) = data.records.get(&curr_ident) else {
								panic!("no record '{}'", data.text(&curr_ident));
							};
							let Some(field_offset) = record.field_offset(data, field_id) else {
								panic!("no field '{}' in record '{}'", data.text(&field_id), data.text(&curr_ident));
							};
							curr_ident = *match record.fields.iter().find(|(id,_)| field_id == *id) {
								Some((_, field_type)) => match field_type {
									crate::Type::Record(id) => Ok(id),
									crate::Type::Table(id) => Ok(id),
									_ => Err(format!("expected a record or table type, found '{}'", data.type_text(*field_type))),
								}
								None => unreachable!(),
							}?;

							self.emit(Tac::BinOp {
								op: BinaryOp::Add,
								left: temp.clone(),
								right: Location::Constant(field_offset),
								dst: temp.clone(),
							});

							i += 1;
						}

						ast::PathSegment::Index(expr_id, field_id) => {
							let Some(expr) = self.lower_node(&data.ast_nodes[expr_id], data)? else {
								return Ok(None);
							};

							let table = &data.tables[&curr_ident];
							let column_offset = table.column_offset(data, field_id)
								.unwrap_or_else(|| panic!("no field '{}' in table '{}'", data.text(&field_id), data.text(&curr_ident)));
							curr_ident = *match table.column_spec.iter().find(|(id,_)| field_id == *id) {
								Some((_, field_type)) => match field_type {
									crate::Type::Record(id) => Ok(id),
									crate::Type::Table(id) => Ok(id),
									_ => Err(format!("expected a record or table type, found '{}'", data.type_text(*field_type))),
								}
								None => unreachable!(),
							}?;

							self.emit(Tac::BinOp {
								op: BinaryOp::Add,
								left: temp.clone(),
								right: Location::Constant(column_offset),
								dst: temp.clone(),
							});

							// NOTE - srenshaw - We can unwrap here because we already checked for field
							// existence to get the column-offset.
							let field_size = table.column_spec.iter()
								.find(|(id,_)| field_id == *id)
								.map(|(_, field_type)| data.type_size(field_type))
								.unwrap();

							// t0 = expr * field_size
							let t0 = self.alloc_temp();
							self.emit(Tac::BinOp {
								op: BinaryOp::Mul,
								left: expr,
								right: Location::Constant(field_size),
								dst: Location::Temp(t0),
							});

							// temp = temp + t0
							self.emit(Tac::BinOp {
								op: BinaryOp::Add,
								left: temp.clone(),
								right: Location::Temp(t0),
								dst: temp.clone(),
							});

							i += 2;
						}
					};
				}

				Ok(Some(temp))
			}
		}
	}

	fn lower_for_loop(
		&mut self,
		data: &Data,
		iter_vars: &[IdentId],
		table_id: &Option<IdentId>,
		bounds: &Option<Bounds>,
		body_block: &ast::Block,
	) -> Result<(), String> {
		// Simple case: for i in 0..N
		if iter_vars.len() == 1 && table_id.is_none() {
			let index_var = iter_vars[0];

			let start_label = self.alloc_label();
			let end_label = self.alloc_label();

			let Some(Bounds::Full { start, end }) = bounds else {
				return Err("L - non-table loop ranges require a min and max value".to_string());
			};

			// i = start
			self.emit(Tac::Copy {
				src: Location::Constant(*start),
				dst: Location::Variable(index_var),
			});

			// start:
			self.emit(Tac::Label(start_label));

			// if i >= end goto end
			let temp = self.alloc_temp();
			self.emit(Tac::BinOp {
				op: BinaryOp::CmpGE,
				left: Location::Variable(index_var),
				right: Location::Constant(*end),
				dst: Location::Temp(temp),
			});

			// body
			for stmt_id in &body_block.0 {
				self.lower_node(&data.ast_nodes[*stmt_id], data)?;
			}

			self.emit(Tac::JumpIf {
				cond: Location::Temp(temp),
				target: end_label,
			});

			// i = i + 1
			let temp2 = self.alloc_temp();
			self.emit(Tac::BinOp {
				op: BinaryOp::Add,
				left: Location::Variable(index_var),
				right: Location::Constant(1),
				dst: Location::Temp(temp2),
			});
			self.emit(Tac::Copy {
				src: Location::Temp(temp2),
				dst: Location::Variable(index_var),
			});

			// goto start
			self.emit(Tac::Jump(start_label));

			// end:
			self.emit(Tac::Label(end_label));
		}

		// TODO - srenshaw - Handle table iteration, multiple variables, etc.
		Ok(())
	}

	fn emit(&mut self, instr: Tac) {
		self.instructions.push(instr);
	}

	fn alloc_temp(&mut self) -> TempId {
		self.next_temp += 1;
		self.next_temp - 1
	}

	fn alloc_label(&mut self) -> LabelId {
		self.next_label += 1;
		self.next_label - 1
	}
}

#[cfg(test)]
mod tests {
	use crate::{lexer, discovery, parser, type_checker};
	use crate::identifier::Identifier;
	use crate::Data;

	use super::*;

	fn setup(source: &str) -> Data {
		let mut data = Data::new("lowering".into(), source.into());
		lexer::eval(&mut data);
		discovery::eval(&mut data);
		parser::eval(&mut data);
		type_checker::eval(&mut data);
		eval(&mut data);
		assert!(data.errors.is_empty(), "{}", data.errors.iter()
				.map(|e| e.display(&data))
				.collect::<Vec<_>>()
				.join("\n"));
		data
	}

	#[test]
	fn return_void() {
		let data = setup("proc a() {
			return;
		}");
		assert_eq!(data.tac_sections.len(), 1);
		assert_eq!(data.tac_sections[&"a".id()].instructions, [
			Tac::Return(None),
		]);
	}

	#[test]
	fn return_expression() {
		let data = setup("proc a() -> s8 {
			return 100 - 200;
		}");
		assert_eq!(data.tac_sections.len(), 1);
		assert_eq!(data.tac_sections[&"a".id()].instructions, [
			Tac::BinOp {
				op: BinaryOp::Sub,
				left: Location::Constant(100),
				right: Location::Constant(200),
				dst: Location::Temp(0),
			},
			Tac::Return(Some(Location::Temp(0))),
		]);
	}
}

