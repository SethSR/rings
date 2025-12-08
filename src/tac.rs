
use crate::ast::{Block as ABlock, Id as AstId, Kind};
use crate::error;
use crate::identifier::Id as IdentId;
use crate::{BinaryOp, Bounds, Data, ProcData, UnaryOp};


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

pub fn eval(data: &mut Data) -> Result<(), crate::Error> {
	let mut result = Ok(());
	for (proc_id, proc_data) in &mut data.proc_db {
		let mut section = Section::default();
		section.name = *proc_id;
		match section.lower(proc_data) {
			Ok(_) => {
				proc_data.tac_data = Some(section);
			},
			Err(e) => {
				result = Err(e.into_comp_error(data));
				break;
			},//.into_comp_error(data, proc_data)),
		}
	}
	result
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Section {
	pub name: IdentId,
	// pub params: Vec<IdentId>,
	pub locals: Vec<(IdentId, crate::Type)>,
	pub instructions: Vec<Tac>,
	pub next_temp: TempId,
	pub next_label: LabelId,
}

impl Section {
	fn lower(&mut self, proc_data: &mut ProcData) -> Result<(), Error> {
		let proc_start = proc_data.ast_start;

		if let Err(err) = self.lower_node(&proc_start, proc_data) {
			//let range = &proc_data.ast_pos_tok[proc_start];
			//error::error(proc_data, &err_msg, range.start);
			return Err(err);
		}

		Ok(())
	}

	fn lower_node(&mut self, id: &AstId, proc_data: &ProcData,
	) -> Result<Option<Location>, Error> {
		let Some(kind) = proc_data.ast_nodes.get(*id) else {
			return Err(Error::missing_ast_node(self.name, *id));
		};

		match kind {
			Kind::Int(value) => {
				// TODO - srenshaw - Ensure value is within u32
				Ok(Some(Location::Constant(*value as u32)))
			}

			Kind::Dec(_value) => {
				todo!()
			}

			Kind::Ident(ident_id) => {
				Ok(Some(Location::Variable(*ident_id)))
			}

			Kind::Define(var_id, type_kind, expr_id) => {
				let Kind::Ident(ident_id) = &proc_data.ast_nodes[*var_id] else {
					return Err(Error::missing_ast_node(self.name, *var_id));
				};

				// Get initializer value
				let Some(init_value) = self.lower_node(expr_id, proc_data)? else {
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

			Kind::Assign(var_id, expr_id) => {
				let Some(lvalue) = self.lower_node(var_id, proc_data)? else {
					todo!("unknown assign L-Value '{:?}'", proc_data.ast_nodes.get(*var_id));
				};

				let Some(rvalue) = self.lower_node(expr_id, proc_data)? else {
					return Ok(None);
				};

				self.emit(Tac::Copy {
					src: rvalue,
					dst: lvalue,
				});

				Ok(None)
			}

			Kind::BinOp(op, left_id, right_id) => {
				let Some(left) = self.lower_node(left_id, proc_data)? else {
					return Ok(None);
				};
				let Some(right) = self.lower_node(right_id, proc_data)? else {
					return Ok(None);
				};

				let (left, right) = match (left, right) {
					(Location::Constant(_), Location::Constant(_)) => (left, right),
					(_, Location::Constant(_)) => (right, left),
					_ => (left, right),
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

			Kind::UnOp(op, right_id) => {
				let Some(node) = self.lower_node(right_id, proc_data)? else {
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

			Kind::Return(maybe_expr) => {
				let value = if let Some(expr_id) = maybe_expr {
					self.lower_node(expr_id, proc_data)?
				} else {
					None
				};

				self.emit(Tac::Return(value));
				Ok(None)
			}

			Kind::Block(stmts) => {
				for stmt_id in &stmts.0 {
					self.lower_node(stmt_id, proc_data)?;
				}
				Ok(None)
			}

			Kind::If(cond_id, then_block, else_block) => {
				let Some(cond) = self.lower_node(cond_id, proc_data)? else {
					return Ok(None);
				};

				let else_label = self.alloc_label();
				let end_label = self.alloc_label();

				// if !cond goto else
				self.emit(Tac::JumpIfNot { cond, target: else_label });

				// then block
				for stmt_id in &then_block.0 {
					self.lower_node(stmt_id, proc_data)?;
				}
				self.emit(Tac::Jump(end_label));

				// else block
				self.emit(Tac::Label(else_label));
				for stmt_id in &else_block.0 {
					self.lower_node(stmt_id, proc_data)?;
				}

				// end
				self.emit(Tac::Label(end_label));
				Ok(None)
			}

			Kind::While(cond_id, body_block) => {
				let cond_label = self.alloc_label();
				let loop_label = self.alloc_label();

				self.emit(Tac::Jump(cond_label));

				// start:
				self.emit(Tac::Label(loop_label));

				// body
				for stmt_id in &body_block.0 {
					self.lower_node(stmt_id, proc_data)?;
				}

				// goto start
				self.emit(Tac::Label(cond_label));

				// if !cond goto end
				let Some(cond) = self.lower_node(cond_id, proc_data)? else {
					return Ok(None);
				};
				self.emit(Tac::JumpIf { cond, target: loop_label });

				Ok(None)
			}

			Kind::For(
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

				self.lower_for_loop(proc_data, id, iter_vars, table_id, bounds, body_block)?;

				Ok(None)
			}

			Kind::Call(_proc_id, _exprs) => {
				todo!("lower proc-call")
			}

			#[cfg(feature="ready")]
			Kind::Access(base_id, segments) => {
				let temp = Location::Temp(self.alloc_temp());
				let address = if let Some(record) = proc_data.records.get(base_id) {
					record.address.unwrap_or_else(|| panic!("no address for record '{}'", proc_data.text(base_id)))
				} else if let Some(table) = proc_data.tables.get(base_id) {
					table.address.unwrap_or_else(|| panic!("no address for table '{}'", proc_data.text(base_id)))
				} else {
					panic!("'{}' is not a Record or Table", proc_data.text(base_id));
				};

				self.emit(Tac::Copy {
					src: Location::Constant(address),
					dst: temp.clone(),
				});

				let mut curr_ident = *base_id;
				let mut i = 0;
				while i < segments.len() {
					match segments[i] {
						PathSegment::Field(field_id) => {
							let Some(record) = proc_data.records.get(&curr_ident) else {
								panic!("no record '{}'", proc_data.text(&curr_ident));
							};
							let Some(field_offset) = record.field_offset(proc_data, field_id) else {
								panic!("no field '{}' in record '{}'", proc_data.text(&field_id), proc_data.text(&curr_ident));
							};
							curr_ident = *match record.fields.iter().find(|(id,_)| field_id == *id) {
								Some((_, field_type)) => match field_type {
									crate::Type::Record(id) => Ok(id),
									crate::Type::Table(id) => Ok(id),
									_ => Err(format!("expected a record or table type, found '{}'", proc_data.type_text(*field_type))),
								}
								None => unreachable!(),
							}?;

							self.emit(Tac::BinOp {
								op: BinaryOp::Add,
								left: Location::Constant(field_offset),
								right: temp.clone(),
								dst: temp.clone(),
							});

							i += 1;
						}

						PathSegment::Index(expr_id, field_id) => {
							let Some(expr) = self.lower_node(&proc_data.ast_nodes[expr_id], proc_data)? else {
								return Ok(None);
							};

							let table = &proc_data.tables[&curr_ident];
							let column_offset = table.column_offset(proc_data, field_id)
								.unwrap_or_else(|| panic!("no field '{}' in table '{}'", proc_data.text(&field_id), proc_data.text(&curr_ident)));
							curr_ident = *match table.column_spec.iter().find(|(id,_)| field_id == *id) {
								Some((_, field_type)) => match field_type {
									crate::Type::Record(id) => Ok(id),
									crate::Type::Table(id) => Ok(id),
									_ => Err(format!("expected a record or table type, found '{}'", proc_data.type_text(*field_type))),
								}
								None => unreachable!(),
							}?;

							self.emit(Tac::BinOp {
								op: BinaryOp::Add,
								left: Location::Constant(column_offset),
								right: temp.clone(),
								dst: temp.clone(),
							});

							// NOTE - srenshaw - We can unwrap here because we already checked for field
							// existence to get the column-offset.
							let field_size = table.column_spec.iter()
								.find(|(id,_)| field_id == *id)
								.map(|(_, field_type)| proc_data.type_size(field_type))
								.unwrap();

							// t0 = expr * field_size
							let t0 = self.alloc_temp();
							self.emit(Tac::BinOp {
								op: BinaryOp::Mul,
								left: Location::Constant(field_size),
								right: expr,
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
		proc_data: &ProcData,
		ast_id: &AstId,
		iter_vars: &[IdentId],
		table_id: &Option<IdentId>,
		bounds: &Option<Bounds>,
		body_block: &ABlock,
	) -> Result<(), Error> {
		// Simple case: for i in 0..N
		if iter_vars.len() == 1 && table_id.is_none() {
			let index_var = iter_vars[0];

			let start_label = self.alloc_label();
			let end_label = self.alloc_label();

			let Some(Bounds::Full { start, end }) = bounds else {
				return Err(Error::missing_loop_bounds(self.name, *ast_id));
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
				op: BinaryOp::CmpLT,
				left: Location::Constant(*end),
				right: Location::Variable(index_var),
				dst: Location::Temp(temp),
			});

			self.emit(Tac::JumpIfNot {
				cond: Location::Temp(temp),
				target: end_label,
			});

			// body
			for stmt_id in &body_block.0 {
				self.lower_node(stmt_id, proc_data)?;
			}

			// i = i + 1
			self.emit(Tac::BinOp {
				op: BinaryOp::Add,
				left: Location::Constant(1),
				right: Location::Variable(index_var),
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

enum ErrorKind {
	MissingAstNode,
	MissingLoopBounds,
}

struct Error {
	kind: ErrorKind,
	proc_id: IdentId,
	ast_id: AstId,
}

impl Error {
	fn missing_ast_node(
		proc_id: IdentId,
		ast_id: AstId,
	) -> Self {
		Self {
			kind: ErrorKind::MissingAstNode,
			proc_id,
			ast_id,
		}
	}

	fn missing_loop_bounds(
		proc_id: IdentId,
		ast_id: AstId,
	) -> Self {
		Self {
			kind: ErrorKind::MissingLoopBounds,
			proc_id,
			ast_id,
		}
	}

	fn into_comp_error(self, db: &Data) -> crate::Error {
		let proc_data = &db.proc_db[&self.proc_id];
		let ast_pos = proc_data.ast_pos_tok[self.ast_id];
		let start = db.tok_pos[ast_pos.start];
		let end = db.tok_pos[ast_pos.end];
		let location = crate::Span { start, end };

		let message = match self.kind {
			ErrorKind::MissingAstNode => {
				format!("{} not found in procedure '{}'", self.ast_id, db.text(&self.proc_id))
			}
			ErrorKind::MissingLoopBounds => {
				format!("type-checker missed a bounds check in {}", db.text(&self.proc_id))
			}
		};

		crate::Error::new(location, message)
			.with_kind(error::Kind::LoweringTAC)
	}
}

#[cfg(test)]
mod tests {
	use crate::{lexer, discovery, parser, type_checker};
	use crate::identifier::Identifier;
	use crate::Data;
	use crate::rings_type::Type;
	use super::*;

	fn setup(source: &str) -> Data {
		let mut data = Data::new("lowering".into(), source.into());
		lexer::eval(&mut data);
		discovery::eval(&mut data);
		parser::eval(&mut data);
		type_checker::eval(&mut data);
		eval(&mut data)
				.map_err(|e| panic!("{e:?}"))
				.unwrap();
		data
	}

	#[test]
	fn return_void() {
		let data = setup("proc a() {
			return;
		}");
		assert!(data.errors.is_empty(), "{}", data.errors_to_string());
		assert_eq!(data.proc_db.len(), 1);
		let section = data.proc_db[&"a".id()].tac_data
			.as_ref()
			.expect("existing tac-data");
		assert_eq!(section.instructions, [
			Tac::Return(None),
		]);
	}

	#[test]
	fn return_expression() {
		let data = setup("proc a() -> s8 {
			return 100 - 200;
		}");
		assert!(data.errors.is_empty(), "{}", data.errors_to_string());
		assert_eq!(data.proc_db.len(), 1);
		let section = data.proc_db[&"a".id()].tac_data
			.as_ref()
			.expect("existing tac-data");
		assert_eq!(section.instructions, [
			Tac::BinOp {
				op: BinaryOp::Sub,
				left: Location::Constant(100),
				right: Location::Constant(200),
				dst: Location::Temp(0),
			},
			Tac::Return(Some(Location::Temp(0))),
		]);
	}

	#[test]
	fn proc_if() {
		let data = setup("proc a() -> s8 {
			let b: s8 = 5;
			let c: s8 = 3;
			if b < 10 {
				c = 2;
			} else {
				b = 1;
			}
			return b + c;
		}");
		assert!(data.errors.is_empty(), "{}", data.errors_to_string());
		let proc_data = &data.proc_db[&"a".id()];
		let tac = proc_data.tac_data
			.as_ref()
			.expect("existing tac-data");
		assert_eq!(tac.locals, [
			("b".id(), Type::s8_top()),
			("c".id(), Type::s8_top()),
		]);
		assert_eq!(tac.instructions, [
			Tac::Copy { src: Location::Constant(5), dst: Location::Variable("b".id()) },
			Tac::Copy { src: Location::Constant(3), dst: Location::Variable("c".id()) },
			Tac::BinOp { op: BinaryOp::CmpLT, left: Location::Constant(10), right: Location::Variable("b".id()), dst: Location::Temp(0) },
			Tac::JumpIfNot { cond: Location::Temp(0), target: 0 },
			Tac::Copy { src: Location::Constant(2), dst: Location::Variable("c".id()) },
			Tac::Jump(1),
			Tac::Label(0),
			Tac::Copy { src: Location::Constant(1), dst: Location::Variable("b".id()) },
			Tac::Label(1),
			Tac::BinOp { op: BinaryOp::Add, left: Location::Variable("b".id()), right: Location::Variable("c".id()), dst: Location::Temp(1) },
			Tac::Return(Some(Location::Temp(1))),
		]);
	}

	#[test]
	fn proc_while() {
		let data = setup("proc a() {
			let b: s8 = 5;
			while b > 0 {
				b -= 1;
			}
		}");
		assert!(data.errors.is_empty(), "{}", data.errors_to_string());
		let proc_data = &data.proc_db[&"a".id()];
		let tac = proc_data.tac_data.as_ref()
			.expect("existing tac-data");
		assert_eq!(tac.locals, [
			("b".id(), Type::s8_top()),
		]);
		assert_eq!(tac.instructions, [
			Tac::Copy { src: Location::Constant(5), dst: Location::Variable("b".id()) },
			Tac::Jump(0),
			Tac::Label(1),
			Tac::BinOp { op: BinaryOp::Sub, left: Location::Constant(1), right: Location::Variable("b".id()), dst: Location::Temp(0) },
			Tac::Copy { src: Location::Temp(0), dst: Location::Variable("b".id()) },
			Tac::Label(0),
			Tac::BinOp { op: BinaryOp::CmpGT, left: Location::Constant(0), right: Location::Variable("b".id()), dst: Location::Temp(1) },
			Tac::JumpIf { cond: Location::Temp(1), target: 1 },
			Tac::Return(None),
		]);
	}

	#[test]
	fn proc_for() {
		let data = setup("main {
			let b: s8 = 4;
			let c: s8 = 0;
			for i in [0..10] {
				c += b * 2;
			}
		}");
		assert!(data.errors.is_empty(), "{}", data.errors_to_string());
		let proc_data = &data.proc_db[&"main".id()];
		let tac = proc_data.tac_data.as_ref()
			.expect("existing tac-data");
		assert_eq!(tac.locals, [
			("b".id(), Type::s8_top()),
			("c".id(), Type::s8_top()),
		]);
		assert_eq!(tac.instructions, [
			Tac::Copy {
				src: Location::Constant(4),
				dst: Location::Variable("b".id()),
			},
			Tac::Copy {
				src: Location::Constant(0),
				dst: Location::Variable("c".id()),
			},
			// Loop head
			Tac::Copy {
				src: Location::Constant(0),
				dst: Location::Variable("i".id()),
			},
			Tac::Label(0),
			Tac::BinOp {
				op: BinaryOp::CmpLT,
				left: Location::Constant(10),
				right: Location::Variable("i".id()),
				dst: Location::Temp(0),
			},
			Tac::JumpIfNot {
				cond: Location::Temp(0),
				target: 1,
			},
			// Loop body
			Tac::BinOp {
				op: BinaryOp::Mul,
				left: Location::Constant(2),
				right: Location::Variable("b".id()),
				dst: Location::Temp(1),
			},
			Tac::BinOp {
				op: BinaryOp::Add,
				left: Location::Variable("c".id()),
				right: Location::Temp(1),
				dst: Location::Temp(2),
			},
			Tac::Copy {
				src: Location::Temp(2),
				dst: Location::Variable("c".id()),
			},
			Tac::BinOp {
				op: BinaryOp::Add,
				left: Location::Constant(1),
				right: Location::Variable("i".id()),
				dst: Location::Variable("i".id()),
			},
			Tac::Jump(0),
			// Loop end
			Tac::Label(1),
			Tac::Return(None),
		]);
	}
}

