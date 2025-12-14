
use crate::ast::{Block as ABlock, Id as AstId, Kind};
use crate::error;
use crate::identifier::Id as IdentId;
use crate::operators::{BinaryOp, UnaryOp};
use crate::{Bounds, Data, ProcData};

pub type LabelId = u32;
pub type TempId = u32; // Temporary variable ID

pub fn eval(data: &mut Data) -> Result<(), crate::Error> {
	let mut result = Ok(());
	for (proc_id, proc_data) in &mut data.proc_db {
		let mut section = Section::default();
		section.name = *proc_id;
		match section.lower(proc_data) {
			Ok(_) => {
				proc_data.tac_data = Some(section);
			}
			Err(e) => {
				result = Err(e.into_comp_error(data));
				break;
			}
		}
	}
	result
}

#[derive(Debug, Clone, PartialEq)]
pub enum Tac {
	// Basic operations
	/// Pushes a value on the stack
	Push(i32),

	// Arithmetic
	/// Pop x2, perform op, and push
	BinOp(BinaryOp),
	/// Pop, perform op, and push
	UnOp(UnaryOp),

	// Memory access (for table/variable access)
	/// Load the value in stack-offset `usize` and push
	Load(usize),
	/// Pop and store the value in stack-offset `usize`
	Store(usize),

	// Control Flow
	Label(LabelId),
	/// Jump unconditionally to LabelId
	Jump(LabelId),
	/// Pop stack and jump to LabelId if > 0
	JumpIf(LabelId),

	// Procedure related
	#[cfg(feature="ready")]
	Call { name: IdentId, args: Vec<Location>, dst: Option<TempId> },
	/// Returns with output in stack-offset 0 if needed
	Return(bool),

	// Comments/debug
	#[cfg(feature="ready")]
	Comment(String),
}

impl Tac {
	pub fn to_text(&self, _data: &Data) -> String {
		match self {
			Tac::Push(value) => {
				format!("PUSH  {value}")
			}
			Tac::BinOp(op) => {
				format!("BINOP {op}")
			}
			Tac::UnOp(op) => {
				format!("UNOP  {op}")
			}
			Tac::Load(offset) => {
				format!("LOAD  {offset}")
			}
			Tac::Store(offset) => {
				format!("STORE {offset}")
			}
			Tac::Label(label_id) => {
				format!("label_{label_id}:")
			}
			Tac::Jump(label_id) => {
				format!("JMP   label_{label_id}")
			}
			Tac::JumpIf(target) => {
				format!("JIF   label_{target}")
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
			Tac::Return(with_value) => {
				format!("RET   {with_value}")
			}
			#[cfg(feature="ready")]
			Tac::Comment(msg) => {
				format!("; {msg}")
			}
		}
	}
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Section {
	pub name: IdentId,
	pub locals: Vec<(IdentId, crate::Type)>,
	//pub variables: HashMap<IdentId, usize>,
	pub stack: Vec<i32>,
	pub instructions: Vec<Tac>,
	pub next_temp: TempId,
	pub next_label: LabelId,
}

impl Section {
	fn lower(&mut self, proc_data: &ProcData) -> Result<(), Error> {
		let proc_start = proc_data.ast_start;

		if let Err(err) = self.lower_node(&proc_start, proc_data) {
			//let range = &proc_data.ast_pos_tok[proc_start];
			//error::error(proc_data, &err_msg, range.start);
			return Err(err);
		}

		Ok(())
	}

	fn lower_node(&mut self, id: &AstId, proc_data: &ProcData,
	) -> Result<Option<IdentId>, Error> {
		let Some(kind) = proc_data.ast_nodes.get(*id) else {
			return Err(Error::missing_ast_node(self.name, *id));
		};

		match kind {
			Kind::Int(value) => {
				// TODO - srenshaw - Ensure value is within u32
				self.emit(Tac::Push(*value as i32));
				Ok(None)
			}

			Kind::Dec(_value) => {
				todo!()
			}

			Kind::Ident(ident_id) => {
				let idx = self.locals.iter()
						.position(|(local_id,_)| local_id == ident_id)
						.unwrap();
				self.emit(Tac::Load(idx));
				Ok(Some(*ident_id))
			},

			Kind::Define(var_id, type_kind) => {
				let Kind::Ident(ident_id) = &proc_data.ast_nodes[*var_id] else {
					return Err(Error::missing_ast_node(self.name, *var_id));
				};

				// Add to locals
				self.locals.push((*ident_id, *type_kind));
				Ok(Some(*ident_id))
			}

			Kind::Assign(var_id, expr_id) => {
				let Some(ident_id) = self.lower_node(var_id, proc_data)? else {
					return Err(Error::missing_ast_node(self.name, *var_id));
				};
				if matches!(self.instructions.last(), Some(Tac::Load(_))) {
					self.instructions.pop();
				}

				self.lower_node(expr_id, proc_data)?;

				let idx = self.locals.iter()
						.position(|(local_id,_)| local_id == &ident_id)
						.unwrap();
				self.emit(Tac::Store(idx));

				Ok(None)
			}

			Kind::BinOp(op, left_id, right_id) => {
				self.lower_node(left_id, proc_data)?;
				self.lower_node(right_id, proc_data)?;
				self.emit(Tac::BinOp(*op));
				Ok(None)
			}

			Kind::UnOp(op, right_id) => {
				self.lower_node(right_id, proc_data)?;
				self.emit(Tac::UnOp(*op));
				Ok(None)
			}

			Kind::Return(maybe_expr) => {
				if let Some(expr_id) = maybe_expr {
					self.lower_node(expr_id, proc_data)?;
					self.emit(Tac::Return(true));
				} else {
					self.emit(Tac::Return(false));
				}
				Ok(None)
			}

			Kind::Block(stmts) => {
				for stmt_id in &stmts.0 {
					self.lower_node(stmt_id, proc_data)?;
				}
				Ok(None)
			}

			Kind::If(cond_id, then_block, else_block) => {
				self.lower_node(cond_id, proc_data)?;

				let else_label = self.alloc_label();
				let end_label = self.alloc_label();

				// if !cond goto else
				self.emit(Tac::UnOp(UnaryOp::Not));
				self.emit(Tac::JumpIf(else_label));

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

				// goto check
				self.emit(Tac::Jump(cond_label));

				// loop:
				self.emit(Tac::Label(loop_label));

				// body
				for stmt_id in &body_block.0 {
					self.lower_node(stmt_id, proc_data)?;
				}

				// check:
				self.emit(Tac::Label(cond_label));

				// if cond goto loop
				self.lower_node(cond_id, proc_data)?;
				self.emit(Tac::JumpIf(loop_label));

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
			let index_id = iter_vars[0];

			let start_label = self.alloc_label();
			let end_label = self.alloc_label();

			let Some(Bounds::Full { start, end }) = bounds else {
				return Err(Error::missing_loop_bounds(self.name, *ast_id));
			};

			self.locals.push((index_id, crate::Type::Int));
			let index_idx = self.locals.len() - 1;

			// i = start
			self.emit(Tac::Push(*start as i32));
			self.emit(Tac::Store(index_idx));

			// start:
			self.emit(Tac::Label(start_label));

			// if i >= end goto end
			self.emit(Tac::Load(index_idx));
			self.emit(Tac::Push(*end as i32));
			self.emit(Tac::BinOp(BinaryOp::CmpGE));
			self.emit(Tac::JumpIf(end_label));

			// body
			for stmt_id in &body_block.0 {
				self.lower_node(stmt_id, proc_data)?;
			}

			// i = i + 1
			self.emit(Tac::Load(index_idx));
			self.emit(Tac::Push(1));
			self.emit(Tac::BinOp(BinaryOp::Add));
			self.emit(Tac::Store(index_idx));

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
			Tac::Return(false),
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
			Tac::Push(100),
			Tac::Push(200),
			Tac::BinOp(BinaryOp::Sub),
			Tac::Return(true),
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
			Tac::Push(5),
			Tac::Store(0),
			Tac::Push(3),
			Tac::Store(1),
			Tac::Load(0),
			Tac::Push(10),
			Tac::BinOp(BinaryOp::CmpLT),
			Tac::UnOp(UnaryOp::Not),
			Tac::JumpIf(0),
			Tac::Push(2),
			Tac::Store(1),
			Tac::Jump(1),
			Tac::Label(0),
			Tac::Push(1),
			Tac::Store(0),
			Tac::Label(1),
			Tac::Load(0),
			Tac::Load(1),
			Tac::BinOp(BinaryOp::Add),
			Tac::Return(true),
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
			Tac::Push(5),
			Tac::Store(0),
			Tac::Jump(0),
			Tac::Label(1),
			Tac::Load(0),
			Tac::Push(1),
			Tac::BinOp(BinaryOp::Sub),
			Tac::Store(0),
			Tac::Label(0),
			Tac::Load(0),
			Tac::Push(0),
			Tac::BinOp(BinaryOp::CmpGT),
			Tac::JumpIf(1),
			Tac::Return(false),
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
			("i".id(), Type::int()),
		]);
		assert_eq!(tac.instructions, [
			Tac::Push(4),
			Tac::Store(0),
			Tac::Push(0),
			Tac::Store(1),
			// Loop head
			Tac::Push(0),
			Tac::Store(2),
			Tac::Label(0),
			Tac::Load(2),
			Tac::Push(10),
			Tac::BinOp(BinaryOp::CmpGE),
			Tac::JumpIf(1),
			// Loop body
			Tac::Load(1),
			Tac::Load(0),
			Tac::Push(2),
			Tac::BinOp(BinaryOp::Mul),
			Tac::BinOp(BinaryOp::Add),
			Tac::Store(1),
			Tac::Load(2),
			Tac::Push(1),
			Tac::BinOp(BinaryOp::Add),
			Tac::Store(2),
			Tac::Jump(0),
			// Loop end
			Tac::Label(1),
			Tac::Return(false),
		]);
	}

	#[test]
	fn proc_internal_sub_expressions() {
		let db = setup("main {
			let a: s8 = (2 + 3) * (4 - 5);
		}");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		let proc_data = &db.proc_db[&"main".id()];
		let tac = proc_data.tac_data.as_ref()
			.expect("existing tac-data");
		assert_eq!(tac.locals, [
			("a".id(), Type::s8_top()),
		]);
		assert_eq!(tac.instructions, [
			Tac::Push(2),
			Tac::Push(3),
			Tac::BinOp(BinaryOp::Add),
			Tac::Push(4),
			Tac::Push(5),
			Tac::BinOp(BinaryOp::Sub),
			Tac::BinOp(BinaryOp::Mul),
			Tac::Store(0),
			Tac::Return(false),
		]);
	}
}

