
use std::collections::HashMap;
use crate::ast::{Block as AstBlock, Id as AstId, Kind};
use crate::discovery::Data as DscData;
use crate::identifier::{Id as IdentId, Identifier, Map as IdentMap};
use crate::rings_type::Type;
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::operators::{BinaryOp, UnaryOp};
use crate::parser::ProcData;
use crate::{
	error,
	text,
	Bounds, Span, SrcPos, Target,
};

pub type LabelId = u32;
pub type TempId = u32; // Temporary variable ID

pub fn print(
	section_db: &IdentMap<Section>,
	input: &InputData,
	lex_data: &LexData,
) {
	if section_db.is_empty() {
		return;
	}

	for (proc_id, section) in section_db.iter() {
		let proc_name = text(input, lex_data, proc_id);

		println!();
		println!("{:<32} | {:<16} | LOCAL-TYPE",
			"TAC-LOCALS", "LOCAL-NAME");
		println!("{:-<32} | {:-<16} | {:-<16}", "", "", "");
		for (local_id, ring_type) in &section.locals {
			let local_name = text(input, lex_data, local_id);
			let local_type = crate::type_text(input, lex_data, ring_type);
			println!("{proc_name:<32} | {local_name:<16} | {local_type:<16}");
		}

		println!();
		println!("{:<32} | INSTRUCTIONS",
			"TAC-SECTIONS");
		println!("{:-<32} | {:-<16}", "", "");
		for tac in &section.instructions {
			println!("{proc_name:<32} | {}", tac.to_text(&input.source, &lex_data.identifiers));
		}
	}
}

pub fn place_records(
	input: &InputData,
	lex_data: &LexData,
	dsc_data: &mut DscData,
) -> IdentMap<u32> {
	let mut out = IdentMap::with_capacity(dsc_data.records.len());

	for (rec_id, rec_data) in &dsc_data.records {
		let Some(reg_id) = rec_data.region else {
			// Skip records without static locations
			continue;
		};
		let Some(region) = dsc_data.regions.get_mut(&reg_id) else {
			panic!();
		};
		let allocation = region.alloc_position + rec_data.size();
		if allocation > region.span.end {
			panic!();
		}

		for param in &rec_data.fields {
			let rec_name = text(input, lex_data, &rec_id);
			let p_name = text(input, lex_data, &param.name);
			let location = format!("{rec_name}_{p_name}");
			out.insert(location.id(), region.alloc_position);
			region.alloc_position += crate::type_size(&dsc_data.records, &param.typ);
		}
	}

	out
}

pub fn eval(
	input: &InputData,
	lex_data: &LexData,
	proc_db: &IdentMap<ProcData>,
	dsc_data: &mut DscData,
) -> Result<(IdentMap<Section>, IdentMap<u32>), Error> {
	let records = place_records(input, lex_data, dsc_data);
	let mut out = IdentMap::<Section>::with_capacity(proc_db.len());

	for (proc_id, proc_data) in proc_db {
		let mut section = Section {
			name: *proc_id,
			target: proc_data.target.unwrap_or(Target::SH2),
			locals: vec![],
			stack: vec![],
			instructions: vec![],
			next_temp: 0,
			next_label: 0,
		};

		let proc_start = proc_data.ast_start;

		section.lower_node(&proc_start, proc_data, &records)?;
		out.insert(*proc_id, section);
	}

	Ok((out, records))
}

/// Virtual Stack-Machine Code
///
/// This is the debug output for now. Output targets will act as a stack-machine regardless of the
/// actual architecture.
#[derive(Debug, Clone, PartialEq)]
pub enum Vsmc {
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
	#[cfg(feature="call")]
	Call { name: IdentId, args: Vec<Location>, dst: Option<TempId> },
	/// Returns with output in stack-offset 0 if needed
	Return(bool),
}

impl Vsmc {
	pub fn to_text(&self, _source: &str, _identifiers: &IdentMap<Span<SrcPos>>) -> String {
		match self {
			Vsmc::Push(value) => {
				format!("PUSH  {value}")
			}
			Vsmc::BinOp(op) => {
				format!("BINOP {op}")
			}
			Vsmc::UnOp(op) => {
				format!("UNOP  {op}")
			}
			Vsmc::Load(offset) => {
				format!("LOAD  {offset}")
			}
			Vsmc::Store(offset) => {
				format!("STORE {offset}")
			}
			Vsmc::Label(label_id) => {
				format!("label_{label_id}:")
			}
			Vsmc::Jump(label_id) => {
				format!("JMP   label_{label_id}")
			}
			Vsmc::JumpIf(target) => {
				format!("JIF   label_{target}")
			}
			#[cfg(feature="call")]
			Vsmc::Call { name, args, dst: Some(dst) } => {
				format!("CALL  {}({}) -> {dst}", text(source, identifiers, name),
					args.iter().map(|loc| loc.to_text(data)).collect::<Vec<_>>().join(","))
			}
			#[cfg(feature="call")]
			Vsmc::Call { name, args, ..} => {
				format!("CALL  {}({})", text(source, identifiers, name),
					args.iter().map(|loc| loc.to_text(data)).collect::<Vec<_>>().join(","))
			}
			Vsmc::Return(with_value) => {
				format!("RET   {with_value}")
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Section {
	pub name: IdentId,
	pub target: Target,
	pub locals: Vec<(IdentId, Type)>,
	//pub variables: HashMap<IdentId, usize>,
	pub stack: Vec<i32>,
	pub instructions: Vec<Vsmc>,
	pub next_temp: TempId,
	pub next_label: LabelId,
}

impl Section {
	fn lower_node(&mut self,
		id: &AstId,
		proc_data: &ProcData,
		records: &HashMap<IdentId, u32>,
	) -> Result<Option<IdentId>, Error> {
		let Some(kind) = proc_data.ast_nodes.get(*id) else {
			return Err(Error::missing_ast_node(self.name, *id));
		};

		match kind {
			Kind::Int(value) => {
				// TODO - srenshaw - Ensure value is within u32
				self.emit(Vsmc::Push(*value as i32));
				Ok(None)
			}

			Kind::Dec(_value) => {
				todo!()
			}

			Kind::Ident(ident_id) => {
				if let Some(idx) = self.locals.iter()
						.position(|(local_id,_)| local_id == ident_id)
				{
					self.emit(Vsmc::Load(idx));
					Ok(Some(*ident_id))
				} else {
					Err(Error::unknown_ident(self.name, *ident_id))
				}
			}

			Kind::Define(var_id, type_kind) => {
				let Kind::Ident(ident_id) = &proc_data.ast_nodes[*var_id] else {
					return Err(Error::missing_ast_node(self.name, *var_id));
				};

				// Add to locals
				self.locals.push((*ident_id, *type_kind));
				Ok(Some(*ident_id))
			}

			Kind::Assign(var_id, expr_id) => {
				let Some(ident_id) = self.lower_node(var_id, proc_data, records)? else {
					return Err(Error::missing_ast_node(self.name, *var_id));
				};
				if matches!(self.instructions.last(), Some(Vsmc::Load(_))) {
					self.instructions.pop();
				}

				self.lower_node(expr_id, proc_data, records)?;

				let idx = self.locals.iter()
						.position(|(local_id,_)| local_id == &ident_id)
						.unwrap();
				self.emit(Vsmc::Store(idx));

				Ok(None)
			}

			Kind::BinOp(op, left_id, right_id) => {
				self.lower_node(left_id, proc_data, records)?;
				self.lower_node(right_id, proc_data, records)?;
				self.emit(Vsmc::BinOp(*op));
				Ok(None)
			}

			Kind::UnOp(op, right_id) => {
				self.lower_node(right_id, proc_data, records)?;
				self.emit(Vsmc::UnOp(*op));
				Ok(None)
			}

			Kind::Return(maybe_expr) => {
				if let Some(expr_id) = maybe_expr {
					self.lower_node(expr_id, proc_data, records)?;
					self.emit(Vsmc::Return(true));
				} else {
					self.emit(Vsmc::Return(false));
				}
				Ok(None)
			}

			Kind::Block(stmts) => {
				for stmt_id in &stmts.0 {
					self.lower_node(stmt_id, proc_data, records)?;
				}
				Ok(None)
			}

			Kind::If(cond_id, then_block, else_block) => {
				self.lower_node(cond_id, proc_data, records)?;

				let else_label = self.alloc_label();
				let end_label = self.alloc_label();

				// if !cond goto else
				self.emit(Vsmc::UnOp(UnaryOp::Not));
				self.emit(Vsmc::JumpIf(else_label));

				// then block
				for stmt_id in &then_block.0 {
					self.lower_node(stmt_id, proc_data, records)?;
				}
				self.emit(Vsmc::Jump(end_label));

				// else block
				self.emit(Vsmc::Label(else_label));
				for stmt_id in &else_block.0 {
					self.lower_node(stmt_id, proc_data, records)?;
				}

				// end
				self.emit(Vsmc::Label(end_label));
				Ok(None)
			}

			Kind::While(cond_id, body_block) => {
				let cond_label = self.alloc_label();
				let loop_label = self.alloc_label();

				// goto check
				self.emit(Vsmc::Jump(cond_label));

				// loop:
				self.emit(Vsmc::Label(loop_label));

				// body
				for stmt_id in &body_block.0 {
					self.lower_node(stmt_id, proc_data, records)?;
				}

				// check:
				self.emit(Vsmc::Label(cond_label));

				// if cond goto loop
				self.lower_node(cond_id, proc_data, records)?;
				self.emit(Vsmc::JumpIf(loop_label));

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

				self.lower_for_loop(proc_data, records, id, iter_vars, table_id, bounds, body_block)?;

				Ok(None)
			}

			Kind::Call(_proc_id, _exprs) => {
				todo!("lower proc-call")
			}

			#[cfg(feature="access")]
			Kind::Access(base_id, segments) => {
				let temp = Location::Temp(self.alloc_temp());
				let address = if let Some(record) = proc_data.records.get(base_id) {
					record.address.unwrap_or_else(|| panic!("no address for record '{}'", proc_data.text(base_id)))
				} else if let Some(table) = proc_data.tables.get(base_id) {
					table.address.unwrap_or_else(|| panic!("no address for table '{}'", proc_data.text(base_id)))
				} else {
					panic!("'{}' is not a Record or Table", proc_data.text(base_id));
				};

				self.emit(Vsmc::Copy {
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

							self.emit(Vsmc::BinOp {
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

							self.emit(Vsmc::BinOp {
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
							self.emit(Vsmc::BinOp {
								op: BinaryOp::Mul,
								left: Location::Constant(field_size),
								right: expr,
								dst: Location::Temp(t0),
							});

							// temp = temp + t0
							self.emit(Vsmc::BinOp {
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
		records: &HashMap<IdentId, u32>,
		ast_id: &AstId,
		iter_vars: &[IdentId],
		table_id: &Option<IdentId>,
		bounds: &Option<Bounds>,
		body_block: &AstBlock,
	) -> Result<(), Error> {
		// Simple case: for i in 0..N
		if iter_vars.len() == 1 && table_id.is_none() {
			let index_id = iter_vars[0];

			let start_label = self.alloc_label();
			let end_label = self.alloc_label();

			let Some(Bounds::Full { start, end }) = bounds else {
				return Err(Error::missing_loop_bounds(self.name, *ast_id));
			};

			self.locals.push((index_id, Type::Int));
			let index_idx = self.locals.len() - 1;

			// i = start
			self.emit(Vsmc::Push(*start as i32));
			self.emit(Vsmc::Store(index_idx));

			// start:
			self.emit(Vsmc::Label(start_label));

			// if i >= end goto end
			self.emit(Vsmc::Load(index_idx));
			self.emit(Vsmc::Push(*end as i32));
			self.emit(Vsmc::BinOp(BinaryOp::CmpGE));
			self.emit(Vsmc::JumpIf(end_label));

			// body
			for stmt_id in &body_block.0 {
				self.lower_node(stmt_id, proc_data, records)?;
			}

			// i = i + 1
			self.emit(Vsmc::Load(index_idx));
			self.emit(Vsmc::Push(1));
			self.emit(Vsmc::BinOp(BinaryOp::Add));
			self.emit(Vsmc::Store(index_idx));

			// goto start
			self.emit(Vsmc::Jump(start_label));

			// end:
			self.emit(Vsmc::Label(end_label));
		}

		// TODO - srenshaw - Handle table iteration, multiple variables, etc.
		Ok(())
	}

	fn emit(&mut self, instr: Vsmc) {
		self.instructions.push(instr);
	}

	pub fn alloc_label(&mut self) -> LabelId {
		self.next_label += 1;
		self.next_label - 1
	}
}

enum ErrorKind {
	MissingAstNode(AstId),
	MissingLoopBounds(AstId),
	UnknownIdentifier(IdentId),
}

pub struct Error {
	kind: ErrorKind,
	proc_id: IdentId,
}

impl Error {
	fn missing_ast_node(
		proc_id: IdentId,
		ast_id: AstId,
	) -> Self {
		Self {
			kind: ErrorKind::MissingAstNode(ast_id),
			proc_id,
		}
	}

	fn missing_loop_bounds(
		proc_id: IdentId,
		ast_id: AstId,
	) -> Self {
		Self {
			kind: ErrorKind::MissingLoopBounds(ast_id),
			proc_id,
		}
	}

	fn unknown_ident(
		proc_id: IdentId,
		ident_id: IdentId,
	) -> Self {
		Self {
			kind: ErrorKind::UnknownIdentifier(ident_id),
			proc_id,
		}
	}

	pub fn into_comp_error(self,
		input: &InputData,
		lex_data: &LexData,
		proc_db: &IdentMap<ProcData>,
	) -> error::Error {
		let proc_data = &proc_db[&self.proc_id];

		let (location, message) = match self.kind {
			ErrorKind::MissingAstNode(ast_id) => {
				let ast_pos = proc_data.ast_pos_tok[ast_id];
				let start = lex_data.tok_pos[ast_pos.start];
				let end = lex_data.tok_pos[ast_pos.end];
				let location = Span { start, end };
				let message = format!("{} not found in procedure '{}'", ast_id, text(input, lex_data, &self.proc_id));
				(location, message)
			}
			ErrorKind::MissingLoopBounds(ast_id) => {
				let ast_pos = proc_data.ast_pos_tok[ast_id];
				let start = lex_data.tok_pos[ast_pos.start];
				let end = lex_data.tok_pos[ast_pos.end];
				let location = Span { start, end };
				let message = format!("type-checker missed a bounds check in {}", text(input, lex_data, &self.proc_id));
				(location, message)
			}
			ErrorKind::UnknownIdentifier(ident_id) => {
				let location = lex_data.identifiers[&ident_id];
				let message = format!("Unknown identifier '{}' in procedure '{}'",
					text(input, lex_data, &ident_id), text(input, lex_data, &self.proc_id));
				(location, message)
			}
		};

		error::Error::new(location, message)
			.with_kind(error::Kind::LoweringVSMC)
	}
}

#[cfg(test)]
mod tests {
	use crate::{input, lexer, discovery, parser, type_checker};
	use crate::discovery::RegionMap;
	use crate::identifier::Identifier;
	use crate::rings_type::Type;

	use super::*;

	fn setup(source: &str) -> (IdentMap<Section>, RegionMap, IdentMap<u32>) {
		let mut source_str = String::new();
		source_str.push_str("region Stack[0] @ 0;");
		source_str.push_str(source);

		let input = input::eval("parser".to_string(), source_str.into());

		let lex_data = lexer::eval(&input.source)
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));

		let (mut dsc_data, task_queue) = discovery::eval(&input, &lex_data)
			.map_err(|e| e.into_comp_error(&input, &lex_data, error::Kind::Discovery))
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));

		let proc_db = parser::eval(&input, &lex_data, &mut dsc_data, task_queue)
			.map_err(|e| e.into_comp_error(&input, &lex_data, error::Kind::Parser))
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));

		let proc_db = type_checker::eval(&input, &lex_data, &dsc_data, proc_db)
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));

		let (sections, records) = eval(&input, &lex_data, &proc_db, &mut dsc_data)
			.map_err(|e| e.into_comp_error(&input, &lex_data, &proc_db))
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));

		(sections, dsc_data.regions, records)
	}

	#[test]
	fn return_void() {
		let (section_db,_,_) = setup("main {}
		proc a() {
			return;
		}");
		assert_eq!(section_db.len(), 2);
		let section = &section_db[&"a".id()];
		assert_eq!(section.instructions, [
			Vsmc::Return(false),
		]);
	}

	#[test]
	fn return_expression() {
		let (section_db,_,_) = setup("main {}
		proc a() -> s8 {
			return 100 - 200;
		}");
		assert_eq!(section_db.len(), 2);
		let section = &section_db[&"a".id()];
		assert_eq!(section.instructions, [
			Vsmc::Push(100),
			Vsmc::Push(200),
			Vsmc::BinOp(BinaryOp::Sub),
			Vsmc::Return(true),
		]);
	}

	#[test]
	fn proc_if() {
		let (section_db,_,_) = setup("main {}
		proc a() -> s8 {
			let b: s8 = 5;
			let c: s8 = 3;
			if b < 10 {
				c = 2;
			} else {
				b = 1;
			}
			return b + c;
		}");
		let section = &section_db[&"a".id()];
		assert_eq!(section.locals, [
			("b".id(), Type::s8_top()),
			("c".id(), Type::s8_top()),
		]);
		assert_eq!(section.instructions, [
			Vsmc::Push(5),
			Vsmc::Store(0),
			Vsmc::Push(3),
			Vsmc::Store(1),
			Vsmc::Load(0),
			Vsmc::Push(10),
			Vsmc::BinOp(BinaryOp::CmpLT),
			Vsmc::UnOp(UnaryOp::Not),
			Vsmc::JumpIf(0),
			Vsmc::Push(2),
			Vsmc::Store(1),
			Vsmc::Jump(1),
			Vsmc::Label(0),
			Vsmc::Push(1),
			Vsmc::Store(0),
			Vsmc::Label(1),
			Vsmc::Load(0),
			Vsmc::Load(1),
			Vsmc::BinOp(BinaryOp::Add),
			Vsmc::Return(true),
		]);
	}

	#[test]
	fn proc_while() {
		let (section_db,_,_) = setup("main {}
		proc a() {
			let b: s8 = 5;
			while b > 0 {
				b -= 1;
			}
		}");
		let section = &section_db[&"a".id()];
		assert_eq!(section.locals, [
			("b".id(), Type::s8_top()),
		]);
		assert_eq!(section.instructions, [
			Vsmc::Push(5),
			Vsmc::Store(0),
			Vsmc::Jump(0),
			Vsmc::Label(1),
			Vsmc::Load(0),
			Vsmc::Push(1),
			Vsmc::BinOp(BinaryOp::Sub),
			Vsmc::Store(0),
			Vsmc::Label(0),
			Vsmc::Load(0),
			Vsmc::Push(0),
			Vsmc::BinOp(BinaryOp::CmpGT),
			Vsmc::JumpIf(1),
			Vsmc::Return(false),
		]);
	}

	#[test]
	fn proc_for() {
		let (section_db,_,_) = setup("main {
			let b: s8 = 4;
			let c: s8 = 0;
			for i in [0..10] {
				c += b * 2;
			}
		}");
		let section = &section_db[&"main".id()];
		assert_eq!(section.locals, [
			("b".id(), Type::s8_top()),
			("c".id(), Type::s8_top()),
			("i".id(), Type::int()),
		]);
		assert_eq!(section.instructions, [
			Vsmc::Push(4),
			Vsmc::Store(0),
			Vsmc::Push(0),
			Vsmc::Store(1),
			// Loop head
			Vsmc::Push(0),
			Vsmc::Store(2),
			Vsmc::Label(0),
			Vsmc::Load(2),
			Vsmc::Push(10),
			Vsmc::BinOp(BinaryOp::CmpGE),
			Vsmc::JumpIf(1),
			// Loop body
			Vsmc::Load(1),
			Vsmc::Load(0),
			Vsmc::Push(2),
			Vsmc::BinOp(BinaryOp::Mul),
			Vsmc::BinOp(BinaryOp::Add),
			Vsmc::Store(1),
			Vsmc::Load(2),
			Vsmc::Push(1),
			Vsmc::BinOp(BinaryOp::Add),
			Vsmc::Store(2),
			Vsmc::Jump(0),
			// Loop end
			Vsmc::Label(1),
			Vsmc::Return(false),
		]);
	}

	#[test]
	fn proc_internal_sub_expressions() {
		let (section_db,_,_) = setup("main {
			let a: s8 = (2 + 3) * (4 - 5);
		}");
		let section = &section_db[&"main".id()];
		assert_eq!(section.locals, [
			("a".id(), Type::s8_top()),
		]);
		assert_eq!(section.instructions, [
			Vsmc::Push(2),
			Vsmc::Push(3),
			Vsmc::BinOp(BinaryOp::Add),
			Vsmc::Push(4),
			Vsmc::Push(5),
			Vsmc::BinOp(BinaryOp::Sub),
			Vsmc::BinOp(BinaryOp::Mul),
			Vsmc::Store(0),
			Vsmc::Return(false),
		]);
	}

	#[test]
	fn static_record_placement() {
		let (_, regions, records) = setup("
		region a[4] @ 0;
		record b @ a {
			c: s8,
			d: s8,
			e: s8,
		}
		main {}
		");
		assert_eq!(regions[&"a".id()].alloc_position, 3);
		assert_eq!(records.get(&"b_c".id()), Some(&0));
		assert_eq!(records.get(&"b_d".id()), Some(&1));
		assert_eq!(records.get(&"b_e".id()), Some(&2));
	}
}

