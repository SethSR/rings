
use crate::identifier::{IdentId, Map as IdentMap};
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::operators::{BinaryOp, UnaryOp};
use crate::packing::Data as PakData;
use crate::parser::{
	AstId,
	AstKind,
	Data as PrsData,
	PathSegment,
	Procedure,
	Type,
	Value,
};
use crate::type_checker::TypedList;
use crate::{
	error,
	SrcPos, Target,
};

pub type LabelId = u32;
pub type VRegId = u32;

pub fn eval(
	prs_data: &PrsData<SrcPos>,
	proc_db: &IdentMap<TypedList>,
	pak_data: &PakData,
	loc_data: &IdentMap<u32>,
) -> Result<IdentMap<Data>, Error> {
	let mut out = IdentMap::<Data>::with_capacity(proc_db.len());

	for (proc_id, typed_body) in proc_db {
		let proc_data = &prs_data.procedures[proc_id];

		let mut tac = TACData::new(typed_body, &prs_data, &pak_data, &loc_data);

		let mut data = Data::new(*proc_id, proc_data, prs_data);

		lower_node(
			(typed_body.len() - 1).into(),
			&mut tac,
			&mut data,
		)?;
		data.next_label = tac.next_label;
		data.next_reg = tac.next_reg;
		out.insert(*proc_id, data);
	}

	Ok(out)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Location {
	Const(i64, Type),
	VReg(VRegId, Type),
	Stack(usize, Type),
	Addr(u32, Type),
}

/// Three Address Code
///
/// Mid to high-level lowering IR. Will be replaced with a graph-based IR for optimizing builds.
#[derive(Debug, Clone, PartialEq)]
pub enum TAC {
	// Move operations
	/// Move data from Location to Location
	Move { src: Location, dst: Location },

	// Arithmetic
	/// dst = lhs \<op\> rhs
	BinOp { op: BinaryOp, lhs: Location, rhs: Location, dst: Location },
	/// vr1 = op(vr0)
	UnOp { op: UnaryOp, typ: Type, vr0: VRegId, vr1: VRegId },

	// Control Flow
	Label(LabelId),
	/// Jump unconditionally to LabelId
	Jump(LabelId),
	/// Jump to LabelId if VRegId != 0
	JumpIf { lbl: LabelId, vr: VRegId },

	// Procedure related
	#[cfg(feature="call")]
	Call { name: IdentId, args: Vec<Location>, dst: Option<TempId> },
	/// Places VRegId into register 0 then returns
	Return(Option<VRegId>),
}

#[derive(Debug)]
struct TACData<'a> {
	typed_body: &'a TypedList,
	local_map: IdentMap<Location>,
	prs_data: &'a PrsData<SrcPos>,
	pak_data: &'a PakData,
	loc_data: &'a IdentMap<u32>,
	next_label: LabelId,
	next_reg: VRegId,
}

impl<'a> TACData<'a> {
	fn new(
		typed_body: &'a TypedList,
		prs_data: &'a PrsData<SrcPos>,
		pak_data: &'a PakData,
		loc_data: &'a IdentMap<u32>,
	) -> Self {
		Self {
			typed_body,
			prs_data,
			pak_data,
			loc_data,
			local_map: IdentMap::default(),
			next_label: 0,
			next_reg: 0,
		}
	}

	fn label(&mut self) -> LabelId {
		self.next_label += 1;
		self.next_label - 1
	}

	fn reg(&mut self) -> VRegId {
		self.next_reg += 1;
		self.next_reg - 1
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Data {
	pub name: IdentId,
	pub target: Target,
	pub locals: Vec<IdentId>,
	pub instructions: Vec<TAC>,
	pub next_label: LabelId,
	pub next_reg: VRegId,
}

impl Data {
	fn new(
		proc_id: IdentId,
		proc_data: &Procedure<SrcPos>,
		prs_data: &PrsData<SrcPos>,
	) -> Self {
		Self {
			name: proc_id,
			target: proc_data.target.unwrap_or(Target::SH2),
			locals: prs_data.types.iter()
					.map(|(_,_,id,_)| *id)
					.collect(),
			instructions: vec![],
			next_label: 0,
			next_reg: 0,
		}
	}

	fn emit(&mut self, instr: TAC) {
		self.instructions.push(instr);
	}
}

fn get_location_reg(
	id: AstId,
	tac: &mut TACData,
	data: &mut Data,
) -> Result<(VRegId, Type), Error> {
	match lower_node(id, tac, data)? {
		Some(src) => match src {
			Location::VReg(vr, typ) => Ok((vr, typ)),
			Location::Addr(_, typ) |
			Location::Const(_, typ) |
			Location::Stack(_, typ) => {
				let vr = tac.reg();
				let typ = typ.clone();
				data.emit(TAC::Move {
					src,
					dst: Location::VReg(vr, typ),
				});
				Ok((vr, typ))
			}
		}
		None => Err(Error::missing_ast_node(data.name, id)),
	}
}

fn lower_node(
	id: AstId,
	tac: &mut TACData,
	data: &mut Data,
) -> Result<Option<Location>, Error> {
	let Some(ast) = tac.typed_body.get(id) else {
		return Err(Error::missing_ast_node(data.name, id));
	};

	match ast.kind {
		AstKind::ScopeBegin => Ok(None),
		AstKind::ScopeEnd => Ok(None),
		AstKind::Mark { region_id: _, mark_id: _ } => Ok(None),
		AstKind::Free { region_id: _, mark_id: _ } => Ok(None),
		AstKind::Use { region_id: _, ident: _ } => Ok(None),

		AstKind::Int(val) => {
			match ast.typ {
				Type::Int => unreachable!("Int"),
				Type::Dec => unreachable!("Dec"),
				Type::Unknown => unreachable!("Unknown"),
				Type::Void => unreachable!("Void"),
				Type::Record(_) => unreachable!("Record"),
				Type::Table(_) => unreachable!("Table"),
				_ => {}
			}

			let src = Location::Const(val, ast.typ);
			let dst = Location::VReg(tac.reg(), ast.typ);
			data.emit(TAC::Move { src, dst });
			Ok(Some(dst))
		}

		AstKind::Dec(_value) => {
			todo!()
		}

		AstKind::Ident(ident_id) => {
			if let Some(loc) = tac.local_map.get(&ident_id) {
				return Ok(Some(*loc));
			}

			if let Some(idx) = data.locals.iter()
					.position(|local_id| *local_id == ident_id)
			{
				let loc = Location::Stack(idx, ast.typ);
				tac.local_map.insert(ident_id, loc);
				Ok(Some(loc))
			} else if let Some(value) = tac.prs_data.values.get(&ident_id) {
				let loc = match value {
					Value::Integer(val) => Location::Const(*val, ast.typ),
					Value::Decimal(_) => todo!("implement TAC constants for decimals"),
				};
				tac.local_map.insert(ident_id, loc);
				Ok(Some(loc))
			} else {
				Err(Error::unknown_ident(data.name, ident_id))
			}
		}

		AstKind::Assign { lhs, rhs } => {
			let Some(dst) = lower_node(lhs, tac, data)? else {
				return Err(Error::missing_ast_node(data.name, lhs));
			};

			let (vr, typ) = get_location_reg(rhs, tac, data)?;

			let src = Location::VReg(vr, typ);
			data.emit(TAC::Move { src, dst });

			Ok(Some(Location::VReg(vr, ast.typ)))
		}

		AstKind::BinOp { op, lhs, rhs } => {
			let Some(lhs) = lower_node(lhs, tac, data)? else {
				return Err(Error::missing_ast_node(data.name, lhs));
			};
			let Some(rhs) = lower_node(rhs, tac, data)? else {
				return Err(Error::missing_ast_node(data.name, rhs));
			};

			let dst = Location::VReg(tac.reg(), ast.typ);
			data.emit(TAC::BinOp { op, lhs, rhs, dst });
			Ok(Some(dst))
		}

		AstKind::UnOp { op, rhs } => {
			let (vr0, _) = get_location_reg(rhs, tac, data)?;
			let vr1 = tac.reg();
			data.emit(TAC::UnOp {
				op,
				typ: ast.typ,
				vr0,
				vr1,
			});
			Ok(Some(Location::VReg(vr1, ast.typ)))
		}

		AstKind::Return(maybe_expr) => {
			let vr = maybe_expr
					.and_then(|id| get_location_reg(id, tac, data).ok())
					.map(|(vr,_)| vr);
			data.emit(TAC::Return(vr));
			Ok(vr.map(|vr| Location::VReg(vr, ast.typ)))
		}

		AstKind::Block(ref stmts) => {
			for stmt_id in stmts {
				lower_node(*stmt_id, tac, data)?;
			}
			Ok(None)
		}

		AstKind::If { cond, ref then_block, ref else_block } => {
			let then_label = tac.label();
			let end_label = tac.label();

			// if cond goto then
			let (cond_vr,_) = get_location_reg(cond, tac, data)?;
			data.emit(TAC::JumpIf {
				lbl: then_label,
				vr: cond_vr,
			});

			// else block
			for stmt_id in else_block {
				lower_node(*stmt_id, tac, data)?;
			}
			data.emit(TAC::Jump(end_label));

			// then block
			data.emit(TAC::Label(then_label));
			for stmt_id in then_block {
				lower_node(*stmt_id, tac, data)?;
			}

			// end
			data.emit(TAC::Label(end_label));
			Ok(None)
		}

		AstKind::While { cond, ref block } => {
			let cond_label = tac.label();
			let loop_label = tac.label();

			// goto check
			data.emit(TAC::Jump(cond_label));

			// loop:
			data.emit(TAC::Label(loop_label));

			// body
			for stmt_id in block {
				lower_node(*stmt_id, tac, data)?;
			}

			// check:
			data.emit(TAC::Label(cond_label));

			// if cond goto loop
			let (vr, _) = get_location_reg(cond, tac, data)?;
			data.emit(TAC::JumpIf {
				lbl: loop_label,
				vr,
			});

			Ok(None)
		}

		AstKind::For {
			indexes: _,
			table: None,
			range_start: None,
			range_end: None,
			block: _,
		} => todo!("empty for-loops"),

		AstKind::For {
			indexes: _,
			table: None,
			range_start: None,
			range_end: Some(_end_id),
			block: _,
		} => todo!("end-bounded for-loops"),

		AstKind::For {
			indexes: _,
			table: None,
			range_start: Some(_start_id),
			range_end: None,
			block: _,
		} => todo!("start-bounded for-loops"),

		AstKind::For {
			ref indexes,
			table: None,
			range_start: Some(start_id),
			range_end: Some(end_id),
			ref block,
		} => {
			// For now, simple version: lower to while loop
			// for i in 0..N becomes:
			// i = 0
			// while i < N:
			//   body
			//   i = i + 1

			// Simple case: for i in 0..N
			if indexes.len() == 1 {
				let index_id = indexes[0];
				let Some(_) = lower_node(index_id, tac, data)? else {
					panic!()
				};

				let start_label = tac.label();
				let end_label = tac.label();

				// i = start
				let Some(idx) = lower_node(start_id, tac, data)? else {
					return Err(Error::missing_ast_node(data.name, start_id));
				};

				// start:
				data.emit(TAC::Label(start_label));

				// if i >= end goto end
				let Some(cmp_rhs) = lower_node(end_id, tac, data)? else {
					return Err(Error::missing_ast_node(data.name, end_id));
				};

				let vr2 = tac.reg();
				let cmp_dst = Location::VReg(vr2, Type::Bool);
				data.emit(TAC::BinOp {
					op: BinaryOp::CmpGE,
					lhs: idx,
					rhs: cmp_rhs,
					dst: cmp_dst,
				});
				data.emit(TAC::JumpIf {
					lbl: end_label,
					vr: vr2,
				});

				// body
				for stmt_id in block {
					lower_node(*stmt_id, tac, data)?;
				}

				// i = i + 1
				data.emit(TAC::BinOp {
					op: BinaryOp::Add,
					lhs: idx,
					rhs: Location::Const(1, Type::S8),
					dst: idx,
				});

				// goto start
				data.emit(TAC::Jump(start_label));

				// end:
				data.emit(TAC::Label(end_label));
			}

			// TODO - srenshaw - Handle table iteration, multiple variables, etc.

			Ok(None)
		}

		AstKind::For {
			indexes: _,
			table: Some(_table),
			range_start: None,
			range_end: None,
			block: _,
		} => todo!("empty table-loops"),

		AstKind::For {
			indexes: _,
			table: Some(_table),
			range_start: None,
			range_end: Some(_end_id),
			block: _,
		} => todo!("end-bounded table-loops"),

		AstKind::For {
			indexes: _,
			table: Some(_table),
			range_start: Some(_start_id),
			range_end: None,
			block: _,
		} => todo!("start-bounded table-loops"),

		AstKind::For {
			indexes: _,
			table: Some(_table),
			range_start: Some(_start_id),
			range_end: Some(_end_id),
			block: _,
		} => todo!("fully-bounded table-loops"),

		AstKind::Call { proc_id: _, block: _ } => {
			todo!("lower proc-call")
		}

		AstKind::Access { base_id, ref path } => {
			let mut curr_id = base_id;
			let mut location = tac.loc_data[&curr_id];

			for segment in path {
				match segment {
					PathSegment::Field(field_id) => {
						let rec_pak = &tac.pak_data.records[&curr_id];
						let record = &tac.prs_data.records[&curr_id];
						let (idx, typ) = record.fields.iter()
								.enumerate()
								.find(|(_, (id, _))| id == field_id)
								.map(|(idx, (_, typ))| (idx, typ))
								.expect("missing field");
						let offset = rec_pak.offsets[idx];
						location += offset as u32;

						match typ {
							Type::Record(rid) => curr_id = *rid,
							Type::Table(_) => panic!("Table used as a field"),
							_ => {}
						}
					}

					PathSegment::Index(_expr_id, _field_id) => {
						todo!("table-indexed access")
					}
				}
			}

			Ok(Some(Location::Addr(location, ast.typ)))
		}
	}
}

enum ErrorKind {
	MissingAstNode(AstId),
	//MissingLoopBounds(AstId),
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

	/*
	fn missing_loop_bounds(
		proc_id: IdentId,
		ast_id: AstId,
	) -> Self {
		Self {
			kind: ErrorKind::MissingLoopBounds(ast_id),
			proc_id,
		}
	}
	*/

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
		proc_db: &IdentMap<Procedure<SrcPos>>,
	) -> error::Error {
		let proc_data = &proc_db[&self.proc_id];

		let (location, message) = match self.kind {
			ErrorKind::MissingAstNode(ast_id) => {
				let location = proc_data.body[ast_id].location;
				let message = format!("{} not found in procedure '{}'", ast_id, lex_data.text(input, &self.proc_id));
				(location, message)
			}
			/*
			ErrorKind::MissingLoopBounds(ast_id) => {
				let location = proc_data.body[ast_id].location;
				let message = format!("type-checker missed a bounds check in {}", lex_data.text(input, &self.proc_id));
				(location, message)
			}
			*/
			ErrorKind::UnknownIdentifier(ident_id) => {
				let location = lex_data.location(&ident_id);
				let message = format!("Unknown identifier '{}' in procedure '{}'",
					lex_data.text(input, &ident_id), lex_data.text(input, &self.proc_id));
				(location, message)
			}
		};

		error::Error::new(location, message)
			.with_kind(error::Kind::LoweringTAC)
	}
}

#[cfg(test)]
mod tests {
	use std::collections::HashMap;
	use crate::{input, layout, lexer, packing, parser, type_checker};
	use crate::identifier::Identifier;

	use super::*;

	fn setup(source: &str) -> (IdentMap<Data>, packing::Data, IdentMap<u32>) {
		let mut source_str = String::new();
		source_str.push_str("region Stack[0] @ 0;");
		source_str.push_str(source);

		let input = input::eval(file!().to_string(), source_str.into());

		let lex_data = lexer::eval(&input.source)
				.unwrap_or_else(|e| panic!("{}", e.display(&input)));

		let prs_data = parser::eval(&input, &lex_data, false)
				.unwrap_or_else(|e| panic!("{}", e.display(&input)));

		let typ_data = type_checker::eval(&input, &lex_data, &prs_data)
				.unwrap_or_else(|e| panic!("{}", e.display(&input)));

		let pak_data = packing::eval(&prs_data);

		let lay_data = layout::eval(&prs_data, &pak_data)
				.unwrap_or_else(|e| panic!("{}", e.display(&input, &lex_data)));

		let sections = eval(&prs_data, &typ_data, &pak_data, &lay_data)
				.map_err(|e| e.into_comp_error(&input, &lex_data, &prs_data.procedures))
				.unwrap_or_else(|e| panic!("{}", e.display(&input)));

		(sections, pak_data, lay_data)
	}

	#[test]
	fn return_void() {
		let (section_db, _, _) = setup("main {}
		proc a() {
			return;
		}");
		assert_eq!(section_db.len(), 2);
		let section = &section_db[&"a".id()];
		assert_eq!(section.instructions, [
			TAC::Return(None),
		]);
	}

	#[test]
	fn return_expression() {
		let (section_db, _, _) = setup("main {}
		proc a() -> s8 {
			return 100 - 200;
		}");
		assert_eq!(section_db.len(), 2);
		let section = &section_db[&"a".id()];
		assert_eq!(section.instructions, [
			TAC::Move {
				src: Location::Const(100, Type::S8),
				dst: Location::VReg(0, Type::S8),
			},
			TAC::Move {
				src: Location::Const(200, Type::S8),
				dst: Location::VReg(1, Type::S8),
			},
			TAC::BinOp {
				op: BinaryOp::Sub,
				lhs: Location::VReg(0, Type::S8),
				rhs: Location::VReg(1, Type::S8),
				dst: Location::VReg(2, Type::S8),
			},
			TAC::Return(Some(2)),
		]);
	}

	#[test]
	fn proc_if() {
		let (section_db, _, _) = setup("main {}
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
			"b".id(),
			"c".id(),
		]);
		assert_eq!(section.instructions, [
			// let b: s8 = 5;
			TAC::Move {
				src: Location::Const(5, Type::S8),
				dst: Location::VReg(0, Type::S8),
			},
			TAC::Move {
				src: Location::VReg(0, Type::S8),
				dst: Location::Stack(0, Type::S8),
			},
			// let c: s8 = 3;
			TAC::Move {
				src: Location::Const(3, Type::S8),
				dst: Location::VReg(1, Type::S8),
			},
			TAC::Move {
				src: Location::VReg(1, Type::S8),
				dst: Location::Stack(1, Type::S8),
			},
			// if b < 10 {
			TAC::Move {
				src: Location::Const(10, Type::S8),
				dst: Location::VReg(2, Type::S8),
			},
			TAC::BinOp {
				op: BinaryOp::CmpLT,
				lhs: Location::Stack(0, Type::S8),
				rhs: Location::VReg(2, Type::S8),
				dst: Location::VReg(3, Type::S8),
			},
			TAC::JumpIf { lbl: 0, vr: 3 },
			// b = 1;
			TAC::Move {
				src: Location::Const(1, Type::S8),
				dst: Location::VReg(4, Type::S8),
			},
			TAC::Move {
				src: Location::VReg(4, Type::S8),
				dst: Location::Stack(0, Type::S8),
			},
			TAC::Jump(1),
			TAC::Label(0),
			// c = 2;
			TAC::Move {
				src: Location::Const(2, Type::S8),
				dst: Location::VReg(5, Type::S8),
			},
			TAC::Move {
				src: Location::VReg(5, Type::S8),
				dst: Location::Stack(1, Type::S8),
			},
			TAC::Label(1),
			// return b + c;
			TAC::BinOp {
				op: BinaryOp::Add,
				lhs: Location::Stack(0, Type::S8),
				rhs: Location::Stack(1, Type::S8),
				dst: Location::VReg(6, Type::S8),
			},
			TAC::Return(Some(6)),
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
			"b".id(),
		]);
		assert_eq!(section.instructions, [
			TAC::Move {
				src: Location::Const(5, Type::S8),
				dst: Location::VReg(0, Type::S8),
			},
			TAC::Move {
				src: Location::VReg(0, Type::S8),
				dst: Location::Stack(0, Type::S8),
			},
			TAC::Jump(0),
			TAC::Label(1),
			TAC::Move {
				src: Location::Const(1, Type::S8),
				dst: Location::VReg(1, Type::S8),
			},
			TAC::BinOp {
				op: BinaryOp::Sub,
				lhs: Location::Stack(0, Type::S8),
				rhs: Location::VReg(1, Type::S8),
				dst: Location::VReg(2, Type::S8),
			},
			TAC::Move {
				src: Location::VReg(2, Type::S8),
				dst: Location::Stack(0, Type::S8),
			},
			TAC::Label(0),
			TAC::Move {
				src: Location::Const(0, Type::S8),
				dst: Location::VReg(3, Type::S8),
			},
			TAC::BinOp {
				op: BinaryOp::CmpGT,
				lhs: Location::Stack(0, Type::S8),
				rhs: Location::VReg(3, Type::S8),
				dst: Location::VReg(4, Type::S8),
			},
			TAC::JumpIf { lbl: 1, vr: 4 },
			TAC::Return(None),
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
			"b".id(),
			"c".id(),
			"i".id(),
		]);
		assert_eq!(section.instructions, [
			TAC::Move {
				src: Location::Const(4, Type::S8),
				dst: Location::VReg(0, Type::S8),
			},
			TAC::Move {
				src: Location::VReg(0, Type::S8),
				dst: Location::Stack(0, Type::S8),
			},
			TAC::Move {
				src: Location::Const(0, Type::S8),
				dst: Location::VReg(1, Type::S8),
			},
			TAC::Move {
				src: Location::VReg(1, Type::S8),
				dst: Location::Stack(1, Type::S8),
			},
			// Loop head
			TAC::Move {
				src: Location::Const(0, Type::U8),
				dst: Location::VReg(2, Type::U8),
			},
			TAC::Label(0),
			TAC::Move {
				src: Location::Const(10, Type::U8),
				dst: Location::VReg(3, Type::U8),
			},
			// TODO - srenshaw - Should probably unify Byte and Bool types at this stage in the compiler
			TAC::BinOp {
				op: BinaryOp::CmpGE,
				lhs: Location::VReg(2, Type::U8),
				rhs: Location::VReg(3, Type::U8),
				dst: Location::VReg(4, Type::Bool),
			},
			TAC::JumpIf { lbl: 1, vr: 4 },
			// Loop body
			TAC::Move {
				src: Location::Const(2, Type::S8),
				dst: Location::VReg(5, Type::S8),
			},
			TAC::BinOp {
				op: BinaryOp::Mul,
				lhs: Location::Stack(0, Type::S8),
				rhs: Location::VReg(5, Type::S8),
				dst: Location::VReg(6, Type::S8),
			},
			TAC::BinOp {
				op: BinaryOp::Add,
				lhs: Location::Stack(1, Type::S8),
				rhs: Location::VReg(6, Type::S8),
				dst: Location::VReg(7, Type::S8),
			},
			TAC::Move {
				src: Location::VReg(7, Type::S8),
				dst: Location::Stack(1, Type::S8),
			},
			// TODO - operands have different types
			TAC::BinOp {
				op: BinaryOp::Add,
				lhs: Location::VReg(2, Type::U8),
				rhs: Location::Const(1, Type::S8),
				dst: Location::VReg(2, Type::U8),
			},
			TAC::Jump(0),
			// Loop end
			TAC::Label(1),
			TAC::Return(None),
		]);
	}

	#[test]
	fn proc_internal_sub_expressions() {
		let (section_db,_,_) = setup("main {
			let a: s8 = (2 + 3) * (4 - 5);
		}");
		let section = &section_db[&"main".id()];
		assert_eq!(section.locals, [
			"a".id(),
		]);
		assert_eq!(section.instructions, [
			TAC::Move {
				src: Location::Const(2, Type::S8),
				dst: Location::VReg(0, Type::S8),
			},
			TAC::Move {
				src: Location::Const(3, Type::S8),
				dst: Location::VReg(1, Type::S8),
			},
			TAC::BinOp { op: BinaryOp::Add, lhs: Location::VReg(0, Type::S8), rhs: Location::VReg(1, Type::S8), dst: Location::VReg(2, Type::S8) },
			TAC::Move {
				src: Location::Const(4, Type::S8),
				dst: Location::VReg(3, Type::S8),
			},
			TAC::Move {
				src: Location::Const(5, Type::S8),
				dst: Location::VReg(4, Type::S8),
			},
			TAC::BinOp { op: BinaryOp::Sub, lhs: Location::VReg(3, Type::S8), rhs: Location::VReg(4, Type::S8), dst: Location::VReg(5, Type::S8) },
			TAC::BinOp { op: BinaryOp::Mul, lhs: Location::VReg(2, Type::S8), rhs: Location::VReg(5, Type::S8), dst: Location::VReg(6, Type::S8) },
			TAC::Move {
				src: Location::VReg(6, Type::S8),
				dst: Location::Stack(0, Type::S8),
			},
			TAC::Return(None),
		]);
	}

	#[test]
	fn static_record_placement() {
		let (_, pak_data, records) = setup("
		region a[4] @ 0;
		record b in a {
			c: s8,
			d: s8,
			e: s8,
		}
		main {}
		");
		assert_eq!(records.get(&"b".id()), Some(&0));
		let rec_packing = pak_data.records.get(&"b".id())
				.expect("missing packing for record 'b'");
		assert_eq!(rec_packing.size, 3);
		assert_eq!(*rec_packing.sizes, [1, 1, 1]);
		assert_eq!(*rec_packing.offsets, [0, 1, 2]);
	}

	#[test]
	fn run() {
		let (tac_data,_,_) = setup("\
		main {
			let b: s8 = 4;
			let c: s8 = 0;
			for i in [0..10] {
				c += b * 2;
			}
		}
		");
		let proc_tac = &tac_data[&"main".id()];
		let tac_emu = interpret(proc_tac);
		assert!(tac_emu.mem.is_empty());
		assert_eq!(tac_emu.labels.len(), 2);
		assert_eq!(tac_emu.stack.len(), 2);
		assert_eq!(tac_emu.stack[&1], 4 * 2 * 10);
	}

	#[derive(Debug, Default)]
	struct TacEmu {
		pc: usize,
		regs: HashMap<VRegId, i64>,
		mem: HashMap<u32, i64>,
		stack: HashMap<usize, i64>,
		labels: HashMap<LabelId, usize>,
	}

	fn interpret(data: &Data) -> TacEmu {
		let mut tac_emu = TacEmu::default();
		for (idx, tac) in data.instructions.iter().enumerate() {
			if let TAC::Label(label) = tac {
				tac_emu.labels.insert(*label, idx);
			}
		}

		loop {
			let tac = &data.instructions[tac_emu.pc];
			tac_emu.pc += 1;
			match tac {
				TAC::Label(_) => {}
				TAC::Return(with_value) => match with_value {
					None => {
						eprintln!("Return");
						break;
					}
					Some(value) => {
						eprintln!("Return {}", tac_emu.regs[value]);
						break;
					}
				}
				TAC::Jump(label) => {
					eprintln!("Jump {label}");
					tac_emu.pc = tac_emu.labels[label];
				}
				TAC::JumpIf { lbl, vr } => {
					eprintln!("JumpIf {lbl}");
					if tac_emu.regs[vr] != 0 {
						tac_emu.pc = tac_emu.labels[lbl];
					}
				}
				TAC::Move { src, dst } => {
					eprintln!("Load {src:?} -> {dst:?}");
					let val = match src {
						Location::Addr(addr,_) => {
							*tac_emu.mem.entry(*addr)
									.or_insert(-1)
						}
						Location::Const(val,_) => {
							*val
						}
						Location::Stack(idx,_) => {
							*tac_emu.stack.entry(*idx)
									.or_insert(-1)
						}
						Location::VReg(vr1,_) => {
							*tac_emu.regs.get(vr1)
									.expect("missing vreg")
						}
					};

					match dst {
						Location::Addr(addr,_) => {
							tac_emu.mem.insert(*addr, val);
						}
						Location::Const(val,_) => {
							panic!("storing into a constant value: {val}");
						}
						Location::Stack(idx,_) => {
							tac_emu.stack.insert(*idx, val);
						}
						Location::VReg(vr,_) => {
							tac_emu.regs.insert(*vr, val);
						}
					}
				}
				TAC::UnOp { op, typ, vr0, vr1 } => {
					eprintln!("UnOp {op}:{typ:?} {vr0}({}) -> {vr1}", tac_emu.regs[vr0]);
					match op {
						UnaryOp::Neg => {
							tac_emu.regs.insert(*vr1, -tac_emu.regs[vr0]);
						}
						UnaryOp::Not => {
							tac_emu.regs.insert(*vr1, !tac_emu.regs[vr0]);
						}
					}
				}
				TAC::BinOp { op, lhs, rhs, dst } => {
					eprintln!("BinOp {lhs:?} {op} {rhs:?} -> {dst:?}");

					let vall = match lhs {
						Location::Addr(adrl,_) => tac_emu.mem.get(adrl).cloned().unwrap_or(-1),
						Location::Const(vall,_) => *vall,
						Location::Stack(idxl,_) => tac_emu.stack.get(idxl).cloned().unwrap_or(-1),
						Location::VReg(vrl,_) => tac_emu.regs[vrl].clone(),
					};

					let valr = match rhs {
						Location::Addr(adrr,_) => tac_emu.mem.get(adrr).cloned().unwrap_or(-1),
						Location::Const(valr,_) => *valr,
						Location::Stack(idxr,_) => tac_emu.stack.get(idxr).cloned().unwrap_or(-1),
						Location::VReg(vrr,_) => tac_emu.regs[vrr].clone(),
					};

					let result = match op {
						BinaryOp::Add => vall + valr,
						BinaryOp::BinAnd => vall & valr,
						BinaryOp::BinOr => vall | valr,
						BinaryOp::BinXor => vall ^ valr,
						BinaryOp::CmpEQ => (vall == valr) as i64,
						BinaryOp::CmpNE => (vall != valr) as i64,
						BinaryOp::CmpLT => (vall < valr) as i64,
						BinaryOp::CmpGT => (vall > valr) as i64,
						BinaryOp::CmpGE => (vall >= valr) as i64,
						BinaryOp::CmpLE => (vall <= valr) as i64,
						BinaryOp::Div => vall / valr,
						BinaryOp::LogAnd => ((vall != 0) && (valr != 0)) as i64,
						BinaryOp::LogOr => ((vall != 0) || (valr != 0)) as i64,
						BinaryOp::LogXor => ((vall != 0) ^ (valr != 0)) as i64,
						BinaryOp::Mod => vall % valr,
						BinaryOp::Mul => vall * valr,
						BinaryOp::ShL => vall << valr,
						BinaryOp::ShR => vall >> valr,
						BinaryOp::Sub => vall - valr,
					};

					match dst {
						Location::Addr(adrd,_) => {
							tac_emu.mem.insert(*adrd, result);
						}
						Location::Const(..) => panic!("cannot store into a constant value"),
						Location::Stack(idxd,_) => {
							tac_emu.stack.insert(*idxd, result);
						}
						Location::VReg(vrd,_) => {
							tac_emu.regs.insert(*vrd, result);
						}
					}
				}
			}
		}

		tac_emu
	}
}

