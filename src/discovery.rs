
use std::collections::VecDeque;

use crate::cursor::{Cursor, Error};
use crate::identifier::{self, Id as IdentId, Identifier, Map as IdentMap};
use crate::rings_type::Type;
use crate::task::{Kind as TaskKind, Task};
use crate::token::{Id as TokenId, Kind as TokenKind, KindList, PosList};
use crate::value::Kind as ValueKind;
use crate::type_size;
use crate::{Span, SrcPos, Target};

pub type ValueMap = identifier::Map<Value>;
pub type RegionMap = identifier::Map<Region>;
pub type ProcMap = identifier::Map<ProcType>;
pub type RecordMap = identifier::Map<Record>;
#[cfg(feature="table")]
pub type TableMap = identifier::Map<Table>;

pub type Param = (IdentId, Type);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
	Integer(i64),
	Decimal(f64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Region {
	pub span: Span<u32>,
	pub alloc_position: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcType {
	pub params: Vec<Param>,
	pub ret_type: Type,
	pub target: Option<Target>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
	pub fields: Vec<Param>,
	pub region: Option<IdentId>,
}

impl Record {
	pub fn size(&self, records: &RecordMap) -> u32 {
		self.fields.iter()
			.map(|(_, field_type)| type_size(records, field_type))
			.sum()
	}
}

#[cfg(feature="table")]
#[derive(Debug, PartialEq)]
pub struct Table {
	pub row_count: u32,
	pub column_spec: Vec<Param>,
	pub address: Option<u32>,
}

#[cfg(feature="table")]
impl Table {
	pub fn size(&self, data: &Data) -> u32 {
		let row_size: u32 = self.column_spec.iter()
			.map(|(_, field_type)| data.type_size(field_type))
			.sum();
		self.row_count * row_size
	}

	pub fn column_offset(&self, data: &Data, field_id: identifier::Id) -> Option<u32> {
		self.column_spec.iter()
			.position(|(id,_)| field_id == *id)
			.map(|idx| self.column_spec[..idx].iter()
				.map(|(_,field_type)| data.type_size(field_type) * self.row_count)
				.sum())
	}
}

type DiscResult<T> = Result<T, Error>;

pub fn eval(
	source: &str,
	identifiers: &IdentMap<Span<SrcPos>>,
	tok_list: &KindList,
	tok_pos: &PosList,
	task_queue: &mut VecDeque<Task>,
	procedures: &mut ProcMap,
	records: &mut RecordMap,
	regions: &mut RegionMap,
	values: &mut ValueMap,
) -> DiscResult<()> {
	let mut cursor = Cursor::default();
	loop {
		match cursor.current(tok_list) {
			TokenKind::Main => {
				let tok_start = discover_init_proc(&mut cursor, tok_list)?;
				let name_id = "main".id();
				task_queue.push_back(Task::new(TaskKind::Proc, name_id, tok_start));
				procedures.insert(name_id, ProcType { params: vec![], ret_type: Type::Unit, target: None });
			}

			TokenKind::Sub => {
				let tok_start = discover_init_proc(&mut cursor, tok_list)?;
				let name_id = "sub".id();
				task_queue.push_back(Task::new(TaskKind::Proc, name_id, tok_start));
				procedures.insert(name_id, ProcType { params: vec![], ret_type: Type::Unit, target: None });
			}

			TokenKind::Proc => {
				let (name_id, tok_start, params, ret_type) = discover_proc(&mut cursor, tok_list, records)?;
				task_queue.push_back(Task::new(TaskKind::Proc, name_id, tok_start));
				procedures.insert(name_id, ProcType { params, ret_type, target: None });
			}

			TokenKind::M68k => {
				cursor.advance();
				let (name_id, tok_start, params, ret_type) = discover_target_proc(&mut cursor, tok_list, records)?;
				task_queue.push_back(Task::new(TaskKind::Proc, name_id, tok_start));
				procedures.insert(name_id, ProcType { params, ret_type, target: Some(Target::M68k) });
			}

			TokenKind::SH2 => {
				cursor.advance();
				let (name_id, tok_start, params, ret_type) = discover_target_proc(&mut cursor, tok_list, records)?;
				task_queue.push_back(Task::new(TaskKind::Proc, name_id, tok_start));
				procedures.insert(name_id, ProcType { params, ret_type, target: Some(Target::SH2) });
			}

			TokenKind::X64 => {
				cursor.advance();
				let (name_id, tok_start, params, ret_type) = discover_target_proc(&mut cursor, tok_list, records)?;
				task_queue.push_back(Task::new(TaskKind::Proc, name_id, tok_start));
				procedures.insert(name_id, ProcType { params, ret_type, target: Some(Target::X86_64) });
			}

			TokenKind::Z80 => {
				cursor.advance();
				let (name_id, tok_start, params, ret_type) = discover_target_proc(&mut cursor, tok_list, records)?;
				task_queue.push_back(Task::new(TaskKind::Proc, name_id, tok_start));
				procedures.insert(name_id, ProcType { params, ret_type, target: Some(Target::Z80) });
			}

			TokenKind::Region => {
				cursor.advance();
				let name_id = cursor.expect_identifier(tok_list, "region name")?;
				let region = discover_region(&mut cursor, source, identifiers, tok_list, tok_pos)?;
				regions.insert(name_id, region);
			}

			TokenKind::Record => {
				cursor.advance();
				let ident_id = cursor.expect_identifier(tok_list, "record name")?;
				let record = discover_record(&mut cursor, tok_list, records, regions)?;
				records.insert(ident_id, record);
			}

			#[cfg(feature="table")]
			TokenKind::Table => {
				cursor.advance();
				let ident_id = cursor.expect_identifier(data, "table name")?;
				let table = discover_table(&mut cursor, data)?;
				data.tables.insert(ident_id, table);
			}

			#[cfg(feature="index")]
			TokenKind::Index => return Err(error::error(data,
				"indexes not yet implemented",
				cursor.index())),

			TokenKind::Value => {
				cursor.advance();
				let name_id = cursor.expect_identifier(tok_list, "value name")?;
				cursor.expect(tok_list, TokenKind::Eq)?;
				let tok_start = cursor.index();
				let expr = crate::value::value_expression(&mut cursor, tok_list, &[TokenKind::Semicolon], None)?;
				match expr.kind {
					ValueKind::Int(num) => {
						values.insert(name_id, Value::Integer(num));
					}
					ValueKind::Dec(num) => {
						values.insert(name_id, Value::Decimal(num));
					}
					ValueKind::Unfinished => {
						task_queue.push_back(Task::new(TaskKind::Value, name_id, tok_start));
					}
				}
			}

			TokenKind::Eof => break,
			_ => return Err(cursor.expected_token("top-level statement")),
		}
	}

	Ok(())
}

fn check_braces(
	cursor: &mut Cursor,
	tok_list: &KindList,
) -> DiscResult<()> {
	cursor.expect(tok_list, TokenKind::OBrace)?;
	let mut brace_count = 1;
	while brace_count > 0 && cursor.current(tok_list) != TokenKind::Eof {
		brace_count += match cursor.current(tok_list) {
			TokenKind::OBrace => 1,
			TokenKind::CBrace => -1,
			TokenKind::Eof => {
				return Err(cursor.expected_token("end of procedure"));
			}
			_ => 0,
		};
		cursor.advance();
	}

	Ok(())
}

fn discover_init_proc(
	cursor: &mut Cursor,
	tok_list: &KindList,
) -> DiscResult<TokenId> {
	cursor.advance();
	let tok_start = cursor.index();
	check_braces(cursor, tok_list)?;
	Ok(tok_start)
}

fn discover_proc(
	cursor: &mut Cursor,
	tok_list: &KindList,
	records: &RecordMap,
) -> DiscResult<(IdentId, TokenId, Vec<Param>, Type)> {
	cursor.expect(tok_list, TokenKind::Proc)?;
	let name_id = cursor.expect_identifier(tok_list, "procedure name")?;
	cursor.expect(tok_list, TokenKind::OParen)?;
	let params = discover_fields(cursor, tok_list, records, TokenKind::CParen)?;
	cursor.expect(tok_list, TokenKind::CParen)?;
	let ret_type = if cursor.expect(tok_list, TokenKind::Arrow).is_ok() {
		cursor.expect_type(tok_list, records)?
	} else {
		Type::Unit
	};
	let tok_start = cursor.index();
	check_braces(cursor, tok_list)?;
	Ok((name_id, tok_start, params, ret_type))
}

fn discover_target_proc(
	cursor: &mut Cursor,
	tok_list: &KindList,
	records: &RecordMap,
) -> DiscResult<(IdentId, TokenId, Vec<Param>, Type)> {
	Ok(match cursor.current(tok_list) {
		TokenKind::Main => {
			("main".id(), discover_init_proc(cursor, tok_list)?, vec![], Type::Unit)
		}
		TokenKind::Sub => {
			("sub".id(), discover_init_proc(cursor, tok_list)?, vec![], Type::Unit)
		}
		_ => discover_proc(cursor, tok_list, records)?,
	})
}

fn discover_fields(
	cursor: &mut Cursor,
	tok_list: &KindList,
	records: &RecordMap,
	end_token: TokenKind,
) -> DiscResult<Vec<Param>> {
	let mut fields = Vec::default();
	while end_token != cursor.current(tok_list) {
		let field_id = cursor.expect_identifier(tok_list, "field name")?;
		cursor.expect(tok_list, TokenKind::Colon)?;
		let field_type = cursor.expect_type(tok_list, records)?;
		fields.push((field_id, field_type));
		if cursor.current(tok_list) != TokenKind::Comma {
			break;
		}
		cursor.advance();
	}
	Ok(fields)
}

fn discover_region(
	cursor: &mut Cursor,
	source: &str,
	identifiers: &IdentMap<Span<SrcPos>>,
	tok_list: &KindList,
	tok_pos: &PosList,
) -> DiscResult<Region> {
	cursor.expect(tok_list, TokenKind::OBracket)?;
	let byte_count = cursor.expect_u32(source, identifiers, tok_list, tok_pos, "region size")?;
	cursor.expect(tok_list, TokenKind::CBracket)?;
	cursor.expect(tok_list, TokenKind::At)?;
	let address = cursor.expect_u32(source, identifiers, tok_list, tok_pos, "region address")?;
	cursor.expect(tok_list, TokenKind::Semicolon)?;
	Ok(Region {
		span: Span { start: address, end: address + byte_count },
		alloc_position: 0,
	})
}

/// Check after an '@' for a defined region or an address location
fn discover_address(
	cursor: &mut Cursor,
	tok_list: &KindList,
	regions: &RegionMap,
) -> DiscResult<Option<IdentId>> {
	if cursor.expect(tok_list, TokenKind::At).is_ok() {
		if let Ok(id) = cursor.expect_identifier(tok_list, "region name") {
			if regions.contains_key(&id) {
				return Ok(Some(id));
			}
		}

		Err(Error::ExpectedToken {
			expected: "address or region ID".to_string(),
			found: cursor.index(),
		})
	} else {
		Ok(None)
	}
}

fn discover_record(
	cursor: &mut Cursor,
	tok_list: &KindList,
	records: &RecordMap,
	regions: &RegionMap,
) -> DiscResult<Record> {
	let region = discover_address(cursor, tok_list, regions)?;
	cursor.expect(tok_list, TokenKind::OBrace)?;
	let fields = discover_fields(cursor, tok_list, records, TokenKind::CBrace)?;
	cursor.expect(tok_list, TokenKind::CBrace)?;
	Ok(Record { region, fields })
}

#[cfg(feature="table")]
fn discover_table(cursor: &mut Cursor, data: &mut Data) -> DiscResult<Table> {
	cursor.expect(data, TokenKind::OBracket)?;
	let row_count = cursor.expect_u32(data, "table size")?;
	cursor.expect(data, TokenKind::CBracket)?;
	let address = discover_address(cursor, data)?;
	cursor.expect(data, TokenKind::OBrace)?;
	let column_spec = discover_fields(cursor, data, TokenKind::CBrace)?;
	cursor.expect(data, TokenKind::CBrace)?;
	Ok(Table {
		address,
		row_count,
		column_spec,
	})
}

#[cfg(test)]
mod can_parse {
	use crate::{error, lexer};
	use crate::Data;

	use super::*;

	fn setup(source: &str) -> Data {
		let mut db = Data::new(file!().to_string(), source.into());
		db .DEBUG_show_tokens = true;

		let result = {
			lexer::eval(
				&db.source,
				&mut db.identifiers,
				&mut db.tok_list,
				&mut db.tok_pos,
				&mut db.line_pos,
			).map_err(|e| e.with_kind(error::Kind::Lexer))
		}.and_then(|_| {
			eval(
				&db.source,
				&db.identifiers,
				&db.tok_list,
				&db.tok_pos,
				&mut db.task_queue,
				&mut db.procedures,
				&mut db.records,
				&mut db.regions,
				&mut db.values,
			).map_err(|e| e.into_comp_error(&db, error::Kind::Discovery))
		});

		match result {
			Ok(_) => db,
			Err(e) => {
				let msg = e.display(
					&db.source_file, &db.source, &db.line_pos);
				panic!("{msg}");
			}
		}
	}

	#[test]
	fn constant_values() {
		let data = setup("value a = 3; value b = 4.2;");
		assert_eq!(data.values.len(), 2);
		assert_eq!(data.values[&"a".id()], Value::Integer(3), "{data}");
		assert_eq!(data.values[&"b".id()], Value::Decimal(4.2), "{data}");
	}

	#[test]
	fn procedures() {
		let data = setup("value a = 5; proc b() {}");
		assert_eq!(data.task_queue.len(), 1);
		assert_eq!(data.task_queue[0], Task::new(
			TaskKind::Proc,
			"b".id(),
			TokenId::new(9),
		));
		assert_eq!(data.procedures.len(), 1);
		assert_eq!(data.procedures[&"b".id()], ProcType {
			params: vec![],
			ret_type: Type::Unit,
			target: None,
		});
	}

	#[test]
	fn procedure_with_params() {
		let data = setup("proc a(b: s8, c: s8) {}");
		assert_eq!(data.task_queue.len(), 1);
		assert_eq!(data.task_queue[0], Task::new(
			TaskKind::Proc,
			"a".id(),
			TokenId::new(11),
		));
		assert_eq!(data.procedures.len(), 1);
		assert_eq!(data.procedures[&"a".id()], ProcType {
			params: vec![
				("b".id(), Type::s8_top()),
				("c".id(), Type::s8_top()),
			],
			ret_type: Type::Unit,
			target: None,
		});
	}

	#[test]
	fn procedure_with_return() {
		let data = setup("proc a() -> s8 {}");
		assert_eq!(data.task_queue.len(), 1);
		assert_eq!(data.task_queue[0], Task::new(
			TaskKind::Proc,
			"a".id(),
			TokenId::new(6),
		));
		assert_eq!(data.procedures.len(), 1);
		assert_eq!(data.procedures[&"a".id()], ProcType {
			params: vec![],
			ret_type: Type::s8_top(),
			target: None,
		});
	}

	#[test]
	fn procedure_with_target() {
		let data = setup("sh2 proc a() {}");
		assert_eq!(data.task_queue.len(), 1);
		assert_eq!(data.task_queue[0], Task::new(
			TaskKind::Proc,
			"a".id(),
			TokenId::new(5),
		));
		assert_eq!(data.procedures.len(), 1);
		assert_eq!(data.procedures[&"a".id()], ProcType {
			params: vec![],
			ret_type: Type::Unit,
			target: Some(Target::SH2),
		});
	}

	#[test]
	fn init_procedure_with_target() {
		let data = setup("sh2 main {} z80 sub {}");
		assert_eq!(data.task_queue.len(), 2);
		assert_eq!(data.task_queue, [
			Task::new(
				TaskKind::Proc,
				"main".id(),
				TokenId::new(2),
			),
			Task::new(
				TaskKind::Proc,
				"sub".id(),
				TokenId::new(6),
			),
		]);
		assert_eq!(data.procedures.len(), 2);
		assert_eq!(data.procedures[&"main".id()], ProcType {
			params: vec![],
			ret_type: Type::Unit,
			target: Some(Target::SH2),
		});
		assert_eq!(data.procedures[&"sub".id()], ProcType {
			params: vec![],
			ret_type: Type::Unit,
			target: Some(Target::Z80),
		});
	}

	#[test]
	fn region() {
		let data = setup("region a[1024] @ 0x0020_0000;");
		assert_eq!(data.regions.len(), 1);
		assert_eq!(data.regions[&"a".id()], Region {
			span: (0x20_0000..0x20_0400).into(),
			alloc_position: 0,
		});
	}

	#[test]
	fn empty_record() {
		let data = setup("record a {}");
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(&data.records), 0);
		assert_eq!(record.region, None);
		assert_eq!(record.fields.len(), 0);
	}

	#[test]
	fn record_with_one_field_no_trailing_comma() {
		let data = setup("record a { b: s8 }");
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(&data.records), 1);
		assert_eq!(record.region, None);
		assert_eq!(record.fields, [("b".id(), Type::s8_top())]);
	}

	#[test]
	fn record_with_one_field_and_trailing_comma() {
		let data = setup("record a { b: s8, }");
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(&data.records), 1);
		assert_eq!(record.region, None);
		assert_eq!(record.fields, [("b".id(), Type::s8_top())]);
	}

	#[test]
	fn record_with_multiple_fields() {
		let data = setup("record a { b: s8, c: s8 }");
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(&data.records), 2);
		assert_eq!(record.region, None);
		assert_eq!(record.fields, [
			("b".id(), Type::s8_top()),
			("c".id(), Type::s8_top()),
		]);
	}

	#[test]
	fn record_with_user_defined_field() {
		let data = setup("record a {} record b { c: a }");
		assert_eq!(data.records.len(), 2);
		let record = &data.records[&"b".id()];
		assert_eq!(record.size(&data.records), 0);
		assert_eq!(record.region, None);
		assert_eq!(record.fields, [
			("c".id(), Type::Record("a".id())),
		]);
	}

	#[test]
	fn record_with_region() {
		let data = setup("
			region b[8] @ 32;
			record a @ b {}
		");
		assert_eq!(data.regions.len(), 1);
		assert_eq!(data.regions[&"b".id()], Region {
			span: (32..40).into(),
			alloc_position: 0,
		});
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(&data.records), 0);
		assert_eq!(record.region, Some("b".id()));
		assert!(record.fields.is_empty());
	}

	#[cfg(feature="table")]
	#[test]
	fn empty_table() {
		let data = setup("table a[10] {}");
		assert_eq!(data.tables.len(), 1);
		let table = &data.tables[&"a".id()];
		assert_eq!(table.row_count, 10);
		assert_eq!(table.column_spec, []);
		assert_eq!(table.size(&data), 0);
		assert_eq!(table.address, None);
	}

	#[cfg(feature="table")]
	#[test]
	fn table_with_one_field() {
		let data = setup("table a[10] { b: u32 }");
		assert_eq!(data.tables.len(), 1);
		let table = &data.tables[&"a".id()];
		assert_eq!(table.row_count, 10);
		assert_eq!(table.column_spec, [("b".id(), Type::U32)]);
		assert_eq!(table.size(&data), 40);
		assert_eq!(table.address, None);
	}

	#[cfg(feature="table")]
	#[test]
	fn table_with_multiple_field() {
		let data = setup("table a[10] { b: u32, c: s16 }");
		assert_eq!(data.tables.len(), 1);
		let table = &data.tables[&"a".id()];
		assert_eq!(table.row_count, 10);
		assert_eq!(table.column_spec, [
			("b".id(), Type::U32),
			("c".id(), Type::S16),
		]);
		assert_eq!(table.size(&data), 60);
		assert_eq!(table.address, None);
	}

	#[cfg(feature="table")]
	#[test]
	fn table_with_user_defined_field() {
		let data = setup("record a { a1: s16 } table b[10] { b1: a }");
		assert_eq!(data.tables.len(), 1);
		let table = &data.tables[&"b".id()];
		assert_eq!(table.row_count, 10);
		assert_eq!(table.column_spec, [
			("b1".id(), Type::Record("a".id())),
		]);
		assert_eq!(table.size(&data), 20);
		assert_eq!(table.address, None);
	}
}

