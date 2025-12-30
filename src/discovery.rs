
use crate::cursor::{Cursor, Error};
use crate::error;
use crate::identifier::{self, Id as IdentId, Identifier};
use crate::task::Kind as TaskKind;
use crate::token::{Id as TokenId, Kind as TokenKind};
use crate::value::Kind as ValueKind;
use crate::{Data, Span, Task, Type};

pub type ValueMap = identifier::Map<Value>;
pub type RegionMap = identifier::Map<Span<u32>>;
pub type ProcMap = identifier::Map<ProcType>;
#[cfg(feature="record")]
pub type RecordMap = identifier::Map<Record>;
#[cfg(feature="table")]
pub type TableMap = identifier::Map<Table>;

pub type Param = (IdentId, Type);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
	Integer(i64),
	Decimal(f64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcType {
	pub params: Vec<Param>,
	pub ret_type: Type,
}

#[cfg(feature="record")]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
	pub fields: Vec<Param>,
	pub address: Option<u32>,
}

#[cfg(feature="record")]
impl Record {
	pub fn size(&self, db: &Data) -> u32 {
		self.fields.iter()
			.map(|(_, field_type)| db.type_size(field_type))
			.sum()
	}

	//   1 2 1 4 1
	// = 2 2 2 4 1
	// = 0 2 4 6 10 = 11b
	pub fn field_offset(&self, _data: &Data, _field_id: IdentId) -> Option<u32> {
		todo!("make an actual record packing algorithm")
	}

	pub fn field_size(&self, data: &Data, field_id: identifier::Id) -> Option<u32> {
		self.fields.iter()
			.find(|(id,_)| field_id == *id)
			.map(|(_, field_type)| data.type_size(field_type))
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

pub fn eval(data: &mut Data) {
	let mut cursor = Cursor::default();
	if let Err(e) = eval_loop(&mut cursor, data) {
		data.errors.push(e.into_comp_error(data)
			.with_kind(error::Kind::Discovery));
	}
}

type DiscResult<T> = Result<T, Error>;

fn eval_loop(cursor: &mut Cursor, data: &mut Data) -> DiscResult<()> {
	loop {
		match cursor.current(data) {
			TokenKind::Main => {
				cursor.advance();
				let tok_start = cursor.index();
				check_braces(cursor, data)?;
				data.task_queue.push_back(Task::new(TaskKind::Proc, "main".id(), tok_start));
				data.procedures.insert("main".id(), ProcType { params: vec![], ret_type: Type::Unit });
			}

			TokenKind::Sub => {
				cursor.advance();
				let tok_start = cursor.index();
				check_braces(cursor, data)?;
				data.task_queue.push_back(Task::new(TaskKind::Proc, "sub".id(), tok_start));
				data.procedures.insert("sub".id(), ProcType { params: vec![], ret_type: Type::Unit });
			}

			TokenKind::Proc => {
				cursor.advance();
				let name_id = cursor.expect_identifier(data, "procedure name")?;
				let (params, ret_type, tok_start) = discover_proc(cursor, data)?;
				data.task_queue.push_back(Task::new(TaskKind::Proc, name_id, tok_start));
				data.procedures.insert(name_id, ProcType { params, ret_type });
			}

			TokenKind::Region => {
				cursor.advance();
				let name_id = cursor.expect_identifier(data, "region name")?;
				let region = discover_region(cursor, data)?;
				data.regions.insert(name_id, region);
			}

			#[cfg(feature="record")]
			TokenKind::Record => {
				cursor.advance();
				let ident_id = cursor.expect_identifier(data, "record name")?;
				let record = discover_record(cursor, data)?;
				data.records.insert(ident_id, record);
			}

			#[cfg(feature="table")]
			TokenKind::Table => {
				cursor.advance();
				let ident_id = cursor.expect_identifier(data, "table name")?;
				let table = discover_table(cursor, data)?;
				data.tables.insert(ident_id, table);
			}

			#[cfg(feature="index")]
			TokenKind::Index => return Err(error::error(data,
				"indexes not yet implemented",
				cursor.index())),

			TokenKind::Value => {
				cursor.advance();
				let name_id = cursor.expect_identifier(data, "value name")?;
				cursor.expect(data, TokenKind::Eq)?;
				let tok_start = cursor.index();
				let expr = crate::value::value_expression(cursor, data, &[TokenKind::Semicolon], false)?;
				match expr.kind {
					ValueKind::Int(num) => {
						data.values.insert(name_id, Value::Integer(num));
					}
					ValueKind::Dec(num) => {
						data.values.insert(name_id, Value::Decimal(num));
					}
					ValueKind::Unfinished => {
						data.task_queue.push_back(Task::new(TaskKind::Value, name_id, tok_start));
					}
				}
			}

			TokenKind::Eof => break,
			_ => return Err(cursor.expected_token("top-level statement")),
		}
	}

	Ok(())
}

fn check_braces(cursor: &mut Cursor, data: &mut Data) -> DiscResult<()> {
	cursor.expect(data, TokenKind::OBrace)?;
	let mut brace_count = 1;
	while brace_count > 0 && cursor.current(data) != TokenKind::Eof {
		brace_count += match cursor.current(data) {
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

fn discover_proc(cursor: &mut Cursor, data: &mut Data,
) -> DiscResult<(Vec<Param>, Type, TokenId)> {
	cursor.expect(data, TokenKind::OParen)?;
	let params = discover_fields(cursor, data, TokenKind::CParen)?;
	cursor.expect(data, TokenKind::CParen)?;
	let ret_type = if cursor.expect(data, TokenKind::Arrow).is_ok() {
		cursor.expect_type(data)?
	} else {
		Type::Unit
	};
	let tok_start = cursor.index();
	check_braces(cursor, data)?;
	Ok((params, ret_type, tok_start))
}

fn discover_fields(cursor: &mut Cursor, data: &mut Data,
	end_token: TokenKind,
) -> DiscResult<Vec<Param>> {
	let mut fields = Vec::default();
	while end_token != cursor.current(data) {
		let field_id = cursor.expect_identifier(data, "field name")?;
		cursor.expect(data, TokenKind::Colon)?;
		let field_type = cursor.expect_type(data)?;
		fields.push((field_id, field_type));
		if cursor.current(data) != TokenKind::Comma {
			break;
		}
		cursor.advance();
	}
	Ok(fields)
}

fn discover_region(cursor: &mut Cursor, data: &mut Data) -> DiscResult<Span<u32>> {
	cursor.expect(data, TokenKind::OBracket)?;
	let byte_count = cursor.expect_u32(data, "region size")?;
	cursor.expect(data, TokenKind::CBracket)?;
	cursor.expect(data, TokenKind::At)?;
	let address = cursor.expect_u32(data, "region address")?;
	cursor.expect(data, TokenKind::Semicolon)?;
	Ok(Span { start: address, end: address + byte_count })
}

#[cfg(feature="record")]
fn discover_address(cursor: &mut Cursor, data: &mut Data) -> DiscResult<Option<u32>> {
	if cursor.expect(data, TokenKind::At).is_ok() {
		if let Ok(num) = cursor.expect_u32(data, "record address") {
			for (id, region) in &data.regions {
				let addr_start = region.address;
				let addr_end = addr_start + region.byte_count;
				if (addr_start..addr_end).contains(&num) {
					return Err(error::error(data,
						&format!("Address '{num}' overlaps with region {}", data.text(id)),
						cursor.index()))
				}
			}
			Ok(Some(num))
		} else if let Ok(id) = cursor.expect_identifier(data, "region name") {
			Ok(Some(data.regions[&id].address))
		} else {
			Err(error::error(data, "address or region ID", cursor.index()))
		}
	} else {
		Ok(None)
	}
}

#[cfg(feature="record")]
fn discover_record(cursor: &mut Cursor, data: &mut Data) -> DiscResult<Record> {
	let address = discover_address(cursor, data)?;
	cursor.expect(data, TokenKind::OBrace)?;
	let fields = discover_fields(cursor, data, TokenKind::CBrace)?;
	cursor.expect(data, TokenKind::CBrace)?;
	Ok(Record { address, fields })
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
	use identifier::Identifier;

	use super::*;

	fn setup(source: &str) -> Data {
		let mut data = Data::new(file!().to_string(), source.into());
		data.DEBUG_show_tokens = true;
		crate::lexer::eval(&mut data);
		eval(&mut data);
		assert!(data.errors.is_empty(), "{}", data.errors_to_string());
		data
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
		});
	}

	#[test]
	fn region() {
		let data = setup("region a[1024] @ 0x0020_0000;");
		assert_eq!(data.regions.len(), 1);
		assert_eq!(data.regions[&"a".id()], Span {
			start: 0x20_0000,
			end:   0x20_0400,
		});
	}

	#[cfg(feature="record")]
	#[test]
	fn empty_record() {
		let data = setup("record a {}");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], Record {
			address: None,
			fields: vec![],
		});
	}

	#[cfg(feature="record")]
	#[test]
	fn record_with_one_field_no_trailing_comma() {
		let data = setup("record a { b: u8 }");
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(&data), 1);
		assert_eq!(record.address, None);
		assert_eq!(record.fields, [("b".id(), crate::Type::U8)]);
	}

	#[cfg(feature="record")]
	#[test]
	fn record_with_one_field_and_trailing_comma() {
		let data = setup("record a { b: u8, }");
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(&data), 1);
		assert_eq!(record.address, None);
		assert_eq!(record.fields, [("b".id(), crate::Type::U8)]);
	}

	#[cfg(feature="record")]
	#[test]
	fn record_with_multiple_fields() {
		let data = setup("record a { b: u8, c: s16 }");
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(&data), 3);
		assert_eq!(record.address, None);
		assert_eq!(record.fields, [
			("b".id(), crate::Type::U8),
			("c".id(), crate::Type::S16),
		]);
	}

	#[cfg(feature="record")]
	#[test]
	fn record_with_user_defined_field() {
		let data = setup("record a {} record b { c: a }");
		assert_eq!(data.records.len(), 2);
		let record = &data.records[&"b".id()];
		assert_eq!(record.size(&data), 0);
		assert_eq!(record.address, None);
		assert_eq!(record.fields, [
			("c".id(), crate::Type::Record("a".id())),
		]);
	}

	#[cfg(feature="record")]
	#[test]
	fn record_with_address() {
		let data = setup("record a @ 32 {}");
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(&data), 0);
		assert_eq!(record.address, Some(32));
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
		assert_eq!(table.column_spec, [("b".id(), crate::Type::U32)]);
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
			("b".id(), crate::Type::U32),
			("c".id(), crate::Type::S16),
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
			("b1".id(), crate::Type::Record("a".id())),
		]);
		assert_eq!(table.size(&data), 20);
		assert_eq!(table.address, None);
	}
}

