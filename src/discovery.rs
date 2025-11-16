
use crate::cursor::Cursor;
use crate::error::{self, CompilerError};
use crate::identifier::{self, Id as IdentId, Identifier};
use crate::token;
use crate::{Data, Type};

type TokenId = token::Id;

pub type ValueMap = identifier::Map<Value>;
#[cfg(feature="ready")]
pub type RegionMap = identifier::Map<Region>;
pub type ProcMap = identifier::Map<ProcType>;
#[cfg(feature="ready")]
pub type RecordMap = identifier::Map<Record>;
#[cfg(feature="ready")]
pub type TableMap = identifier::Map<Table>;

pub type Param = (IdentId, Type);

// TODO - srenshaw - At some point, we'll want the discovery phase to use a work queue to allow
// out-of-order type recognition.

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
	Integer(i64),
	Decimal(f64),
}

#[cfg(feature="ready")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Region {
	pub address: u32,
	pub byte_count: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcType {
	pub params: Vec<Param>,
	pub ret_type: Type,
}

#[cfg(feature="ready")]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
	pub fields: Vec<Param>,
	pub address: Option<u32>,
}

#[cfg(feature="ready")]
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

#[cfg(feature="ready")]
#[derive(Debug, PartialEq)]
pub struct Table {
	pub row_count: u32,
	pub column_spec: Vec<Param>,
	pub address: Option<u32>,
}

#[cfg(feature="ready")]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Task {
	MainProc {
		tok_start: TokenId,
	},
	SubProc {
		tok_start: TokenId,
	},
	Proc {
		name_id: IdentId,
		tok_start: TokenId,
	}
	// TODO - srenshaw - Add the rest of the top-level constructs
}

pub fn eval(data: &mut Data) {
	let mut cursor = Cursor::default();
	if let Err(mut e) = eval_loop(&mut cursor, data) {
		e.set_kind(error::Kind::Discovery);
		data.errors.push(e);
	}
}

fn eval_loop(cursor: &mut Cursor, data: &mut Data) -> Result<(), CompilerError> {
	loop {
		match cursor.current(data) {
			token::Kind::Main => {
				cursor.advance();
				let tok_start = cursor.index();
				check_braces(cursor, data)?;
				data.parse_queue.push_back(Task::MainProc { tok_start });
				data.procedures.insert("main".id(), ProcType { params: vec![], ret_type: Type::Unit });
			}

			token::Kind::Sub => {
				cursor.advance();
				let tok_start = cursor.index();
				check_braces(cursor, data)?;
				data.parse_queue.push_back(Task::SubProc { tok_start });
				data.procedures.insert("sub".id(), ProcType { params: vec![], ret_type: Type::Unit });
			}

			token::Kind::Proc => {
				cursor.advance();
				let name_id = cursor.expect_identifier(data, "procedure name")?;
				let (params, ret_type, tok_start) = discover_proc(cursor, data)?;
				data.parse_queue.push_back(Task::Proc { name_id, tok_start });
				data.procedures.insert(name_id, ProcType { params, ret_type });
			}

			#[cfg(feature="ready")]
			token::Kind::Region => {
				cursor.advance();
				let ident_id = cursor.expect_identifier(data, "region name")?;
				let region = discover_region(cursor, data)?;
				data.regions.insert(ident_id, region);
			}

			#[cfg(feature="ready")]
			token::Kind::Record => {
				cursor.advance();
				let ident_id = cursor.expect_identifier(data, "record name")?;
				let record = discover_record(cursor, data)?;
				data.records.insert(ident_id, record);
			}

			#[cfg(feature="ready")]
			token::Kind::Table => {
				cursor.advance();
				let ident_id = cursor.expect_identifier(data, "table name")?;
				let table = discover_table(cursor, data)?;
				data.tables.insert(ident_id, table);
			}

			token::Kind::Index => return Err(error::error(data,
				"indexes not yet implemented",
				cursor.index())),

			token::Kind::Identifier(ident_id) => {
				cursor.advance();
				cursor.expect(data, token::Kind::Colon2)?;
				match cursor.peek(data, 0) {
					token::Kind::Integer(value) => {
						cursor.advance(); // increment past the integer, as we already have it
						cursor.expect(data, token::Kind::Semicolon)?;
						data.values.insert(ident_id, Value::Integer(value));
					}
					token::Kind::Decimal(value) => {
						cursor.advance(); // increment past the decimal, as we already have it
						cursor.expect(data, token::Kind::Semicolon)?;
						data.values.insert(ident_id, Value::Decimal(value));
					}
					_ => return Err(error::expected_token(data, "value statement", cursor.index())),
				}
			}

			token::Kind::Eof => break,
			_ => return Err(error::expected_token(data,
				"top-level statement",
				cursor.index())),
		}
	}

	Ok(())
}

fn check_braces(cursor: &mut Cursor, data: &mut Data) -> Result<(), CompilerError> {
	cursor.expect(data, token::Kind::OBrace)?;
	let mut brace_count = 1;
	while brace_count > 0 && cursor.current(data) != token::Kind::Eof {
		brace_count += match cursor.current(data) {
			token::Kind::OBrace => 1,
			token::Kind::CBrace => -1,
			token::Kind::Eof => {
				return Err(crate::error::expected_token(data, "end of procedure", cursor.index()));
			}
			_ => 0,
		};
		cursor.advance();
	}

	Ok(())
}

fn discover_proc(cursor: &mut Cursor, data: &mut Data,
) -> Result<(Vec<Param>, Type, token::Id), CompilerError> {
	cursor.expect(data, token::Kind::OParen)?;
	let params = discover_fields(cursor, data, token::Kind::CParen)?;
	cursor.expect(data, token::Kind::CParen)?;
	let ret_type = if cursor.expect(data, token::Kind::Arrow).is_ok() {
		cursor.expect_type(data)?
	} else {
		crate::Type::Unit
	};
	let tok_start = cursor.index();
	check_braces(cursor, data)?;
	Ok((params, ret_type, tok_start))
}

fn discover_fields(cursor: &mut Cursor, data: &mut Data,
	end_token: token::Kind,
) -> Result<Vec<Param>, CompilerError> {
	let mut fields = Vec::default();
	while end_token != cursor.current(data) {
		let field_id = cursor.expect_identifier(data, "field name")?;
		cursor.expect(data, token::Kind::Colon)?;
		let field_type = cursor.expect_type(data)?;
		fields.push((field_id, field_type));
		if cursor.current(data) != token::Kind::Comma {
			break;
		}
		cursor.advance();
	}
	Ok(fields)
}

#[cfg(feature="ready")]
fn discover_region(cursor: &mut Cursor, data: &mut Data) -> Result<Region, CompilerError> {
	cursor.expect(data, token::Kind::OBracket)?;
	let byte_count = cursor.expect_u32(data, "region size")?;
	cursor.expect(data, token::Kind::CBracket)?;
	cursor.expect(data, token::Kind::At)?;
	let address = cursor.expect_u32(data, "region address")?;
	cursor.expect(data, token::Kind::Semicolon)?;
	Ok(Region { byte_count, address })
}

#[cfg(feature="ready")]
fn discover_address(cursor: &mut Cursor, data: &mut Data) -> Result<Option<u32>, CompilerError> {
	if cursor.expect(data, token::Kind::At).is_ok() {
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

#[cfg(feature="ready")]
fn discover_record(cursor: &mut Cursor, data: &mut Data) -> Result<Record, CompilerError> {
	let address = discover_address(cursor, data)?;
	cursor.expect(data, token::Kind::OBrace)?;
	let fields = discover_fields(cursor, data, token::Kind::CBrace)?;
	cursor.expect(data, token::Kind::CBrace)?;
	Ok(Record { address, fields })
}

#[cfg(feature="ready")]
fn discover_table(cursor: &mut Cursor, data: &mut Data) -> Result<Table, CompilerError> {
	cursor.expect(data, token::Kind::OBracket)?;
	let row_count = cursor.expect_u32(data, "table size")?;
	cursor.expect(data, token::Kind::CBracket)?;
	let address = discover_address(cursor, data)?;
	cursor.expect(data, token::Kind::OBrace)?;
	let column_spec = discover_fields(cursor, data, token::Kind::CBrace)?;
	cursor.expect(data, token::Kind::CBrace)?;
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
		assert!(data.errors.is_empty(), "{}", data.errors.iter()
			.map(|e| e.display(&data))
			.collect::<Vec<_>>()
			.join("\n"));
		data
	}

	#[test]
	fn constant_values() {
		let data = setup("a :: 3; b :: 4.2;");
		assert_eq!(data.values.len(), 2);
		assert_eq!(data.values[&"a".id()], Value::Integer(3), "{data}");
		assert_eq!(data.values[&"b".id()], Value::Decimal(4.2), "{data}");
	}

	#[test]
	fn procedures() {
		let data = setup("a :: 5; proc b() {}");
		assert_eq!(data.parse_queue.len(), 1);
		assert_eq!(data.parse_queue[0], Task::Proc {
			name_id: "b".id(),
			tok_start: token::Id::new(8),
		});
		assert_eq!(data.procedures.len(), 1);
		assert_eq!(data.procedures[&"b".id()], ProcType {
			params: vec![],
			ret_type: Type::Unit,
		});
	}

	#[test]
	fn procedure_with_params() {
		let data = setup("proc a(b: s8, c: s8) {}");
		assert_eq!(data.parse_queue.len(), 1);
		assert_eq!(data.parse_queue[0], Task::Proc {
			name_id: "a".id(),
			tok_start: token::Id::new(11),
		});
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
		assert_eq!(data.parse_queue.len(), 1);
		assert_eq!(data.parse_queue[0], Task::Proc {
			name_id: "a".id(),
			tok_start: token::Id::new(6),
		});
		assert_eq!(data.procedures.len(), 1);
		assert_eq!(data.procedures[&"a".id()], ProcType {
			params: vec![],
			ret_type: Type::s8_top(),
		});
	}

	#[cfg(feature="ready")]
	#[test]
	fn region() {
		let data = setup("region a[1024] @ 0x0020_0000;");
		assert_eq!(data.regions.len(), 1);
		assert_eq!(data.regions[&"a".id()], Region {
			byte_count: 1024,
			address: 0x0020_0000,
		});
	}

	#[cfg(feature="ready")]
	#[test]
	fn empty_record() {
		let data = setup("record a {}");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], Record {
			address: None,
			fields: vec![],
		});
	}

	#[cfg(feature="ready")]
	#[test]
	fn record_with_one_field_no_trailing_comma() {
		let data = setup("record a { b: u8 }");
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(&data), 1);
		assert_eq!(record.address, None);
		assert_eq!(record.fields, [("b".id(), crate::Type::U8)]);
	}

	#[cfg(feature="ready")]
	#[test]
	fn record_with_one_field_and_trailing_comma() {
		let data = setup("record a { b: u8, }");
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(&data), 1);
		assert_eq!(record.address, None);
		assert_eq!(record.fields, [("b".id(), crate::Type::U8)]);
	}

	#[cfg(feature="ready")]
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

	#[cfg(feature="ready")]
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

	#[cfg(feature="ready")]
	#[test]
	fn record_with_address() {
		let data = setup("record a @ 32 {}");
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(&data), 0);
		assert_eq!(record.address, Some(32));
		assert!(record.fields.is_empty());
	}

	#[cfg(feature="ready")]
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

	#[cfg(feature="ready")]
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

	#[cfg(feature="ready")]
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

	#[cfg(feature="ready")]
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

