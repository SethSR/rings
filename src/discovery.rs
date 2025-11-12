
use std::ops::Range;

use crate::cursor::Cursor;
use crate::error::{self, CompilerError};
use crate::identifier;
use crate::token;
use crate::Data;

pub type ValueMap = identifier::Map<Value>;
pub type RegionMap = identifier::Map<Region>;
pub type ProcMap = identifier::Map<Procedure>;
pub type RecordMap = identifier::Map<Record>;
pub type TableMap = identifier::Map<Table>;

pub type Field = (identifier::Id, crate::Type);

// TODO - srenshaw - At some point, we'll want the discovery phase to use a work queue to allow
// out-of-order type recognition.

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
	Integer(i64),
	Decimal(f64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Region {
	pub address: u32,
	pub byte_count: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Procedure {
	pub params: Vec<(identifier::Id, crate::Type)>,
	pub ret_type: crate::Type,
	pub tok_start: token::Id,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
	pub fields: Vec<Field>,
	pub size: usize,
	pub address: Option<i64>,
}

#[derive(Debug, PartialEq)]
pub struct Table {
	pub row_count: u32,
	pub column_spec: Vec<Field>,
	pub size: usize,
	pub address: Option<i64>,
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
			token::Kind::Identifier(ident_id) => {
				if data.text(&ident_id) == "main" {
					cursor.advance();
					let tok_start = cursor.index();
					check_braces(cursor, data)?;
					data.procedures.insert(ident_id, Procedure {
						params: vec![],
						ret_type: crate::Type::Unit,
						tok_start,
					});
				} else {
					cursor.advance();
					cursor.expect(data, token::Kind::ColonColon)?;
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
						token::Kind::Proc => {
							let procedure = discover_proc(cursor, data)?;
							data.procedures.insert(ident_id, procedure);
						}
						token::Kind::Region => {
							let region = discover_region(cursor, data)?;
							data.regions.insert(ident_id, region);
						}
						token::Kind::Record => {
							let record = discover_record(cursor, data)?;
							data.records.insert(ident_id, record);
						}
						token::Kind::Table => {
							let table = discover_table(cursor, data)?;
							data.tables.insert(ident_id, table);
						}
						token::Kind::Index => return Err(error::error(data,
							"indexes not yet implemented",
							cursor.index())),
						_ => return Err(error::expected_token(data, "value statement", cursor.index())),
					}
				};
			}
			token::Kind::Eof => break,
			_ => return Err(error::expected_token(data,
				"top-level statement",
				cursor.index())),
		}
	}

	Ok(())
}

fn check_integer_as_u32(expected: &str, found: i64, span: Range<usize>) -> Result<u32, CompilerError> {
	if !(0..u32::MAX as i64).contains(&found) {
		Err(crate::error::expected(span, expected, &found.to_string()))
	} else {
		Ok(found as u32)
	}
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
) -> Result<Procedure, CompilerError> {
	let tok_start = cursor.index();
	cursor.expect(data, token::Kind::Proc)?;
	cursor.expect(data, token::Kind::OParen)?;
	let params = discover_fields(cursor, data, token::Kind::CParen)?;
	cursor.expect(data, token::Kind::CParen)?;
	let ret_type = if cursor.expect(data, token::Kind::Arrow).is_ok() {
		cursor.expect_type(data)?
	} else {
		crate::Type::Unit
	};
	check_braces(cursor, data)?;
	Ok(Procedure {
		params,
		ret_type,
		tok_start,
	})
}

fn discover_fields(cursor: &mut Cursor, data: &mut Data,
	end_token: token::Kind,
) -> Result<Vec<Field>, CompilerError> {
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

fn expect_u32(cursor: &mut Cursor, data: &Data, expected: &str) -> Result<u32, CompilerError> {
	cursor.expect_integer(data, expected)
		.and_then(|num| {
			check_integer_as_u32(&format!("valid {expected}"), num,
				(cursor.index() - 1).into()..cursor.index().into())
		})
}

fn discover_region(cursor: &mut Cursor, data: &mut Data) -> Result<Region, CompilerError> {
	cursor.expect(data, token::Kind::Region)?;
	cursor.expect(data, token::Kind::OBracket)?;
	let byte_count = expect_u32(cursor, data, "region size")?;
	cursor.expect(data, token::Kind::CBracket)?;
	cursor.expect(data, token::Kind::At)?;
	let address = expect_u32(cursor, data, "region address")?;
	cursor.expect(data, token::Kind::Semicolon)?;
	Ok(Region { byte_count, address })
}

fn discover_address(cursor: &mut Cursor, data: &mut Data) -> Result<Option<i64>, CompilerError> {
	if cursor.expect(data, token::Kind::At).is_ok() {
		if let Ok(num) = cursor.expect_integer(data, "record address") {
			for (id, region) in &data.regions {
				let addr_start = region.address as i64;
				let addr_end = addr_start + region.byte_count as i64;
				if (addr_start..addr_end).contains(&num) {
					return Err(error::error(data,
						&format!("Address '{num}' overlaps with region {}", data.text(id)),
						cursor.index()))
				}
			}
			Ok(Some(num))
		} else if let Ok(id) = cursor.expect_identifier(data, "region name") {
			Ok(Some(data.regions[&id].address as i64))
		} else {
			Err(error::error(data, "address or region ID", cursor.index()))
		}
	} else {
		Ok(None)
	}
}

fn discover_record(cursor: &mut Cursor, data: &mut Data) -> Result<Record, CompilerError> {
	cursor.expect(data, token::Kind::Record)?;
	let address = discover_address(cursor, data)?;
	cursor.expect(data, token::Kind::OBrace)?;
	let fields = discover_fields(cursor, data, token::Kind::CBrace)?;
	cursor.expect(data, token::Kind::CBrace)?;
	let size = fields.iter()
		.map(|(_, field_type)| data.type_size(*field_type))
		.sum();
	Ok(Record { size, address, fields })
}

fn discover_table(cursor: &mut Cursor, data: &mut Data) -> Result<Table, CompilerError> {
	cursor.expect(data, token::Kind::Table)?;
	cursor.expect(data, token::Kind::OBracket)?;
	let row_count = expect_u32(cursor, data, "table size")?;
	cursor.expect(data, token::Kind::CBracket)?;
	let address = discover_address(cursor, data)?;
	cursor.expect(data, token::Kind::OBrace)?;
	let column_spec = discover_fields(cursor, data, token::Kind::CBrace)?;
	cursor.expect(data, token::Kind::CBrace)?;
	let col_size: usize = column_spec.iter()
		.map(|(_, col_type)| data.type_size(*col_type))
		.sum();
	Ok(Table {
		address,
		row_count,
		column_spec,
		size: row_count as usize * col_size,
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
		let data = setup("a :: 5; b :: proc() {}");
		assert_eq!(data.procedures.len(), 1);
		assert_eq!(data.procedures[&"b".id()], Procedure {
			params: vec![],
			ret_type: crate::Type::Unit,
			tok_start: token::Id::new(6),
		});
	}

	#[test]
	fn procedure_with_params() {
		let data = setup("a :: proc(b: u8, c: s32) {}");
		assert_eq!(data.procedures.len(), 1);
		assert_eq!(data.procedures[&"a".id()], Procedure {
			params: vec![
				("b".id(), crate::Type::U8),
				("c".id(), crate::Type::S32),
			],
			ret_type: crate::Type::Unit,
			tok_start: token::Id::new(2),
		});
	}

	#[test]
	fn procedure_with_return() {
		let data = setup("a :: proc() -> s32 {}");
		assert_eq!(data.procedures.len(), 1);
		assert_eq!(data.procedures[&"a".id()], Procedure {
			params: vec![],
			ret_type: crate::Type::S32,
			tok_start: token::Id::new(2),
		});
	}

	#[test]
	fn region() {
		let data = setup("a :: region[1024] at 0x0020_0000;");
		assert_eq!(data.regions.len(), 1);
		assert_eq!(data.regions[&"a".id()], Region {
			byte_count: 1024,
			address: 0x0020_0000,
		});
	}

	#[test]
	fn empty_record() {
		let data = setup("a :: record {}");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], Record {
			size: 0,
			address: None,
			fields: vec![],
		});
	}

	#[test]
	fn record_with_one_field_no_trailing_comma() {
		let data = setup("a :: record { b: u8 }");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], Record {
			size: 1,
			address: None,
			fields: vec![("b".id(), crate::Type::U8)],
		});
	}

	#[test]
	fn record_with_one_field_and_trailing_comma() {
		let data = setup("a :: record { b: u8, }");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], Record {
			size: 1,
			address: None,
			fields: vec![("b".id(), crate::Type::U8)],
		});
	}

	#[test]
	fn record_with_multiple_fields() {
		let data = setup("a :: record { b: u8, c: s16 }");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], Record {
			size: 3,
			address: None,
			fields: vec![
				("b".id(), crate::Type::U8),
				("c".id(), crate::Type::S16),
			],
		});
	}

	#[test]
	fn record_with_user_defined_field() {
		let data = setup("a :: record {} b :: record { c: a }");
		assert_eq!(data.records.len(), 2);
		assert_eq!(data.records[&"b".id()], Record {
			size: 0,
			address: None,
			fields: vec![
				("c".id(), crate::Type::Record("a".id())),
			],
		});
	}

	#[test]
	fn record_with_address() {
		let data = setup("a :: record at 32 {}");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], Record {
			size: 0,
			address: Some(32),
			fields: vec![],
		});
	}

	#[test]
	fn empty_table() {
		let data = setup("a :: table[10] {}");
		assert_eq!(data.tables.len(), 1);
		assert_eq!(data.tables[&"a".id()], Table {
			row_count: 10,
			column_spec: vec![],
			size: 0,
			address: None,
		});
	}

	#[test]
	fn table_with_one_field() {
		let data = setup("a :: table[10] { b: u32 }");
		assert_eq!(data.tables.len(), 1);
		assert_eq!(data.tables[&"a".id()], Table {
			row_count: 10,
			column_spec: vec![("b".id(), crate::Type::U32)],
			size: 40,
			address: None,
		});
	}

	#[test]
	fn table_with_multiple_field() {
		let data = setup("a :: table[10] { b: u32, c: s16 }");
		assert_eq!(data.tables.len(), 1);
		assert_eq!(data.tables[&"a".id()], Table {
			row_count: 10,
			column_spec: vec![
				("b".id(), crate::Type::U32),
				("c".id(), crate::Type::S16),
			],
			size: 60,
			address: None,
		});
	}

	#[test]
	fn table_with_user_defined_field() {
		let data = setup("a :: record { a1: s16 } b :: table[10] { b1: a }");
		assert_eq!(data.tables.len(), 1);
		assert_eq!(data.tables[&"b".id()], Table {
			row_count: 10,
			column_spec: vec![
				("b1".id(), crate::Type::Record("a".id())),
			],
			size: 20,
			address: None,
		});
	}
}

