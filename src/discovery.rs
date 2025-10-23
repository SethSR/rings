
use std::ops::Range;

use crate::cursor::Cursor;
use crate::error;
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
}

#[derive(Debug, PartialEq)]
pub struct Table {
	pub row_count: u32,
	pub column_spec: Vec<Field>,
	pub size: usize,
}

pub fn eval(data: &mut Data) {
	let mut cursor = Cursor::default();
	let cursor = &mut cursor;

	loop {
		match cursor.current(data) {
			token::Kind::Identifier(ident_id) => {
				let result = if data.text(ident_id) == "main" {
					discover_main_proc(cursor, data)
				} else {
					match cursor.peek(data, 2) {
						token::Kind::Integer(value) => discover_integer(cursor, data, ident_id, value),
						token::Kind::Decimal(value) => discover_decimal(cursor, data, ident_id, value),
						_ => {
							error::expected_token(data, "value statement", cursor.index() + 2);
							return;
						}
					}
				};

				if result.is_none() {
					break;
				}
			}
			token::Kind::Proc => if discover_proc(cursor, data).is_none() {
				error::error(data, "procedure definition", cursor.index());
				return;
			}
			token::Kind::Region => if discover_region(cursor, data).is_none() {
				error::error(data, "region definition", cursor.index());
				return;
			}
			token::Kind::Record => if discover_record(cursor, data).is_none() {
				error::error(data, "record definition", cursor.index());
				return;
			}
			token::Kind::Table => if discover_table(cursor, data).is_none() {
				error::error(data, "table definition", cursor.index());
				return;
			}
			token::Kind::Index => {
				error::error(data, "indexes not yet implemented", cursor.index());
				return;
			}
			token::Kind::Eof => break,
			_ => {
				error::expected_token(data, "top-level statement", cursor.index());
				return
			}
		}
	}
}

fn check_integer_as_u32(data: &mut Data, expected: &str, found: i64, span: Range<usize>) -> Option<u32> {
	if !(0..u32::MAX as i64).contains(&found) {
		error::expected(data, span, expected, &found.to_string());
		return None;
	}
	Some(found as u32)
}

fn check_braces(cursor: &mut Cursor, data: &mut Data) -> Option<()> {
	let mut brace_count = 1;
	while brace_count > 0 && cursor.current(data) != token::Kind::Eof {
		brace_count += match cursor.current(data) {
			token::Kind::OBrace => 1,
			token::Kind::CBrace => -1,
			token::Kind::Eof => {
				error::expected_token(data, "end of procedure", cursor.index());
				return None;
			}
			_ => 0,
		};
		cursor.advance();
	}

	Some(())
}

fn discover_main_proc(cursor: &mut Cursor, data: &mut Data) -> Option<()> {
	let tok_start = cursor.index();
	let Some(ident_id) = cursor.expect_identifier(data) else {
		error::expected_token(data, "COMPILER ERROR", cursor.index());
		return None;
	};
	cursor.expect(data, token::Kind::OBrace)?;
	check_braces(cursor, data)?;
	data.procedures.insert(ident_id, Procedure {
		params: vec![], // `main` has no parameters 
		ret_type: crate::Type::Unit, // `main` has no return type
		tok_start,
	});
	Some(())
}

fn discover_proc(cursor: &mut Cursor, data: &mut Data) -> Option<()> {
	let tok_start = cursor.index();
	cursor.expect(data, token::Kind::Proc)?;
	let Some(ident_id) = cursor.expect_identifier(data) else {
		error::expected_token(data, "procedure name", cursor.index());
		return None;
	};
	cursor.expect(data, token::Kind::OParen)?;
	let params = discover_fields(cursor, data, token::Kind::CParen)?;
	cursor.expect(data, token::Kind::CParen)?;
	// TODO - srenshaw - Handle return type declarations
	cursor.expect(data, token::Kind::OBrace)?;
	check_braces(cursor, data);
	data.procedures.insert(ident_id, Procedure {
		params,
		// TODO - srenshaw - Handle return type
		ret_type: crate::Type::Unit,
		tok_start,
	});
	Some(())
}

fn discover_fields(cursor: &mut Cursor, data: &mut Data,
	end_token: token::Kind,
) -> Option<Vec<Field>> {
	let mut fields = Vec::default();
	while end_token != cursor.current(data) {
		let Some(field_id) = cursor.expect_identifier(data) else {
			error::expected_token(data, "field name", cursor.index());
			return None;
		};
		cursor.expect(data, token::Kind::Colon)?;
		let Some(field_type) = cursor.expect_type(data) else {
			error::expected_token(data, "type-specifier", cursor.index());
			return None;
		};
		fields.push((field_id, field_type));
		if cursor.current(data) != token::Kind::Comma {
			break;
		}
		cursor.advance();
	}
	Some(fields)
}

fn discover_integer(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id, value: i64,
) -> Option<()> {
	cursor.advance();
	cursor.expect(data, token::Kind::ColonColon)?;
	cursor.advance(); // increment past the integer, as we already have it
	cursor.expect(data, token::Kind::Semicolon)?;
	data.values.insert(ident_id, Value::Integer(value));
	Some(())
}

fn discover_decimal(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id, value: f64,
) -> Option<()> {
	cursor.advance();
	cursor.expect(data, token::Kind::ColonColon)?;
	cursor.advance(); // increment past the decimal, as we already have it
	cursor.expect(data, token::Kind::Semicolon)?;
	data.values.insert(ident_id, Value::Decimal(value));
	Some(())
}

fn discover_region(cursor: &mut Cursor, data: &mut Data) -> Option<()> {
	cursor.expect(data, token::Kind::Region)?;
	let Some(ident_id) = cursor.expect_identifier(data) else {
		error::expected_token(data, "region name", cursor.index());
		return None;
	};
	cursor.expect(data, token::Kind::OBracket)?;
	let Some(byte_count) = cursor.expect_integer(data) else {
		error::expected_token(data, "region size", cursor.index());
		return None;
	};
	let byte_count = check_integer_as_u32(data, "valid region size", byte_count,
		(cursor.index() - 1).into()..cursor.index().into(),
	)?;
	cursor.expect(data, token::Kind::CBracket)?;
	cursor.expect(data, token::Kind::At)?;
	let Some(address) = cursor.expect_integer(data) else {
		error::expected_token(data, "region address", cursor.index());
		return None;
	};
	let address = check_integer_as_u32(data, "valid region address", address,
		(cursor.index() - 1).into()..cursor.index().into(),
	)?;
	cursor.expect(data, token::Kind::Semicolon)?;
	data.regions.insert(ident_id, Region {
		address,
		byte_count,
	});
	Some(())
}

fn discover_record(cursor: &mut Cursor, data: &mut Data) -> Option<()> {
	cursor.expect(data, token::Kind::Record)?;
	let Some(ident_id) = cursor.expect_identifier(data) else {
		error::expected_token(data, "record name", cursor.index());
		return None;
	};
	// TODO - srenshaw - Handle region allocation
	cursor.expect(data, token::Kind::OBrace)?;
	let fields = discover_fields(cursor, data, token::Kind::CBrace)?;
	cursor.expect(data, token::Kind::CBrace)?;
	let size = fields.iter()
		.map(|(_, field_type)| data.type_size(*field_type))
		.sum();
	data.records.insert(ident_id, 	Record { size, fields });
	Some(())
}

fn discover_table(cursor: &mut Cursor, data: &mut Data) -> Option<()> {
	cursor.expect(data, token::Kind::Table)?;
	let Some(ident_id) = cursor.expect_identifier(data) else {
		error::expected_token(data, "table name", cursor.index());
		return None;
	};
	cursor.expect(data, token::Kind::OBracket)?;
	let Some(row_count) = cursor.expect_integer(data) else {
		error::expected_token(data, "table size", cursor.index());
		return None;
	};
	let row_count = check_integer_as_u32(data, "valid table size", row_count,
		(cursor.index() - 1).into()..cursor.index().into(),
	)?;
	cursor.expect(data, token::Kind::CBracket)?;
	cursor.expect(data, token::Kind::OBrace)?;
	let column_spec = discover_fields(cursor, data, token::Kind::CBrace)?;
	cursor.expect(data, token::Kind::CBrace)?;
	let col_size: usize = column_spec.iter()
		.map(|(_, col_type)| data.type_size(*col_type))
		.sum();
	data.tables.insert(ident_id, Table {
		row_count,
		column_spec,
		size: row_count as usize * col_size,
	});
	Some(())
}

#[cfg(test)]
mod can_parse {
	use identifier::Identifier;

	use super::*;

	fn setup(source: &str) -> Data {
		let mut data = Data::new(file!().to_string(), source.into());
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
	fn procedure_locations() {
		let data = setup("a :: 5; proc b() {}");
		assert_eq!(data.procedures.len(), 1);
		assert_eq!(data.procedures[&"b".id()], Procedure {
			params: vec![],
			ret_type: crate::Type::Unit,
			tok_start: token::Id::new(4),
		});
	}

	#[test]
	fn procedure_with_params() {
		let data = setup("proc a(b: u8, c: s32) {}");
		assert_eq!(data.procedures.len(), 1);
		assert_eq!(data.procedures[&"a".id()], Procedure {
			params: vec![
				("b".id(), crate::Type::U8),
				("c".id(), crate::Type::S32),
			],
			ret_type: crate::Type::Unit,
			tok_start: token::Id::new(0),
		});
	}

	#[test]
	fn region() {
		let data = setup("region a[1024] @ 0x0020_0000;");
		assert_eq!(data.regions.len(), 1);
		assert_eq!(data.regions[&"a".id()], Region {
			byte_count: 1024,
			address: 0x0020_0000,
		});
	}

	#[test]
	fn empty_record() {
		let data = setup("record a {}");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], Record {
			size: 0,
			fields: vec![],
		});
	}

	#[test]
	fn record_with_one_field_no_trailing_comma() {
		let data = setup("record a { b: u8 }");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], Record {
			size: 1,
			fields: vec![("b".id(), crate::Type::U8)],
		});
	}

	#[test]
	fn record_with_one_field_and_trailing_comma() {
		let data = setup("record a { b: u8, }");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], Record {
			size: 1,
			fields: vec![("b".id(), crate::Type::U8)],
		});
	}

	#[test]
	fn record_with_multiple_fields() {
		let data = setup("record a { b: u8, c: s16 }");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], Record {
			size: 3,
			fields: vec![
				("b".id(), crate::Type::U8),
				("c".id(), crate::Type::S16),
			],
		});
	}

	#[test]
	fn record_with_user_defined_field() {
		let data = setup("record a {} record b { c: a }");
		assert_eq!(data.records.len(), 2);
		assert_eq!(data.records[&"b".id()], Record {
			size: 0,
			fields: vec![
				("c".id(), crate::Type::Record("a".id())),
			],
		});
	}

	#[test]
	fn empty_table() {
		let data = setup("table a[10] {}");
		assert_eq!(data.tables.len(), 1);
		assert_eq!(data.tables[&"a".id()], Table {
			row_count: 10,
			column_spec: vec![],
			size: 0,
		});
	}

	#[test]
	fn table_with_one_field() {
		let data = setup("table a[10] { b: u32 }");
		assert_eq!(data.tables.len(), 1);
		assert_eq!(data.tables[&"a".id()], Table {
			row_count: 10,
			column_spec: vec![("b".id(), crate::Type::U32)],
			size: 40,
		});
	}

	#[test]
	fn table_with_multiple_field() {
		let data = setup("table a[10] { b: u32, c: s16 }");
		assert_eq!(data.tables.len(), 1);
		assert_eq!(data.tables[&"a".id()], Table {
			row_count: 10,
			column_spec: vec![
				("b".id(), crate::Type::U32),
				("c".id(), crate::Type::S16),
			],
			size: 60,
		});
	}

	#[test]
	fn table_with_user_defined_field() {
		let data = setup("record a { a1: s16 } table b[10] { b1: a }");
		assert_eq!(data.tables.len(), 1);
		assert_eq!(data.tables[&"b".id()], Table {
			row_count: 10,
			column_spec: vec![
				("b1".id(), crate::Type::Record("a".id())),
			],
			size: 20,
		});
	}
}

