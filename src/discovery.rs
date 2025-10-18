
use std::ops::Range;

use crate::cursor::Cursor;
use crate::error;
use crate::identifier;
use crate::token;
use crate::{Data, ValueKind};

pub type ColumnData = Vec<(identifier::Id, crate::Type)>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RegionData {
	pub address: u32,
	pub byte_count: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcType {
	pub params: Vec<(identifier::Id, crate::Type)>,
	pub ret_type: crate::Type,
}

#[derive(Debug, PartialEq)]
pub struct TableData {
	pub row_count: u32,
	pub column_spec: ColumnData,
}

pub fn eval(data: &mut Data) {
	let mut cursor = Cursor::default();
	let cursor = &mut cursor;

	loop {
		let start = cursor.index();
		match cursor.current(data) {
			token::Kind::Identifier(ident_id) => {
				let result = if data.text(ident_id) == "main" {
					discover_main_proc(cursor, data, ident_id, start)
				} else {
					match cursor.peek(data, 2) {
						token::Kind::Integer(value) => discover_integer(cursor, data, ident_id, value),
						token::Kind::Decimal(value) => discover_decimal(cursor, data, ident_id, value),
						token::Kind::Region => discover_region(cursor, data, ident_id),
						token::Kind::Proc => discover_proc(cursor, data, ident_id, start),
						token::Kind::Record => discover_record(cursor, data, ident_id),
						token::Kind::Table => discover_table(cursor, data, ident_id),
						_ => {
							error::expected_token(data, "top-level statement", cursor.index() + 2);
							return;
						}
					}
				};

				if result.is_none() {
					break;
				}
			}
			token::Kind::Eof => break,
			_ => {
				error::expected_token(data, "identifier or EoF", cursor.index());
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

fn discover_main_proc(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id, start: token::Id,
) -> Option<()> {
	cursor.advance();
	cursor.expect(data, token::Kind::OBrace)?;
	check_braces(cursor, data)?;
	data.procedures.insert(ident_id, ProcType {
		params: vec![], // `main` has no parameters 
		ret_type: crate::Type::Unit, // `main` has no return type
	});
	data.proc_start.insert(ident_id, start);
	Some(())
}

fn discover_proc(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id, start: token::Id,
) -> Option<()> {
	cursor.advance();
	cursor.expect(data, token::Kind::ColonColon)?;
	cursor.expect(data, token::Kind::Proc)?;
	cursor.expect(data, token::Kind::OParen)?;
	let params = discover_fields(cursor, data, token::Kind::CParen)?;
	cursor.expect(data, token::Kind::CParen)?;
	// TODO - srenshaw - Handle return type declarations
	cursor.expect(data, token::Kind::OBrace)?;
	check_braces(cursor, data);
	data.procedures.insert(ident_id, ProcType {
		params,
		// TODO - srenshaw - Handle return type
		ret_type: crate::Type::Unit,
	});
	data.proc_start.insert(ident_id, start);
	Some(())
}

fn discover_fields(cursor: &mut Cursor, data: &mut Data,
	end_token: token::Kind,
) -> Option<ColumnData> {
	let mut fields = ColumnData::default();
	while end_token != cursor.current(data) {
		let field_id = cursor.expect_identifier(data, "field name")?;
		cursor.expect(data, token::Kind::Colon)?;
		let field_type = cursor.expect_type(data, "type-specifier")?;
		cursor.advance();
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
	data.values.insert(ident_id, ValueKind::Integer(value));
	Some(())
}

fn discover_decimal(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id, value: f64,
) -> Option<()> {
	cursor.advance();
	cursor.expect(data, token::Kind::ColonColon)?;
	cursor.advance(); // increment past the decimal, as we already have it
	cursor.expect(data, token::Kind::Semicolon)?;
	data.values.insert(ident_id, ValueKind::Decimal(value));
	Some(())
}

fn discover_region(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id,
) -> Option<()> {
	cursor.advance();
	cursor.expect(data, token::Kind::ColonColon)?;
	cursor.advance(); // skip over the 'region' keyword
	cursor.expect(data, token::Kind::OBracket)?;
	let byte_count = cursor.expect_integer(data, "region size")?;
	let byte_count = check_integer_as_u32(data, "valid region size", byte_count,
		(cursor.index() - 1).into()..cursor.index().into(),
	)?;
	cursor.expect(data, token::Kind::CBracket)?;
	cursor.expect(data, token::Kind::At)?;
	let address = cursor.expect_integer(data, "region address")?;
	let address = check_integer_as_u32(data, "valid region address", address,
		(cursor.index() - 1).into()..cursor.index().into(),
	)?;
	cursor.expect(data, token::Kind::Semicolon)?;
	data.regions.insert(ident_id, RegionData {
		address,
		byte_count,
	});
	Some(())
}

fn discover_record(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id,
) -> Option<()> {
	cursor.advance();
	cursor.expect(data, token::Kind::ColonColon)?;
	cursor.advance(); // skip over the 'record' keyword
	cursor.expect(data, token::Kind::OBrace)?;
	let fields = discover_fields(cursor, data, token::Kind::CBrace)?;
	cursor.expect(data, token::Kind::CBrace)?;
	let size = fields.iter()
		.map(|(_, field_type)| data.type_size(*field_type))
		.sum();
	data.records.insert(ident_id, 	fields);
	data.record_sizes.insert(ident_id, size);
	Some(())
}

fn discover_table(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id,
) -> Option<()> {
	cursor.advance();
	cursor.expect(data, token::Kind::ColonColon)?;
	cursor.advance(); // skip over the 'table' keyword
	cursor.expect(data, token::Kind::OBracket)?;
	let row_count = cursor.expect_integer(data, "table size")?;
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
	data.tables.insert(ident_id, TableData {
		row_count,
		column_spec,
	});
	data.table_sizes.insert(ident_id, row_count as usize * col_size);
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
		assert_eq!(data.values[&"a".id()], ValueKind::Integer(3), "{data}");
		assert_eq!(data.values[&"b".id()], ValueKind::Decimal(4.2), "{data}");
	}

	#[test]
	fn procedure_locations() {
		let data = setup("a :: 5; b :: proc() {}");
		assert_eq!(data.proc_start.len(), 1);
		assert_eq!(data.proc_start[&"b".id()], 4, "{data}");
		assert_eq!(data.procedures[&"b".id()], ProcType {
			params: vec![],
			ret_type: crate::Type::Unit,
		});
	}

	#[test]
	fn procedure_with_params() {
		let data = setup("a :: proc(b: u8, c: s32) {}");
		assert_eq!(data.proc_start.len(), 1);
		assert_eq!(data.proc_start[&"a".id()], 0, "{data}");
		assert_eq!(data.procedures[&"a".id()], ProcType {
			params: vec![
				("b".id(), crate::Type::U8),
				("c".id(), crate::Type::S32),
			],
			ret_type: crate::Type::Unit,
		});
	}

	#[test]
	fn region() {
		let data = setup("a :: region[1024] at 0x0020_0000;");
		assert_eq!(data.regions.len(), 1);
		assert_eq!(data.regions[&"a".id()], RegionData {
			byte_count: 1024,
			address: 0x0020_0000,
		});
	}

	#[test]
	fn empty_record() {
		let data = setup("a :: record {}");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], vec![]);
	}

	#[test]
	fn record_with_one_field_no_trailing_comma() {
		let data = setup("a :: record { b: u8 }");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], vec![("b".id(), crate::Type::U8)]);
	}

	#[test]
	fn record_with_one_field_and_trailing_comma() {
		let data = setup("a :: record { b: u8, }");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], vec![("b".id(), crate::Type::U8)]);
	}

	#[test]
	fn record_with_multiple_fields() {
		let data = setup("a :: record { b: u8, c: s16 }");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], vec![
			("b".id(), crate::Type::U8),
			("c".id(), crate::Type::S16),
		]);
	}

	#[test]
	fn record_with_user_defined_field() {
		let data = setup("a :: record {} b :: record { c: a }");
		assert_eq!(data.records.len(), 2);
		assert_eq!(data.records[&"b".id()], vec![
			("c".id(), crate::Type::Record("a".id())),
		]);
	}

	#[test]
	fn empty_table() {
		let data = setup("a :: table[10] {}");
		assert_eq!(data.tables.len(), 1);
		assert_eq!(data.tables[&"a".id()], TableData {
			row_count: 10,
			column_spec: vec![],
		});
	}
}

