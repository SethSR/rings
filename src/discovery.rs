
use crate::{Data, MemoryLocation, RingType, TokenKind, ValueKind};

pub fn eval(data: &mut Data) {
	let mut cursor = Cursor::default();
	let cursor = &mut cursor;

	loop {
		let kind = cursor.current(data);
		let start = cursor.index();
		cursor.advance();

		if let TokenKind::Identifier(ident_id) = kind {
			if &data.source[data.identifiers[&ident_id].clone()] == "main" {
				discover_main_proc(cursor, data, start);
			} else if let TokenKind::Integer(value) = cursor.peek(data, 1) {
				discover_integer(cursor, data, ident_id, value);
			} else if let TokenKind::Decimal(value) = cursor.peek(data, 1) {
				discover_decimal(cursor, data, ident_id, value);
			} else if let TokenKind::Proc = cursor.peek(data, 1) {
				discover_proc(cursor, data, start);
			} else if TokenKind::Record == cursor.peek(data, 1) {
				discover_record(cursor, data, ident_id);
			} else if TokenKind::Table == cursor.peek(data, 1) {
				discover_table(cursor, data, ident_id);
			}
		} else if TokenKind::Eof == kind {
			break;
		} else {
			error_expected_token(data, "identifier or EoF", kind)
		}
	}
}

fn error(data: &mut Data, msg: &str) -> ! {
	data.error = msg.to_string();
	panic!("{data}")
}

fn error_expected(data: &mut Data, expected: &str, found: &str) -> ! {
	error(data, &format!("Expected {expected}, found {found}"))
}

fn error_expected_token(data: &mut Data, expected: &str, found: TokenKind) -> ! {
	error(data, &format!("Expected {expected}, found {found:?}"))
}

fn error_expected_token2(data: &mut Data, expected: TokenKind, found: TokenKind) -> ! {
	error(data, &format!("Expected {expected:?}, found {found:?}"))
}

#[derive(Default)]
struct Cursor(usize);

impl Cursor {
	pub fn index(&self) -> usize {
		self.0
	}

	pub fn advance(&mut self) {
		self.0 += 1;
	}

	pub fn peek(&self, data: &Data, offset: usize) -> TokenKind {
		data.tok_list
			.get(self.0 + offset)
			.copied()
			.unwrap_or(TokenKind::Eof)
	}

	pub fn current(&self, data: &Data) -> TokenKind {
		self.peek(data, 0)
	}

	fn expect(&mut self, data: &mut Data, expected: TokenKind) {
		let found = self.current(data);
		if found != expected {
			error_expected_token2(data, expected, found)
		}
		self.advance();
	}
}

fn check_braces(cursor: &mut Cursor, data: &mut Data) {
	let mut brace_count = 1;
	while brace_count > 0 && cursor.current(data) != TokenKind::Eof {
		brace_count += match cursor.current(data) {
			TokenKind::OBrace => 1,
			TokenKind::CBrace => -1,
			TokenKind::Eof => {
				error_expected_token(data, "end of procedure", TokenKind::Eof)
			}
			_ => 0,
		};
		cursor.advance();
	}
}

fn discover_main_proc(cursor: &mut Cursor, data: &mut Data, start: usize) {
	cursor.expect(data, TokenKind::OBrace);
	check_braces(cursor, data);
	data.proc_start.push(start);
}

fn discover_proc(cursor: &mut Cursor, data: &mut Data, start: usize) {
	cursor.expect(data, TokenKind::ColonColon);
	cursor.expect(data, TokenKind::Proc);
	cursor.expect(data, TokenKind::OParen);
	// TODO - srenshaw - Handle parameter lists in function declarations
	cursor.expect(data, TokenKind::CParen);
	// TODO - srenshaw - Handle return type declarations
	cursor.expect(data, TokenKind::OBrace);
	check_braces(cursor, data);
	data.proc_start.push(start);
}

fn expect_type(data: &mut Data, kind: TokenKind) -> RingType {
	match kind {
		TokenKind::Identifier(ident_id) => {
			if data.records.contains_key(&ident_id) {
				RingType::Record(ident_id)
			} else if data.tables.contains_key(&ident_id) {
				RingType::Table(ident_id)
			} else {
				let found = &data.source[data.identifiers[&ident_id].clone()].to_string();
				error_expected(data, "type-specifier", found)
			}
		}
		TokenKind::Bool => RingType::Bool,
		TokenKind::U8 => RingType::U8,
		TokenKind::S8 => RingType::S8,
		TokenKind::U16 => RingType::U16,
		TokenKind::S16 => RingType::S16,
		TokenKind::U32 => RingType::U32,
		TokenKind::S32 => RingType::S32,
		kind => {
			error_expected_token(data, "type-specifier", kind)
		}
	}
}

fn discover_fields(cursor: &mut Cursor, data: &mut Data) -> crate::RowData {
	let mut fields = crate::RowData::default();
	while TokenKind::CBrace != cursor.current(data) {
		let TokenKind::Identifier(field_id) = cursor.current(data) else {
			error_expected_token(data, "field name", cursor.current(data))
		};
		cursor.advance();
		cursor.expect(data, TokenKind::Colon);
		let field_type = expect_type(data, cursor.current(data));
		cursor.advance();
		fields.push((field_id, field_type));
		if cursor.current(data) != TokenKind::Comma {
			break;
		}
		cursor.advance();
	}
	fields
}

fn discover_integer(cursor: &mut Cursor, data: &mut Data, ident_id: u64, value: i64) {
	cursor.expect(data, TokenKind::ColonColon);
	cursor.advance(); // increment past the integer, as we already have it
	cursor.expect(data, TokenKind::Semicolon);
	data.values.insert(ident_id, ValueKind::Integer(value));
}

fn discover_decimal(cursor: &mut Cursor, data: &mut Data, ident_id: u64, value: f64) {
	cursor.expect(data, TokenKind::ColonColon);
	cursor.advance(); // increment past the decimal, as we already have it
	cursor.expect(data, TokenKind::Semicolon);
	data.values.insert(ident_id, ValueKind::Decimal(value));
}

fn discover_record(cursor: &mut Cursor, data: &mut Data, ident_id: u64) {
	cursor.expect(data, TokenKind::ColonColon);
	cursor.advance(); // skip over the 'record' keyword
	cursor.expect(data, TokenKind::OBrace);
	let fields = discover_fields(cursor, data);
	cursor.expect(data, TokenKind::CBrace);
	data.records.insert(ident_id, 	fields);
}

fn discover_table(cursor: &mut Cursor, data: &mut Data, ident_id: u64) {
	cursor.expect(data, TokenKind::ColonColon);
	cursor.advance(); // skip over the 'table' keyword
	cursor.expect(data, TokenKind::OBracket);
	let TokenKind::Integer(row_count) = cursor.current(data) else {
		error_expected_token(data, "table size", cursor.current(data))
	};
	if !(0..u32::MAX as i64).contains(&row_count) {
		error_expected(data, "valid table size", &row_count.to_string())
	}
	cursor.advance();
	cursor.expect(data, TokenKind::CBracket);
	cursor.expect(data, TokenKind::At);
	let memory_location = match cursor.current(data) {
		TokenKind::Identifier(region) => MemoryLocation::Region(region),
		TokenKind::Integer(address) => {
			if !(0..u32::MAX as i64).contains(&address) {
				error_expected(data, "memory region or valid address", &address.to_string())
			}
			MemoryLocation::Address(address as u32)
		}
		kind => {
			error_expected_token(data, "memory region or address", kind)
		}
	};
	cursor.advance();
	cursor.expect(data, TokenKind::OBrace);
	let row_spec = discover_fields(cursor, data);
	cursor.expect(data, TokenKind::CBrace);
	data.tables.insert(ident_id, crate::TableData {
		row_count: row_count as u32,
		memory_location,
		row_spec,
	});
}

#[cfg(test)]
mod can_parse {
	use crate::Identifier;

	use super::*;

	fn setup(source: &str) -> Data {
		let mut data = Data::new(source.into());
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
		assert_eq!(data.proc_start[0], 4, "{data}");
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
		assert_eq!(data.records[&"a".id()], vec![("b".id(), RingType::U8)]);
	}

	#[test]
	fn record_with_one_field_and_trailing_comma() {
		let data = setup("a :: record { b: u8, }");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], vec![("b".id(), RingType::U8)]);
	}

	#[test]
	fn record_with_multiple_fields() {
		let data = setup("a :: record { b: u8, c: s16 }");
		assert_eq!(data.records.len(), 1);
		assert_eq!(data.records[&"a".id()], vec![
			("b".id(), RingType::U8),
			("c".id(), RingType::S16),
		]);
	}

	#[test]
	fn record_with_user_defined_field() {
		let data = setup("a :: record {} b :: record { c: a }");
		assert_eq!(data.records.len(), 2);
		assert_eq!(data.records[&"b".id()], vec![
			("c".id(), RingType::Record("a".id())),
		]);
	}

	#[test]
	fn empty_table() {
		let data = setup("a :: table[10] @ high_work_ram {}");
		assert_eq!(data.tables.len(), 1);
		assert_eq!(data.tables[&"a".id()], crate::TableData {
			row_count: 10,
			memory_location: MemoryLocation::Region("high_work_ram".id()),
			row_spec: vec![],
		});
	}
}

