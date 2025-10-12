
use crate::identifier;
use crate::token;
use crate::{
	Data, MemoryLocation, ProcData, RingType, RowData, TableData,
	ValueKind,
};

pub fn eval(data: &mut Data) {
	let mut cursor = Cursor::default();
	let cursor = &mut cursor;

	loop {
		let kind = cursor.current(data);
		let start = cursor.index();
		cursor.advance();

		if let token::Kind::Identifier(ident_id) = kind {
			if &data.source[data.identifiers[&ident_id].clone()] == "main" {
				discover_main_proc(cursor, data, ident_id, start);
			} else if let token::Kind::Integer(value) = cursor.peek(data, 1) {
				discover_integer(cursor, data, ident_id, value);
			} else if let token::Kind::Decimal(value) = cursor.peek(data, 1) {
				discover_decimal(cursor, data, ident_id, value);
			} else if let token::Kind::Proc = cursor.peek(data, 1) {
				discover_proc(cursor, data, ident_id, start);
			} else if token::Kind::Record == cursor.peek(data, 1) {
				discover_record(cursor, data, ident_id);
			} else if token::Kind::Table == cursor.peek(data, 1) {
				discover_table(cursor, data, ident_id);
			}
		} else if token::Kind::Eof == kind {
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

fn error_expected_token(data: &mut Data, expected: &str, found: token::Kind) -> ! {
	error(data, &format!("Expected {expected}, found {found:?}"))
}

fn error_expected_token2(data: &mut Data, expected: token::Kind, found: token::Kind) -> ! {
	error(data, &format!("Expected {expected:?}, found {found:?}"))
}

#[derive(Default)]
struct Cursor(token::Id);

impl Cursor {
	pub fn index(&self) -> token::Id {
		self.0
	}

	pub fn advance(&mut self) {
		self.0 += 1;
	}

	pub fn peek(&self, data: &Data, offset: usize) -> token::Kind {
		data.tok_list
			.get(self.0 + offset)
			.copied()
			.unwrap_or(token::Kind::Eof)
	}

	pub fn current(&self, data: &Data) -> token::Kind {
		self.peek(data, 0)
	}

	fn expect(&mut self, data: &mut Data, expected: token::Kind) {
		let found = self.current(data);
		if found != expected {
			error_expected_token2(data, expected, found)
		}
		self.advance();
	}
}

fn check_braces(cursor: &mut Cursor, data: &mut Data) {
	let mut brace_count = 1;
	while brace_count > 0 && cursor.current(data) != token::Kind::Eof {
		brace_count += match cursor.current(data) {
			token::Kind::OBrace => 1,
			token::Kind::CBrace => -1,
			token::Kind::Eof => {
				error_expected_token(data, "end of procedure", token::Kind::Eof)
			}
			_ => 0,
		};
		cursor.advance();
	}
}

fn discover_main_proc(cursor: &mut Cursor, data: &mut Data, ident_id: identifier::Id, start: token::Id) {
	cursor.expect(data, token::Kind::OBrace);
	check_braces(cursor, data);
	data.procedures.insert(ident_id, ProcData {
		params: vec![], // `main` has no parameters 
		ret_type: RingType::Unit, // `main` has no return type
	});
	data.proc_start.insert(ident_id, start);
}

fn discover_proc(cursor: &mut Cursor, data: &mut Data, ident_id: identifier::Id, start: token::Id) {
	cursor.expect(data, token::Kind::ColonColon);
	cursor.expect(data, token::Kind::Proc);
	cursor.expect(data, token::Kind::OParen);
	let params = discover_fields(cursor, data, token::Kind::CParen);
	cursor.expect(data, token::Kind::CParen);
	// TODO - srenshaw - Handle return type declarations
	cursor.expect(data, token::Kind::OBrace);
	check_braces(cursor, data);
	data.procedures.insert(ident_id, ProcData {
		params,
		// TODO - srenshaw - Handle return type
		ret_type: RingType::Unit,
	});
	data.proc_start.insert(ident_id, start);
}

fn expect_type(data: &mut Data, kind: token::Kind) -> RingType {
	match kind {
		token::Kind::Identifier(ident_id) => {
			if data.records.contains_key(&ident_id) {
				RingType::Record(ident_id)
			} else if data.tables.contains_key(&ident_id) {
				RingType::Table(ident_id)
			} else {
				let found = &data.source[data.identifiers[&ident_id].clone()].to_string();
				error_expected(data, "type-specifier", found)
			}
		}
		token::Kind::Bool => RingType::Bool,
		token::Kind::U8 => RingType::U8,
		token::Kind::S8 => RingType::S8,
		token::Kind::U16 => RingType::U16,
		token::Kind::S16 => RingType::S16,
		token::Kind::U32 => RingType::U32,
		token::Kind::S32 => RingType::S32,
		kind => {
			error_expected_token(data, "type-specifier", kind)
		}
	}
}

fn discover_fields(cursor: &mut Cursor, data: &mut Data, end_token: token::Kind) -> RowData {
	let mut fields = RowData::default();
	while end_token != cursor.current(data) {
		let token::Kind::Identifier(field_id) = cursor.current(data) else {
			error_expected_token(data, "field name", cursor.current(data))
		};
		cursor.advance();
		cursor.expect(data, token::Kind::Colon);
		let field_type = expect_type(data, cursor.current(data));
		cursor.advance();
		fields.push((field_id, field_type));
		if cursor.current(data) != token::Kind::Comma {
			break;
		}
		cursor.advance();
	}
	fields
}

fn discover_integer(cursor: &mut Cursor, data: &mut Data, ident_id: identifier::Id, value: i64) {
	cursor.expect(data, token::Kind::ColonColon);
	cursor.advance(); // increment past the integer, as we already have it
	cursor.expect(data, token::Kind::Semicolon);
	data.values.insert(ident_id, ValueKind::Integer(value));
}

fn discover_decimal(cursor: &mut Cursor, data: &mut Data, ident_id: identifier::Id, value: f64) {
	cursor.expect(data, token::Kind::ColonColon);
	cursor.advance(); // increment past the decimal, as we already have it
	cursor.expect(data, token::Kind::Semicolon);
	data.values.insert(ident_id, ValueKind::Decimal(value));
}

fn discover_record(cursor: &mut Cursor, data: &mut Data, ident_id: identifier::Id) {
	cursor.expect(data, token::Kind::ColonColon);
	cursor.advance(); // skip over the 'record' keyword
	cursor.expect(data, token::Kind::OBrace);
	let fields = discover_fields(cursor, data, token::Kind::CBrace);
	cursor.expect(data, token::Kind::CBrace);
	data.records.insert(ident_id, 	fields);
}

fn discover_table(cursor: &mut Cursor, data: &mut Data, ident_id: identifier::Id) {
	cursor.expect(data, token::Kind::ColonColon);
	cursor.advance(); // skip over the 'table' keyword
	cursor.expect(data, token::Kind::OBracket);
	let token::Kind::Integer(row_count) = cursor.current(data) else {
		error_expected_token(data, "table size", cursor.current(data))
	};
	if !(0..u32::MAX as i64).contains(&row_count) {
		error_expected(data, "valid table size", &row_count.to_string())
	}
	cursor.advance();
	cursor.expect(data, token::Kind::CBracket);
	cursor.expect(data, token::Kind::At);
	let memory_location = match cursor.current(data) {
		token::Kind::Identifier(region) => MemoryLocation::Region(region),
		token::Kind::Integer(address) => {
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
	cursor.expect(data, token::Kind::OBrace);
	let row_spec = discover_fields(cursor, data, token::Kind::CBrace);
	cursor.expect(data, token::Kind::CBrace);
	data.tables.insert(ident_id, TableData {
		row_count: row_count as u32,
		memory_location,
		row_spec,
	});
}

#[cfg(test)]
mod can_parse {
	use identifier::Identifier;

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
		assert_eq!(data.proc_start[&"b".id()], 4, "{data}");
		assert_eq!(data.procedures[&"b".id()], ProcData {
			params: vec![],
			ret_type: RingType::Unit,
		});
	}

	#[test]
	fn procedure_with_params() {
		let data = setup("a :: proc(b: u8, c: s32) {}");
		assert_eq!(data.proc_start.len(), 1);
		assert_eq!(data.proc_start[&"a".id()], 0, "{data}");
		assert_eq!(data.procedures[&"a".id()], ProcData {
			params: vec![
				("b".id(), RingType::U8),
				("c".id(), RingType::S32),
			],
			ret_type: RingType::Unit,
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
		assert_eq!(data.tables[&"a".id()], TableData {
			row_count: 10,
			memory_location: MemoryLocation::Region("high_work_ram".id()),
			row_spec: vec![],
		});
	}
}

