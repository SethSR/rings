
use crate::identifier::Identifier;
use crate::parser::TableMap;

use super::{MemoryPlacement, Type};

fn setup(source: &str) -> Result<TableMap, String> {
	let input = crate::input::eval(file!().into(), source.into());
	let lex_data = crate::lexer::eval(source)
			.map_err(|e| e.display(&input))?;
	super::eval(&input, &lex_data, false)
			.map_err(|e| e.display(&input))
			.map(|data| data.tables)
}

#[test]
fn minimal() {
	let tables = setup("table users[10] {}")
			.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(tables.len(), 1);
	let table = tables.get(&"users".id())
			.expect("missing users table");
	assert_eq!(table.row_count, 10);
	assert_eq!(table.placement, None);
	assert_eq!(table.fields, []);
}

#[test]
fn single_field() {
	let tables = setup("
		table users[10] { id: s32 }
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(tables.len(), 1);
	let table = tables.get(&"users".id())
			.expect("missing users table");
	assert_eq!(table.row_count, 10);
	assert_eq!(table.placement, None);
	assert_eq!(table.fields, [
		("id".id(), Type::S32),
	]);
}

#[test]
fn multiple_fields() {
	let tables = setup("
		table users[100] { id: u32, name: u16, age: u8 }
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(tables.len(), 1);
	let table = tables.get(&"users".id())
			.expect("missing users table");
	assert_eq!(table.row_count, 100);
	assert_eq!(table.placement, None);
	assert_eq!(table.fields, [
		("id".id(), Type::U32),
		("name".id(), Type::U16),
		("age".id(), Type::U8),
	]);
}

#[test]
fn trailing_comma() {
	let tables = setup("
		table users[80] { id: u32, name: u16, }
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(tables.len(), 1);
	let table = tables.get(&"users".id())
			.expect("missing users table");
	assert_eq!(table.row_count, 80);
	assert_eq!(table.placement, None);
	assert_eq!(table.fields, [
		("id".id(), Type::U32),
		("name".id(), Type::U16),
	]);
}

#[test]
fn with_placement() {
	let tables = setup("
		region memory[0] @ 0x00;
		table users[10] in memory { id: u32 }
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(tables.len(), 1);
	let table = tables.get(&"users".id())
			.expect("missing users table");
	assert_eq!(table.row_count, 10);
	assert_eq!(table.placement, Some(MemoryPlacement::Region("memory".id())));
	assert_eq!(table.fields, [
		("id".id(), Type::U32),
	]);
}

#[test]
fn with_placement_expression() {
	let tables = setup("
		value base = 0x1000;
		table users[10] @ base + 35 * 4 {}
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(tables.len(), 1);
	let table = tables.get(&"users".id())
			.expect("missing users table");
	assert_eq!(table.row_count, 10);
	assert_eq!(table.placement, Some(MemoryPlacement::Address(0x108C)));
	assert_eq!(table.fields, []);
}

#[test]
fn with_row_count_expression() {
	let tables = setup("
		value count = 25;
		table users[count + 18] {}
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(tables.len(), 1);
	let table = tables.get(&"users".id())
			.expect("missing users table");
	assert_eq!(table.row_count, 43);
	assert_eq!(table.placement, None);
	assert_eq!(table.fields, []);
}

#[test]
#[should_panic="Expected table name"]
fn missing_name() {
	setup("table [10] {}")
			.unwrap_or_else(|e| panic!("{e}"));
}

#[test]
#[should_panic="Expected capacity expression"]
fn missing_capacity() {
	setup("table users[] {}")
			.unwrap_or_else(|e| panic!("{e}"));
}

#[test]
#[should_panic="Expected region name"]
fn missing_region_placement() {
	setup("table users[10] in {}")
			.unwrap_or_else(|e| panic!("{e}"));
}

#[test]
#[should_panic="Expected address expression"]
fn missing_address_placement() {
	setup("table users[10] @ {}")
			.unwrap_or_else(|e| panic!("{e}"));
}

#[test]
#[should_panic="out of range"]
fn negative_capacity() {
	setup("table users[-10] @ {}")
			.unwrap_or_else(|e| panic!("{e}"));
}

#[test]
fn unicode_identifiers() {
	let tables = setup("
		table 用户[10] { 名字: s32 }
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(tables.len(), 1);
	let table = tables.get(&"用户".id())
			.expect("missing table");
	assert_eq!(table.fields, [
		("名字".id(), Type::S32),
	]);
}
