
use crate::identifier::Identifier;

use super::*;

fn setup(source: &str) -> Result<RecordMap, String> {
	let input = crate::input::eval(file!().into(), source.into());
	let lex_data = crate::lexer::eval(source)
		.map_err(|e| e.display(&input))?;
	eval(&input, &lex_data, false)
		.map_err(|e| e.display(&input))
		.map(|data| data.records)
}

#[test]
fn empty_record() {
	let records = setup("record a {}")
		.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(records.len(), 1);
	let record = &records[&"a".id()];
	assert_eq!(record.placement, None);
	assert_eq!(record.fields.len(), 0);
}

#[test]
fn simple_records() {
	let records = setup("
		record Person { name: s8, age: s8 }
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(records.len(), 1);
	let record = &records[&"Person".id()];
	assert_eq!(record.placement, None);
	assert_eq!(record.fields, [
		("name".id(), Type::S8),
		("age".id(), Type::S8),
	]);
}

#[test]
fn nested_records() {
	let records = setup("
		record Address { street: s8 }
		record Person { addr: Address }
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(records.len(), 2);
	assert!(records.contains_key(&"Address".id()));
	assert!(records.contains_key(&"Person".id()));
}

#[test]
fn recursive_detection() {
	let result = setup("
		record Node { next: Node }
	");
	assert!(result.is_err());
	let err = result.unwrap_err();
	assert!(format!("{err}").contains("circular dependency"), "{err}");
}

#[test]
fn indirect_recursion() {
	let result = setup("
		record A { b: B }
		record B { a: A }
	");
	assert!(result.is_err());
	let err = result.unwrap_err();
	assert!(format!("{err}").contains("circular dependency"), "{err}");
}

#[test]
fn record_with_one_field_no_trailing_comma() {
	let records = setup("record a { b: s8 }")
		.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(records.len(), 1);
	let record = &records[&"a".id()];
	assert_eq!(record.placement, None);
	assert_eq!(record.fields, [
		("b".id(), Type::S8),
	]);
}

#[test]
fn record_with_one_field_and_trailing_comma() {
	let records = setup("record a { b: s8, }")
		.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(records.len(), 1);
	let record = &records[&"a".id()];
	assert_eq!(record.placement, None);
	assert_eq!(record.fields, [
		("b".id(), Type::S8),
	]);
}

#[test]
fn record_with_multiple_fields() {
	let records = setup("record a { b: s8, c: s8 }")
		.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(records.len(), 1);
	let record = &records[&"a".id()];
	assert_eq!(record.placement, None);
	assert_eq!(record.fields, [
		("b".id(), Type::S8),
		("c".id(), Type::S8),
	]);
}

#[test]
fn record_with_user_defined_field() {
	let records = setup("
		record a {}
		record b { c: a }
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(records.len(), 2);
	let record = &records[&"b".id()];
	assert_eq!(record.placement, None);
	assert_eq!(record.fields, [
		("c".id(), Type::Record("a".id())),
	]);
}

#[test]
fn record_with_region() {
	let records = setup("
		region b[32..40];
		record a in b {}
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(records.len(), 1);
	let record = &records[&"a".id()];
	assert_eq!(record.placement, Some(MemoryPlacement::Region("b".id())));
	assert!(record.fields.is_empty());
}

#[test]
#[should_panic="Expected address expression"]
fn record_with_at_region() {
	let records = setup("
		region b[32..40];
		record a @ b {}
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(records.len(), 1);
	let record = &records[&"a".id()];
	assert_eq!(record.placement, Some(MemoryPlacement::Region("b".id())));
	assert!(record.fields.is_empty());
}
