
use crate::identifier::Identifier;

use super::data::{Value, ValueMap};

fn setup(source: &str) -> Result<ValueMap, String> {
	let input = crate::input::eval(file!().into(), source.into());
	let lex_data = crate::lexer::eval(source)
		.map_err(|e| e.display(&input))?;
	super::eval(&input, &lex_data, false)
		.map_err(|e| e.display(&input))
		.map(|data| data.values)
}

#[test]
fn simple_value() {
	let values = setup("value x = 42;")
		.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(values.get(&"x".id()), Some(&Value::Integer(42)));
}

#[test]
fn constant_values() {
	let values = setup("value a = 3; value b = 4.2;")
		.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(values.len(), 2);
	assert_eq!(values.get(&"a".id()), Some(&Value::Integer(3)));
	assert_eq!(values.get(&"b".id()), Some(&Value::Decimal(4.2)));
}

#[test]
fn expression() {
	let values = setup("value x = 10 + 5 * 2;")
		.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(values.get(&"x".id()), Some(&Value::Integer(20)));
}

#[test]
fn out_of_order() {
	let values = setup("value y = x + 5; value x = 10;")
		.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(values.get(&"x".id()), Some(&Value::Integer(10)));
	assert_eq!(values.get(&"y".id()), Some(&Value::Integer(15)));
}

#[test]
fn undefined_variable() {
	let result = setup("value y = undefined;");
	assert!(result.is_err());
	let err = result.unwrap_err();
	assert!(format!("{err}").contains("undefined variable"));
}

#[test]
fn circular_dependency() {
	let result = setup("value a = b; value b = a;");
	assert!(result.is_err());
	let err = result.unwrap_err();
	assert!(format!("{err}").contains("circular dependency"));
}

#[test]
fn values_basic() {
	let values = setup("value a = 2;")
		.unwrap_or_else(|e| panic!("{e}"));
	let a_id = "a".id();
	assert!(values.contains_key(&a_id));
	assert_eq!(values[&a_id], Value::Integer(2));
}

#[test]
fn values_expressions() {
	let values = setup("value a = 2 * 5;")
		.unwrap_or_else(|e| panic!("{e}"));
	let a_id = "a".id();
	assert!(values.contains_key(&a_id));
	assert_eq!(values[&a_id], Value::Integer(10));
}

#[test]
fn values_compound() {
	let values = setup("value a = 3; value b = a * 5;")
		.unwrap_or_else(|e| panic!("{e}"));
	let a_id = "a".id();
	let b_id = "b".id();
	assert!(values.contains_key(&a_id));
	assert!(values.contains_key(&b_id));
	assert_eq!(values[&a_id], Value::Integer(3));
	assert_eq!(values[&b_id], Value::Integer(15));
}

#[test]
fn values_compound_inverted() {
	let values = setup("value b = a * 5; value a = 3;")
		.unwrap_or_else(|e| panic!("{e}"));
	let a_id = "a".id();
	let b_id = "b".id();
	assert!(values.contains_key(&a_id));
	assert!(values.contains_key(&b_id));
	assert_eq!(values[&a_id], Value::Integer(3));
	assert_eq!(values[&b_id], Value::Integer(15));
}
