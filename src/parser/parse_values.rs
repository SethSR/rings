
use std::collections::{HashSet, VecDeque};

use crate::identifier::Id as IdentId;
use crate::lexer::Data as LexData;
use crate::token::{Id as TokenId, Kind as TokenKind};

use super::cursor::Cursor;
use super::error::Error;
use super::expression::evaluate_expr;
use super::value::ValueMap;
use super::skip_through;

struct Task { ident: IdentId, start: TokenId }

pub fn eval(lex_data: &LexData,
) -> Result<ValueMap, Error> {
	let tasks = scan(lex_data)?;
	process(lex_data, tasks)
}

fn scan(lex_data: &LexData,
) -> Result<Vec<Task>, Error> {
	let mut tasks = vec![];
	let mut cursor = Cursor::default();

	while cursor.current(lex_data) != TokenKind::Eof {
		if cursor.expect(lex_data, TokenKind::Value).is_err() {
			// Skip non-value constructs
			cursor.advance();
			continue;
		}

		let ident = cursor.expect_identifier(lex_data, "value name")?;
		cursor.expect(lex_data, TokenKind::Eq)?;

		let start = cursor.index();
		skip_through(&mut cursor, lex_data, TokenKind::Semicolon)?;

		tasks.push(Task { ident, start });
	}

	Ok(tasks)
}

fn process(lex_data: &LexData,
	tasks: Vec<Task>,
) -> Result<ValueMap, Error> {
	let mut values = ValueMap::default();
	let mut queue: VecDeque<Task> = tasks.into_iter().collect();
	let mut failed_tasks: HashSet<IdentId> = HashSet::new();
	let mut consecutive_failures = 0;

	while let Some(task) = queue.pop_front() {
		match evaluate_expr(&mut Cursor::new(task.start), lex_data, &values, TokenKind::Semicolon) {
			Ok(value) => {
				values.insert(task.ident, value);
				failed_tasks.remove(&task.ident);
				consecutive_failures = 0;
			}
			Err(Error::UndefinedVariable { location, ident_id }) => {
				if failed_tasks.contains(&task.ident) {
					consecutive_failures += 1;

					if consecutive_failures > queue.len() {
						return Err(Error::CircularDependency {
							location,
							name_id: task.ident,
							ident_id,
						});
					}
				} else {
					failed_tasks.insert(task.ident);
				}

				queue.push_back(task);
			}
			Err(e) => return Err(e),
		}
	}

	Ok(values)
}

#[cfg(test)]
mod tests {
	use crate::identifier::Identifier;
	use crate::error::Kind;

	use super::super::value::Value;
	use super::*;

	fn setup(source: &str) -> Result<ValueMap, String> {
		let input = crate::input::eval(file!().into(), source.into());
		let lex_data = crate::lexer::eval(source)
			.map_err(|e| e.display(&input))?;
		eval(&lex_data)
			.map_err(|e| e.into_comp_error(&input, &lex_data, Kind::Parser))
			.map_err(|e| e.display(&input))
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
}
