
use std::collections::{HashMap, HashSet, VecDeque};

use crate::cursor::{Cursor, Error};
use crate::identifier::Id as IdentId;
use crate::lexer::Data as LexData;
use crate::parser::Value;
use crate::token::{Id as TokenId, Kind as TokenKind};
use crate::{text, token_source};

pub enum ParseError {
	Cursor(Error),
	UnexpectedEof { location: TokenId },
	DivisionByZero { location: TokenId },
	UndefinedVariable { location: TokenId, ident_id: IdentId },
	CircularDependency { location: TokenId, value_id: IdentId, ident_id: IdentId },
}

impl ParseError {
	pub fn into_comp_error(self,
		input: &crate::input::Data,
		lex_data: &LexData,
		err_kind: crate::error::Kind,
	) -> crate::error::Error {
		match self {
			Self::Cursor(e) => {
				e.into_comp_error(input, lex_data, err_kind)
			}
			Self::UnexpectedEof { location } => {
				let span = token_source(input, lex_data, location);
				crate::error::Error::new(span, "unexpected EOF")
			}
			Self::DivisionByZero { location } => {
				let span = token_source(input, lex_data, location);
				crate::error::Error::new(span, "division by zero")
			}
			Self::UndefinedVariable { location , ident_id } => {
				let span = token_source(input, lex_data, location);
				let message = format!("undefined variable `{}`",
					text(input, lex_data, &ident_id));
				crate::error::Error::new(span, message)
			}
			Self::CircularDependency { location , value_id, ident_id } => {
				let span = token_source(input, lex_data, location);
				let message = format!("Cannot resolve '{}' - circular dependency or undefined variable '{}'",
					text(input, lex_data, &value_id), text(input, lex_data, &ident_id));
				crate::error::Error::new(span, message)
			}
		}.with_kind(err_kind)
	}
}

impl From<Error> for ParseError {
	fn from(e: Error) -> Self {
		Self::Cursor(e)
	}
}

struct Task { ident: IdentId, start: TokenId }

pub fn eval(lex_data: &LexData,
) -> Result<HashMap<IdentId, Value>, ParseError> {
	let tasks = scan(lex_data)?;
	process(lex_data, tasks)
}

fn scan(lex_data: &LexData,
) -> Result<Vec<Task>, ParseError> {
	let mut tasks = vec![];
	let mut cursor = Cursor::default();

	while cursor.current(lex_data) != TokenKind::Eof {
		if cursor.expect(lex_data, TokenKind::Value).is_err() {
			// Skip non-value constructs
			cursor.advance();
			continue;
		}
		let ident = cursor.expect_identifier(lex_data, "identifier")?;
		cursor.expect(lex_data, TokenKind::Eq)?;
		let start_token = scan_expr_tokens(&mut cursor, lex_data)?;
		tasks.push(Task { ident, start: start_token });
	}

	Ok(tasks)
}

fn scan_expr_tokens(cursor: &mut Cursor, lex_data: &LexData,
) -> Result<TokenId, ParseError> {
	let start = cursor.index();

	loop {
		match cursor.current(lex_data) {
			TokenKind::Eof => return Err(ParseError::UnexpectedEof { location: cursor.index() }),
			TokenKind::Semicolon => break cursor.advance(),
			_ => cursor.advance(),
		}
	}

	Ok(start)
}

fn process(lex_data: &LexData,
	tasks: Vec<Task>,
) -> Result<HashMap<IdentId, Value>, ParseError> {
	let mut values = HashMap::new();
	let mut queue: VecDeque<Task> = tasks.into_iter().collect();
	let mut failed_tasks: HashSet<IdentId> = HashSet::new();
	let mut consecutive_failures = 0;

	while let Some(task) = queue.pop_front() {
		match evaluate_expr(lex_data, task.start, &values) {
			Ok(value) => {
				values.insert(task.ident, value);
				failed_tasks.remove(&task.ident);
				consecutive_failures = 0;
			}
			Err(ParseError::UndefinedVariable { location, ident_id }) => {
				if failed_tasks.contains(&task.ident) {
					consecutive_failures += 1;

					if consecutive_failures > queue.len() {
						return Err(ParseError::CircularDependency {
							location,
							value_id: task.ident,
							ident_id,
						});
					}
				} else {
					failed_tasks.insert(task.ident);
				}

				queue.push_back(task);
			}
			Err(e) => {
				return Err(e);
			}
		}
	}

	Ok(values)
}

fn evaluate_expr(lex_data: &LexData,
	start: TokenId,
	values: &HashMap<IdentId, Value>,
) -> Result<Value, ParseError> {
	let mut cursor = Cursor::new(start);
	let val = parse_additive(&mut cursor, lex_data, values)?;
	cursor.expect(lex_data, TokenKind::Semicolon)?;
	Ok(val)
}

fn parse_additive(cursor: &mut Cursor, lex_data: &LexData,
	values: &HashMap<IdentId, Value>,
) -> Result<Value, ParseError> {
	let mut left = parse_multiplicative(cursor, lex_data, values)?;

	while cursor.current(lex_data) != TokenKind::Semicolon {
		match cursor.current(lex_data) {
			TokenKind::Plus => {
				cursor.advance();
				let right = parse_multiplicative(cursor, lex_data, values)?;
				left = left + right;
			}
			TokenKind::Dash => {
				cursor.advance();
				let right = parse_multiplicative(cursor, lex_data, values)?;
				left = left - right;
			}
			_ => break,
		}
	}

	Ok(left)
}

fn parse_multiplicative(cursor: &mut Cursor, lex_data: &LexData,
	values: &HashMap<IdentId, Value>,
) -> Result<Value, ParseError> {
	let mut left = parse_primary(cursor, lex_data, values)?;

	while cursor.current(lex_data) != TokenKind::Semicolon {
		match cursor.current(lex_data) {
			TokenKind::Star => {
				cursor.advance();
				let right = parse_primary(cursor, lex_data, values)?;
				left = left * right;
			}
			TokenKind::Slash => {
				cursor.advance();
				let location = cursor.index();
				let right = parse_primary(cursor, lex_data, values)?;
				match right {
					Value::Integer(0) |
					Value::Decimal(0.) => return Err(ParseError::DivisionByZero { location }),
					_ => {}
				}
				left = left / right;
			}
			TokenKind::Percent => {
				cursor.advance();
				let location = cursor.index();
				let right = parse_primary(cursor, lex_data, values)?;
				match right {
					Value::Integer(0) |
					Value::Decimal(0.) => return Err(ParseError::DivisionByZero { location }),
					_ => {}
				}
				left = left % right;
			}
			_ => break,
		}
	}

	Ok(left)
}

fn parse_primary(cursor: &mut Cursor, lex_data: &LexData,
	values: &HashMap<IdentId, Value>,
) -> Result<Value, ParseError> {
	match cursor.current(lex_data) {
		TokenKind::Integer(n) => {
			cursor.advance();
			Ok(Value::Integer(n))
		}
		TokenKind::Decimal(n) => {
			cursor.advance();
			Ok(Value::Decimal(n))
		}
		TokenKind::Identifier(ident_id) => {
			let location = cursor.index();
			cursor.advance();
			values.get(&ident_id)
				.copied()
				.ok_or_else(|| ParseError::UndefinedVariable { location, ident_id })
		}
		TokenKind::OParen => {
			cursor.advance();
			let val = parse_additive(cursor, lex_data, values)?;
			cursor.expect(lex_data, TokenKind::CParen)?;
			Ok(val)
		}
		_ => Err(cursor.expected_token("Number, Identifier, or '('").into()),
	}
}

#[cfg(test)]
mod tests {
	use crate::identifier::Identifier;
	use crate::error::Kind;
	use super::*;

	fn setup(source: &str) -> Result<HashMap<IdentId, Value>, String> {
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
