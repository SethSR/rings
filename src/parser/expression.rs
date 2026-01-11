
use crate::lexer::Data as LexData;
use crate::token::Kind as TokenKind;

use super::cursor::Cursor;
use super::error::Error;
use super::value::{Value, ValueMap};

pub(super) fn evaluate_expr(cursor: &mut Cursor, lex_data: &LexData,
	values: &ValueMap,
	end_token: TokenKind,
) -> Result<Value, Error> {
	let val = parse_additive(cursor, lex_data, values, end_token)?;
	cursor.expect(lex_data, end_token)?;
	Ok(val)
}

fn parse_additive(cursor: &mut Cursor, lex_data: &crate::lexer::Data,
	values: &ValueMap,
	end_token: crate::token::Kind,
) -> Result<Value, Error> {
	let mut left = parse_multiplicative(cursor, lex_data, values, end_token)?;

	while cursor.current(lex_data) != end_token {
		match cursor.current(lex_data) {
			crate::token::Kind::Plus => {
				cursor.advance();
				let right = parse_multiplicative(cursor, lex_data, values, end_token)?;
				left = left + right;
			}
			crate::token::Kind::Dash => {
				cursor.advance();
				let right = parse_multiplicative(cursor, lex_data, values, end_token)?;
				left = left - right;
			}
			_ => break,
		}
	}

	Ok(left)
}

fn parse_multiplicative(cursor: &mut Cursor, lex_data: &crate::lexer::Data,
	values: &ValueMap,
	end_token: crate::token::Kind,
) -> Result<Value, Error> {
	let mut left = parse_primary(cursor, lex_data, values, end_token)?;

	while cursor.current(lex_data) != crate::token::Kind::Semicolon {
		match cursor.current(lex_data) {
			crate::token::Kind::Star => {
				cursor.advance();
				let right = parse_primary(cursor, lex_data, values, end_token)?;
				left = left * right;
			}
			crate::token::Kind::Slash => {
				cursor.advance();
				let location = cursor.index();
				let right = parse_primary(cursor, lex_data, values, end_token)?;
				match right {
					Value::Integer(0) |
					Value::Decimal(0.) => return Err(Error::DivisionByZero { location }),
					_ => {}
				}
				left = left / right;
			}
			crate::token::Kind::Percent => {
				cursor.advance();
				let location = cursor.index();
				let right = parse_primary(cursor, lex_data, values, end_token)?;
				match right {
					Value::Integer(0) |
					Value::Decimal(0.) => return Err(Error::DivisionByZero { location }),
					_ => {}
				}
				left = left % right;
			}
			_ => break,
		}
	}

	Ok(left)
}

fn parse_primary(cursor: &mut Cursor, lex_data: &crate::lexer::Data,
	values: &ValueMap,
	end_token: crate::token::Kind,
) -> Result<Value, Error> {
	match cursor.current(lex_data) {
		crate::token::Kind::Integer(n) => {
			cursor.advance();
			Ok(Value::Integer(n))
		}
		crate::token::Kind::Decimal(n) => {
			cursor.advance();
			Ok(Value::Decimal(n))
		}
		crate::token::Kind::Identifier(ident_id) => {
			let location = cursor.index();
			cursor.advance();
			values.get(&ident_id)
					.copied()
					.ok_or_else(|| Error::UndefinedVariable { location, ident_id })
		}
		crate::token::Kind::OParen => {
			cursor.advance();
			let val = parse_additive(cursor, lex_data, values, end_token)?;
			cursor.expect(lex_data, crate::token::Kind::CParen)?;
			Ok(val)
		}
		_ => Err(cursor.expected_token("Number, Identifier, or '('").into()),
	}
}
