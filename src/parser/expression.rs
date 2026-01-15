
use crate::token::{Id as TokenId, Kind as TokenKind};

use super::cursor::Cursor;
use super::error::Error;
use super::{Data, Value};

pub(super) fn evaluate_expr(cursor: &mut Cursor,
	data: &Data<TokenId>,
	end_token: TokenKind,
) -> Result<Value, Error> {
	let val = parse_additive(cursor, data, end_token)?;
	cursor.expect(end_token)?;
	Ok(val)
}

fn parse_additive(cursor: &mut Cursor,
	data: &Data<TokenId>,
	end_token: crate::token::Kind,
) -> Result<Value, Error> {
	let mut left = parse_multiplicative(cursor, data, end_token)?;

	while cursor.current() != end_token {
		match cursor.current() {
			crate::token::Kind::Plus => {
				cursor.advance();
				let right = parse_multiplicative(cursor, data, end_token)?;
				left = left + right;
			}
			crate::token::Kind::Dash => {
				cursor.advance();
				let right = parse_multiplicative(cursor, data, end_token)?;
				left = left - right;
			}
			_ => break,
		}
	}

	Ok(left)
}

fn parse_multiplicative(cursor: &mut Cursor,
	data: &Data<TokenId>,
	end_token: crate::token::Kind,
) -> Result<Value, Error> {
	let mut left = parse_primary(cursor, data, end_token)?;

	while cursor.current() != crate::token::Kind::Semicolon {
		match cursor.current() {
			crate::token::Kind::Star => {
				cursor.advance();
				let right = parse_primary(cursor, data, end_token)?;
				left = left * right;
			}
			crate::token::Kind::Slash => {
				cursor.advance();
				let location = cursor.index();
				let right = parse_primary(cursor, data, end_token)?;
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
				let right = parse_primary(cursor, data, end_token)?;
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

fn parse_primary(cursor: &mut Cursor,
	data: &Data<TokenId>,
	end_token: crate::token::Kind,
) -> Result<Value, Error> {
	match cursor.current() {
		crate::token::Kind::Integer(n) => {
			cursor.advance();
			Ok(Value::Integer(n))
		}
		crate::token::Kind::Decimal(n) => {
			cursor.advance();
			Ok(Value::Decimal(n))
		}
		crate::token::Kind::Identifier(ident_id) => {
			if data.records.contains_key(&ident_id) ||
					data.tables.contains_key(&ident_id) ||
					data.procedures.contains_key(&ident_id) ||
					data.regions.contains_key(&ident_id)
			{
				return Err(cursor.expected_token("value name"));
			}

			data.values.get(&ident_id)
					.copied()
					.inspect(|_| cursor.advance())
					.ok_or_else(|| Error::UndefinedType { location: cursor.index(), ident_id })
		}
		crate::token::Kind::OParen => {
			cursor.advance();
			let val = parse_additive(cursor, data, end_token)?;
			cursor.expect(crate::token::Kind::CParen)?;
			Ok(val)
		}
		_ => Err(cursor.expected_token("Number, Identifier, or '('").into()),
	}
}
