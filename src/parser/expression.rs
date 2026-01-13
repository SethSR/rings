
use crate::token::Kind as TokenKind;

use super::cursor::Cursor;
use super::error::Error;
use super::data::{RegionMap, Value, ValueMap};
use super::MemoryPlacement;

pub(super) fn evaluate_placement(cursor: &mut Cursor,
	values: &ValueMap,
	regions: &RegionMap,
	end_token: TokenKind,
) -> Result<MemoryPlacement, Error> {
	match cursor.current() {
		TokenKind::Identifier(id) if regions.contains_key(&id) => {
			if cursor.peek(1) == end_token {
				Ok(MemoryPlacement::Region(id))
			} else {
				cursor.advance();
				Err(cursor.expected_token(format!("'{end_token:?}' after region name")))
			}
		}
		_ => {
			evaluate_expr(cursor, values, end_token).and_then(|value| {
				match value {
					Value::Integer(address) => {
						if !(0..u32::MAX as i64).contains(&address) {
							panic!("address ({address}) out of range")
						}

						Ok(MemoryPlacement::Address(address as u32))
					}
					Value::Decimal(_) => {
						panic!("decimal values cannot be used in address specifiers")
					}
				}
			}).map_err(|_| cursor.expected_token("placement specifier"))
		}
	}
}

pub(super) fn evaluate_expr(cursor: &mut Cursor,
	values: &ValueMap,
	end_token: TokenKind,
) -> Result<Value, Error> {
	let val = parse_additive(cursor, values, end_token)?;
	cursor.expect(end_token)?;
	Ok(val)
}

fn parse_additive(cursor: &mut Cursor,
	values: &ValueMap,
	end_token: crate::token::Kind,
) -> Result<Value, Error> {
	let mut left = parse_multiplicative(cursor, values, end_token)?;

	while cursor.current() != end_token {
		match cursor.current() {
			crate::token::Kind::Plus => {
				cursor.advance();
				let right = parse_multiplicative(cursor, values, end_token)?;
				left = left + right;
			}
			crate::token::Kind::Dash => {
				cursor.advance();
				let right = parse_multiplicative(cursor, values, end_token)?;
				left = left - right;
			}
			_ => break,
		}
	}

	Ok(left)
}

fn parse_multiplicative(cursor: &mut Cursor,
	values: &ValueMap,
	end_token: crate::token::Kind,
) -> Result<Value, Error> {
	let mut left = parse_primary(cursor, values, end_token)?;

	while cursor.current() != crate::token::Kind::Semicolon {
		match cursor.current() {
			crate::token::Kind::Star => {
				cursor.advance();
				let right = parse_primary(cursor, values, end_token)?;
				left = left * right;
			}
			crate::token::Kind::Slash => {
				cursor.advance();
				let location = cursor.index();
				let right = parse_primary(cursor, values, end_token)?;
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
				let right = parse_primary(cursor, values, end_token)?;
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
	values: &ValueMap,
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
			let location = cursor.index();
			cursor.advance();
			values.get(&ident_id)
					.copied()
					.ok_or_else(|| Error::UndefinedType { location, ident_id })
		}
		crate::token::Kind::OParen => {
			cursor.advance();
			let val = parse_additive(cursor, values, end_token)?;
			cursor.expect(crate::token::Kind::CParen)?;
			Ok(val)
		}
		_ => Err(cursor.expected_token("Number, Identifier, or '('").into()),
	}
}
