use std::ops::Range;
use crate::error::{self, CompilerError};
use crate::identifier;
use crate::token;
use crate::Data;

#[derive(Default)]
pub struct Cursor(token::Id);

impl Cursor {
	pub fn new(start: token::Id) -> Self {
		Self(start)
	}

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

	pub fn expect(&mut self, data: &Data, expected: token::Kind) -> Result<(), CompilerError> {
		if self.current(data) == expected {
			self.advance();
			Ok(())
		} else {
			Err(error::expected_token(data, &format!("{expected:?}"), self.index()))
		}
	}

	pub fn expect_identifier(&mut self, data: &Data,
		expected: &str,
	) -> Result<identifier::Id, CompilerError> {
		if let token::Kind::Identifier(ident_id) = self.current(data) {
			self.advance();
			Ok(ident_id)
		} else {
			Err(error::expected_token(data, expected, self.index()))
		}
	}

	pub fn expect_integer(&mut self, data: &Data,
		expected: &str,
	) -> Result<i64, CompilerError> {
		if let token::Kind::Integer(num) = self.current(data) {
			self.advance();
			Ok(num)
		} else {
			Err(error::expected_token(data, expected, self.index()))
		}
	}

	pub fn expect_u32(&mut self, data: &Data, expected: &str) -> Result<u32, CompilerError> {
		self.expect_integer(data, expected)
			.and_then(|num| {
				check_integer_as_u32(&format!("valid {expected}"), num,
					(self.index() - 1).into()..self.index().into())
			})
	}

	pub fn expect_type(&mut self, data: &Data) -> Result<crate::Type, CompilerError> {
		let result = match self.current(data) {
			#[cfg(feature="ready")]
			token::Kind::Identifier(ident_id) => {
				if data.records.contains_key(&ident_id) {
					Ok(crate::Type::Record(ident_id))
				} else if data.tables.contains_key(&ident_id) {
					Ok(crate::Type::Table(ident_id))
				} else {
					return Err(error::expected_token(data, "type-specifier", self.index()))
				}
			}
			#[cfg(feature="ready")]
			token::Kind::Bool => Ok(crate::Type::Bool),
			#[cfg(feature="ready")]
			token::Kind::U8 => Ok(crate::Type::U8),
			token::Kind::S8 => Ok(crate::Type::s8_top()),
			#[cfg(feature="ready")]
			token::Kind::U16 => Ok(crate::Type::U16),
			#[cfg(feature="ready")]
			token::Kind::S16 => Ok(crate::Type::S16),
			#[cfg(feature="ready")]
			token::Kind::U32 => Ok(crate::Type::U32),
			#[cfg(feature="ready")]
			token::Kind::S32 => Ok(crate::Type::S32),
			_ => return Err(error::expected_token(data, "type-specifier", self.index())),
		};
		self.advance();
		result
	}
}

fn check_integer_as_u32(expected: &str, found: i64, span: Range<usize>) -> Result<u32, CompilerError> {
	if !(0..u32::MAX as i64).contains(&found) {
		Err(error::expected(span, expected, &found.to_string()))
	} else {
		Ok(found as u32)
	}
}

