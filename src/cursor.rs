
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

	pub fn location(&self, data: &Data) -> usize {
		data.tok_pos[self.0]
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

	pub fn expect_type(&mut self, data: &Data) -> Result<crate::Type, CompilerError> {
		let result = match self.current(data) {
			token::Kind::Identifier(ident_id) => {
				if data.records.contains_key(&ident_id) {
					Ok(crate::Type::Record(ident_id))
				} else if data.tables.contains_key(&ident_id) {
					Ok(crate::Type::Table(ident_id))
				} else {
					return Err(error::expected_token(data, "type-specifier", self.index()))
				}
			}
			token::Kind::Bool => Ok(crate::Type::Bool),
			token::Kind::U8 => Ok(crate::Type::U8),
			token::Kind::S8 => Ok(crate::Type::S8),
			token::Kind::U16 => Ok(crate::Type::U16),
			token::Kind::S16 => Ok(crate::Type::S16),
			token::Kind::U32 => Ok(crate::Type::U32),
			token::Kind::S32 => Ok(crate::Type::S32),
			_ => return Err(error::expected_token(data, "type-specifier", self.index())),
		};
		self.advance();
		result
	}
}

