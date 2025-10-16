
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

	pub fn expect(&mut self, data: &mut Data, expected: token::Kind) -> Result<(), String> {
		let found = self.current(data);
		if found == expected {
			self.advance();
			Ok(())
		} else {
			Err(format!("Expected {expected:?}, found {found:?}"))
		}
	}

	pub fn expect_identifier(&mut self, data: &Data,
		expected: &str,
	) -> Result<identifier::Id, String> {
		let kind = self.current(data);
		if let token::Kind::Identifier(ident_id) = kind {
			self.advance();
			Ok(ident_id)
		} else {
			Err(format!("Expected {expected}, found {kind:?}"))
		}
	}

	pub fn expect_integer(&mut self, data: &Data,
		expected: &str,
	) -> Result<i64, String> {
		let kind = self.current(data);
		if let token::Kind::Integer(num) = kind {
			self.advance();
			Ok(num)
		} else {
			Err(format!("Expected {expected}, found {kind:?}"))
		}
	}

	pub fn expect_type(&mut self, data: &mut Data,
		expected: &str,
	) -> Result<crate::Type, String> {
		match self.current(data) {
			token::Kind::Identifier(ident_id) => {
				if data.records.contains_key(&ident_id) {
					Ok(crate::Type::Record(ident_id))
				} else if data.tables.contains_key(&ident_id) {
					Ok(crate::Type::Table(ident_id))
				} else {
					Err(format!("Expected {expected}, found {}", data.text(ident_id)))
					// error::expected(data, "type-specifier", found)
				}
			}
			token::Kind::Bool => Ok(crate::Type::Bool),
			token::Kind::U8 => Ok(crate::Type::U8),
			token::Kind::S8 => Ok(crate::Type::S8),
			token::Kind::U16 => Ok(crate::Type::U16),
			token::Kind::S16 => Ok(crate::Type::S16),
			token::Kind::U32 => Ok(crate::Type::U32),
			token::Kind::S32 => Ok(crate::Type::S32),
			kind => {
				Err(format!("Expected {expected}, found {kind:?}"))
				// error::expected_token(data, "type-specifier", kind)
			}
		}
	}
}

