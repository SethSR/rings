
use crate::error;
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

	pub fn expect(&mut self, data: &mut Data, expected: token::Kind) -> Option<()> {
		let found = self.current(data);
		if found == expected {
			self.advance();
			Some(())
		} else {
			error::expected_token(data, &format!("{expected:?}"), self.index());
			None
		}
	}

	pub fn expect_identifier(&mut self, data: &mut Data,
		expected: &str,
	) -> Option<identifier::Id> {
		let kind = self.current(data);
		if let token::Kind::Identifier(ident_id) = kind {
			self.advance();
			Some(ident_id)
		} else {
			error::expected_token(data, expected, self.index());
			None
		}
	}

	pub fn expect_integer(&mut self, data: &mut Data,
		expected: &str,
	) -> Option<i64> {
		let kind = self.current(data);
		if let token::Kind::Integer(num) = kind {
			self.advance();
			Some(num)
		} else {
			error::expected_token(data, expected, self.index());
			None
		}
	}

	pub fn expect_type(&mut self, data: &mut Data,
		expected: &str,
	) -> Option<crate::Type> {
		match self.current(data) {
			token::Kind::Identifier(ident_id) => {
				if data.records.contains_key(&ident_id) {
					Some(crate::Type::Record(ident_id))
				} else if data.tables.contains_key(&ident_id) {
					Some(crate::Type::Table(ident_id))
				} else {
					error::expected_token(data, expected, self.index());
					None
				}
			}
			token::Kind::Bool => Some(crate::Type::Bool),
			token::Kind::U8 => Some(crate::Type::U8),
			token::Kind::S8 => Some(crate::Type::S8),
			token::Kind::U16 => Some(crate::Type::U16),
			token::Kind::S16 => Some(crate::Type::S16),
			token::Kind::U32 => Some(crate::Type::U32),
			token::Kind::S32 => Some(crate::Type::S32),
			_ => {
				error::expected_token(data, expected, self.index());
				None
			}
		}
	}
}

