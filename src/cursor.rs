
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

	pub fn expect(&mut self, data: &Data, expected: token::Kind) -> Option<()> {
		if self.current(data) == expected {
			self.advance();
			Some(())
		} else {
			None
		}
	}

	pub fn expect_identifier(&mut self, data: &Data) -> Option<identifier::Id> {
		if let token::Kind::Identifier(ident_id) = self.current(data) {
			self.advance();
			Some(ident_id)
		} else {
			None
		}
	}

	pub fn expect_integer(&mut self, data: &Data) -> Option<i64> {
		if let token::Kind::Integer(num) = self.current(data) {
			self.advance();
			Some(num)
		} else {
			None
		}
	}

	pub fn expect_type(&mut self, data: &Data) -> Option<crate::Type> {
		let result = match self.current(data) {
			token::Kind::Identifier(ident_id) => {
				if data.records.contains_key(&ident_id) {
					Some(crate::Type::Record(ident_id))
				} else if data.tables.contains_key(&ident_id) {
					Some(crate::Type::Table(ident_id))
				} else {
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
			_ => None,
		};
		self.advance();
		result
	}
}

