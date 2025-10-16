
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

	pub fn expect(&mut self, data: &mut Data, expected: token::Kind) {
		let found = self.current(data);
		if found != expected {
			error::expected_token2(data, expected, found)
		}
		self.advance();
	}

	pub fn expect_identifier(&mut self, data: &mut Data, expected: &str) -> identifier::Id {
		let found = self.current(data);
		let token::Kind::Identifier(ident_id) = found else {
			error::expected_token(data, expected, found)
		};
		self.advance();
		ident_id
	}

	pub fn expect_integer(&mut self, data: &mut Data, expected: &str) -> i64 {
		let found = self.current(data);
		let token::Kind::Integer(value) = found else {
			error::expected_token(data, expected, found)
		};
		self.advance();
		value
	}

	pub fn expect_type(&mut self, data: &mut Data) -> crate::Type {
		match self.current(data) {
			token::Kind::Identifier(ident_id) => {
				if data.records.contains_key(&ident_id) {
					crate::Type::Record(ident_id)
				} else if data.tables.contains_key(&ident_id) {
					crate::Type::Table(ident_id)
				} else {
					let found = &data.text(ident_id).to_owned();
					error::expected(data, "type-specifier", found)
				}
			}
			token::Kind::Bool => crate::Type::Bool,
			token::Kind::U8 => crate::Type::U8,
			token::Kind::S8 => crate::Type::S8,
			token::Kind::U16 => crate::Type::U16,
			token::Kind::S16 => crate::Type::S16,
			token::Kind::U32 => crate::Type::U32,
			token::Kind::S32 => crate::Type::S32,
			kind => {
				error::expected_token(data, "type-specifier", kind)
			}
		}
	}
}

