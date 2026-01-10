
use crate::error;
use crate::identifier::Id as IdentId;
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::operators::{BinaryOp, UnaryOp};
use crate::parser::RecordMap;
use crate::rings_type::Type;
use crate::token::{Id as TokenId, Kind as TokenKind};
use crate::{
	text, token_source,
	Span, SrcPos,
};

pub enum Error {
	ExpectedToken { expected: String, found: TokenId },
	Expected { span: Span<SrcPos>, expected: String, found: String },
}

impl Error {
	pub fn into_comp_error(self,
		input: &InputData,
		lex_data: &LexData,
		kind: error::Kind,
	) -> error::Error {
		match self {
			Self::ExpectedToken { expected, found: token_id } => {
				let found = lex_data.tok_list[token_id];
				let message = if let TokenKind::Identifier(ident_id) = found {
					format!("Expected {expected}, found '{}'", text(&input, &lex_data, &ident_id))
				} else {
					format!("Expected {expected}, found {found:?}")
				};
				error::Error::new(token_source(&input, &lex_data, token_id), message)
			}
			Self::Expected { span, expected, found } => {
				error::Error::new(span, format!("Expected {expected}, found {found}"))
			}
		}.with_kind(kind)
	}
}

#[derive(Default)]
pub struct Cursor(TokenId);

impl Cursor {
	pub fn new(start: TokenId) -> Self {
		Self(start)
	}

	pub fn index(&self) -> TokenId {
		self.0
	}

	pub fn advance(&mut self) {
		self.0 += 1;
	}

	pub fn peek(&self, lex_data: &LexData, offset: usize) -> TokenKind {
		lex_data.tok_list
			.get(self.0 + offset)
			.copied()
			.unwrap_or(TokenKind::Eof)
	}

	pub fn current(&self, lex_data: &LexData) -> TokenKind {
		self.peek(lex_data, 0)
	}

	pub fn expect(&mut self, lex_data: &LexData, expected: TokenKind) -> Result<(), Error> {
		if self.current(lex_data) == expected {
			self.advance();
			Ok(())
		} else {
			Err(self.expected_token(&format!("{expected:?}")))
		}
	}

	pub fn expect_identifier(&mut self, lex_data: &LexData,
		expected: &str,
	) -> Result<IdentId, Error> {
		if let TokenKind::Identifier(ident_id) = self.current(lex_data) {
			self.advance();
			Ok(ident_id)
		} else {
			Err(self.expected_token(expected))
		}
	}

	pub fn expect_integer(&mut self, lex_data: &LexData,
		expected: &str,
	) -> Result<i64, Error> {
		if let TokenKind::Integer(num) = self.current(lex_data) {
			self.advance();
			Ok(num)
		} else {
			Err(self.expected_token(expected))
		}
	}

	pub fn expect_u32(&mut self,
		input: &InputData,
		lex_data: &LexData,
		expected: &str,
	) -> Result<u32, Error> {
		self.expect_integer(&lex_data, expected)
			.and_then(|num| {
				let start = token_source(input, lex_data, self.index() - 1);
				let end = token_source(input, lex_data, self.index());
				check_integer_as_u32(
					&format!("valid {expected}"),
					num,
					start + end,
				)
			})
	}

	pub fn expect_type(&mut self,
		lex_data: &LexData,
		records: &RecordMap,
	) -> Result<Type, Error> {
		let result = match self.current(lex_data) {
			TokenKind::Identifier(ident_id) if records.contains_key(&ident_id) => {
				Ok(Type::Record(ident_id))
			}
			#[cfg(feature="table")]
			TokenKind::Identifier(ident_id) if data.tables.contains_key(&ident_id) => {
				Ok(Type::Table(ident_id));
			}
			#[cfg(feature="types")]
			TokenKind::Bool => Ok(Type::Bool),
			#[cfg(feature="types")]
			TokenKind::U8 => Ok(Type::U8),
			TokenKind::S8 => Ok(Type::s8_top()),
			#[cfg(feature="types")]
			TokenKind::U16 => Ok(Type::U16),
			TokenKind::S16 => Ok(Type::s16_top()),
			#[cfg(feature="types")]
			TokenKind::U32 => Ok(Type::U32),
			TokenKind::S32 => Ok(Type::s32_top()),
			_ => return Err(self.expected_token("type-specifier")),
		};
		self.advance();
		result
	}

	pub fn expect_bin_op(&mut self, lex_data: &LexData) -> Result<BinaryOp, Error> {
		match self.current(lex_data) {
			TokenKind::Amp     => { self.advance(); Ok(BinaryOp::BinAnd) }
			TokenKind::Amp2    => { self.advance(); Ok(BinaryOp::LogAnd) }
			TokenKind::BangEq  => { self.advance(); Ok(BinaryOp::CmpNE) }
			TokenKind::Bar     => { self.advance(); Ok(BinaryOp::BinOr) }
			TokenKind::Bar2    => { self.advance(); Ok(BinaryOp::LogOr) }
			TokenKind::Carrot  => { self.advance(); Ok(BinaryOp::BinXor) }
			TokenKind::Carrot2 => { self.advance(); Ok(BinaryOp::LogXor) }
			TokenKind::Dash    => { self.advance(); Ok(BinaryOp::Sub) }
			TokenKind::Eq2     => { self.advance(); Ok(BinaryOp::CmpEQ) }
			TokenKind::LArr    => { self.advance(); Ok(BinaryOp::CmpLT) }
			TokenKind::LArr2   => { self.advance(); Ok(BinaryOp::ShL) }
			TokenKind::LArrEq  => { self.advance(); Ok(BinaryOp::CmpLE) }
			TokenKind::Percent => { self.advance(); Ok(BinaryOp::Mod) }
			TokenKind::Plus    => { self.advance(); Ok(BinaryOp::Add) }
			TokenKind::RArr    => { self.advance(); Ok(BinaryOp::CmpGT) }
			TokenKind::RArr2   => { self.advance(); Ok(BinaryOp::ShR) }
			TokenKind::RArrEq  => { self.advance(); Ok(BinaryOp::CmpGE) }
			TokenKind::Slash   => { self.advance(); Ok(BinaryOp::Div) }
			TokenKind::Star    => { self.advance(); Ok(BinaryOp::Mul) }
			_ => Err(self.expected_token("binary operator")),
		}
	}

	pub fn expect_unary_op(&mut self, lex_data: &LexData) -> Option<UnaryOp> {
		match self.current(lex_data) {
			TokenKind::Dash => { self.advance(); Some(UnaryOp::Neg) }
			TokenKind::Bang => { self.advance(); Some(UnaryOp::Not) }
			_ => None,
		}
	}

	pub fn expected_token(&self, expected: impl Into<String>) -> Error {
		Error::ExpectedToken { expected: expected.into(), found: self.index() }
	}
}

fn check_integer_as_u32(expected: &str, found: i64, span: Span<SrcPos>) -> Result<u32, Error> {
	if !(0..u32::MAX as i64).contains(&found) {
		Err(Error::Expected { span, expected: expected.into(), found: found.to_string() })
	} else {
		Ok(found as u32)
	}
}

