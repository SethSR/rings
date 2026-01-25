
use std::ops::Range;

use crate::identifier::IdentId;
use crate::lexer::Data as LexData;
use crate::operators::{BinaryOp, UnaryOp};
use crate::token::{Id as TokenId, Kind as TokenKind};

use super::error::Error;
use super::types::Type;
use super::Data;

pub struct Cursor<'a> {
	tokens: &'a [TokenKind],
	range: Range<usize>,
}

impl<'a> Cursor<'a> {
	pub fn new(lex_data: &'a LexData) -> Self {
		Self {
			tokens: &lex_data.tok_list.raw,
			range: 0..lex_data.tok_list.len(),
		}
	}

	pub fn from_start(lex_data: &'a LexData, start: TokenId) -> Self {
		Self {
			tokens: &lex_data.tok_list.raw,
			range: start.index()..lex_data.tok_list.len(),
		}
	}

	pub fn index(&self) -> TokenId {
		self.range.start.into()
	}

	pub fn advance(&mut self) {
		self.range.start += 1;
	}

	pub fn peek(&self, offset: usize) -> TokenKind {
		self.tokens[self.range.start..self.range.end]
			.get(offset)
			.copied()
			.unwrap_or(TokenKind::Eof)
	}

	pub fn current(&self) -> TokenKind {
		self.peek(0)
	}

	pub fn expect(&mut self, expected: TokenKind) -> Result<(), Error> {
		if self.current() == expected {
			self.advance();
			Ok(())
		} else {
			Err(self.expected_token(&format!("{expected:?}")))
		}
	}

	pub fn expect_identifier(&mut self, expected: &str) -> Result<IdentId, Error> {
		if let TokenKind::Identifier(ident_id) = self.current() {
			self.advance();
			Ok(ident_id)
		} else {
			Err(self.expected_token(expected))
		}
	}

	pub fn expect_type(&mut self,
		data: &Data<TokenId>,
	) -> Result<Type, Error> {
		let result = match self.current() {
			TokenKind::Identifier(ident_id) if data.records.contains_key(&ident_id) => {
				Ok(Type::Record(ident_id))
			}
			TokenKind::Identifier(ident_id) if data.tables.contains_key(&ident_id) => {
				Ok(Type::Table(ident_id))
			}
			TokenKind::Identifier(ident_id) => {
				Err(Error::UndefinedType { location: self.index(), ident_id })
			}
			TokenKind::Bool => Ok(Type::Bool),
			TokenKind::S16 => Ok(Type::S16),
			TokenKind::S32 => Ok(Type::S32),
			TokenKind::S8 => Ok(Type::S8),
			TokenKind::U16 => Ok(Type::U16),
			TokenKind::U32 => Ok(Type::U32),
			TokenKind::U8 => Ok(Type::U8),
			_ => return Err(self.expected_token("type-specifier")),
		};
		self.advance();
		result
	}

	pub fn expect_bin_op(&mut self) -> Result<BinaryOp, Error> {
		match self.current() {
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

	pub fn expect_unary_op(&mut self) -> Option<UnaryOp> {
		match self.current() {
			TokenKind::Dash => { self.advance(); Some(UnaryOp::Neg) }
			TokenKind::Bang => { self.advance(); Some(UnaryOp::Not) }
			_ => None,
		}
	}

	pub fn expected_token(&self, expected: impl Into<String>) -> Error {
		Error::ExpectedToken { expected: expected.into(), found: self.index() }
	}
}
