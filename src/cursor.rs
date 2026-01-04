
use crate::discovery::RecordMap;
use crate::error;
use crate::identifier::{Id as IdentId, Map as IdentMap};
use crate::operators::{BinaryOp, UnaryOp};
use crate::rings_type::Type;
use crate::token::{Id as TokenId, Kind as TokenKind, KindList, PosList};
use crate::{
	token_source,
	Data, Span, SrcPos,
};

pub enum Error {
	ExpectedToken { expected: String, found: TokenId },
	Expected { span: Span<SrcPos>, expected: String, found: String },
	UnresolvedType { span: Span<SrcPos>, msg: String },
}

impl Error {
	pub fn into_comp_error(self, db: &Data) -> error::Error {
		match self {
			Self::ExpectedToken { expected, found: token_id } => {
				let found = db.tok_list[token_id];
				let message = if let TokenKind::Identifier(ident_id) = found {
					format!("Expected {expected}, found '{}'", db.text(&ident_id))
				} else {
					format!("Expected {expected}, found {found:?}")
				};
				error::Error::new(db.token_source(token_id), message)
			}
			Self::Expected { span, expected, found } => {
				error::Error::new(span, format!("Expected {expected}, found {found}"))
			}
			Self::UnresolvedType { span, msg } => {
				error::Error::new(span, msg)
			}
		}
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

	pub fn peek(&self, tok_list: &KindList, offset: usize) -> TokenKind {
		tok_list
			.get(self.0 + offset)
			.copied()
			.unwrap_or(TokenKind::Eof)
	}

	pub fn current(&self, tok_list: &KindList) -> TokenKind {
		self.peek(tok_list, 0)
	}

	pub fn expect(&mut self, tok_list: &KindList, expected: TokenKind) -> Result<(), Error> {
		if self.current(tok_list) == expected {
			self.advance();
			Ok(())
		} else {
			Err(self.expected_token(&format!("{expected:?}")))
		}
	}

	pub fn expect_identifier(&mut self, tok_list: &KindList,
		expected: &str,
	) -> Result<IdentId, Error> {
		if let TokenKind::Identifier(ident_id) = self.current(tok_list) {
			self.advance();
			Ok(ident_id)
		} else {
			Err(self.expected_token(expected))
		}
	}

	pub fn expect_integer(&mut self, tok_list: &KindList,
		expected: &str,
	) -> Result<i64, Error> {
		if let TokenKind::Integer(num) = self.current(tok_list) {
			self.advance();
			Ok(num)
		} else {
			Err(self.expected_token(expected))
		}
	}

	pub fn expect_u32(&mut self,
		source: &str,
		identifiers: &IdentMap<Span<SrcPos>>,
		tok_list: &KindList,
		tok_pos: &PosList,
		expected: &str,
	) -> Result<u32, Error> {
		self.expect_integer(tok_list, expected)
			.and_then(|num| {
				let start = token_source(source, identifiers, tok_list, tok_pos, self.index() - 1);
				let end = token_source(source, identifiers, tok_list, tok_pos, self.index());
				check_integer_as_u32(
					&format!("valid {expected}"),
					num,
					start + end,
				)
			})
	}

	pub fn expect_type(&mut self,
		tok_list: &KindList,
		records: &RecordMap,
	) -> Result<Type, Error> {
		let result = match self.current(tok_list) {
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
			#[cfg(feature="types")]
			TokenKind::S16 => Ok(Type::S16),
			#[cfg(feature="types")]
			TokenKind::U32 => Ok(Type::U32),
			#[cfg(feature="types")]
			TokenKind::S32 => Ok(Type::S32),
			_ => return Err(self.expected_token("type-specifier")),
		};
		self.advance();
		result
	}

	pub fn expect_bin_op(&mut self, tok_list: &KindList) -> Result<BinaryOp, Error> {
		match self.current(tok_list) {
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

	pub fn expect_unary_op(&mut self, tok_list: &KindList) -> Option<UnaryOp> {
		match self.current(tok_list) {
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

