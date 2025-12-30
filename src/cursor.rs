
use crate::identifier;
use crate::operators::{BinaryOp, UnaryOp};
use crate::token;
use crate::{Data, Span};
use crate::token::Kind;

pub enum Error {
	ExpectedToken { expected: String, found: token::Id },
	Expected { span: Span<usize>, expected: String, found: String },
}

impl Error {
	pub fn into_comp_error(self, db: &Data) -> crate::Error {
		match self {
			Self::ExpectedToken { expected, found: token_id } => {
				let found = db.tok_list[token_id];
				let message = if let Kind::Identifier(ident_id) = found {
					format!("Expected {expected}, found '{}'", db.text(&ident_id))
				} else {
					format!("Expected {expected}, found {found:?}")
				};
				crate::Error::new(db.token_source(token_id), message)
			}
			Self::Expected { span, expected, found } => {
				crate::Error::new(span, format!("Expected {expected}, found {found}"))
			}
		}
	}
}

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

	pub fn peek(&self, data: &Data, offset: usize) -> Kind {
		data.tok_list
			.get(self.0 + offset)
			.copied()
			.unwrap_or(Kind::Eof)
	}

	pub fn current(&self, data: &Data) -> Kind {
		self.peek(data, 0)
	}

	pub fn expect(&mut self, data: &Data, expected: Kind) -> Result<(), Error> {
		if self.current(data) == expected {
			self.advance();
			Ok(())
		} else {
			Err(self.expected_token(&format!("{expected:?}")))
		}
	}

	pub fn expect_identifier(&mut self, data: &Data,
		expected: &str,
	) -> Result<identifier::Id, Error> {
		if let Kind::Identifier(ident_id) = self.current(data) {
			self.advance();
			Ok(ident_id)
		} else {
			Err(self.expected_token(expected))
		}
	}

	pub fn expect_integer(&mut self, data: &Data,
		expected: &str,
	) -> Result<i64, Error> {
		if let Kind::Integer(num) = self.current(data) {
			self.advance();
			Ok(num)
		} else {
			Err(self.expected_token(expected))
		}
	}

	pub fn expect_u32(&mut self, data: &Data, expected: &str) -> Result<u32, Error> {
		self.expect_integer(data, expected)
			.and_then(|num| check_integer_as_u32(
				&format!("valid {expected}"),
				num,
				data.token_source(self.index() - 1) + data.token_source(self.index())
			))
	}

	pub fn expect_type(&mut self, data: &Data) -> Result<crate::Type, Error> {
		let result = match self.current(data) {
			#[cfg(all(feature="record", feature="table"))]
			Kind::Identifier(ident_id) => {
				if data.records.contains_key(&ident_id) {
					Ok(crate::Type::Record(ident_id))
				} else if data.tables.contains_key(&ident_id) {
					Ok(crate::Type::Table(ident_id))
				} else {
					return Err(error::expected_token(data, "type-specifier", self.index()))
				}
			}
			#[cfg(feature="types")]
			Kind::Bool => Ok(crate::Type::Bool),
			#[cfg(feature="types")]
			Kind::U8 => Ok(crate::Type::U8),
			Kind::S8 => Ok(crate::Type::s8_top()),
			#[cfg(feature="types")]
			Kind::U16 => Ok(crate::Type::U16),
			#[cfg(feature="types")]
			Kind::S16 => Ok(crate::Type::S16),
			#[cfg(feature="types")]
			Kind::U32 => Ok(crate::Type::U32),
			#[cfg(feature="types")]
			Kind::S32 => Ok(crate::Type::S32),
			_ => return Err(self.expected_token("type-specifier")),
		};
		self.advance();
		result
	}

	pub fn expect_bin_op(&mut self, data: &mut Data) -> Result<BinaryOp, Error> {
		match self.current(data) {
			Kind::Amp     => { self.advance(); Ok(BinaryOp::BinAnd) }
			Kind::Amp2    => { self.advance(); Ok(BinaryOp::LogAnd) }
			Kind::BangEq  => { self.advance(); Ok(BinaryOp::CmpNE) }
			Kind::Bar     => { self.advance(); Ok(BinaryOp::BinOr) }
			Kind::Bar2    => { self.advance(); Ok(BinaryOp::LogOr) }
			Kind::Carrot  => { self.advance(); Ok(BinaryOp::BinXor) }
			Kind::Carrot2 => { self.advance(); Ok(BinaryOp::LogXor) }
			Kind::Dash    => { self.advance(); Ok(BinaryOp::Sub) }
			Kind::Eq2     => { self.advance(); Ok(BinaryOp::CmpEQ) }
			Kind::LArr    => { self.advance(); Ok(BinaryOp::CmpLT) }
			Kind::LArr2   => { self.advance(); Ok(BinaryOp::ShL) }
			Kind::LArrEq  => { self.advance(); Ok(BinaryOp::CmpLE) }
			Kind::Percent => { self.advance(); Ok(BinaryOp::Mod) }
			Kind::Plus    => { self.advance(); Ok(BinaryOp::Add) }
			Kind::RArr    => { self.advance(); Ok(BinaryOp::CmpGT) }
			Kind::RArr2   => { self.advance(); Ok(BinaryOp::ShR) }
			Kind::RArrEq  => { self.advance(); Ok(BinaryOp::CmpGE) }
			Kind::Slash   => { self.advance(); Ok(BinaryOp::Div) }
			Kind::Star    => { self.advance(); Ok(BinaryOp::Mul) }
			_ => Err(self.expected_token("binary operator")),
		}
	}

	pub fn expect_unary_op(&mut self, data: &Data) -> Option<UnaryOp> {
		match self.current(data) {
			Kind::Dash => { self.advance(); Some(UnaryOp::Neg) }
			Kind::Bang => { self.advance(); Some(UnaryOp::Not) }
			_ => None,
		}
	}

	pub fn expected_token(&self, expected: impl Into<String>) -> Error {
		Error::ExpectedToken { expected: expected.into(), found: self.index() }
	}
}

fn check_integer_as_u32(expected: &str, found: i64, span: Span<usize>) -> Result<u32, Error> {
	if !(0..u32::MAX as i64).contains(&found) {
		Err(Error::Expected { span, expected: expected.into(), found: found.to_string() })
	} else {
		Ok(found as u32)
	}
}

