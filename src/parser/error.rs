
use crate::identifier::Id as IdentId;
use crate::token::Id as TokenId;
use crate::{text, token_source};
use crate::{Span, SrcPos};

pub enum Error {
	ExpectedToken { expected: String, found: TokenId },
	Expected { span: Span<SrcPos>, expected: String, found: String },
	UndefinedType { location: TokenId, ident_id: IdentId },
	UnexpectedEof { location: TokenId },
	DivisionByZero { location: TokenId },
	UndefinedVariable { location: TokenId, ident_id: IdentId },
	RecursiveType { location: TokenId, name_id: IdentId },
	DuplicateDeclaration { location: TokenId, name_id: IdentId },
	CircularDependency { location: TokenId, name_id: IdentId, ident_id: IdentId },
}

impl Error {
	pub fn into_comp_error(self,
		input: &crate::input::Data,
		lex_data: &crate::lexer::Data,
		err_kind: crate::error::Kind,
	) -> crate::error::Error {
		match self {
			Self::ExpectedToken { expected, found: token_id } => {
				let found = lex_data.tok_list[token_id];
				let message = if let crate::token::Kind::Identifier(ident_id) = found {
					format!("Expected {expected}, found '{}'", text(&input, &lex_data, &ident_id))
				} else {
					format!("Expected {expected}, found {found:?}")
				};
				crate::error::Error::new(token_source(&input, &lex_data, token_id), message)
			}
			Self::Expected { span, expected, found } => {
				crate::error::Error::new(span, format!("Expected {expected}, found {found}"))
			}
			Self::UndefinedType { location, ident_id } => {
				let span = token_source(&input, &lex_data, location);
				let message = format!("Undefined type '{}'", text(&input, &lex_data, &ident_id));
				crate::error::Error::new(span, message)
			}
			Self::UnexpectedEof { location } => {
				let span = token_source(input, lex_data, location);
				crate::error::Error::new(span, "unexpected EOF")
			},
			Self::DivisionByZero { location } => {
				let span = token_source(input, lex_data, location);
				crate::error::Error::new(span, "division by zero")
			}
			Self::UndefinedVariable { location , ident_id } => {
				let span = token_source(input, lex_data, location);
				let message = format!("undefined variable `{}`",
					text(input, lex_data, &ident_id));
				crate::error::Error::new(span, message)
			}
			Self::CircularDependency { location , name_id, ident_id } => {
				let span = token_source(input, lex_data, location);
				let message = format!("Cannot resolve '{}' - circular dependency or undefined variable '{}'",
					text(input, lex_data, &name_id), text(input, lex_data, &ident_id));
				crate::error::Error::new(span, message)
			}
			Self::RecursiveType { location, name_id } => {
				let span = token_source(input, lex_data, location);
				let message = format!("Cannot resolve '{}' - recursive definition",
					text(input, lex_data, &name_id));
				crate::error::Error::new(span, message)
			}
			Self::DuplicateDeclaration { location, name_id } => {
				let span = token_source(input, lex_data, location);
				let message = format!("record '{}' already defined",
					text(input, lex_data, &name_id));
				crate::error::Error::new(span, message)
			}
		}.with_kind(err_kind)
	}
}
