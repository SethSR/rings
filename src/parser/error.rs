
use crate::identifier::IdentId;
use crate::token::Id as TokenId;
use crate::token_source;

pub enum Error {
	ExpectedToken { expected: String, found: TokenId },
	UndefinedType { location: TokenId, ident_id: IdentId },
	UnexpectedEof { location: TokenId },
	DivisionByZero { location: TokenId },
	RecursiveType { location: TokenId, name_id: IdentId },
	DuplicateDeclaration { location: TokenId, name_id: IdentId },
	CircularDependency { location: TokenId, name_id: IdentId, ident_id: IdentId },
}

impl Error {
	pub fn into_comp_error(self,
		input: &crate::input::Data,
		lex_data: &crate::lexer::Data,
	) -> crate::error::Error {
		match self {
			Self::ExpectedToken { expected, found: token_id } => {
				let found = lex_data.tok_list[token_id];
				let span = token_source(input, lex_data, token_id);
				let message = if let crate::token::Kind::Identifier(ident_id) = found {
					format!("Expected {expected}, found '{}'", lex_data.text(input, &ident_id))
				} else {
					format!("Expected {expected}, found {found:?}")
				};
				crate::error::Error::new(span, message)
			}
			Self::UndefinedType { location, ident_id } => {
				let span = token_source(&input, &lex_data, location);
				let message = format!("Undefined type '{}'", lex_data.text(input, &ident_id));
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
			Self::CircularDependency { location , name_id, ident_id } => {
				let span = token_source(input, lex_data, location);
				let message = format!("Cannot resolve '{}' - circular dependency or undefined variable '{}'",
					lex_data.text(input, &name_id), lex_data.text(input, &ident_id));
				crate::error::Error::new(span, message)
			}
			Self::RecursiveType { location, name_id } => {
				let span = token_source(input, lex_data, location);
				let message = format!("Cannot resolve '{}' - recursive definition",
					lex_data.text(input, &name_id));
				crate::error::Error::new(span, message)
			}
			Self::DuplicateDeclaration { location, name_id } => {
				let span = token_source(input, lex_data, location);
				let message = format!("record '{}' already defined",
					lex_data.text(input, &name_id));
				crate::error::Error::new(span, message)
			}
		}
	}
}
