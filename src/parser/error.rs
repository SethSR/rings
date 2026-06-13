
use crate::identifier::IdentId;
use crate::token::Id as TokenId;

pub enum Error {
	ExpectedToken { expected: String, found: TokenId },
	UndefinedType { location: TokenId, ident_id: IdentId },
	UnexpectedEof { location: TokenId },
	DivisionByZero { location: TokenId },
	RecursiveType { location: TokenId, name_id: IdentId },
	DuplicateDeclaration { location: TokenId, name_id: IdentId },
	CircularDependency { location: TokenId, name_id: IdentId, ident_id: IdentId },
	ValueOutOfRange { location: TokenId, value: i64, max: i64 },
	DecimalAddressValue { location: TokenId },
}

impl Error {
	pub fn into_comp_error(self,
		input: &crate::input::Data,
		lex_data: &crate::lexer::Data,
	) -> crate::error::Error {
		match self {
			Self::ExpectedToken { expected, found: token_id } => {
				let found = lex_data.tok_list[token_id];
				let span = lex_data.token_source(input, token_id);
				let message = if let crate::token::Kind::Identifier(ident_id) = found {
					format!("Expected {expected}, found '{}'", lex_data.text(input, &ident_id))
				} else {
					format!("Expected {expected}, found {found:?}")
				};
				crate::error::Error::new(span, message)
			}
			Self::UndefinedType { location, ident_id } => {
				let span = lex_data.token_source(&input, location);
				let message = format!("Undefined type '{}'", lex_data.text(input, &ident_id));
				crate::error::Error::new(span, message)
			}
			Self::UnexpectedEof { location } => {
				let span = lex_data.token_source(input, location);
				crate::error::Error::new(span, "unexpected EOF")
			},
			Self::DivisionByZero { location } => {
				let span = lex_data.token_source(input, location);
				crate::error::Error::new(span, "division by zero")
			}
			Self::CircularDependency { location , name_id, ident_id } => {
				let span = lex_data.token_source(input, location);
				let message = format!("Cannot resolve '{}' - circular dependency or undefined variable '{}'",
					lex_data.text(input, &name_id), lex_data.text(input, &ident_id));
				crate::error::Error::new(span, message)
			}
			Self::RecursiveType { location, name_id } => {
				let span = lex_data.token_source(input, location);
				let message = format!("Cannot resolve '{}' - recursive definition",
					lex_data.text(input, &name_id));
				crate::error::Error::new(span, message)
			}
			Self::DuplicateDeclaration { location, name_id } => {
				let span = lex_data.token_source(input, location);
				let message = format!("record '{}' already defined",
					lex_data.text(input, &name_id));
				crate::error::Error::new(span, message)
			}
			Self::ValueOutOfRange { location, value, max } => {
				let span = lex_data.token_source(input, location);
				let message = format!("Calculated value (0x{value:X}) is beyond max value (0x{max:X})");
				crate::error::Error::new(span, message)
			}
			Self::DecimalAddressValue { location } => {
				let span = lex_data.token_source(input, location);
				let message = "Decimal values cannot be used in address specifiers".to_string();
				crate::error::Error::new(span, message)
			}
		}
	}
}

