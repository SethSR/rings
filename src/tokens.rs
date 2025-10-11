
use crate::IdentId;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
	Identifier(IdentId),
	Integer(i64),
	Decimal(f64),

	// TODO - srenshaw - Add Fixed-Point identifiers

	// FixedF(u8),
	// FixedD(u8),

	Record,
	Table,
	Proc,
	Return,
	Bool,
	True,
	False,
	U8, S8,
	U16, S16,
	U32, S32,
	In,
	If,
	Else,
	For,
	Arrow,
	At,
	Semicolon,
	Comma,
	Colon, ColonColon, ColonEqual,
	OParen, CParen,
	OBrace, CBrace,
	OBracket, CBracket,
	Plus,    PlusEqual,
	Dash,    DashEqual,
	Star,    StarEqual,
	Slash,   SlashEqual,
	Less,    LessEqual,
	Greater, GreaterEqual,
	Equal,   EqualEqual,
	BangEqual,
	Eof,
}

