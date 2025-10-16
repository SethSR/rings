
use index_vec::IndexVec;
use index_vec::define_index_type;

use crate::identifier;
use crate::SrcPos;

pub type KindList = IndexVec<Id, Kind>;
pub type PosList = IndexVec<Id, SrcPos>;

define_index_type! {
	pub struct Id = usize;
	DEFAULT = Id::from_raw_unchecked(0);
	DEBUG_FORMAT = "tokens::Id({})";
	DISPLAY_FORMAT = "{}";
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kind {
	Identifier(identifier::Id),
	Integer(i64),
	Decimal(f64),

	// TODO - srenshaw - Add Fixed-Point identifiers

	// FixedF(u8),
	// FixedD(u8),

	Region,
	Record,
	Table,
	Index, // TODO - srenshaw - Add table-indexes
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
	Where,
	Arrow,
	At,
	Semicolon,
	Comma,
	Dot,
	DotDot,
	Colon, ColonColon, ColonEqual,
	OParen, CParen,
	OBrace, CBrace,
	OBracket, CBracket,
	Plus,    PlusEqual,
	Dash,    DashEqual,
	Star,    StarEqual,
	Slash,   SlashEqual,
	Carrot,  CarrotEqual,
	Less,    LessEqual,
	Greater, GreaterEqual,
	Equal,   EqualEqual,
	BangEqual,
	Eof,
}

