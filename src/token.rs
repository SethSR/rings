
use index_vec::IndexVec;
use index_vec::define_index_type;

use crate::identifier;
use crate::{Data, SrcPos};

pub type KindList = IndexVec<Id, Kind>;
pub type PosList = IndexVec<Id, SrcPos>;

define_index_type! {
	pub struct Id = usize;
	DEFAULT = Id::from_raw_unchecked(0);
	DEBUG_FORMAT = "TOK<{}>";
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
	While,
	Arrow,
	At,
	Semicolon,
	Comma,
	Dot,
	DotDot,
	Colon,    ColonColon,     ColonEqual,
	Carrot,   CarrotCarrot,   CarrotEqual,
	Less,     LessLess,       LessEqual,
	Greater,  GreaterGreater, GreaterEqual,
	OParen,   CParen,
	OBrace,   CBrace,
	OBracket, CBracket,
	Plus,     PlusEqual,
	Dash,     DashEqual,
	Star,     StarEqual,
	Slash,    SlashEqual,
	Percent,  PercentEqual,
	Equal,    EqualEqual,
	Amp,      AmpAmp,
	Bar,      BarBar,
	Bang,     BangEqual,
	Eof,
}

impl Kind {
	pub fn size(&self, data: &Data) -> usize {
		match self {
			Self::Identifier(ident_id) => data.text(ident_id).len(),
			Self::Integer(num) => num.to_string().len(),
			Self::Decimal(num) => num.to_string().len(),

			// FixedF(u8),
			// FixedD(u8),

			Self::Record |
			Self::Region |
			Self::Return => 6,
			Self::Arrow |
			Self::False |
			Self::Index |
			Self::Table |
			Self::Where |
			Self::While => 5,
			Self::Bool |
			Self::Else |
			Self::Proc |
			Self::True => 4,
			Self::For |
			Self::S16 | Self::S32 |
			Self::U16 | Self::U32 => 3,
			Self::AmpAmp |
			Self::At |
			Self::BarBar |
			Self::BangEqual |
			Self::CarrotCarrot |
			Self::CarrotEqual |
			Self::ColonColon |
			Self::ColonEqual |
			Self::DashEqual |
			Self::DotDot |
			Self::EqualEqual |
			Self::If |
			Self::In |
			Self::GreaterEqual |
			Self::GreaterGreater |
			Self::LessEqual |
			Self::LessLess |
			Self::PercentEqual |
			Self::PlusEqual |
			Self::SlashEqual |
			Self::StarEqual |
			Self::S8 | Self::U8 => 2,
			Self::Bang |
			Self::Carrot |
			Self::Colon |
			Self::Comma |
			Self::CBrace |
			Self::CBracket |
			Self::CParen |
			Self::Dash |
			Self::Dot |
			Self::Greater |
			Self::Less |
			Self::OBrace |
			Self::OBracket |
			Self::OParen |
			Self::Plus |
			Self::Semicolon => 1,
			Self::Amp |
			Self::Bar |
			Self::Equal |
			Self::Percent |
			Self::Star |
			Self::Slash |
			Self::Eof => 0,
		}
	}
}

