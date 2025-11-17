
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
	// Basic Tokens
	Identifier(identifier::Id),
	Integer(i64),
	Decimal(f64),

	// TODO - srenshaw - Add Fixed-Point identifiers

	// Built-in Types
	// Fix16(u8),
	// Fix32(u8),
	Bool,
	U8, S8,
	U16, S16,
	U32, S32,

	// Top-Level Stmts
	Index, // TODO - srenshaw - Add table-indexes
	Main,
	Overlay,
	Proc,
	Record,
	Region,
	Sub,
	Table,
	Value,

	Let,
	Return,
	True,
	False,
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
	OParen,   CParen,
	OBrace,   CBrace,
	OBracket, CBracket,
	Amp,      Amp2,
	Bar,      Bar2,
	Dot,      Dot2,
	Colon,    Colon2,    ColonEq,
	Carrot,   Carrot2,   CarrotEq,
	LArr,     LArr2,     LArrEq,
	RArr,     RArr2,     RArrEq,
	Plus,     PlusEq,
	Dash,     DashEq,
	Star,     StarEq,
	Slash,    SlashEq,
	Percent,  PercentEq,
	Eq,       Eq2,
	Bang,     BangEq,
	Eof,
}

impl Kind {
	pub fn size(&self, data: &Data) -> usize {
		match self {
			Self::Identifier(ident_id) => data.text(ident_id).len(),
			Self::Integer(num) => num.to_string().len(),
			Self::Decimal(num) => num.to_string().len(),

			// Self::Fix16(b) => if b < 10 { 2 } else { 3 },
			// Self::Fix32(b) => if b < 10 { 2 } else { 3 },

			Self::Overlay => 7,

			Self::Record => 6,
			Self::Region => 6,
			Self::Return => 6,

			Self::Value => 5,
			Self::Arrow => 5,
			Self::False => 5,
			Self::Index => 5,
			Self::Table => 5,
			Self::Where => 5,
			Self::While => 5,

			Self::Bool => 4,
			Self::Else => 4,
			Self::Main => 4,
			Self::Proc => 4,
			Self::True => 4,

			Self::For => 3,
			Self::Let => 3,
			Self::Sub => 3,
			Self::S16 => 3,
			Self::S32 => 3,
			Self::U16 => 3,
			Self::U32 => 3,

			Self::Amp2 => 2,
			Self::At => 2,
			Self::Bar2 => 2,
			Self::BangEq => 2,
			Self::Carrot2 => 2,
			Self::CarrotEq => 2,
			Self::Colon2 => 2,
			Self::ColonEq => 2,
			Self::DashEq => 2,
			Self::Dot2 => 2,
			Self::Eq2 => 2,
			Self::If => 2,
			Self::In => 2,
			Self::RArrEq => 2,
			Self::RArr2 => 2,
			Self::LArrEq => 2,
			Self::LArr2 => 2,
			Self::PercentEq => 2,
			Self::PlusEq => 2,
			Self::SlashEq => 2,
			Self::StarEq => 2,
			Self::S8 => 2,
			Self::U8 => 2,

			Self::Bang => 1,
			Self::Carrot => 1,
			Self::Colon => 1,
			Self::Comma => 1,
			Self::CBrace => 1,
			Self::CBracket => 1,
			Self::CParen => 1,
			Self::Dash => 1,
			Self::Dot => 1,
			Self::RArr => 1,
			Self::LArr => 1,
			Self::OBrace => 1,
			Self::OBracket => 1,
			Self::OParen => 1,
			Self::Plus => 1,
			Self::Semicolon => 1,
			Self::Amp => 1,
			Self::Bar => 1,
			Self::Eq => 1,
			Self::Percent => 1,
			Self::Star => 1,
			Self::Slash => 1,

			Self::Eof => 0,
		}
	}
}

