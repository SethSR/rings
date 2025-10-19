
use std::ops::Range;

use index_vec::define_index_type;
use index_vec::IndexVec;

use crate::identifier;
use crate::token;
use crate::{BinaryOp, RangeType, SrcPos, UnaryOp};

define_index_type! {
	pub struct Id = usize;
	DEFAULT = Id::from_raw_unchecked(0);
	DEBUG_FORMAT = "ast::Id({})";
	DISPLAY_FORMAT = "{}";
}

#[derive(Debug)]
pub struct Block(pub Vec<Id>);

#[derive(Debug)]
pub enum Kind {
	Ident(identifier::Id),
	Int(i64),
	Dec(f64),
	Define(identifier::Id, crate::Type, Id),
	Assign(identifier::Id, Id),
	BinOp(BinaryOp, Id, Id),
	UnOp(UnaryOp, Id),
	Return(Option<Id>),
	If(Id, Block, Block),
	Block(Block),
	While(Id, Block),
	For(Vec<identifier::Id>, Option<identifier::Id>, Option<RangeType>, Block),
}

pub type KindList = IndexVec<Id, Kind>;
pub type PosList = IndexVec<Id, Range<SrcPos>>;
pub type LocList = IndexVec<Id, Range<token::Id>>;

#[derive(Debug, Default)]
pub struct Graph {
	node_kinds: Vec<Kind>,
	use_defs: Vec<Vec<Id>>,
	def_uses: Vec<Vec<Id>>,
}

