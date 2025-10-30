
use std::ops::Range;

use index_vec::define_index_type;
use index_vec::IndexVec;

use crate::identifier::Id as IdentId;
use crate::token;
use crate::{BinaryOp, Bounds, SrcPos, UnaryOp};

define_index_type! {
	pub struct Id = usize;
	DEFAULT = Id::from_raw_unchecked(0);
	DEBUG_FORMAT = "ast::Id({})";
	DISPLAY_FORMAT = "{}";
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block(pub Vec<Id>);

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
	Ident(IdentId),
	Int(i64),
	Dec(f64),
	Define(IdentId, crate::Type, Id),
	Assign(IdentId, Id),
	BinOp(BinaryOp, Id, Id),
	UnOp(UnaryOp, Id),
	Return(Option<Id>),
	If(Id, Block, Block),
	Block(Block),
	While(Id, Block),
	For(Vec<IdentId>, Option<IdentId>, Option<Bounds>, Block),
	TableIndex(IdentId, Id),
	Call(IdentId, Vec<Id>),
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

