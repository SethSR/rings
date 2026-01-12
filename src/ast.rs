
use index_vec::define_index_type;
use index_vec::IndexVec;

use crate::identifier::IdentId;
use crate::operators::{BinaryOp, UnaryOp};
use crate::parser::Type;
use crate::token;
use crate::{Bounds, Span};

define_index_type! {
	pub struct AstId = usize;
	DEFAULT = AstId::from_raw_unchecked(0);
	DEBUG_FORMAT = "ast::Id({})";
	DISPLAY_FORMAT = "{}";
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block(pub Vec<AstId>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathSegment {
	Field(IdentId),
	Index(AstId, IdentId),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstKind {
	Ident(IdentId),
	Int(i64),
	Dec(f64),
	Define(AstId, Type),
	Assign(AstId, AstId),
	BinOp(BinaryOp, AstId, AstId),
	UnOp(UnaryOp, AstId),
	Return(Option<AstId>),
	If(AstId, Block, Block),
	Block(Block),
	While(AstId, Block),
	For(Vec<IdentId>, Option<IdentId>, Option<Bounds>, Block),
	Call(IdentId, Vec<AstId>),
	#[cfg(feature="access")]
	Access(IdentId, Vec<PathSegment>),
}

pub type KindList = IndexVec<AstId, AstKind>;
pub type LocList = IndexVec<AstId, Span<token::Id>>;

#[cfg(feature="optimize")]
#[derive(Debug, Default)]
pub struct Graph {
	node_kinds: Vec<Kind>,
	use_defs: Vec<Vec<Id>>,
	def_uses: Vec<Vec<Id>>,
}

