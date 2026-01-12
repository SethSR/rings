
use index_vec::define_index_type;
use index_vec::IndexVec;

use crate::identifier::IdentId;
use crate::operators::{BinaryOp, UnaryOp};
use crate::parser::Type;
use crate::Bounds;

define_index_type! {
	pub struct AstId = usize;
	DEFAULT = AstId::from_raw_unchecked(0);
	DEBUG_FORMAT = "ast::Id({})";
	DISPLAY_FORMAT = "{}";
}

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
	If(AstId, Vec<AstId>, Vec<AstId>),
	Block(Vec<AstId>),
	While(AstId, Vec<AstId>),
	For(Vec<IdentId>, Option<IdentId>, Option<Bounds>, Vec<AstId>),
	Call(IdentId, Vec<AstId>),
	Access(IdentId, Vec<PathSegment>),
}

pub type KindList = IndexVec<AstId, AstKind>;

#[cfg(feature="optimize")]
#[derive(Debug, Default)]
pub struct Graph {
	node_kinds: Vec<Kind>,
	use_defs: Vec<Vec<Id>>,
	def_uses: Vec<Vec<Id>>,
}

