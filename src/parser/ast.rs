
use index_vec::define_index_type;
use index_vec::IndexVec;

use crate::identifier::IdentId;
use crate::operators::{BinaryOp, UnaryOp};
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
	Mark { region_id: IdentId, mark_id: IdentId },
	Free { region_id: IdentId, mark_id: Option<IdentId> },
	Use { region_id: IdentId, ident: IdentId },
}

impl AstKind {
	pub fn new_mark(region_id: IdentId, mark_id: IdentId) -> Self {
		Self::Mark { region_id, mark_id }
	}

	pub fn new_free(region_id: IdentId, mark_id: Option<IdentId>) -> Self {
		Self::Free { region_id, mark_id }
	}

	pub fn new_use(region_id: IdentId, ident: IdentId) -> Self {
		Self::Use { region_id, ident }
	}
}

pub type KindList = IndexVec<AstId, AstKind>;

#[cfg(feature="optimize")]
#[derive(Debug, Default)]
pub struct Graph {
	node_kinds: Vec<Kind>,
	use_defs: Vec<Vec<Id>>,
	def_uses: Vec<Vec<Id>>,
}

