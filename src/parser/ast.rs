
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
	Assign { lhs: AstId, rhs: AstId },
	BinOp { op: BinaryOp, lhs: AstId, rhs: AstId },
	UnOp { op: UnaryOp, rhs: AstId },
	Return(Option<AstId>),
	If {
		cond: AstId,
		then_block: Vec<AstId>,
		else_block: Vec<AstId>,
	},
	Block(Vec<AstId>),
	While { cond: AstId, block: Vec<AstId> },
	For {
		indexes: Vec<IdentId>,
		table: Option<IdentId>,
		bounds: Option<Bounds>,
		block: Vec<AstId>,
	},
	Call  { proc_id: IdentId, block: Vec<AstId> },
	Access { base_id: IdentId, path: Vec<PathSegment> },
	Mark { region_id: IdentId, mark_id: IdentId },
	Free { region_id: IdentId, mark_id: Option<IdentId> },
	Use { region_id: IdentId, ident: IdentId },
}

impl AstKind {
	pub fn assign (lhs: AstId, rhs: AstId) -> Self {
		Self::Assign { lhs, rhs }
	}

	pub fn bin_op(op: BinaryOp, lhs: AstId, rhs: AstId) -> Self {
		Self::BinOp { op, lhs, rhs }
	}

	pub fn un_op(op: UnaryOp, rhs: AstId) -> Self {
		Self::UnOp { op, rhs }
	}

	pub fn if_(cond: AstId, then_block: Vec<AstId>, else_block: Vec<AstId>) -> Self {
		Self::If { cond, then_block, else_block }
	}

	pub fn while_(cond: AstId, block: Vec<AstId>) -> Self {
		Self::While { cond, block }
	}

	pub fn for_(
		indexes: Vec<IdentId>,
		table: Option<IdentId>,
		bounds: Option<Bounds>,
		block: Vec<AstId>,
	) -> Self {
		Self::For { indexes, table, bounds, block }
	}

	pub fn call(proc_id: IdentId, block: Vec<AstId>) -> Self {
		Self::Call { proc_id, block }
	}

	pub fn access(base_id: IdentId, path: Vec<PathSegment>) -> Self {
		Self::Access { base_id, path }
	}

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

