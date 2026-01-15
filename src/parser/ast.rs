
use index_vec::define_index_type;
use index_vec::IndexVec;

use crate::identifier::IdentId;
use crate::operators::{BinaryOp, UnaryOp};
use crate::{Bounds, Span};

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
pub enum Kind {
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
	Call { proc_id: IdentId, block: Vec<AstId> },
	Access { base_id: IdentId, path: Vec<PathSegment> },
	Mark { region_id: IdentId, mark_id: IdentId },
	Free { region_id: IdentId, mark_id: Option<IdentId> },
	Use { region_id: IdentId, ident: IdentId },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ast<T> {
	pub kind: Kind,
	pub location: Span<T>,
}

impl<T> Ast<T> {
	pub fn ident(
		id: IdentId,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::Ident(id),
			location,
		}
	}

	pub fn int(
		value: i64,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::Int(value),
			location,
		}
	}

	pub fn dec(
		value: f64,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::Dec(value),
			location,
		}
	}

	pub fn assign(
		lhs: AstId,
		rhs: AstId,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::Assign { lhs, rhs },
			location,
		}
	}

	pub fn bin_op(
		op: BinaryOp,
		lhs: AstId,
		rhs: AstId,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::BinOp { op, lhs, rhs },
			location
		}
	}

	pub fn un_op(
		op: UnaryOp,
		rhs: AstId,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::UnOp { op, rhs },
			location,
		}
	}

	pub fn return_(
		value: Option<AstId>,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::Return(value),
			location,
		}
	}

	pub fn if_(
		cond: AstId,
		then_block: Vec<AstId>,
		else_block: Vec<AstId>,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::If { cond, then_block, else_block },
			location,
		}
	}

	pub fn block(
		block: Vec<AstId>,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::Block(block),
			location,
		}
	}

	pub fn while_(
		cond: AstId,
		block: Vec<AstId>,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::While { cond, block },
			location,
		}
	}

	pub fn for_(
		indexes: Vec<IdentId>,
		table: Option<IdentId>,
		bounds: Option<Bounds>,
		block: Vec<AstId>,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::For { indexes, table, bounds, block },
			location,
		}
	}

	pub fn call(
		proc_id: IdentId,
		block: Vec<AstId>,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::Call { proc_id, block },
			location,
		}
	}

	pub fn access(
		base_id: IdentId,
		path: Vec<PathSegment>,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::Access { base_id, path },
			location,
		}
	}

	pub fn new_mark(
		region_id: IdentId,
		mark_id: IdentId,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::Mark { region_id, mark_id },
			location,
		}
	}

	pub fn new_free(
		region_id: IdentId,
		mark_id: Option<IdentId>,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::Free { region_id, mark_id },
			location,
		}
	}

	pub fn new_use(
		region_id: IdentId,
		ident: IdentId,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::Use { region_id, ident },
			location,
		}
	}
}

impl<T> PartialEq<Kind> for Ast<T> {
	fn eq(&self, kind: &Kind) -> bool {
		&self.kind == kind
	}
}

pub type AstList<T> = IndexVec<AstId, Ast<T>>;

#[cfg(feature="optimize")]
#[derive(Debug, Default)]
pub struct Graph {
	node_kinds: Vec<Kind>,
	use_defs: Vec<Vec<Id>>,
	def_uses: Vec<Vec<Id>>,
}

