
use std::fmt::{Display, Formatter, Result};

use index_vec::define_index_type;
use index_vec::IndexVec;

use crate::identifier::IdentId;
use crate::operators::{BinaryOp, UnaryOp};
use crate::Span;

pub type AstList<K,T> = IndexVec<AstId, Ast<K,T>>;

define_index_type! {
	pub struct AstId = usize;
	DEFAULT = AstId::from_raw_unchecked(0);
	DEBUG_FORMAT = "ast::Id({})";
	DISPLAY_FORMAT = "{}";
}

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
	Int(i64),
	Dec(f64),
	Ident(IdentId),
	Assign { lhs: AstId, rhs: AstId },
	BinOp { op: BinaryOp, lhs: AstId, rhs: AstId },
	UnOp { op: UnaryOp, rhs: AstId },
	Return(Option<AstId>),
	ScopeBegin,
	ScopeEnd,
	Block(Vec<AstId>),
	If {
		cond: AstId,
		then_block: Vec<AstId>,
		else_block: Vec<AstId>,
	},
	While { cond: AstId, block: Vec<AstId> },
	For {
		indexes: Vec<AstId>,
		table: Option<IdentId>,
		range_start: Option<AstId>,
		range_end: Option<AstId>,
		block: Vec<AstId>,
	},
	Call { proc_id: IdentId, block: Vec<AstId> },
	Access { base_id: IdentId, path: Vec<PathSegment> },
	Mark { region_id: IdentId, mark_id: IdentId },
	Free { region_id: IdentId, mark_id: Option<IdentId> },
	Use { region_id: IdentId, ident: IdentId },
}

impl Display for Kind {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::Int {..} => write!(f, "Int"),
			Self::Dec {..} => write!(f, "Dec"),
			Self::Ident {..} => write!(f, "Ident"),
			Self::Assign {..} => write!(f, "Assign"),
			Self::BinOp {..} => write!(f, "Binary-Op"),
			Self::UnOp {..} => write!(f, "Unary-Op"),
			Self::Return {..} => write!(f, "Return"),
			Self::ScopeBegin {..} => write!(f, "Scope-Begin"),
			Self::ScopeEnd {..} => write!(f, "Scope-End"),
			Self::Block {..} => write!(f, "Block"),
			Self::If {..} => write!(f, "If"),
			Self::While {..} => write!(f, "While"),
			Self::For {..} => write!(f, "For"),
			Self::Call {..} => write!(f, "Call"),
			Self::Access {..} => write!(f, "Access"),
			Self::Mark {..} => write!(f, "Mark"),
			Self::Free {..} => write!(f, "Free"),
			Self::Use {..} => write!(f, "Use"),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ast<K,T> {
	pub kind: K,
	pub location: Span<T>,
}

impl<T> Ast<Kind,T> {
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

	pub fn scope_begin(location: Span<T>) -> Self {
		Self {
			kind: Kind::ScopeBegin,
			location,
		}
	}

	pub fn scope_end(location: Span<T>) -> Self {
		Self {
			kind: Kind::ScopeEnd,
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
		indexes: Vec<AstId>,
		table: Option<IdentId>,
		range_start: Option<AstId>,
		range_end: Option<AstId>,
		block: Vec<AstId>,
		location: Span<T>,
	) -> Self {
		Self {
			kind: Kind::For { indexes, table, range_start, range_end, block },
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

impl<T> PartialEq<Kind> for Ast<Kind, T> {
	fn eq(&self, kind: &Kind) -> bool {
		&self.kind == kind
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathSegment {
	Field(IdentId),
	Index(AstId, IdentId),
}

#[cfg(feature="optimize")]
#[derive(Debug, Default)]
pub struct Graph {
	node_kinds: Vec<Kind>,
	use_defs: Vec<Vec<Id>>,
	def_uses: Vec<Vec<Id>>,
}

