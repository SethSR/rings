
use crate::identifier::Id as IdentId;
use crate::token::Id as TokenId;
use crate::Data;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
	// procedure name
	Proc(IdentId),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Task {
	pub kind: Kind,
	pub tok_start: TokenId,
	pub prev_furthest_token: TokenId,
	pub prev_ready_proc_count: usize,
}

impl Task {
	pub fn new_proc(name_id: IdentId, tok_start: TokenId) -> Self {
		Self {
			kind: Kind::Proc(name_id),
			tok_start,
			prev_furthest_token: TokenId::default(),
			prev_ready_proc_count: 0,
		}
	}

	pub fn name<'a>(&self, data: &'a Data) -> &'a str {
		match self.kind {
			Kind::Proc(proc_name) => data.text(&proc_name),
		}
	}

	pub fn name_id(&self) -> IdentId {
		match self.kind {
			Kind::Proc(proc_name) => proc_name,
		}
	}
}

