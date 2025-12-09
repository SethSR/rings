
use crate::identifier::Id as IdentId;
use crate::token::Id as TokenId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
	Proc,
	Value,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Task {
	pub kind: Kind,
	pub name_id: IdentId,
	pub tok_start: TokenId,
	pub prev_furthest_token: TokenId,
	pub prev_ready_proc_count: usize,
}

impl Task {
	pub fn new_proc(name_id: IdentId, tok_start: TokenId) -> Self {
		Self {
			kind: Kind::Proc,
			name_id,
			tok_start,
			prev_furthest_token: TokenId::default(),
			prev_ready_proc_count: 0,
		}
	}

	pub fn new_value(name_id: IdentId, tok_start: TokenId) -> Self {
		Self {
			kind: Kind::Value,
			name_id,
			tok_start,
			prev_furthest_token: TokenId::default(),
			prev_ready_proc_count: 0,
		}
	}
}

