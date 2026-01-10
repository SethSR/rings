
use crate::identifier::Id as IdentId;
use crate::token::Id as TokenId;

#[derive(Debug, Clone, PartialEq)]
pub struct Task {
	pub name_id: IdentId,
	pub tok_start: TokenId,
	pub prev_furthest_token: TokenId,
	pub prev_queue_length: Option<usize>,
}

impl Task {
	pub fn new(
		name_id: IdentId,
		tok_start: TokenId,
	) -> Self {
		Self {
			name_id,
			tok_start,
			prev_furthest_token: TokenId::default(),
			prev_queue_length: None,
		}
	}
}

