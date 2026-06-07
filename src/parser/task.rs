
use crate::identifier::IdentId;
use crate::token::Id as TokenId;
use crate::Target;

#[derive(Debug, Clone, Copy)]
pub enum RegionParseType {
	Range { start: TokenId, end: TokenId },
	Location { size: TokenId, address: TokenId },
}

#[derive(Debug)]
pub enum Task {
	Value {
		ident: IdentId,
		start: TokenId,
	},
	Region {
		ident: IdentId,
		parse_type: RegionParseType,
	},
	Record {
		ident: IdentId,
		start_placement: Option<TokenId>,
		start_fields: TokenId,
	},
	Table {
		ident: IdentId,
		start_rows: TokenId,
		start_fields: TokenId,
		start_placement: Option<TokenId>,
	},
	Proc {
		ident: IdentId,
		target: Option<Target>,
		start: TokenId,
	},
}

