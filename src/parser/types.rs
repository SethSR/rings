
use crate::identifier::IdentId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
	Unknown,
	Int,
	Dec,

	Bool,
	Record(IdentId),
	S16,
	S32,
	S8,
	Table(IdentId),
	U16,
	U32,
	U8,
	Void,
}

impl Type {
	pub fn is_integer(self) -> bool {
		matches!(self,
			Self::S8 | Self::S16 | Self::S32 |
			Self::U8 | Self::U16 | Self::U32 |
			Self::Int
		)
	}

	pub fn is_decimal(self) -> bool {
		self == Self::Dec
	}
}
