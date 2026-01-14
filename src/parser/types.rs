
use crate::identifier::IdentId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
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
