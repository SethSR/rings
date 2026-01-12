
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
	Record(crate::identifier::IdentId),
	S16,
	S32,
	S8,
	U16,
	U32,
	U8,
	Void,
}
