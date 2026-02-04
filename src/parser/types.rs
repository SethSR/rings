
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

	pub fn is_signed_integer(self) -> bool {
		matches!(self, Self::S8 | Self::S16 | Self::S32 | Self::Int)
	}

	pub fn is_decimal(self) -> bool {
		self == Self::Dec
	}
}

/// Maps (procedure name, scope depth, variable name) to variable type
#[derive(Debug, Default, Clone)]
pub struct TypeMap {
	data: Vec<(IdentId, u16, IdentId, Type)>,
}

impl TypeMap {
	pub fn insert(&mut self, proc_id: IdentId, scope_depth: u16, id: IdentId, typ: Type) {
		self.data.push((proc_id, scope_depth, id, typ));
	}

	pub fn get(&self, proc_id: IdentId, scope_depth: u16, id: IdentId) -> Option<Type> {
		self.data.iter()
				.rev()
				.find(|(p_id, depth, t_id, _)| *p_id == proc_id && *t_id == id && *depth <= scope_depth)
				.map(|(_,_,_,typ)| *typ)
	}
	
	pub fn iter(&self) -> impl Iterator<Item=&(IdentId, u16, IdentId, Type)> {
		self.data.iter()
	}
}
