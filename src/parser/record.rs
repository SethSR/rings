
use crate::identifier::{Id as IdentId, Map as IdentMap};
use crate::rings_type::Type;
use crate::type_size;

use super::Param;

pub type RecordMap = IdentMap<Record>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
	pub fields: Vec<Param>,
	pub region: Option<IdentId>,
	pub size: u32,
}

impl Record {
	pub fn new(
		records: &RecordMap,
		fields: Vec<(IdentId, Type)>,
		region: Option<IdentId>,
	) -> Self {
		let mut offset = 0;
		let mut offsets = vec![];

		for (f_name, f_type) in fields {
			let f_size = type_size(records, &f_type);
			offset += match f_size {
				1 => 0,
				2 => offset & 1,
				_ => (4 - (offset & 3)) & 3,
			};
			offsets.push(Param { name: f_name, typ: f_type, size: f_size, offset });
			offset += match f_size {
				1 => 1,
				2 => 2,
				_ => f_size as u16,
			}
		}

		Self {
			fields: offsets,
			region,
			size: offset as u32,
		}
	}

	pub fn size(&self) -> u32 {
		self.size
	}
}

