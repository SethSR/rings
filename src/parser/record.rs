
use crate::identifier::{IdentId, Map as IdentMap};

use super::{MemoryPlacement, Type};

pub type RecordMap = IdentMap<Record>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
	pub fields: Vec<(IdentId, Type)>,
	pub placement: Option<MemoryPlacement>,
}

