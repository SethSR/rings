
use crate::identifier::Map as IdentMap;
use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Region {
	pub span: Span<u32>,
	pub alloc_position: u32,
}

pub type RegionMap = IdentMap<Region>;

