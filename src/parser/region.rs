
use crate::identifier::Map as IdentMap;
use crate::span::Span;

pub type RegionMap = IdentMap<Region>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Region {
	pub span: Span<u32>,
}

impl Region {
	pub fn new(start: u32, end: u32) -> Self {
		Self {
			span: Span { start, end }
		}
	}
}

