
use std::ops::Range;

use interavl::IntervalTree;

use crate::identifier::{IdentId, Map as IdentMap};
use crate::packing::Data as PakData;
use crate::parser::{Data as PrsData, MemoryPlacement};
use crate::{Span, SrcPos};

enum EType {
	Basic,
	Regions(Range<u32>, Range<u32>),
}

pub struct Error {
	e_type: EType,
	first: IdentId,
	second: IdentId,
}

impl Error {
	fn with_regions(
		first: IdentId, first_range: Range<u32>,
		second: IdentId, second_range: Range<u32>,
	) -> Self {
		Self { first, second,
			e_type: EType::Regions(first_range, second_range),
		}
	}

	fn new(first: IdentId, second: IdentId) -> Self {
		Self { first, second, e_type: EType::Basic }
	}

	pub fn display(self,
		input: &crate::input::Data,
		lex_data: &crate::lexer::Data,
	) -> String {
		match self.e_type {
			EType::Basic => {
				format!("Overlap between {} and {}",
					lex_data.text(input, &self.first),
					lex_data.text(input, &self.second),
				)
			}
			EType::Regions(range, range2) => {
				format!("Overlap between regions {}[{}..{}] and {}[{}..{}]",
					lex_data.text(input, &self.first), range.start, range.end,
					lex_data.text(input, &self.second), range2.start, range2.end,
				)
			}
		}
	}
}

pub fn eval(
	prs_data: &PrsData<SrcPos>,
	pak_data: &PakData,
) -> Result<IdentMap<u32>, Error> {
	let mut memory_map = IntervalTree::<u32, IdentId>::default();
	for (id, region) in &prs_data.regions {
		memory_map.insert(region.span.start..region.span.end, *id);
	}

	// Check for region overlap
	for (id, region) in &prs_data.regions {
		let Span { start, end } = region.span;
		for (interval, name) in memory_map.iter_overlaps(&(start..end)) {
			if name == id { continue };
			return Err(Error::with_regions(*id, start..end, *name, interval.clone()));
		}
	}

	let mut region_offsets = IdentMap::<u32>::new();

	let mut table_map = IntervalTree::<u32, IdentId>::default();
	for (id, table) in &prs_data.tables {
		let data_size = pak_data.tables[id].size as u32;

		let Some(start) = table.placement.map(|mp| {
			placement_start(prs_data, data_size, &mut region_offsets, mp)
		}) else {
			continue
		};

		let end = start + data_size;
		if let Some(old_id) = table_map.insert(start..end, *id) {
			return Err(Error::new(*id, old_id));
		}
	}

	let mut record_map = IntervalTree::<u32, IdentId>::default();
	for (id, record) in &prs_data.records {
		let data_size = pak_data.records[id].size as u32;

		let Some(start) = record.placement.map(|mp| {
			placement_start(prs_data, data_size, &mut region_offsets, mp)
		}) else {
			continue
		};

		let end = start + data_size;
		if let Some(old_id) = record_map.insert(start..end, *id) {
			return Err(Error::new(*id, old_id));
		}

		for (_, name) in table_map.iter_overlaps(&(start..end)) {
			return Err(Error::new(*id, *name));
		}
	}

	Ok(region_offsets)
}

fn placement_start(
	prs_data: &PrsData<SrcPos>,
	data_size: u32,
	region_offsets: &mut IdentMap<u32>,
	placement: MemoryPlacement,
) -> u32 {
	match placement {
		MemoryPlacement::Region(region) => {
			let region_base = prs_data.regions[&region].span.start;
			let region_offset = region_offsets.entry(region)
					.or_default();
			let start = region_base + *region_offset;
			*region_offset += data_size;
			start
		}
		MemoryPlacement::Address(start) => start,
	}
}
