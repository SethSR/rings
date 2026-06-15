
use std::ops::Range;

use interavl::IntervalTree;

use crate::identifier::{IdentId, Map as IdentMap};
use crate::packing::Data as PakData;
use crate::parser::{Data as PrsData, RegionMap};
use crate::{Span, SrcPos};

enum EType {
	Basic,
	Regions(Span<u32>, Span<u32>),
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
			e_type: EType::Regions(first_range.into(), second_range.into()),
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
				format!("Overlap between regions {}[{}] and {}[{}]",
					lex_data.text(input, &self.first), range,
					lex_data.text(input, &self.second), range2,
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
		memory_map.insert(region.span.into(), *id);
	}

	// Check for region overlap
	for (id, region) in &prs_data.regions {
		for (interval, name) in memory_map.iter_overlaps(&region.span.into()) {
			if name == id { continue };
			return Err(Error::with_regions(*id, region.span.into(), *name, interval.clone()));
		}
	}

	let mut region_tracker = RegionPlacementTracker::new(&prs_data.regions);
	let mut interval_tracker = IntervalMap::default();

	// Place Tables at their address
	for (id, start_addr) in &prs_data.table_address {
		let data_size = pak_data.tables[id].size as u32;
		let interval = *start_addr..(start_addr + data_size);
		interval_tracker.insert_table(*id, interval)?;
	}

	// Place Tables in their Region
	for (id, region_id) in &prs_data.table_regions {
		let data_size = pak_data.tables[id].size as u32;
		let start_addr = region_tracker.next_location(*region_id, data_size);
		let interval = start_addr..(start_addr + data_size);
		interval_tracker.insert_table(*id, interval)?;
	}

	// Records at their address
	for (id, start_addr) in &prs_data.record_address {
		let data_size = pak_data.records[id].size as u32;
		let interval = *start_addr..(start_addr + data_size);
		interval_tracker.insert_record(*id, interval)?;
	}

	// Place Records in their Region
	for (id, region_id) in &prs_data.record_regions {
		let data_size = pak_data.records[id].size as u32;
		let start_addr = region_tracker.next_location(*region_id, data_size);
		let interval = start_addr..(start_addr + data_size);
		interval_tracker.insert_record(*id, interval)?;
	}

	Ok(interval_tracker.locations)
}

struct RegionPlacementTracker<'a> {
	regions: &'a RegionMap,
	offsets: IdentMap<u32>,
}

impl<'a> RegionPlacementTracker<'a> {
	fn new(regions: &'a RegionMap) -> Self {
		Self {
			regions,
			offsets: IdentMap::default(),
		}
	}

	fn next_location(&mut self,
		region_id: IdentId,
		data_size: u32,
	) -> u32 {
		let region_base = self.regions[&region_id].span.start;
		let region_offset = self.offsets.entry(region_id)
			.or_default();
		*region_offset += data_size;
		region_base + *region_offset - data_size
	}
}

#[derive(Default)]
struct IntervalMap {
	tables: IntervalTree<u32, IdentId>,
	records: IntervalTree<u32, IdentId>,
	locations: IdentMap<u32>,
}

impl IntervalMap {
	fn insert_table(&mut self,
		table_id: IdentId,
		interval: Range<u32>,
	) -> Result<(), Error> {
		let start_addr = interval.start;

		if let Some(old_id) = self.tables.insert(interval, table_id) {
			return Err(Error::new(table_id, old_id));
		}

		self.locations.insert(table_id, start_addr);

		Ok(())
	}

	fn insert_record(&mut self,
		record_id: IdentId,
		interval: Range<u32>,
	) -> Result<(), Error> {
		let start_addr = interval.start;

		for (_, table_id) in self.tables.iter_overlaps(&interval) {
			return Err(Error::new(record_id, *table_id));
		}

		if let Some(old_id) = self.records.insert(interval, record_id) {
			return Err(Error::new(record_id, old_id));
		}

		self.locations.insert(record_id, start_addr);
		Ok(())
	}
}

