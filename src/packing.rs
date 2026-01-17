
use std::collections::VecDeque;

use crate::identifier::{IdentId, Map as IdentMap};
use crate::parser::{Data as PrsData, RecordMap, TableMap, Type};
use crate::SrcPos;

#[derive(Debug)]
pub struct Data {
	pub records: IdentMap<RecordPacking>,
	pub tables: IdentMap<TablePacking>,
}

pub fn eval(prs_data: &PrsData<SrcPos>) -> Data {
	let records = layout_records(&prs_data.records);
	let tables = layout_tables(&prs_data.tables, &records);
	Data { records, tables }
}

#[derive(Debug)]
pub struct RecordPacking {
	pub offsets: Box<[u8]>,
	pub sizes: Box<[u8]>,
	pub size: u8,
}

#[derive(Debug)]
pub struct TablePacking {
	pub offsets: Box<[u16]>,
	pub sizes: Box<[u8]>,
	pub size: u16,
}

fn calc_offset(location: u16, size: u16) -> u16 {
	match size {
		1 => 0,
		2 => location & 1,
		_ => (4 - (location & 3)) & 3,
	}
}

fn calc_size(typ: &Type, rec_sizes: &IdentMap<RecordPacking>) -> Option<u8> {
	match typ {
		Type::Bool | Type::S8 | Type:: U8 => Some(1),
		Type::S16 | Type::U16 => Some(2),
		Type::S32 | Type::U32 => Some(4),
		Type::Record(rec) => rec_sizes.get(rec)
				.map(|layout| layout.sizes.iter().sum()),
		Type::Table(_) => panic!("table types not allowed in compound types"),
		Type::Void => panic!("void types not allowed in compound types"),
	}
}

fn layout_records(records: &RecordMap) -> IdentMap<RecordPacking> {
	let mut tasks: VecDeque<IdentId> = records.iter()
			.map(|(rec_id,_)| *rec_id)
			.collect();
	let mut rec_sizes: IdentMap<RecordPacking> = IdentMap::with_capacity(records.len());

	while let Some(rec_id) = tasks.pop_front() {
		let mut offsets = vec![];
		let mut sizes = vec![];
		let mut offset = 0u8;

		for (_, field_type) in &records[&rec_id].fields {
			let Some(size) = calc_size(field_type, &rec_sizes) else {
				// Unknown record. Recycle task and try again.
				tasks.push_back(rec_id);
				continue;
			};
			offset += calc_offset(offset as u16, size as u16) as u8;
			offsets.push(offset);
			sizes.push(size);
			offset += size;
		}

		rec_sizes.insert(rec_id, RecordPacking {
			offsets: offsets.into(),
			sizes: sizes.into(),
			size: offset,
		});
	}

	rec_sizes
}

fn layout_tables(tables: &TableMap, rec_sizes: &IdentMap<RecordPacking>) -> IdentMap<TablePacking> {
	let mut tab_sizes: IdentMap<TablePacking> = IdentMap::with_capacity(tables.len());

	for (tab_id,_) in tables {
		let mut offsets = vec![];
		let mut sizes = vec![];
		let mut offset = 0;

		let row_count= tables[&tab_id].row_count;
		for (_, field_type) in &tables[&tab_id].fields {
			let size = calc_size(field_type, &rec_sizes)
					.expect("Unknown record after record-layout pass");
			let column_size = size as u16 * row_count;
			// NOTE - srenshaw - We use the column size to calculate the offset to ensure any loads
			// directly to that memory's offset will have a 4-byte boundary. Which should help with any
			// alignment issues.
			offset += calc_offset(offset, column_size);
			offsets.push(offset);
			sizes.push(size);
			offset += column_size;
		}

		tab_sizes.insert(*tab_id, TablePacking {
			offsets: offsets.into(),
			sizes: sizes.into(),
			// NOTE - srenshaw - Similar to above, aligning to 4-bytes should help with ensuring region
			// layout won't cause any alignment issues on hardware.
			size: offset + calc_offset(offset, 4),
		});
	}

	tab_sizes
}

#[cfg(test)]
mod tests {
	use crate::identifier::Identifier;

	fn setup(source: &str) -> super::Data {
		let in_data = crate::input::eval(file!().into(), source.into());
		let lex_data = crate::lexer::eval(source)
				.map_err(|e| e.display(&in_data))
				.unwrap_or_else(|e| panic!("{e}"));
		let prs_data = crate::parser::eval(&in_data, &lex_data, true)
				.map_err(|e| e.display(&in_data))
				.unwrap_or_else(|e| panic!("{e}"));
		super::eval(&prs_data)
	}

	#[test]
	fn empty_record() {
		let lay_data = setup("record a {}");
		let record = lay_data.records.get(&"a".id())
				.expect("record packing must exist");
		assert_eq!(*record.offsets, []);
		assert_eq!(*record.sizes, []);
		assert_eq!(record.size, 0);
	}

	#[test]
	fn simple_record() {
		let lay_data = setup("record a { b: u8 }");
		let record = lay_data.records.get(&"a".id())
				.expect("record packing must exist");
		assert_eq!(*record.offsets, [ 0 ]);
		assert_eq!(*record.sizes, [ 1 ]);
		assert_eq!(record.size, 1);
	}

	#[test]
	fn complex_records_increasing() {
		let lay_data = setup("record a { b: u8, c: u16, d: u32 }");
		let record = lay_data.records.get(&"a".id())
				.expect("record packing must exist");
		assert_eq!(*record.offsets, [ 0, 2, 4 ]);
		assert_eq!(*record.sizes, [ 1, 2, 4 ]);
		assert_eq!(record.size, 8);
	}

	#[test]
	fn complex_records_decreasing() {
		let lay_data = setup("record a { b: u32, c: u16, d: u8 }");
		let record = lay_data.records.get(&"a".id())
				.expect("record packing must exist");
		assert_eq!(*record.offsets, [ 0, 4, 6 ]);
		assert_eq!(*record.sizes, [ 4, 2, 1 ]);
		assert_eq!(record.size, 7);
	}

	#[test]
	fn small_types_record() {
		let lay_data = setup("record a { b: u8, c: u8, d: u8, e: u8, f: u8 }");
		let record = lay_data.records.get(&"a".id())
				.expect("record packing must exist");
		assert_eq!(*record.offsets, [ 0, 1, 2, 3, 4 ]);
		assert_eq!(*record.sizes, [ 1, 1, 1, 1, 1 ]);
		assert_eq!(record.size, 5);
	}

	#[test]
	fn medium_types_record() {
		let lay_data = setup("record a { b: u16, c: u16, d: u16, e: u16, f: u16 }");
		let record = lay_data.records.get(&"a".id())
				.expect("record packing must exist");
		assert_eq!(*record.offsets, [ 0, 2, 4, 6, 8 ]);
		assert_eq!(*record.sizes, [ 2, 2, 2, 2, 2 ]);
		assert_eq!(record.size, 10);
	}

	#[test]
	fn large_types_record() {
		let lay_data = setup("record a { b: u32, c: u32, d: u32, e: u32, f: u32 }");
		let record = lay_data.records.get(&"a".id())
				.expect("record packing must exist");
		assert_eq!(*record.offsets, [ 0, 4, 8, 12, 16 ]);
		assert_eq!(*record.sizes, [ 4, 4, 4, 4, 4 ]);
		assert_eq!(record.size, 20);
	}

	#[test]
	fn empty_table() {
		let lay_data = setup("table a[0] {}");
		let table = lay_data.tables.get(&"a".id())
				.expect("table packing must exist");
		assert_eq!(*table.offsets, []);
		assert_eq!(*table.sizes, []);
		assert_eq!(table.size, 0);
	}

	#[test]
	fn simple_table_with_no_rows() {
		let lay_data = setup("table a[0] { b: u8 }");
		let table = lay_data.tables.get(&"a".id())
				.expect("table packing must exist");
		assert_eq!(*table.offsets, [ 0 ]);
		assert_eq!(*table.sizes, [ 1 ]);
		assert_eq!(table.size, 0);
	}

	#[test]
	fn empty_table_with_rows() {
		let lay_data = setup("table a[10] {}");
		let table = lay_data.tables.get(&"a".id())
				.expect("table packing must exist");
		assert_eq!(*table.offsets, []);
		assert_eq!(*table.sizes, []);
		assert_eq!(table.size, 0);
	}

	#[test]
	fn complex_table_increasing() {
		let lay_data = setup("table a[9] { b: u8, c: u16, d: u32 }");
		let table = lay_data.tables.get(&"a".id())
				.expect("table packing must exist");
		assert_eq!(*table.offsets, [ 0, 12, 32 ]);
		assert_eq!(*table.sizes, [ 1, 2, 4 ]);
		assert_eq!(table.size, 68);
	}

	#[test]
	fn complex_table_decreasing() {
		let lay_data = setup("table a[9] { b: u32, c: u16, d: u8 }");
		let table = lay_data.tables.get(&"a".id())
				.expect("table packing must exist");
		assert_eq!(*table.offsets, [ 0, 36, 56 ]);
		assert_eq!(*table.sizes, [ 4, 2, 1 ]);
		assert_eq!(table.size, 68);
	}
}
