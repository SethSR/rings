
use std::collections::VecDeque;

use crate::cursor::{Cursor, Error};
use crate::identifier::{self, Id as IdentId, Identifier};
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::rings_type::Type;
use crate::task::Task;
use crate::token::{Id as TokenId, Kind as TokenKind};
use crate::{fmt_size, type_size};
use crate::{Span, Target};
use super::ValueMap;

pub type RegionMap = identifier::Map<Region>;
pub type ProcMap = identifier::Map<ProcType>;
pub type RecordMap = identifier::Map<Record>;
#[cfg(feature="table")]
pub type TableMap = identifier::Map<Table>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
	pub name: IdentId,
	pub typ: Type,
	pub size: u32,
	pub offset: u16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Region {
	pub span: Span<u32>,
	pub alloc_position: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcType {
	pub params: Vec<(IdentId, Type)>,
	pub ret_type: Type,
	pub target: Option<Target>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
	pub fields: Vec<Param>,
	pub region: Option<IdentId>,
	pub size: u32,
}

impl Record {
	fn new(
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

#[cfg(feature="table")]
#[derive(Debug, PartialEq)]
pub struct Table {
	pub row_count: u32,
	pub column_spec: Vec<Param>,
	pub address: Option<u32>,
}

#[cfg(feature="table")]
impl Table {
	pub fn size(&self, data: &Data) -> u32 {
		let row_size: u32 = self.column_spec.iter()
			.map(|(_, field_type)| data.type_size(field_type))
			.sum();
		self.row_count * row_size
	}

	pub fn column_offset(&self, data: &Data, field_id: identifier::Id) -> Option<u32> {
		self.column_spec.iter()
			.position(|(id,_)| field_id == *id)
			.map(|idx| self.column_spec[..idx].iter()
				.map(|(_,field_type)| data.type_size(field_type) * self.row_count)
				.sum())
	}
}

type DiscResult<T> = Result<T, Error>;

#[derive(Debug, Default)]
pub struct Data {
	pub procedures: ProcMap,
	pub records: RecordMap,
	pub regions: RegionMap,
	pub values: ValueMap,
}

pub fn print(
	dsc_data: &Data,
	task_queue: &VecDeque<Task>,
	input: &InputData,
	lex_data: &LexData,
) {
	fn fields_to_str(input: &InputData,
		lex_data: &LexData,
		fields: &[Param],
	) -> String {
		fields.iter()
			.map(|param| {
				let field_name = crate::text(input, lex_data, &param.name);
				let type_name = crate::type_text(input, lex_data, &param.typ);
				format!("{field_name}:{type_name}")
			})
			.collect::<Vec<_>>()
			.join(", ")
	}

	println!("{:<16} | {:<9} | {:<9}", "REGION", "ADDRESS", "SIZE");
	println!("{:-<16} | {:-<9} | {:-<9}", "", "", "");
	for (ident_id, data) in dsc_data.regions.iter() {
		let name = crate::text(input, lex_data, ident_id);
		let address = data.span.start;
		let size = fmt_size((data.span.end - data.span.start) as usize);
		println!("{name:<16} | #{address:0>8X} | {size:<8}");
	}

	println!();
	println!("{:<16} | {:<8} | {:<9} | FIELDS",
		"RECORD", "SIZE", "ADDRESS");
	println!("{:-<16} | {:-<8} | {:-<9} | {:-<16}", "", "", "", "");
	for (ident_id, record) in dsc_data.records.iter() {
		let name = crate::text(input, lex_data, ident_id);
		let size = record.size();
		let address = record.region
			.map(|id| format!("#{:0>8X}", dsc_data.regions[&id].span.start))
			.unwrap_or("-".to_string());
		let field_str = fields_to_str(input, lex_data, &record.fields);
		println!("{name:<16} | {size:<8} | {address:9} | {field_str}");
	}

	#[cfg(feature="table")]
	{
		println!(f)?;
		println!("{:<16} | {:<10} | {:<8} | {:<9} | {:<9} | COLUMNS",
			"TABLE", "TOTAL SIZE", "ROW SIZE", "ROW COUNT", "ADDRESS")?;
		println!("{:-<16} | {:-<10} | {:-<8} | {:-<9} | {:-<9} | {:-<16}", "", "", "", "", "", "")?;
		for (ident_id, table) in dsc_data.tables.iter() {
			let name = dsc_data.text(ident_id);
			let size = table.size(dsc_data);
			let row_size = size / table.row_count;
			let address = table.address
					.map(|num| format!("#{num:0>8X}"))
					.unwrap_or("-".to_string());
			let field_str = fields_to_str(sel&table.column_spec);
			println!("{name:<16} | {size:<10} | {row_size:<8} | {:<9} | {address:9} | {field_str}", table.row_count)?;
		}
	}

	if !dsc_data.procedures.is_empty() {
		let (types, params): (Vec<_>, Vec<_>) = dsc_data.procedures.iter()
			.map(|(ident_id, data)| {
				let name = crate::text(input, lex_data, ident_id);
				let params = data.params.iter()
					.map(|(param_name, param_type)| {
						let param_name = crate::text(input, lex_data, param_name);
						let param_type = crate::type_text(input, lex_data, param_type);
						format!("{name:<32} | {param_name:<16} | {param_type:<16}")
					})
					.collect::<Vec<_>>()
					.join("\n");
				(
					format!("{name:<32} | {:<16?} | {:<16?}", data.ret_type, data.target),
					params,
				)
			})
			.unzip();

		println!();
		println!("{:<32} | {:<16} | TARGET",
			"PROCEDURE", "RETURN-TYPE");
		println!("{:-<32} | {:-<16} | {:-<16}", "", "", "");
		println!("{}", types.join("\n"));

		let params = params.into_iter()
				.filter(|p| !p.is_empty())
				.collect::<Vec<_>>();
		if !params.is_empty() {
			println!();
			println!("{:<32} | {:<16} | PARAM-TYPE",
				"PROCEDURE", "PARAM-NAME");
			println!("{:-<32} | {:-<16} | {:-<16}", "", "", "");
			println!("{}", params.join("\n"));
		}
	}

	if !task_queue.is_empty() {
		println!();
		println!("{:<32} | {:<11} | {:<10} | PREV QUEUE LENGTH",
			"TASK", "START TOKEN", "LATEST TOKEN");
		println!("{:-<32} | {:-<11} | {:-<10} | {:-<16}", "", "", "", "");
		for task in task_queue.iter() {
			println!("{:<32} | {:<11} | {:<10} | {:?}",
				crate::text(input, lex_data, &task.name_id),
				task.tok_start.index(),
				task.prev_furthest_token.index(),
				task.prev_queue_length,
			);
		}
	}

	println!();
}

pub fn eval(input: &InputData, lex_data: &LexData) -> DiscResult<(Data, VecDeque<Task>)> {
	let mut cursor = Cursor::default();
	let mut out = Data::default();
	let mut task_queue = VecDeque::default();

	loop {
		match cursor.current(&lex_data) {
			TokenKind::Main => {
				let tok_start = discover_init_proc(&mut cursor, &lex_data)?;
				let name_id = "main".id();
				task_queue.push_back(Task::new(name_id, tok_start));
				out.procedures.insert(name_id, ProcType { params: vec![], ret_type: Type::Unit, target: None });
			}

			TokenKind::Sub => {
				let tok_start = discover_init_proc(&mut cursor, &lex_data)?;
				let name_id = "sub".id();
				task_queue.push_back(Task::new(name_id, tok_start));
				out.procedures.insert(name_id, ProcType { params: vec![], ret_type: Type::Unit, target: None });
			}

			TokenKind::Proc => {
				let (name_id, tok_start, params, ret_type) = discover_proc(&mut cursor, &lex_data, &out.records)?;
				task_queue.push_back(Task::new(name_id, tok_start));
				out.procedures.insert(name_id, ProcType { params, ret_type, target: None });
			}

			TokenKind::M68k => {
				cursor.advance();
				let (name_id, tok_start, params, ret_type) = discover_target_proc(&mut cursor, &lex_data, &out.records)?;
				task_queue.push_back(Task::new(name_id, tok_start));
				out.procedures.insert(name_id, ProcType { params, ret_type, target: Some(Target::M68k) });
			}

			TokenKind::SH2 => {
				cursor.advance();
				let (name_id, tok_start, params, ret_type) = discover_target_proc(&mut cursor, &lex_data, &out.records)?;
				task_queue.push_back(Task::new(name_id, tok_start));
				out.procedures.insert(name_id, ProcType { params, ret_type, target: Some(Target::SH2) });
			}

			TokenKind::X64 => {
				cursor.advance();
				let (name_id, tok_start, params, ret_type) = discover_target_proc(&mut cursor, &lex_data, &out.records)?;
				task_queue.push_back(Task::new(name_id, tok_start));
				out.procedures.insert(name_id, ProcType { params, ret_type, target: Some(Target::X86_64) });
			}

			TokenKind::Z80 => {
				cursor.advance();
				let (name_id, tok_start, params, ret_type) = discover_target_proc(&mut cursor, &lex_data, &out.records)?;
				task_queue.push_back(Task::new(name_id, tok_start));
				out.procedures.insert(name_id, ProcType { params, ret_type, target: Some(Target::Z80) });
			}

			TokenKind::Region => {
				cursor.advance();
				let name_id = cursor.expect_identifier(&lex_data, "region name")?;
				let region = discover_region(&mut cursor, &input, &lex_data)?;
				out.regions.insert(name_id, region);
			}

			TokenKind::Record => {
				cursor.advance();
				let ident_id = cursor.expect_identifier(&lex_data, "record name")?;
				let record = discover_record(&mut cursor, &lex_data, &out.records, &out.regions)?;
				out.records.insert(ident_id, record);
			}

			#[cfg(feature="table")]
			TokenKind::Table => {
				cursor.advance();
				let ident_id = cursor.expect_identifier(data, "table name")?;
				let table = discover_table(&mut cursor, data)?;
				data.tables.insert(ident_id, table);
			}

			#[cfg(feature="index")]
			TokenKind::Index => return Err(error::error(data,
				"indexes not yet implemented",
				cursor.index())),

			TokenKind::Value => loop {
				match cursor.current(lex_data) {
					TokenKind::Eof => return Err(cursor.expected_token("top-level statement")),
					TokenKind::Semicolon => break cursor.advance(),
					_ => cursor.advance(),
				}
			}

			TokenKind::Eof => break,
			_ => return Err(cursor.expected_token("top-level statement")),
		}
	}

	Ok((out, task_queue))
}

fn check_braces(cursor: &mut Cursor, lex_data: &LexData) -> DiscResult<()> {
	cursor.expect(lex_data, TokenKind::OBrace)?;
	let mut brace_count = 1;
	while brace_count > 0 && cursor.current(lex_data) != TokenKind::Eof {
		brace_count += match cursor.current(lex_data) {
			TokenKind::OBrace => 1,
			TokenKind::CBrace => -1,
			TokenKind::Eof => {
				return Err(cursor.expected_token("end of procedure"));
			}
			_ => 0,
		};
		cursor.advance();
	}

	Ok(())
}

fn discover_init_proc(cursor: &mut Cursor, lex_data: &LexData,
) -> DiscResult<TokenId> {
	cursor.advance();
	let tok_start = cursor.index();
	check_braces(cursor, lex_data)?;
	Ok(tok_start)
}

fn discover_proc(cursor: &mut Cursor, lex_data: &LexData,
	records: &RecordMap,
) -> DiscResult<(IdentId, TokenId, Vec<(IdentId, Type)>, Type)> {
	cursor.expect(lex_data, TokenKind::Proc)?;
	let name_id = cursor.expect_identifier(lex_data, "procedure name")?;
	cursor.expect(lex_data, TokenKind::OParen)?;
	let params = discover_fields(cursor, lex_data, records, TokenKind::CParen)?;
	cursor.expect(lex_data, TokenKind::CParen)?;
	let ret_type = if cursor.expect(lex_data, TokenKind::Arrow).is_ok() {
		cursor.expect_type(lex_data, records)?
	} else {
		Type::Unit
	};
	let tok_start = cursor.index();
	check_braces(cursor, lex_data)?;
	Ok((name_id, tok_start, params, ret_type))
}

fn discover_target_proc(cursor: &mut Cursor, lex_data: &LexData,
	records: &RecordMap,
) -> DiscResult<(IdentId, TokenId, Vec<(IdentId, Type)>, Type)> {
	Ok(match cursor.current(lex_data) {
		TokenKind::Main => {
			("main".id(), discover_init_proc(cursor, lex_data)?, vec![], Type::Unit)
		}
		TokenKind::Sub => {
			("sub".id(), discover_init_proc(cursor, lex_data)?, vec![], Type::Unit)
		}
		_ => discover_proc(cursor, lex_data, records)?,
	})
}

fn discover_fields(cursor: &mut Cursor, lex_data: &LexData,
	records: &RecordMap,
	end_token: TokenKind,
) -> DiscResult<Vec<(IdentId, Type)>> {
	let mut fields = Vec::default();
	while end_token != cursor.current(lex_data) {
		let field_id = cursor.expect_identifier(lex_data, "field name")?;
		cursor.expect(lex_data, TokenKind::Colon)?;
		let field_type = cursor.expect_type(lex_data, records)?;
		fields.push((field_id, field_type));
		if cursor.current(lex_data) != TokenKind::Comma {
			break;
		}
		cursor.advance();
	}
	Ok(fields)
}

fn discover_region(cursor: &mut Cursor, input: &InputData, lex_data: &LexData,
) -> DiscResult<Region> {
	cursor.expect(lex_data, TokenKind::OBracket)?;
	let byte_count = cursor.expect_u32(input, lex_data, "region size")?;
	cursor.expect(lex_data, TokenKind::CBracket)?;
	cursor.expect(lex_data, TokenKind::At)?;
	let address = cursor.expect_u32(input, lex_data, "region address")?;
	cursor.expect(lex_data, TokenKind::Semicolon)?;
	Ok(Region {
		span: Span { start: address, end: address + byte_count },
		alloc_position: 0,
	})
}

/// Check after an '@' for a defined region or an address location
fn discover_address(cursor: &mut Cursor, lex_data: &LexData,
	regions: &RegionMap,
) -> DiscResult<Option<IdentId>> {
	if cursor.expect(lex_data, TokenKind::At).is_ok() {
		if let Ok(id) = cursor.expect_identifier(lex_data, "region name") {
			if regions.contains_key(&id) {
				return Ok(Some(id));
			}
		}

		Err(Error::ExpectedToken {
			expected: "address or region ID".to_string(),
			found: cursor.index(),
		})
	} else {
		Ok(None)
	}
}

fn discover_record(cursor: &mut Cursor, lex_data: &LexData,
	records: &RecordMap, regions: &RegionMap,
) -> DiscResult<Record> {
	let region = discover_address(cursor, lex_data, regions)?;
	cursor.expect(lex_data, TokenKind::OBrace)?;
	let fields = discover_fields(cursor, lex_data, records, TokenKind::CBrace)?;
	cursor.expect(lex_data, TokenKind::CBrace)?;
	Ok(Record::new(records, fields, region))
}

#[cfg(feature="table")]
fn discover_table(cursor: &mut Cursor, data: &mut Data) -> DiscResult<Table> {
	cursor.expect(data, TokenKind::OBracket)?;
	let row_count = cursor.expect_u32(data, "table size")?;
	cursor.expect(data, TokenKind::CBracket)?;
	let address = discover_address(cursor, data)?;
	cursor.expect(data, TokenKind::OBrace)?;
	let column_spec = discover_fields(cursor, data, TokenKind::CBrace)?;
	cursor.expect(data, TokenKind::CBrace)?;
	Ok(Table {
		address,
		row_count,
		column_spec,
	})
}

#[cfg(test)]
mod can_parse {
	use crate::{error, lexer};

	use super::*;

	fn setup(source: &str) -> (Data, VecDeque<Task>) {
		let input = crate::input::eval(file!().to_string(), source.into());

		let lex_data = lexer::eval(&input.source)
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));

		let (dsc_data, task_queue) = eval(&input, &lex_data)
			.map_err(|e| e.into_comp_error(&input, &lex_data, error::Kind::Discovery))
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));
		(dsc_data, task_queue)
	}

	#[test]
	fn procedures() {
		let (data, task_queue) = setup("value a = 5; proc b() {}");
		assert_eq!(task_queue.len(), 1);
		assert_eq!(task_queue[0], Task::new(
			"b".id(),
			TokenId::new(9),
		));
		assert_eq!(data.procedures.len(), 1);
		assert_eq!(data.procedures[&"b".id()], ProcType {
			params: vec![],
			ret_type: Type::Unit,
			target: None,
		});
	}

	#[test]
	fn procedure_with_params() {
		let (data, task_queue) = setup("proc a(b: s8, c: s8) {}");
		assert_eq!(task_queue.len(), 1);
		assert_eq!(task_queue[0], Task::new(
			"a".id(),
			TokenId::new(11),
		));
		assert_eq!(data.procedures.len(), 1);
		assert_eq!(data.procedures[&"a".id()], ProcType {
			params: vec![
				("b".id(), Type::s8_top()),
				("c".id(), Type::s8_top()),
			],
			ret_type: Type::Unit,
			target: None,
		});
	}

	#[test]
	fn procedure_with_return() {
		let (data, task_queue) = setup("proc a() -> s8 {}");
		assert_eq!(task_queue.len(), 1);
		assert_eq!(task_queue[0], Task::new(
			"a".id(),
			TokenId::new(6),
		));
		assert_eq!(data.procedures.len(), 1);
		assert_eq!(data.procedures[&"a".id()], ProcType {
			params: vec![],
			ret_type: Type::s8_top(),
			target: None,
		});
	}

	#[test]
	fn procedure_with_target() {
		let (data, task_queue) = setup("sh2 proc a() {}");
		assert_eq!(task_queue.len(), 1);
		assert_eq!(task_queue[0], Task::new(
			"a".id(),
			TokenId::new(5),
		));
		assert_eq!(data.procedures.len(), 1);
		assert_eq!(data.procedures[&"a".id()], ProcType {
			params: vec![],
			ret_type: Type::Unit,
			target: Some(Target::SH2),
		});
	}

	#[test]
	fn init_procedure_with_target() {
		let (data, task_queue) = setup("sh2 main {} z80 sub {}");
		assert_eq!(task_queue.len(), 2);
		assert_eq!(task_queue, [
			Task::new("main".id(), TokenId::new(2)),
			Task::new("sub".id(), TokenId::new(6)),
		]);
		assert_eq!(data.procedures.len(), 2);
		assert_eq!(data.procedures[&"main".id()], ProcType {
			params: vec![],
			ret_type: Type::Unit,
			target: Some(Target::SH2),
		});
		assert_eq!(data.procedures[&"sub".id()], ProcType {
			params: vec![],
			ret_type: Type::Unit,
			target: Some(Target::Z80),
		});
	}

	#[test]
	fn region() {
		let (data, _) = setup("region a[1024] @ 0x0020_0000;");
		assert_eq!(data.regions.len(), 1);
		assert_eq!(data.regions[&"a".id()], Region {
			span: (0x20_0000..0x20_0400).into(),
			alloc_position: 0,
		});
	}

	#[test]
	fn empty_record() {
		let (data, _) = setup("record a {}");
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(), 0);
		assert_eq!(record.region, None);
		assert_eq!(record.fields.len(), 0);
	}

	#[test]
	fn record_with_one_field_no_trailing_comma() {
		let (data, _) = setup("record a { b: s8 }");
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(), 1);
		assert_eq!(record.region, None);
		assert_eq!(record.fields, [Param { name: "b".id(), typ: Type::s8_top(), size: 1, offset: 0 }]);
	}

	#[test]
	fn record_with_one_field_and_trailing_comma() {
		let (data, _) = setup("record a { b: s8, }");
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(), 1);
		assert_eq!(record.region, None);
		assert_eq!(record.fields, [Param { name: "b".id(), typ: Type::s8_top(), size: 1, offset: 0 }]);
	}

	#[test]
	fn record_with_multiple_fields() {
		let (data, _) = setup("record a { b: s8, c: s8 }");
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(), 2);
		assert_eq!(record.region, None);
		assert_eq!(record.fields, [
			Param { name: "b".id(), typ: Type::s8_top(), size: 1, offset: 0 },
			Param { name: "c".id(), typ: Type::s8_top(), size: 1, offset: 1 },
		]);
	}

	#[test]
	fn record_with_user_defined_field() {
		let (data, _) = setup("record a {} record b { c: a }");
		assert_eq!(data.records.len(), 2);
		let record = &data.records[&"b".id()];
		assert_eq!(record.size(), 0);
		assert_eq!(record.region, None);
		assert_eq!(record.fields, [
			Param { name: "c".id(), typ: Type::Record("a".id()), size: 0, offset: 0 },
		]);
	}

	#[test]
	fn record_with_region() {
		let (data, _) = setup("
			region b[8] @ 32;
			record a @ b {}
		");
		assert_eq!(data.regions.len(), 1);
		assert_eq!(data.regions[&"b".id()], Region {
			span: (32..40).into(),
			alloc_position: 0,
		});
		assert_eq!(data.records.len(), 1);
		let record = &data.records[&"a".id()];
		assert_eq!(record.size(), 0);
		assert_eq!(record.region, Some("b".id()));
		assert!(record.fields.is_empty());
	}

	#[cfg(feature="table")]
	#[test]
	fn empty_table() {
		let (data, task_queue) = setup("table a[10] {}");
		assert_eq!(data.tables.len(), 1);
		let table = &data.tables[&"a".id()];
		assert_eq!(table.row_count, 10);
		assert_eq!(table.column_spec, []);
		assert_eq!(table.size(&data), 0);
		assert_eq!(table.address, None);
	}

	#[cfg(feature="table")]
	#[test]
	fn table_with_one_field() {
		let (data, task_queue) = setup("table a[10] { b: u32 }");
		assert_eq!(data.tables.len(), 1);
		let table = &data.tables[&"a".id()];
		assert_eq!(table.row_count, 10);
		assert_eq!(table.column_spec, [("b".id(), Type::U32)]);
		assert_eq!(table.size(&data), 40);
		assert_eq!(table.address, None);
	}

	#[cfg(feature="table")]
	#[test]
	fn table_with_multiple_field() {
		let (data, task_queue) = setup("table a[10] { b: u32, c: s16 }");
		assert_eq!(data.tables.len(), 1);
		let table = &data.tables[&"a".id()];
		assert_eq!(table.row_count, 10);
		assert_eq!(table.column_spec, [
			("b".id(), Type::U32),
			("c".id(), Type::S16),
		]);
		assert_eq!(table.size(&data), 60);
		assert_eq!(table.address, None);
	}

	#[cfg(feature="table")]
	#[test]
	fn table_with_user_defined_field() {
		let (data, task_queue) = setup("record a { a1: s16 } table b[10] { b1: a }");
		assert_eq!(data.tables.len(), 1);
		let table = &data.tables[&"b".id()];
		assert_eq!(table.row_count, 10);
		assert_eq!(table.column_spec, [
			("b1".id(), Type::Record("a".id())),
		]);
		assert_eq!(table.size(&data), 20);
		assert_eq!(table.address, None);
	}
}

