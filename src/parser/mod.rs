
use std::collections::{HashMap, HashSet, VecDeque};

use crate::identifier::{IdentId, Identifier};
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::token::{Id as TokenId, Kind as TokenKind};
use crate::Target;

mod ast;
mod cursor;
mod data;
mod error;
mod expression;
mod parse_procedures;
mod types;

#[cfg(test)]
mod value_tests;
#[cfg(test)]
mod region_tests;
#[cfg(test)]
mod record_tests;
#[cfg(test)]
mod proc_tests;
#[cfg(test)]
mod table_tests;

use expression::{evaluate_placement, evaluate_expr};
use ast::KindList;
use data::{Procedure, Record, Region, Table, Value};

pub use ast::{AstId, AstKind};
pub use data::{ProcMap, RecordMap, RegionMap, TableMap, TypeMap, ValueMap};
pub use types::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemoryPlacement {
	Address(u32),
	Region(IdentId),
}

#[derive(Debug, Default)]
pub struct Data {
	pub values: ValueMap,
	pub regions: RegionMap,
	pub records: RecordMap,
	pub tables: TableMap,
	pub procedures: ProcMap,
	pub types: TypeMap,
}

pub fn eval(input: &InputData, lex_data: &LexData, should_print: bool,
) -> Result<Data, crate::error::Error> {
	let tasks = scan_tasks(lex_data)
		.map_err(|e| e.into_comp_error(input, lex_data, crate::error::Kind::Parser))?;
	if should_print {
		eprintln!("{tasks:?}");
	}

	let data = process_tasks(lex_data, tasks)
		.map_err(|e| e.into_comp_error(input, lex_data, crate::error::Kind::Parser))?;
	if should_print {
		eprintln!("{data:?}");
	}

	Ok(data)
}

#[derive(Debug)]
enum Task {
	Value {
		ident: IdentId,
		start: TokenId,
	},
	Region {
		ident: IdentId,
		start_size: TokenId,
		start_address: TokenId,
	},
	Record {
		ident: IdentId,
		location: TokenId,
		start_placement: Option<TokenId>,
		start_fields: TokenId,
	},
	Table {
		ident: IdentId,
		start_rows: TokenId,
		start_fields: TokenId,
		start_placement: Option<TokenId>,
	},
	Proc {
		ident: IdentId,
		target: Option<Target>,
		start: TokenId,
	},
}

fn scan_tasks(lex_data: &LexData,
) -> Result<VecDeque<Task>, error::Error> {
	let mut tasks = vec![];

	let mut cursor = cursor::Cursor::new(lex_data);
	while cursor.current() != TokenKind::Eof {
		let token = cursor.current();
		cursor.advance();
		match token {
			TokenKind::Value => {
				tasks.push(scan_value_task(&mut cursor)?);
			}

			TokenKind::Region => {
				tasks.push(scan_region_task(&mut cursor)?);
			}

			TokenKind::Record => {
				tasks.push(scan_record_task(&mut cursor)?);
			}

			TokenKind::Table => {
				tasks.push(scan_table_task(&mut cursor)?);
			}

			TokenKind::Main => {
				tasks.push(scan_proc(&mut cursor, "main".id(), None)?);
			}

			TokenKind::Sub => {
				tasks.push(scan_proc(&mut cursor, "sub".id(), None)?);
			}

			TokenKind::Proc => {
				tasks.push(scan_named_proc(&mut cursor, None)?);
			}

			TokenKind::M68k => {
				tasks.push(scan_target_proc(&mut cursor, Some(Target::M68k))?);
			}

			TokenKind::SH2 => {
				tasks.push(scan_target_proc(&mut cursor, Some(Target::SH2))?);
			}

			TokenKind::X64 => {
				tasks.push(scan_target_proc(&mut cursor, Some(Target::X86_64))?);
			}

			TokenKind::Z80 => {
				tasks.push(scan_target_proc(&mut cursor, Some(Target::Z80))?);
			}
			_ => {
				return Err(cursor.expected_token("top-level statement"))
			}
		}
	}

	Ok(tasks.into_iter().collect())
}

fn scan_value_task(cursor: &mut cursor::Cursor,
) -> Result<Task, error::Error> {
	let ident = cursor.expect_identifier("value name")?;
	cursor.expect(TokenKind::Eq)?;
	let start = skip_until(cursor, TokenKind::Semicolon)?;
	cursor.expect(TokenKind::Semicolon)?;
	Ok(Task::Value { ident, start })
}

fn scan_region_task(cursor: &mut cursor::Cursor,
) -> Result<Task, error::Error> {
	let ident = cursor.expect_identifier("region name")?;
	cursor.expect(TokenKind::OBracket)?;
	let start_size = skip_until(cursor, TokenKind::CBracket)?;
	cursor.expect(TokenKind::CBracket)?;
	cursor.expect(TokenKind::At)?;
	let start_address = skip_until(cursor, TokenKind::Semicolon)?;
	cursor.expect(TokenKind::Semicolon)?;
	Ok(Task::Region { ident, start_size, start_address })
}

/// Matches record syntax:
/// - `record <ident> @ <placement> {...}`
/// - `record <ident> {...}`
fn scan_record_task(cursor: &mut cursor::Cursor,
) -> Result<Task, error::Error> {
	let location = cursor.index();
	let ident = cursor.expect_identifier("record name")?;

	let start_placement = if cursor.expect(TokenKind::At).is_ok() {
		Some(skip_until(cursor, TokenKind::OBrace)?)
	} else {
		None
	};

	cursor.expect(TokenKind::OBrace)?;
	let start_fields = skip_until(cursor, TokenKind::CBrace)?;
	cursor.expect(TokenKind::CBrace)?;

	Ok(Task::Record { ident, location, start_placement, start_fields })
}

/// Matches Table syntax
/// - `table <ident>[<rows>] @ <placement> {...}`
/// - `table <ident>[<rows>] {...}`
fn scan_table_task(cursor: &mut cursor::Cursor,
) -> Result<Task, error::Error> {
	let ident = cursor.expect_identifier("table name")?;

	cursor.expect(TokenKind::OBracket)?;
	let start_rows = skip_until(cursor, TokenKind::CBracket)?;
	cursor.expect(TokenKind::CBracket)?;

	let start_placement = if cursor.expect(TokenKind::At).is_ok() {
		Some(skip_until(cursor, TokenKind::OBrace)?)
	} else {
		None
	};

	cursor.expect(TokenKind::OBrace)?;
	let start_fields = skip_until(cursor, TokenKind::CBrace)?;
	cursor.expect(TokenKind::CBrace)?;

	Ok(Task::Table { ident, start_rows, start_placement, start_fields })
}

fn check_braces(cursor: &mut cursor::Cursor) -> Result<(), error::Error> {
	skip_until(cursor, TokenKind::OBrace)?;
	cursor.expect(TokenKind::OBrace)?;
	let mut brace_count = 1;
	while brace_count > 0 && cursor.current() != TokenKind::Eof {
		brace_count += match cursor.current() {
			TokenKind::OBrace => 1,
			TokenKind::CBrace => -1,
			TokenKind::Eof => {
				return Err(cursor.expected_token("end of procedure").into());
			}
			_ => 0,
		};
		cursor.advance();
	}

	Ok(())
}

/// Matches initial procedures:
/// - `main {...}`
/// - `sub {...}`
fn scan_proc(cursor: &mut cursor::Cursor,
	ident: IdentId,
	target: Option<Target>,
) -> Result<Task, error::Error> {
	let start = cursor.index();
	check_braces(cursor)?;
	Ok(Task::Proc { ident, target, start })
}

/// Matches named procedures:
/// - `proc <ident>(...) <return> {...}`
fn scan_named_proc(cursor: &mut cursor::Cursor,
	target: Option<Target>,
) -> Result<Task, error::Error> {
	let name_id = cursor.expect_identifier("procedure name")?;
	scan_proc(cursor, name_id, target)
}

/// Matches target specific procedures:
/// - `<target> proc <ident>(...) <return> {...}`
/// - `<target> main {...}`
/// - `<target> sub {...}`
fn scan_target_proc(cursor: &mut cursor::Cursor,
	target: Option<Target>,
) -> Result<Task, error::Error> {
	match cursor.current() {
		TokenKind::Main => {
			cursor.advance();
			scan_proc(cursor, "main".id(), target)
		}
		TokenKind::Sub => {
			cursor.advance();
			scan_proc(cursor, "sub".id(), target)
		}
		_ => {
			cursor.expect(TokenKind::Proc)?;
			scan_named_proc(cursor, target)
		}
	}
}

fn skip_until(cursor: &mut cursor::Cursor,
	end_token: TokenKind,
) -> Result<TokenId, error::Error> {
	let start = cursor.index();
	loop {
		match cursor.current() {
			TokenKind::Eof => return Err(error::Error::UnexpectedEof { location: cursor.index() }),
			token if token == end_token => break Ok(start),
			_ => cursor.advance(),
		}
	}
}

fn process_tasks(lex_data: &LexData,
	mut queue: VecDeque<Task>,
) -> Result<Data, error::Error> {
	let mut data = Data::default();
	let mut locations = HashMap::default();

	let mut failed_tasks = HashSet::<IdentId>::new();
	let mut consecutive_failures = 0;

	while let Some(task) = queue.pop_front() {
		match task {
			Task::Value { ident, start } => {
				let mut cursor = cursor::Cursor::from_start(lex_data, start);
				match evaluate_expr(&mut cursor, &data.values, TokenKind::Semicolon) {
					Ok(value) => {
						data.values.insert(ident, value);
						failed_tasks.remove(&ident);
						consecutive_failures = 0;
					}
					Err(error::Error::UndefinedType { location, ident_id }) => {
						consecutive_failures = check_task_failure(
							&queue, &mut failed_tasks, consecutive_failures,
							location, ident, ident_id,
						)?;
						queue.push_back(task);
					}
					Err(e) => return Err(e),
				}
			}

			Task::Region { ident, start_size, start_address } => {
				match process_region(lex_data, &data, start_size, start_address) {
					Ok((start, end)) => {
						data.regions.insert(ident, Region::new(start, end));
						failed_tasks.remove(&ident);
						consecutive_failures = 0;
					}
					Err(error::Error::UndefinedType { location, ident_id }) => {
						consecutive_failures = check_task_failure(
							&queue, &mut failed_tasks, consecutive_failures,
							location, ident, ident_id,
						)?;
						queue.push_back(task);
					}
					Err(e) => return Err(e),
				}
			}

			Task::Record { ident, location, start_placement, start_fields } => {
				locations.insert(ident, location);

				match process_record(lex_data, &data, start_placement, start_fields) {
					Ok((placement, fields)) => {
						data.records.insert(ident, Record { placement, fields });
						failed_tasks.remove(&ident);
						consecutive_failures = 0;
					}
					Err(error::Error::UndefinedType { location, ident_id }) => {
						consecutive_failures = check_task_failure(
							&queue, &mut failed_tasks, consecutive_failures,
							location, ident, ident_id,
						)?;
						queue.push_back(task);
					}
					Err(e) => return Err(e),
				}
			}

			Task::Table { ident, start_rows, start_placement, start_fields } => {
				match process_table(lex_data, &data, start_placement, start_rows, start_fields) {
					Ok((placement, row_count, fields)) => {
						data.tables.insert(ident, Table { placement, row_count, fields});
						failed_tasks.remove(&ident);
						consecutive_failures = 0;
					}
					Err(error::Error::UndefinedType { location, ident_id }) => {
						consecutive_failures = check_task_failure(
							&queue, &mut failed_tasks, consecutive_failures,
							location, ident, ident_id,
						)?;
						queue.push_back(task);
					}
					Err(e) => return Err(e),
				}
			}

			Task::Proc { ident, target, start } => {
				match process_proc(lex_data, &mut data, ident, target, start) {
					Ok(proc) => {
						data.procedures.insert(ident, proc);
						failed_tasks.remove(&ident);
						consecutive_failures = 0;
					}
					Err(error::Error::UndefinedType { location, ident_id }) => {
						consecutive_failures = check_task_failure(
							&queue, &mut failed_tasks, consecutive_failures,
							location, ident, ident_id,
						)?;
						queue.push_back(task);
					}
					Err(e) => return Err(e),
				}
			}
		}
	}

	let mut duplicates = HashSet::with_capacity(data.records.len());
	for (name,_) in &data.records {
		if duplicates.contains(name) {
			return Err(error::Error::DuplicateDeclaration {
				location: locations[name],
				name_id: *name,
			});
		}
		duplicates.insert(name);
	}

	for (name,_) in &data.records {
		check_recursion(name, name, &data.records, &locations, &mut vec![])?;
	}

	Ok(data)
}

fn check_task_failure(
	queue: &VecDeque<Task>,
	failed_tasks: &mut HashSet<IdentId>,
	consecutive_failures: usize,
	location: TokenId,
	task_ident: IdentId,
	item_id: IdentId,
) -> Result<usize, error::Error> {
	if failed_tasks.contains(&task_ident) {
		let failure_count = consecutive_failures + 1;

		if failure_count > queue.len() {
			Err(error::Error::CircularDependency {
				location,
				name_id: task_ident,
				ident_id: item_id,
			})
		} else {
			Ok(failure_count)
		}
	} else {
		failed_tasks.insert(task_ident);
		Ok(consecutive_failures)
	}
}

fn check_recursion(
	root: &IdentId,
	curr: &IdentId,
	records: &RecordMap,
	locations: &HashMap<IdentId, TokenId>,
	visited: &mut Vec<IdentId>,
) -> Result<(), error::Error> {
	if visited.contains(curr) {
		return Ok(());
	}

	visited.push(*curr);

	if let Some(record) = records.get(curr) {
		for field in &record.fields {
			if let Type::Record(name) = &field.1 {
				if name == root {
					return Err(error::Error::RecursiveType {
						location: locations[name],
						name_id: *root,
					});
				}
				check_recursion(root, name, records, locations, visited)?;
			}
		}
	}

	Ok(())
}

fn process_region (
	lex_data: &LexData,
	data: &Data,
	start_size: TokenId, start_address: TokenId,
) -> Result<(u32,u32), error::Error> {
	let mut cursor_size = cursor::Cursor::from_start(lex_data, start_size);
	let result_size = evaluate_expr(&mut cursor_size, &data.values, TokenKind::CBracket);
	let mut cursor_address = cursor::Cursor::from_start(lex_data, start_address);
	let result_address = evaluate_expr(&mut cursor_address, &data.values, TokenKind::Semicolon);

	match (result_size, result_address) {
		(Ok(Value::Integer(region_size)), Ok(Value::Integer(region_address))) => {
			if !(0..u32::MAX as i64).contains(&region_address) {
				panic!("region address ({region_address}) out of range")
			}
			if !(0..u32::MAX as i64).contains(&(region_address + region_size)) {
				panic!("region address + size ({}) out of range", region_address + region_size)
			}
			let start = region_address as u32;
			let end = (region_address + region_size) as u32;
			Ok((start, end))
		}
		(Ok(Value::Decimal(_)),_) | (_,Ok(Value::Decimal(_))) => {
			panic!("decimal values not allowed in region declarations")
		}
		(Err(e),_) | (_,Err(e)) => Err(e),
	}
}

fn process_record(
	lex_data: &LexData,
	data: &Data,
	start_placement: Option<TokenId>,
	start_fields: TokenId,
) -> Result<(Option<MemoryPlacement>, Vec<(IdentId, Type)>), error::Error> {
	let placement = process_placement(lex_data, data, start_placement, TokenKind::OBrace)?;

	let mut cursor_fields = cursor::Cursor::from_start(lex_data, start_fields);
	process_fields(&mut cursor_fields, &data.records, TokenKind::CBrace)
		.map(|fields| (placement, fields))
}

fn process_table(
	lex_data: &LexData,
	data: &Data,
	start_placement: Option<TokenId>,
	start_rows: TokenId,
	start_fields: TokenId,
) -> Result<(Option<MemoryPlacement>, u16, Vec<(IdentId, Type)>), error::Error> {
	let mut cursor_rows = cursor::Cursor::from_start(lex_data, start_rows);
	let row_count = evaluate_expr(&mut cursor_rows, &data.values, TokenKind::CBracket)
			.map_err(|_| cursor_rows.expected_token("capacity expression"))?;
	let Value::Integer(row_count) = row_count else {
		panic!("decimal values not allowed in table row count declarations")
	};
	if !(0..u16::MAX as i64).contains(&row_count) {
		panic!("table row count ({row_count}) out of range")
	}

	let placement = process_placement(lex_data, data, start_placement, TokenKind::OBrace)?;

	let mut cursor_fields = cursor::Cursor::from_start(lex_data, start_fields);
	process_fields(&mut cursor_fields, &data.records, TokenKind::CBrace)
			.map(|fields| (placement, row_count as u16, fields))
}

fn process_placement(
	lex_data: &LexData,
	data: &Data,
	start_placement: Option<TokenId>,
	end_token: TokenKind,
) -> Result<Option<MemoryPlacement>, error::Error> {
	if let Some(start_placement) = start_placement {
		let mut cursor = cursor::Cursor::from_start(lex_data, start_placement);
		Ok(Some(evaluate_placement(&mut cursor, &data.values, &data.regions, end_token)?))
	} else {
		Ok(None)
	}
}

fn process_fields(cursor: &mut cursor::Cursor,
	records: &RecordMap,
	end_token: TokenKind,
) -> Result<Vec<(IdentId, Type)>, error::Error> {
	let mut fields = vec![];

	while cursor.current() != end_token {
		let ident = cursor.expect_identifier("field name")?;
		cursor.expect(TokenKind::Colon)?;
		let typ = cursor.expect_type(records)?;
		fields.push((ident, typ));
		if cursor.expect(TokenKind::Comma).is_err() {
			break;
		}
	}

	Ok(fields)
}

fn process_proc(
	lex_data: &LexData,
	data: &mut Data,
	proc_id: IdentId,
	target: Option<Target>,
	start: TokenId,
) -> Result<Procedure, error::Error> {
	let mut proc = Procedure {
		target,
		params: vec![],
		body: KindList::default(),
		ret_type: Type::Void,
	};

	let mut cursor = cursor::Cursor::from_start(lex_data, start);

	proc.params = if cursor.expect(TokenKind::OParen).is_ok() {
		let params = process_fields(&mut cursor, &data.records, TokenKind::CParen)?;
		cursor.expect(TokenKind::CParen)?;
		params
	} else {
		vec![]
	};

	proc.ret_type = if cursor.expect(TokenKind::Arrow).is_ok() {
		cursor.expect_type(&data.records)?
	} else {
		Type::Void
	};

	let start = AstId::new(proc.body.len());
	let mut block = parse_procedures::parse_block(
		&mut cursor, &mut proc.body, &mut data.types,
		proc_id, 0, &data.records,
	)?;
	let end = AstId::new(proc.body.len());

	let has_return = proc.body[start..end]
		.iter()
		.any(|kind| matches!(kind, AstKind::Return(_)));

	if !has_return {
		let ast_id = proc.body.push(AstKind::Return(None));
		block.push(ast_id);
	}

	proc.body.push(AstKind::Block(block));

	Ok(proc)
}
