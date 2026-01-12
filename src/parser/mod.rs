
use std::collections::{HashMap, HashSet, VecDeque};

use crate::identifier::IdentId;
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::token::{Id as TokenId, Kind as TokenKind};

mod cursor;
mod discovery;
mod error;
mod expression;
mod parser;
mod record;
mod region;
mod task;
mod types;
mod value;

#[cfg(test)]
mod value_tests;
#[cfg(test)]
mod region_tests;
#[cfg(test)]
mod record_tests;

use expression::{evaluate_address, evaluate_expr};
use record::Record;
use region::Region;
use value::Value;

//pub use discovery::Data as DscData;
//pub use parser::ProcData;
pub use record::RecordMap;
pub use region::RegionMap;
pub use types::Type;
pub use value::ValueMap;

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
	//procedures: discovery::ProcMap,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
	pub name: IdentId,
	pub typ: Type,
	pub size: u32,
	pub offset: u16,
}

pub fn eval(input: &InputData, lex_data: &LexData, should_print: bool,
) -> Result<Data, crate::error::Error> {
	let tasks = scan_tasks(lex_data)
		.map_err(|e| e.into_comp_error(input, lex_data, crate::error::Kind::Parser))?;
	if should_print {
		println!("{tasks:?}");
	}

	let data = process_tasks(lex_data, tasks)
		.map_err(|e| e.into_comp_error(input, lex_data, crate::error::Kind::Parser))?;
	if should_print {
		println!("{data:?}");
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
		start_address: Option<TokenId>,
		start_fields: TokenId,
	},
}

fn scan_tasks(lex_data: &LexData,
) -> Result<Vec<Task>, error::Error> {
	let mut tasks = vec![];

	let mut cursor = cursor::Cursor::new(lex_data);
	while cursor.current() != TokenKind::Eof {
		let token = cursor.current();
		match token {
			TokenKind::Value => {
				cursor.advance();
				tasks.push(scan_value_task(&mut cursor)?);
			}
			TokenKind::Region => {
				cursor.advance();
				tasks.push(scan_region_task(&mut cursor)?);
			}
			TokenKind::Record => {
				cursor.advance();
				tasks.push(scan_record_task(&mut cursor)?);
			}
			_ => {
				eprintln!("WARNING - unexpected token {token:?}");
				cursor.advance();
			}
		}
	}

	Ok(tasks)
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

fn scan_record_task(cursor: &mut cursor::Cursor,
) -> Result<Task, error::Error> {
	let location = cursor.index();
	let ident = cursor.expect_identifier("record name")?;
	let start_address = if cursor.expect(TokenKind::At).is_ok() {
		Some(skip_until(cursor, TokenKind::OBrace)?)
	} else {
		None
	};
	cursor.expect(TokenKind::OBrace)?;
	let start_fields = skip_until(cursor, TokenKind::CBrace)?;
	cursor.expect(TokenKind::CBrace)?;
	Ok(Task::Record { ident, location, start_address, start_fields })
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
	tasks: Vec<Task>,
) -> Result<Data, error::Error> {
	let mut data = Data::default();
	let mut locations = HashMap::default();

	let mut queue: VecDeque<Task> = tasks.into_iter().collect();
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
						data.regions.insert(ident, Region {
							span: crate::Span { start, end },
							alloc_position: 0,
						});
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

			Task::Record { ident, location, start_address, start_fields } => {
				locations.insert(ident, location);

				match process_record(lex_data, &data, start_address, start_fields) {
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
	start_address: Option<TokenId>,
	start_fields: TokenId,
) -> Result<(Option<MemoryPlacement>, Vec<(IdentId, Type)>), error::Error> {
	let placement = if let Some(start_address) = start_address {
		let mut cursor = cursor::Cursor::from_start(lex_data, start_address);
		let placement = evaluate_address(&mut cursor, &data.values, &data.regions, TokenKind::OBrace)?;
		Some(placement)
	} else {
		None
	};

	let mut cursor_fields = cursor::Cursor::from_start(lex_data, start_fields);
	process_fields(&mut cursor_fields, &data.records, TokenKind::CBrace)
		.map(|fields| (placement, fields))
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
