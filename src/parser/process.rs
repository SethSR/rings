
use std::collections::VecDeque;

use crate::identifier::Map as IdentMap;
use crate::lexer::Data as LexData;

use super::ast::{Ast, AstId, AstList, Kind as AstKind};
use super::cursor::Cursor;
use super::data::{Procedure, Record, RecordMap, Region, Table};
use super::error::Error;
use super::parse_procedures::parse_block;
use super::task::{RegionParseType, Task};
use super::{Data, IdentId, Kind, Target, TokenId, TokenKind, Type, Value};
use super::evaluate_expr;

type IdentSet = std::collections::HashSet<IdentId>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryPlacement {
	Address(u32),
	Region(IdentId),
}

fn error_duplication(name_id: IdentId, locations: &IdentMap<TokenId>) -> Error {
	let location = locations[&name_id];
	Error::DuplicateDeclaration { name_id, location }
}

fn error_recursion(name_id: IdentId, locations: &IdentMap<TokenId>) -> Error {
	let location = locations[&name_id];
	Error::RecursiveType { name_id, location }
}

pub fn process_tasks(
	lex_data: &LexData,
	locations: &IdentMap<TokenId>,
	mut queue: VecDeque<Task>,
) -> Result<Data<TokenId>, Error> {
	let mut data = Data::default();

	let mut failed_tasks = IdentSet::default();
	let mut consecutive_failures = 0;

	while let Some(task) = queue.pop_front() {
		match task {
			Task::Value { ident, start } => {
				let mut cursor = Cursor::from_start(lex_data, start);
				match evaluate_expr(&mut cursor, &data, TokenKind::Semicolon) {
					Ok(value) => {
						if data.values.insert(ident, value).is_some() {
							return Err(error_duplication(ident, &locations));
						}
						data.kinds.insert(ident, Kind::Value);
						failed_tasks.remove(&ident);
						consecutive_failures = 0;
					}
					Err(Error::UndefinedType { location, ident_id }) => {
						consecutive_failures = check_task_failure(
							&queue, &mut failed_tasks, consecutive_failures,
							location, ident, ident_id,
						)?;
						queue.push_back(task);
					}
					Err(e) => return Err(e),
				}
			}

			Task::Region { ident, parse_type } => {
				match process_region(lex_data, &data, parse_type) {
					Ok((start, end)) => {
						if data.regions.insert(ident, Region::new(start, end)).is_some() {
							return Err(error_duplication(ident, locations));
						}
						data.kinds.insert(ident, Kind::Region);
						failed_tasks.remove(&ident);
						consecutive_failures = 0;
					}
					Err(Error::UndefinedType { location, ident_id }) => {
						consecutive_failures = check_task_failure(
							&queue, &mut failed_tasks, consecutive_failures,
							location, ident, ident_id,
						)?;
						queue.push_back(task);
					}
					Err(e) => return Err(e),
				}
			}

			Task::Record { ident, start_placement, start_fields } => {
				match process_record(lex_data, &data, start_placement, start_fields) {
					Ok((placement, fields)) => {
						if data.records.insert(ident, Record { placement, fields }).is_some() {
							return Err(error_duplication(ident, locations));
						}
						data.kinds.insert(ident, Kind::Record);
						failed_tasks.remove(&ident);
						consecutive_failures = 0;
					}
					Err(Error::UndefinedType { location, ident_id }) => {
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
					Ok(table) => {
						if data.tables.insert(ident, table).is_some() {
							return Err(error_duplication(ident, locations));
						}
						data.kinds.insert(ident, Kind::Table);
						failed_tasks.remove(&ident);
						consecutive_failures = 0;
					}
					Err(Error::UndefinedType { location, ident_id }) => {
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
						if data.procedures.insert(ident, proc).is_some() {
							return Err(error_duplication(ident, locations));
						}
						data.kinds.insert(ident, Kind::Procedure);
						failed_tasks.remove(&ident);
						consecutive_failures = 0;
					}
					Err(Error::UndefinedType { location, ident_id }) => {
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

	for (name,_) in &data.records {
		check_recursion(name, name, &data.records, locations, &mut vec![])?;
	}

	Ok(data)
}

fn check_task_failure(
	queue: &VecDeque<Task>,
	failed_tasks: &mut IdentSet,
	consecutive_failures: usize,
	location: TokenId,
	task_ident: IdentId,
	item_id: IdentId,
) -> Result<usize, Error> {
	if failed_tasks.contains(&task_ident) {
		let failure_count = consecutive_failures + 1;

		if failure_count > queue.len() {
			Err(Error::CircularDependency {
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
	locations: &IdentMap<TokenId>,
	visited: &mut Vec<IdentId>,
) -> Result<(), Error> {
	if visited.contains(curr) {
		return Ok(());
	}

	visited.push(*curr);

	if let Some(record) = records.get(curr) {
		for field in &record.fields {
			if let Type::Record(name) = &field.1 {
				if name == root {
					return Err(error_recursion(*root, locations));
				}
				check_recursion(root, name, records, locations, visited)?;
			}
		}
	}

	Ok(())
}

fn process_region(
	lex_data: &LexData,
	data: &Data<TokenId>,
	parse_type: RegionParseType,
) -> Result<(u32,u32), Error> {
	match parse_type {
		RegionParseType::Range { start, end } => {
			let mut cursor = Cursor::from_start(lex_data, start);
			let result_start = evaluate_expr(&mut cursor, data, TokenKind::Dot2);

			let mut cursor = Cursor::from_start(lex_data, end);
			let result_end = evaluate_expr(&mut cursor, data, TokenKind::CBracket);

			match (result_start, result_end) {
				(Ok(Value::Integer(region_start)), Ok(Value::Integer(region_end))) => {
					if !(0..=u32::MAX as i64).contains(&region_start) {
						panic!("region start ({region_start}) out of range")
					}
					if !(0..=u32::MAX as i64).contains(&region_end) {
						panic!("region end ({region_end}) out of range")
					}
					Ok((region_start as u32, region_end as u32))
				}
				(Ok(Value::Decimal(_)),_) | (_,Ok(Value::Decimal(_))) => {
					panic!("decimal values not allowed in region declarations")
				}
				(Err(e),_) | (_,Err(e)) => Err(e),
			}
		}

		RegionParseType::Location { size, address } => {
			let mut cursor = Cursor::from_start(lex_data, size);
			let result_size = evaluate_expr(&mut cursor, data, TokenKind::CBracket);

			let mut cursor = Cursor::from_start(lex_data, address);
			let result_addr = evaluate_expr(&mut cursor, data, TokenKind::Semicolon);

			match (result_size, result_addr) {
				(Ok(Value::Integer(region_size)), Ok(Value::Integer(region_addr))) => {
					if !(0..=u32::MAX as i64).contains(&region_addr) {
						panic!("region address ({region_addr}) out of range")
					}
					if !(0..=u32::MAX as i64).contains(&(region_addr + region_size)) {
						panic!("region end ({}) out of range", region_addr + region_size)
					}
					Ok((region_addr as u32, (region_addr + region_size) as u32))
				}
				(Ok(Value::Decimal(_)),_) | (_,Ok(Value::Decimal(_))) => {
					panic!("decimal values not allowed in region declarations")
				}
				(Err(e),_) | (_,Err(e)) => Err(e),
			}
		}
	}
}

fn process_record(
	lex_data: &LexData,
	data: &Data<TokenId>,
	start_placement: Option<TokenId>,
	start_fields: TokenId,
) -> Result<(Option<MemoryPlacement>, Vec<(IdentId, Type)>), Error> {
	let placement = start_placement
			.map(|start| process_placement(lex_data, data, start, TokenKind::OBrace))
			.transpose()?;

	let mut cursor_fields = Cursor::from_start(lex_data, start_fields);
	process_fields(&mut cursor_fields, data, TokenKind::CBrace)
			.map(|fields| (placement, fields))
}

fn process_table(
	lex_data: &LexData,
	data: &Data<TokenId>,
	start_placement: Option<TokenId>,
	start_rows: TokenId,
	start_fields: TokenId,
) -> Result<Table, Error> {
	let mut cursor_rows = Cursor::from_start(lex_data, start_rows);
	let row_count = evaluate_expr(&mut cursor_rows, data, TokenKind::CBracket)
			.map_err(|_| cursor_rows.expected_token("capacity expression"))?;
	let Value::Integer(row_count) = row_count else {
		panic!("decimal values not allowed in table row count declarations")
	};
	if !(0..=u16::MAX as i64).contains(&row_count) {
		panic!("table row count ({row_count}) out of range")
	}

	let placement = start_placement
			.map(|start| process_placement(lex_data, data, start, TokenKind::OBrace))
			.transpose()?;

	let mut cursor_fields = Cursor::from_start(lex_data, start_fields);
	let fields = process_fields(&mut cursor_fields, data, TokenKind::CBrace)?;

	Ok(Table { placement, row_count: row_count as u16, fields })
}

fn process_proc(
	lex_data: &LexData,
	data: &mut Data<TokenId>,
	proc_id: IdentId,
	target: Option<Target>,
	start: TokenId,
) -> Result<Procedure<TokenId>, Error> {
	const PROC_ROOT_DEPTH: u16 = 0;

	let mut proc = Procedure {
		target,
		params: vec![],
		body: AstList::default(),
		ret_type: Type::Void,
	};

	let cursor = &mut Cursor::from_start(lex_data, start);

	proc.params = if cursor.expect(TokenKind::OParen).is_ok() {
		let params = process_fields(cursor, data, TokenKind::CParen)?;
		cursor.expect(TokenKind::CParen)?;
		for (param_id, param_type) in &params {
			data.types.insert(proc_id, PROC_ROOT_DEPTH, *param_id, *param_type);
		}
		params
	} else {
		vec![]
	};

	proc.ret_type = if cursor.expect(TokenKind::Arrow).is_ok() {
		cursor.expect_type(data)?
	} else {
		Type::Void
	};

	let start = AstId::new(proc.body.len());
	let tok_start = cursor.index();
	let mut block = parse_block(
		cursor, &mut proc.body, data, proc_id, PROC_ROOT_DEPTH + 1,
	)?;
	let end = AstId::new(proc.body.len());
	let tok_end = cursor.index();

	let has_return = proc.body[start..end]
		.iter()
		.any(|ast| matches!(ast.kind, AstKind::Return(_)));

	if !has_return {
		let tok_loc = cursor.index();
		let ast_id = proc.body.push(Ast::return_(None, (tok_loc..tok_loc).into()));
		block.push(ast_id);
	}

	proc.body.push(Ast::block(block, (tok_start..tok_end).into()));

	Ok(proc)
}

fn process_placement(
	lex_data: &LexData,
	data: &Data<TokenId>,
	start_placement: TokenId,
	end_token: TokenKind,
) -> Result<MemoryPlacement, Error> {
	let mut cursor = Cursor::from_start(lex_data, start_placement);
	match cursor.current() {
		TokenKind::In => process_in(&mut cursor, data, end_token),
		TokenKind::At => process_at(&mut cursor, data, end_token)
		.map_err(|e| match e {
			Error::ExpectedToken { found, ..} => {
				Error::ExpectedToken {
					expected: "address expression".to_string(),
					found,
				}
			}
			e => e,
		}),
		_ => Err(cursor.expected_token("placement specifier")),
	}
}

/// Matches AT syntax:
/// - `at <expr>`
///
/// Expects an address expression.
fn process_at(cursor: &mut Cursor,
	data: &Data<TokenId>,
	end_token: TokenKind,
) -> Result<MemoryPlacement, Error> {
	cursor.expect(TokenKind::At)?;
	evaluate_expr(cursor, data, end_token)
			.and_then(|value| {
				match value {
					Value::Integer(address) => {
						if !(0..=u32::MAX as i64).contains(&address) {
							panic!("address ({address}) out of range")
						}

						Ok(MemoryPlacement::Address(address as u32))
					}
					Value::Decimal(_) => {
						panic!("decimal values cannot be used in address specifiers")
					}
				}
			})
}

/// Matches IN syntax:
/// - `in <ident>`
///
/// Expects a Region name as the `<ident>`.
fn process_in(cursor: &mut Cursor,
	data: &Data<TokenId>,
	end_token: TokenKind,
) -> Result<MemoryPlacement, Error> {
	cursor.expect(TokenKind::In)?;
	let location = cursor.index();
	let ident = cursor.expect_identifier("region name")?;
	if !data.regions.contains_key(&ident) {
		return Err(Error::UndefinedType {
			location,
			ident_id: ident,
		});
	}

	if cursor.current() != end_token {
		return Err(cursor.expected_token(format!("'{end_token:?}' after region name")));
	}

	Ok(MemoryPlacement::Region(ident))
}

fn process_fields(cursor: &mut Cursor,
	data: &Data<TokenId>,
	end_token: TokenKind,
) -> Result<Vec<(IdentId, Type)>, Error> {
	let mut fields = vec![];

	while cursor.current() != end_token {
		let ident = cursor.expect_identifier("field name")?;
		cursor.expect(TokenKind::Colon)?;
		let typ = cursor.expect_type(data)?;
		fields.push((ident, typ));
		if cursor.expect(TokenKind::Comma).is_err() {
			break;
		}
	}

	Ok(fields)
}

