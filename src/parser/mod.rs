
use std::collections::{HashMap, HashSet, VecDeque};

use crate::identifier::{IdentId, Map as IdentMap, Identifier};
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::token::{Id as TokenId, Kind as TokenKind};
use crate::{SrcPos, Target};
use crate::token_source;

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

use expression::evaluate_expr;
use ast::AstList;
use data::{Procedure, Record, Region, Table, Value};

pub use ast::{Ast, AstId, Kind as AstKind};
pub use data::{ProcMap, RecordMap, RegionMap, TableMap, TypeMap, ValueMap};
pub use types::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemoryPlacement {
	Address(u32),
	Region(IdentId),
}

/// The constructs recognized by the language
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
	Value,
	Region,
	Record,
	Table,
	Procedure,
}
pub type KindMap = IdentMap<Kind>;

#[derive(Default)]
pub struct Data<T> {
	pub kinds: KindMap,
	pub values: ValueMap,
	pub regions: RegionMap,
	pub records: RecordMap,
	pub tables: TableMap,
	pub procedures: ProcMap<T>,
	pub types: TypeMap,
}

impl<T: std::fmt::Debug> std::fmt::Debug for Data<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		writeln!(f, "Data {{")?;
		writeln!(f, "kinds:\n{}", self.kinds.iter()
				.map(|a| format!("  {a:?}"))
				.collect::<Vec<_>>()
				.join("\n"))?;
		writeln!(f, "values: \n{}", self.values.iter()
				.map(|a| format!("  {a:?}"))
				.collect::<Vec<_>>()
				.join("\n"))?;
		writeln!(f, "regions: \n{}", self.regions.iter()
				.map(|(id, region)| format!("  {id:?}: 0x{:08X}..0x{:08X}", region.span.start, region.span.end))
				.collect::<Vec<_>>()
				.join("\n"))?;
		writeln!(f, "records: \n{}", self.records.iter()
				.map(|(id, record)| format!("  {id:?}: {:?} {:?}", record.placement, record.fields))
				.collect::<Vec<_>>()
				.join("\n"))?;
		writeln!(f, "tables: \n{}", self.tables.iter()
				.map(|a| format!("  {a:?}"))
				.collect::<Vec<_>>()
				.join("\n"))?;
		writeln!(f, "procedures: \n{}", self.procedures.iter()
				.map(|(id, proc)| format!("  {id:?}: {:?} {:?} -> {:?}", proc.target, proc.params, proc.ret_type))
				.collect::<Vec<_>>()
				.join("\n"))?;
		writeln!(f, "types: \n{}", self.types.iter()
				.map(|a| format!("  {a:?}"))
				.collect::<Vec<_>>()
				.join("\n"))?;
		writeln!(f, "}}")
	}
}

pub fn eval(input: &InputData, lex_data: &LexData, should_print: bool,
) -> Result<Data<SrcPos>, crate::error::Error> {
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

	Ok(Data {
		kinds: data.kinds,
		values: data.values,
		regions: data.regions,
		records: data.records,
		tables: data.tables,
		types: data.types,
		procedures: data.procedures.into_iter()
				.map(|(proc_id, proc_data)| (proc_id, Procedure {
					target: proc_data.target,
					params: proc_data.params,
					ret_type: proc_data.ret_type,
					body: proc_data.body.into_iter()
							.map(|Ast { kind, location }| {
								let tok_start = location.start;
								let tok_end = location.end;
								let src_start = token_source(input, lex_data, tok_start).start;
								let src_end = token_source(input, lex_data, tok_end).end;
								Ast { kind, location: (src_start..src_end).into() }
							})
							.collect(),
				}))
				.collect(),
	})
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

/// Matches REGION syntax:
/// - `region <ident>[<expr>] @ <expr>;`
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

/// Matches RECORD syntax:
/// - `record <ident> @ <expr> {...}`
/// - `record <ident> in <region> {...}`
/// - `record <ident> {...}`
fn scan_record_task(cursor: &mut cursor::Cursor,
) -> Result<Task, error::Error> {
	let location = cursor.index();
	let ident = cursor.expect_identifier("record name")?;

	let start_placement = scan_placement(cursor)?;

	cursor.expect(TokenKind::OBrace)?;
	let start_fields = skip_until(cursor, TokenKind::CBrace)?;
	cursor.expect(TokenKind::CBrace)?;

	Ok(Task::Record { ident, location, start_placement, start_fields })
}

/// Matches Table syntax
/// - `table <ident>[<rows>] @ <expr> {...}`
/// - `table <ident>[<rows>] in <region> {...}`
/// - `table <ident>[<rows>] {...}`
fn scan_table_task(cursor: &mut cursor::Cursor,
) -> Result<Task, error::Error> {
	let ident = cursor.expect_identifier("table name")?;

	cursor.expect(TokenKind::OBracket)?;
	let start_rows = skip_until(cursor, TokenKind::CBracket)?;
	cursor.expect(TokenKind::CBracket)?;

	let start_placement = scan_placement(cursor)?;

	cursor.expect(TokenKind::OBrace)?;
	let start_fields = skip_until(cursor, TokenKind::CBrace)?;
	cursor.expect(TokenKind::CBrace)?;

	Ok(Task::Table { ident, start_rows, start_placement, start_fields })
}

fn scan_placement(cursor: &mut cursor::Cursor,
) -> Result<Option<TokenId>, error::Error> {
	if matches!(cursor.current(), TokenKind::At | TokenKind::In) {
		let start = cursor.index();
		skip_until(cursor, TokenKind::OBrace)?;
		Ok(Some(start))
	} else {
		Ok(None)
	}
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
) -> Result<Data<TokenId>, error::Error> {
	let mut data = Data::default();
	let mut locations = HashMap::default();

	let mut failed_tasks = HashSet::<IdentId>::new();
	let mut consecutive_failures = 0;

	while let Some(task) = queue.pop_front() {
		match task {
			Task::Value { ident, start } => {
				let mut cursor = cursor::Cursor::from_start(lex_data, start);
				match evaluate_expr(&mut cursor, &data, TokenKind::Semicolon) {
					Ok(value) => {
						data.values.insert(ident, value);
						data.kinds.insert(ident, Kind::Value);
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
						data.kinds.insert(ident, Kind::Region);
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
						data.kinds.insert(ident, Kind::Record);
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
					Ok(table) => {
						data.tables.insert(ident, table);
						data.kinds.insert(ident, Kind::Table);
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
						data.kinds.insert(ident, Kind::Procedure);
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
	data: &Data<TokenId>,
	start_size: TokenId, start_address: TokenId,
) -> Result<(u32,u32), error::Error> {
	let mut cursor_size = cursor::Cursor::from_start(lex_data, start_size);
	let result_size = evaluate_expr(&mut cursor_size, &data, TokenKind::CBracket);
	let mut cursor_address = cursor::Cursor::from_start(lex_data, start_address);
	let result_address = evaluate_expr(&mut cursor_address, &data, TokenKind::Semicolon);

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
	data: &Data<TokenId>,
	start_placement: Option<TokenId>,
	start_fields: TokenId,
) -> Result<(Option<MemoryPlacement>, Vec<(IdentId, Type)>), error::Error> {
	let placement = start_placement
			.map(|start| process_placement(lex_data, data, start, TokenKind::OBrace))
			.transpose()?;

	let mut cursor_fields = cursor::Cursor::from_start(lex_data, start_fields);
	process_fields(&mut cursor_fields, data, TokenKind::CBrace)
			.map(|fields| (placement, fields))
}

fn process_table(
	lex_data: &LexData,
	data: &Data<TokenId>,
	start_placement: Option<TokenId>,
	start_rows: TokenId,
	start_fields: TokenId,
) -> Result<Table, error::Error> {
	let mut cursor_rows = cursor::Cursor::from_start(lex_data, start_rows);
	let row_count = evaluate_expr(&mut cursor_rows, &data, TokenKind::CBracket)
			.map_err(|_| cursor_rows.expected_token("capacity expression"))?;
	let Value::Integer(row_count) = row_count else {
		panic!("decimal values not allowed in table row count declarations")
	};
	if !(0..u16::MAX as i64).contains(&row_count) {
		panic!("table row count ({row_count}) out of range")
	}

	let placement = start_placement
			.map(|start| process_placement(lex_data, data, start, TokenKind::OBrace))
			.transpose()?;

	let mut cursor_fields = cursor::Cursor::from_start(lex_data, start_fields);
	let fields = process_fields(&mut cursor_fields, data, TokenKind::CBrace)?;

	Ok(Table { placement, row_count: row_count as u16, fields })
}

fn process_proc(
	lex_data: &LexData,
	data: &mut Data<TokenId>,
	proc_id: IdentId,
	target: Option<Target>,
	start: TokenId,
) -> Result<Procedure<TokenId>, error::Error> {
	let mut proc = Procedure {
		target,
		params: vec![],
		body: AstList::default(),
		ret_type: Type::Void,
	};

	let cursor = &mut cursor::Cursor::from_start(lex_data, start);

	proc.params = if cursor.expect(TokenKind::OParen).is_ok() {
		let params = process_fields(cursor, data, TokenKind::CParen)?;
		cursor.expect(TokenKind::CParen)?;
		for (param_id, param_type) in &params {
			data.types.insert((proc_id, 0, *param_id), *param_type);
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
	let mut block = parse_procedures::parse_block(
		cursor, &mut proc.body, data, proc_id, 0,
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

/// Matches AT syntax:
/// - `at <expr>`
///
/// Expects an address expression.
fn process_at(cursor: &mut cursor::Cursor,
	data: &Data<TokenId>,
	end_token: TokenKind,
) -> Result<MemoryPlacement, error::Error> {
	cursor.expect(TokenKind::At)?;
	evaluate_expr(cursor, &data, end_token)
			.and_then(|value| {
				match value {
					Value::Integer(address) => {
						if !(0..u32::MAX as i64).contains(&address) {
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
fn process_in(cursor: &mut cursor::Cursor,
	data: &Data<TokenId>,
	end_token: TokenKind,
) -> Result<MemoryPlacement, error::Error> {
	cursor.expect(TokenKind::In)?;
	let ident = cursor.expect_identifier("region name")?;
	if !data.regions.contains_key(&ident) {
		return Err(cursor.expected_token("region name"));
	}

	if cursor.current() != end_token {
		return Err(cursor.expected_token(format!("'{end_token:?}' after region name")));
	}

	Ok(MemoryPlacement::Region(ident))
}

fn process_placement(
	lex_data: &LexData,
	data: &Data<TokenId>,
	start_placement: TokenId,
	end_token: TokenKind,
) -> Result<MemoryPlacement, error::Error> {
	let mut cursor = cursor::Cursor::from_start(lex_data, start_placement);
	match cursor.current() {
		TokenKind::In => process_in(&mut cursor, &data, end_token),
		TokenKind::At => process_at(&mut cursor, &data, end_token)
		.map_err(|e| match e {
			error::Error::ExpectedToken { found, ..} => {
				error::Error::ExpectedToken {
					expected: "address expression".to_string(),
					found,
				}
			}
			e => e,
		}),
		_ => Err(cursor.expected_token("placement specifier")),
	}
}

fn process_fields(cursor: &mut cursor::Cursor,
	data: &Data<TokenId>,
	end_token: TokenKind,
) -> Result<Vec<(IdentId, Type)>, error::Error> {
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
