
use std::collections::{HashMap, HashSet, VecDeque};

use crate::parser::cursor::Cursor;
use crate::identifier::{Id as IdentId, Map as IdentMap};
use crate::lexer::Data as LexData;
use crate::parser::RegionMap;
use crate::rings_type::Type;
use crate::token::{Id as TokenId, Kind as TokenKind};

use super::error::Error;
use super::expression::evaluate_expr;
use super::record::{Record, RecordMap};
use super::value::{Value, ValueMap};
use super::skip_through;

struct Task { ident: IdentId, start: TokenId }

pub fn eval(lex_data: &LexData,
	values: &ValueMap,
	regions: &RegionMap,
) -> Result<IdentMap<Record>, Error> {
	let tasks = scan(lex_data)?;
	process(lex_data, values, regions, tasks)
}

fn scan(lex_data: &LexData,
) -> Result<Vec<Task>, Error> {
	let mut tasks = vec![];
	let mut cursor = Cursor::default();

	while cursor.current(lex_data) != TokenKind::Eof {
		if cursor.expect(lex_data, TokenKind::Record).is_err() {
			// Skip non-record constructs
			cursor.advance();
			continue;
		}
		let ident = cursor.expect_identifier(lex_data, "record name")?;
		let start = cursor.index();
		if cursor.expect(lex_data, TokenKind::At).is_ok() {
			skip_through(&mut cursor, lex_data, TokenKind::OBrace)?;
		}
		cursor.expect(lex_data, TokenKind::OBrace)?;

		skip_through(&mut cursor, lex_data, TokenKind::CBrace)?;
		cursor.expect(lex_data, TokenKind::CBrace)?;

		tasks.push(Task { ident, start });
	}

	Ok(tasks)
}

fn process(lex_data: &LexData,
	values: &ValueMap,
	regions: &RegionMap,
	tasks: Vec<Task>,
) -> Result<RecordMap, Error> {
	let mut records = RecordMap::with_capacity(tasks.len());
	let mut locations = HashMap::with_capacity(tasks.len());
	let mut queue: VecDeque<Task> = tasks.into_iter().collect();
	let mut failed_tasks: HashSet<IdentId> = HashSet::with_capacity(queue.len());
	let mut consecutive_failures = 0;

	while let Some(task) = queue.pop_front() {
		match parse_record(lex_data, task.start, &values, &regions, &records) {
			Ok((record, location)) => {
				records.insert(task.ident, record);
				locations.insert(task.ident, location);
				failed_tasks.remove(&task.ident);
				consecutive_failures = 0;
			}
			Err(Error::UndefinedType { location, ident_id }) => {
				if failed_tasks.contains(&task.ident) {
					consecutive_failures += 1;

					if consecutive_failures > queue.len() {
						return Err(Error::CircularDependency {
							location,
							name_id: task.ident,
							ident_id,
						})
					}
				} else {
					failed_tasks.insert(task.ident);
				}

				queue.push_back(task);
			}
			Err(e) => return Err(e),
		}
	}

	let mut duplicates = HashSet::with_capacity(records.len());
	for (name,_) in &records {
		if duplicates.contains(name) {
			return Err(Error::DuplicateDeclaration {
				location: locations[name],
				name_id: *name,
			});
		}
		duplicates.insert(name);
	}

	for (name,_) in &records {
		check_recursion(name, name, &records, &locations, &mut vec![])?;
	}

	Ok(records)
}

fn parse_record(lex_data: &LexData,
	start: TokenId,
	values: &ValueMap,
	regions: &RegionMap,
	records: &RecordMap,
) -> Result<(Record, TokenId), Error> {
	let mut cursor = Cursor::new(start);
	let region = parse_address(&mut cursor, lex_data, values, regions)?;
	cursor.expect(lex_data, TokenKind::OBrace)?;
	let fields = super::parse_fields(&mut cursor, lex_data, records, TokenKind::CBrace)?;
	cursor.expect(lex_data, TokenKind::CBrace)?;
	Ok((Record::new(records, fields, region), start))
}

/// Check after an '@' for a defined region or an address location
fn parse_address(cursor: &mut Cursor, lex_data: &LexData,
	values: &ValueMap,
	regions: &RegionMap,
) -> Result<Option<IdentId>, Error> {
	if cursor.expect(lex_data, TokenKind::At).is_ok() {
		let location = cursor.index();
		if let Ok(id) = cursor.expect_identifier(lex_data, "region name") {
			if regions.contains_key(&id) {
				return Ok(Some(id));
			}
		} else {
			let Value::Integer(size) = evaluate_expr(cursor, lex_data, values, TokenKind::CBrace)? else {
				panic!()
			};
			if !(0..u32::MAX as i64).contains(&size) {
				panic!()
			}
		}

		Err(Error::ExpectedToken {
			expected: "address or region specifier".into(),
			found: location,
		})
	} else {
		Ok(None)
	}
}

fn check_recursion(
	root: &IdentId,
	curr: &IdentId,
	records: &RecordMap,
	locations: &HashMap<IdentId, TokenId>,
	visited: &mut Vec<IdentId>,
) -> Result<(), Error> {
	if visited.contains(curr) {
		return Ok(());
	}

	visited.push(*curr);

	if let Some(record) = records.get(curr) {
		for field in &record.fields {
			if let Type::Record(name) = &field.typ {
				if name == root {
					return Err(Error::RecursiveType {
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

#[cfg(test)]
mod tests {
	use crate::error::Kind;
	use crate::identifier::Identifier;
	use crate::parser::Param;
	use crate::parser::parse_values::eval as value_eval;
	use crate::parser::parse_regions::eval as region_eval;

	use super::*;

	fn setup(source: &str) -> Result<RecordMap, String> {
		let input = crate::input::eval(file!().into(), source.into());
		let lex_data = crate::lexer::eval(source)
			.map_err(|e| e.display(&input))?;
		let values = value_eval(&lex_data)
			.map_err(|e| e.into_comp_error(&input, &lex_data, Kind::Parser))
			.map_err(|e| e.display(&input))?;
		let regions = region_eval(&lex_data, &values)
			.map_err(|e| e.into_comp_error(&input, &lex_data, Kind::Parser))
			.map_err(|e| e.display(&input))?;
		eval(&lex_data, &values, &regions)
			.map_err(|e| e.into_comp_error(&input, &lex_data, Kind::Parser))
			.map_err(|e| e.display(&input))
	}

	#[test]
	fn empty_record() {
		let records = setup("record a {}")
			.unwrap_or_else(|e| panic!("{e}"));
		assert_eq!(records.len(), 1);
		let record = &records[&"a".id()];
		assert_eq!(record.size(), 0);
		assert_eq!(record.region, None);
		assert_eq!(record.fields.len(), 0);
	}

	#[test]
	fn simple_records() {
		let records = setup("
			record Person { name: s8, age: s8 }
		").unwrap_or_else(|e| panic!("{e}"));
		assert_eq!(records.len(), 1);
		let record = &records[&"Person".id()];
		assert_eq!(record.size(), 2);
		assert_eq!(record.region, None);
		assert_eq!(record.fields, [
			Param { name: "name".id(), typ: Type::s8_top(), size: 1, offset: 0 },
			Param { name: "age".id(), typ: Type::s8_top(), size: 1, offset: 1 },
		]);
	}

	#[test]
	fn nested_records() {
		let records = setup("
			record Address { street: s8 }
			record Person { addr: Address }
		").unwrap_or_else(|e| panic!("{e}"));
		assert_eq!(records.len(), 2);
		assert!(records.contains_key(&"Address".id()));
		assert!(records.contains_key(&"Person".id()));
	}

	#[test]
	fn recursive_detection() {
		let result = setup("
			record Node { next: Node }
		");
		assert!(result.is_err());
		let err = result.unwrap_err();
		assert!(format!("{err}").contains("circular dependency"), "{err}");
	}

	#[test]
	fn indirect_recursion() {
		let result = setup("
			record A { b: B }
			record B { a: A }
		");
		assert!(result.is_err());
		let err = result.unwrap_err();
		assert!(format!("{err}").contains("circular dependency"), "{err}");
	}

	#[test]
	fn record_with_one_field_no_trailing_comma() {
		let records = setup("record a { b: s8 }")
			.unwrap_or_else(|e| panic!("{e}"));
		assert_eq!(records.len(), 1);
		let record = &records[&"a".id()];
		assert_eq!(record.size(), 1);
		assert_eq!(record.region, None);
		assert_eq!(record.fields, [
			Param { name: "b".id(), typ: Type::s8_top(), size: 1, offset: 0 },
		]);
	}

	#[test]
	fn record_with_one_field_and_trailing_comma() {
		let records = setup("record a { b: s8, }")
			.unwrap_or_else(|e| panic!("{e}"));
		assert_eq!(records.len(), 1);
		let record = &records[&"a".id()];
		assert_eq!(record.size(), 1);
		assert_eq!(record.region, None);
		assert_eq!(record.fields, [
			Param { name: "b".id(), typ: Type::s8_top(), size: 1, offset: 0 },
		]);
	}

	#[test]
	fn record_with_multiple_fields() {
		let records = setup("record a { b: s8, c: s8 }")
			.unwrap_or_else(|e| panic!("{e}"));
		assert_eq!(records.len(), 1);
		let record = &records[&"a".id()];
		assert_eq!(record.size(), 2);
		assert_eq!(record.region, None);
		assert_eq!(record.fields, [
			Param { name: "b".id(), typ: Type::s8_top(), size: 1, offset: 0 },
			Param { name: "c".id(), typ: Type::s8_top(), size: 1, offset: 1 },
		]);
	}

	#[test]
	fn record_with_user_defined_field() {
		let records = setup("
			record a {}
			record b { c: a }
		").unwrap_or_else(|e| panic!("{e}"));
		assert_eq!(records.len(), 2);
		let record = &records[&"b".id()];
		assert_eq!(record.size(), 0);
		assert_eq!(record.region, None);
		assert_eq!(record.fields, [
			Param { name: "c".id(), typ: Type::Record("a".id()), size: 0, offset: 0 },
		]);
	}

	#[test]
	fn record_with_region() {
		let records = setup("
			region b[8] @ 32;
			record a @ b {}
		").unwrap_or_else(|e| panic!("{e}"));
		assert_eq!(records.len(), 1);
		let record = &records[&"a".id()];
		assert_eq!(record.size(), 0);
		assert_eq!(record.region, Some("b".id()));
		assert!(record.fields.is_empty());
	}
}
