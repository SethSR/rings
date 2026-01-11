
use std::collections::{HashSet, VecDeque};

use crate::parser::cursor::Cursor;
use crate::identifier::Id as IdentId;
use crate::lexer::Data as LexData;
use crate::token::{Id as TokenId, Kind as TokenKind};
use crate::Span;

use super::error::Error;
use super::expression::evaluate_expr;
use super::region::{Region, RegionMap};
use super::value::{Value, ValueMap};
use super::skip_through;

struct Task { ident: IdentId, start: TokenId }

pub fn eval(lex_data: &LexData, values: &ValueMap,
) -> Result<RegionMap, Error> {
	let tasks = scan(lex_data)?;
	process(lex_data, values, tasks)
}

fn scan(lex_data: &LexData,
) -> Result<Vec<Task>, Error> {
	let mut tasks = vec![];
	let mut cursor = Cursor::default();

	while cursor.current(lex_data) != TokenKind::Eof {
		if cursor.expect(lex_data, TokenKind::Region).is_err() {
			// Skip non-region constructs
			cursor.advance();
			continue;
		}

		let ident = cursor.expect_identifier(&lex_data, "region name")?;

		let start = cursor.index();
		skip_through(&mut cursor, lex_data, TokenKind::Semicolon)?;

		tasks.push(Task { ident, start })
	}

	Ok(tasks)
}

fn process(lex_data: &LexData,
	values: &ValueMap,
	tasks: Vec<Task>,
) -> Result<RegionMap, Error> {
	let mut regions = RegionMap::default();
	let mut queue: VecDeque<Task> = tasks.into_iter().collect();
	let mut failed_tasks: HashSet<IdentId> = HashSet::new();
	let mut consecutive_failures = 0;

	while let Some(task) = queue.pop_front() {
		match evaluate_region(lex_data, values, task.start) {
			Ok(region) => {
				regions.insert(task.ident, region);
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
						});
					}
				} else {
					failed_tasks.insert(task.ident);
				}

				queue.push_back(task);
			}
			Err(e) => return Err(e),
		}
	}

	Ok(regions)
}

fn evaluate_region(lex_data: &LexData,
	values: &ValueMap,
	start_token: TokenId,
) -> Result<Region, Error> {
	let mut cursor = Cursor::new(start_token);

	cursor.expect(lex_data, TokenKind::OBracket)?;
	let Value::Integer(size) = evaluate_expr(&mut cursor, lex_data, values, TokenKind::CBracket)? else {
		panic!()
	};

	cursor.expect(lex_data, TokenKind::At)?;
	let Value::Integer(start) = evaluate_expr(&mut cursor, lex_data, values, TokenKind::Semicolon)? else {
		panic!()
	};

	if !(0..u32::MAX as i64).contains(&start) {
		panic!()
	}
	if !(0..u32::MAX as i64).contains(&(start + size)) {
		panic!()
	}
	let span = Span {
		start: start as u32,
		end: (start + size) as u32,
	};
	Ok(Region { span, alloc_position: 0 })
}

#[cfg(test)]
mod tests {
	use crate::error::Kind;
	use crate::identifier::Identifier;

	use super::super::parse_values::eval as value_eval;
	use super::*;

	fn setup(source: &str) -> Result<RegionMap, String> {
		let input = crate::input::eval(file!().into(), source.into());
		let lex_data = crate::lexer::eval(source)
			.map_err(|e| e.display(&input))?;
		let values = value_eval(&lex_data)
			.map_err(|e| e.into_comp_error(&input, &lex_data, Kind::Parser))
			.map_err(|e| e.display(&input))?;
		eval(&lex_data, &values)
			.map_err(|e| e.into_comp_error(&input, &lex_data, Kind::Parser))
			.map_err(|e| e.display(&input))
	}

	#[test]
	fn region() {
		let regions = setup("region a[1024] @ 0x0020_0000;")
			.unwrap_or_else(|e| panic!("{e}"));
		assert_eq!(regions.len(), 1);
		assert_eq!(regions[&"a".id()], Region {
			span: (0x20_0000..0x20_0400).into(),
			alloc_position: 0,
		});
	}
}
