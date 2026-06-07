
use std::collections::VecDeque;

use crate::identifier::{IdentId, Identifier};
use crate::lexer::Data as LexData;
use crate::token::{Id as TokenId, Kind as TokenKind};
use crate::Target;

use super::cursor::Cursor;
use super::error::Error;
use super::task::{RegionParseType, Task};

pub fn scan_tasks(lex_data: &LexData,
) -> Result<VecDeque<Task>, Error> {
	let mut tasks = vec![];

	let mut cursor = Cursor::new(lex_data);
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

fn scan_value_task(cursor: &mut Cursor,
) -> Result<Task, Error> {
	let ident = cursor.expect_identifier("value name")?;
	cursor.expect(TokenKind::Eq)?;
	let start = skip_until(cursor, &[TokenKind::Semicolon])?;
	cursor.expect(TokenKind::Semicolon)?;
	Ok(Task::Value { ident, start })
}

/// Matches REGION syntax:
/// - `region <ident>[<expr>..<expr>];`
/// - `region <ident>[<expr>] @ <expr>;`
fn scan_region_task(cursor: &mut Cursor,
) -> Result<Task, Error> {
	let ident = cursor.expect_identifier("region name")?;
	cursor.expect(TokenKind::OBracket)?;
	let first = skip_until(cursor, &[TokenKind::Dot2, TokenKind::CBracket])?;
	let parse_type = if cursor.expect(TokenKind::Dot2).is_ok() {
		let end = skip_until(cursor, &[TokenKind::CBracket])?;
		cursor.expect(TokenKind::CBracket)?;
		RegionParseType::Range { start: first, end }
	} else {
		cursor.expect(TokenKind::CBracket)?;
		cursor.expect(TokenKind::At)?;
		let address = skip_until(cursor, &[TokenKind::Semicolon])?;
		RegionParseType::Location { size: first, address }
	};
	cursor.expect(TokenKind::Semicolon)?;
	Ok(Task::Region { ident, parse_type })
}

/// Matches RECORD syntax:
/// - `record <ident> @ <expr> {...}`
/// - `record <ident> in <region> {...}`
/// - `record <ident> {...}`
fn scan_record_task(cursor: &mut Cursor,
) -> Result<Task, Error> {
	let location = cursor.index();
	let ident = cursor.expect_identifier("record name")?;

	let start_placement = scan_placement(cursor)?;

	cursor.expect(TokenKind::OBrace)?;
	let start_fields = skip_until(cursor, &[TokenKind::CBrace])?;
	cursor.expect(TokenKind::CBrace)?;

	Ok(Task::Record { ident, location, start_placement, start_fields })
}

/// Matches Table syntax
/// - `table <ident>[<rows>] @ <expr> {...}`
/// - `table <ident>[<rows>] in <region> {...}`
/// - `table <ident>[<rows>] {...}`
fn scan_table_task(cursor: &mut Cursor,
) -> Result<Task, Error> {
	let ident = cursor.expect_identifier("table name")?;

	cursor.expect(TokenKind::OBracket)?;
	let start_rows = skip_until(cursor, &[TokenKind::CBracket])?;
	cursor.expect(TokenKind::CBracket)?;

	let start_placement = scan_placement(cursor)?;

	cursor.expect(TokenKind::OBrace)?;
	let start_fields = skip_until(cursor, &[TokenKind::CBrace])?;
	cursor.expect(TokenKind::CBrace)?;

	Ok(Task::Table { ident, start_rows, start_placement, start_fields })
}

fn scan_placement(cursor: &mut Cursor,
) -> Result<Option<TokenId>, Error> {
	if matches!(cursor.current(), TokenKind::At | TokenKind::In) {
		let start = cursor.index();
		skip_until(cursor, &[TokenKind::OBrace])?;
		Ok(Some(start))
	} else {
		Ok(None)
	}
}

fn skip_brace_block(cursor: &mut Cursor) -> Result<(), Error> {
	skip_until(cursor, &[TokenKind::OBrace])?;
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
fn scan_proc(cursor: &mut Cursor,
	ident: IdentId,
	target: Option<Target>,
) -> Result<Task, Error> {
	let start = cursor.index();
	skip_brace_block(cursor)?;
	Ok(Task::Proc { ident, target, start })
}

/// Matches named procedures:
/// - `proc <ident>(...) <return> {...}`
fn scan_named_proc(cursor: &mut Cursor,
	target: Option<Target>,
) -> Result<Task, Error> {
	let name_id = cursor.expect_identifier("procedure name")?;
	scan_proc(cursor, name_id, target)
}

/// Matches target specific procedures:
/// - `<target> proc <ident>(...) <return> {...}`
/// - `<target> main {...}`
/// - `<target> sub {...}`
fn scan_target_proc(cursor: &mut Cursor,
	target: Option<Target>,
) -> Result<Task, Error> {
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

fn skip_until(cursor: &mut Cursor,
	end_tokens: &[TokenKind],
) -> Result<TokenId, Error> {
	let start = cursor.index();
	loop {
		match cursor.current() {
			TokenKind::Eof => return Err(Error::UnexpectedEof { location: cursor.index() }),
			token if end_tokens.contains(&token) => break Ok(start),
			_ => cursor.advance(),
		}
	}
}

