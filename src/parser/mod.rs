
use crate::error::Kind as EKind;
use crate::identifier::{Id as IdentId, Map as IdentMap};
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::rings_type::Type;
use crate::token::Kind as TokenKind;

mod cursor;
mod discovery;
mod error;
mod expression;
mod parse_records;
mod parse_regions;
mod parse_values;
mod parser;
mod record;
mod region;
mod task;
mod value;

pub use discovery::Data as DscData;
pub use parser::ProcData;
pub use record::RecordMap;
pub use region::RegionMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
	pub name: IdentId,
	pub typ: Type,
	pub size: u32,
	pub offset: u16,
}

pub fn eval(input: &InputData, lex_data: &LexData, should_print: bool,
) -> Result<(discovery::Data, IdentMap<ProcData>), crate::error::Error> {
	let values = parse_values::eval(lex_data)
		.map_err(|e| e.into_comp_error(&input, &lex_data, EKind::Parser))?;

	let regions = parse_regions::eval(&lex_data, &values)
		.map_err(|e| e.into_comp_error(&input, &lex_data, EKind::Parser))?;

	let records = parse_records::eval(&lex_data, &values, &regions)
		.map_err(|e| e.into_comp_error(&input, &lex_data, EKind::Parser))?;

	let (mut dsc_data, task_queue) = discovery::eval(&lex_data)
		.map_err(|e| e.into_comp_error(&input, &lex_data, EKind::Discovery))?;
	dsc_data.values = values;
	dsc_data.regions = regions;
	dsc_data.records = records;
	if should_print {
		discovery::print(&dsc_data, &task_queue, &input, &lex_data);
	}

	let proc_db = parser::eval(&input, &lex_data, &mut dsc_data, task_queue)
		.map_err(|e| e.into_comp_error(&input, &lex_data, EKind::Parser))?;
	if should_print {
		parser::print(&proc_db, &input, &lex_data);
	}

	Ok((dsc_data, proc_db))
}

fn skip_through(cursor: &mut cursor::Cursor, lex_data: &LexData,
	end_token: TokenKind,
) -> Result<(), error::Error> {
	loop {
		match cursor.current(lex_data) {
			TokenKind::Eof => return Err(error::Error::UnexpectedEof { location: cursor.index() }),
			token if token == end_token => break Ok(()),
			_ => cursor.advance(),
		}
	}
}

fn parse_fields(cursor: &mut cursor::Cursor, lex_data: &LexData,
	records: &RecordMap,
	end_token: TokenKind,
) -> Result<Vec<(IdentId, Type)>, error::Error> {
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

