
use crate::error::{Error, Kind as EKind};
use crate::identifier::Map as IdentMap;
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;

mod discovery;
mod parser;

pub use discovery::{Data as DscData, RecordMap, Value, ValueMap};
pub use parser::ProcData;

#[cfg(test)]
pub use discovery::RegionMap;

pub fn eval(input: &InputData, lex_data: &LexData, should_print: bool) -> Result<(discovery::Data, IdentMap<ProcData>), Error> {
	let (mut dsc_data, task_queue) = discovery::eval(&input, &lex_data)
		.map_err(|e| e.into_comp_error(&input, &lex_data, EKind::Discovery))?;
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
