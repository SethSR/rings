
use std::fmt::{Display, Formatter, Result};

use crate::token;

#[derive(Debug)]
pub struct Data {
	pub source_file: String,
	pub source: Box<str>,
	// stores the position of each newline (\n) character in the source
	pub line_pos: token::PosList,
}

pub fn eval(source_file: String, source: Box<str>) -> Data {
	let line_pos = [0].into_iter()
		.chain(source.char_indices()
			.filter(|(_,c)| *c == '\n')
			.map(|(idx,_)| idx))
		.collect();
	Data { source_file, source, line_pos }
}

impl Display for Data {
	fn fmt(&self, f: &mut Formatter) -> Result {
		writeln!(f, "Source: {}", self.source_file)?;
		writeln!(f, "LoC: {}", self.line_pos.len())?;
		writeln!(f)
	}
}

