
use std::{env, fmt, fs};
use std::collections::VecDeque;
use std::ops::Range;

mod ast;
mod cursor;
mod discovery;
mod error;
mod identifier;
mod lexer;
mod lowering;
mod parser;
mod token;
mod type_checker;

use error::CompilerError;

fn main() {
	let mut args = env::args();
	args.next();

	let file_path = args.next()
		.expect("expected source file");
	let source = fs::read_to_string(&file_path)
		.expect("unable to read source file");

	let data = compile(file_path, source.into());
	println!("{data}")
}

type SrcPos = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Type {
	Bool,
	U8, S8,
	U16, S16,
	U32, S32,
	// F16, F32
	Record(identifier::Id),
	Table(identifier::Id),
	Unit,
}

impl Type {
	pub fn display<'a>(&self, data: &'a Data) -> &'a str {
		match self {
			Self::Bool => "bool",
			Self::S8 => "s8",
			Self::U8 => "u8",
			Self::S16 => "s16",
			Self::U16 => "u16",
			Self::S32 => "s32",
			Self::U32 => "u32",
			Self::Record(ident_id) => data.text(*ident_id),
			Self::Table(ident_id) => data.text(*ident_id),
			Self::Unit => "_",
		}
	}
}

pub fn compile(file_path: String, source: Box<str>) -> Data {
	let mut data = Data::new(file_path, source);
	lexer::eval(&mut data);
	discovery::eval(&mut data);
	parser::eval(&mut data);
	type_checker::eval(&mut data);
	lowering::eval(&mut data);
	data
}

// TODO - srenshaw - Need to add Table location calculations.

#[derive(Default)]
pub struct Data {
	source_file: String,
	source: Box<str>,
	// stores the position of each newline (\n) character in the source
	line_pos: token::PosList,
	errors: Vec<CompilerError>,
	/* Lexer */
	tok_list: token::KindList,
	tok_pos: token::PosList,
	identifiers: identifier::Map<Range<SrcPos>>,
	/* Discovery */
	procedures: discovery::ProcMap,
	values: discovery::ValueMap,
	regions: discovery::RegionMap,
	records: discovery::RecordMap,
	tables: discovery::TableMap,
	/* Parsing */
	proc_queue: VecDeque<parser::Task>,
	ast_nodes: ast::KindList,
	ast_pos_src: ast::PosList,
	ast_pos_tok: ast::LocList,
	// procedures ready to be type-checked
	completed_procs: identifier::Map<ast::Id>,
}

fn fmt_size(size: usize) -> String {
	let mut buffer = [size,0,0,0];
	for idx in 0..3 {
		if buffer[idx] > 1023 {
			buffer[idx + 1] = buffer[idx] / 1024;
			buffer[idx] %= 1024;
		}
	}
	if buffer[3] > 0 {
		return format!("\x1b[31m{size}B\x1b[0m");
	}
	let mut out = String::with_capacity(20);
	if buffer[2] > 0 {
		out.push_str(&format!("{} MB", buffer[2]));
	}
	if buffer[1] > 0 {
		out.push_str(&format!(" {} KB", buffer[1]));
	}
	if buffer[0] > 0 {
		out.push_str(&format!(" {} B", buffer[0]));
	}
	out.trim_start().to_string()
}

impl Data {
	pub fn new(source_file: String, source: Box<str>) -> Self {
		Self {
			source_file,
			source,
			..Default::default()
		}
	}

	pub fn text(&self, ident_id: identifier::Id) -> &str {
		&self.source[self.identifiers[&ident_id].clone()]
	}

	fn type_text(&self, ring_type: Type) -> String {
		match ring_type {
			Type::Record(ident_id) => self.text(ident_id).to_string(),
			Type::Table(ident_id) => self.text(ident_id).to_string(),
			_ => format!("{ring_type:?}"),
		}
	}

	fn type_size(&self, ring_type: Type) -> usize {
		match ring_type {
			Type::Bool |
			Type::U8 |
			Type::S8 => 1,
			Type::U16 |
			Type::S16 => 2,
			Type::U32 |
			Type::S32 => 4,
			Type::Record(ident_id) => self.records[&ident_id].size,
			Type::Table(ident_id) => self.tables[&ident_id].size,
			Type::Unit => 0,
		}
	}

	// Get the text of a specific line
	pub fn get_line(&self, line_number: usize) -> String {
		self.get_line_text(line_number).replace('\t', "  ")
	}

	// Convert byte offset to line and column
	pub fn lookup_position(&self, tok_pos: SrcPos) -> (usize, usize) {
		let line = match self.line_pos.binary_search(&tok_pos) {
			Ok(line) => line,
			Err(line) => line - 1,
		};

		let line_pos = self.line_pos.get(line)
			.unwrap_or_else(|| panic!("missing position for line number {line}"));
		assert!(tok_pos > *line_pos, "tok({tok_pos}) line({line_pos})");
		let column = tok_pos - line_pos;
		let line_text = self.get_line_text(line.index() + 1);
		let tab_count = line_text.chars()
			.filter(|ch| *ch == '\t')
			.count();

		(line.index() + 1, column + tab_count + 1)
	}

	fn get_line_text(&self, line_number: usize) -> &str {
		if !(1..=self.line_pos.len()).contains(&line_number) {
			return "";
		}

		let start = self.line_pos[line_number - 1] + 1;
		let end = if line_number < self.line_pos.len() {
			self.line_pos[line_number]
		} else {
			self.source.len()
		};

		&self.source[start..end]
	}
}

impl fmt::Display for Data {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		writeln!(f, "=== Rings Compiler ===")?;
		writeln!(f)?;

		fn fields_to_str(data: &Data, fields: &[(identifier::Id, Type)]) -> String {
			fields.iter()
				.map(|(field_id, field_type)| {
					format!("{} : {}", data.text(*field_id), data.type_text(*field_type))
				})
				.collect::<Vec<_>>()
				.join(" , ")
		}

		write!(f, "Tokens:\n ")?;
		for (token, start) in self.tok_list.iter().zip(self.tok_pos.iter()) {
			match token {
				token::Kind::Identifier(ident_id) => {
					write!(f, " Identifier({})[{start}]", self.text(*ident_id))?;
				}
				_ => write!(f, " {token:?}[{start}]")?,
			}
		}
		writeln!(f)?;
		writeln!(f)?;

		writeln!(f, "{:<16} | HASH-VALUE",
			"IDENTIFIER")?;
		writeln!(f, "{:-<16} | {:-<16}", "", "")?;
		for ident_id in self.identifiers.keys() {
			writeln!(f, "{:<16} | {ident_id}", self.text(*ident_id))?;
		}
		writeln!(f)?;

		writeln!(f, "{:<16} | {:<9} | {:<9}", "REGION", "ADDRESS", "SIZE")?;
		writeln!(f, "{:-<16} | {:-<9} | {:-<9}", "", "", "")?;
		for (ident_id, data) in self.regions.iter() {
			let name = self.text(*ident_id);
			let address = data.address;
			let size = fmt_size(data.byte_count as usize);
			writeln!(f, "{name:<16} | #{address:<08X} | {size:<8}")?;
		}
		writeln!(f)?;

		writeln!(f, "{:<16} | {:<8} | FIELDS",
			"RECORD", "SIZE")?;
		writeln!(f, "{:-<16} | {:-<8} | {:-<16}", "", "", "")?;
		for (ident_id, record) in self.records.iter() {
			let name = self.text(*ident_id);
			let size = record.size;
			let field_str = fields_to_str(self, &record.fields);
			writeln!(f, "{name:<16} | {size:<8} | {field_str}")?;
		}
		writeln!(f)?;

		writeln!(f, "{:<16} | {:<10} | {:<8} | {:<9} | COLUMNS",
			"TABLE", "TOTAL SIZE", "ROW SIZE", "ROW COUNT")?;
		writeln!(f, "{:-<16} | {:-<10} | {:-<8} | {:-<9} | {:-<16}", "", "", "", "", "")?;
		for (ident_id, table) in self.tables.iter() {
			let name = self.text(*ident_id);
			let size = table.size;
			let row_size = size / table.row_count as usize;
			let field_str = fields_to_str(self, &table.column_spec);
			writeln!(f, "{name:<16} | {size:<10} | {row_size:<8} | {:<9} | {field_str}", table.row_count)?;
		}
		writeln!(f)?;

		writeln!(f, "{:<16} | {:<16} | PARAMETERS",
			"PROC-TYPE", "RETURN-TYPE")?;
		writeln!(f, "{:-<16} | {:-<16} | {:-<16}", "", "", "")?;
		for ident_id in self.procedures.keys() {
			let name = self.text(*ident_id);
			let data = &self.procedures[ident_id];
			let ret_type = format!("{:?}", data.ret_type);
			let param_str = fields_to_str(self, &data.params);
			writeln!(f, "{name:<16} | {ret_type:<16} | {param_str}")?;
		}
		writeln!(f)?;

		writeln!(f, "{:<16} | {:<11} | {:<10} | PREV READY COUNT",
			"PROC-TASK", "START TOKEN", "PREV TOKEN")?;
		writeln!(f, "{:-<16} | {:-<11} | {:-<10} | {:-<16}", "", "", "", "")?;
		for task in &self.proc_queue {
			writeln!(f, "{:<16} | {:<11} | {:<10} | {}",
				self.text(task.proc_name),
				task.start_token.index(),
				task.prev_furthest_token.index(),
				task.prev_ready_proc_count,
			)?;
		}
		writeln!(f)?;

		writeln!(f, "{:<16} | AST-NODES", "PROCEDURE")?;
		writeln!(f, "{:-<16} | {:-<16}", "", "")?;
		for (ident_id, &proc_start) in &self.completed_procs {
			write!(f, "{:<16} | ", self.text(*ident_id))?;
			let nodes = self.ast_nodes[proc_start..].iter();
			let src_pos = self.ast_pos_src[proc_start..].iter();
			let tok_pos = self.ast_pos_tok[proc_start..].iter();
			for ((token, pos), location) in nodes.zip(src_pos).zip(tok_pos) {
				if let ast::Kind::Ident(ident_id) = token {
					write!(f, " Ident({})", self.text(*ident_id))?;
				} else {
					write!(f, " {token:?}")?;
				}
				write!(f, "[{pos:?}]<{location:?}>")?;
			}
		}
		writeln!(f)?;

		for err in &self.errors {
			writeln!(f, "{}\n", err.display(self))?;
		}
		writeln!(f)?;

		Ok(())
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinaryOp {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	ShL,
	ShR,
	BinAnd,
	BinOr,
	BinXor,
	LogAnd,
	LogOr,
	LogXor,
	CmpEQ,
	CmpNE,
	CmpGE,
	CmpGT,
	CmpLE,
	CmpLT,
	Access,
}

impl fmt::Display for BinaryOp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Add => write!(f, "+"),
			Self::Sub => write!(f, "-"),
			Self::Mul => write!(f, "*"),
			Self::Div => write!(f, "/"),
			Self::Mod => write!(f, "%"),
			Self::ShL => write!(f, "<<"),
			Self::ShR => write!(f, ">>"),
			Self::BinAnd => write!(f, "&"),
			Self::BinOr => write!(f, "|"),
			Self::BinXor => write!(f, "^"),
			Self::LogAnd => write!(f, "&&"),
			Self::LogOr => write!(f, "||"),
			Self::LogXor => write!(f, "^^"),
			Self::CmpEQ => write!(f, "=="),
			Self::CmpNE => write!(f, "!="),
			Self::CmpGE => write!(f, ">="),
			Self::CmpGT => write!(f, ">"),
			Self::CmpLE => write!(f, "<="),
			Self::CmpLT => write!(f, "<"),
			Self::Access => write!(f, "."),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UnaryOp {
	Neg,
	Not,
}

impl fmt::Display for UnaryOp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Neg => write!(f, "-"),
			Self::Not => write!(f, "!"),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Bounds {
	Full { start: i64, end: i64 },
	From { start: i64 },
	To { end: i64 },
}

impl Bounds {
	fn get_start(&self) -> i64 {
		match self {
			Self::Full { start, ..} => *start,
			Self::From { start } => *start,
			Self::To {..} => 0,
		}
	}

	fn get_end(&self, table_size: usize) -> i64 {
		match self {
			Self::Full { end, ..} => *end,
			Self::From {..} => table_size as i64,
			Self::To { end } => *end,
		}
	}
}

impl fmt::Display for Bounds {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Full {start, end} => write!(f, "[{start}..{end}]"),
			Self::From {start} => write!(f, "[{start}..]"),
			Self::To {end} => write!(f, "[..{end}]"),
		}
	}
}

