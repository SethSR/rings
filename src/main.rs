
use std::{env, fmt, fs};
use std::collections::{HashMap, VecDeque};
use std::ops::Range;

mod ast;
mod cursor;
mod discovery;
mod error;
mod identifier;
mod lexer;
mod parser;
mod token;

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

pub fn compile(file_path: String, source: Box<str>) -> Data {
	let mut data = Data::new(file_path, source);
	lexer::eval(&mut data);
	discovery::eval(&mut data);
	parser::eval(&mut data);
	data
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ValueKind {
	Integer(i64),
	Decimal(f64),
}

// TODO - srenshaw - Need to add Record and Table size calculations, and Table location
// calculations.

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
	proc_start: identifier::Map<token::Id>,
	procedures: identifier::Map<ProcType>,
	// val-name -> value-kind
	values: identifier::Map<ValueKind>,
	regions: identifier::Map<RegionData>,
	// rec-name -> (field, type)*
	records: identifier::Map<ColumnData>,
	record_sizes: identifier::Map<usize>,
	tables: identifier::Map<TableData>,
	table_sizes: identifier::Map<usize>,
	/* Parsing */
	proc_queue: VecDeque<Task>,
	ast_nodes: ast::KindList,
	ast_locations: ast::LocList,
	// procedures ready to be type-checked
	completed_procs: identifier::Map<Range<ast::Id>>,
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
			Type::Record(ident_id) => self.record_sizes[&ident_id],
			Type::Table(ident_id) => self.table_sizes[&ident_id],
			Type::Unit => 0,
		}
	}

	// Get the text of a specific line
	pub fn get_line(&self, line_number: usize) -> &str {
		if !(1..=self.line_pos.len()).contains(&line_number) {
			return "";
		}

		let start = self.line_pos[line_number - 1] as usize;
		let end = if line_number < self.line_pos.len() {
			self.line_pos[line_number] as usize - 1
		} else {
			self.source.len()
		};

		&self.source[start..end]
	}

	// Convert byte offset to line and column
	pub fn lookup_position(&self, tok_pos: SrcPos) -> (usize, usize) {
		let line = match self.line_pos.binary_search(&tok_pos) {
			Ok(line) => line,
			Err(line) => line - 1,
		};

		let line_pos = self.line_pos[line];
		let column = tok_pos - line_pos;

		(line.index() + 1, column + 1)
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
		for (ident_id, fields) in self.records.iter() {
			let name = self.text(*ident_id);
			let size = self.record_sizes[ident_id];
			let field_str = fields_to_str(self, fields);
			writeln!(f, "{name:<16} | {size:<8} | {field_str}")?;
		}
		writeln!(f)?;

		writeln!(f, "{:<16} | {:<10} | {:<8} | {:<9} | COLUMNS",
			"TABLE", "TOTAL SIZE", "ROW SIZE", "ROW COUNT")?;
		writeln!(f, "{:-<16} | {:-<10} | {:-<8} | {:-<9} | {:-<16}", "", "", "", "", "")?;
		for (ident_id, data) in self.tables.iter() {
			let name = self.text(*ident_id);
			let size = self.table_sizes[ident_id];
			let row_size = size / data.row_count as usize;
			let field_str = fields_to_str(self, &data.column_spec);
			writeln!(f, "{name:<16} | {size:<10} | {row_size:<8} | {:<9} | {field_str}", data.row_count)?;
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
		for (ident_id, data) in &self.completed_procs {
			write!(f, "{:<16} | ", self.text(*ident_id))?;
			let nodes = self.ast_nodes[data.clone()].iter();
			let locations = self.ast_locations[data.clone()].iter();
			for (token, location) in nodes.zip(locations) {
				if let ast::Kind::Ident(ident_id) = token {
					write!(f, " Ident({})", self.text(*ident_id))?;
				} else {
					write!(f, " {token:?}")?;
				}
				write!(f, "[{location:?}]")?;
			}
		}
		writeln!(f)?;

		if !self.errors.is_empty() {
			// Errors are printed in bold(1m) red(31m)
			writeln!(f, "Error:")?;
			for err in &self.errors {
				writeln!(f, "{}", err.display(self))?;
			}
			writeln!(f)?;
		}

		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ProcType {
	params: Vec<(identifier::Id, Type)>,
	ret_type: Type,
}

type ColumnData = Vec<(identifier::Id, Type)>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RegionData {
	address: u32,
	byte_count: u32,
}

#[derive(Debug, PartialEq)]
struct TableData {
	row_count: u32,
	column_spec: ColumnData,
}

#[derive(Debug)]
struct Task {
	proc_name: identifier::Id,
	start_token: token::Id,
	prev_furthest_token: token::Id,
	prev_ready_proc_count: usize,
}

#[derive(Debug)]
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
}

#[derive(Debug)]
enum UnaryOp {
	Neg,
	Not,
}

#[derive(Debug)]
enum RangeType {
	Full { start: i64, end: i64 },
	From { start: i64 },
	To { end: i64 },
}

#[derive(Debug)]
struct ScopeStack(Vec<HashMap<identifier::Id, ast::Id>>);

impl Default for ScopeStack {
	fn default() -> Self {
		Self(vec![HashMap::default()])
	}
}

