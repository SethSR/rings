
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
mod tac;
mod token;
mod type_checker;
mod rings_type;

use error::CompilerError;
use rings_type::Type;

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

pub fn compile(file_path: String, source: Box<str>) -> Data {
	let mut data = Data::new(file_path, source);
	lexer::eval(&mut data);
	discovery::eval(&mut data);
	parser::eval(&mut data);
	type_checker::eval(&mut data);
	tac::eval(&mut data);
	data
}

// TODO - srenshaw - Need to add Table location calculations.

#[allow(non_snake_case)]
#[derive(Default)]
pub struct Data {
	DEBUG_show_tokens: bool,

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
	parse_queue: VecDeque<discovery::Task>,
	procedures: discovery::ProcMap,
	values: discovery::ValueMap,
	#[cfg(feature="ready")]
	regions: discovery::RegionMap,
	#[cfg(feature="ready")]
	records: discovery::RecordMap,
	#[cfg(feature="ready")]
	tables: discovery::TableMap,
	/* Parsing */
	proc_queue: VecDeque<parser::Task>,
	ast_nodes: ast::KindList,
	ast_pos_tok: ast::LocList,
	// procedures ready to be type-checked
	completed_procs: identifier::Map<ast::Id>,
	/* Type Checking */
	ast_to_type: HashMap<ast::Id, Type>,
	ident_to_type: HashMap<identifier::Id, Type>,
	/* Lowering TAC */
	tac_sections: identifier::Map<tac::TacSection>,
}

#[cfg(feature="ready")]
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

	pub fn text(&self, ident_id: &identifier::Id) -> &str {
		&self.source[self.identifiers[ident_id].clone()]
	}

	fn type_text(&self, ring_type: Type) -> String {
		match ring_type {
			#[cfg(feature="ready")]
			Type::Record(ident_id) => self.text(&ident_id).to_string(),
			#[cfg(feature="ready")]
			Type::Table(ident_id) => self.text(&ident_id).to_string(),
			_ => format!("{ring_type:?}"),
		}
	}

	#[cfg(feature="ready")]
	fn type_size(&self, ring_type: &Type) -> u32 {
		match ring_type {
			#[cfg(feature="ready")]
			Type::Bool => 1,
			#[cfg(feature="ready")]
			Type::U8 => 1,
			Type::S8(_) => 1,
			#[cfg(feature="ready")]
			Type::U16 => 2,
			#[cfg(feature="ready")]
			Type::S16 => 2,
			#[cfg(feature="ready")]
			Type::U32 => 4,
			#[cfg(feature="ready")]
			Type::S32 => 4,
			#[cfg(feature="ready")]
			Type::Record(ident_id) => self.records[&ident_id].size(self),
			#[cfg(feature="ready")]
			Type::Table(ident_id) => self.tables[&ident_id].size(self),
			Type::Unit => 0,
			Type::Top | Type::Bot => 0,
		}
	}

	// Get the text of a specific line
	pub fn get_line(&self, line_number: usize) -> String {
		self.get_line_text(line_number).replace('\t', "  ")
	}

	// Convert byte offset to line and column
	pub fn lookup_position(&self, tok_pos: SrcPos) -> (usize, usize) {
		let line = self.line_pos.binary_search(&tok_pos)
			.unwrap_or_else(|line| line - 1);

		let line_pos = self.line_pos.get(line)
			.unwrap_or_else(|| panic!("missing position for line number {line}"));
		assert!(tok_pos >= *line_pos, "tok({tok_pos}) line({line_pos})");
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
					format!("{}:{}", data.text(field_id), data.type_text(*field_type))
				})
				.collect::<Vec<_>>()
				.join(", ")
		}

		if self.DEBUG_show_tokens {
			write!(f, "Tokens:\n ")?;
			for (token, start) in self.tok_list.iter().zip(self.tok_pos.iter()) {
				match token {
					token::Kind::Identifier(ident_id) => {
						write!(f, " Identifier({})[{start}]", self.text(ident_id))?;
					}
					_ => write!(f, " {token:?}[{start}]")?,
				}
			}
			writeln!(f)?;
			writeln!(f)?;
		}

		let mut identifiers = self.identifiers.keys()
			.map(|id| (id, self.text(id)))
			.collect::<Vec<_>>();
		identifiers.sort_by(|(_,a),(_,b)| a.cmp(b));

		writeln!(f, "{:<32} | HASH-VALUE",
			"IDENTIFIER")?;
		writeln!(f, "{:-<32} | {:-<16}", "", "")?;
		for (ident_id, ident) in identifiers {
			writeln!(f, "{ident:<32} | {ident_id}")?;
		}
		writeln!(f)?;

		#[cfg(feature="ready")]
		writeln!(f, "{:<16} | {:<9} | {:<9}", "REGION", "ADDRESS", "SIZE")?;
		#[cfg(feature="ready")]
		writeln!(f, "{:-<16} | {:-<9} | {:-<9}", "", "", "")?;
		#[cfg(feature="ready")]
		for (ident_id, data) in self.regions.iter() {
			let name = self.text(ident_id);
			let address = data.address;
			let size = fmt_size(data.byte_count as usize);
			writeln!(f, "{name:<16} | #{address:0>8X} | {size:<8}")?;
		}
		#[cfg(feature="ready")]
		writeln!(f)?;

		#[cfg(feature="ready")]
		writeln!(f, "{:<16} | {:<8} | {:<9} | FIELDS",
			"RECORD", "SIZE", "ADDRESS")?;
		#[cfg(feature="ready")]
		writeln!(f, "{:-<16} | {:-<8} | {:-<9} | {:-<16}", "", "", "", "")?;
		#[cfg(feature="ready")]
		for (ident_id, record) in self.records.iter() {
			let name = self.text(ident_id);
			let size = record.size(self);
			let address = record.address
				.map(|num| format!("#{num:0>8X}"))
				.unwrap_or("-".to_string());
			let field_str = fields_to_str(self, &record.fields);
			writeln!(f, "{name:<16} | {size:<8} | {address:9} | {field_str}")?;
		}
		#[cfg(feature="ready")]
		writeln!(f)?;

		#[cfg(feature="ready")]
		writeln!(f, "{:<16} | {:<10} | {:<8} | {:<9} | {:<9} | COLUMNS",
			"TABLE", "TOTAL SIZE", "ROW SIZE", "ROW COUNT", "ADDRESS")?;
		#[cfg(feature="ready")]
		writeln!(f, "{:-<16} | {:-<10} | {:-<8} | {:-<9} | {:-<9} | {:-<16}", "", "", "", "", "", "")?;
		#[cfg(feature="ready")]
		for (ident_id, table) in self.tables.iter() {
			let name = self.text(ident_id);
			let size = table.size(self);
			let row_size = size / table.row_count;
			let address = table.address
				.map(|num| format!("#{num:0>8X}"))
				.unwrap_or("-".to_string());
			let field_str = fields_to_str(self, &table.column_spec);
			writeln!(f, "{name:<16} | {size:<10} | {row_size:<8} | {:<9} | {address:9} | {field_str}", table.row_count)?;
		}
		#[cfg(feature="ready")]
		writeln!(f)?;

		if !self.procedures.is_empty() {
			writeln!(f, "{:<32} | {:<16} | PARAMETERS",
				"PROC-TYPE", "RETURN-TYPE")?;
			writeln!(f, "{:-<32} | {:-<16} | {:-<16}", "", "", "")?;
			for ident_id in self.procedures.keys() {
				let name = self.text(ident_id);
				let data = &self.procedures[ident_id];
				let ret_type = format!("{:?}", data.ret_type);
				let param_str = fields_to_str(self, &data.params);
				writeln!(f, "{name:<32} | {ret_type:<16} | {param_str}")?;
			}
			writeln!(f)?;
		}

		if !self.proc_queue.is_empty() {
			writeln!(f, "{:<32} | {:<11} | {:<10} | PREV READY COUNT",
				"PROC-TASK", "START TOKEN", "PREV TOKEN")?;
			writeln!(f, "{:-<32} | {:-<11} | {:-<10} | {:-<16}", "", "", "", "")?;
			for task in &self.proc_queue {
				writeln!(f, "{:<32} | {:<11} | {:<10} | {}",
					task.name(self),
					task.tok_start.index(),
					task.prev_furthest_token.index(),
					task.prev_ready_proc_count,
				)?;
			}
			writeln!(f)?;
		}

		#[cfg(feature="ready")]
		if !self.completed_procs.is_empty() {
			writeln!(f, "{:<32} | AST-NODE-COUNT",
				"PROCEDURE")?;
			writeln!(f, "{:-<32} | {:-<16}", "", "")?;
			for (ident_id, &proc_start) in &self.completed_procs {
				let node_count = self.ast_nodes[proc_start..].len();
				writeln!(f, "{:<32} | {node_count}", self.text(ident_id))?;
			}
			writeln!(f)?;
		}

		if !self.ast_to_type.is_empty() {
			writeln!(f, "{:<8} | TYPE",
				"AST-ID")?;
			writeln!(f, "{:-<8} | {:-<8}", "", "")?;
			for (ast_id, rings_type) in &self.ast_to_type {
				let ast = ast_id.to_string();
				writeln!(f, "{ast:<8} | {:<8}", self.type_text(*rings_type))?;
			}
			writeln!(f)?;
		}

		if !self.ident_to_type.is_empty() {
			writeln!(f, "{:<16} | TYPE",
				"VARIABLE")?;
			writeln!(f, "{:-<16} | {:-<8}", "", "")?;
			for (ident_id, rings_type) in &self.ident_to_type {
				writeln!(f, "{:<16} | {:<8}", self.text(ident_id), self.type_text(*rings_type))?;
			}
			writeln!(f)?;
		}

		if !self.tac_sections.is_empty() {
			writeln!(f, "{:<32} | {:<16} | LOCAL-TYPE",
				"TAC-LOCALS", "LOCAL-NAME")?;
			writeln!(f, "{:-<32} | {:-<16} | {:-<16}", "", "", "")?;
			for (ident_id, section) in &self.tac_sections {
				for (local_id, ring_type) in &section.locals {
					writeln!(f, "{:<32} | {:<16} | {:<16}",
						self.text(ident_id),
						self.text(local_id),
						self.type_text(*ring_type),
					)?;
				}
			}
			writeln!(f)?;

			writeln!(f, "{:<32} | INSTRUCTIONS",
				"TAC-SECTIONS")?;
			writeln!(f, "{:-<32} | {:-<16}", "", "")?;
			for (ident_id, section) in &self.tac_sections {
				for tac in &section.instructions {
					writeln!(f, "{:<32} | {}", self.text(ident_id), tac.to_text(self))?;
				}
			}
			writeln!(f)?;
		}

		for err in &self.errors {
			writeln!(f, "{}\n", err.display(self))?;
		}

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
	Index,
	Call,
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
			Self::Index => write!(f, "[]"),
			Self::Call => write!(f, "()"),
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
	Full { start: u32, end: u32 },
	From { start: u32 },
	To { end: u32 },
}

#[cfg(feature="ready")]
impl Bounds {
	fn get_start(&self) -> u32 {
		match self {
			Self::Full { start, ..} => *start,
			Self::From { start } => *start,
			Self::To {..} => 0,
		}
	}

	fn get_end(&self, table_size: u32) -> u32 {
		match self {
			Self::Full { end, ..} => *end,
			Self::From {..} => table_size,
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

