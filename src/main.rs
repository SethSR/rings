
use std::{env, fmt, fs};
use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::ops::Range;
use std::path::PathBuf;
use std::process::{Command, Stdio};

mod asm;
mod ast;
mod cursor;
mod discovery;
mod error;
mod identifier;
mod lexer;
mod operators;
mod parser;
mod rings_type;
mod span;
mod task;
mod token;
mod type_checker;
mod value;
mod vsmc;

use span::Span;

fn main() {
	let mut args = env::args();
	args.next();

	let file_path = args.next()
		.expect("expected source file");
	let source = fs::read_to_string(&file_path)
		.expect("unable to read source file");

	compile(file_path, &source);
}

type SrcPos = usize;

pub fn compile(file_path: String, source: &str) {
	let result = lexer::eval(Data::new(file_path, source.into()))
		.and_then(discovery::eval)
		.and_then(discovery::eval)
		.and_then(parser::eval)
		.and_then(|mut db| {
			type_checker::eval(
				&mut db.proc_db,
				&db.regions,
				&db.source,
				&db.identifiers,
				&db.procedures,
				&db.tok_list,
				&db.tok_pos,
			).map_err(|e| e.display(&db.source_file, &db.source, &db.line_pos))
				.map(|_| db)
		})
		.and_then(vsmc::eval)
		.map(|mut db| {
			asm::eval(&db.source, &db.identifiers, &mut db.proc_db, &mut db.asm_db);
			db
		});
	match result {
		Ok(data) => {
			println!("{data}");
			output(&data.source_file, data.asm_db);
		}
		Err(msg) => eprintln!("{msg}"),
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Target {
	M68k,
	SH2,
	X86_64,
	Z80,
}

// TODO - srenshaw - We need to validate 'targets' for their respective consoles.

// TODO - srenshaw - Determine how to get 'signed/unsigned' info to assembly arithmetic/comparison ops, and type size info to assembly storage ops.

// TODO - srenshaw - Lookup where to setup Data and Call Stack Regions on Saturn, MegaDrive, and x64 PC.

// TODO - srenshaw - Need to add Table location calculations.

#[allow(non_snake_case)]
#[derive(Default)]
pub struct Data {
	DEBUG_show_tokens: bool,

	source_file: String,
	source: Box<str>,
	// stores the position of each newline (\n) character in the source
	line_pos: token::PosList,

	/* Lexer */
	tok_list: token::KindList,
	tok_pos: token::PosList,
	identifiers: identifier::Map<Span<SrcPos>>,

	/* Discovery */
	procedures: discovery::ProcMap,
	values: discovery::ValueMap,
	regions: discovery::RegionMap,
	records: discovery::RecordMap,
	#[cfg(feature="table")]
	tables: discovery::TableMap,

	/* Parsing */
	task_queue: VecDeque<task::Task>,

	/* Backend Procedure Data */
	proc_db: identifier::Map<ProcData>,
	asm_db: identifier::Map<asm::Data>,
}

#[derive(Debug, Default, Clone)]
pub struct ProcData {
	target: Option<Target>,
	ast_start: ast::Id,
	ast_nodes: ast::KindList,
	ast_pos_tok: ast::LocList,
	/* Type Checking */
	ast_to_type: HashMap<ast::Id, rings_type::Type>,
	ident_to_type: identifier::Map<rings_type::Type>,
	/* Lowering TAC */
	tac_data: Option<vsmc::Section>,
}

impl ProcData {
	pub fn new(ast_start: ast::Id) -> Self {
		let mut this = Self::default();
		this.ast_start = ast_start;
		this
	}

	pub fn add_ast(&mut self, kind: ast::Kind,
		tok_range: Range<token::Id>,
	) -> ast::Id {
		self.ast_nodes.push(kind);
		self.ast_pos_tok.push(tok_range.into());
		ast::Id::new(self.ast_nodes.len() - 1)
	}
}

fn output(source_file: &str, asm_db: identifier::Map<asm::Data>) {
	let mut out_path = PathBuf::from(source_file);
	out_path.set_extension("");

	let mut out_data: HashMap<Target, Vec<asm::Data>> = HashMap::new();
	for (_, asm_data) in asm_db {
		let target_entry = match asm_data {
			asm::Data::M68k(_) => out_data.entry(Target::M68k),
			asm::Data::SH2(_) => out_data.entry(Target::SH2),
			asm::Data::X86(_) => out_data.entry(Target::X86_64),
			asm::Data::Z80(_) => out_data.entry(Target::Z80),
		};
		target_entry.or_default().push(asm_data);
	}

	for (target, data) in out_data {
		use std::io::Write;

		let out_path = match target {
			Target::M68k => out_path.with_extension("m68k"),
			Target::SH2 => out_path.with_extension("sh2"),
			Target::X86_64 => out_path.with_extension("x64"),
			Target::Z80 => out_path.with_extension("z80"),
		};

		let out_path = out_path.with_added_extension("asm");
		let out_file = File::create(&out_path)
				.expect("unable to create output file");

		if target == Target::SH2 {
			writeln!(&out_file, "include 'sh2.inc'")
					.expect("unable to write to sh2 file");
		}

		for out in data {
			writeln!(&out_file, "{out}")
					.expect("unable to write to output file");
		}

		let output = match target {
			Target::M68k => {
				Command::new("./vasmm68k_std")
						.arg(&out_path)
						.arg("-o")
						.arg(out_path.with_extension("o"))
						.output()
			}
			Target::SH2 => {
				Command::new("./fasmg")
						.arg(&out_path)
						.arg(out_path.with_extension("bin"))
						.output()
			}
			Target::X86_64 => {
				let output = Command::new("./vasmx86_std")
						.arg("-m64")
						.arg("-Felf")
						.arg(&out_path)
						.arg("-o")
						.arg(out_path.with_extension("o"))
						.stdin(Stdio::piped())
						.output();
				match output {
					Err(e) => panic!("{e}"),
					Ok(out) if out.status.success() => {
						Command::new("ld")
								.arg(out_path.with_extension("o"))
								.arg("-o")
								.arg(out_path.with_extension(""))
								.output()
					}
					Ok(out) => panic!("{out:?}"),
				}
			}
			Target::Z80 => {
				Command::new("./vasmz80_std")
						.arg(&out_path)
						.arg("-o")
						.arg(out_path.with_extension("o"))
						.output()
			}
		};

		match output {
			Err(e) => eprintln!("{e}"),
			Ok(out) if out.status.success() => {}
			Ok(out) => eprintln!("{:?}", out.stderr),
		}
	}
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

fn text<'a>(
	source: &'a str,
	identifiers: &identifier::Map<Span<SrcPos>>,
	ident_id: &identifier::Id,
) -> &'a str {
	let Span { start, end } = identifiers[ident_id];
	&source[start..end]
}

fn token_source(
	source: &str,
	identifiers: &identifier::Map<Span<SrcPos>>,
	tok_list: &token::KindList,
	tok_pos: &token::PosList,
	token_id: token::Id,
) -> Span<SrcPos> {
	let kind = tok_list[token_id];
	let start = tok_pos[token_id];
	Span { start, end: start + kind.size(source, identifiers) }
}

fn ast_source(
	source: &str,
	identifiers: &identifier::Map<Span<SrcPos>>,
	tok_list: &token::KindList,
	tok_pos: &token::PosList,
	proc_db: &identifier::Map<ProcData>,
	proc_id: identifier::Id,
	ast_id: ast::Id,
) -> Span<SrcPos> {
	let span = proc_db[&proc_id].ast_pos_tok[ast_id];
	let start = tok_pos[span.start];
	let end = tok_pos[span.end] + tok_list[span.end].size(source, identifiers);
	Span { start, end }
}

fn type_size(
	records: &discovery::RecordMap,
	#[cfg(feature="table")]
	tables: &discovery::TableMap,
	ring_type: &rings_type::Type,
) -> u32 {
	match ring_type {
		#[cfg(feature="types")]
		rings_type::Type::Bool => 1,
		#[cfg(feature="types")]
		rings_type::Type::U8 => 1,
		rings_type::Type::S8(_) => 1,
		#[cfg(feature="types")]
		rings_type::Type::U16 => 2,
		#[cfg(feature="types")]
		rings_type::Type::S16 => 2,
		#[cfg(feature="types")]
		rings_type::Type::U32 => 4,
		#[cfg(feature="types")]
		rings_type::Type::S32 => 4,
		rings_type::Type::Record(ident_id) => records[ident_id].size(records),
		#[cfg(feature="table")]
		rings_type::Type::Table(ident_id) => {
			tables[&ident_id].size(self)
		}
		rings_type::Type::Unit => 0,
		rings_type::Type::Top | rings_type::Type::Bot => 0,
		rings_type::Type::Int => 0,
	}
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
		text(
			&self.source, &self.identifiers,
			ident_id,
		)
	}

	pub fn token_source(&self, token_id: token::Id) -> Span<SrcPos> {
		token_source(
			&self.source, &self.identifiers,
			&self.tok_list, &self.tok_pos,
			token_id,
		)
	}

	pub fn ast_source(&self, proc_id: identifier::Id, ast_id: ast::Id) -> Span<usize> {
		ast_source(
			&self.source, &self.identifiers,
			&self.tok_list, &self.tok_pos,
			&self.proc_db, proc_id, ast_id,
		)
	}

	fn type_text(&self, ring_type: &rings_type::Type) -> String {
		match ring_type {
			rings_type::Type::Record(ident_id) => {
				text(
					&self.source, &self.identifiers,
					ident_id,
				).to_string()
			}
			#[cfg(feature="table")]
			Type::Table(ident_id) => {
				text(
					&self.source, &self.identifiers,
					ident_id,
				).to_string()
			}
			_ => format!("{ring_type:?}"),
		}
	}
}

impl fmt::Display for Data {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		writeln!(f, "=== Rings Compiler ===")?;
		writeln!(f)?;

		fn fields_to_str(data: &Data, fields: &[(identifier::Id, rings_type::Type)]) -> String {
			fields.iter()
				.map(|(field_id, field_type)| {
					format!("{}:{}", data.text(field_id), data.type_text(field_type))
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

		writeln!(f, "{:<16} | {:<9} | {:<9}", "REGION", "ADDRESS", "SIZE")?;
		writeln!(f, "{:-<16} | {:-<9} | {:-<9}", "", "", "")?;
		for (ident_id, data) in self.regions.iter() {
			let name = self.text(ident_id);
			let address = data.span.start;
			let size = fmt_size((data.span.end - data.span.start) as usize);
			writeln!(f, "{name:<16} | #{address:0>8X} | {size:<8}")?;
		}
		writeln!(f)?;

		writeln!(f, "{:<16} | {:<8} | {:<9} | FIELDS",
			"RECORD", "SIZE", "ADDRESS")?;
		writeln!(f, "{:-<16} | {:-<8} | {:-<9} | {:-<16}", "", "", "", "")?;
		for (ident_id, record) in self.records.iter() {
			let name = self.text(ident_id);
			let size = record.size(&self.records);
			let address = record.region
				.map(|id| format!("#{:0>8X}", self.regions[&id].span.start))
				.unwrap_or("-".to_string());
			let field_str = fields_to_str(self, &record.fields);
			writeln!(f, "{name:<16} | {size:<8} | {address:9} | {field_str}")?;
		}
		writeln!(f)?;

		#[cfg(feature="table")]
		writeln!(f, "{:<16} | {:<10} | {:<8} | {:<9} | {:<9} | COLUMNS",
			"TABLE", "TOTAL SIZE", "ROW SIZE", "ROW COUNT", "ADDRESS")?;
		#[cfg(feature="table")]
		writeln!(f, "{:-<16} | {:-<10} | {:-<8} | {:-<9} | {:-<9} | {:-<16}", "", "", "", "", "", "")?;
		#[cfg(feature="table")]
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
		#[cfg(feature="table")]
		writeln!(f)?;

		if !self.procedures.is_empty() {
			let (types, params): (Vec<_>, Vec<_>) = self.procedures.iter()
					.map(|(ident_id, data)| {
						let name = self.text(ident_id);
						let params = data.params.iter()
							.map(|(p_name, p_type)| {
								format!("{name:<32} | {:<16} | {:<16}", self.text(p_name), self.type_text(p_type))
							})
							.collect::<Vec<_>>()
							.join("\n");
						(
							format!("{name:<32} | {:<16?} | {:<16?}", data.ret_type, data.target),
							params,
						)
					})
					.unzip();

			writeln!(f, "{:<32} | {:<16} | TARGET",
				"PROCEDURE", "RETURN-TYPE")?;
			writeln!(f, "{:-<32} | {:-<16} | {:-<16}", "", "", "")?;
			writeln!(f, "{}", types.join("\n"))?;
			writeln!(f)?;

			let params = params.into_iter()
				.filter(|p| !p.is_empty())
				.collect::<Vec<_>>();
			if !params.is_empty() {
				writeln!(f, "{:<32} | {:<16} | PARAM-TYPE",
					"PROCEDURE", "PARAM-NAME")?;
				writeln!(f, "{:-<32} | {:-<16} | {:-<16}", "", "", "")?;
				writeln!(f, "{}", params.join("\n"))?;
				writeln!(f)?;
			}
		}

		if !self.task_queue.is_empty() {
			writeln!(f, "{:<32} | {:<11} | {:<11} | {:<10} | PREV QUEUE LENGTH",
				"TASK", "KIND", "START TOKEN", "LATEST TOKEN")?;
			writeln!(f, "{:-<32} | {:-<11} | {:-<11} | {:-<10} | {:-<16}", "", "", "", "", "")?;
			for task in &self.task_queue {
				writeln!(f, "{:<32} | {:<11?} | {:<11} | {:<10} | {:?}",
					self.text(&task.name_id),
					task.kind,
					task.tok_start.index(),
					task.prev_furthest_token.index(),
					task.prev_queue_length,
				)?;
			}
			writeln!(f)?;
		}

		if !self.proc_db.is_empty() {
			for (proc_id, proc_data) in &self.proc_db {
				if !proc_data.ast_to_type.is_empty() {
					writeln!(f, "{:<32} | {:<8} | TYPE",
						"PROCEDURE", "AST-ID")?;
					writeln!(f, "{:-<32} | {:-<8} | {:-<8}", "", "", "")?;
					for (ast_id, rings_type) in &proc_data.ast_to_type {
						let ast = ast_id.to_string();
						writeln!(f, "{:<32} | {ast:<8} | {:<8}",
							self.text(proc_id), self.type_text(rings_type))?;
					}
					writeln!(f)?;
				}

				if !proc_data.ident_to_type.is_empty() {
					writeln!(f, "{:<32} | {:<16} | TYPE",
						"PROCEDURE", "VARIABLE")?;
					writeln!(f, "{:-<32} | {:-<16} | {:-<8}", "", "", "")?;
					for (ident_id, rings_type) in &proc_data.ident_to_type {
						writeln!(f, "{:<32} | {:<16} | {:<8}",
							self.text(proc_id), self.text(ident_id), self.type_text(rings_type))?;
					}
					writeln!(f)?;
				}

				if let Some(tac_data) = &proc_data.tac_data {
					writeln!(f, "{:<32} | {:<16} | LOCAL-TYPE",
						"TAC-LOCALS", "LOCAL-NAME")?;
					writeln!(f, "{:-<32} | {:-<16} | {:-<16}", "", "", "")?;
					for (local_id, ring_type) in &tac_data.locals {
						writeln!(f, "{:<32} | {:<16} | {:<16}",
							self.text(proc_id), self.text(local_id), self.type_text(ring_type))?;
					}
					writeln!(f)?;

					writeln!(f, "{:<32} | INSTRUCTIONS",
						"TAC-SECTIONS")?;
					writeln!(f, "{:-<32} | {:-<16}", "", "")?;
					for tac in &tac_data.instructions {
						writeln!(f, "{:<32} | {}", self.text(proc_id), tac.to_text(&self.source, &self.identifiers))?;
					}
					writeln!(f)?;
				}
			}
		}

		Ok(())
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bounds {
	Full { start: u32, end: u32 },
	From { start: u32 },
	To { end: u32 },
}

#[cfg(feature="forloop")]
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

