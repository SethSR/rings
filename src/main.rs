
use std::{env, fmt, fs};
use std::collections::HashMap;
use std::fs::File;
use std::path::PathBuf;
use std::process::{Command, Stdio};

mod asm;
mod ast;
mod cursor;
mod discovery;
mod error;
mod identifier;
mod input;
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

	compile(file_path, &source)
		.unwrap_or_else(|msg| panic!("{msg}"));
}

type SrcPos = usize;

pub fn compile(file_path: String, source: &str) -> Result<(), String> {
	println!("=== Rings Compiler ===");
	println!();

	let input = input::eval(file_path, source.into());
	//println!("{input}");

	let lex_data = lexer::eval(&input.source)
		.map_err(|e| e.display(&input))?;
	lex_data.print(&input, false);

	let (mut dsc_data, task_queue) = discovery::eval(&input, &lex_data)
		.map_err(|e| e.into_comp_error(&input, &lex_data, error::Kind::Discovery))
		.map_err(|e| e.display(&input))?;
	discovery::print(&dsc_data, &task_queue, &input, &lex_data);

	let proc_db = parser::eval(&input, &lex_data, &mut dsc_data, task_queue)
		.map_err(|e| e.into_comp_error(&input, &lex_data, error::Kind::Parser))
		.map_err(|e| e.display(&input))?;
	parser::print(&proc_db, &input, &lex_data);

	let proc_db = type_checker::eval(&input, &lex_data, &dsc_data, proc_db)
		.map_err(|e| e.display(&input))?;
	//println!("{proc_db:?}");

	let (section_db,_) = vsmc::eval(&input, &lex_data, &proc_db, &mut dsc_data)
		.map_err(|e| e.into_comp_error(&input, &lex_data, &proc_db))
		.map_err(|e| e.display(&input))?;
	vsmc::print(&section_db, &input, &lex_data);

	let asm_db = asm::eval(&input, &lex_data, section_db);
	//println!("{asm_db:?}");

	output(&input.source_file, asm_db);
	Ok(())
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
	input: &'a input::Data,
	lex_data: &lexer::Data,
	ident_id: &identifier::Id,
) -> &'a str {
	let Span { start, end } = lex_data.identifiers[ident_id];
	&input.source[start..end]
}

fn token_source(
	input: &input::Data,
	lex_data: &lexer::Data,
	token_id: token::Id,
) -> Span<SrcPos> {
	let kind = lex_data.tok_list[token_id];
	let start = lex_data.tok_pos[token_id];
	Span { start, end: start + kind.size(&input, &lex_data) }
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

fn type_text(
	input: &input::Data,
	lex_data: &lexer::Data,
	ring_type: &rings_type::Type,
) -> String {
	match ring_type {
		rings_type::Type::Record(ident_id) => {
			text(input, lex_data, ident_id).to_string()
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

