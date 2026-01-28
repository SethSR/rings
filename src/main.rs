
use std::{env, fs};
//use std::collections::HashMap;
//use std::fs::File;
//use std::path::PathBuf;
//use std::process::{Command, Stdio};

//mod asm;
mod error;
mod identifier;
mod input;
mod packing;
mod layout;
mod lexer;
mod operators;
mod parser;
mod types;
mod span;
mod token;
mod type_checker;
//mod vsmc;

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

	let prs_data = parser::eval(&input, &lex_data, false)
		.map_err(|e| e.display(&input))?;
	println!("{prs_data:?}\n");

	let proc_db = type_checker::eval(&input, &lex_data, &prs_data)
		.map_err(|e| e.display(&input))?;
	println!("Procedures: {proc_db:?}");
	for (proc_id, list) in &proc_db {
		println!("  {}:", lex_data.text(&input, proc_id));
		for ast in list {
			println!("    {ast:?}");
		}
	}
	println!();

	let pak_data = packing::eval(&prs_data);
	println!("Packing:");
	for (rec_id, record) in &pak_data.records {
		println!("  {}: {record:?}", lex_data.text(&input, rec_id));
	}
	for (tab_id, table) in &pak_data.tables {
		println!("  {}: {table:?}", lex_data.text(&input, tab_id));
	}

	println!("Layout:");
	let lay_data = layout::eval(&prs_data, &pak_data)
		.map_err(|e| e.display(&input, &lex_data))?;
	for (reg_id, offset) in lay_data {
		println!("  {}: {offset}", lex_data.text(&input, &reg_id));
	}

	/*
	let section_db = vsmc::eval(&input, &lex_data, &prs_data, &lay_data)
		.map_err(|e| e.display(&input))?;
	vsmc::print(&section_db, &input, &lex_data);

	let asm_db = asm::eval(&input, &lex_data, section_db);
	//println!("{asm_db:?}");

	output(&input.source_file, asm_db);
	*/

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

/*
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
*/

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

fn token_source(
	input: &input::Data,
	lex_data: &lexer::Data,
	token_id: token::Id,
) -> Span<SrcPos> {
	let kind = lex_data.tok_list[token_id];
	let start = lex_data.tok_pos[token_id];
	Span { start, end: start + kind.size(&input, &lex_data) }
}

fn type_text(
	input: &input::Data,
	lex_data: &lexer::Data,
	ring_type: &types::Type,
) -> String {
	match ring_type {
		types::Type::Record(ident_id) => {
			lex_data.text(input, ident_id).to_string()
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
