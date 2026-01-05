
use std::collections::hash_map::Entry;
use std::fmt::{Display, Formatter, Result};

use crate::identifier::Map as IdentMap;
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::vsmc::{LabelId, Section};
use crate::text;
use crate::Target;

mod m68k;
mod sh2;
mod x86_64;
mod z80;

pub fn eval(
	input: &InputData,
	lex_data: &LexData,
	section_db: IdentMap<Section>,
) -> IdentMap<Data> {
	let mut out = IdentMap::<Data>::default();

	for (proc_id, section) in section_db {
		let proc_name = text(input, lex_data, &proc_id).to_owned();

		let data = match section.target {
			Target::M68k => Data::M68k(m68k::lower(&proc_name, section)),
			Target::SH2 => Data::SH2(sh2::lower(&proc_name, section)),
			Target::X86_64 => Data::X86(x86_64::lower(&proc_name, section)),
			Target::Z80 => Data::Z80(z80::lower(&proc_name, section)),
		};

		match out.entry(proc_id) {
			Entry::Vacant(e) => {
				e.insert(data);
			}
			Entry::Occupied(mut e) => {
				e.insert(data);
			}
		}
	}
	
	out
}

#[derive(Debug)]
pub enum Data {
	M68k(Vec<m68k::Asm>),
	SH2(Vec<sh2::Asm>),
	X86(Vec<x86_64::Asm>),
	Z80(Vec<z80::Asm>),
}

impl Display for Data {
	fn fmt(&self, f: &mut Formatter) -> Result {
		let mut out = vec![];
		match self {
			Self::M68k(data) => {
				for asm in data {
					out.push(asm.to_string());
				}
			}
			Self::SH2(data) => {
				for asm in data {
					out.push(asm.to_string());
				}
			}
			Self::X86(data) => {
				for asm in data {
					out.push(asm.to_string());
				}
			}
			Self::Z80(data) => {
				for asm in data {
					out.push(asm.to_string());
				}
			}
		}
		write!(f, "{}", out.join("\n"))
	}
}

pub fn inner_label(name: &str, id: &mut LabelId) -> String {
	*id += 1;
	format!("{name}_{}", *id - 1)
}

