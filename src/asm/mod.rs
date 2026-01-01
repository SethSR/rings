
use std::collections::hash_map::Entry;
use std::fmt::{Display, Formatter, Result as FmtResult};

use crate::{Data as MainDB, Error, Target};

mod m68k;
mod sh2;
mod x86_64;
mod z80;

pub fn eval(mut db: MainDB) -> Result<MainDB, Vec<Error>> {
	let source = &db.source;
	let identifiers = &db.identifiers;

	for (proc_id, proc_data) in &mut db.proc_db {
		let proc_name = MainDB::text_internal(source, identifiers, proc_id).to_owned();

		let data = match proc_data.target {
			Some(Target::M68k) => Data::M68k(m68k::lower(&proc_name, proc_data)),
			None |
			Some(Target::SH2) => Data::SH2(sh2::lower(&proc_name, proc_data)),
			Some(Target::X86_64) => Data::X86(x86_64::lower(&proc_name, proc_data)),
			Some(Target::Z80) => Data::Z80(z80::lower(&proc_name, proc_data)),
		};

		match db.asm_db.entry(*proc_id) {
			Entry::Vacant(e) => {
				e.insert(data);
			}
			Entry::Occupied(mut e) => {
				e.insert(data);
			}
		}
	}

	Ok(db)
}

#[derive(Debug)]
pub enum Data {
	M68k(Vec<m68k::Asm>),
	SH2(Vec<sh2::Asm>),
	X86(Vec<x86_64::Asm>),
	Z80(Vec<z80::Asm>),
}

impl Display for Data {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
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

