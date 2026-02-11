
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result};

use crate::identifier::Map as IdentMap;
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::tac::{LabelId, Data as TacData};
use crate::{Span, Target};

mod m68k;
//mod sh2;
//mod x86_64;
//mod z80;

pub fn eval(
	input: &InputData,
	lex_data: &LexData,
	tac_data: IdentMap<TacData>,
) -> IdentMap<Data> {
	let mut out = IdentMap::<Data>::default();

	for (proc_id, section) in tac_data {
		let proc_name = lex_data.text(input, &proc_id).to_owned();

		let data = match section.target {
			Target::M68k => Data::M68k(m68k::lower(&proc_name, section)),
			//Target::SH2 => Data::SH2(sh2::lower(&proc_name, section)),
			//Target::X86_64 => Data::X86(x86_64::lower(&proc_name, section)),
			//Target::Z80 => Data::Z80(z80::lower(&proc_name, section)),
			_ => unreachable!(),
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
	//SH2(Vec<sh2::Asm>),
	//X86(Vec<x86_64::Asm>),
	//Z80(Vec<z80::Asm>),
}

impl Display for Data {
	fn fmt(&self, f: &mut Formatter) -> Result {
		let mut out = vec![];
		match self {
			Self::M68k(data) => out.extend(
				data.iter().map(|asm| asm.to_string())
			),
			//Self::SH2(data) => {
				//for asm in data {
					//out.push(asm.to_string());
				//}
			//}
			//Self::X86(data) => {
				//for asm in data {
					//out.push(asm.to_string());
				//}
			//}
			//Self::Z80(data) => {
				//for asm in data {
					//out.push(asm.to_string());
				//}
			//}
		}
		write!(f, "{}", out.join("\n"))
	}
}

struct LabelGenerator(LabelId);
impl LabelGenerator {
	fn new(lbl: LabelId) -> Self {
		Self(lbl)
	}

	fn next(&mut self, name: &str) -> String {
		self.0 += 1;
		format!("{name}_{}", self.0 - 1)
	}
}

type Addr = u32;
type Interval = Span<usize>;

#[derive(Debug)]
struct Allocator<Reg> {
	active: Vec<Interval>,
	registers: HashMap<Interval, Reg>,
	locations: HashMap<Interval, Addr>,
	pool: Vec<Reg>,
	reg_max: usize,
}

impl<Reg: Copy> Allocator<Reg> {
	fn new(pool: &[Reg]) -> Self {
		Self {
			active: Vec::default(),
			registers: HashMap::default(),
			locations: HashMap::default(),
			reg_max: pool.len(),
			pool: pool.iter().rev().copied().collect(),
		}
	}

	fn eval(&mut self, mut intervals: Vec<Interval>) {
		intervals.sort_by(|a,b| a.start.cmp(&b.start));

		for interval_i in intervals {
			self.expire_old_intervals(&interval_i);

			if self.active.len() == self.reg_max {
				self.spill(&interval_i);
			} else {
				self.registers.insert(
					interval_i.clone(),
					self.pool.pop()
							.unwrap(),
				);

				self.active.push(interval_i.clone());
				self.active.sort_by(|a,b| a.end.cmp(&b.end));
			}
		}
	}

	fn expire_old_intervals(&mut self, interval_i: &Interval) {
		let mut split_idx = 0;

		for interval_j in &self.active {
			if interval_j.end >= interval_i.end {
				break;
			}

			split_idx += 1;
			self.pool.push(self.registers[&interval_j]);
		}

		let (_, active) = self.active.split_at(split_idx);
		self.active = active.to_vec();
	}

	fn spill(&mut self, interval_i: &Interval) {
		let spill = self.active.pop()
				.unwrap();

		if spill.end > interval_i.end {
			self.registers.insert(
				interval_i.clone(),
				self.registers[&spill],
			);

			let stack_loc = self.new_loc();
			self.locations.insert(spill, stack_loc);

			self.active.push(interval_i.clone());
			self.active.sort_by(|a,b| a.end.cmp(&b.end));
		} else {
			// `spill` is still active so put it back
			self.active.push(spill);

			let stack_loc = self.new_loc();
			self.locations.insert(interval_i.clone(), stack_loc);
		}
	}

	fn new_loc(&mut self) -> Addr {
		// TODO - srenshaw - Actually use the data stack
		0xDEAD_BEEF
	}
}
