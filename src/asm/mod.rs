
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result};

use crate::identifier::Map as IdentMap;
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::parser::Data as PrsData;
use crate::tac::{LabelId, Data as TacData, VRegId, TAC, Location};
use crate::{Span, SrcPos, Target};

mod m68k;
mod sh2;
mod x86_64;
//mod z80;

pub fn eval(
	input: &InputData,
	lex_data: &LexData,
	prs_data: &PrsData<SrcPos>,
	tac_data: IdentMap<TacData>,
	stack_addr: u32,
) -> IdentMap<Data> {
	let mut out = IdentMap::<Data>::default();

	for (proc_id, section) in tac_data {
		let proc_name = lex_data.text(input, &proc_id).to_owned();
		let ret_type = prs_data.procedures[&proc_id].ret_type;

		let data = match section.target {
			Target::M68k => Data::M68k(m68k::lower(&proc_name, section, stack_addr)),
			Target::SH2 => Data::SH2(sh2::lower(&proc_name, section, stack_addr, ret_type)),
			Target::X86_64 => Data::X86(x86_64::lower(&proc_name, section, stack_addr)),
			//Target::Z80 => Data::Z80(z80::lower(&proc_name, section, ret_type)),
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
	M68k((Vec<m68k::Asm>, Vec<Block>)),
	SH2((Vec<sh2::Asm>, Vec<Block>)),
	X86((Vec<x86_64::Asm>, Vec<Block>)),
	//Z80(Vec<z80::Asm>),
}

impl Display for Data {
	fn fmt(&self, f: &mut Formatter) -> Result {
		let mut out = vec![];
		match self {
			Self::M68k((data, blocks)) => {
				for (idx, block) in blocks.iter().enumerate() {
					out.push(format!("_{idx}: ; -> {:?}", block.next_blocks));
					for asm in &data[block.span.start..block.span.end] {
						out.push(asm.to_string());
					}
				}
			}
			Self::SH2((data, blocks)) => {
				for (idx, block) in blocks.iter().enumerate() {
					out.push(format!("_{idx}: ; -> {:?}", block.next_blocks));
					for asm in &data[block.span.start..block.span.end] {
						out.push(asm.to_string());
					}
				}
			}
			Self::X86((data, blocks)) => {
				for (idx, block) in blocks.iter().enumerate() {
					out.push(format!("_{idx}: ; -> {:?}", block.next_blocks));
					for asm in &data[block.span.start..block.span.end] {
						out.push(asm.to_string());
					}
				}
			}
			//Self::Z80(data) => out.extend(
				//data.iter().map(|asm| asm.to_string())
			//),
		}
		write!(f, "{}", out.join("\n"))
	}
}

#[derive(Debug, Clone)]
pub struct Block {
	span: Span<usize>,
	next_blocks: Vec<LabelId>,
}

struct BasicToAsmConverter {
	basic_blocks: HashMap<LabelId, crate::tac::BasicBlock>,
	block_idx: LabelId,
	block_start: usize,
	asm_blocks: Vec<Block>,
}
impl BasicToAsmConverter {
	fn new(basic_blocks: HashMap<LabelId, crate::tac::BasicBlock>) -> Self {
		Self {
			basic_blocks,
			block_idx: 0,
			block_start: 0,
			asm_blocks: vec![],
		}
	}

	fn check(&mut self, idx: usize, block_end: usize) {
		if let Some(block) = self.basic_blocks.get(&self.block_idx) {
			if idx == block.span.end {
				self.asm_blocks.push(Block {
					span: (self.block_start..block_end).into(),
					next_blocks: block.next_blocks.clone(),
				});
				self.block_start = block_end;
				self.block_idx += 1;
			}
		}
	}

	fn finish(&mut self, block_end: usize) {
		if let Some(block) = self.basic_blocks.get(&self.block_idx) {
			self.asm_blocks.push(Block {
				span: (self.block_start..block_end).into(),
				next_blocks: block.next_blocks.clone(),
			});
		}
	}
}

struct LabelGenerator(LabelId);
impl LabelGenerator {
	fn next(&mut self, name: &str) -> String {
		self.0 += 1;
		format!("{name}_{}", self.0 - 1)
	}
}

fn allocate<Reg: Copy>(pool: &[Reg], instructions: &[TAC]) -> HashMap<u32, Reg> {
	let mut allocator = Allocator::new(pool);
	allocator.eval(instructions)
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

	fn eval(&mut self, instructions: &[TAC]) -> HashMap<u32, Reg> {
		fn update_interval(interval_map: &mut HashMap<VRegId, Interval>, vr: VRegId, idx: usize) {
			interval_map.entry(vr)
					.and_modify(|interval| interval.start = idx)
					.or_insert(Span::point(idx));
		}

		let mut interval_map = HashMap::default();
		for (idx, tac) in instructions.iter().enumerate().rev() {
			match tac {
				TAC::Move { src, dst } => {
					if let Location::VReg(vr,_) = src {
						update_interval(&mut interval_map, *vr, idx);
					}
					if let Location::VReg(vr,_) = dst {
						update_interval(&mut interval_map, *vr, idx);
					}
				}
				TAC::Return(Some(vr)) => {
					update_interval(&mut interval_map, *vr, idx);
				}
				TAC::UnOp { rhs, dst, ..} => {
					if let Location::VReg(vr, _) = rhs {
						update_interval(&mut interval_map, *vr, idx);
					}
					if let Location::VReg(vr, _) = dst {
						update_interval(&mut interval_map, *vr, idx);
					}
				}
				TAC::BinOp { lhs, rhs, dst, ..} => {
					if let Location::VReg(vr,_) = lhs {
						update_interval(&mut interval_map, *vr, idx);
					}
					if let Location::VReg(vr,_) = rhs {
						update_interval(&mut interval_map, *vr, idx);
					}
					if let Location::VReg(vr,_) = dst {
						update_interval(&mut interval_map, *vr, idx);
					}
				}
				_ => {}
			}
		}

		let mut intervals: Vec<Interval> = interval_map.values().cloned().collect();
		intervals.sort_by(|a, b| a.start.cmp(&b.start));

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

		interval_map.into_iter()
				.map(|(vreg, interval)| (vreg, self.registers[&interval]))
				.collect()
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
