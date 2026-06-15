
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

fn allocate<Reg: Copy>(
	pool: &[Reg],
	instructions: &[TAC],
	stack_addr: u32,
) -> (HashMap<VRegId, Reg>, HashMap<VRegId, Addr>) {
	let mut allocator = Allocator::new(pool, stack_addr);
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
	stack_addr: u32,
}

impl<Reg: Copy> Allocator<Reg> {
	fn new(pool: &[Reg], stack_addr: u32) -> Self {
		Self {
			active: Vec::default(),
			registers: HashMap::default(),
			locations: HashMap::default(),
			reg_max: pool.len(),
			pool: pool.iter().rev().copied().collect(),
			stack_addr,
		}
	}

	fn eval(&mut self, instructions: &[TAC]) -> (HashMap<VRegId, Reg>, HashMap<VRegId, Addr>) {
		fn update_interval(interval_map: &mut HashMap<VRegId, Interval>, vr: VRegId, idx: usize) {
			interval_map.entry(vr)
				.and_modify(|i| i.start = idx)
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

		for i in intervals {
			self.expire_old_intervals(&i);

			if self.active.len() == self.reg_max {
				self.spill(&i);
			} else {
				self.registers.insert(
					i.clone(),
					self.pool.pop().expect("no free registers remaining"),
				);

				self.active.push(i.clone());
				self.active.sort_by(|a,b| a.end.cmp(&b.end));
			}
		}

		let mut registers = HashMap::default();
		let mut spills = HashMap::default();
		for (vreg, interval) in interval_map {
			match self.registers.get(&interval) {
				Some(reg) => {
					registers.insert(vreg, *reg);
				}
				None => {
					spills.insert(vreg, self.locations[&interval]);
				}
			}
		}

		(registers, spills)
	}

	fn expire_old_intervals(&mut self, i: &Interval) {
		let mut split_idx = 0;

		for j in &self.active {
			if j.end > i.start {
				break;
			}

			split_idx += 1;
			self.pool.push(self.registers[&j]);
		}

		self.active = self.active[split_idx..].to_vec();
	}

	fn spill(&mut self, i: &Interval) {
		let Some(spill) = self.active.pop() else {
			let stack_loc = self.new_loc();
			self.locations.insert(i.clone(), stack_loc);
			return;
		};

		if spill.end > i.end {
			self.registers.insert(
				i.clone(),
				self.registers[&spill],
			);

			let stack_loc = self.new_loc();
			self.locations.insert(spill, stack_loc);

			self.active.push(i.clone());
			self.active.sort_by(|a,b| a.end.cmp(&b.end));
		} else {
			// `spill` is still active so put it back
			self.active.push(spill);

			let stack_loc = self.new_loc();
			self.locations.insert(i.clone(), stack_loc);
		}
	}

	fn new_loc(&mut self) -> Addr {
		self.stack_addr -= 4;
		self.stack_addr
	}
}

#[cfg(test)]
mod linear_scan {
	use std::collections::{HashMap, HashSet};
	use crate::operators::{BinaryOp, UnaryOp};
	use crate::parser::Type;
	use super::*;

	const STACK_ADDR: u32 = 100;

	#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
	enum MockReg { R0, R1, R2, R3 }

	const ALL_REGS: &[MockReg] = &[MockReg::R0, MockReg::R1, MockReg::R2, MockReg::R3];
	const TWO_REGS: &[MockReg] = &[MockReg::R0, MockReg::R1];
	const ONE_REG : &[MockReg] = &[MockReg::R0];

	type Alloc = (HashMap<VRegId, MockReg>, HashMap<VRegId, u32>);

	fn vreg(id: u32) -> VRegId { id }
	fn vrloc(id: u32) -> Location { Location::VReg(id, Type::S32) }
	fn immloc(v: i64) -> Location { Location::Const(v, Type::S32) }

	fn assert_complete_and_disjoint(result: &Alloc, vregs: &[VRegId]) {
		let (regs, spills) = result;
		for v in vregs {
			let in_reg = regs.contains_key(v);
			let in_spill = spills.contains_key(v);
			assert!(
				in_reg || in_spill,
				"VReg {v} is neither in a physical register nor spilled",
			);
			assert!(
				!(in_reg && in_spill),
				"VReg {v} appears in both register map and spill map",
			);
		}
	}

	fn assert_no_register_collision(regs: &HashMap<VRegId, MockReg>, pairs: &[(VRegId, VRegId)]) {
		for (a, b) in pairs {
			if let (Some(ra), Some(rb)) = (regs.get(a), regs.get(b)) {
				assert_ne!(
					ra, rb,
					"VRegs {a} and {b} are simultaneously live but share register {ra:?}",
				);
			}
		}
	}

	#[test]
	fn single_def_gets_register() {
		let vr0 = vreg(0);
		let instrs = [
			TAC::Move { src: immloc(42), dst: vrloc(0) },
			TAC::Return(Some(vr0)),
		];
		let (regs, spills) = allocate(ALL_REGS, &instrs, STACK_ADDR);
		assert!(regs.contains_key(&vr0), "vr0 should be in a physical register");
		assert!(!spills.contains_key(&vr0), "vr0 should not be spilled");
	}

	#[test]
	fn non_overlapping_ranges_may_reuse_register() {
		let vr0 = vreg(0);
		let vr1 = vreg(1);
		let instrs = [
			TAC::Move { src: immloc(1), dst: vrloc(0) },
			TAC::Return(Some(vr0)),
			TAC::Move { src: immloc(2), dst: vrloc(1) },
			TAC::Return(Some(vr1)),
		];
		let result = allocate(ONE_REG, &instrs, STACK_ADDR);
		assert_complete_and_disjoint(&result, &[vr0, vr1]);
		assert!(result.1.is_empty(), "no spills expected when ranges are disjoint");
	}

	#[test]
	fn overlapping_ranges_get_distinct_registers() {
		let vr0 = vreg(0);
		let vr1 = vreg(1);
		let instrs = [
			TAC::Move { src: immloc(1), dst: vrloc(0) },
			TAC::Move { src: immloc(2), dst: vrloc(1) },
			TAC::BinOp { op: BinaryOp::Add, lhs: vrloc(0), rhs: vrloc(1), dst: vrloc(1) },
			TAC::Return(Some(vr1)),
		];
		let (regs, spills) = allocate(TWO_REGS, &instrs, STACK_ADDR);
		assert!(regs.contains_key(&vr0));
		assert!(regs.contains_key(&vr1));
		assert!(spills.is_empty(), "no spills expected with 2 regs for 2 live vregs");
		assert_no_register_collision(&regs, &[(vr0, vr1)]);
	}

	#[test]
	fn spill_when_pressure_exceeds_pool() {
		let instrs = [
			TAC::Move { src: immloc(1), dst: vrloc(0) },
			TAC::Move { src: immloc(2), dst: vrloc(1) },
			TAC::Move { src: immloc(3), dst: vrloc(2) },
			TAC::BinOp { op: BinaryOp::Add, lhs: vrloc(0), rhs: vrloc(1), dst: vrloc(1) },
			TAC::BinOp { op: BinaryOp::Add, lhs: vrloc(1), rhs: vrloc(2), dst: vrloc(2) },
			TAC::Return(Some(vreg(2))),
		];
		let result = allocate(TWO_REGS, &instrs, STACK_ADDR);
		let (regs, spills) = &result;
		assert_eq!(spills.len(), 1, "3 vregs requires a single spill");
		assert_eq!(
			regs.len() + spills.len(), 3,
			"all 3 vregs must be accounted for",
		);
		assert!(!spills.is_empty(), "at least one vreg must be spilled");
		assert_complete_and_disjoint(&result, &[vreg(0), vreg(1), vreg(2)]);
	}

	#[test]
	fn spill_slots_are_unique() {
		let instrs = [
			TAC::Move { src: immloc(10), dst: vrloc(0) },
			TAC::Move { src: immloc(20), dst: vrloc(1) },
			TAC::Move { src: immloc(30), dst: vrloc(2) },
			TAC::BinOp { op: BinaryOp::Add, lhs: vrloc(0), rhs: vrloc(1), dst: vrloc(2) },
			TAC::Return(Some(vreg(2))),
		];
		let (_, spills) = allocate(ONE_REG, &instrs, STACK_ADDR);
		let mut slots: Vec<u32> = spills.values().copied().collect();
		slots.sort();
		slots.dedup();
		assert_eq!( slots.len(), spills.len(), "every spilled vreg must have a unique stack slot",
		);
	}

	#[test]
	fn empty_instructions_produce_empty_maps() {
		let (regs, spills) = allocate(ALL_REGS, &[], STACK_ADDR);
		assert!(regs.is_empty());
		assert!(spills.is_empty());
	}

	#[test]
	fn empty_register_pool_spills_everything() {
		let instrs = [
			TAC::Move { src: immloc(1), dst: vrloc(0) },
			TAC::Move { src: immloc(2), dst: vrloc(1) },
			TAC::Return(Some(vreg(0))),
		];
		let (regs, spills) = allocate(&[], &instrs, STACK_ADDR);
		assert!(regs.is_empty(), "no physical registers available");
		assert_eq!(spills.len(), 2, "2 vregs requires 2 spills");
		assert_complete_and_disjoint(&(regs, spills), &[vreg(0), vreg(1)]);
	}

	#[test]
	fn unop_chain_allocates_correctly() {
		let instrs = [
			TAC::Move { src: immloc(5), dst: vrloc(0) },
			TAC::UnOp { op: UnaryOp::Neg, rhs: vrloc(0), dst: vrloc(1) },
			TAC::Return(Some(vreg(1))),
		];
		let result = allocate(TWO_REGS, &instrs, STACK_ADDR);
		assert_complete_and_disjoint(&result, &[vreg(0), vreg(1)]);
		assert!(result.1.is_empty(), "2 regs sufficient for 2 vregs");
	}

	#[test]
	fn jumpif_keeps_vreg_live() {
		let instrs = [
			TAC::Move { src: immloc(1), dst: vrloc(0) },
			TAC::Move { src: immloc(99), dst: vrloc(1) },
			TAC::JumpIf { lbl: 1, vr: vreg(0) },
			TAC::Return(Some(vreg(1))),
		];
		let result = allocate(TWO_REGS, &instrs, STACK_ADDR);
		assert_eq!(result.0.len(), 2);
		assert_complete_and_disjoint(&result, &[vreg(0), vreg(1)]);
	}

	#[test]
	fn long_live_range_not_evicted_prematurely() {
		let instrs = [
			TAC::Move { src: immloc(1),  dst: vrloc(0) },
			TAC::Move { src: immloc(10), dst: vrloc(1) },
			TAC::Move { src: immloc(20), dst: vrloc(2) },
			TAC::Move { src: immloc(30), dst: vrloc(3) },
			TAC::BinOp { op: BinaryOp::Add, lhs: vrloc(0), rhs: immloc(0), dst: vrloc(4) },
			TAC::Return(Some(vreg(4))),
		];
		let result = allocate(ALL_REGS, &instrs, STACK_ADDR);
		let (regs, spills) = &result;
		assert!(
			regs.contains_key(&vreg(0)) || spills.contains_key(&vreg(0)),
			"vr0 must be tracked across its entire live range",
		);
		assert_complete_and_disjoint(&result, &[vreg(0), vreg(1), vreg(2), vreg(3), vreg(4)]);
	}

	#[test]
	fn void_return_allocates_defs_only() {
		let instrs = [
			TAC::Move { src: immloc(0), dst: vrloc(0) },
			TAC::Return(None),
		];
		let (regs, spills) = allocate(ALL_REGS, &instrs, STACK_ADDR);
		assert_complete_and_disjoint(&(regs, spills), &[vreg(0)]);
	}

	#[test]
	fn fills_all_registers_before_spilling() {
		let instrs = [
			TAC::Move { src: immloc(1), dst: vrloc(0) },
			TAC::Move { src: immloc(2), dst: vrloc(1) },
			TAC::Move { src: immloc(3), dst: vrloc(2) },
			TAC::Move { src: immloc(4), dst: vrloc(3) },
			TAC::BinOp { op: BinaryOp::Add, lhs: vrloc(0), rhs: vrloc(1), dst: vrloc(4) },
			TAC::BinOp { op: BinaryOp::Add, lhs: vrloc(2), rhs: vrloc(3), dst: vrloc(5) },
			TAC::BinOp { op: BinaryOp::Add, lhs: vrloc(4), rhs: vrloc(5), dst: vrloc(6) },
			TAC::Return(Some(vreg(6))),
		];
		let (regs, spills) = allocate(ALL_REGS, &instrs, STACK_ADDR);
		assert!(
			spills.is_empty(),
			"should not spill when register count equals pressure",
		);
		let used: HashSet<_> = regs.values().collect();
		assert_eq!(used.len(), ALL_REGS.len(), "all 4 physical registers should be used");
	}
}

