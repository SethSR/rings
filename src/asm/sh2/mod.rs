
use std::collections::HashMap;

use crate::operators::{BinaryOp, UnaryOp};
use crate::tac::{Data as TacData, Location, TAC};
use crate::parser::Type;

use super::{BasicToAsmConverter, Block, LabelGenerator};

mod ins;

#[cfg(test)] mod interpreter;
#[cfg(test)] mod tests;

use ins::{Ins, Reg};
pub use ins::Asm;

fn build_constant(data: &mut Vec<Asm>, typ: Type, c: i64) {
	match typ {
		Type::S8 => {
			data.push(Asm::Ins(Ins::MovI(c as i8, R0)));
		}
		Type::U8 => {
			data.push(Asm::Ins(Ins::MovI(c as i8, R0)));
			data.push(Asm::Ins(Ins::ExtUB(R0, R0)));
		}
		Type::S16 => {
			if (i8::MAX as i64..u8::MAX as i64).contains(&c) {
				data.push(Asm::Ins(Ins::MovI(c as i8, R0)));
				data.push(Asm::Ins(Ins::ExtUB(R0, R0)));
			} else if (i8::MIN as i64..i8::MAX as i64).contains(&c) {
				data.push(Asm::Ins(Ins::MovI(c as i8, R0)));
			} else {
				data.push(Asm::MovWI(c as i16, R0));
			}
		}
		Type::U16 => {
			if (0..u8::MAX as i64).contains(&c) {
				data.push(Asm::Ins(Ins::MovI(c as i8, R0)));
				data.push(Asm::Ins(Ins::ExtUB(R0, R0)));
			} else {
				data.push(Asm::MovWI(c as i16, R0));
				data.push(Asm::Ins(Ins::ExtUW(R0, R0)));
			}
		}
		Type::S32 | Type::U32 => {
			if (i8::MAX as i64..u8::MAX as i64).contains(&c) {
				data.push(Asm::Ins(Ins::MovI(c as i8, R0)));
				data.push(Asm::Ins(Ins::ExtUB(R0, R0)));
			} else if (i8::MIN as i64..i8::MAX as i64).contains(&c) {
				data.push(Asm::Ins(Ins::MovI(c as i8, R0)));
			} else if (i16::MAX as i64..u16::MAX as i64).contains(&c) {
				data.push(Asm::MovWI(c as i16, R0));
				data.push(Asm::Ins(Ins::ExtUW(R0, R0)));
			} else if (i16::MIN as i64..i16::MAX as i64).contains(&c) {
				data.push(Asm::MovWI(c as i16, R0));
				if typ == Type::U32 {
					data.push(Asm::Ins(Ins::ExtUW(R0, R0)));
				}
			} else {
				data.push(Asm::MovLI(c as i32, R0));
			}
		}
		_ => unreachable!("cannot build immediates from non-integer types"),
	}
}

/// Utility and return register
const R0: Reg = 0;
/// Call stack address
//const FP: Reg = 14;
/// Data stack address
const SP: Reg = 15;

const TL: Reg = 10;
const TR: Reg = 11;
const XL: Reg = 12;
const XR: Reg = 13;

pub fn lower(
	proc_name: &str,
	tac_data: TacData,
	stack_addr: u32,
	ret_type: Type,
) -> (Vec<Asm>, Vec<Block>) {
	let TacData {
		instructions,
		blocks,
		curr_label,
		..
	} = tac_data;

	let mut block_converter = BasicToAsmConverter::new(blocks);

	let registers = super::allocate(
		&[ 1, 2, 3, 4, 5, 6, 7, 8, 9 ],
		&instructions,
	);

	let mut lbl_gen = LabelGenerator(curr_label);

	let mut data = vec![];

	if proc_name == "main" {
		data.push(Asm::Comment("Setup stack pointer".into()));
		build_constant(&mut data, Type::U32, stack_addr as i64);
		data.push(Asm::Ins(Ins::Mov(R0, SP)));
	}

	for (idx, tac) in instructions.iter().enumerate() {
		block_converter.check(idx, data.len());

		match tac {
			TAC::Move { src, dst } => {
				data.push(Asm::Comment(format!("Move {src:?} -> {dst:?}")));

				move_src_to_target(&mut data, &registers, src, TL);
				move_target_to_dst(&mut data, &registers, TL, dst);
			}

			TAC::Return(with_value) => {
				data.push(Asm::Comment(format!("Return {with_value:?}")));

				data.push(Asm::Ins(Ins::Rts));
				if let Some(vr) = with_value {
					// Return through R0
					data.push(Asm::Ins(Ins::Mov(registers[vr], R0)));
				} else {
					data.push(Asm::Ins(Ins::Nop));
				}
			}

			TAC::Jump(lbl) => {
				let label = format!("{proc_name}_{lbl}");
				data.push(Asm::Comment(format!("Jump {label}")));

				data.push(Asm::Bra(label));
				data.push(Asm::Ins(Ins::Nop));
			}

			TAC::JumpIf { lbl, vr } => {
				let label = format!("{proc_name}_{lbl}");
				data.push(Asm::Comment(format!("JumpIf VReg({vr},{ret_type:?}) > 0 -> {label}")));

				data.push(Asm::Ins(Ins::CmpPL(registers[vr])));
				data.push(Asm::BT(label));
				data.push(Asm::Ins(Ins::Nop));
			}

			TAC::UnOp { op, rhs, dst } => {
				data.push(Asm::Comment(format!("UnOp {op} {rhs:?} -> {dst:?}")));

				move_src_to_target(&mut data, &registers, rhs, TL);
				match op {
					UnaryOp::Not => data.push(Asm::Ins(Ins::Not(TL, TL))),
					UnaryOp::Neg => data.push(Asm::Ins(Ins::Neg(TL, TL))),
				}
				move_target_to_dst(&mut data, &registers, TL, dst);
			}

			TAC::BinOp { op, lhs, rhs, dst } => {
				data.push(Asm::Comment(format!("BinOp {lhs:?} {op} {rhs:?} -> {dst:?}")));

				let ltyp = move_src_to_target(&mut data, &registers, lhs, TL);
				let rtyp = move_src_to_target(&mut data, &registers, rhs, TR);

				match op {
					BinaryOp::Add => data.push(Asm::Ins(Ins::Add(TR, TL))),
					BinaryOp::Sub => data.push(Asm::Ins(Ins::Sub(TR, TL))),

					BinaryOp::BinAnd => data.push(Asm::Ins(Ins::And(TR, TL))),
					BinaryOp::BinOr => data.push(Asm::Ins(Ins::Or(TR, TL))),
					BinaryOp::BinXor => data.push(Asm::Ins(Ins::Xor(TR, TL))),

					BinaryOp::CmpEQ => {
						data.push(Asm::Ins(Ins::CmpEQ(TR, TL)));
						data.push(Asm::Ins(Ins::MovT(TL)));
					}
					BinaryOp::CmpGE => {
						data.push(Asm::Ins(Ins::CmpGE(TR, TL)));
						data.push(Asm::Ins(Ins::MovT(TL)));
					}
					BinaryOp::CmpGT => {
						data.push(Asm::Ins(Ins::CmpGT(TR, TL)));
						data.push(Asm::Ins(Ins::MovT(TL)));
					}
					BinaryOp::CmpLE => {
						data.push(Asm::Ins(Ins::CmpGT(TL, TR)));
						data.push(Asm::Ins(Ins::MovT(TL)));
					}
					BinaryOp::CmpLT => {
						data.push(Asm::Ins(Ins::CmpGE(TL, TR)));
						data.push(Asm::Ins(Ins::MovT(TL)));
					}
					BinaryOp::CmpNE => {
						data.push(Asm::Ins(Ins::CmpEQ(TR, TL)));
						data.push(Asm::Ins(Ins::MovT(TL)));
						data.push(Asm::Ins(Ins::AddI(-1, TL)));
						data.push(Asm::Ins(Ins::Neg(TL, TL)));
					}

					BinaryOp::LogAnd => {
						data.push(Asm::Ins(Ins::CmpPL(TL)));
						data.push(Asm::Ins(Ins::MovT(TL)));
						data.push(Asm::Ins(Ins::CmpPL(TR)));
						data.push(Asm::Ins(Ins::MovT(TR)));
						data.push(Asm::Ins(Ins::And(TR, TL)));
					}
					BinaryOp::LogOr => {
						data.push(Asm::Ins(Ins::CmpPL(TL)));
						data.push(Asm::Ins(Ins::MovT(TL)));
						data.push(Asm::Ins(Ins::CmpPL(TR)));
						data.push(Asm::Ins(Ins::MovT(TR)));
						data.push(Asm::Ins(Ins::Or(TR, TL)));
					}
					BinaryOp::LogXor => {
						data.push(Asm::Ins(Ins::CmpPL(TL)));
						data.push(Asm::Ins(Ins::MovT(TL)));
						data.push(Asm::Ins(Ins::CmpPL(TR)));
						data.push(Asm::Ins(Ins::MovT(TR)));
						data.push(Asm::Ins(Ins::Xor(TR, TL)));
					}

					BinaryOp::Mul => {
						data.push(Asm::Ins(Ins::MulL(TR, TL)));
						data.push(Asm::Ins(Ins::StMacl(TL)));
					}

					BinaryOp::Div | BinaryOp::Mod => {
						// DIV/MOD is expensive, so shouldn't happen in a loop and may be a decent location for
						// data-pool table generation.
						data.push(Asm::Table);

						let is_signed = ltyp.is_signed_integer();
						match rtyp {
							Type::S8 => {
								data.push(Asm::Ins(Ins::ExtSB(TR, TR)));
								data.push(Asm::Ins(Ins::ShLL16(TR)));
								data.append(&mut setup_div16(TR, TL, XR, is_signed));
							}
							Type::U8 => {
								data.push(Asm::Ins(Ins::ExtUB(TR, TR)));
								data.push(Asm::Ins(Ins::ShLL16(TR)));
								data.append(&mut setup_div16(TR, TL, XR, is_signed));
							}
							Type::S16 => {
								data.push(Asm::Ins(Ins::ExtSW(TR, TR)));
								data.push(Asm::Ins(Ins::ShLL16(TR)));
								data.append(&mut setup_div16(TR, TL, XR, is_signed));
							}
							Type::U16 => {
								data.push(Asm::Ins(Ins::ExtUW(TR, TR)));
								data.push(Asm::Ins(Ins::ShLL16(TR)));
								data.append(&mut setup_div16(TR, TL, XR, is_signed));
							}
							Type::S32 | Type::U32 => {
								data.append(&mut setup_div32(TR, TL, XR, XL, is_signed));
							}
							_ => unreachable!(),
						}

						if *op == BinaryOp::Div {
						} else {
							// TODO - srenshaw - Finish converting quotient, dividend, and divisor into remainder.
						}
					}

					BinaryOp::ShL => {
						let loop_label = lbl_gen.next(proc_name);
						data.push(Asm::Ins(Ins::Bra(1)));
						data.push(Asm::Ins(Ins::Nop));
						data.push(Asm::Label(loop_label.clone()));
						if ltyp.is_signed_integer() {
							data.push(Asm::Ins(Ins::ShAL(TL)));
						} else {
							data.push(Asm::Ins(Ins::ShLL(TL)));
						}
						data.push(Asm::Ins(Ins::DT(TR)));
						data.push(Asm::BF(loop_label));
					}

					BinaryOp::ShR => {
						let loop_label = lbl_gen.next(proc_name);
						data.push(Asm::Ins(Ins::Bra(1)));
						data.push(Asm::Ins(Ins::Nop));
						data.push(Asm::Label(loop_label.clone()));
						if ltyp.is_signed_integer() {
							data.push(Asm::Ins(Ins::ShAR(TL)));
						} else {
							data.push(Asm::Ins(Ins::ShLR(TL)));
						}
						data.push(Asm::Ins(Ins::DT(TR)));
						data.push(Asm::BF(loop_label));
					}
				}

				move_target_to_dst(&mut data, &registers, TL, dst);
			}

			#[cfg(feature="call")]
			Vsmc::Call { name, args, dst } => {
				todo!("call convention is not implemented yet: {}({}){}",
					db.text(name),
					args.iter().map(|loc| match loc {
						Location::Constant(c) => c.to_string(),
						Location::Temp(t) => format!("?{t}"),
						Location::Variable(v) => db.text(&v).to_string(),
					}).collect::<Vec<_>>().join(", "),
					dst.map(|x| format!(" -> {x}")).unwrap_or("".to_string()),
				);
			}
		}
	}

	block_converter.finish(data.len());

	let output = add_data_pools(&data);

	eprintln!("-- ASM Debug --");
	for (i,asm) in output.iter().enumerate() {
		eprintln!("[{i:3}] :{asm}");
	}
	eprintln!("-- ASM Debug --");

	(output, block_converter.asm_blocks)
}

fn setup_div16(
	m: u8, n: u8,
	x0: u8,
	is_signed: bool,
) -> Vec<Asm> {
	let mut output = vec![];

	output.push(Asm::Ins(Ins::ShLL16(m)));

	if is_signed {
		output.push(Asm::Ins(Ins::ExtSW(n, n)));
		output.push(Asm::Ins(Ins::Mov(n, x0)));
		output.push(Asm::Ins(Ins::RotCL(x0)));
		output.push(Asm::Ins(Ins::Xor(x0, x0)));
		output.push(Asm::Ins(Ins::SubC(x0, n)));

		output.push(Asm::Ins(Ins::Div0S(m, n)));
	} else {
		output.push(Asm::Ins(Ins::Div0U));
	}

	for _ in 0..16 {
		output.push(Asm::Ins(Ins::Div1(m, n)));
	}

	if is_signed {
		output.push(Asm::Ins(Ins::ExtSW(n, n)));
	}

	output.push(Asm::Ins(Ins::RotCL(n)));

	if is_signed {
		output.push(Asm::Ins(Ins::AddC(x0, n)));
		output.push(Asm::Ins(Ins::ExtSW(n, n)));
	} else {
		output.push(Asm::Ins(Ins::ExtUW(n, n)));
	}

	output
}

fn setup_div32(
	m: u8, n: u8,
	x0: u8, x1: u8,
	is_signed: bool,
) -> Vec<Asm> {
	let mut output = vec![];

	if is_signed {
		output.push(Asm::Ins(Ins::Mov(n, x1)));
		output.push(Asm::Ins(Ins::RotCL(x1)));
		output.push(Asm::Ins(Ins::SubC(x0, x0)));
		output.push(Asm::Ins(Ins::Xor(x1, x1)));
		output.push(Asm::Ins(Ins::SubC(x1, n)));
	}

	if is_signed {
		output.push(Asm::Ins(Ins::Div0S(m, x0)));
	} else {
		output.push(Asm::Ins(Ins::Div0U));
	}

	for _ in 0..32 {
		output.push(Asm::Ins(Ins::RotCL(n)));
		output.push(Asm::Ins(Ins::Div1(m, x0)));
	}

	output.push(Asm::Ins(Ins::RotCL(n)));

	if is_signed {
		output.push(Asm::Ins(Ins::AddC(x1, n)));
	}

	output
}

fn add_data_pools(data: &[Asm]) -> Vec<Asm> {
	let mut data_pool = DataPool::default();
	let mut output = vec![];
	let mut idx = 0;
	let mut jump_asm = false;
	for (asm_idx, asm) in data.iter().enumerate() {
		match asm {
			Asm::MovWI(s,_) => {
				output.push(asm.clone());
				data_pool.insert_word(asm_idx, output.len() - 1, *s as u16);
				if data_pool.should_create_table(idx) {
					idx += data_pool.create_table(&mut output, true);
				}
				idx += 1;
			}
			Asm::MovLI(s,_) => {
				output.push(asm.clone());
				data_pool.insert_long(asm_idx, output.len() - 1, *s as u32);
				if data_pool.should_create_table(idx) {
					idx += data_pool.create_table(&mut output, true);
				}
				idx += 1;
			}
			Asm::Ins(Ins::Bra(_)) | Asm::Ins(Ins::BraF(_)) | Asm::Ins(Ins::Jmp(_)) | Asm::Ins(Ins::Rts) => {
				output.push(asm.clone());
				jump_asm = true;
				idx += 1;
			}
			Asm::Table => {
				idx += data_pool.create_table(&mut output, true);
			}
			Asm::Label(_) | Asm::Comment(_) => output.push(asm.clone()),
			_ => {
				output.push(asm.clone());
				if jump_asm {
					jump_asm = false;
					idx += data_pool.create_table(&mut output, false);
				}
				idx += 1;
			}
		}
	}

	// Empty the Data Pool
	data_pool.create_table(&mut output, false);

	output
}

#[derive(Debug, Default)]
struct DataPool {
	idx_to_words: HashMap<usize,(usize,usize)>,
	idx_to_longs: HashMap<usize,(usize,usize)>,
	words: Vec<u16>,
	longs: Vec<u32>,
}

impl DataPool {
	fn insert_word(&mut self, idx: usize, pos: usize, word: u16) {
		let word_idx = if let Some(word_idx) = self.words.iter()
				.position(|x| *x == word)
		{
			word_idx
		} else {
			self.words.push(word);
			self.words.len() - 1
		};
		self.idx_to_words.insert(idx, (pos, word_idx));
	}

	fn insert_long(&mut self, idx: usize, pos: usize, long: u32) {
		let long_idx = if let Some(long_idx) = self.longs.iter()
				.position(|x| *x == long)
		{
			long_idx
		} else {
			self.longs.push(long);
			self.longs.len() - 1
		};
		self.idx_to_longs.insert(idx, (pos, long_idx));
	}

	fn should_create_table(&self, idx: usize) -> bool {
		let num_buffer_bytes = 4;
		let words_too_far = self.idx_to_words.iter()
				.any(|(i,_)| 255 + num_buffer_bytes + i < (idx + self.words.len()));
		let word_pad = self.words.len() & 1;
		let longs_too_far = self.idx_to_longs.iter()
				.any(|(i,_)| 510 + num_buffer_bytes + i < (idx + self.words.len() + word_pad + self.longs.len()));
		words_too_far || longs_too_far
	}

	/// Generates a Literal-Pool for the given instruction stream
	///
	/// - returns the count of new 16-bit words
	fn create_table(&mut self, output: &mut Vec<Asm>, needs_branch: bool) -> usize {
		let start = output.len();
		let word_pad = (self.words.len() + start) & 1;

		if needs_branch {
			let total = self.words.len() + word_pad + self.longs.len() * 2;
			debug_assert!(total <= 0xFFF);
			output.push(Asm::Ins(Ins::Bra(total as i16)));
			output.push(Asm::Ins(Ins::Nop));
		}

		for (asm_idx, (pos, word_idx)) in self.idx_to_words.drain() {
			if let Asm::MovWI(_,r) = output[pos] {
				let offset = output.len() + word_idx - asm_idx;
				output[pos] = Asm::Ins(Ins::MovWI(offset as u8, r));
			} else {
				eprintln!("non-MovWI")
			}
		}

		let base = output.len() + self.words.len() + word_pad;
		for (asm_idx, (pos, long_idx)) in self.idx_to_longs.drain() {
			if let Asm::MovLI(_,r) = output[pos] {
				let offset = base + long_idx - asm_idx;
				output[pos] = Asm::Ins(Ins::MovLI((offset >> 1) as u8, r));
			} else {
				eprintln!("non-MovL")
			}
		}

		for word in self.words.drain(..) {
			output.push(Asm::Ins(Ins::Word(word)));
		}
		if word_pad > 0 {
			output.push(Asm::Ins(Ins::Nop));
		}
		for long in self.longs.drain(..) {
			output.push(Asm::Ins(Ins::Word((long >> 16) as u16)));
			output.push(Asm::Ins(Ins::Word(long as u16)));
		}

		output.len() - start
	}
}

fn extend_register(data: &mut Vec<Asm>, reg: Reg, typ: Type) {
	match typ {
		Type::Bool => {}
		Type::S8 => data.push(Asm::Ins(Ins::ExtSB(reg, reg))),
		Type::U8 => data.push(Asm::Ins(Ins::ExtUB(reg, reg))),
		Type::S16 => data.push(Asm::Ins(Ins::ExtSW(reg, reg))),
		Type::U16 => data.push(Asm::Ins(Ins::ExtUW(reg, reg))),
		Type::S32 | Type::U32 => {}
		_ => unreachable!("{typ:?}"),
	}
}

/// This will rewrite R0 for address, constant, and stack locations.
fn move_src_to_target(
	data: &mut Vec<Asm>,
	registers: &HashMap<u32,Reg>,
	loc: &Location,
	target: Reg,
) -> Type {
	match loc {
		Location::Addr(adr,typ) => {
			build_constant(data, Type::U32, *adr as i64);
			match typ {
				Type::S8 | Type::U8 => data.push(Asm::Ins(Ins::MovBL(R0, target))),
				Type::S16 | Type::U16 => data.push(Asm::Ins(Ins::MovWL(R0, target))),
				Type::S32 | Type::U32 => data.push(Asm::Ins(Ins::MovLL(R0, target))),
				_ => unreachable!(),
			}
			*typ
		}
		Location::Const(val,typ) => {
			build_constant(data, *typ, *val);
			data.push(Asm::Ins(Ins::Mov(R0, target)));
			*typ
		}
		Location::Stack(idx,typ) => {
			build_constant(data, Type::U8, *idx as i64);
			data.push(Asm::Ins(Ins::ShLL2(R0)));
			match typ {
				Type::S8 | Type::U8 => data.push(Asm::Ins(Ins::MovBL0(SP, target))),
				Type::S16 | Type::U16 => data.push(Asm::Ins(Ins::MovWL0(SP, target))),
				Type::S32 | Type::U32 => data.push(Asm::Ins(Ins::MovLL0(SP, target))),
				_ => unreachable!(),
			}
			*typ
		}
		Location::VReg(vr,typ) => {
			data.push(Asm::Ins(Ins::Mov(registers[vr], target)));
			extend_register(data, target, *typ);
			*typ
		}
	}
}

/// This will rewrite R0 for address and stack locations.
fn move_target_to_dst(
	data: &mut Vec<Asm>,
	registers: &HashMap<u32,Reg>,
	target: Reg,
	loc: &Location,
) -> Type {
	match loc {
		Location::Addr(adr,typ) => {
			build_constant(data, Type::U32, *adr as i64);
			match typ {
				Type::S8 | Type::U8 => data.push(Asm::Ins(Ins::MovBS(target, R0))),
				Type::S16 | Type::U16 => data.push(Asm::Ins(Ins::MovWS(target, R0))),
				Type::S32 | Type::U32 => data.push(Asm::Ins(Ins::MovLS(target, R0))),
				_ => unreachable!(),
			}
			*typ
		}
		Location::Const(..) => panic!("cannot have a constant as a destination"),
		Location::Stack(idx,typ) => {
			build_constant(data, Type::U8, *idx as i64);
			data.push(Asm::Ins(Ins::ShLL2(R0)));
			match typ {
				Type::S8 | Type::U8 => data.push(Asm::Ins(Ins::MovBS0(target, SP))),
				Type::S16 | Type::U16 => data.push(Asm::Ins(Ins::MovWS0(target, SP))),
				Type::S32 | Type::U32 => data.push(Asm::Ins(Ins::MovLS0(target, SP))),
				_ => unreachable!(),
			}
			*typ
		}
		Location::VReg(vr,typ) => {
			let r = registers[vr];
			data.push(Asm::Ins(Ins::Mov(target, r)));
			extend_register(data, r, *typ);
			*typ
		}
	}
}
