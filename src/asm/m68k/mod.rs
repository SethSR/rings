
use std::fmt::{Display, Formatter, Result};

use crate::operators::{BinaryOp, UnaryOp};
use crate::parser::Type;
use crate::tac::{Data as TacData, Location, TAC};

use super::{BasicToAsmConverter, Block, LabelId};

#[cfg(test)] mod interpreter;
#[cfg(test)] mod tests;

type VRegMap = std::collections::HashMap<u32, Data>;

fn get_arithmetic_size(typ: &Type) -> Sz {
	match typ {
		Type::S8 | Type::U8 => Sz::B,
		Type::S16 | Type::U16 => Sz::W,
		Type::S32 | Type::U32 => Sz::L,
		Type::Bool => Sz::B,
		_ => panic!("unexpected ADD type: {typ:?}"),
	}
}

// The base address for the variable stack
const VAR_SP: Addr = Addr::A6;
/// Left-hand side temporary register
const TL: Data = Data::D6;
/// Right-hand side temporary register
const TR: Data = Data::D7;

pub fn lower(proc_name: &str, tac_data: TacData, stack_addr: u32) -> (Vec<Asm>, Vec<Block>) {
	let TacData {
		instructions,
		blocks,
		..
	} = tac_data;

	let mut block_converter = BasicToAsmConverter::new(blocks);

	let registers = super::allocate(
		&[ Data::D0, Data::D1, Data::D2, Data::D3, Data::D4, Data::D5 ],
		&instructions,
		stack_addr,
	).0;

	let mut data = vec![
		Asm::Label(proc_name.to_string()),
	];

	if proc_name == "main" {
		data.push(Asm::Comment("Setup stack pointer".into()));
		data.push(Asm::Move(Sz::L, EA::Imm(stack_addr as i32), EA::Adr(VAR_SP)));
	}

	for (idx, tac) in instructions.iter().enumerate() {
		block_converter.check(idx, data.len());

		match tac {
			TAC::BinOp { op, lhs, rhs, dst } => {
				data.push(Asm::Comment(format!("{lhs:?} {op} {rhs:?} -> {dst:?}")));

				let (ea_lhs, lsz, is_lneg) = get_src_from_location(&registers, lhs);
				let (ea_rhs, rsz, is_rneg) = get_src_from_location(&registers, rhs);
				let (ea_dst, dsz, is_dneg) = get_dst_from_location(&registers, dst);

				match op {
					BinaryOp::Add => {
						let msz = lsz.max(rsz);

						data.push(Asm::Move(lsz, ea_lhs, EA::Dat(TL)));
						extend_register(lsz, msz, is_lneg, TL, &mut data);

						data.push(Asm::Move(rsz, ea_rhs, EA::Dat(TR)));
						extend_register(rsz, msz, is_rneg, TR, &mut data);

						data.push(Asm::Add1(msz, EA::Dat(TR), TL));
						extend_register(msz, dsz, is_dneg, TL, &mut data);

						data.push(Asm::Move(dsz, EA::Dat(TL), ea_dst));
					}

					BinaryOp::Sub => {
						let msz = lsz.max(rsz);

						data.push(Asm::Move(lsz, ea_lhs, EA::Dat(TL)));
						extend_register(lsz, msz, is_lneg, TL, &mut data);

						data.push(Asm::Move(rsz, ea_rhs, EA::Dat(TR)));
						extend_register(rsz, msz, is_rneg, TR, &mut data);

						data.push(Asm::Sub1(msz, EA::Dat(TR), TL));
						extend_register(msz, dsz, is_dneg, TL, &mut data);

						data.push(Asm::Move(dsz, EA::Dat(TL), ea_dst));
					}

					// 15*23
					// = (10 + 5)*(20 + 3)
					// = 10*(20 + 3) + 5*(20 + 3)
					// = 10*20 + 10*3 + 5*20 + 5*3
					// = 1*10*2*10 + 1*10*3 + 5*2*10 + 5*3
					// = (1*2)*100 + (1*3)*10 + (5*2)*10 + 5*3
					// = (1*2)*100 + (1*3 + 5*2)*10 + 5*3
					// -> (h1*h2)<<32 + (h1*l2 + l1*h2)<<16 + l1*l2
					BinaryOp::Mul => {
						debug_assert!(lsz < Sz::L);
						debug_assert!(rsz < Sz::L);

						data.push(Asm::Move(lsz, ea_lhs, EA::Dat(TL)));
						extend_register(lsz, Sz::W, is_lneg, TL, &mut data);

						data.push(Asm::Move(rsz, ea_rhs, EA::Dat(TR)));
						extend_register(rsz, Sz::W, is_rneg, TR, &mut data);

						if is_dneg {
							data.push(Asm::MulS(EA::Dat(TR), TL));
						} else {
							data.push(Asm::MulU(EA::Dat(TR), TL));
						}

						data.push(Asm::Move(dsz, EA::Dat(TL), ea_dst));
					}

					BinaryOp::Div | BinaryOp::Mod => {
						debug_assert!(rsz < Sz::L);
						debug_assert!(lsz <= dsz && rsz <= dsz);

						data.push(Asm::Move(lsz, ea_lhs, EA::Dat(TL)));
						extend_register(lsz, Sz::L, is_lneg, TL, &mut data);

						data.push(Asm::Move(rsz, ea_rhs, EA::Dat(TR)));
						extend_register(rsz, Sz::W, is_rneg, TR, &mut data);

						if is_dneg {
							data.push(Asm::DivS(EA::Dat(TR), TL));
						} else {
							data.push(Asm::DivU(EA::Dat(TR), TL));
						}

						// r2 = rem
						if *op == BinaryOp::Mod {
							data.push(Asm::Swap(TL));
						}
						extend_register(Sz::W, dsz, is_dneg, TL, &mut data);

						data.push(Asm::Move(dsz, EA::Dat(TL), ea_dst));
					}

					BinaryOp::ShL => {
						let msz = lsz.max(rsz);

						data.push(Asm::Move(lsz, ea_lhs, EA::Dat(TL)));
						extend_register(lsz, msz, is_lneg, TL, &mut data);

						data.push(Asm::Move(rsz, ea_rhs, EA::Dat(TR)));
						extend_register(rsz, msz, is_rneg, TR, &mut data);

						if is_dneg {
							data.push(Asm::Asl(msz, TR, TL));
						} else {
							data.push(Asm::Lsl(msz, TR, TL));
						}
						extend_register(msz, dsz, is_dneg, TL, &mut data);

						data.push(Asm::Move(dsz, EA::Dat(TL), ea_dst));
					}

					BinaryOp::ShR => {
						let msz = lsz.max(rsz);

						data.push(Asm::Move(lsz, ea_lhs, EA::Dat(TL)));
						extend_register(lsz, msz, is_lneg, TL, &mut data);

						data.push(Asm::Move(rsz, ea_rhs, EA::Dat(TR)));
						extend_register(rsz, msz, is_rneg, TR, &mut data);

						if is_dneg {
							data.push(Asm::Asr(msz, TR, TL));
						} else {
							data.push(Asm::Lsr(msz, TR, TL));
						}
						extend_register(msz, dsz, is_dneg, TL, &mut data);

						data.push(Asm::Move(dsz, EA::Dat(TL), ea_dst));
					}

					BinaryOp::BinAnd => output_binary_logic(&registers, lhs, rhs, dst, Asm::And2, &mut data),
					BinaryOp::BinOr => output_binary_logic(&registers, lhs, rhs, dst, Asm::Or2, &mut data),
					BinaryOp::BinXor => output_binary_logic(&registers, lhs, rhs, dst, Asm::Eor, &mut data),
					BinaryOp::LogAnd => output_boolean_logic(&registers, lhs, rhs, dst, Asm::And2, &mut data),
					BinaryOp::LogOr => output_boolean_logic(&registers, lhs, rhs, dst, Asm::Or2, &mut data),
					BinaryOp::LogXor => output_boolean_logic(&registers, lhs, rhs, dst, Asm::Eor, &mut data),
					BinaryOp::CmpEQ => output_cmp(&registers, lhs, rhs, dst, Cond::EQ, Cond::EQ, &mut data),
					BinaryOp::CmpNE => output_cmp(&registers, lhs, rhs, dst, Cond::NE, Cond::NE, &mut data),
					BinaryOp::CmpGE => output_cmp(&registers, lhs, rhs, dst, Cond::GE, Cond::CC, &mut data),
					BinaryOp::CmpGT => output_cmp(&registers, lhs, rhs, dst, Cond::GT, Cond::HI, &mut data),
					BinaryOp::CmpLE => output_cmp(&registers, lhs, rhs, dst, Cond::LE, Cond::LS, &mut data),
					BinaryOp::CmpLT => output_cmp(&registers, lhs, rhs, dst, Cond::LT, Cond::CS, &mut data),
				}
			}

			TAC::UnOp { op, rhs, dst } => {
				data.push(Asm::Comment(format!("{op}{rhs:?} -> {dst:?}")));

				let (ea_rhs, rsz, is_rneg) = get_src_from_location(&registers, rhs);
				let (ea_dst, dsz,_) = get_dst_from_location(&registers, dst);

				data.push(Asm::Move(rsz, ea_rhs, EA::Dat(TR)));
				extend_register(rsz, dsz, is_rneg, TR, &mut data);

				match op {
					UnaryOp::Neg => data.push(Asm::Neg(dsz, EA::Dat(TR))),
					UnaryOp::Not => data.push(Asm::Not(dsz, EA::Dat(TR))),
				}

				data.push(Asm::Move(dsz, EA::Dat(TR), ea_dst));
			}

			TAC::Move { src, dst } => {
				data.push(Asm::Comment(format!("{src:?} -> {dst:?}")));

				let (ea_src,ssz,_) = get_src_from_location(&registers, src);
				let (ea_dst,dsz,_) = get_dst_from_location(&registers, dst);
				let sz = ssz.min(dsz);
				data.push(Asm::Move(sz, ea_src, ea_dst));
			}

			//TAC::Label(id) => {
			//	data.push(Asm::Label(format!("{proc_name}_{id}")));
			//}

			TAC::Jump(id) => {
				data.push(Asm::Bra(*id));
			}

			TAC::JumpIf { lbl, vr } => {
				data.push(Asm::Tst(Sz::L, EA::Dat(registers[vr])));
				data.push(Asm::Bcc(Cond::NE, *lbl));
			}

			TAC::Return(with_value) => {
				if let Some(vr) = with_value {
					let r = registers[vr];
					if r != Data::D0 {
						data.push(Asm::Move(Sz::W, EA::Dat(r), EA::Dat(Data::D0)));
					}
				}
				data.push(Asm::Rts);
			}
		}
	}

	block_converter.finish(data.len());

	(data, block_converter.asm_blocks)
}

fn get_src_from_location(regs: &VRegMap, loc: &Location) -> (EA, Sz, bool) {
	match loc {
		Location::Addr(adr,typ) => {
			let ea = if *adr < i16::MAX as u32 {
				EA::AbW(*adr as i16)
			} else {
				EA::AbL(*adr as i32)
			};
			(ea, get_arithmetic_size(typ), typ.is_signed_integer())
		}
		Location::Const(val,typ) => {
			(EA::Imm(*val as i32), get_arithmetic_size(typ), typ.is_signed_integer())
		}
		Location::Stack(idx,typ) => {
			(EA::Dsp(*idx as i16 * 4, VAR_SP), get_arithmetic_size(typ), typ.is_signed_integer())
		}
		Location::VReg(vr,typ) => {
			(EA::Dat(regs[vr]), get_arithmetic_size(typ), typ.is_signed_integer())
		}
	}
}

fn get_dst_from_location(regs: &VRegMap, loc: &Location) -> (EA, Sz, bool) {
	match loc {
		Location::Addr(adr,typ) => {
			let ea = if *adr < i16::MAX as u32 {
				EA::AbW(*adr as i16)
			} else {
				EA::AbL(*adr as i32)
			};
			(ea, get_arithmetic_size(typ), typ.is_signed_integer())
		}
		Location::Const(..) => panic!("cannot have a constant as a destination"),
		Location::Stack(idx,typ) => {
			(EA::Dsp(*idx as i16 * 4, VAR_SP), get_arithmetic_size(typ), typ.is_signed_integer())
		}
		Location::VReg(vr,typ) => {
			(EA::Dat(regs[vr]), get_arithmetic_size(typ), typ.is_signed_integer())
		}
	}
}

fn extend_register(from: Sz, to: Sz, is_signed: bool, reg: Data, data: &mut Vec<Asm>) {
	if is_signed {
		match (from,to) {
			(Sz::B, Sz::W) => {
				data.push(Asm::Ext(Sz::W, reg));
			}
			(Sz::B, Sz::L) => {
				data.push(Asm::Ext(Sz::W, reg));
				data.push(Asm::Ext(Sz::L, reg));
			}
			(Sz::W, Sz::L) => {
				data.push(Asm::Ext(Sz::L, reg));
			}
			_ => {}
		}
	} else {
		match (from,to) {
			(Sz::B, Sz::W) => {
				data.push(Asm::And1(Sz::W, EA::Imm(0xFF), reg));
			}
			(Sz::B, Sz::L) => {
				data.push(Asm::And1(Sz::L, EA::Imm(0xFF), reg));
			}
			(Sz::W, Sz::L) => {
				data.push(Asm::And1(Sz::L, EA::Imm(0xFFFF), reg));
			}
			_ => {}
		}
	}
}

fn output_binary_logic(
	registers: &VRegMap,
	lhs: &Location, rhs: &Location, dst: &Location,
	logic_fn: fn(Sz,Data,EA) -> Asm,
	data: &mut Vec<Asm>,
) {
	let (ea_lhs, lsz, is_lneg) = get_src_from_location(&registers, lhs);
	let (ea_rhs, rsz, is_rneg) = get_src_from_location(&registers, rhs);
	let (ea_dst, dsz,_) = get_dst_from_location(&registers, dst);

	debug_assert!(dsz >= lsz && dsz >= rsz);

	data.push(Asm::Move(lsz, ea_lhs, EA::Dat(TL)));
	extend_register(lsz, dsz, is_lneg, TL, data);

	data.push(Asm::Move(rsz, ea_rhs, EA::Dat(TR)));
	extend_register(rsz, dsz, is_rneg, TR, data);

	data.push(logic_fn(dsz, TR, EA::Dat(TL)));
	data.push(Asm::Move(dsz, EA::Dat(TL), ea_dst));
}

fn output_boolean_logic(
	registers: &VRegMap,
	lhs: &Location, rhs: &Location, dst: &Location,
	logic_fn: fn(Sz,Data,EA) -> Asm,
	data: &mut Vec<Asm>,
) {
	let (ea_lhs, lsz,_) = get_src_from_location(&registers, lhs);
	let (ea_rhs, rsz,_) = get_src_from_location(&registers, rhs);
	let (ea_dst, dsz, is_dneg) = get_dst_from_location(&registers, dst);

	data.push(Asm::Tst(lsz, ea_lhs));
	data.push(Asm::Scc(Cond::NE, EA::Dat(TL)));

	data.push(Asm::Tst(rsz, ea_rhs));
	data.push(Asm::Scc(Cond::NE, EA::Dat(TR)));

	data.push(logic_fn(Sz::B, TR, EA::Dat(TL)));
	extend_register(Sz::B, dsz, is_dneg, TL, data);

	data.push(Asm::Move(dsz, EA::Dat(TL), ea_dst));
}

fn output_cmp(
	registers: &VRegMap,
	lhs: &Location, rhs: &Location, dst: &Location,
	signed_cc: Cond, unsigned_cc: Cond,
	data: &mut Vec<Asm>,
) {
	let (ea_lhs, lsz, is_lneg) = get_src_from_location(&registers, lhs);
	let (ea_rhs, rsz, is_rneg) = get_src_from_location(&registers, rhs);
	let (ea_dst, dsz, is_signed) = get_dst_from_location(&registers, dst);

	let cc = if is_lneg || is_rneg { signed_cc } else { unsigned_cc };
	let sz = lsz.max(rsz).max(dsz);

	data.push(Asm::Move(lsz, ea_lhs, EA::Dat(TL)));
	extend_register(lsz, sz, is_lneg, TL, data);

	data.push(Asm::Move(rsz, ea_rhs, EA::Dat(TR)));
	extend_register(rsz, sz, is_rneg, TR, data);

	data.push(Asm::Cmp(sz, EA::Dat(TR), TL));
	data.push(Asm::Scc(cc, EA::Dat(TL)));
	extend_register(Sz::B, dsz, is_signed, TL, data);

	data.push(Asm::Move(dsz, EA::Dat(TL), ea_dst));
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Sz { B, W, L }
impl Display for Sz {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::B => write!(f, "b"),
			Self::W => write!(f, "w"),
			Self::L => write!(f, "l"),
		}
	}
}
impl From<&Sz> for u32 {
	fn from(sz: &Sz) -> u32 {
		match sz {
			Sz::B => 1,
			Sz::W => 2,
			Sz::L => 4,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Data { D0, D1, D2, D3, D4, D5, D6, D7 }
impl Display for Data {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::D0 => write!(f, "d0"),
			Self::D1 => write!(f, "d1"),
			Self::D2 => write!(f, "d2"),
			Self::D3 => write!(f, "d3"),
			Self::D4 => write!(f, "d4"),
			Self::D5 => write!(f, "d5"),
			Self::D6 => write!(f, "d6"),
			Self::D7 => write!(f, "d7"),
		}
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Addr { A0, A1, A2, A3, A4, A5, A6, A7 }
impl Display for Addr {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::A0 => write!(f, "a0"),
			Self::A1 => write!(f, "a1"),
			Self::A2 => write!(f, "a2"),
			Self::A3 => write!(f, "a3"),
			Self::A4 => write!(f, "a4"),
			Self::A5 => write!(f, "a5"),
			Self::A6 => write!(f, "a6"),
			Self::A7 => write!(f, "a7"),
		}
	}
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum EA {
	/// Data Register Direct
	/// - Dn
	Dat(Data),
	/// Address Register Direct
	/// - An
	Adr(Addr),
	/// Address Register Indirect
	/// - (An)
	Ind(Addr),
	/// Address Register Indirect with Postincrement
	/// - (An)+
	Pst(Addr),
	/// Address Register Indirect with Predecrement
	/// - -(An)
	Pre(Addr),
	/// Address Register Indirect with Displacement
	/// - (d16,An)
	Dsp(i16,Addr),
	/// Address Register Indirect with Index (8-Bit Displacement)
	/// - (d8,An,Xn)
	Idx(i8,Addr,Addr),
	/// Absolute Short Addressing
	/// - (xxx).W
	AbW(i16),
	/// Absolute Long Addressing
	/// - (xxx).L
	AbL(i32),
	/// Immediate Data
	/// - #<xxx>
	Imm(i32),
}

impl Display for EA {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::Dat(d) => write!(f, "{d}"),
			Self::Adr(a) => write!(f, "{a}"),
			Self::Ind(a) => write!(f, "({a})"),
			Self::Pst(a) => write!(f, "({a})+"),
			Self::Pre(a) => write!(f, "-({a})"),
			Self::Dsp(d,a) => write!(f, "{d}({a})"),
			Self::Idx(d,a,x) => write!(f, "{d}({a},{x})"),
			Self::AbW(a) => write!(f, "(${a:04X})"),
			Self::AbL(a) => write!(f, "(${a:08X})"),
			Self::Imm(a) => write!(f, "#${a:X}"),
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Cond { CC, CS, EQ, GE, GT, HI, LE, LS, LT, NE }
impl Display for Cond {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::CC => write!(f, "cc"),
			Self::CS => write!(f, "cs"),
			Self::EQ => write!(f, "eq"),
			Self::GE => write!(f, "ge"),
			Self::GT => write!(f, "gt"),
			Self::HI => write!(f, "hi"),
			Self::LE => write!(f, "le"),
			Self::LS => write!(f, "ls"),
			Self::LT => write!(f, "lt"),
			Self::NE => write!(f, "ne"),
		}
	}
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Asm {
	Comment(String),

	Label(String),

	Add1(Sz,EA,Data),
	/// Syntax: `ADD.<sz> Dn,<ea>`
	///
	/// Size: (Byte, Word, Long)
	Add2(Sz,Data,EA),
	/// Syntax: `AND.<sz> <ea>,Dn`
	///
	/// Size: (Byte, Word, Long)
	And1(Sz,EA,Data),
	/// Syntax: `AND.<sz> Dn,<ea>`
	///
	/// Size: (Byte, Word, Long)
	And2(Sz,Data,EA),
	Asl(Sz,Data,Data),
	Asr(Sz,Data,Data),
	Bcc(Cond,LabelId),
	Bra(LabelId),
	Clr(Sz,EA),
	Cmp(Sz,EA,Data),
	CmpI(Sz,i32,EA),
	DivS(EA,Data),
	DivU(EA,Data),
	Eor(Sz,Data,EA),
	Ext(Sz,Data),
	Lsl(Sz,Data,Data),
	Lsr(Sz,Data,Data),
	Move(Sz,EA,EA),
	MulS(EA,Data),
	MulU(EA,Data),
	Neg(Sz,EA),
	Nop,
	Not(Sz,EA),
	Or1(Sz,EA,Data),
	Or2(Sz,Data,EA),
	Rts,
	Scc(Cond,EA),
	Sub1(Sz,EA,Data),
	/// Syntax: `SUB.<sz> Dn,<ea>`
	///
	/// Size: (Byte, Word, Long)
	Sub2(Sz,Data,EA),
	Swap(Data),
	Trap(u8),
	Tst(Sz,EA),
}

impl Display for Asm {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::Comment(msg) => write!(f, "\t\t\t; {msg}"),

			Self::Label(label) => write!(f, "{label}:"),

			Self::Add1(sz,ea,d) => write!(f, "\tadd.{sz} {ea},{d}"),
			Self::Add2(sz,d,ea) => write!(f, "\tadd.{sz} {d},{ea}"),
			Self::And1(sz,ea,d) => write!(f, "\tand.{sz} {ea},{d}"),
			Self::And2(sz,d,ea) => write!(f, "\tand.{sz} {d},{ea}"),
			Self::Asl(sz,dx,dy) => write!(f, "\tasl.{sz} {dx},{dy}"),
			Self::Asr(sz,dx,dy) => write!(f, "\tasr.{sz} {dx},{dy}"),
			Self::Bcc(cond,label) => write!(f, "\tb{cond} {label}"),
			Self::Bra(label) => write!(f, "\tbra {label}"),
			Self::Clr(sz,ea) => write!(f, "\tcls.{sz} {ea}"),
			Self::Cmp(sz,ea,d) => write!(f, "\tcmp.{sz} {ea},{d}"),
			Self::CmpI(sz,i,ea) => write!(f, "\tcmpi.{sz} #{i},{ea}"),
			Self::DivS(ea,d) => write!(f, "\tdivs {ea},{d}"),
			Self::DivU(ea,d) => write!(f, "\tdivu {ea},{d}"),
			Self::Eor(sz,d,ea) => write!(f, "\teor.{sz} {d},{ea}"),
			Self::Ext(sz,d) => write!(f, "\text.{sz} {d}"),
			Self::Lsl(sz,dx,dy) => write!(f, "\tlsl.{sz} {dx},{dy}"),
			Self::Lsr(sz,dx,dy) => write!(f, "\tlsr.{sz} {dx},{dy}"),
			Self::Move(sz,src,dst) => write!(f, "\tmove.{sz} {src},{dst}"),
			Self::MulS(ea,d) => write!(f, "\tmuls {ea},{d}"),
			Self::MulU(ea,d) => write!(f, "\tmulu {ea},{d}"),
			Self::Neg(sz,reg) => write!(f, "\tneg.{sz} {reg}"),
			Self::Nop => write!(f, "\tnop"),
			Self::Not(sz,reg) => write!(f, "\tnot.{sz} {reg}"),
			Self::Or1(sz,ea,d) => write!(f, "\tor.{sz} {ea},{d}"),
			Self::Or2(sz,d,ea) => write!(f, "\tor.{sz} {d},{ea}"),
			Self::Rts => write!(f, "\trts"),
			Self::Scc(cond,ea) => write!(f, "\ts{cond} {ea}"),
			Self::Sub1(sz,ea,d) => write!(f, "\tsub.{sz} {ea},{d}"),
			Self::Sub2(sz,d,ea) => write!(f, "\tsub.{sz} {d},{ea}"),
			Self::Swap(d) => write!(f, "\tswap {d}"),
			Self::Trap(v) => write!(f, "\ttrap #{v}"),
			Self::Tst(sz,ea) => write!(f, "\ttst.{sz} {ea}"),
		}
	}
}

