
use std::fmt::{Display, Formatter, Result};

use crate::operators::{BinaryOp, UnaryOp};
use crate::parser::Type;
use crate::tac::{Data as TacData, Location, TAC};

use super::LabelGenerator;

pub fn lower(proc_name: &str, tac_data: TacData, ret_type: Type) -> Vec<Asm> {
	let stack_cells = tac_data.locals.len() + tac_data.next_reg as usize;

	// Use extra stack space as "registers" to fit 16 bit arguments
	let regs: Vec<usize> = (tac_data.locals.len()..stack_cells).collect();
	let registers = super::allocate(&regs, &tac_data.instructions);

	let mut labels = LabelGenerator::new(tac_data.next_label);
	let mut data = vec![
		Asm::Label(proc_name.to_owned()),
		Asm::MovIX16(0),
		Asm::AddIXSP,
	];

	for asm in tac_data.instructions {
		match asm {
			TAC::BinOp { op, lhs, rhs, dst } => {
				let r0 = registers[&vr0];
				let r1 = registers[&vr1];
				let r2 = registers[&vr2];

				data.push(Asm::Comment(format!("binary {op}:{typ:?}")));
				match op {
					BinaryOp::Add => {
						z80_byte_op(&mut data, 1, Asm::AddIX, r0, r1, r2);
						if is_16bit(typ) {
							z80_byte_op(&mut data, 0, Asm::AdCIX, r0, r1, r2);
						}
					}
					BinaryOp::Sub => {
						z80_byte_op(&mut data, 1, Asm::SubIX, r0, r1, r2);
						if is_16bit(typ) {
							z80_byte_op(&mut data, 0, Asm::SbCIX, r0, r1, r2);
						}
					}
					BinaryOp::Mul => {}
					BinaryOp::Div => {}
					BinaryOp::Mod => {}
					BinaryOp::ShL => {}
					BinaryOp::ShR => {}
					BinaryOp::BinAnd => {
						z80_byte_op(&mut data, 1, Asm::AndIX, r0, r1, r2);
						if is_16bit(typ) {
							z80_byte_op(&mut data, 0, Asm::AndIX, r0, r1, r2);
						}
					}
					BinaryOp::BinOr => {
						z80_byte_op(&mut data, 1, Asm::OrIX, r0, r1, r2);
						if is_16bit(typ) {
							z80_byte_op(&mut data, 0, Asm::OrIX, r0, r1, r2);
						}
					}
					BinaryOp::BinXor => {
						z80_byte_op(&mut data, 1, Asm::XorIX, r0, r1, r2);
						if is_16bit(typ) {
							z80_byte_op(&mut data, 0, Asm::XorIX, r0, r1, r2);
						}
					}
					BinaryOp::LogAnd => {}
					BinaryOp::LogOr => {}
					BinaryOp::LogXor => {}
					BinaryOp::CmpEQ => {}
					BinaryOp::CmpNE => {}
					BinaryOp::CmpGE => {}
					BinaryOp::CmpGT => {}
					BinaryOp::CmpLE => {}
					BinaryOp::CmpLT => {}
				}
			}

			TAC::UnOp { op, typ, vr0, vr1 } => {
				let r0 = registers[&vr0];
				let r1 = registers[&vr1];

				data.push(Asm::Comment(format!("unary {op}:{typ:?}")));

				// 8/46 or 7/42
				data.push(Asm::LdWithIX(R8::A, r0 as i8 * 2 + 1)); // 3/19
				match op {
					UnaryOp::Neg => data.push(Asm::Neg),             // 2/8
					UnaryOp::Not => data.push(Asm::Cpl),             // 1/4
				}
				data.push(Asm::StIntoIX(R8::A, r1 as i8 * 2 + 1)); // 3/19

				// 16/92 or 14/84
				if is_16bit(typ) {
					data.push(Asm::LdWithIX(R8::A, r0 as i8 * 2)); // 3/19
					match op {
						UnaryOp::Neg => data.push(Asm::Neg),         // 2/8
						UnaryOp::Not => data.push(Asm::Cpl),         // 1/4
					}
					data.push(Asm::StIntoIX(R8::A, r1 as i8 * 2)); // 3/19
				}
			}

			TAC::Move { src, dst } => match (src, dst) {
				(_, Location::Const(..)) =>  unreachable!("cannot store into a constant"),

				(Location::Addr(adrs,typs), Location::Addr(adrd, typd)) => {
					data.push(Asm::Comment(format!("move ({adrs:X}:{typs:?}) -> ({adrd:X}:{typd:?})")));

					if is_16bit(typs) && is_16bit(typd) {
						data.push(Asm::LdAMem(adrs as u16 + 1));
						data.push(Asm::StAMem(adrd as u16 + 1));
						data.push(Asm::LdAMem(adrs as u16));
						data.push(Asm::StAMem(adrd as u16));
					} else {
						data.push(Asm::LdAMem(adrs as u16));
						data.push(Asm::StAMem(adrd as u16));
					}
				}

				(Location::Addr(adrs,typs), Location::Stack(idxd, typd)) => {
					data.push(Asm::Comment(format!("load ({adrs:X}:{typs:?}) -> [{idxd}:{typd:?}]")));

					if is_16bit(typs) && is_16bit(typd) {
						data.push(Asm::LdAMem(adrs as u16 + 1));
						data.push(Asm::StIntoIX(R8::A, idxd as i8 * 2 + 1));
						data.push(Asm::LdAMem(adrs as u16));
						data.push(Asm::StIntoIX(R8::A, idxd as i8 * 2));
					} else {
						data.push(Asm::LdAMem(adrs as u16));
						data.push(Asm::StIntoIX(R8::A, idxd as i8 * 2 + 1));
					}
				}

				(Location::Addr(adrs, typs), Location::VReg(vrd, typd)) => {
					let r0 = registers[&vrd];

					data.push(Asm::Comment(format!("load ({adrs:X}:{typs:?}) -> {vrd}:{typd:?}")));

					if is_16bit(typs) && is_16bit(typd) {
						data.push(Asm::LdAMem(adrs as u16 + 1));
						data.push(Asm::StIntoIX(R8::A, r0 as i8 * 2 + 1));
						data.push(Asm::LdAMem(adrs as u16));
						data.push(Asm::StIntoIX(R8::A, r0 as i8 * 2));
					} else {
						data.push(Asm::LdAMem(adrs as u16));
						data.push(Asm::StIntoIX(R8::A, r0 as i8 * 2 + 1));
					}
				}

				(Location::Const(vals, typs), Location::Addr(adrd, typd)) => {
					data.push(Asm::Comment(format!("load #{vals}:{typs:?} -> ({adrd:X}:{typd:?})")));

					// 4/19 | 8/38
					if is_16bit(typs) && is_16bit(typd) {
						data.push(Asm::Mov8(R8::A, vals as u8));
						data.push(Asm::StAMem(adrd as u16 + 1));
						data.push(Asm::Mov8(R8::A, (vals >> 8) as u8)); // 4/19
						data.push(Asm::StAMem(adrd as u16));
					} else {
						data.push(Asm::Mov8(R8::A, vals as u8));
						data.push(Asm::StAMem(adrd as u16));
					}
				}

				(Location::Const(vals, typs), Location::Stack(idxd, typd)) => {
					data.push(Asm::Comment(format!("load #{vals}:{typs:?} -> [{idxd}:{typd:?}]")));

					data.push(Asm::StIX8(idxd as i8 * 2 + 1, vals as u8));
					if is_16bit(typs) && is_16bit(typd) {
						data.push(Asm::StIX8(idxd as i8 * 2, (vals >> 8) as u8));
					}
				}

				(Location::Const(vals, typs), Location::VReg(vrd, typd)) => {
					let r0 = registers[&vrd];

					data.push(Asm::Comment(format!("load #{vals}:{typs:?} -> {vrd}:{typd:?}")));

					data.push(Asm::StIX8(r0 as i8 * 2 + 1, vals as u8));
					if is_16bit(typs) && is_16bit(typd) {
						data.push(Asm::StIX8(r0 as i8 * 2, (vals >> 8) as u8));
					}
				}

				(Location::Stack(idxs, typs), Location::Addr(adrd, typd)) => {
					data.push(Asm::Comment(format!("load [{idxs}:{typs:?}] -> ({adrd:X}:{typd:?})")));

					if is_16bit(typs) && is_16bit(typd) {
						data.push(Asm::LdWithIX(R8::A, idxs as i8 * 2 + 1));
						data.push(Asm::StAMem(adrd as u16 + 1));
						data.push(Asm::LdWithIX(R8::A, idxs as i8 * 2));
						data.push(Asm::StAMem(adrd as u16));
					} else {
						data.push(Asm::LdWithIX(R8::A, idxs as i8 * 2 + 1));
						data.push(Asm::StAMem(adrd as u16));
					}
				}

				(Location::Stack(idxs, typs), Location::Stack(idxd, typd)) => {
					data.push(Asm::Comment(format!("load [{idxs}:{typs:?}] -> [{idxd}:{typd:?}]")));

					data.push(Asm::LdWithIX(R8::A, idxs as i8 * 2 + 1));
					data.push(Asm::StIntoIX(R8::A, idxd as i8 * 2 + 1));
					if is_16bit(typs) && is_16bit(typd) {
						data.push(Asm::LdWithIX(R8::A, idxs as i8 * 2));
						data.push(Asm::StIntoIX(R8::A, idxd as i8 * 2));
					}
				}

				(Location::Stack(idxs, typs), Location::VReg(vrd, typd)) => {
					let r0 = registers[&vrd];

					data.push(Asm::Comment(format!("load [{idxs}:{typs:?}] -> {vrd}:{typd:?}")));

					data.push(Asm::LdWithIX(R8::A, idxs as i8 * 2 + 1));
					data.push(Asm::StIntoIX(R8::A, r0 as i8 * 2 + 1));
					if is_16bit(typs) && is_16bit(typd) {
						data.push(Asm::LdWithIX(R8::A, idxs as i8 * 2));
						data.push(Asm::StIntoIX(R8::A, r0 as i8 * 2));
					}
				}

				(Location::VReg(vrs, typs), Location::Addr(adrd, typd)) => {
					let r0 = registers[&vrs];

					data.push(Asm::Comment(format!("load {vrs}:{typs:?} -> ({adrd:X}:{typd:?})")));

					if is_16bit(typs) && is_16bit(typd) {
						for i in (0..2).rev() {
							data.push(Asm::LdWithIX(R8::A, r0 as i8 * 2 + i as i8));
							data.push(Asm::StAMem(adrd as u16 + i as u16));
						}
					} else {
						data.push(Asm::LdWithIX(R8::A, r0 as i8 * 2 + 1));
						data.push(Asm::StAMem(adrd as u16));
					}
				}

				(Location::VReg(vrs, typs), Location::Stack(idxd, typd)) => {
					let r0 = registers[&vrs];

					data.push(Asm::Comment(format!("load {vrs}:{typs:?} -> [{idxd}:{typd:?}]")));

					data.push(Asm::LdWithIX(R8::A, r0 as i8 * 2 + 1));
					data.push(Asm::StIntoIX(R8::A, idxd as i8 * 2 + 1));
					if is_16bit(typs) && is_16bit(typd) {
						data.push(Asm::LdWithIX(R8::A, r0 as i8 * 2));
						data.push(Asm::StIntoIX(R8::A, idxd as i8 * 2));
					}
				}

				(Location::VReg(vrs, typs), Location::VReg(vrd, typd)) => {
					let r0 = registers[&vrs];
					let r1 = registers[&vrd];

					data.push(Asm::Comment(format!("load {vrs}:{typs:?} -> {vrd}:{typd:?}")));

					data.push(Asm::LdWithIX(R8::A, r0 as i8 * 2 + 1));
					data.push(Asm::StIntoIX(R8::A, r1 as i8 * 2 + 1));
					if is_16bit(typs) && is_16bit(typd) {
						data.push(Asm::LdWithIX(R8::A, r0 as i8 * 2));
						data.push(Asm::StIntoIX(R8::A, r1 as i8 * 2));
					}
				}
			}

			TAC::Label(lbl) => data.push(Asm::Label(format!("{proc_name}_{lbl}"))),

			TAC::Jump(lbl) => {
				data.push(Asm::Comment("jump".into()));
				let label = format!("{proc_name}_{lbl}");
				data.push(Asm::Jp(label));
			}

			TAC::JumpIf { lbl, vr } => {
				let r0 = registers[&vr];

				data.push(Asm::Comment("jump-if".into()));

				let label = format!("{proc_name}_{lbl}");
				data.push(Asm::LdWithIX(R8::A, r0 as i8 * 2 + 1));
				data.push(Asm::Cpl);
				data.push(Asm::JpC(Cond::NZ,label.clone()));
				data.push(Asm::LdWithIX(R8::A, r0 as i8 * 2));
				data.push(Asm::Cpl);
				data.push(Asm::JpC(Cond::NZ,label));
			}

			TAC::Return(with_value) => {
				data.push(Asm::Comment("return".into()));

				if let Some(vr) = with_value {
					let r0 = registers[&vr];

					data.push(Asm::LdWithIX(R8::E, r0 as i8 * 2 + 1));
					if is_16bit(ret_type) {
						data.push(Asm::LdWithIX(R8::D, r0 as i8 * 2));
					}
				}

				data.push(Asm::Ret);
			}
		}
	}

	data
}

fn is_16bit(typ: Type) -> bool {
	match typ {
		Type::S8 | Type::U8 | Type::Bool => false,
		Type::S16 | Type::U16 => true,
		Type::S32 | Type::U32 => {
			println!("Found {typ:?} variable");
			true
		}
		_ => unreachable!("Unexpected type: {typ:?}"),
	}
}

fn z80_byte_op(data: &mut Vec<Asm>,
	i: i8, op: fn(i8) -> Asm,
	r0: usize, r1: usize, r2: usize,
) {
	data.push(Asm::LdWithIX(R8::A, r0 as i8 * 2 + i)); // 3/19
	data.push(op(r1 as i8 * 2 + i));                   // 3/19
	data.push(Asm::StIntoIX(R8::A, r2 as i8 * 2 + i)); // 3/19
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum R8 { A, B, C, D, E, H, L }
impl Display for R8 {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "{self:?}")
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum R16 { BC, DE, HL }
impl Display for R16 {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "{self:?}")
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum Cond { NC, NZ, C, Z, PO, PE, P, M }
impl Display for Cond {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "{self:?}")
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum RstTgt { H00, H08, H10, H18, H20, H28, H30, H38 }
impl Display for RstTgt {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::H00 => write!(f, "00h"),
			Self::H08 => write!(f, "08h"),
			Self::H10 => write!(f, "10h"),
			Self::H18 => write!(f, "18h"),
			Self::H20 => write!(f, "20h"),
			Self::H28 => write!(f, "28h"),
			Self::H30 => write!(f, "30h"),
			Self::H38 => write!(f, "38h"),
		}
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Asm {
	Comment(String),

	Label(String),

	/// nop
	/// - bytes: 1
	/// - cycles: 4
	Nop,
	/// ld r,r
	/// - bytes: 1
	/// - cycles: 4
	Mov(R8, R8),
	/// ld A,n | ld B,n | ld C,n | ld D,n | ld E,n | ld H,n | ld L,n
	/// - bytes: 2
	/// - cycles: 7
	Mov8(R8, u8),
	/// ld BC,nn | ld DE,nn | ld HL,nn
	/// - bytes: 3
	/// - cycles: 10
	Mov16(R16, u16),
	/// ld IX,nn
	/// - bytes: 4
	/// - cycles: 14
	MovIX16(u16),
	/// ld SP,nn
	/// - bytes: 3
	/// - cycles: 10
	MovSP(u16),
	/// ld SP,HL
	/// - bytes: 1
	/// - cycles: 6
	MovAdr,
	/// ldi : (DE++) = (HL++); BC--
	/// - bytes: 2
	/// - cycles: 16
	MovMemInc,
	/// ldd : (DE--) = (HL--); BC--
	/// - bytes: 2
	/// - cycles: 16
	MovMemDec,

	/// ld A,(BC)
	LdAWithBC,
	/// ld A,(DE)
	LdAWithDE,
	/// ld A,(nn)
	LdAMem(u16),
	/// ld HL,(nn)
	LdHL(u16),
	/// ld A,(HL) | ld B,(HL) | ld C,(HL) | ld D,(HL) | ld E,(HL) | ld H,(HL) | ld L,(HL)
	LdWithHL(R8),
	/// ld A,(IX+n) | ld B,(IX+n) | ld C,(IX+n) | ld D,(IX+n) | ld E,(IX+n) | ld H,(IX+n) | ld L,(IX+n)
	LdWithIX(R8, i8),

	/// ld (BC),A
	StAIntoBC,
	/// ld (DE),A
	StAIntoDE,
	/// ld (nn),A
	StAMem(u16),
	/// ld (nn),HL
	StHL(u16),
	/// ld (HL),A | ld (HL),B | ld (HL),C | ld (HL),D | ld (HL),E | ld (HL),H | ld (HL),L
	StIntoHL(R8),
	/// ld (IX+n),A | ld (IX+n),B | ld (IX+n),C | ld (IX+n),D | ld (IX+n),E | ld (IX+n),H | ld (IX+n),L
	StIntoIX(R8, i8),
	/// ld (HL),n
	St8(u8),
	/// ld (IX+d),n
	StIX8(i8, u8),

	/// inc BC | inc DE | inc HL
	Inc16(R16),
	/// inc SP
	IncSP,
	/// inc (HL)
	IncMem,
	/// inc A | inc B | inc C | inc D | inc E | inc H | inc L
	Inc(R8),

	/// dec BC | dec DE | dec HL
	Dec16(R16),
	/// dec SP
	DecSP,
	/// dec (HL)
	DecMem,
	/// dec A | dec B | dec C | dec D | dec E | dec H | dec L
	Dec(R8),

	/// djnz d
	DJNZ(String),

	/// jr d
	Jr(String),
	/// jr NZ,d
	JrNZ(String),
	/// jr Z,d
	JrZ(String),
	/// jr NC,d
	JrNC(String),
	/// jr C,d
	JrC(String),

	/// jp nn
	Jp(String),
	/// jp (HL)
	JpHL,
	/// jp NZ,nn
	/// jp Z,nn
	/// jp NC,nn
	/// jp C,nn
	/// jp PO,nn
	/// jp PE,nn
	/// jp P,nn
	/// jp M,nn
	JpC(Cond, String),

	/// add HL,BC | add HL,DE | add HL,HL
	Add16(R16),
	/// add HL,SP
	AddSP,
	/// add IX,SP
	AddIXSP,
	/// add A | add B | add C | add D | add E | add H | add L
	Add(R8),
	/// add (HL)
	AddMem,
	/// add n
	AddN(u8),
	/// add (IX+n)
	AddIX(i8),
	/// adc A | adc B | adc C | adc D | adc E | adc H | adc L
	AdC(R8),
	/// adc (HL)
	AdCMem,
	/// adc n
	AdCN(u8),
	/// adc (IX+n)
	AdCIX(i8),

	/// sub A | sub B | sub C | sub D | sub E | sub H | sub L
	Sub(R8),
	/// sub (HL)
	SubMem,
	/// sub n
	SubN(u8),
	/// sub (IX+n)
	SubIX(i8),
	/// sbc A | sbc B | sbc C | sbc D | sbc E | sbc H | sbc L
	SbC(R8),
	/// sbc (HL)
	SbCMem,
	/// sbc n
	SbCN(u8),
	/// sbc (IX+n)
	SbCIX(i8),

	/// rlc A | rlc B | rlc C | rlc D | rlc E | rlc H | rlc L
	Rlc(R8),
	/// rlc (HL)
	RlcHL,
	/// rl A | rl B | rl C | rl D | rl E | rl H | rl L
	Rl(R8),
	/// rl (HL)
	RlHL,
	/// rrc A | rrc B | rrc C | rrc D | rrc E | rrc H | rrc L
	Rrc(R8),
	/// rrc (HL)
	RrcHL,
	/// rr A | rr B | rr C | rr D | rr E | rr H | rr L
	Rr(R8),
	/// rr (HL)
	RrHL,

	/// sla A | sla B | sla C | sla D | sla E | sla H | sla L
	Sla(R8),
	/// sla (HL)
	SlaHL,
	/// sra A | sra B | sra C | sra D | sra E | sra H | sra L
	Sra(R8),
	/// sra (HL)
	SraHL,
	/// sll A | sll B | sll C | sll D | sll E | sll H | sll L
	Sll(R8),
	/// sll (HL)
	SllHL,
	/// srl A | srl B | srl C | srl D | srl E | srl H | srl L
	Srl(R8),
	/// srl (HL)
	SrlHL,

	/// daa
	DAA,

	/// cpl
	Cpl,

	/// scf
	SCF,
	/// ccf
	CCF,

	/// exx
	Exx,
	/// ex AF,AF'
	ExAF,
	/// ex (SP),HL
	ExSP,
	/// ex DE,HL
	ExDE,

	/// halt
	Halt,

	/// and a | and b | and c | and d | and e | and h | and l
	And(R8),
	/// and (HL)
	AndMem,
	/// and n
	AndN(u8),
	/// and (IX+n)
	AndIX(i8),
	/// xor a | xor b | xor c | xor d | xor e | xor h | xor l
	Xor(R8),
	/// xor (HL)
	XorMem,
	/// xor n
	XorN(u8),
	/// xor (IX+n)
	XorIX(i8),
	/// or a | or b | or c | or d | or e | or h | or l
	Or(R8),
	/// or (HL)
	OrMem,
	/// or n
	OrN(u8),
	/// or (IX+n)
	OrIX(i8),
	/// cp a | cp b | cp c | cp d | cp e | cp h | cp l
	Cp(R8),
	/// cp (HL)
	CpMem,
	/// cp n
	CpN(u8),
	/// cp (IX+n)
	CpIX(i8),
	/// neg
	Neg,
	/// ret
	Ret,
	/// ret NZ | ret NC | ret PO | ret P | ret Z | ret C | ret PE | ret M
	RetC(Cond),
	/// rst 00h | rst 08h | rst 10h | rst 18h | rst 20h | rst 28h | rst 30h | rst 38h
	Rst(RstTgt),
	/// pop BC | pop DE | pop HL
	Pop(R16),
	/// pop AF
	PopAF,
	/// push BC | push DE | push HL
	Push(R16),
	/// push AF
	PushAF,
	/// di | ei
	IrqEn(bool),
	/// in a,(n)
	In(u8),
	/// out (n),a
	Out(u8),
	/// call nn
	Call(u16),
	/// call NZ,nn | call NC,nn | call PO,nn | call P,nn | call Z,nn | call C,nn | call PE,nn | call M,nn
	CallC(Cond,u16),

	/// bit n,A | bit n,B | bit n,C | bit n,D | bit n,E | bit n,H | bit n,L
	Bit(u8,R8),
	/// bit n,(HL)
	BitHL(u8),
	/// res n,A | res n,B | res n,C | res n,D | res n,E | res n,H | res n,L
	Res(u8,R8),
	/// res n,(HL)
	ResHL(u8),
	/// set n,A | set n,B | set n,C | set n,D | set n,E | set n,H | set n,L
	Set(u8,R8),
	/// set n,(HL)
	SetHL(u8),
}

impl Display for Asm {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::Comment(msg) => write!(f, "\t\t\t; {msg}"),
			Self::Label(label) => write!(f, "{label}:"),
			Self::Nop => write!(f, "\tnop"),
			Self::Mov(rd,rs) => write!(f, "\tld {rd},{rs}"),
			Self::Mov8(rd,n) => write!(f, "\tld {rd},{n}"),
			Self::Mov16(rd,nn) => write!(f, "\tld {rd},{nn}"),
			Self::MovIX16(nn) => write!(f, "\tld IX,{nn}"),
			Self::MovSP(nn) => write!(f, "\tld SP,{nn}"),
			Self::MovAdr => write!(f, "\tld SP,HL"),
			Self::MovMemInc => write!(f, "\tldi"),
			Self::MovMemDec => write!(f, "\tldd"),
			Self::LdAWithBC => write!(f, "\tld A,(BC)"),
			Self::LdAWithDE => write!(f, "\tld A,(DE)"),
			Self::LdAMem(nn) => write!(f, "\tld A,({nn})"),
			Self::LdHL(nn) => write!(f, "\tld HL,({nn})"),
			Self::LdWithHL(rd) => write!(f, "\tld {rd},(HL)"),
			Self::LdWithIX(rd,n) => write!(f, "\tld {rd},(IX+{n})"),
			Self::StAIntoBC => write!(f, "\tld (BC),A"),
			Self::StAIntoDE => write!(f, "\tld (DE),A"),
			Self::StAMem(nn) => write!(f, "\tld ({nn}),A"),
			Self::StHL(nn) => write!(f, "\tld ({nn}),HL"),
			Self::StIntoHL(rs) => write!(f, "\tld (HL),{rs}"),
			Self::StIntoIX(rs,n) => write!(f, "\tld (IX+{n}),{rs}"),
			Self::St8(n) => write!(f, "\tld (HL),{n}"),
			Self::StIX8(d,n) => write!(f, "\tld (IX+{d}),{n}"),
			Self::Inc(rd) => write!(f, "\tinc {rd}"),
			Self::Inc16(rd) => write!(f, "\tinc {rd}"),
			Self::IncSP => write!(f, "\tinc SP"),
			Self::IncMem => write!(f, "\tinc (HL)"),
			Self::Dec(rd) => write!(f, "\tdec {rd}"),
			Self::Dec16(rd) => write!(f, "\tdec {rd}"),
			Self::DecSP => write!(f, "\tdec SP"),
			Self::DecMem => write!(f, "\tdec (HL)"),
			Self::DJNZ(d) => write!(f, "\tdjnz {d}"),
			Self::Jr(d) => write!(f, "\tjr {d}"),
			Self::JrC(d) => write!(f, "\tjr C,{d}"),
			Self::JrZ(d) => write!(f, "\tjr Z,{d}"),
			Self::JrNC(d) => write!(f, "\tjr NC,{d}"),
			Self::JrNZ(d) => write!(f, "\tjr NZ,{d}"),
			Self::Jp(nn) => write!(f, "\tjp {nn}"),
			Self::JpHL => write!(f, "\tjp (HL)"),
			Self::JpC(cc, nn) => write!(f, "\tjp {cc},{nn}"),
			Self::Add16(rs) => write!(f, "\tadd HL,{rs}"),
			Self::AddSP => write!(f, "\tadd HL,SP"),
			Self::Add(rs) => write!(f, "\tadd {rs}"),
			Self::AddMem => write!(f, "\tadd (HL)"),
			Self::AddN(n) => write!(f, "\tadd {n}"),
			Self::AddIX(n) => write!(f, "\tadd (IX+{n})"),
			Self::AddIXSP => write!(f, "\tadd IX,SP"),
			Self::AdC(rs) => write!(f, "\tadc {rs}"),
			Self::AdCMem => write!(f, "\tadc (HL)"),
			Self::AdCN(n) => write!(f, "\tadc {n}"),
			Self::AdCIX(n) => write!(f, "\tadc (IX+{n})"),
			Self::Sub(rs) => write!(f, "\tsub {rs}"),
			Self::SubMem => write!(f, "\tsub (HL)"),
			Self::SubN(n) => write!(f, "\tsub {n}"),
			Self::SubIX(n) => write!(f, "\tsub (IX+{n})"),
			Self::SbC(rs) => write!(f, "\tsbc {rs}"),
			Self::SbCMem => write!(f, "\tsbc (HL)"),
			Self::SbCN(n) => write!(f, "\tsbc {n}"),
			Self::SbCIX(n) => write!(f, "\tsbc (IX+{n})"),
			Self::Rlc(rd) => write!(f, "\trlc {rd}"),
			Self::RlcHL => write!(f, "\trlc (HL)"),
			Self::Rl(rd) => write!(f, "\trl {rd}"),
			Self::RlHL => write!(f, "\trl (HL)"),
			Self::Rrc(rd) => write!(f, "\trrc {rd}"),
			Self::RrcHL => write!(f, "\trrc (HL)"),
			Self::Rr(rd) => write!(f, "\trr {rd}"),
			Self::RrHL => write!(f, "\trr (HL)"),
			Self::Sla(rd) => write!(f, "\tsla {rd}"),
			Self::SlaHL => write!(f, "\tsla (HL)"),
			Self::Sra(rd) => write!(f, "\tsra {rd}"),
			Self::SraHL => write!(f, "\tsra (HL)"),
			Self::Sll(rd) => write!(f, "\tsll {rd}"),
			Self::SllHL => write!(f, "\tsll (HL)"),
			Self::Srl(rd) => write!(f, "\tsrl {rd}"),
			Self::SrlHL => write!(f, "\tsrl (HL)"),
			Self::DAA => write!(f, "\tdaa"),
			Self::Cpl => write!(f, "\tcpl"),
			Self::SCF => write!(f, "\tscf"),
			Self::CCF => write!(f, "\tccf"),
			Self::Exx => write!(f, "\texx"),
			Self::ExAF => write!(f, "\tex AF,AF'"),
			Self::ExSP => write!(f, "\tex (SP),HL"),
			Self::ExDE => write!(f, "\tex DE,HL"),
			Self::Halt => write!(f, "\thalt"),
			Self::And(rs) => write!(f, "\tand {rs}"),
			Self::AndMem => write!(f, "\tand (HL)"),
			Self::AndN(n) => write!(f, "\tand {n}"),
			Self::AndIX(n) => write!(f, "\tand (IX+{n})"),
			Self::Xor(rs) => write!(f, "\txor {rs}"),
			Self::XorMem => write!(f, "\txor (HL)"),
			Self::XorN(n) => write!(f, "\txor {n}"),
			Self::XorIX(n) => write!(f, "\txor (IX+{n})"),
			Self::Or(rs) => write!(f, "\tor {rs}"),
			Self::OrMem => write!(f, "\tor (HL)"),
			Self::OrN(n) => write!(f, "\tor {n}"),
			Self::OrIX(n) => write!(f, "\tor (IX+{n})"),
			Self::Cp(rs) => write!(f, "\tcp {rs}"),
			Self::CpMem => write!(f, "\tcp (HL)"),
			Self::CpN(n) => write!(f, "\tcp {n}"),
			Self::CpIX(n) => write!(f, "\tcp (IX+{n})"),
			Self::Neg => write!(f, "\tneg"),
			Self::Ret => write!(f, "\tret"),
			Self::RetC(cc) => write!(f, "\tret {cc}"),
			Self::Rst(tgt) => write!(f, "\trst {tgt}"),
			Self::Pop(rs) => write!(f, "\tpop {rs}"),
			Self::PopAF => write!(f, "\tpop AF"),
			Self::Push(rs) => write!(f, "\tpush {rs}"),
			Self::PushAF => write!(f, "\tpush AF"),
			Self::IrqEn(false) => write!(f, "\tdi"),
			Self::IrqEn(true) => write!(f, "\tei"),
			Self::In(n) => write!(f, "\tin A,({n})"),
			Self::Out(n) => write!(f, "\tout ({n}),A"),
			Self::Call(nn) => write!(f, "\tcall {nn}"),
			Self::CallC(cc, nn) => write!(f, "\tcall {cc},{nn}"),
			Self::Bit(n,rs) => write!(f, "\tbit {n},{rs}"),
			Self::BitHL(n) => write!(f, "\tbit {n},(HL)"),
			Self::Res(n,rs) => write!(f, "\tres {n},{rs}"),
			Self::ResHL(n) => write!(f, "\tres {n},(HL)"),
			Self::Set(n,rs) => write!(f, "\tset {n},{rs}"),
			Self::SetHL(n) => write!(f, "\tset {n},(HL)"),
		}
	}
}
