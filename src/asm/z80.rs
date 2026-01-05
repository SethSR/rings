
use std::fmt::{Display, Formatter, Result};
use crate::operators::{BinaryOp, UnaryOp};
use crate::vsmc::{Section, Vsmc};

use super::inner_label;

pub fn lower(proc_name: &str, section: Section) -> Vec<Asm> {
	let Section {
		instructions,
		next_label: mut label_id,
		..
	} = section;

	let mut data = vec![
		Asm::Label(proc_name.to_owned()),
	];
	for asm in instructions {
		match asm {
			Vsmc::Push(value) => {
				data.push(Asm::Mov8(value as u8));
				data.push(Asm::Dec16(R16::HL));
				data.push(Asm::StIntoHL(R8::A));
			}
			Vsmc::BinOp(op) => {
				data.push(Asm::LdWithHL(R8::B));
				data.push(Asm::Inc16(R16::HL));
				data.push(Asm::LdWithHL(R8::A));
				match op {
					BinaryOp::Add => data.push(Asm::Add(R8::B)),
					BinaryOp::Sub => data.push(Asm::Sub(R8::B)),
					BinaryOp::Mul => {
						let clear = inner_label(proc_name, &mut label_id);
						let mul_loop = inner_label(proc_name, &mut label_id);
						let end = inner_label(proc_name, &mut label_id);
						// if c == 0 goto end
						data.push(Asm::CpN(0));
						data.push(Asm::JrZ(clear.clone()));
						data.push(Asm::Mov(R8::C, R8::A));
						data.push(Asm::Xor(R8::A));
						data.push(Asm::Label(mul_loop.clone()));
						data.push(Asm::Add(R8::C));
						data.push(Asm::DJNZ(mul_loop));
						data.push(Asm::Jr(end.clone()));
						data.push(Asm::Label(clear));
						data.push(Asm::Xor(R8::A));
						data.push(Asm::Label(end));
					}
					BinaryOp::Div => {
						let b_neg = inner_label(proc_name, &mut label_id);
						let a_neg = inner_label(proc_name, &mut label_id);
						let check = inner_label(proc_name, &mut label_id);
						let div_loop = inner_label(proc_name, &mut label_id);
						let end = inner_label(proc_name, &mut label_id);
						// let flip = false;
						data.push(Asm::Mov(R8::C, R8::A)); // remember A
						data.push(Asm::Mov8(0));
						data.push(Asm::Mov(R8::D, R8::A)); // D = false
						// if b < 0 {
						data.push(Asm::Bit(7, R8::B));     // if (B & 0x80) > 0
						data.push(Asm::JrZ(b_neg.clone()));
						//   flip = !flip;
						data.push(Asm::Mov(R8::A, R8::D));
						data.push(Asm::Cpl);
						data.push(Asm::Mov(R8::D, R8::A)); // D = !D
						//   b = -b;
						data.push(Asm::Mov(R8::A, R8::B));
						data.push(Asm::Neg);
						data.push(Asm::Mov(R8::B, R8::A)); // B = -B
						// }
						data.push(Asm::Label(b_neg));
						data.push(Asm::Mov(R8::A, R8::C)); // restore A
						// if a < 0 {
						data.push(Asm::Bit(7, R8::A));     // if (A & 0x80) > 0
						data.push(Asm::JrZ(a_neg.clone()));
						//   flip = !flip;
						data.push(Asm::Mov(R8::A, R8::D));
						data.push(Asm::Cpl);
						data.push(Asm::Mov(R8::D, R8::A)); // D = !D
						//   a = -a;
						data.push(Asm::Mov(R8::A, R8::C)); // restore A
						data.push(Asm::Neg);               // A = -A
						// }
						data.push(Asm::Label(a_neg));
						data.push(Asm::Mov(R8::C, R8::A)); // remember A
						data.push(Asm::Xor(R8::A));
						data.push(Asm::Mov(R8::E, R8::A));
						data.push(Asm::Mov(R8::A, R8::C));
						// while a > b {
						data.push(Asm::Jr(check.clone()));
						data.push(Asm::Label(div_loop.clone()));
						//   a -= b;
						data.push(Asm::Sub(R8::B));
						//   e += 1;
						data.push(Asm::Inc(R8::E));
						data.push(Asm::Label(check));
						data.push(Asm::Cp(R8::B));
						data.push(Asm::JrNC(div_loop));
						// }
						// e -= 1;
						data.push(Asm::Dec(R8::E));
						// a = e;
						data.push(Asm::Mov(R8::A, R8::E));
						// if flip {
						data.push(Asm::Bit(0, R8::D));
						data.push(Asm::JrZ(end.clone()));
						//   a = -a;
						data.push(Asm::Neg);
						// }
						data.push(Asm::Label(end));
					}
					BinaryOp::Mod => {
						let b_check = inner_label(proc_name, &mut label_id);
						let a_check = inner_label(proc_name, &mut label_id);
						let check = inner_label(proc_name, &mut label_id);
						let div_loop = inner_label(proc_name, &mut label_id);
						let rem_check = inner_label(proc_name, &mut label_id);
						let end = inner_label(proc_name, &mut label_id);
						// let a_neg = false; // D = false;
						// let b_neg = false; // E = false;
						data.push(Asm::Mov(R8::C, R8::A)); // remember A
						data.push(Asm::Xor(R8::A));        // A = 0
						data.push(Asm::Mov(R8::D, R8::A)); // D = false
						data.push(Asm::Mov(R8::E, R8::A)); // E = false
						// if b < 0 {
						data.push(Asm::Bit(7, R8::B));     // if (B & 0x80) > 0
						data.push(Asm::JrZ(b_check.clone()));
						//   b_neg = true;
						data.push(Asm::Inc(R8::E));
						//   b = -b;
						data.push(Asm::Mov(R8::A, R8::B));
						data.push(Asm::Neg);
						data.push(Asm::Mov(R8::B, R8::A)); // B = -B
						// }
						data.push(Asm::Label(b_check));
						data.push(Asm::Mov(R8::A, R8::C)); // restore A
						// if a < 0 {
						data.push(Asm::Bit(7, R8::A));     // if (A & 0x80) > 0
						data.push(Asm::JrZ(a_check.clone()));
						//   a_neg = true;
						data.push(Asm::Inc(R8::D));
						//   a = -a;
						data.push(Asm::Mov(R8::A, R8::C)); // restore A
						data.push(Asm::Neg);               // A = -A
						// }
						data.push(Asm::Label(a_check));
						// while a > b {
						data.push(Asm::Jr(check.clone()));
						data.push(Asm::Label(div_loop.clone()));
						//   a -= b;
						data.push(Asm::Label(check));
						data.push(Asm::Sub(R8::B));
						data.push(Asm::JrNC(div_loop));
						// }
						data.push(Asm::Add(R8::B));
						data.push(Asm::Mov(R8::C, R8::A)); // C = remainder
						data.push(Asm::Mov(R8::A, R8::E));
						data.push(Asm::Xor(R8::D));
						// if A^B == 0 goto inv_rem
						data.push(Asm::Mov(R8::A, R8::B)); // A = B
						data.push(Asm::JrNZ(rem_check.clone()));
						data.push(Asm::Xor(R8::A));        // A = 0
						data.push(Asm::Label(rem_check));
						data.push(Asm::Sub(R8::C));        // A = A - remainder
						data.push(Asm::Mov(R8::C, R8::A)); // C = A
						data.push(Asm::Mov(R8::A, R8::D)); // A = a_neg
						data.push(Asm::CpN(1));
						data.push(Asm::Mov(R8::A, R8::C)); // A = C
						data.push(Asm::JrZ(end.clone()));
						data.push(Asm::Neg);
						data.push(Asm::Label(end));
					}
					BinaryOp::ShL => {
						let shift_loop = inner_label(proc_name, &mut label_id);
						let end = inner_label(proc_name, &mut label_id);
						data.push(Asm::Mov(R8::C, R8::A));
						data.push(Asm::Mov(R8::A, R8::B));
						data.push(Asm::CpN(0));
						data.push(Asm::Mov(R8::A, R8::C));
						data.push(Asm::JrNZ(end.clone()));
						data.push(Asm::Label(shift_loop.clone()));
						data.push(Asm::Sla(R8::A));
						data.push(Asm::DJNZ(shift_loop));
						data.push(Asm::Label(end));
					}
					BinaryOp::ShR => {
						let shift_loop = inner_label(proc_name, &mut label_id);
						let end = inner_label(proc_name, &mut label_id);
						data.push(Asm::Mov(R8::C, R8::A));
						data.push(Asm::Mov(R8::A, R8::B));
						data.push(Asm::CpN(0));
						data.push(Asm::Mov(R8::A, R8::C));
						data.push(Asm::JrNZ(end.clone()));
						data.push(Asm::Label(shift_loop.clone()));
						data.push(Asm::Sra(R8::A));
						data.push(Asm::DJNZ(shift_loop));
						data.push(Asm::Label(end));
					}
					BinaryOp::BinAnd => data.push(Asm::And(R8::B)),
					BinaryOp::BinOr => data.push(Asm::Or(R8::B)),
					BinaryOp::BinXor => data.push(Asm::Xor(R8::B)),
					BinaryOp::LogAnd => {
						let set_true = inner_label(proc_name, &mut label_id);
						let end = inner_label(proc_name, &mut label_id);
						data.push(Asm::CpN(0));
						data.push(Asm::JrZ(set_true.clone()));
						data.push(Asm::Mov(R8::A, R8::B));
						data.push(Asm::CpN(0));
						data.push(Asm::JrZ(end.clone()));
						data.push(Asm::Label(set_true));
						data.push(Asm::Mov8(1));
						data.push(Asm::Label(end));
					}
					BinaryOp::LogOr => {
						let set_true = inner_label(proc_name, &mut label_id);
						let end = inner_label(proc_name, &mut label_id);
						data.push(Asm::CpN(0));
						data.push(Asm::JrNZ(set_true.clone()));
						data.push(Asm::Mov(R8::A, R8::B));
						data.push(Asm::CpN(0));
						data.push(Asm::JrNZ(end.clone()));
						data.push(Asm::Jr(end.clone()));
						data.push(Asm::Label(set_true));
						data.push(Asm::Mov8(1));
						data.push(Asm::Label(end));
					}
					BinaryOp::LogXor => {
						let a_is_true = inner_label(proc_name, &mut label_id);
						let set_true = inner_label(proc_name, &mut label_id);
						let end = inner_label(proc_name, &mut label_id);
						data.push(Asm::CpN(0));
						data.push(Asm::Mov(R8::A, R8::B));
						data.push(Asm::JrZ(a_is_true.clone()));
						data.push(Asm::CpN(0));
						data.push(Asm::JrZ(set_true.clone()));
						data.push(Asm::Xor(R8::A));
						data.push(Asm::Jr(end.clone()));
						data.push(Asm::Label(a_is_true));
						data.push(Asm::CpN(0));
						data.push(Asm::JrZ(end.clone()));
						data.push(Asm::Label(set_true));
						data.push(Asm::Inc(R8::A));
						data.push(Asm::Label(end));
					}
					BinaryOp::CmpEQ => {
						let end = inner_label(proc_name, &mut label_id);
						data.push(Asm::Cp(R8::B));
						data.push(Asm::Mov8(0));
						data.push(Asm::JrNZ(end.clone()));
						data.push(Asm::Mov8(1));
						data.push(Asm::Label(end));
					}
					BinaryOp::CmpNE => {
						let end = inner_label(proc_name, &mut label_id);
						data.push(Asm::Cp(R8::B));
						data.push(Asm::Mov8(1));
						data.push(Asm::JrNZ(end.clone()));
						data.push(Asm::Mov8(0));
						data.push(Asm::Label(end));
					}
					BinaryOp::CmpGE => {
						let end = inner_label(proc_name, &mut label_id);
						data.push(Asm::Cp(R8::B));
						data.push(Asm::Mov8(1));
						data.push(Asm::JrNC(end.clone()));
						data.push(Asm::Mov8(0));
						data.push(Asm::Label(end));
					}
					BinaryOp::CmpGT => {
						let end = inner_label(proc_name, &mut label_id);
						data.push(Asm::Cp(R8::B));
						data.push(Asm::Mov8(0));
						data.push(Asm::JrZ(end.clone()));
						data.push(Asm::JrC(end.clone()));
						data.push(Asm::Mov8(1));
						data.push(Asm::Label(end));
					}
					BinaryOp::CmpLE => {
						let end = inner_label(proc_name, &mut label_id);
						data.push(Asm::Cp(R8::B));
						data.push(Asm::Mov8(1));
						data.push(Asm::JrZ(end.clone()));
						data.push(Asm::JrC(end.clone()));
						data.push(Asm::Mov8(0));
						data.push(Asm::Label(end));
					}
					BinaryOp::CmpLT => {
						let end = inner_label(proc_name, &mut label_id);
						data.push(Asm::Cp(R8::B));
						data.push(Asm::Mov8(0));
						data.push(Asm::JrNC(end.clone()));
						data.push(Asm::Mov8(1));
						data.push(Asm::Label(end));
					}
				}
				data.push(Asm::StIntoHL(R8::A));
			}
			Vsmc::UnOp(op) => {
				data.push(Asm::LdWithHL(R8::A));
				match op {
					UnaryOp::Neg => data.push(Asm::Neg),
					UnaryOp::Not => data.push(Asm::Cpl),
				}
				data.push(Asm::StIntoHL(R8::A));
			}
			Vsmc::Load(stack_idx) => {
				// load `stack_base + stack_idx` to -(HL)
				todo!("ld -(HL),(stack_base + {stack_idx})")
			}
			Vsmc::Store(stack_idx) => {
				// store (HL)+ to `stack_base + stack_idx`
				todo!("ld (stack_base + {stack_idx}),(HL)+")
			}
			Vsmc::Label(id) => data.push(Asm::Label(format!("{proc_name}_{id}"))),
			Vsmc::Jump(id) => {
				let label = format!("{proc_name}_{id}");
				data.push(Asm::Jp(label));
			}
			Vsmc::JumpIf(id) => {
				let label = format!("{proc_name}_{id}");
				data.push(Asm::LdWithHL(R8::A));
				data.push(Asm::Cpl);
				data.push(Asm::JpC(Cond::NZ,label));
			}
			Vsmc::Return(with_value) => {
				if with_value {
					data.push(Asm::LdWithHL(R8::A));
				}
				data.push(Asm::Ret);
			}
		}
	}

	data
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum R8 { A, B, C, D, E, H, L }
impl Display for R8 {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "{self:?}")
	}
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum R16 { BC, DE, HL }
impl Display for R16 {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "{self:?}")
	}
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Cond { NC, NZ, C, Z, PO, PE, P, M }
impl Display for Cond {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "{self:?}")
	}
}

#[allow(dead_code)]
#[derive(Debug)]
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
#[derive(Debug)]
pub enum Asm {
	Label(String),

	Nop,
	/// ld r,r
	Mov(R8, R8),
	/// ld A,n
	Mov8(u8),
	/// ld BC,nn | ld DE,nn | ld HL,nn
	Mov16(R16, u16),
	/// ld SP,nn
	MovSP(u16),
	/// ld SP,HL
	MovAdr,

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
	/// ld (HL),n
	St8(u8),

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
	/// add A | add B | add C | add D | add E | add H | add L
	Add(R8),
	/// add (HL)
	AddMem,
	/// add n
	AddN(u8),
	/// adc A | adc B | adc C | adc D | adc E | adc H | adc L
	AdC(R8),
	/// adc (HL)
	AdCMem,
	/// adc n
	AdCN(u8),

	/// sub A | sub B | sub C | sub D | sub E | sub H | sub L
	Sub(R8),
	/// sub (HL)
	SubMem,
	/// sub n
	SubN(u8),
	/// sbc A | sbc B | sbc C | sbc D | sbc E | sbc H | sbc L
	SbC(R8),
	/// sbc (HL)
	SbCMem,
	/// sbc n
	SbCN(u8),

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
	/// xor a | xor b | xor c | xor d | xor e | xor h | xor l
	Xor(R8),
	/// xor (HL)
	XorMem,
	/// xor n
	XorN(u8),
	/// or a | or b | or c | or d | or e | or h | or l
	Or(R8),
	/// or (HL)
	OrMem,
	/// or n
	OrN(u8),
	/// cp a | cp b | cp c | cp d | cp e | cp h | cp l
	Cp(R8),
	/// cp (HL)
	CpMem,
	/// cp n
	CpN(u8),
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
			Self::Label(label) => write!(f, "{label}:"),
			Self::Nop => write!(f, "  nop"),
			Self::Mov(rd,rs) => write!(f, "  ld {rd},{rs}"),
			Self::Mov8(n) => write!(f, "  ld a,{n}"),
			Self::Mov16(rd,nn) => write!(f, "  ld {rd},{nn}"),
			Self::MovSP(nn) => write!(f, "  ld SP,{nn}"),
			Self::MovAdr => write!(f, "  ld SP,HL"),
			Self::LdAWithBC => write!(f, "  ld A,(BC)"),
			Self::LdAWithDE => write!(f, "  ld A,(DE)"),
			Self::LdAMem(nn) => write!(f, "  ld A,({nn})"),
			Self::LdHL(nn) => write!(f, "  ld HL,({nn})"),
			Self::LdWithHL(rd) => write!(f, "  ld {rd},(HL)"),
			Self::StAIntoBC => write!(f, "  ld (BC),A"),
			Self::StAIntoDE => write!(f, "  ld (DE),A"),
			Self::StAMem(nn) => write!(f, "  ld ({nn}),A"),
			Self::StHL(nn) => write!(f, "  ld ({nn}),HL"),
			Self::StIntoHL(rd) => write!(f, "  ld (HL),{rd}"),
			Self::St8(n) => write!(f, "  ld (HL),{n}"),
			Self::Inc(rd) => write!(f, "  inc {rd}"),
			Self::Inc16(rd) => write!(f, "  inc {rd}"),
			Self::IncSP => write!(f, "  inc SP"),
			Self::IncMem => write!(f, "  inc (HL)"),
			Self::Dec(rd) => write!(f, "  dec {rd}"),
			Self::Dec16(rd) => write!(f, "  dec {rd}"),
			Self::DecSP => write!(f, "  dec SP"),
			Self::DecMem => write!(f, "  dec (HL)"),
			Self::DJNZ(d) => write!(f, "  djnz {d}"),
			Self::Jr(d) => write!(f, "  jr {d}"),
			Self::JrC(d) => write!(f, "  jr C,{d}"),
			Self::JrZ(d) => write!(f, "  jr Z,{d}"),
			Self::JrNC(d) => write!(f, "  jr NC,{d}"),
			Self::JrNZ(d) => write!(f, "  jr NZ,{d}"),
			Self::Jp(nn) => write!(f, "  jp {nn}"),
			Self::JpHL => write!(f, "  jp (HL)"),
			Self::JpC(cc, nn) => write!(f, "  jp {cc},{nn}"),
			Self::Add16(rs) => write!(f, "  add HL,{rs}"),
			Self::AddSP => write!(f, "  add HL,SP"),
			Self::Add(rs) => write!(f, "  add {rs}"),
			Self::AddMem => write!(f, "  add (HL)"),
			Self::AddN(n) => write!(f, "  add {n}"),
			Self::AdC(rs) => write!(f, "  adc {rs}"),
			Self::AdCMem => write!(f, "  adc (HL)"),
			Self::AdCN(n) => write!(f, "  adc {n}"),
			Self::Sub(rs) => write!(f, "  sub {rs}"),
			Self::SubMem => write!(f, "  sub (HL)"),
			Self::SubN(n) => write!(f, "  sub {n}"),
			Self::SbC(rs) => write!(f, "  sbc {rs}"),
			Self::SbCMem => write!(f, "  sbc (HL)"),
			Self::SbCN(n) => write!(f, "  sbc {n}"),
			Self::Rlc(rd) => write!(f, "  rlc {rd}"),
			Self::RlcHL => write!(f, "  rlc (HL)"),
			Self::Rl(rd) => write!(f, "  rl {rd}"),
			Self::RlHL => write!(f, "  rl (HL)"),
			Self::Rrc(rd) => write!(f, "  rrc {rd}"),
			Self::RrcHL => write!(f, "  rrc (HL)"),
			Self::Rr(rd) => write!(f, "  rr {rd}"),
			Self::RrHL => write!(f, "  rr (HL)"),
			Self::Sla(rd) => write!(f, "  sla {rd}"),
			Self::SlaHL => write!(f, "  sla (HL)"),
			Self::Sra(rd) => write!(f, "  sra {rd}"),
			Self::SraHL => write!(f, "  sra (HL)"),
			Self::Sll(rd) => write!(f, "  sll {rd}"),
			Self::SllHL => write!(f, "  sll (HL)"),
			Self::Srl(rd) => write!(f, "  srl {rd}"),
			Self::SrlHL => write!(f, "  srl (HL)"),
			Self::DAA => write!(f, "  daa"),
			Self::Cpl => write!(f, "  cpl"),
			Self::SCF => write!(f, "  scf"),
			Self::CCF => write!(f, "  ccf"),
			Self::Exx => write!(f, "  exx"),
			Self::ExAF => write!(f, "  ex AF,AF'"),
			Self::ExSP => write!(f, "  ex (SP),HL"),
			Self::ExDE => write!(f, "  ex DE,HL"),
			Self::Halt => write!(f, "  halt"),
			Self::And(rs) => write!(f, "  and {rs}"),
			Self::AndMem => write!(f, "  and (HL)"),
			Self::AndN(n) => write!(f, "  and {n}"),
			Self::Xor(rs) => write!(f, "  xor {rs}"),
			Self::XorMem => write!(f, "  xor (HL)"),
			Self::XorN(n) => write!(f, "  xor {n}"),
			Self::Or(rs) => write!(f, "  or {rs}"),
			Self::OrMem => write!(f, "  or (HL)"),
			Self::OrN(n) => write!(f, "  or {n}"),
			Self::Cp(rs) => write!(f, "  cp {rs}"),
			Self::CpMem => write!(f, "  cp (HL)"),
			Self::CpN(n) => write!(f, "  cp {n}"),
			Self::Neg => write!(f, "  neg"),
			Self::Ret => write!(f, "  ret"),
			Self::RetC(cc) => write!(f, "  ret {cc}"),
			Self::Rst(tgt) => write!(f, "  rst {tgt}"),
			Self::Pop(rs) => write!(f, "  pop {rs}"),
			Self::PopAF => write!(f, "  pop AF"),
			Self::Push(rs) => write!(f, "  push {rs}"),
			Self::PushAF => write!(f, "  push AF"),
			Self::IrqEn(false) => write!(f, "  di"),
			Self::IrqEn(true) => write!(f, "  ei"),
			Self::In(n) => write!(f, "  in A,({n})"),
			Self::Out(n) => write!(f, "  out ({n}),A"),
			Self::Call(nn) => write!(f, "  call {nn}"),
			Self::CallC(cc, nn) => write!(f, "  call {cc},{nn}"),
			Self::Bit(n,rs) => write!(f, "bit {n},{rs}"),
			Self::BitHL(n) => write!(f, "bit {n},(HL)"),
			Self::Res(n,rs) => write!(f, "res {n},{rs}"),
			Self::ResHL(n) => write!(f, "res {n},(HL)"),
			Self::Set(n,rs) => write!(f, "set {n},{rs}"),
			Self::SetHL(n) => write!(f, "set {n},(HL)"),
		}
	}
}
