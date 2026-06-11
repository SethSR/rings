
use std::fmt::{Display, Formatter, Result};

use crate::operators::{BinaryOp, UnaryOp};
use crate::tac::{Data as TacData, Location, TAC};

use super::{BasicToAsmConverter, Block};

/// Stack frame layout:
/// - Each virtual register occupies one 8-byte slot at a negative offset from rbp.
///
/// vreg N  -> -8 * (N+1)(%rbp)
fn vreg_offset(vr: u32) -> i32 {
	-8 * (vr as i32 + 1)
}

/// Stack frame layout:
/// - Each stack (local) variable occupies one 8-byte slot continuing below the vregs.
///
/// stack N -> -8 * (next_reg + N + 1)(%rbp)
fn stack_offset(idx: usize, next_reg: u32) -> i32 {
	-8 * (next_reg as i32 + idx as i32 + 1)
}

/// Total frame size in bytes, rounded up to 16-byte alignment.
fn frame_size(next_reg: u32, num_locals: usize) -> u16 {
	let raw = 8 * (next_reg as usize + num_locals);
	let aligned = (raw + 15) & !15;
	aligned as u16
}

/// Emit instructions that load a Location value into `r_x`.
fn load_to(reg: Reg, loc: &Location, next_reg: u32, data: &mut Vec<Asm>) {
	match loc {
		Location::Const(val,_) => {
			data.push(Asm::MovI(*val as u32, reg));
		}
		Location::VReg(vr,_) => {
			data.push(Asm::Load(vreg_offset(*vr), Reg::Rbp, reg));
		}
		Location::Stack(idx,_) => {
			data.push(Asm::Load(stack_offset(*idx, next_reg), Reg::Rbp, reg));
		}
		Location::Addr(addr,_) => {
			data.push(Asm::MovI(*addr, reg));
			data.push(Asm::Load(0, reg, reg));
		}
	}
}

/// Emit instructions that store `rax` into a Location (must be an lvalue).
fn store_from_rax(loc: &Location, next_reg: u32, data: &mut Vec<Asm>) {
	match loc {
		Location::Const(..) => panic!("cannot store into a Const location"),
		Location::VReg(vr,_) => {
			data.push(Asm::Store(Reg::Rax, vreg_offset(*vr), Reg::Rbp));
		}
		Location::Stack(idx,_) => {
			data.push(Asm::Store(Reg::Rax, stack_offset(*idx, next_reg), Reg::Rbp));
		}
		Location::Addr(addr,_) => {
			data.push(Asm::MovI(*addr, Reg::Rbx));
			data.push(Asm::Store(Reg::Rax, 0, Reg::Rbx));
		}
	}
}

pub fn lower(proc_name: &str, tac_data: TacData, stack_addr: u32) -> (Vec<Asm>, Vec<Block>) {
	let TacData {
		instructions,
		blocks,
		next_reg,
		locals,
		..
	} = tac_data;

	let num_locals = locals.len();
	let fsize = frame_size(next_reg, num_locals);

	let mut block_converter = BasicToAsmConverter::new(blocks);

	let mut data = vec![
		Asm::Label(proc_name.to_string()),
		// Standard x86-64 prologue
		Asm::Push(Reg::Rbp),
		Asm::Mov(Reg::Rsp, Reg::Rbp),
		Asm::Enter(fsize, 0),
	];

	for (idx, tac) in instructions.iter().enumerate() {
		block_converter.check(idx, data.len());

		match tac {
			TAC::Move { src, dst } => {
				data.push(Asm::Comment(format!("{src:?} -> {dst:?}")));
				load_to(Reg::Rax, src, next_reg, &mut data);
				store_from_rax(dst, next_reg, &mut data);
			}

			TAC::BinOp { op, lhs, rhs, dst } => {
				data.push(Asm::Comment(format!("{lhs:?} {op} {rhs:?} -> {dst:?}")));

				match op {
					BinaryOp::Add => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						load_to(Reg::Rbx, rhs, next_reg, &mut data);
						data.push(Asm::Add(Reg::Rbx, Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::Sub => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						load_to(Reg::Rbx, rhs, next_reg, &mut data);
						data.push(Asm::Sub(Reg::Rbx, Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::Mul => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						load_to(Reg::Rbx, rhs, next_reg, &mut data);
						data.push(Asm::IMul(Reg::Rbx));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::Div => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						// sign extend RAX to RDX
						data.push(Asm::Cdq);
						load_to(Reg::Rbx, rhs, next_reg, &mut data);
						data.push(Asm::IDiv(Reg::Rbx));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::Mod => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						// sign extend RAX to RDX
						data.push(Asm::Cdq);
						load_to(Reg::Rbx, rhs, next_reg, &mut data);
						data.push(Asm::IDiv(Reg::Rbx));
						data.push(Asm::Mov(Reg::Rdx, Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::BinAnd => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						load_to(Reg::Rbx, rhs, next_reg, &mut data);
						data.push(Asm::And(Reg::Rbx, Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::BinOr => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						load_to(Reg::Rbx, rhs, next_reg, &mut data);
						data.push(Asm::Or(Reg::Rbx, Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::BinXor => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						load_to(Reg::Rbx, rhs, next_reg, &mut data);
						data.push(Asm::Xor(Reg::Rbx, Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::ShL => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						load_to(Reg::Rcx, rhs, next_reg, &mut data);
						data.push(Asm::ShL(Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::ShR => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						load_to(Reg::Rcx, rhs, next_reg, &mut data);
						data.push(Asm::ShR(Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::CmpEQ => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						load_to(Reg::Rbx, rhs, next_reg, &mut data);
						data.push(Asm::Cmp(Reg::Rbx, Reg::Rax));
						data.push(Asm::SetEQ(Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::CmpNE => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						load_to(Reg::Rbx, rhs, next_reg, &mut data);
						data.push(Asm::Cmp(Reg::Rbx, Reg::Rax));
						data.push(Asm::SetNE(Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::CmpLT => {
						// Flip RAX and RBX to use '>' in place of '<'
						load_to(Reg::Rax, rhs, next_reg, &mut data);
						load_to(Reg::Rbx, lhs, next_reg, &mut data);
						data.push(Asm::Cmp(Reg::Rax, Reg::Rbx));
						data.push(Asm::SetGT(Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::CmpLE => {
						// Flip RAX and RBX to use '>=' in place of '<='
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						load_to(Reg::Rbx, rhs, next_reg, &mut data);
						data.push(Asm::Cmp(Reg::Rax, Reg::Rbx));
						data.push(Asm::SetGE(Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::CmpGT => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						load_to(Reg::Rbx, rhs, next_reg, &mut data);
						data.push(Asm::Cmp(Reg::Rbx, Reg::Rax));
						data.push(Asm::SetGT(Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::CmpGE => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						load_to(Reg::Rbx, rhs, next_reg, &mut data);
						data.push(Asm::Cmp(Reg::Rbx, Reg::Rax));
						data.push(Asm::SetGE(Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::LogAnd => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						data.push(Asm::CmpI(0, Reg::Rax));
						data.push(Asm::SetNZ(Reg::Rax));
						load_to(Reg::Rbx, lhs, next_reg, &mut data);
						data.push(Asm::CmpI(0, Reg::Rbx));
						data.push(Asm::SetNZ(Reg::Rbx));
						data.push(Asm::And(Reg::Rbx, Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::LogOr => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						data.push(Asm::CmpI(0, Reg::Rax));
						data.push(Asm::SetNZ(Reg::Rax));
						load_to(Reg::Rbx, lhs, next_reg, &mut data);
						data.push(Asm::CmpI(0, Reg::Rbx));
						data.push(Asm::SetNZ(Reg::Rbx));
						data.push(Asm::Or(Reg::Rbx, Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					BinaryOp::LogXor => {
						load_to(Reg::Rax, lhs, next_reg, &mut data);
						data.push(Asm::CmpI(0, Reg::Rax));
						data.push(Asm::SetNZ(Reg::Rax));
						load_to(Reg::Rbx, lhs, next_reg, &mut data);
						data.push(Asm::CmpI(0, Reg::Rbx));
						data.push(Asm::SetNZ(Reg::Rbx));
						data.push(Asm::Xor(Reg::Rbx, Reg::Rax));
						store_from_rax(dst, next_reg, &mut data);
					}
					_ => todo!("{op}"),
				}
			}

			TAC::UnOp { op, rhs, dst } => {
				data.push(Asm::Comment(format!("{op}{rhs:?} -> {dst:?}")));
				load_to(Reg::Rax, rhs, next_reg, &mut data);
				match op {
					UnaryOp::Neg => data.push(Asm::Neg(Reg::Rax)),
					UnaryOp::Not => data.push(Asm::Not(Reg::Rax)),
				}
				store_from_rax(dst, next_reg, &mut data);
			}

			TAC::Jump(lbl) => {
				data.push(Asm::Jmp(format!(".L{lbl}")));
			}

			TAC::JumpIf { lbl, vr } => {
				data.push(Asm::Load(vreg_offset(*vr), Reg::Rbp, Reg::Rax));
				data.push(Asm::CmpI(0, Reg::Rax));
				data.push(Asm::JNE(format!(".L{lbl}")));
			}

			#[cfg(feature="call")]
			TAC::Call { name, args, dst } => {
				// System V AMD64 ABI: first 6 integer args go in RDI, RSI, RDX, RCX, R8, R9
				let arg_regs = [Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx];
				for (i, arg) in args.iter().enumerate() {
					load_to(Reg::Rax, arg, next_reg, &mut data);

					if i < arg_regs.len() {
						data.push(Asm::Mov(Reg::Rax, arg_regs[i]));
					} else {
						data.push(Asm::Push(Reg::Rax));
					}
				}

				// TODO - srenshaw - This name is incorrect. We'll probably want to resolve the procedure
				// `name` in the TAC resolution stage.
				data.push(Asm::Call(format!("{name:?}")));

				if let Some(dst_vr) = dst {
					// Return value goes in RAX
					data.push(Asm::Store(Reg::Rax, vreg_offset(*dst_vr), Reg::Rbp));
				}
			}

			TAC::Return(Some(vr)) => {
				// Return values must be in RAX
				data.push(Asm::Load(vreg_offset(*vr), Reg::Rbp, Reg::Rax));
				data.push(Asm::Leave);
				data.push(Asm::Ret);
			}

			TAC::Return(None) => {
				data.push(Asm::Leave);
				data.push(Asm::Ret);
			}
		}
	}

	block_converter.finish(data.len());

	(data, block_converter.asm_blocks)
}

#[derive(Debug, Clone, Copy)]
pub enum Reg {
	Rax,
	Rbx,
	Rcx,
	Rdx,
	Rdi,
	Rsi,
	Rbp,
	Rsp,
}

impl Display for Reg {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::Rax => write!(f, "rax"),
			Self::Rbx => write!(f, "rbx"),
			Self::Rcx => write!(f, "rcx"),
			Self::Rdx => write!(f, "rdx"),
			Self::Rdi => write!(f, "rdi"),
			Self::Rsi => write!(f, "rsi"),
			Self::Rbp => write!(f, "rbp"),
			Self::Rsp => write!(f, "rsp"),
		}
	}
}

#[derive(Debug, Default)]
pub enum Asm {
	Comment(String),
	#[default]
	Nop,
	Label(String),
	Enter(u16,u8),
	Leave,
	Load(i32,Reg,Reg),
	Store(Reg,i32,Reg),
	Mov(Reg,Reg),
	MovI(u32,Reg),
	Cmp(Reg,Reg),
	CmpI(u32,Reg),
	Pop(Reg),
	Push(Reg),
	PushI(i32),
	ShL(Reg),
	ShR(Reg),
	Not(Reg),
	Neg(Reg),
	Add(Reg,Reg),
	Sub(Reg,Reg),
	IMul(Reg),
	IDiv(Reg),
	And(Reg,Reg),
	Or(Reg,Reg),
	Xor(Reg,Reg),
	SetEQ(Reg),
	SetNE(Reg),
	SetGE(Reg),
	SetGT(Reg),
	SetNZ(Reg),
	Jmp(String),
	JE(String),
	JNE(String),
	Call(String),
	Cdq,
	Ret,
	SysCall,
}

impl Display for Asm {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::Comment(s) => write!(f, "\t\t\t; {s}"),
			Self::Nop => write!(f, "\tnop"),
			Self::Label(label) => write!(f, "{label}:"),
			Self::Enter(imm16,imm8) => write!(f, "\tenter ${imm16},${imm8}"),
			Self::Leave => write!(f, "\tleave"),
			Self::Load(imm32,rs,rd) => write!(f, "\tmov {imm32}(%{rs}),%{rd}"),
			Self::Store(rs,imm32,rd) => write!(f, "\tmov %{rs},{imm32}(%{rd})"),
			Self::Mov(rs,rd) => write!(f, "\tmov %{rs},%{rd}"),
			Self::MovI(imm32,rd) => write!(f, "\tmov ${imm32},%{rd}"),
			Self::Cmp(rs,rd) => write!(f, "\tcmp %{rs},{rd}"),
			Self::CmpI(imm32,reg) => write!(f, "\tcmp ${imm32},%{reg}"),
			Self::Pop(rs) => write!(f, "\tpop %{rs}"),
			Self::Push(rd) => write!(f, "\tpush %{rd}"),
			Self::PushI(imm32) => write!(f, "\tpush ${imm32}"),
			Self::ShL(rd) => write!(f, "\tshl %cl,%{rd}"),
			Self::ShR(rd) => write!(f, "\tshr %cl,%{rd}"),
			Self::Not(rd) => write!(f, "\tnot %{rd}"),
			Self::Neg(rd) => write!(f, "\tneg %{rd}"),
			Self::Add(rs,rd) => write!(f, "\tadd %{rs},%{rd}"),
			Self::Sub(rs,rd) => write!(f, "\tsub %{rs},%{rd}"),
			Self::IMul(rs) => write!(f, "\timul %{rs}"),
			Self::IDiv(rs) => write!(f, "\tidiv %{rs}"),
			Self::And(rs,rd) => write!(f, "\tand %{rs},%{rd}"),
			Self::Or(rs,rd) => write!(f, "\tor %{rs},%{rd}"),
			Self::Xor(rs,rd) => write!(f, "\txor %{rs},%{rd}"),
			Self::SetEQ(rd) => write!(f, "\tsete %{rd}"),
			Self::SetNE(rd) => write!(f, "\tsetne %{rd}"),
			Self::SetGE(rd) => write!(f, "\tsetge %{rd}"),
			Self::SetGT(rd) => write!(f, "\tsetg %{rd}"),
			Self::SetNZ(rd) => write!(f, "\tsetnz %{rd}"),
			Self::Jmp(target) => write!(f, "\tjmp {target}"),
			Self::JE(target) => write!(f, "\tje {target}"),
			Self::JNE(target) => write!(f, "\tjne {target}"),
			Self::Call(target) => write!(f, "\tcall {target}"),
			Self::Cdq => write!(f, "\tcdq"),
			Self::Ret => write!(f, "\tret"),
			Self::SysCall => write!(f, "\tsyscall"),
		}
	}
}
