
use std::fmt::{Display, Formatter, Result};

use crate::operators::{BinaryOp, UnaryOp};
use crate::vsmc::Vsmc;
use crate::ProcData;

pub fn lower(proc_name: &str, proc_data: &ProcData) -> Vec<Asm> {
	let mut data = vec![];

	let section = proc_data.tac_data.as_ref()
		.expect("section data in ProcData");

	data.push(Asm::Label(if proc_name == "main" {
		"_start"
	} else {
		proc_name
	}.to_string()));
	data.push(Asm::Enter(section.locals.len() as u16, 0));

	for code in &section.instructions {
		match code {
			Vsmc::BinOp(op) => {
				data.push(Asm::Pop(Reg::Rbx));
				data.push(Asm::Pop(Reg::Rax));
				match op {
					BinaryOp::Add => {
						data.push(Asm::Add(Reg::Rbx, Reg::Rax));
					}
					BinaryOp::Sub => {
						data.push(Asm::Sub(Reg::Rbx, Reg::Rax));
					}
					BinaryOp::Mul => {
						data.push(Asm::IMul(Reg::Rbx));
					}
					BinaryOp::Div => {
						data.push(Asm::IDiv(Reg::Rbx));
					}
					BinaryOp::Mod => {
						data.push(Asm::IDiv(Reg::Rbx));
						data.push(Asm::Mov(Reg::Rdx, Reg::Rax));
					}
					BinaryOp::ShL => {
						data.push(Asm::Mov(Reg::Rbx, Reg::Rcx));
						data.push(Asm::ShL(Reg::Rax));
					}
					BinaryOp::ShR => {
						data.push(Asm::Mov(Reg::Rbx, Reg::Rcx));
						data.push(Asm::ShR(Reg::Rax));
					}
					BinaryOp::BinAnd => {
						data.push(Asm::And(Reg::Rbx, Reg::Rax))
					}
					BinaryOp::BinOr => {
						data.push(Asm::Or(Reg::Rbx, Reg::Rax))
					}
					BinaryOp::BinXor => {
						data.push(Asm::Xor(Reg::Rbx, Reg::Rax))
					}
					BinaryOp::LogAnd => {
						data.push(Asm::CmpI(0, Reg::Rbx));
						data.push(Asm::SetNZ(Reg::Rbx));
						data.push(Asm::CmpI(0, Reg::Rax));
						data.push(Asm::SetNZ(Reg::Rax));
						data.push(Asm::And(Reg::Rbx, Reg::Rax));
					}
					BinaryOp::LogOr => {
						data.push(Asm::CmpI(0, Reg::Rbx));
						data.push(Asm::SetNZ(Reg::Rbx));
						data.push(Asm::CmpI(0, Reg::Rax));
						data.push(Asm::SetNZ(Reg::Rax));
						data.push(Asm::Or(Reg::Rbx, Reg::Rax));
					}
					BinaryOp::LogXor => {
						data.push(Asm::CmpI(0, Reg::Rbx));
						data.push(Asm::SetNZ(Reg::Rbx));
						data.push(Asm::CmpI(0, Reg::Rax));
						data.push(Asm::SetNZ(Reg::Rax));
						data.push(Asm::Xor(Reg::Rbx, Reg::Rax));
					}
					BinaryOp::CmpEQ => {
						data.push(Asm::Cmp(Reg::Rbx, Reg::Rax));
						data.push(Asm::SetEQ(Reg::Rax));
					}
					BinaryOp::CmpNE => {
						data.push(Asm::Cmp(Reg::Rbx, Reg::Rax));
						data.push(Asm::SetNE(Reg::Rax));
					}
					BinaryOp::CmpGE => {
						data.push(Asm::Cmp(Reg::Rbx, Reg::Rax));
						data.push(Asm::SetGE(Reg::Rax));
					}
					BinaryOp::CmpGT => {
						data.push(Asm::Cmp(Reg::Rbx, Reg::Rax));
						data.push(Asm::SetGT(Reg::Rax));
					}
					BinaryOp::CmpLE => {
						data.push(Asm::Cmp(Reg::Rax, Reg::Rbx));
						data.push(Asm::SetGT(Reg::Rax));
					}
					BinaryOp::CmpLT => {
						data.push(Asm::Cmp(Reg::Rax, Reg::Rbx));
						data.push(Asm::SetGE(Reg::Rax));
					}
				}
				data.push(Asm::Push(Reg::Rax));
			}
			Vsmc::Jump(id) => {
				data.push(Asm::Jmp(format!("{proc_name}_{id}")));
			}
			Vsmc::UnOp(op) => {
				data.push(Asm::Pop(Reg::Rax));
				match op {
					UnaryOp::Not => {
						data.push(Asm::Not(Reg::Rax));
					}
					UnaryOp::Neg => {
						data.push(Asm::Neg(Reg::Rax));
					}
				}
				data.push(Asm::Push(Reg::Rax));
			}
			Vsmc::Label(id) => {
				data.push(Asm::Label(format!("{proc_name}_{id}")));
			}
			Vsmc::JumpIf(id) => {
				data.push(Asm::Pop(Reg::Rax));
				data.push(Asm::CmpI(1, Reg::Rax));
				data.push(Asm::JE(format!("{proc_name}_{id}")));
			}
			Vsmc::Load(value) => {
				data.push(Asm::Load(-(*value as i32), Reg::Rbp, Reg::Rax));
				data.push(Asm::Push(Reg::Rax));
			}
			Vsmc::Store(value) => {
				data.push(Asm::Pop(Reg::Rax));
				data.push(Asm::Store(Reg::Rax, -(*value as i32), Reg::Rbp));
			}
			Vsmc::Return(with_value) => {
				if *with_value {
					data.push(Asm::Pop(Reg::Rax));
				}
				data.push(Asm::Leave);

				if proc_name == "main" {
					data.push(Asm::MovI(60, Reg::Rax));
					data.push(Asm::MovI(0, Reg::Rdi));
					data.push(Asm::SysCall);
				} else {
					data.push(Asm::Ret);
				}
			}
			Vsmc::Push(value) => {
				data.push(Asm::PushI(*value));
			}
		}
	}

	data
}

#[derive(Debug)]
pub enum Reg {
	Rax,
	Rbx,
	Rcx,
	Rdx,
	Rdi,
	Rbp,
}

impl Display for Reg {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::Rax => write!(f, "rax"),
			Self::Rbx => write!(f, "rbx"),
			Self::Rcx => write!(f, "rcx"),
			Self::Rdx => write!(f, "rdx"),
			Self::Rdi => write!(f, "rdi"),
			Self::Rbp => write!(f, "rbp"),
		}
	}
}

#[derive(Debug, Default)]
pub enum Asm {
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
	Ret,
	SysCall,
}

impl Display for Asm {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::Nop => write!(f, "\tnop"),
			Self::Label(label) => write!(f, "{label}:"),
			Self::Enter(imm16,imm8) => write!(f, "\tenter ${imm16},${imm8}"),
			Self::Leave => write!(f, "\tleave"),
			Self::Load(imm32,rs,rd) => write!(f, "\tmov {imm32}(%{rs}),%{rd}"),
			Self::Store(rs,imm32,rd) => write!(f, "\tmov %{rs},{imm32}(%{rd})"),
			Self::Mov(rs,rd) => write!(f, "\tmov ${rs},%{rd}"),
			Self::MovI(imm32,rd) => write!(f, "\tmov ${imm32},%{rd}"),
			Self::Cmp(rs,rd) => write!(f, "\tcmp ${rs},{rd}"),
			Self::CmpI(imm32,reg) => write!(f, "\tcmp ${imm32},%{reg}"),
			Self::Pop(rs) => write!(f, "\tpop %{rs}"),
			Self::Push(rd) => write!(f, "\tpush %{rd}"),
			Self::PushI(imm32) => write!(f, "\tpush ${imm32}"),
			Self::ShL(rd) => write!(f, "\tshl %rcx,%{rd}"),
			Self::ShR(rd) => write!(f, "\tshr %rcx,%{rd}"),
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
			Self::Ret => write!(f, "\tret"),
			Self::SysCall => write!(f, "\tsyscall"),
		}
	}
}
