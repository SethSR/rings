
use std::fmt::{Display, Formatter, Result};
use crate::operators::{BinaryOp, UnaryOp};
use crate::ProcData;
use crate::vsmc::Vsmc;

pub fn lower(proc_name: &str, proc_data: &mut ProcData) -> Vec<Asm> {
	let section = proc_data.tac_data.as_mut().unwrap();

	// The base address for the variable stack
	const VAR_SP: Addr = Addr::A6;
	// The base address for the function (call) stack
	const FUN_SP: Addr = Addr::A7;

	let mut data = vec![
		Asm::Label(proc_name.to_string()),
	];
	for asm in section.instructions.clone() {
		match asm {
			Vsmc::Push(value) => {
				data.push(Asm::Move(Sz::W, EA::Imm(value), EA::Pre(FUN_SP)));
			}
			Vsmc::BinOp(op) => {
				data.push(Asm::Move(Sz::W, EA::Pst(FUN_SP), EA::Dat(Data::D0)));
				match op {
					BinaryOp::Add => data.push(Asm::Add(Sz::W, Data::D0, EA::Ind(FUN_SP))),
					BinaryOp::Sub => data.push(Asm::Sub(Sz::W, Data::D0, EA::Ind(FUN_SP))),
					BinaryOp::Mul => {
						data.push(Asm::MulS(EA::Ind(FUN_SP), Data::D0));
						data.push(Asm::Move(Sz::W, EA::Dat(Data::D0), EA::Ind(FUN_SP)));
					}
					BinaryOp::Div => {
						data.push(Asm::Move(Sz::L, EA::Ind(FUN_SP), EA::Dat(Data::D1)));
						data.push(Asm::DivS(EA::Dat(Data::D0), Data::D1));
						data.push(Asm::Move(Sz::W, EA::Dat(Data::D1), EA::Ind(FUN_SP)));
					}
					BinaryOp::Mod => {
						data.push(Asm::Move(Sz::L, EA::Ind(FUN_SP), EA::Dat(Data::D1)));
						data.push(Asm::DivS(EA::Dat(Data::D0), Data::D1));
						data.push(Asm::Swap(Data::D1));
						data.push(Asm::Move(Sz::W, EA::Dat(Data::D1), EA::Ind(FUN_SP)));
					}
					BinaryOp::ShL => {
						data.push(Asm::Move(Sz::W, EA::Ind(FUN_SP), EA::Dat(Data::D1)));
						data.push(Asm::Asl(Data::D0, Data::D1));
						data.push(Asm::Move(Sz::W, EA::Dat(Data::D1), EA::Ind(FUN_SP)));
					}
					BinaryOp::ShR => {
						data.push(Asm::Move(Sz::W, EA::Ind(FUN_SP), EA::Dat(Data::D1)));
						data.push(Asm::Asr(Data::D0, Data::D1));
						data.push(Asm::Move(Sz::W, EA::Dat(Data::D1), EA::Ind(FUN_SP)));
					}
					BinaryOp::BinAnd => data.push(Asm::And(Sz::W, Data::D0, EA::Ind(FUN_SP))),
					BinaryOp::BinOr => data.push(Asm::Or(Sz::W, Data::D0, EA::Ind(FUN_SP))),
					BinaryOp::BinXor => data.push(Asm::Eor(Sz::W, Data::D0, EA::Ind(FUN_SP))),
					BinaryOp::LogAnd => {
						data.push(Asm::Tst(Sz::W, EA::Dat(Data::D0)));
						data.push(Asm::Scc(Cond::NE, EA::Dat(Data::D0)));
						data.push(Asm::Tst(Sz::W, EA::Ind(FUN_SP)));
						data.push(Asm::Scc(Cond::NE, EA::Ind(FUN_SP)));
						data.push(Asm::And(Sz::W, Data::D0, EA::Ind(FUN_SP)));
					}
					BinaryOp::LogOr => {
						data.push(Asm::Tst(Sz::W, EA::Dat(Data::D0)));
						data.push(Asm::Scc(Cond::NE, EA::Dat(Data::D0)));
						data.push(Asm::Tst(Sz::W, EA::Ind(FUN_SP)));
						data.push(Asm::Scc(Cond::NE, EA::Ind(FUN_SP)));
						data.push(Asm::Or(Sz::W, Data::D0, EA::Ind(FUN_SP)));
					}
					BinaryOp::LogXor => {
						data.push(Asm::Tst(Sz::W, EA::Dat(Data::D0)));
						data.push(Asm::Scc(Cond::NE, EA::Dat(Data::D0)));
						data.push(Asm::Tst(Sz::W, EA::Ind(FUN_SP)));
						data.push(Asm::Scc(Cond::NE, EA::Ind(FUN_SP)));
						data.push(Asm::Eor(Sz::W, Data::D0, EA::Ind(FUN_SP)));
					}
					BinaryOp::CmpEQ => {
						data.push(Asm::Cmp(Sz::W, EA::Ind(FUN_SP), Data::D0));
						data.push(Asm::Scc(Cond::EQ, EA::Ind(FUN_SP)))
					}
					BinaryOp::CmpNE => {
						data.push(Asm::Cmp(Sz::W, EA::Ind(FUN_SP), Data::D0));
						data.push(Asm::Scc(Cond::NE, EA::Ind(FUN_SP)))
					}
					BinaryOp::CmpGE => {
						data.push(Asm::Cmp(Sz::W, EA::Ind(FUN_SP), Data::D0));
						data.push(Asm::Scc(Cond::LT, EA::Ind(FUN_SP)))
					}
					BinaryOp::CmpGT => {
						data.push(Asm::Cmp(Sz::W, EA::Ind(FUN_SP), Data::D0));
						data.push(Asm::Scc(Cond::LE, EA::Ind(FUN_SP)))
					}
					BinaryOp::CmpLE => {
						data.push(Asm::Cmp(Sz::W, EA::Ind(FUN_SP), Data::D0));
						data.push(Asm::Scc(Cond::GT, EA::Ind(FUN_SP)))
					}
					BinaryOp::CmpLT => {
						data.push(Asm::Cmp(Sz::W, EA::Ind(FUN_SP), Data::D0));
						data.push(Asm::Scc(Cond::GE, EA::Ind(FUN_SP)))
					}
				}
			}
			Vsmc::UnOp(op) => {
				match op {
					UnaryOp::Neg => data.push(Asm::Neg(Sz::W, EA::Ind(FUN_SP))),
					UnaryOp::Not => data.push(Asm::Not(Sz::W, EA::Ind(FUN_SP))),
				}
			}
			Vsmc::Load(stack_idx) => {
				data.push(Asm::Move(Sz::W, EA::Dsp(stack_idx as i16, VAR_SP), EA::Pre(FUN_SP)));
			}
			Vsmc::Store(stack_idx) => {
				data.push(Asm::Move(Sz::W, EA::Pst(FUN_SP), EA::Dsp(stack_idx as i16, VAR_SP)));
			}
			Vsmc::Label(id) => {
				data.push(Asm::Label(format!("{proc_name}_{id}")));
			}
			Vsmc::Jump(id) => {
				data.push(Asm::Bra(format!("{proc_name}_{id}")));
			}
			Vsmc::JumpIf(id) => {
				data.push(Asm::Tst(Sz::W, EA::Pst(FUN_SP)));
				data.push(Asm::Bcc(Cond::NE, format!("{proc_name}_{id}")));
			}
			Vsmc::Return(with_value) => {
				if with_value {
					data.push(Asm::Move(Sz::W, EA::Pst(FUN_SP), EA::Dat(Data::D0)));
				}
				data.push(Asm::Rts);
			}
		}
	}
	data
}

#[derive(Debug)]
pub enum Sz { W, L }
impl Display for Sz {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::W => write!(f, "w"),
			Self::L => write!(f, "l"),
		}
	}
}

#[derive(Debug)]
pub enum Data { D0, D1, D2, D3, D4, D5, D6, D7 }
impl Display for Data {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "{self:?}")
	}
}

#[derive(Debug)]
pub enum Addr { A0, A1, A2, A3, A4, A5, A6, A7 }
impl Display for Addr {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "{self:?}")
	}
}

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
			Self::Dsp(d,a) => write!(f, "({d},{a})"),
			Self::Idx(d,a,x) => write!(f, "({d},{a},{x})"),
			Self::AbW(a) => write!(f, "({a}).W"),
			Self::AbL(a) => write!(f, "({a}).L"),
			Self::Imm(a) => write!(f, "#{a}"),
		}
	}
}

#[derive(Debug)]
pub enum Cond { EQ, NE, GE, GT, LE, LT }
impl Display for Cond {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::EQ => write!(f, "eq"),
			Self::NE => write!(f, "ne"),
			Self::GE => write!(f, "ge"),
			Self::GT => write!(f, "gt"),
			Self::LE => write!(f, "le"),
			Self::LT => write!(f, "lt"),
		}
	}
}

#[derive(Debug)]
pub enum Asm {
	Label(String),

	/// Syntax: `ADD.<sz> Dn,<ea>`
	///
	/// Size: (Byte, Word, Long)
	Add(Sz,Data,EA),
	/// Syntax: `AND.<sz> Dn,<ea>`
	///
	/// Size: (Byte, Word, Long)
	And(Sz,Data,EA),
	Asl(Data,Data),
	Asr(Data,Data),
	Bcc(Cond,String),
	Bra(String),
	Cmp(Sz,EA,Data),
	CmpI(Sz,i32,EA),
	DivS(EA,Data),
	Eor(Sz,Data,EA),
	Move(Sz,EA,EA),
	MulS(EA,Data),
	Neg(Sz,EA),
	Nop,
	Not(Sz,EA),
	Or(Sz,Data,EA),
	Rts,
	Scc(Cond,EA),
	/// Syntax: `SUB.<sz> Dn,<ea>`
	///
	/// Size: (Byte, Word, Long)
	Sub(Sz,Data,EA),
	Swap(Data),
	Tst(Sz,EA),
}

impl Display for Asm {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::Label(label) => write!(f, "{label}:"),

			Self::Add(sz,d,ea) => write!(f, "\tadd.{sz} {d},{ea}"),
			Self::And(sz,d,ea) => write!(f, "\tand.{sz} {d},{ea}"),
			Self::Asl(dx,dy) => write!(f, "\tasl {dx},{dy}"),
			Self::Asr(dx,dy) => write!(f, "\tasr {dx},{dy}"),
			Self::Bcc(cond,label) => write!(f, "\tb{cond} {label}"),
			Self::Bra(label) => write!(f, "\tbra {label}"),
			Self::Cmp(sz,ea,d) => write!(f, "\tcmp.{sz} {ea},{d}"),
			Self::CmpI(sz,i,ea) => write!(f, "\tcmpi.{sz} #{i},{ea}"),
			Self::DivS(ea,d) => write!(f, "\tdivs {ea},{d}"),
			Self::Eor(sz,d,ea) => write!(f, "\teor.{sz} {d},{ea}"),
			Self::Move(sz,src,dst) => write!(f, "\tmove.{sz} {src},{dst}"),
			Self::MulS(ea,d) => write!(f, "\tmuls {ea},{d}"),
			Self::Neg(sz,reg) => write!(f, "\tneg.{sz} {reg}"),
			Self::Nop => write!(f, "\tnop"),
			Self::Not(sz,reg) => write!(f, "\tnot.{sz} {reg}"),
			Self::Or(sz,d,ea) => write!(f, "\tor.{sz} {d},{ea}"),
			Self::Rts => write!(f, "\trts"),
			Self::Scc(cond,ea) => write!(f, "\ts{cond} {ea}"),
			Self::Sub(sz,d,ea) => write!(f, "\tsub.{sz} {d},{ea}"),
			Self::Swap(d) => write!(f, "\tswap {d}"),
			Self::Tst(sz,ea) => write!(f, "\ttst.{sz} {ea}"),
		}
	}
}
