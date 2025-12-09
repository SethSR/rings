
use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	ShL,
	ShR,
	BinAnd,
	BinOr,
	BinXor,
	LogAnd,
	LogOr,
	LogXor,
	CmpEQ,
	CmpNE,
	CmpGE,
	CmpGT,
	CmpLE,
	CmpLT,
}

impl BinaryOp {
	pub fn binding_power(&self) -> usize {
		match self {
			BinaryOp::Add | BinaryOp::Sub => 20,
			BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 30,
			BinaryOp::ShL | BinaryOp::ShR => 40,
			BinaryOp::LogAnd | BinaryOp::LogOr | BinaryOp::LogXor => 50,
			BinaryOp::BinAnd | BinaryOp::BinOr | BinaryOp::BinXor => 60,
			BinaryOp::CmpEQ | BinaryOp::CmpNE |
			BinaryOp::CmpGE | BinaryOp::CmpGT |
			BinaryOp::CmpLE | BinaryOp::CmpLT => 70,
		}
	}
}

impl Display for BinaryOp {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::Add => write!(f, "+"),
			Self::Sub => write!(f, "-"),
			Self::Mul => write!(f, "*"),
			Self::Div => write!(f, "/"),
			Self::Mod => write!(f, "%"),
			Self::ShL => write!(f, "<<"),
			Self::ShR => write!(f, ">>"),
			Self::BinAnd => write!(f, "&"),
			Self::BinOr => write!(f, "|"),
			Self::BinXor => write!(f, "^"),
			Self::LogAnd => write!(f, "&&"),
			Self::LogOr => write!(f, "||"),
			Self::LogXor => write!(f, "^^"),
			Self::CmpEQ => write!(f, "=="),
			Self::CmpNE => write!(f, "!="),
			Self::CmpGE => write!(f, ">="),
			Self::CmpGT => write!(f, ">"),
			Self::CmpLE => write!(f, "<="),
			Self::CmpLT => write!(f, "<"),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
	Neg,
	Not,
}

impl Display for UnaryOp {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::Neg => write!(f, "-"),
			Self::Not => write!(f, "!"),
		}
	}
}

