
use std::ops::{Add, Sub, Mul, Div, Rem};

use crate::parser::ast::KindList;
use crate::identifier::{IdentId, Map as IdentMap};
use crate::{Span, Target};

use super::{MemoryPlacement, Type};

pub type ValueMap = IdentMap<Value>;
pub type RegionMap = IdentMap<Region>;
pub type TypeMap = std::collections::HashMap<(IdentId, u16, IdentId), Type>;
pub type RecordMap = IdentMap<Record>;
pub type TableMap = IdentMap<Table>;
pub type ProcMap = IdentMap<Procedure>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
	Integer(i64),
	Decimal(f64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
	pub fields: Vec<(IdentId, Type)>,
	pub placement: Option<MemoryPlacement>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Region {
	pub span: Span<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Table {
	pub row_count: u16,
	pub placement: Option<MemoryPlacement>,
	pub fields: Vec<(IdentId, Type)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Procedure {
	pub target: Option<Target>,
	pub params: Vec<(IdentId, Type)>,
	pub body: KindList,
	pub ret_type: Type,
}

impl Region {
	pub fn new(start: u32, end: u32) -> Self {
		Self {
			span: Span { start, end }
		}
	}
}

impl Add for Value {
	type Output = Self;
	fn add(self, rhs: Self) -> Self {
		match (self, rhs) {
			(Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs + rhs),
			(Value::Integer(lhs), Value::Decimal(rhs)) => Value::Decimal(lhs as f64 + rhs),
			(Value::Decimal(lhs), Value::Integer(rhs)) => Value::Decimal(lhs + rhs as f64),
			(Value::Decimal(lhs), Value::Decimal(rhs)) => Value::Decimal(lhs + rhs),
		}
	}
}

impl Sub for Value {
	type Output = Self;
	fn sub(self, rhs: Self) -> Self {
		match (self, rhs) {
			(Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs - rhs),
			(Value::Integer(lhs), Value::Decimal(rhs)) => Value::Decimal(lhs as f64 - rhs),
			(Value::Decimal(lhs), Value::Integer(rhs)) => Value::Decimal(lhs - rhs as f64),
			(Value::Decimal(lhs), Value::Decimal(rhs)) => Value::Decimal(lhs - rhs),
		}
	}
}

impl Mul for Value {
	type Output = Self;
	fn mul(self, rhs: Self) -> Self {
		match (self, rhs) {
			(Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs * rhs),
			(Value::Integer(lhs), Value::Decimal(rhs)) => Value::Decimal(lhs as f64 * rhs),
			(Value::Decimal(lhs), Value::Integer(rhs)) => Value::Decimal(lhs * rhs as f64),
			(Value::Decimal(lhs), Value::Decimal(rhs)) => Value::Decimal(lhs * rhs),
		}
	}
}

impl Div for Value {
	type Output = Self;
	fn div(self, rhs: Self) -> Self {
		match (self, rhs) {
			(Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs / rhs),
			(Value::Integer(lhs), Value::Decimal(rhs)) => Value::Decimal(lhs as f64 / rhs),
			(Value::Decimal(lhs), Value::Integer(rhs)) => Value::Decimal(lhs / rhs as f64),
			(Value::Decimal(lhs), Value::Decimal(rhs)) => Value::Decimal(lhs / rhs),
		}
	}
}

impl Rem for Value {
	type Output = Self;
	fn rem(self, rhs: Self) -> Self {
		match (self, rhs) {
			(Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs % rhs),
			(Value::Integer(lhs), Value::Decimal(rhs)) => Value::Decimal(lhs as f64 % rhs),
			(Value::Decimal(lhs), Value::Integer(rhs)) => Value::Decimal(lhs % rhs as f64),
			(Value::Decimal(lhs), Value::Decimal(rhs)) => Value::Decimal(lhs % rhs),
		}
	}
}
