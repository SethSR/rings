
use std::ops::{Add, Sub, Mul, Div, Rem};

pub type ValueMap = crate::identifier::Map<Value>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
	Integer(i64),
	Decimal(f64),
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
