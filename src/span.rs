
use std::fmt::{Debug, Formatter, Result};
use std::ops::Range;

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span<T> {
	pub start: T,
	pub end: T,
}

impl<T> Span<T> {
	pub fn new(start: T, end: T) -> Self {
		Self { start, end }
	}
}

impl<T: Copy> Span<T> {
	pub fn point(start: T) -> Self {
		Self::new(start, start)
	}
}

impl<T: Debug> Debug for Span<T> {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "Span({:?},{:?})", self.start, self.end)
	}
}

impl<T> std::ops::Add for Span<T> {
	type Output = Self;
	fn add(self, rhs: Self) -> Self::Output {
		Self::new(self.start, rhs.end)
	}
}

impl<T> From<Range<T>> for Span<T> {
	fn from(value: Range<T>) -> Self {
		Self::new(value.start, value.end)
	}
}

impl<T> From<Span<T>> for Range<T> {
	fn from(value: Span<T>) -> Self {
		value.start .. value.end
	}
}

