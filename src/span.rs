
use std::ops::Range;

#[derive(Debug, Default, Clone, Copy)]
pub struct Span<T> {
	pub start: T,
	pub end: T,
}

impl<T> std::ops::Add for Span<T> {
	type Output = Self;
	fn add(self, rhs: Self) -> Self::Output {
		Self { start: self.start, end: rhs.end }
	}
}

impl<T> From<Range<T>> for Span<T> {
	fn from(value: Range<T>) -> Self {
		Self {
			start: value.start,
			end: value.end,
		}
	}
}

