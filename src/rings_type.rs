
pub trait Meet {
	fn meet(&self, rhs: &Self) -> Self;
}

pub trait Dual {
	fn dual(&self) -> Self;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
	Top,
	S8(Lattice<i8>),
	Unit,
	Bot,
}

impl Type {
	pub fn top() -> Self { Self::Top }

	pub fn s8_top() -> Self { Self::S8(Lattice::Top) }
	pub fn s8_val(v: i8) -> Self { Self::S8(Lattice::Val(v)) }
	pub fn s8_bot() -> Self { Self::S8(Lattice::Bot) }

	pub fn unit() -> Self { Self::Unit }

	pub fn bot() -> Self { Self::Bot }
}

impl std::fmt::Display for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Self::Top => write!(f, "top"),
			Self::S8(_) => write!(f, "s8"),
			Self::Unit => write!(f, "unit"),
			Self::Bot => write!(f, "bot"),
		}
	}
}

impl Meet for Type {
	fn meet(&self, rhs: &Self) -> Self {
		match (self, rhs) {
			(Self::Top, _) => *rhs,
			(_, Self::Top) => *self,
			(Self::Bot, _) | (_, Self::Bot) => Self::Bot,

			(Self::S8(a), Self::S8(b)) => Self::S8(a.meet(b)),
			(Self::Unit, Self::Unit) => Self::Unit,

			_ => Self::Bot,
		}
	}
}

impl Dual for Type {
	fn dual(&self) -> Self {
		match self {
			Self::Top => Self::Bot,
			Self::S8(a) => Self::S8(a.dual()),
			Self::Unit => Self::Unit,
			Self::Bot => Self::Top,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Lattice<T> where T: Clone + Copy {
	Top, // Any/unknown type
	Val(T), // a known type
	Bot, // All/invalid type
}

impl<T: std::fmt::Display + Copy> std::fmt::Display for Lattice<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Self::Top => write!(f, "top"),
			Self::Val(v) => write!(f, "{v}"),
			Self::Bot => write!(f, "bot"),
		}
	}
}

impl<T: Clone + Copy + PartialEq> Meet for Lattice<T> {
	fn meet(&self, rhs: &Self) -> Self {
		match (self, rhs) {
			(Self::Top, _) => *rhs,
			(_, Self::Top) => *self,

			(Self::Bot, _) | (_, Self::Bot) => Self::Bot,

			(Self::Val(a), Self::Val(b)) if a == b => Self::Val(*a),

			_ => Self::Bot,
		}
	}
}

impl<T: Clone + Copy> Dual for Lattice<T> {
	fn dual(&self) -> Self {
		match self {
			Self::Top => Self::Bot,
			Self::Bot => Self::Top,
			Self::Val(t) => Self::Val(*t),
		}
	}
}

