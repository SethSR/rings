
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result};
use std::hash::{DefaultHasher, Hash, Hasher};

// TODO - srenshaw - We might want to switch to a HashMap with non-random state, so builds are deterministic.
pub type Map<T> = HashMap<IdentId, T>;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IdentId(u64);
impl Display for IdentId {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "<{}>", self.0)
	}
}

pub trait Identifier: Hash {
	fn id(&self) -> IdentId {
		let mut hasher = DefaultHasher::default();
		self.hash(&mut hasher);
		IdentId(hasher.finish())
	}
}

impl Identifier for String {}
impl Identifier for &String {}
impl Identifier for &mut String {}
impl Identifier for str {}
impl Identifier for &str {}
