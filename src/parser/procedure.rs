
use crate::ast::KindList;
use crate::identifier::{IdentId, Map as IdentMap};
use crate::Target;

use super::Type;

pub type ProcMap = IdentMap<Procedure>;

#[derive(Debug, Clone, PartialEq)]
pub struct Procedure {
	pub target: Option<Target>,
	pub params: Vec<(IdentId, Type)>,
	pub body: KindList,
	pub ret_type: Type,
}
