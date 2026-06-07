
use std::fmt::{Debug, Formatter, Result as FmtResult};

use crate::error::{Error, Kind as ErrKind};
use crate::identifier::{IdentId, Map as IdentMap};
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::token::{Id as TokenId, Kind as TokenKind};
use crate::{SrcPos, Target};
use crate::token_source;

mod ast;
mod cursor;
mod data;
mod error;
mod expression;
mod parse_procedures;
mod process;
mod scan;
mod task;
mod types;

#[cfg(test)] mod value_tests;
#[cfg(test)] mod region_tests;
#[cfg(test)] mod record_tests;
#[cfg(test)] mod proc_tests;
#[cfg(test)] mod table_tests;

use expression::evaluate_expr;

pub use ast::{Ast, AstId, Kind as AstKind, PathSegment};
pub use data::{Procedure, Table, Value};
pub use data::{ProcMap, RecordMap, RegionMap, TableMap, ValueMap};
pub use types::{Type, TypeMap};
pub use process::MemoryPlacement;

/// The constructs recognized by the language
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
	Value,
	Region,
	Record,
	Table,
	Procedure,
}
pub type KindMap = IdentMap<Kind>;

#[derive(Default)]
pub struct Data<T> {
	pub kinds: KindMap,
	pub values: ValueMap,
	pub regions: RegionMap,
	pub records: RecordMap,
	pub tables: TableMap,
	pub procedures: ProcMap<T>,
	pub types: TypeMap,
}

impl<T: Debug> Debug for Data<T> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		writeln!(f, "Data {{")?;
		writeln!(f, "kinds:\n{}", self.kinds.iter()
				.map(|a| format!("  {a:?}"))
				.collect::<Vec<_>>()
				.join("\n"))?;
		writeln!(f, "values: \n{}", self.values.iter()
				.map(|a| format!("  {a:?}"))
				.collect::<Vec<_>>()
				.join("\n"))?;
		writeln!(f, "regions: \n{}", self.regions.iter()
				.map(|(id, region)| format!("  {id:?}: 0x{:08X}..0x{:08X}", region.span.start, region.span.end))
				.collect::<Vec<_>>()
				.join("\n"))?;
		writeln!(f, "records: \n{}", self.records.iter()
				.map(|(id, record)| format!("  {id:?}: {:?} {:?}", record.placement, record.fields))
				.collect::<Vec<_>>()
				.join("\n"))?;
		writeln!(f, "tables: \n{}", self.tables.iter()
				.map(|a| format!("  {a:?}"))
				.collect::<Vec<_>>()
				.join("\n"))?;
		writeln!(f, "procedures: \n{}", self.procedures.iter()
				.map(|(id, proc)| format!("  {id:?}: {:?} {:?} -> {:?}", proc.target, proc.params, proc.ret_type))
				.collect::<Vec<_>>()
				.join("\n"))?;
		writeln!(f, "types: \n{}", self.types.iter()
				.map(|(proc_id, depth, id, typ)| format!("  ({proc_id:?}, {depth}, {id:?}) -> {typ:?}"))
				.collect::<Vec<_>>()
				.join("\n"))?;
		writeln!(f, "}}")
	}
}

pub fn eval(input: &InputData, lex_data: &LexData, should_print: bool,
) -> Result<Data<SrcPos>, Error> {
	let (tasks, locations) = scan::scan_tasks(lex_data)
		.map_err(|e| e.into_comp_error(input, lex_data))
		.map_err(|e| e.with_kind(ErrKind::Parser))?;
	if should_print {
		eprintln!("{tasks:?}");
	}

	let data = process::process_tasks(lex_data, &locations, tasks)
		.map_err(|e| e.into_comp_error(input, lex_data))
		.map_err(|e| e.with_kind(ErrKind::Parser))?;
	if should_print {
		eprintln!("{data:?}");
	}

	Ok(Data {
		kinds: data.kinds,
		values: data.values,
		regions: data.regions,
		records: data.records,
		tables: data.tables,
		types: data.types,
		procedures: data.procedures.into_iter()
				.map(|(proc_id, proc_data)| (proc_id, convert_proc_idx_to_src(input, lex_data, proc_data)))
				.collect(),
	})
}

fn convert_proc_idx_to_src(
	input: &InputData,
	lex_data: &LexData,
	proc_data: Procedure<TokenId>,
) -> Procedure<SrcPos> {
	Procedure {
		target: proc_data.target,
		params: proc_data.params,
		ret_type: proc_data.ret_type,
		body: proc_data.body.into_iter()
				.map(|node| convert_ast_idx_to_src(input, lex_data, node))
				.collect(),
	}
}

fn convert_ast_idx_to_src(
	input: &InputData,
	lex_data: &LexData,
	node: Ast<AstKind, TokenId>,
) -> Ast<AstKind, SrcPos> {
	let tok_start = node.location.start;
	let tok_end = node.location.end;
	let src_start = token_source(input, lex_data, tok_start).start;
	let src_end = token_source(input, lex_data, tok_end).end;
	Ast { kind: node.kind, location: (src_start..src_end).into() }
}

