
use std::fmt::Debug;

use crate::error::{Error, Kind as ErrKind};
use crate::identifier::{IdentId, Map as IdentMap};
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::token::{Id as TokenId, Kind as TokenKind};
use crate::{SrcPos, Target};

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

impl<T: Debug> Data<T> {
	#[cfg(feature="debug_parser")]
	pub fn print_debug(&self, input: &InputData, lex_data: &LexData) {
		use data::{Record, Region};

		println!("== Parser ==");
		println!();

		let kind_str = self.kinds.iter()
			.map(|(id, kind)| {
				format!("  {:<16}: {kind:?}", lex_data.text(input, id))
			})
			.collect::<Vec<_>>()
			.join("\n");
		println!("Kinds:\n{kind_str}");

		let value_str = self.values.iter()
			.map(|(id, value)| {
				format!("  {:<16}: {value:?}", lex_data.text(input, id))
			})
			.collect::<Vec<_>>()
			.join("\n");
		println!("Values:\n{value_str}");

		let region_str = self.regions.iter()
			.map(|(id, Region { span })| {
				format!("  {:<16}: {span}", lex_data.text(input, id))
			})
			.collect::<Vec<_>>()
			.join("\n");
		println!("Regions:\n{region_str}");

		let record_str = self.records.iter()
			.map(|(id, Record { placement, fields })| {
				let placement_str = placement.map(|p| p.as_text(input, lex_data))
					.unwrap_or_default();

				let field_str = fields.iter()
					.map(|(fid, ftype)| {
						format!("    {}: {ftype:?}", lex_data.text(input, fid))
					})
					.collect::<Vec<_>>()
					.join("\n");

				format!("  {}: {placement_str}\n{field_str}", lex_data.text(input, id))
			})
			.collect::<Vec<_>>()
			.join("\n");
		println!("Records:\n{record_str}");

		let table_str = self.tables.iter()
			.map(|(id, Table { row_count, placement, fields })| {
				let placement_str = placement.map(|p| p.as_text(input, lex_data))
					.unwrap_or_default();

				let field_str = fields.iter()
					.map(|(fid, ftype)| {
						format!("    {}: {ftype:?}", lex_data.text(input, fid))
					})
					.collect::<Vec<_>>()
					.join("\n");

				format!("  {}[{row_count}]: {placement_str}\n{field_str}", lex_data.text(input, id))
			})
			.collect::<Vec<_>>()
			.join("\n");
		println!("Tables:\n{table_str}");

		let proc_str = self.procedures.iter()
			.map(|(id, Procedure { target, params, ret_type, ..})| {
				let param_str = params.iter()
					.map(|(pid, ptype)| {
						format!("    {}: {ptype:?}", lex_data.text(input, pid))
					})
					.collect::<Vec<_>>()
					.join("\n");
				let tgt_str = target.map(|tgt| format!("{tgt:?}"))
					.unwrap_or_default();
				format!("  {} [{tgt_str}]({param_str}) -> {ret_type:?}", lex_data.text(input, id))
			})
			.collect::<Vec<_>>()
			.join("\n");
		println!("Procedures:\n{proc_str}");

		let type_str = self.types.iter()
			.map(|(proc_id, depth, id, typ)| {
				format!("  ({}, {depth}, {}) -> {typ:?}",
					lex_data.text(input, proc_id),
					lex_data.text(input, id),
				)
			})
			.collect::<Vec<_>>()
			.join("\n");
		println!("Types:\n{type_str}");

		println!();
	}
}

pub fn eval(input: &InputData, lex_data: &LexData) -> Result<Data<SrcPos>, Error> {
	let (tasks, locations) = scan::scan_tasks(lex_data)
		.map_err(|e| e.into_comp_error(input, lex_data))
		.map_err(|e| e.with_kind(ErrKind::Parser))?;

	#[cfg(feature="debug_tasks")]
	eprintln!("{tasks:?}");

	let data = process::process_tasks(lex_data, &locations, tasks)
		.map_err(|e| e.into_comp_error(input, lex_data))
		.map_err(|e| e.with_kind(ErrKind::Parser))?;

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
	let src_start = lex_data.token_source(input, tok_start).start;
	let src_end = lex_data.token_source(input, tok_end).end;
	Ast { kind: node.kind, location: (src_start..src_end).into() }
}

