
use std::{env, fmt, fs};
use std::collections::{HashMap, VecDeque};
use std::ops::Range;

mod discovery;
mod identifier;
mod lexer;
mod token;

fn main() {
	let mut args = env::args();
	args.next();

	let file_path = args.next()
		.expect("expected source file");
	let source = fs::read_to_string(file_path)
		.expect("unable to read source file");

	let data = compile(source.into());
	println!("{data}")
}

type SrcPos = usize;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
struct GraphId(usize);
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
struct ScopeId(usize);
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
struct NodeId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RingType {
	Bool,
	U8, S8,
	U16, S16,
	U32, S32,
	// F16, F32
	Record(identifier::Id),
	Table(identifier::Id),
	Unit,
}

pub fn compile(source: Box<str>) -> Data {
	let mut data = Data::new(source);
	lexer::eval(&mut data);
	discovery::eval(&mut data);
	data
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ValueKind {
	Integer(i64),
	Decimal(f64),
}

// TODO - srenshaw - Need to add Record and Table size calculations, and Table location
// calculations.

#[derive(Default)]
pub struct Data {
	source: Box<str>,
	error: String,
	/* Lexer */
	tok_list: token::KindList,
	tok_pos: token::PosList,
	identifiers: identifier::Map<Range<SrcPos>>,
	/* Discovery */
	proc_start: identifier::Map<token::Id>,
	procedures: identifier::Map<ProcData>,
	// val-name -> value-kind
	values: identifier::Map<ValueKind>,
	// rec-name -> (field, type)*
	records: identifier::Map<ColumnData>,
	record_sizes: identifier::Map<usize>,
	tables: identifier::Map<TableData>,
	table_sizes: identifier::Map<usize>,
	/* Parsing */
	proc_queue: VecDeque<Task>,
	node_graphs: Vec<NodeGraph>,
	scope_stacks: Vec<ScopeStack>,
	// procedures ready to be inlined and/or lowered
	ready_procs: identifier::Map<NodeGraph>,
}

impl Data {
	pub fn new(source: Box<str>) -> Self {
		Self {
			source,
			..Default::default()
		}
	}

	pub fn text(&self, ident_id: identifier::Id) -> &str {
		&self.source[self.identifiers[&ident_id].clone()]
	}

	fn type_text(&self, ring_type: RingType) -> String {
		match ring_type {
			RingType::Record(ident_id) => self.text(ident_id).to_string(),
			RingType::Table(ident_id) => self.text(ident_id).to_string(),
			_ => format!("{ring_type:?}"),
		}
	}

	fn type_size(&self, ring_type: RingType) -> usize {
		match ring_type {
			RingType::Bool |
			RingType::U8 |
			RingType::S8 => 1,
			RingType::U16 |
			RingType::S16 => 2,
			RingType::U32 |
			RingType::S32 => 4,
			RingType::Record(ident_id) => self.record_sizes[&ident_id],
			RingType::Table(ident_id) => self.table_sizes[&ident_id],
			RingType::Unit => 0,
		}
	}
}

impl fmt::Display for Data {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		writeln!(f, "=== Rings Compiler ===")?;
		writeln!(f)?;

		if !self.error.is_empty() {
			// Errors are printed in bold(1m) red(31m)
			writeln!(f, "Error: \x1b[31;1m{}\x1b[0m", self.error)?;
			writeln!(f)?;
		}

		fn fields_to_str(data: &Data, fields: &[(identifier::Id, RingType)]) -> String {
			fields.iter()
				.map(|(field_id, field_type)| {
					format!("{} : {}", data.text(*field_id), data.type_text(*field_type))
				})
				.collect::<Vec<_>>()
				.join(" , ")
		}

		write!(f, "Tokens:\n ")?;
		for (token, start) in self.tok_list.iter().zip(self.tok_pos.iter()) {
			match token {
				token::Kind::Identifier(ident_id) => {
					write!(f, " Identifier({})[{start}]", self.text(*ident_id))?;
				}
				_ => write!(f, " {token:?}[{start}]")?,
			}
		}
		writeln!(f)?;
		writeln!(f)?;

		writeln!(f, "{:<16} | HASH-VALUE",
			"IDENTIFIER")?;
		for ident_id in self.identifiers.keys() {
			writeln!(f, "{:<16} | {ident_id}", self.text(*ident_id))?;
		}
		writeln!(f)?;

		writeln!(f, "{:<16} | {:<8} | FIELDS",
			"RECORD", "SIZE")?;
		for (ident_id, fields) in self.records.iter() {
			let name = self.text(*ident_id);
			let size = self.record_sizes[ident_id];
			let field_str = fields_to_str(self, fields);
			writeln!(f, "{name:<16} | {size:<8} | {field_str}")?;
		}
		writeln!(f)?;

		writeln!(f, "{:<16} | {:<10} | {:<8} | {:<9} | COLUMNS",
			"TABLE", "TOTAL SIZE", "ROW SIZE", "ROW COUNT")?;
		for (ident_id, data) in self.tables.iter() {
			let name = self.text(*ident_id);
			let size = self.table_sizes[ident_id];
			let row_size = size / data.row_count as usize;
			let field_str = fields_to_str(self, &data.column_spec);
			writeln!(f, "{name:<16} | {size:<10} | {row_size:<8} | {:<9} | {field_str}", data.row_count)?;
		}
		writeln!(f)?;

		writeln!(f, "{:<16} | {:<16} | PARAMETERS",
			"PROCEDURE", "RETURN-TYPE")?;
		for ident_id in self.procedures.keys() {
			let name = self.text(*ident_id);
			let data = &self.procedures[ident_id];
			let ret_type = format!("{:?}", data.ret_type);
			let param_str = fields_to_str(self, &data.params);
			writeln!(f, "{name:<16} | {ret_type:<16} | {param_str}")?;
		}
		writeln!(f)?;

		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ProcData {
	params: Vec<(identifier::Id, RingType)>,
	ret_type: RingType,
}

type ColumnData = Vec<(identifier::Id, RingType)>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MemoryLocation {
	Region(identifier::Id),
	Address(u32),
}

#[derive(Debug, PartialEq)]
struct TableData {
	row_count: u32,
	memory_location: MemoryLocation,
	column_spec: ColumnData,
}

struct Task {
	proc_name: identifier::Id,
	tok_pos: token::Id,
	graph_id: GraphId,
	scope_id: ScopeId,
}

struct NodeGraph {
	node_kinds: Vec<NodeKind>,
	use_defs: Vec<Vec<NodeId>>,
	def_uses: Vec<Vec<NodeId>>,
}

struct ScopeStack(Vec<HashMap<identifier::Id, NodeId>>);

enum NodeKind {}

