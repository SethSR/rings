
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
	// val-name -> value-kind
	values: identifier::Map<ValueKind>,
	procedures: identifier::Map<ProcData>,
	// rec-name -> (field, type)*
	records: identifier::Map<RowData>,
	tables: identifier::Map<TableData>,
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
}

impl fmt::Display for Data {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		writeln!(f, "=== Rings Compiler ===")?;
		writeln!(f)?;

		if !self.error.is_empty() {
			writeln!(f, "Error: {}", self.error)?;
			writeln!(f)?;
		}

		write!(f, "Tokens:\n ")?;
		for (token, start) in self.tok_list.iter().zip(self.tok_pos.iter()) {
			write!(f, " {token:?}[{start}]")?;
		}
		writeln!(f)?;

		write!(f, "identifier::ifiers:\n ")?;
		for (ident_id, identifier) in self.identifiers.iter() {
			write!(f, " [{ident_id}] '{}'", &self.source[identifier.clone()])?;
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

type RowData = Vec<(identifier::Id, RingType)>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MemoryLocation {
	Region(identifier::Id),
	Address(u32),
}

#[derive(Debug, PartialEq)]
struct TableData {
	row_count: u32,
	memory_location: MemoryLocation,
	row_spec: RowData,
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

