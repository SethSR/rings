
use std::{env, fmt, fs};
use std::collections::{HashMap, VecDeque};
use std::ops::Range;

mod discovery;
mod lexer;
mod tokens;

use tokens::TokenKind;

fn main() {
	let mut args = env::args();
	args.next();

	let file_path = args.next()
		.expect("expected source file");
	let source = fs::read_to_string(file_path)
		.expect("unable to read source file");

	let data = compile(source);
	println!("{data}")
}

type SrcPos  = usize;
type TokenId = usize;
type IdentId = usize;
type GraphId = usize;
type ScopeId = usize;
type NodeId  = usize;

type RowData = Vec<(IdentId, Range<usize>)>;

pub fn compile(source: String) -> Data {
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

#[derive(Default)]
pub struct Data {
	source: String,
	error: String,
	/* Lexer */
	tok_list: Vec<TokenKind>,
	tok_pos: Vec<SrcPos>,
	identifiers: Vec<Range<usize>>,
	/* Discovery */
	proc_start: Vec<TokenId>,
	// val-name -> value-kind
	values: HashMap<IdentId, ValueKind>,
	// rec-name -> (field, type)*
	records: HashMap<IdentId, RowData>,
	tables: HashMap<IdentId, TableData>,
	/* Parsing */
	proc_queue: VecDeque<Task>,
	node_graphs: Vec<NodeGraph>,
	scope_stacks: Vec<ScopeStack>,
	// procedures ready to be inlined and/or lowered
	ready_procs: HashMap<IdentId, NodeGraph>,
}

impl Data {
	pub fn new(source: String) -> Self {
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

		write!(f, "Identifiers:\n ")?;
		for (ident_id, identifier) in self.identifiers.iter().enumerate() {
			write!(f, " [{ident_id}] {}", &self.source[identifier.clone()])?;
		}
		writeln!(f)?;

		Ok(())
	}
}

struct TableData {
	row_count: u32,
	memory_region: IdentId,
	row_spec: RowData,
}

struct Task {
	proc_name: IdentId,
	tok_pos: SrcPos,
	graph_id: GraphId,
	scope_id: ScopeId,
}

struct NodeGraph {
	node_kinds: Vec<NodeKind>,
	use_defs: Vec<Vec<NodeId>>,
	def_uses: Vec<Vec<NodeId>>,
}

struct ScopeStack(Vec<HashMap<IdentId, NodeId>>);

enum TypeDef {
	Int(u8),
	Bool,
}

enum NodeKind {}

