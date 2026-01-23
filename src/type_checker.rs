
use crate::error;
use crate::identifier::{IdentId, Identifier, Map as IdentMap};
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::operators::{BinaryOp, UnaryOp};
use crate::parser::{Ast, AstId, AstKind, AstList, Data as PrsData, PathSegment, Type, Value};
use crate::token::Id as TokenId;
use crate::{token_source, SrcPos};

pub type TypedList = AstList<TypedKind, SrcPos>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExType {
	Type(Type),
	ExInt,
	ExDec,
}

impl ExType {
	fn void() -> Self {
		Self::Type(Type::Void)
	}

	fn is_integer(&self) -> bool {
		matches!(self,
			Self::ExInt |
			Self::Type(Type::S8) |
			Self::Type(Type::S16) |
			Self::Type(Type::S32) |
			Self::Type(Type::U8) |
			Self::Type(Type::U16) |
			Self::Type(Type::U32)
		)
	}

	fn is_decimal(&self) -> bool {
		matches!(self, Self::ExDec)
	}
}

#[derive(Debug, Clone)]
pub struct TypedKind(AstKind, ExType);

type TypedAst = Ast<TypedKind, SrcPos>;

impl TypedAst {
	fn new(value: Ast<AstKind, SrcPos>, ex_type: ExType) -> Self {
		Self {
			kind: TypedKind(value.kind, ex_type),
			location: value.location,
		}
	}
}

enum Error {
	InternalValue,
	AlreadyDefined(IdentId),
	MismatchedTypes(ExType, ExType),
	InvalidBinOp(BinaryOp, ExType, ExType),
	InvalidUnOp(UnaryOp, ExType),
	TooManyLoopVariables,
	NegativeLoopRange,
	MissingLoopBounds,
	// Compiler Errors
	NoType { msg: &'static str, ast_id: AstId },
	MissingAstNode(IdentId, AstId),
}

impl Error {
	fn to_string(&self, input: &InputData, lex_data: &LexData) -> String {
		match self {
			Self::InternalValue => {
				"Cannot define internal values, assign instead".to_string()
			}
			Self::AlreadyDefined(ident_id) => {
				format!("'{}' already defined", lex_data.text(input, ident_id))
			}
			Self::MismatchedTypes(expected, found) => {
				format!("Variable has type {expected:?}, but expression has type {found:?}")
			}
			Self::InvalidBinOp(op, lhs, rhs) => {
				format!("Unable to apply '{op}' to '{lhs:?}' and '{rhs:?}'")
			}
			Self::InvalidUnOp(op, rhs) => {
				format!("Unable to apply '{op}' to '{rhs:?}'")
			}
			Self::TooManyLoopVariables => {
				"simple for-loops require a single loop variable".to_string()
			}
			Self::NegativeLoopRange => {
				"start value must be less than or equal to end value".to_string()
			}
			Self::MissingLoopBounds => {
				"simple for-loops require a fully specified range '[start..end]'".to_string()
			}
			Self::NoType { msg, ast_id } => {
				format!("{msg} has no type: {ast_id:?}")
			}
			Self::MissingAstNode(proc_id, ast_id) => {
				format!("{} not found in procedure '{}'", ast_id, lex_data.text(input, &proc_id))
			}
		}
	}
}

pub fn eval(
	input: &InputData,
	lex_data: &LexData,
	prs_data: &PrsData<SrcPos>,
) -> Result<IdentMap<TypedList>, error::Error> {
	// Check for 'main' procedure
	if !prs_data.procedures.contains_key(&"main".id()) {
		let tok_src = token_source(input, lex_data, TokenId::default());
		let message = "missing 'main' procedure";
		let err = error::Error::new(tok_src, message);
		return Err(err.with_kind(error::Kind::Checker));
	}

	// Check for stack regions
	let has_call_stack = prs_data.regions.contains_key(&"CallStack".id());
	let has_data_stack = prs_data.regions.contains_key(&"DataStack".id());
	if let Some(stack_src_loc) = prs_data.regions.get(&"Stack".id()) {
		// call and data stack are combined, reject explicitly named regions
		if has_call_stack || has_data_stack {
			let tok_src = token_source(input, lex_data, (stack_src_loc.span.start as usize).into());
			let message = "Combined Call/Data stack already defined";
			let err = error::Error::new(tok_src, message);
			return Err(err.with_kind(error::Kind::Checker));
		}
	} else {
		// no combined stack region, call and data stack are required
		if !has_call_stack {
			let tok_src = token_source(input, lex_data, TokenId::default());
			let message = "No combined Call/Data stack defined, declare a dedicated 'CallStack' region.";
			let err = error::Error::new(tok_src, message);
			return Err(err.with_kind(error::Kind::Checker));
		} else if !has_data_stack {
			let tok_src = token_source(input, lex_data, TokenId::default());
			let message = "No combined Call/Data stack defined, declare a dedicated 'DataStack' region.";
			let err = error::Error::new(tok_src, message);
			return Err(err.with_kind(error::Kind::Checker));
		}
	}

	// Check for region overlap
	let regions_vec: Vec<_> = prs_data.regions.iter().collect();

	for i in 0..regions_vec.len() {
		for j in i+1..regions_vec.len() {
			let (i_name, i_span) = regions_vec[i];
			let (j_name, j_span) = regions_vec[j];
			if !(i_span.span.start >= j_span.span.end || i_span.span.end <= j_span.span.start) {
				let i_src_loc = lex_data.location(i_name);
				let j_src_loc = lex_data.location(j_name);
				let i_text = lex_data.text(input, i_name);
				let j_text = lex_data.text(input, j_name);
				let msg = format!("regions {i_text} and {j_text} overlap");
				let i_msg = format!("{i_text} has a memory range of {i_span:?}");
				let j_msg = format!("{j_text} has a memory range of {j_span:?}");
				let err = error::Error::new(i_src_loc, msg)
					.with_note_at(i_src_loc, &i_msg)
					.with_note_at(j_src_loc, &j_msg)
					.with_kind(error::Kind::Checker);
				return Err(err);
			}
		}
	}

	// Check Record placement
	// TODO - srenshaw - Ensure records fit within their respective regions.

	// Check procedures
	let mut typed_procedures = IdentMap::with_capacity(prs_data.procedures.len());
	for (proc_id, proc_data) in &prs_data.procedures {
		let proc_start: AstId = 0.into();

		match check_stmt(&prs_data, *proc_id) {
			Ok(new_ast) => {
				typed_procedures.insert(*proc_id, new_ast);
			}

			Err(err) => {
				let tok_src = proc_data.body[proc_start].location;
				let message = err.to_string(input, lex_data);
				let err = error::Error::new(tok_src, message)
						.with_kind(error::Kind::Checker);
				return Err(err);
			}
		}
	}

	Ok(typed_procedures)
}

fn meet(lhs: ExType, rhs: ExType) -> Result<ExType, Error> {
	match (lhs, rhs) {
		(ExType::ExInt, ExType::ExDec) |
		(ExType::ExDec, ExType::ExInt) => Ok(ExType::ExDec),
		(ExType::Type(Type::S8), ExType::ExInt) |
		(ExType::ExInt, ExType::Type(Type::S8)) => Ok(ExType::Type(Type::S8)),
		(ExType::Type(Type::S16), ExType::ExInt) |
		(ExType::ExInt, ExType::Type(Type::S16)) => Ok(ExType::Type(Type::S16)),
		(ExType::Type(Type::S32), ExType::ExInt) |
		(ExType::ExInt, ExType::Type(Type::S32)) => Ok(ExType::Type(Type::S32)),
		(ExType::Type(Type::U8), ExType::ExInt) |
		(ExType::ExInt, ExType::Type(Type::U8)) => Ok(ExType::Type(Type::U8)),
		(ExType::Type(Type::U16), ExType::ExInt) |
		(ExType::ExInt, ExType::Type(Type::U16)) => Ok(ExType::Type(Type::U16)),
		(ExType::Type(Type::U32), ExType::ExInt) |
		(ExType::ExInt, ExType::Type(Type::U32)) => Ok(ExType::Type(Type::U32)),
		(ExType::Type(Type::U8), ExType::Type(Type::S16)) |
		(ExType::Type(Type::S16), ExType::Type(Type::U8)) => Ok(ExType::Type(Type::S16)),
		(ExType::Type(Type::U8), ExType::Type(Type::S32)) |
		(ExType::Type(Type::S32), ExType::Type(Type::U8)) => Ok(ExType::Type(Type::S32)),
		(ExType::Type(Type::U16), ExType::Type(Type::S32)) |
		(ExType::Type(Type::S32), ExType::Type(Type::U16)) => Ok(ExType::Type(Type::S32)),
		(ExType::Type(Type::S8), ExType::Type(Type::S16)) |
		(ExType::Type(Type::S16), ExType::Type(Type::S8)) => Ok(ExType::Type(Type::S16)),
		(ExType::Type(Type::S8), ExType::Type(Type::S32)) |
		(ExType::Type(Type::S32), ExType::Type(Type::S8)) => Ok(ExType::Type(Type::S32)),
		(ExType::Type(Type::S16), ExType::Type(Type::S32)) |
		(ExType::Type(Type::S32), ExType::Type(Type::S16)) => Ok(ExType::Type(Type::S32)),
		(ExType::Type(Type::Bool), ExType::ExInt) |
		(ExType::ExInt, ExType::Type(Type::Bool)) => Ok(ExType::Type(Type::Bool)),
		(a, b) if a == b => Ok(a),
		_ => Err(Error::MismatchedTypes(lhs, rhs)),
	}
}

fn check_stmt(
	prs_data: &PrsData<SrcPos>,
	proc_id: IdentId,
) -> Result<AstList<TypedKind, SrcPos>, Error> {
	let proc_data = &prs_data.procedures[&proc_id];
	let mut out = AstList::default();
	let mut scope_depth = 0;

	for ast_id in 0..proc_data.body.len() {
		let ast = proc_data.body[ast_id].clone();
		match ast.kind {
			AstKind::Int(_) => {
				out.push(TypedAst::new(ast, ExType::ExInt));
			}

			AstKind::Dec(_) => {
				out.push(TypedAst::new(ast, ExType::ExDec));
			}

			AstKind::Ident(ident_id) => {
				let ex_type = match prs_data.values.get(&ident_id) {
					Some(Value::Integer(_)) => ExType::ExInt,
					Some(Value::Decimal(_)) => ExType::ExDec,
					None => prs_data.types.get(&(proc_id, scope_depth, ident_id))
							.map(|typ| ExType::Type(*typ))
							.unwrap_or_else(|| panic!("missing type for {ident_id}: ({proc_id}, {scope_depth}, {ident_id})")),
				};
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::Assign { lhs, rhs } => {
				let lhs = &out[lhs];
				let rhs = &out[rhs];
				let ex_type = meet(lhs.kind.1, rhs.kind.1)?;
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::BinOp { op, lhs, rhs } => {
				let lhs = &out[lhs];
				let rhs = &out[rhs];
				let ex_type = meet(lhs.kind.1, rhs.kind.1)?;

				let valid_op = match op {
					BinaryOp::Add |
					BinaryOp::Div |
					BinaryOp::Mul |
					BinaryOp::Mod |
					BinaryOp::ShL |
					BinaryOp::ShR |
					BinaryOp::Sub => {
						// u8, u16, u32, s8, s16, s32, int, dec
						ex_type.is_integer() || ex_type.is_decimal()
					}

					BinaryOp::BinAnd |
					BinaryOp::BinOr |
					BinaryOp::BinXor => {
						// u8, u16, u32, s8, s16, s32
						ex_type.is_integer()
					}

					BinaryOp::CmpEQ |
					BinaryOp::CmpGE |
					BinaryOp::CmpGT |
					BinaryOp::CmpLE |
					BinaryOp::CmpLT |
					BinaryOp::CmpNE |
					BinaryOp::LogAnd |
					BinaryOp::LogOr |
					BinaryOp::LogXor => {
						ex_type == ExType::Type(Type::Bool)
					}
				};

				if !valid_op {
					panic!("invalid binary-op: {op:?}");
				}

				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::UnOp { op, rhs } => {
				let rhs = out[rhs].clone();
				out.push(TypedAst::new(ast, rhs.kind.1));
				todo!("Checking UnOp: {op} <=> {rhs:?}")
			}

			AstKind::Return(Some(ret)) => {
				let ret = &out[ret];
				let ex_type = meet(ret.kind.1, ExType::Type(proc_data.ret_type))?;
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::Return(None) => {
				let ex_type = meet(ExType::void(), ExType::Type(proc_data.ret_type))?;
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::ScopeBegin => {
				scope_depth += 1;
				out.push(TypedAst::new(ast, ExType::void()));
			}

			AstKind::ScopeEnd => {
				scope_depth -= 1;
				out.push(TypedAst::new(ast, ExType::void()));
			}

			AstKind::Block(ref block) => {
				let ex_type = block.last()
						.map(|&id| out[id].kind.1)
						.unwrap_or(ExType::void());
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::If { cond, ref then_block, ref else_block } => {
				let cond = out[cond].clone();
				let then_type = then_block.last()
						.map(|&id| out[id].kind.1)
						.unwrap_or(ExType::void());
				let else_type = else_block.last()
						.map(|&id| out[id].kind.1)
						.unwrap_or(ExType::void());
				let ex_type = meet(then_type, else_type)?;
				out.push(TypedAst::new(ast, ex_type));
				todo!("Checking If condition: {cond:?} <=> {ex_type:?}")
			}

			AstKind::While { cond, ..} => {
				let cond = out[cond].clone();
				out.push(TypedAst::new(ast, ExType::void()));
				todo!("Checking While: {cond:?}")
			}

			AstKind::For { ref indexes, table, range_start, range_end, ..} => {
				let indexes_len = indexes.len();

				// TODO - srenshaw - Add index identifiers as AST nodes so we can verify them here.

				if let Some(table) = table
						.map(|id| &prs_data.tables[&id])
				{
					// TODO - assert indexes.len() == table.params.len() when a table is specified.

					todo!("Table looping not supported yet")
				}

				if indexes_len > 1 {
					return Err(Error::TooManyLoopVariables);
				}

				let Some(start) = range_start else {
					return Err(Error::MissingLoopBounds)
				};

				let Some(end) = range_end else {
					return Err(Error::MissingLoopBounds)
				};

				debug_assert!(start <= end);
				if start > end {
					return Err(Error::NegativeLoopRange);
				}

				out.push(TypedAst::new(ast, ExType::void()));
			}

			#[cfg(feature = "forloop")]
			AstKind::For(vars, Some(table_id), range, block) => {
				let table = &proc_data.tables[table_id];
				debug_assert!(vars.len() <= table.column_spec.len());

				let (start, end) = match range {
					Some(range) => (range.get_start(), range.get_end(table.row_count)),
					None => (0, table.row_count),
				};

				if start >= table.row_count {
					return Err("'start' value in bounds must be less than table row-count".to_string());
				}
				if end > table.row_count {
					return Err("'end' value in bounds must be less than or equal to table row-count".to_string());
				}
				if start > end {
					return Err("start value must be less than or equal to end value".to_string());
				}

				for i in 0..vars.len() {
					let var = &vars[i];
					if vars[i + 1..].contains(var) {
						return Some(format!("duplicate field name '{}' in table", proc_data.text(var)));
					}

					if !table.column_spec.iter().any(|(a, _)| var == a) {
						return Err(format!("field '{}' not found in table '{}'", proc_data.text(var), data.text(table_id)));
					}
				}
				self.check_block(proc_data, block, ret_type)
			}

			AstKind::Call { proc_id, block } => {
				todo!("procedure-call: {proc_id} {}", block.len())
			}

			AstKind::Access { base_id, ref path } => {
				let mut curr_id = base_id;
				let mut ex_type = None;

				for segment in path {
					match segment {
						PathSegment::Field(field_id) => {
							let Some(record) = prs_data.records.get(&curr_id) else {
								panic!("no record named '{curr_id}' found")
							};
							let Some((_, field_type)) = record.fields.iter().find(|(f_id, _)| field_id == f_id) else {
								panic!("no field '{field_id}' in record '{curr_id}'")
							};
							curr_id = *field_id;
							ex_type = Some(ExType::Type(*field_type));
						}
						PathSegment::Index(expr_id, field_id) => {
							todo!("table-index-2: [{expr_id}].{field_id}")
						}
					}
				}

				if ex_type.is_none() {
					panic!("no type for access {ast:?}")
				}

				out.push(TypedAst::new(ast, ex_type.unwrap()));
			}

			AstKind::Mark { region_id, mark_id } => {
				todo!("Checking Mark: {region_id} {mark_id}")
			}

			AstKind::Free { region_id, mark_id } => {
				todo!("Checking Free: {region_id} {mark_id:?}")
			}

			AstKind::Use { region_id, ident } => {
				todo!("Checking Use: {region_id} {ident}")
			}
		}
	}

	Ok(out)
}
