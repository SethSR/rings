
use std::collections::HashSet;

use index_vec::IndexVec;

use crate::error;
use crate::identifier::{IdentId, Identifier, Map as IdentMap};
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::operators::{BinaryOp, UnaryOp};
use crate::parser::{Ast, AstId, AstKind, Data as PrsData, PathSegment, Type, Value};
use crate::token::Id as TokenId;
use crate::{token_source, Span, SrcPos};

pub type TypedList = IndexVec<AstId, TypedAst>;

#[derive(Debug, Clone)]
pub struct TypedAst {
	pub kind: AstKind,
	pub typ: Type,
	pub location: Span<SrcPos>,
}

impl TypedAst {
	fn new(value: &Ast<AstKind, SrcPos>, typ: Type) -> Self {
		Self {
			kind: value.kind.clone(),
			typ,
			location: value.location,
		}
	}
}

enum Error {
	AlreadyDefined {
		ident_id: IdentId,
		location: Span<SrcPos>,
	},
	TypeMismatch {
		expected: Type,
		found: Type,
		location: Span<SrcPos>,
	},
	ParamCountMismatch {
		proc_id: IdentId,
		param_count: usize,
		arg_count: usize,
		location: Span<SrcPos>,
	},
	FieldCountMismatch {
		table_id: IdentId,
		field_count: usize,
		index_count: usize,
		location: Span<SrcPos>,
	},
	InvalidBinOp {
		op: BinaryOp,
		lhs: Type,
		rhs: Type,
		location: Span<SrcPos>,
	},
	InvalidUnOp {
		op: UnaryOp,
		rhs: Type,
		location: Span<SrcPos>,
	},
	TooManyLoopVariables(Span<SrcPos>),
	NegativeLoopRange(Span<SrcPos>),
	NegativeLoopBound(Span<SrcPos>),
	MissingLoopBounds(Span<SrcPos>),
	LoopStartBeyondTableSize {
		table_id: IdentId,
		table_size: u16,
		location: Span<SrcPos>,
	},
	LoopEndBeyondTableSize {
		table_id: IdentId,
		table_size: u16,
		location: Span<SrcPos>,
	},
	NonIdentifierField {
		field_kind: AstKind,
		location: Span<SrcPos>,
	},
	UnknownTableField {
		table_id: IdentId,
		field_id: IdentId,
		location: Span<SrcPos>,
	},
	UnknownRecord {
		proc_id: IdentId,
		record_id: IdentId,
		location: Span<SrcPos>,
	},
	UnknownRecordField {
		proc_id: IdentId,
		record_id: IdentId,
		field_id: IdentId,
		location: Span<SrcPos>,
	},
	UnknownRegion {
		region_id: IdentId,
		location: Span<SrcPos>,
	},
	UnknownMark {
		region_id: IdentId,
		mark_id: IdentId,
		location: Span<SrcPos>,
	},
	NoType {
		kind: String,
		location: Span<SrcPos>
	},
}

impl Error {
	fn no_type(ast: &Ast<AstKind, SrcPos>) -> Self {
		Self::NoType {
			kind: ast.kind.to_string(),
			location: ast.location,
		}
	}

	fn int_type_mismatch(found: Type, location: Span<SrcPos>) -> Self {
		Self::TypeMismatch { expected: Type::Int, found, location }
	}

	/// Conversion to full compiler error
	fn into_comp_error(self,
		input: &InputData,
		lex_data: &LexData,
	) -> error::Error {
		match self {
			Self::AlreadyDefined { ident_id, location } => {
				let message = format!("'{}' already defined", lex_data.text(input, &ident_id));
				error::Error::new(location, message)
			}
			Self::TypeMismatch { expected, found, location } => {
				let message = format!("Variable has type {expected:?}, but expression has type {found:?}");
				error::Error::new(location, message)
			}
			Self::ParamCountMismatch { proc_id, param_count, arg_count, location } => {
				let message = format!("'{}' has {param_count} parameters, found {arg_count} arguments",
					lex_data.text(input, &proc_id),
				);
				error::Error::new(location, message)
			}
			Self::FieldCountMismatch { table_id, field_count, index_count, location } => {
				let message = format!("'{}' has {field_count} fields, found {index_count} indexes",
					lex_data.text(input, &table_id),
				);
				error::Error::new(location, message)
			}
			Self::InvalidBinOp { op, lhs, rhs, location } => {
				let message = format!("Unable to apply '{op}' to '{lhs:?}' and '{rhs:?}'");
				error::Error::new(location, message)
			}
			Self::InvalidUnOp{ op, rhs, location } => {
				let message = format!("Unable to apply '{op}' to '{rhs:?}'");
				error::Error::new(location, message)
			}
			Self::TooManyLoopVariables(location) => {
				let message = "Simple for-loops require a single loop variable".to_string();
				error::Error::new(location, message)
			}
			Self::NegativeLoopRange(location) => {
				let message = "Start-index must not exceed end-index".to_string();
				error::Error::new(location, message)
			}
			Self::NegativeLoopBound(location) => {
				let message = "Loop bounds must be non-negative".to_string();
				error::Error::new(location, message)
			}
			Self::MissingLoopBounds(location) => {
				let message = "Simple for-loops require a fully specified range '[start..end]'".to_string();
				error::Error::new(location, message)
			}
			Self::LoopStartBeyondTableSize { table_id, table_size, location } => {
				let message = format!("Start-index is larger than the size of table '{}': {table_size}",
					lex_data.text(input, &table_id),
				);
				error::Error::new(location, message)
			}
			Self::LoopEndBeyondTableSize { table_id, table_size, location } => {
				let message = format!("End-index is larger than the size of table '{}': {table_size}",
					lex_data.text(input, &table_id),
				);
				error::Error::new(location, message)
			}
			Self::NonIdentifierField { field_kind, location } => {
				let message = format!("Expected an identifier, found '{field_kind}'");
				error::Error::new(location, message)
			}
			Self::UnknownTableField { table_id, field_id, location } => {
				let message = format!("Unknown field '{}' in table '{}'",
					lex_data.text(input, &field_id),
					lex_data.text(input, &table_id),
				);
				error::Error::new(location, message)
			}
			Self::UnknownRecord { proc_id, record_id, location } => {
				let message = format!("In procedure '{}': unknown record '{}'",
					lex_data.text(input, &proc_id),
					lex_data.text(input, &record_id),
				);
				error::Error::new(location, message)
			}
			Self::UnknownRecordField { proc_id, record_id, field_id, location } => {
				let message = format!("In procedure '{}': unknown field '{}' in record '{}'",
					lex_data.text(input, &proc_id),
					lex_data.text(input, &field_id),
					lex_data.text(input, &record_id),
				);
				error::Error::new(location, message)
			}
			Self::UnknownRegion { region_id, location } => {
				let message = format!("Unknown region '{}'", lex_data.text(input, &region_id));
				error::Error::new(location, message)
			}
			Self::UnknownMark { region_id, mark_id, location } => {
				let message = format!("Unknown mark '{}' for region '{}'",
					lex_data.text(input, &mark_id),
					lex_data.text(input, &region_id),
				);
				error::Error::new(location, message)
			}
			Self::NoType { kind, location } => {
				let message = format!("no type for item {kind}");
				error::Error::new(location, message)
			}
		}
	}
}

pub fn eval(
	input: &InputData,
	lex_data: &LexData,
	prs_data: &PrsData<SrcPos>,
) -> Result<IdentMap<TypedList>, error::Error> {
	check_for_main(input, lex_data, prs_data)
		.map_err(|e| e.with_kind(error::Kind::Checker))?;

	check_stack_regions(input, lex_data, prs_data)
		.map_err(|e| e.with_kind(error::Kind::Checker))?;

	check_procedures(prs_data)
		.map_err(|e| e.into_comp_error(input, lex_data))
		.map_err(|e| e.with_kind(error::Kind::Checker))
}

fn check_for_main(
	input: &InputData,
	lex_data: &LexData,
	prs_data: &PrsData<SrcPos>,
) -> Result<(), error::Error> {
	if prs_data.procedures.contains_key(&"main".id()) {
		return Ok(());
	}

	let tok_src = token_source(input, lex_data, TokenId::default());
	let message = "missing 'main' procedure";
	Err(error::Error::new(tok_src, message))
}

fn check_stack_regions(
	input: &InputData,
	lex_data: &LexData,
	prs_data: &PrsData<SrcPos>,
) -> Result<(), error::Error> {
	fn stack_error(
		input: &InputData,
		lex_data: &LexData,
		token_id: TokenId,
		message: &str,
	) -> error::Error {
		let tok_src = token_source(input, lex_data, token_id);
		error::Error::new(tok_src, message)
	}

	let has_call_stack = prs_data.regions.contains_key(&"CallStack".id());
	let has_data_stack = prs_data.regions.contains_key(&"DataStack".id());
	if let Some(stack_src_loc) = prs_data.regions.get(&"Stack".id()) {
		// call and data stack are combined, reject explicitly named regions
		if has_call_stack || has_data_stack {
			return Err(stack_error(input, lex_data,
				(stack_src_loc.span.start as usize).into(),
				"Combined Call/Data stack already defined"));
		}
	} else {
		// no combined stack region, call and data stack are required
		let missing_stack = if !has_call_stack {
			Some("CallStack")
		} else if !has_data_stack {
			Some("DataStack")
		} else {
			None
		};

		if let Some(stack_name) = missing_stack {
			return Err(stack_error(input, lex_data, TokenId::default(),
				&format!("No combined Call/Data stack defined, declare a dedicated '{stack_name}' region, or a combined 'Stack'.")));
		}
	}

	Ok(())
}

fn check_procedures(
	prs_data: &PrsData<SrcPos>,
) -> Result<IdentMap<TypedList>, Error> {
	let mut typed_procedures = IdentMap::with_capacity(prs_data.procedures.len());

	for proc_id in prs_data.procedures.keys() {
		let new_ast = check_proc(prs_data, *proc_id)
			.and_then(resolve_types)?;
		typed_procedures.insert(*proc_id, new_ast);
	}

	Ok(typed_procedures)
}

fn meet_ast(lhs: &TypedAst, rhs: &TypedAst) -> Result<Type, Error> {
	meet(lhs.typ, rhs.typ, lhs.location + rhs.location)
}

fn meet(lhs: Type, rhs: Type, location: Span<SrcPos>) -> Result<Type, Error> {
	// TODO - srenshaw - Restrict 32-bit values when targeting Z80
	match (lhs, rhs) {
		(Type::Unknown, typ) |
		(typ, Type::Unknown) => Ok(typ),

		(Type::Int, Type::Dec) |
		(Type::Dec, Type::Int) => Ok(Type::Dec),

		(Type::Int, typ) |
		(typ, Type::Int) if typ.is_integer() => Ok(typ),

		(Type::U8, Type::U16) |
		(Type::U16, Type::U8) => Ok(Type::U16),

		(Type::U8, Type::U32) |
		(Type::U32, Type::U8) => Ok(Type::U32),

		(Type::U8, Type::S16) |
		(Type::S16, Type::U8) => Ok(Type::S16),

		(Type::U8, Type::S32) |
		(Type::S32, Type::U8) => Ok(Type::S32),

		(Type::U16, Type::U32) |
		(Type::U32, Type::U16) => Ok(Type::U32),

		(Type::U16, Type::S32) |
		(Type::S32, Type::U16) => Ok(Type::S32),

		(Type::S8, Type::S32) |
		(Type::S32, Type::S8) => Ok(Type::S32),

		(Type::S16, Type::S32) |
		(Type::S32, Type::S16) => Ok(Type::S32),

		(Type::S8, Type::S16) |
		(Type::S16, Type::S8) => Ok(Type::S16),

		(a, b) if a == b => Ok(a),

		_ => Err(Error::TypeMismatch {
			expected: lhs,
			found: rhs,
			location,
		}),
	}
}

/// This expects the AST node-list in each prodecure body to be laid out in a def-use order. All
/// nodes must contain only back-references to previously defined nodes. This is what guarantees
/// correctness despite the simple, forward traversal of the AST.
///
/// Eventually, this should probably be rewritten as a work-queue or some other stable, recursive
/// algorithm to remove this restriction and, more importantly, to unify `check_proc` and
/// `resolve_types` as currently they both "own" type-propogation responsibilities, which is not
/// ideal.
// TODO - srenshaw - Add a debug_assert to check def-use ordering invariant during development.
// TODO - srenshaw - Restructure check_proc and resolve_types into a single work-queue algorithm.
fn check_proc(
	prs_data: &PrsData<SrcPos>,
	proc_id: IdentId,
) -> Result<TypedList, Error> {
	let mut out = TypedList::default();
	let mut scope_depth = 0;
	let mut mark_set = HashSet::<IdentId>::default();

	let proc_data = &prs_data.procedures[&proc_id];

	for ast in &proc_data.body {
		// Because most of the used fields are indexes or IDs, we match by value to make using those
		// indexes easier. Non-trivial types are explicitly matched by reference.
		match ast.kind {
			AstKind::Int(_) => {
				out.push(TypedAst::new(ast, Type::Int));
			}

			AstKind::Dec(_) => {
				out.push(TypedAst::new(ast, Type::Dec));
			}

			AstKind::Ident(ident_id) => {
				let ex_type = match prs_data.values.get(&ident_id) {
					Some(Value::Integer(_)) => Type::Int,
					Some(Value::Decimal(_)) => Type::Dec,
					None => prs_data.types.get(proc_id, scope_depth, ident_id)
						.ok_or_else(|| Error::no_type(ast))?,
				};
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::Assign { lhs, rhs } => {
				let lhs_ast = &out[lhs];
				let rhs_ast = &out[rhs];
				let ex_type = meet(lhs_ast.typ, rhs_ast.typ, rhs_ast.location)?;
				out[lhs].typ = ex_type;
				out[rhs].typ = ex_type;
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::BinOp { op, lhs, rhs } => {
				let lhs_ast = &out[lhs];
				let rhs_ast = &out[rhs];
				let typ = meet_ast(lhs_ast, rhs_ast)?;

				let valid_op = match op {
					BinaryOp::Add |
					BinaryOp::Div |
					BinaryOp::Mul |
					BinaryOp::Mod |
					BinaryOp::ShL |
					BinaryOp::ShR |
					BinaryOp::Sub => {
						// u8, u16, u32, s8, s16, s32, int, dec
						typ.is_integer() || typ.is_decimal()
					}

					BinaryOp::BinAnd |
					BinaryOp::BinOr |
					BinaryOp::BinXor |
					BinaryOp::CmpEQ |
					BinaryOp::CmpGE |
					BinaryOp::CmpGT |
					BinaryOp::CmpLE |
					BinaryOp::CmpLT |
					BinaryOp::CmpNE => {
						// u8, u16, u32, s8, s16, s32
						typ.is_integer()
					}

					BinaryOp::LogAnd |
					BinaryOp::LogOr |
					BinaryOp::LogXor => {
						typ == Type::Bool
					}
				};

				if !valid_op {
					return Err(Error::InvalidBinOp {
						op,
						lhs: lhs_ast.typ,
						rhs: rhs_ast.typ,
						location: ast.location,
					});
				}

				out[lhs].typ = typ;
				out[rhs].typ = typ;
				out.push(TypedAst::new(ast, typ));
			}

			AstKind::UnOp { op, rhs } => {
				let rhs_type = out[rhs].typ;

				let valid_op = match op {
					UnaryOp::Neg => {
						rhs_type.is_signed_integer()
					}
					UnaryOp::Not => {
						rhs_type.is_integer() || rhs_type == Type::Bool
					}
				};

				if !valid_op {
					return Err(Error::InvalidUnOp {
						op,
						rhs: rhs_type,
						location: ast.location,
					});
				}

				out.push(TypedAst::new(ast, rhs_type));
			}

			AstKind::Return(Some(ret)) => {
				let ret_ast = &out[ret];
				let ex_type = meet(ret_ast.typ, proc_data.ret_type, ret_ast.location)?;
				out[ret].typ = ex_type;
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::Return(None) => {
				let ex_type = meet(Type::Void, proc_data.ret_type, ast.location)?;
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::ScopeBegin => {
				scope_depth += 1;
				// NOTE - srenshaw - We convert scope nodes to keep index references correct.
				out.push(TypedAst::new(ast, Type::Void));
			}

			AstKind::ScopeEnd => {
				scope_depth -= 1;
				// NOTE - srenshaw - We convert scope nodes to keep index references correct.
				out.push(TypedAst::new(ast, Type::Void));
			}

			AstKind::Block(ref block) => {
				let ex_type = block.last()
					.map(|&id| out[id].typ)
					.unwrap_or(Type::Void);
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::If { cond, ref then_block, ref else_block } => {
				let cond_type = out[cond].typ;
				if !cond_type.is_integer() {
					return Err(Error::int_type_mismatch(cond_type, ast.location));
				}

				let then_type = then_block.last()
						.map(|&id| out[id].typ)
						.unwrap_or(Type::Void);
				let else_type = else_block.last()
						.map(|&id| out[id].typ)
						.unwrap_or(Type::Void);
				let ex_type = meet(then_type, else_type, ast.location)?;

				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::While { cond, ..} => {
				let cond_type = out[cond].typ;
				if !cond_type.is_integer() {
					return Err(Error::int_type_mismatch(cond_type, ast.location));
				}
				out.push(TypedAst::new(ast, Type::Void));
			}

			AstKind::For { ref indexes, table: None, range_start, range_end, ..} => {
				let index_type = check_range_loop(ast, indexes, range_start, range_end, &mut out)?;
				out.push(TypedAst::new(ast, index_type));
			}

			AstKind::For { ref indexes, table: Some(table_id), range_start, range_end, ..} => {
				let index_type = check_table_loop(prs_data, ast, indexes, table_id, range_start, range_end, &mut out)?;
				out.push(TypedAst::new(ast, index_type));
			}

			AstKind::Call { proc_id, ref block } => {
				let proc = &prs_data.procedures[&proc_id];

				if proc.params.len() != block.len() {
					return Err(Error::ParamCountMismatch {
						proc_id,
						param_count: proc.params.len(),
						arg_count: block.len(),
						location: ast.location,
					});
				}

				for ((_, p_type), arg_id) in proc.params.iter().zip(block.iter()) {
					let arg_ast = &out[*arg_id];
					// NOTE - srenshaw - This return is intentionally dropped for now. We want to ensure
					// parameter and argument types match, but we don't want to narrow the argument types,
					// and we don't want to update parameter types at all.
					meet(*p_type, arg_ast.typ, arg_ast.location)?;
				}

				out.push(TypedAst::new(ast, proc.ret_type));
			}

			AstKind::Access { base_id, ref path } => {
				let mut curr_id = base_id;
				let mut access_type = prs_data.types.get(proc_id, scope_depth, curr_id)
					.ok_or_else(|| Error::no_type(ast))?;

				for segment in path {
					match segment {
						PathSegment::Field(field_id) => {
							let Some(record) = prs_data.records.get(&curr_id) else {
								return Err(Error::UnknownRecord {
									proc_id,
									record_id: curr_id,
									location: ast.location,
								});
							};
							let Some((_, field_type)) = record.fields.iter()
								.find(|(f_id, _)| f_id == field_id)
							else {
								return Err(Error::UnknownRecordField {
									proc_id,
									record_id: curr_id,
									field_id: *field_id,
									location: ast.location,
								});
							};
							curr_id = *field_id;
							access_type = *field_type;
						}
						PathSegment::Index(expr_id, field_id) => {
							todo!("table-index-2: [{expr_id}].{field_id}")
						}
					}
				}

				out.push(TypedAst::new(ast, access_type));
			}

			AstKind::Mark { region_id, mark_id } => {
				if !prs_data.regions.contains_key(&region_id) {
					return Err(Error::UnknownRegion {
						region_id,
						location: ast.location,
					});
				};

				if mark_set.contains(&mark_id) {
					return Err(Error::AlreadyDefined {
						ident_id: mark_id,
						location: ast.location,
					});
				}
				mark_set.insert(mark_id);

				out.push(TypedAst::new(ast, Type::Void));
			}

			AstKind::Free { region_id, mark_id } => {
				if !prs_data.regions.contains_key(&region_id) {
					return Err(Error::UnknownRegion {
						region_id,
						location: ast.location,
					});
				}

				if let Some(mark_id) = mark_id {
					if !mark_set.contains(&mark_id) {
						return Err(Error::UnknownMark {
							region_id,
							mark_id,
							location: ast.location,
						});
					}
					mark_set.remove(&mark_id);
				}

				out.push(TypedAst::new(ast, Type::Void));
			}

			AstKind::Use { region_id, ident } => {
				todo!("Checking overlay activation: {region_id} {ident}")
			}
		}
	}

	Ok(out)
}

fn check_range_loop(
	ast: &Ast<AstKind, SrcPos>,
	indexes: &[AstId],
	range_start: Option<AstId>,
	range_end: Option<AstId>,
	out: &mut TypedList,
) -> Result<Type, Error> {
	// TODO - srenshaw - Handle non-table special cases

	if indexes.len() > 1 {
		return Err(Error::TooManyLoopVariables(ast.location));
	}

	let (Some(start_id), Some(end_id)) = (range_start, range_end) else {
		return Err(Error::MissingLoopBounds(ast.location));
	};

	// TODO - srenshaw - Check for constant values and Z80 issues

	let start_ast = &out[start_id];
	let end_ast = &out[end_id];

	let bound_type = meet(start_ast.typ, end_ast.typ, ast.location)?;

	let index_type = match (&start_ast.kind, &end_ast.kind) {
		// TODO - srenshaw - While I'd like to allow negative ranges and indexes for single variable
		// loops eventually, for now let's force positive bounds to simplify the rest of the compiler.
		(AstKind::Int(sval), AstKind::Int(eval)) => {
			if sval.is_negative() {
				return Err(Error::NegativeLoopBound(start_ast.location));
			}

			if eval.is_negative() {
				return Err(Error::NegativeLoopBound(end_ast.location));
			}

			if sval > eval {
				return Err(Error::NegativeLoopRange(ast.location));
			}

			let diff = eval - sval;
			if (u8::MIN as i64..=u8::MAX as i64).contains(&diff) {
				meet(bound_type, Type::U8, ast.location)?
			} else if (u16::MIN as i64..=u16::MAX as i64).contains(&diff) {
				meet(bound_type, Type::U16, ast.location)?
			} else {
				meet(bound_type, Type::U32, ast.location)?
			}
		}
		_ => bound_type,
	};

	if !index_type.is_integer() {
		return Err(Error::int_type_mismatch(index_type, ast.location));
	}

	for idx_id in indexes {
		out[*idx_id].typ = index_type;
	}

	Ok(index_type)
}

fn check_table_loop(
	prs_data: &PrsData<SrcPos>,
	ast: &Ast<AstKind, SrcPos>,
	indexes: &[AstId],
	table_id: IdentId,
	range_start: Option<AstId>,
	range_end: Option<AstId>,
	out: &mut TypedList,
) -> Result<Type, Error> {
	let table = check_table(prs_data, ast, indexes, table_id, out)?;

	let index_type = match (range_start, range_end) {
		(None, None) => {
			if table.row_count <= u8::MAX as u16 { Type::U8 } else { Type::U16 }
		}

		(None, Some(end_id)) => {
			// TODO - srenshaw - Check for constant values and Z80 issues
			let end_ast = &out[end_id];
			if let AstKind::Int(val) = &end_ast.kind {
				if val.is_negative() {
					return Err(Error::NegativeLoopBound(ast.location));
				}

				if *val >= table.row_count as i64 {
					return Err(Error::LoopEndBeyondTableSize {
						table_id,
						table_size: table.row_count,
						location: ast.location,
					});
				}
			}

			let start_type = if table.row_count <= u8::MAX as u16 { Type::U8 } else { Type::U16 };

			let index_type = meet(start_type, end_ast.typ, ast.location)?;
			if !index_type.is_integer() {
				return Err(Error::int_type_mismatch(index_type, ast.location));
			}

			index_type
		}

		(Some(start_id), None) => {
			// TODO - srenshaw - Check for constant values and Z80 issues
			let start_ast = &out[start_id];
			if let AstKind::Int(val) = &start_ast.kind {
				if val.is_negative() {
					return Err(Error::NegativeLoopBound(ast.location));
				}

				if *val >= table.row_count as i64 {
					return Err(Error::LoopStartBeyondTableSize {
						table_id,
						table_size: table.row_count,
						location: ast.location,
					});
				}
			}

			let end_type = if table.row_count <= u8::MAX as u16 { Type::U8 } else { Type::U16 };

			let index_type = meet(start_ast.typ, end_type, ast.location)?;
			if !index_type.is_integer() {
				return Err(Error::int_type_mismatch(index_type, ast.location));
			}

			index_type
		}

		(Some(start_id), Some(end_id)) => {
			// TODO - srenshaw - Check for constant values and Z80 issues
			let start_ast = &out[start_id];
			let end_ast = &out[end_id];

			if let (AstKind::Int(sval), AstKind::Int(eval)) = (&start_ast.kind, &end_ast.kind) {
				if sval.is_negative() {
					return Err(Error::NegativeLoopBound(start_ast.location));
				}

				if *sval >= table.row_count as i64 {
					return Err(Error::LoopStartBeyondTableSize {
						table_id,
						table_size: table.row_count,
						location: ast.location,
					});
				}

				if eval.is_negative() {
					return Err(Error::NegativeLoopBound(end_ast.location));
				}

				if *eval >= table.row_count as i64 {
					return Err(Error::LoopEndBeyondTableSize {
						table_id,
						table_size: table.row_count,
						location: ast.location,
					});
				}

				if sval > eval {
					return Err(Error::NegativeLoopRange(ast.location));
				}
			}

			let index_type = meet(start_ast.typ, end_ast.typ, ast.location)?;
			if !index_type.is_integer() {
				return Err(Error::int_type_mismatch(index_type, ast.location));
			}

			index_type
		}
	};

	Ok(index_type)
}

fn check_table<'a>(
	prs_data: &'a PrsData<SrcPos>,
	ast: &Ast<AstKind, SrcPos>,
	indexes: &[AstId],
	table_id: IdentId,
	out: &mut TypedList,
) -> Result<&'a crate::parser::Table, Error> {
	let table = &prs_data.tables[&table_id];

	if indexes.len() > table.fields.len() {
		return Err(Error::FieldCountMismatch {
			table_id,
			field_count: table.fields.len(),
			index_count: indexes.len(),
			location: ast.location,
		});
	}

	for idx_id in indexes.iter() {
		let idx_ast = &out[*idx_id];

		let AstKind::Ident(idx_ident) = idx_ast.kind else {
			return Err(Error::NonIdentifierField {
				field_kind: idx_ast.kind.clone(),
				location: idx_ast.location,
			});
		};

		// TODO - srenshaw - We may want to add destructuring for Records eventually.

		if idx_ast.typ != Type::Unresolved {
			return Err(Error::TypeMismatch {
				expected: Type::Unresolved,
				found: idx_ast.typ,
				location: idx_ast.location,
			});
		}

		let Some((_, f_type)) = table.fields.iter()
				.find(|(field_id,_)| *field_id == idx_ident) else {
			return Err(Error::UnknownTableField {
				table_id,
				field_id: idx_ident,
				location: ast.location,
			});
		};

		out[*idx_id].typ = *f_type;
	}

	// TODO - srenshaw - Handle Table special cases

	Ok(table)
}

/// This currently converges to a stable, type-lattice fixed-point. This works because there's a
/// finite number of types, the `meet` operator only widens types, and nodes should never
/// forward-reference, guaranteeing eventual stability and no recursive type updates.
fn resolve_types(
	mut typed_list: TypedList,
) -> Result<TypedList, Error> {
	fn update_check(outer_ast: &TypedAst, ast_id: AstId, list: &mut TypedList) -> bool {
		let ast = &mut list[ast_id];
		if ast.typ != outer_ast.typ {
			ast.typ = outer_ast.typ;
			true
		} else {
			false
		}
	}

	let mut needs_update = true;
	let mut update_count = 0;

	while needs_update {
		needs_update = false;

		for ast_id in 0..typed_list.len() {
			let ast = typed_list[ast_id].clone();
			match ast.kind {
				AstKind::Assign { lhs, rhs } |
				AstKind::BinOp { lhs, rhs, ..} => {
					needs_update |= update_check(&ast, lhs, &mut typed_list);
					needs_update |= update_check(&ast, rhs, &mut typed_list);
				}

				AstKind::Return(Some(ret)) => {
					needs_update |= update_check(&ast, ret, &mut typed_list);
				}

				// This handles both Table and Ranged based for-loops for now, we might need to split this
				// out later once negative indexes & ranges are added.
				AstKind::For { ref indexes, range_start, range_end, .. } => {
					if let Some(start_id) = range_start {
						needs_update |= update_check(&ast, start_id, &mut typed_list);
					}

					if let Some(end_id) = range_end {
						needs_update |= update_check(&ast, end_id, &mut typed_list);
					}

					for index in indexes {
						needs_update |= update_check(&ast, *index, &mut typed_list);
					}
				}

				_ => {}
			}
		}

		update_count += 1;
		debug_assert!(update_count <= 10_000, "Infinite resolution limit hit");
	}

	Ok(typed_list)
}

#[cfg(test)]
fn setup(source: &str) -> Result<IdentMap<TypedList>, error::Error> {
	let source = format!("region Stack[0] @ 0;\n{source}");

	let input = crate::input::eval(file!().into(), source.into());

	let lex_data = crate::lexer::eval(&input.source)?;

	let prs_data = crate::parser::eval(&input, &lex_data, true)?;

	let chk_data = crate::type_checker::eval(&input, &lex_data, &prs_data)?;

	Ok(chk_data)
}

#[cfg(test)]
mod literals {
	use super::*;

	#[test]
	fn u8_resolves_correctly() -> Result<(), error::Error> {
		setup("
main {
	let x: u8 = 1;
}")?;
		Ok(())
	}

// 	#[test]
// 	fn f32_resolves_correctly() -> Result<(), error::Error> {
// 		setup("
// main {
// let x: f32 = 1.0;
// }")?;
// 		Ok(())
// 	}

	#[test]
	#[should_panic="no type for item"]
	fn undeclared_variable_has_no_type() {
		setup("
main {
	let x: u8 = unknown_var;
}
		").unwrap();
	}
}

#[cfg(test)]
mod assignment {
	use super::*;

	#[test]
	fn narrow_ints_can_widen() -> Result<(), error::Error> {
		setup("
main {
	let x: u8 = 5;
	let y: u16 = x;
}
		")?;
		Ok(())
	}

	#[test]
	#[should_panic="Variable has type U8, but expression has type Dec"]
	fn cannot_assign_dec_to_int() {
		setup("
main {
	let x: u8 = 1.0;
}
		").unwrap();
	}
}

