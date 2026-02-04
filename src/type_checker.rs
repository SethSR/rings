
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
	AlreadyDefined { ident_id: IdentId, location: Span<SrcPos> },
	TypeMismatch { expected: Type, found: Type, location: Span<SrcPos> },
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
	MissingLoopBounds(Span<SrcPos>),
	NonIdentifierField {
		field_kind: AstKind,
		location: Span<SrcPos>,
	},
	UnknownField {
		table_id: IdentId,
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
	NoType(Ast<AstKind, SrcPos>),
}

impl Error {
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
				let message = "simple for-loops require a single loop variable".to_string();
				error::Error::new(location, message)
			}
			Self::NegativeLoopRange(location) => {
				let message = "start value must be less than or equal to end value".to_string();
				error::Error::new(location, message)
			}
			Self::MissingLoopBounds(location) => {
				let message = "simple for-loops require a fully specified range '[start..end]'".to_string();
				error::Error::new(location, message)
			}
			Self::NonIdentifierField { field_kind, location } => {
				let message = format!("Expected an identifier, found '{field_kind}'");
				error::Error::new(location, message)
			}
			Self::UnknownField { table_id, field_id, location } => {
				let message = format!("Unknown field '{}' in table '{}'",
					lex_data.text(input, &field_id),
					lex_data.text(input, &table_id),
				);
				error::Error::new(location, message)
			}
			Self::UnknownRegion { region_id , location } => {
				let message = format!("Unknown region '{}'", lex_data.text(input, &region_id));
				error::Error::new(location.clone(), message)
			}
			Self::UnknownMark { region_id, mark_id, location } => {
				let message = format!("Unknown mark '{}' for region '{}'",
					lex_data.text(input, &mark_id),
					lex_data.text(input, &region_id),
				);
				error::Error::new(location, message)
			}
			Self::NoType(ast) => {
				let message = format!("no type for item {ast:?}");
				error::Error::new(ast.location, message)
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

	// Check procedures
	let mut typed_procedures = IdentMap::with_capacity(prs_data.procedures.len());
	for proc_id in prs_data.procedures.keys() {
		match check_proc(&prs_data, *proc_id).and_then(resolve_types) {
			Ok(new_ast) => {
				typed_procedures.insert(*proc_id, new_ast);
			}

			Err(err) => {
				return Err(err.into_comp_error(input, lex_data)
						.with_kind(error::Kind::Checker));
			}
		}
	}

	Ok(typed_procedures)
}

fn meet_ast(
	lhs: &TypedAst,
	rhs: &TypedAst,
) -> Result<Type, Error> {
	meet(lhs.typ, rhs.typ, lhs.location + rhs.location)
}

fn meet(lhs: Type, rhs: Type, location: Span<SrcPos>,
) -> Result<Type, Error> {
	match (lhs, rhs) {
		(Type::Unknown, typ) |
		(typ, Type::Unknown) => Ok(typ),
		(Type::Int, Type::Dec) |
		(Type::Dec, Type::Int) => Ok(Type::Dec),
		(Type::Int, typ) |
		(typ, Type::Int) if typ.is_integer() => Ok(typ),
		(Type::U8, Type::S16) |
		(Type::S16, Type::U8) => Ok(Type::S16),
		(Type::U8, Type::S32) |
		(Type::S32, Type::U8) => Ok(Type::S32),
		(Type::U16, Type::S32) |
		(Type::S32, Type::U16) => Ok(Type::S32),
		(Type::S8, Type::S16) |
		(Type::S16, Type::S8) => Ok(Type::S16),
		(Type::S8, Type::S32) |
		(Type::S32, Type::S8) => Ok(Type::S32),
		(Type::S16, Type::S32) |
		(Type::S32, Type::S16) => Ok(Type::S32),
		(a, b) if a == b => Ok(a),
		_ => Err(Error::TypeMismatch {
			expected: lhs,
			found: rhs,
			location,
		}),
	}
}

fn check_proc(
	prs_data: &PrsData<SrcPos>,
	proc_id: IdentId,
) -> Result<TypedList, Error> {
	let mut out = TypedList::default();
	let mut scope_depth = 0;
	let mut mark_set = HashSet::<IdentId>::default();

	let proc_data = &prs_data.procedures[&proc_id];

	for ast in &proc_data.body {
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
							.ok_or_else(|| Error::NoType(ast.clone()))?,
				};
				out.push(TypedAst::new(ast, ex_type));
			}

			AstKind::Assign { lhs, rhs } => {
				let lhs_ast = &out[lhs];
				let rhs_ast = &out[rhs];
				let ex_type = meet_ast(lhs_ast, rhs_ast)?;
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
					return Err(Error::TypeMismatch {
						expected: Type::Int,
						found: cond_type,
						location: ast.location,
					});
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
					return Err(Error::TypeMismatch {
						expected: Type::Int,
						found: cond_type,
						location: ast.location,
					});
				}
				out.push(TypedAst::new(ast, Type::Void));
			}

			AstKind::For { ref indexes, table, range_start, range_end, ..} => {
				fn get_bounds_type(list: &TypedList, location: Span<SrcPos>, ast_id_opt: Option<AstId>) -> Result<Type, Error> {
					let Some(ast_id) = ast_id_opt else {
						return Err(Error::MissingLoopBounds(location));
					};
					let ast_type = list[ast_id].typ;

					// TODO - srenshaw - Check for constant values

					Ok(ast_type)
				}

				if let Some((table_id, table)) = table
						.map(|id| (id, &prs_data.tables[&id]))
				{
					if table.fields.len() < indexes.len() {
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
								location: out[*idx_id].location,
							});
						};

						// TODO - srenshaw - We may want to add destructuring for Records eventually.

						if idx_ast.typ != Type::Unknown {
							return Err(Error::TypeMismatch {
								expected: Type::Unknown,
								found: idx_ast.typ,
								location: out[*idx_id].location,
							});
						}

						let Some((_, f_type)) = table.fields.iter()
								.find(|field| field.0 == idx_ident) else {
							return Err(Error::UnknownField {
								table_id,
								field_id: idx_ident,
								location: ast.location,
							});
						};

						out[*idx_id].typ = *f_type;
					}

					// TODO - srenshaw - Handle Table special cases

					let start_type = get_bounds_type(&out, ast.location, range_start)?;
					let end_type = get_bounds_type(&out, ast.location, range_end)?;

					let bound_type = meet(start_type, end_type, ast.location)?;
					if !bound_type.is_integer() {
						return Err(Error::TypeMismatch {
							expected: Type::Int,
							found: bound_type,
							location: ast.location,
						});
					}

					out.push(TypedAst::new(ast, bound_type));
				} else {
					if indexes.len() > 1 {
						return Err(Error::TooManyLoopVariables(ast.location));
					}

					for idx_id in indexes {
						out[*idx_id].typ = Type::Int;
					}

					// TODO - srenshaw - Handle non-table special cases

					if let Some(start_id) = range_start {
						if let AstKind::Int(val) = out[start_id].kind {
							//
						}
					}
					let start_type = get_bounds_type(&out, ast.location, range_start)?;
					if let Some(end_id) = range_end {
						if let AstKind::Int(val) = out[end_id].kind {
							//
						}
					}
					let end_type = get_bounds_type(&out, ast.location, range_end)?;

					let bound_type = meet(start_type, end_type, ast.location)?;
					let index_type = out[indexes[0]].typ;

					let loop_type = meet(index_type, bound_type, ast.location)?;
					if !loop_type.is_integer() {
						return Err(Error::TypeMismatch {
							expected: Type::Int,
							found: loop_type,
							location: ast.location,
						});
					}

					out.push(TypedAst::new(ast, loop_type));
				}
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
					let arg_type = out[*arg_id].typ;
					meet(*p_type, arg_type, out[*arg_id].location)?;
				}

				out.push(TypedAst::new(ast, proc.ret_type));
			}

			AstKind::Access { base_id, ref path } => {
				let mut curr_id = base_id;
				let mut typ = None;

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
							typ = Some(*field_type);
						}
						PathSegment::Index(expr_id, field_id) => {
							todo!("table-index-2: [{expr_id}].{field_id}")
						}
					}
				}

				if typ.is_none() {
					return Err(Error::NoType(ast.clone()));
				}

				out.push(TypedAst::new(ast, typ.unwrap()));
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

fn resolve_types(
	mut typed_list: TypedList,
) -> Result<TypedList, Error> {
	let mut needs_update = true;

	while needs_update {
		needs_update = false;
		for ast_id in 0..typed_list.len() {
			let ast = typed_list[ast_id].clone();
			match ast.kind {
				AstKind::Assign { lhs, rhs } |
				AstKind::BinOp { lhs, rhs, ..} => {
					let lhs_ast = &mut typed_list[lhs];
					if lhs_ast.typ != ast.typ {
						lhs_ast.typ = ast.typ;
						needs_update = true;
					}

					let rhs_ast = &mut typed_list[rhs];
					if rhs_ast.typ != ast.typ {
						rhs_ast.typ = ast.typ;
						needs_update = true;
					}
				}

				AstKind::Return(Some(ret)) => {
					let ret_ast = &mut typed_list[ret];
					if ret_ast.typ != ast.typ {
						ret_ast.typ = ast.typ;
						needs_update = true;
					}
				}

				AstKind::For { ref indexes, range_start, range_end, .. } => {
					let start_id = range_start.unwrap();
					let start = &mut typed_list[start_id];
					if start.typ != ast.typ {
						start.typ = ast.typ;
						needs_update = true;
					}

					let end_id = range_end.unwrap();
					let end = &mut typed_list[end_id];
					if end.typ != ast.typ {
						end.typ = ast.typ;
						needs_update = true;
					}

					for index in indexes {
						let index_ast = &mut typed_list[*index];
						if index_ast.typ != ast.typ {
							index_ast.typ = ast.typ;
							needs_update = true;
						}
					}
				}

				_ => {}
			}
		}
	}

	Ok(typed_list)
}

// TODO - srenshaw - Add type-checker tests
