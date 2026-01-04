
use std::collections::VecDeque;

use crate::ast::{Block as AstBlock, Id as AstId, Kind as AKind, PathSegment};
use crate::cursor::{Cursor, Error};
use crate::discovery::{ProcMap, RecordMap, Value, ValueMap};
use crate::identifier::{Id as IdentId, Map as IdentMap};
use crate::operators::BinaryOp;
use crate::task::{Kind as TaskKind, Task};
use crate::token::{Id as TokenId, Kind as TKind, Kind, KindList, PosList};
use crate::{Bounds, ProcData, Span, SrcPos};

type ParseResult<T> = Result<T, Error>;

pub fn eval(
	source: &str, identifiers: &IdentMap<Span<SrcPos>>,
	tok_list: &KindList, tok_pos: &PosList,
	records: &RecordMap,
	procedures: &ProcMap,
	task_queue: &mut VecDeque<Task>,
	values: &mut ValueMap,
	proc_db: &mut IdentMap<ProcData>,
) -> Result<(), Error> {
	let queue_start_len = task_queue.len();
	for task in &mut *task_queue {
		task.prev_queue_length = Some(queue_start_len);
	}

	while let Some(mut task) = task_queue.pop_front() {
		if let Err((token_id, err)) = parse(
			source, identifiers,
			tok_list, tok_pos,
			records,
			values, proc_db,
			&task,
		) {
			// We didn't finish...
			if token_id > task.prev_furthest_token {
				// ...but we made progress, so we're not stuck yet. Re-queue and try again.
				task.prev_furthest_token = token_id;
				task.prev_queue_length = Some(task_queue.len());
				task_queue.push_back(task);
			} else if task.prev_queue_length > Some(task_queue.len()) {
				// ...but someone else made progress, so maybe a different dependency will finish. Re-queue
				// and try again.
				task.prev_queue_length = Some(task_queue.len());
				task_queue.push_back(task);
			} else {
				task_queue.push_back(task);
				return Err(err);//.into_comp_error(&data)
						//.with_kind(error::Kind::Parser)
						//.display(&data.source_file, &data.source, &data.line_pos));
			}
		} else {
			proc_db.entry(task.name_id)
				.and_modify(|proc_data| {
					proc_data.target = procedures[&task.name_id].target;
				});
		}
	}

	Ok(())
}

fn parse(
	source: &str, identifiers: &IdentMap<Span<SrcPos>>,
	tok_list: &KindList, tok_pos: &PosList,
	records: &RecordMap,
	values: &mut ValueMap,
	proc_db: &mut IdentMap<ProcData>,
	task: &Task,
) -> Result<(), (TokenId, Error)> {
	let cursor = Cursor::new(task.tok_start);
	match task.kind {
		TaskKind::Value => parse_value(cursor, tok_list, tok_pos, values)
			.map(|value| {
				values.insert(task.name_id, value);
			}),
		TaskKind::Proc => parse_procedure(cursor, source, identifiers, tok_list, tok_pos, records)
			.map(|proc_data| {
				proc_db.insert(task.name_id, proc_data);
			}),
	}
}

fn parse_value(
	mut cursor: Cursor,
	tok_list: &KindList, tok_pos: &PosList,
	values: &ValueMap,
) -> Result<Value, (TokenId, Error)> {
	use crate::value::{Kind as VKind, value_expression};

	value_expression(&mut cursor, tok_list, &[Kind::Semicolon], Some(values))
		.map_err(|e| (cursor.index(), e))
		.and_then(|result| match result.kind {
			VKind::Int(num) => Ok(Value::Integer(num)),
			VKind::Dec(num) => Ok(Value::Decimal(num)),
			VKind::Unfinished => {
				let start = tok_pos[result.loc.start];
				let end = tok_pos[result.loc.end];
				let err = Error::UnresolvedType {
					span: (start..end).into(),
					msg: format!("{:?}", result.kind),
				};
				Err((cursor.index(), err))
			},
		})
}

fn parse_procedure(
	mut cursor: Cursor,
	source: &str, identifiers: &IdentMap<Span<SrcPos>>,
	tok_list: &KindList, tok_pos: &PosList,
	records: &RecordMap,
) -> Result<ProcData, (TokenId, Error)> {
	let cursor = &mut cursor;
	let mut proc_data = ProcData::default();
	let start = AstId::new(proc_data.ast_nodes.len());

	// skip to the procedure body
	while TKind::OBrace != cursor.peek(tok_list, 0) {
		cursor.advance();
	}

	let tok_start = cursor.index();
	let mut block = parse_block(cursor, source, identifiers, tok_list, tok_pos, records, &mut proc_data)
		.map_err(|e| (cursor.index(), e))?;
	let tok_end = cursor.index();

	let end = AstId::new(proc_data.ast_nodes.len());

	let has_return = proc_data.ast_nodes[start..end]
		.iter()
		.any(|kind| matches!(kind, AKind::Return(_)));

	if !has_return {
		let tok_pos = cursor.index();
		let ast_id = proc_data.add_ast(AKind::Return(None),
			tok_pos..tok_pos,
		);
		block.0.push(ast_id);
	}

	proc_data.ast_start = proc_data.add_ast(AKind::Block(block),
		tok_start..tok_end,
	);
	Ok(proc_data)
}

fn parse_block(
	cursor: &mut Cursor,
	source: &str,
	identifiers: &IdentMap<Span<SrcPos>>,
	tok_list: &KindList,
	tok_pos: &PosList,
	records: &RecordMap,
	proc_data: &mut ProcData,
) -> ParseResult<AstBlock> {
	cursor.expect(tok_list, TKind::OBrace)?;

	let mut block = vec![];
	while ![TKind::Eof, TKind::CBrace].contains(&cursor.current(tok_list)) {
		block.push(match cursor.current(tok_list) {
			TKind::Let => parse_let_statement(cursor, tok_list, records, proc_data)?,
			TKind::Identifier(ident_id) => parse_ident_statement(cursor, tok_list, proc_data, ident_id)?,
			TKind::OBrace => {
				let tok_start = cursor.index();
				let b = parse_block(cursor, source, identifiers, tok_list, tok_pos, records, proc_data)?;
				let tok_range = tok_start..cursor.index();
				proc_data.add_ast(AKind::Block(b), tok_range)
			}
			TKind::Return => parse_return_statement(cursor, tok_list, proc_data)?,
			TKind::If => parse_if_statement(cursor, source, identifiers, tok_list, tok_pos, records, proc_data)?,
			TKind::For => parse_for_statement(cursor, source, identifiers, tok_list, tok_pos, records, proc_data)?,
			TKind::While => parse_while_statement(cursor, source, identifiers, tok_list, tok_pos, records, proc_data)?,
			_ => return Err(cursor.expected_token("definition, assignment, return, if, or for statement")),
		});
	}

	cursor.expect(tok_list, TKind::CBrace)?;

	Ok(AstBlock(block))
}

fn parse_ident_statement(
	cursor: &mut Cursor,
	tok_list: &KindList,
	proc_data: &mut ProcData,
	ident_id: IdentId,
) -> ParseResult<AstId> {
	let left_id = parse_access(cursor, tok_list, proc_data, ident_id)?;

	match cursor.current(tok_list) {
		TKind::Eq => parse_assignment(cursor, tok_list, proc_data, left_id),
		TKind::PlusEq => parse_op_assignment(cursor, tok_list, proc_data, left_id, BinaryOp::Add),
		TKind::DashEq => parse_op_assignment(cursor, tok_list, proc_data, left_id, BinaryOp::Sub),
		TKind::StarEq => parse_op_assignment(cursor, tok_list, proc_data, left_id, BinaryOp::Mul),
		TKind::SlashEq => parse_op_assignment(cursor, tok_list, proc_data, left_id, BinaryOp::Div),
		_ => Err(cursor.expected_token("definition or assignment statement")),
	}
}

fn parse_let_statement(
	cursor: &mut Cursor,
	tok_list: &KindList,
	records: &RecordMap,
	proc_data: &mut ProcData,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.expect(tok_list, TKind::Let)?;
	let tok_ident_start = cursor.index();
	let ident_id = cursor.expect_identifier(tok_list, "variable identifier")?;
	let tok_ident_range = tok_ident_start..cursor.index();
	cursor.expect(tok_list, TKind::Colon)?;
	let var_type = cursor.expect_type(tok_list, records)?;
	cursor.expect(tok_list, TKind::Eq)?;
	let ast_id = parse_expression(cursor, tok_list, proc_data, &[TKind::Semicolon])?;
	cursor.expect(tok_list, TKind::Semicolon)?;
	let tok_range = tok_start..cursor.index();
	let ident = proc_data.add_ast(AKind::Ident(ident_id), tok_ident_range);
	let define = proc_data.add_ast(AKind::Define(ident, var_type), tok_range.clone());
	Ok(proc_data.add_ast(AKind::Assign(define, ast_id), tok_range))
}

fn parse_access(
	cursor: &mut Cursor,
	tok_list: &KindList,
	proc_data: &mut ProcData,
	ident_id: IdentId,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.advance(); // skip the identifier
	let mut accesses = vec![];
	while [TKind::Dot, TKind::OBracket].contains(&cursor.current(tok_list)) {
		match cursor.current(tok_list) {
			// a.b -> location of 'a' + offset of 'b'
			TKind::Dot => {
				cursor.expect(tok_list, TKind::Dot)?;
				let id = cursor.expect_identifier(tok_list, "field name")?;
				accesses.push(PathSegment::Field(id));
			}
			// a[b].c -> location of 'a' + 'b' * size of 'c'
			TKind::OBracket => {
				cursor.expect(tok_list, TKind::OBracket)?;
				let index = parse_expression(cursor, tok_list, proc_data, &[TKind::CBracket])?;
				cursor.expect(tok_list, TKind::CBracket)?;
				cursor.expect(tok_list, TKind::Dot)?;
				let id = cursor.expect_identifier(tok_list, "field name")?;
				accesses.push(PathSegment::Index(index, id));
			}
			_ => break,
		}
	}
	let tok_range = tok_start..cursor.index();
	let kind = if accesses.is_empty() {
		AKind::Ident(ident_id)
	} else {
		#[cfg(feature="access")]
		AKind::Access(ident_id, accesses);
		todo!()
	};
	Ok(proc_data.add_ast(kind, tok_range))
}

fn parse_assignment(
	cursor: &mut Cursor,
	tok_list: &KindList,
	proc_data: &mut ProcData,
	lvalue_id: AstId,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.expect(tok_list, TKind::Eq)?;
	let ast_id = parse_expression(cursor, tok_list, proc_data, &[TKind::Semicolon])?;
	cursor.expect(tok_list, TKind::Semicolon)?;
	let tok_range = tok_start..cursor.index();
	Ok(proc_data.add_ast(AKind::Assign(lvalue_id, ast_id), tok_range))
}

fn parse_op_assignment(
	cursor: &mut Cursor,
	tok_list: &KindList,
	proc_data: &mut ProcData,
	lvalue_id: AstId, op: BinaryOp,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.advance(); // skip the operator token
	let ast_id = parse_expression(cursor, tok_list, proc_data, &[TKind::Semicolon])?;
	cursor.expect(tok_list, TKind::Semicolon)?;
	let tok_range = tok_start..cursor.index();
	let op_id = proc_data.add_ast(AKind::BinOp(op, lvalue_id, ast_id),
		tok_range.clone());
	Ok(proc_data.add_ast(AKind::Assign(lvalue_id, op_id), tok_range))
}

fn parse_return_statement(
	cursor: &mut Cursor,
	tok_list: &KindList,
	proc_data: &mut ProcData,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.expect(tok_list, TKind::Return)?;
	let ast_id = match cursor.expect(tok_list, TKind::Semicolon) {
		Ok(_) => None,
		Err(_) => {
			let result = parse_expression(cursor, tok_list, proc_data, &[TKind::Semicolon])?;
			cursor.expect(tok_list, TKind::Semicolon)?;
			Some(result)
		},
	};
	let tok_range = tok_start..cursor.index();
	Ok(proc_data.add_ast(AKind::Return(ast_id), tok_range))
}

fn parse_if_statement(
	cursor: &mut Cursor,
	source: &str, identifiers: &IdentMap<Span<SrcPos>>,
	tok_list: &KindList, tok_pos: &PosList,
	records: &RecordMap,
	proc_data: &mut ProcData,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.expect(tok_list, TKind::If)?;
	let cond_id = parse_expression(cursor, tok_list, proc_data, &[TKind::OBrace])?;
	let then_block = parse_block(cursor, source, identifiers, tok_list, tok_pos, records, proc_data)?;
	let else_block = if TKind::Else == cursor.current(tok_list) {
		cursor.advance();
		parse_block(cursor, source, identifiers, tok_list, tok_pos, records, proc_data)?
	} else {
		AstBlock(vec![])
	};
	let tok_range = tok_start..cursor.index();
	Ok(proc_data.add_ast(AKind::If(cond_id, then_block, else_block), tok_range))
}

fn parse_while_statement(
	cursor: &mut Cursor,
	source: &str, identifiers: &IdentMap<Span<SrcPos>>,
	tok_list: &KindList, tok_pos: &PosList,
	records: &RecordMap,
	proc_data: &mut ProcData,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.expect(tok_list, TKind::While)?;
	let cond = parse_expression(cursor, tok_list, proc_data, &[TKind::OBrace])?;
	let block = parse_block(cursor, source, identifiers, tok_list, tok_pos, records, proc_data)?;
	let tok_range = tok_start..cursor.index();
	Ok(proc_data.add_ast(AKind::While(cond, block), tok_range))
}

fn parse_for_statement(
	cursor: &mut Cursor,
	source: &str, identifiers: &IdentMap<Span<SrcPos>>,
	tok_list: &KindList, tok_pos: &PosList,
	records: &RecordMap,
	proc_data: &mut ProcData,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.expect(tok_list, TKind::For)?;

	let mut vars = vec![];
	let ident_id = cursor.expect_identifier(tok_list, "identifier")?;
	vars.push(ident_id);

	while TKind::Comma == cursor.current(tok_list) {
		cursor.advance();
		let ident_id = cursor.expect_identifier(tok_list, "identifier")?;
		vars.push(ident_id);
	}

	while TKind::In != cursor.current(tok_list) {
		let ident_id = cursor.expect_identifier(tok_list, "identifier")?;
		vars.push(ident_id);
		if TKind::In == cursor.current(tok_list) {
			break;
		}
		cursor.expect(tok_list, TKind::Comma)?;
	}

	cursor.expect(tok_list, TKind::In)?;

	// for x in Table {}
	// for x in Table[..] {}
	// for x in [0..10] {}
	let (table_id, range) = if let Ok(table_id) = cursor.expect_identifier(tok_list, "COMPILER ERROR") {
		// Table loop
		let range = if TKind::OBracket == cursor.current(tok_list) {
			cursor.advance();
			let range_start = cursor.expect_u32(source, identifiers, tok_list, tok_pos, "COMPILER ERROR").ok();
			cursor.expect(tok_list, TKind::Dot2)?;
			let range_end = cursor.expect_u32(source, identifiers, tok_list, tok_pos, "COMPILER ERROR").ok();
			cursor.expect(tok_list, TKind::CBracket)?;
			match (range_start, range_end) {
				(Some(start), Some(end)) => Some(Bounds::Full { start, end }),
				(Some(start), None) => Some(Bounds::From { start }),
				(None, Some(end)) => Some(Bounds::To { end }),
				(None, None) => None,
			}
		} else {
			None
		};
		(Some(table_id), range)
	} else if TKind::OBracket == cursor.current(tok_list) {
		cursor.advance();
		let start = cursor.expect_u32(source, identifiers, tok_list, tok_pos, "start (inclusive) value")?;
		cursor.expect(tok_list, TKind::Dot2)?;
		let end = cursor.expect_u32(source, identifiers, tok_list, tok_pos, "end (exclusive) value")?;
		cursor.expect(tok_list, TKind::CBracket)?;
		(None, Some(Bounds::Full { start, end }))
	} else {
		return Err(cursor.expected_token("table name or bracketed range"));
	};

	let block = parse_block(cursor, source, identifiers, tok_list, tok_pos, records, proc_data)?;

	let tok_range = tok_start..cursor.index();
	Ok(proc_data.add_ast(AKind::For(vars, table_id, range, block), tok_range))
}

fn parse_call(
	cursor: &mut Cursor,
	tok_list: &KindList,
	proc_data: &mut ProcData,
	ident_id: IdentId,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.advance();
	cursor.expect(tok_list, TKind::OParen)?;
	let end_tokens = &[TKind::CParen, TKind::Comma];

	let mut exprs = vec![];
	while TKind::CParen != cursor.current(tok_list) {
		let expr = parse_expression(cursor, tok_list, proc_data, end_tokens)?;
		exprs.push(expr);

		if TKind::Comma == cursor.current(tok_list) {
			cursor.advance();
			if TKind::CParen == cursor.current(tok_list) {
				break;
			}
		} else if TKind::CParen != cursor.current(tok_list) {
			break;
		}
	}

	cursor.expect(tok_list, TKind::CParen)?;
	let tok_range = tok_start..cursor.index();
	Ok(proc_data.add_ast(AKind::Call(ident_id, exprs), tok_range))
}

fn parse_expression(
	cursor: &mut Cursor,
	tok_list: &KindList,
	proc_data: &mut ProcData,
	end_tokens: &[TKind],
) -> ParseResult<AstId> {
	parse_expr_main(cursor, tok_list, proc_data, 0, end_tokens)
}

fn parse_expr_main(
	cursor: &mut Cursor,
	tok_list: &KindList,
	proc_data: &mut ProcData,
	min_binding_power: usize, end_tokens: &[TKind],
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	let left = parse_primary(cursor, tok_list, proc_data)?;
	parse_expr_sub(cursor, tok_list, proc_data, min_binding_power, tok_start, left, end_tokens)
}

fn parse_expr_sub(
	cursor: &mut Cursor,
	tok_list: &KindList,
	proc_data: &mut ProcData,
	min_binding_power: usize, tok_start: TokenId, left: AstId,
	end_tokens: &[TKind],
) -> ParseResult<AstId> {
	if end_tokens.contains(&cursor.current(tok_list)) {
		return Ok(left);
	}
	let Ok(op) = cursor.expect_bin_op(tok_list) else {
		return Ok(left);
	};
	let op_binding_power = op.binding_power();
	let tok_start_inner = cursor.index();
	let right = parse_primary(cursor, tok_list, proc_data)?;
	let right = if min_binding_power <= op_binding_power {
		parse_expr_sub(cursor, tok_list, proc_data, op_binding_power,
			tok_start_inner, right, end_tokens)?
	} else {
		right
	};

	let tok_range = tok_start..cursor.index();
	Ok(proc_data.add_ast(AKind::BinOp(op, left, right), tok_range))
}

fn parse_primary(
	cursor: &mut Cursor,
	tok_list: &KindList,
	proc_data: &mut ProcData,
) -> ParseResult<AstId> {
	let tok_start_op = cursor.index();
	let unary_op = cursor.expect_unary_op(tok_list);
	let tok_end_op = cursor.index();

	fn primary_node(cursor: &mut Cursor, proc_data: &mut ProcData, kind: AKind) -> AstId {
		let tok_start_expr = cursor.index();
		cursor.advance();
		let tok_range = tok_start_expr..cursor.index();
		proc_data.add_ast(kind, tok_range)
	}

	let node = match cursor.current(tok_list) {
		TKind::Identifier(ident_id) => {
			match cursor.peek(tok_list, 1) {
				TKind::OParen => parse_call(cursor, tok_list, proc_data, ident_id)?,
				TKind::Dot | TKind::OBracket => parse_access(cursor, tok_list, proc_data, ident_id)?,
				_ => primary_node(cursor, proc_data, AKind::Ident(ident_id)),
			}
		}
		TKind::Integer(num) => primary_node(cursor, proc_data, AKind::Int(num)),
		TKind::Decimal(num) => primary_node(cursor, proc_data, AKind::Dec(num)),
		TKind::True => primary_node(cursor, proc_data, AKind::Int(1)),
		TKind::False => primary_node(cursor, proc_data, AKind::Int(0)),
		TKind::OParen => {
			cursor.advance();
			let expr = parse_expression(cursor, tok_list, proc_data, &[TKind::CParen])?;
			cursor.expect(tok_list, TKind::CParen)?;
			expr
		}
		_ => return Err(cursor.expected_token("identifier or number")),
	};

	if let Some(op) = unary_op {
		let tok_range = tok_start_op..tok_end_op;
		Ok(proc_data.add_ast(AKind::UnOp(op, node), tok_range))
	} else {
		Ok(node)
	}
}

#[cfg(test)]
mod can_parse_proc {
	use crate::{ error, lexer, discovery, ast };
	use crate::identifier::Identifier;
	use crate::Data;

	use super::*;

	fn setup(source: &str) -> Data {
		let mut db = Data::new("parser".to_string(), source.into());
		db.DEBUG_show_tokens = true;

		let result = {
			lexer::eval(
				&db.source,
				&mut db.identifiers,
				&mut db.tok_list,
				&mut db.tok_pos,
				&mut db.line_pos,
			).map_err(|e| e.with_kind(error::Kind::Lexer))
		}.and_then(|_| {
			discovery::eval(
				&db.source, &db.identifiers,
				&db.tok_list, &db.tok_pos,
				&mut db.task_queue,
				&mut db.procedures,
				&mut db.records,
				&mut db.regions,
				&mut db.values,
			).map_err(|e| e.into_comp_error(&db, error::Kind::Discovery))
		}).and_then(|_| {
			eval(
				&db.source, &db.identifiers,
				&db.tok_list, &db.tok_pos,
				&db.records, &db.procedures,
				&mut db.task_queue,
				&mut db.values,
				&mut db.proc_db,
			).map_err(|e| e.into_comp_error(&db, error::Kind::Parser))
		});

		match result {
			Ok(_) => db,
			Err(e) => {
				let msg = e.display(
					&db.source_file, &db.source, &db.line_pos);
				panic!("{msg}");
			}
		}
	}

	#[test]
	fn main() {
		let db = setup("main{}");
		assert_eq!(db.proc_db[&"main".id()].ast_nodes, [
			ast::Kind::Return(None),
			ast::Kind::Block(ast::Block(vec![0.into()])),
		]);
	}

	#[test]
	fn values_basic() {
		let db = setup("value a = 2;");
		assert!(db.task_queue.is_empty());
		let a_id = "a".id();
		assert!(db.values.contains_key(&a_id));
		assert_eq!(db.values[&a_id], Value::Integer(2));
	}

	#[test]
	fn values_expressions() {
		let db = setup("value a = 2 * 5;");
		assert!(db.task_queue.is_empty());
		let a_id = "a".id();
		assert!(db.values.contains_key(&a_id));
		assert_eq!(db.values[&a_id], Value::Integer(10));
	}

	#[test]
	fn values_compound() {
		let db = setup("value a = 3; value b = a * 5;");
		assert!(db.task_queue.is_empty());
		let a_id = "a".id();
		let b_id = "b".id();
		assert!(db.values.contains_key(&a_id));
		assert!(db.values.contains_key(&b_id));
		assert_eq!(db.values[&a_id], Value::Integer(3));
		assert_eq!(db.values[&b_id], Value::Integer(15));
	}

	#[test]
	fn values_compound_inverted() {
		let db = setup("value b = a * 5; value a = 3;");
		assert!(db.task_queue.is_empty());
		let a_id = "a".id();
		let b_id = "b".id();
		assert!(db.values.contains_key(&a_id));
		assert!(db.values.contains_key(&b_id));
		assert_eq!(db.values[&a_id], Value::Integer(3));
		assert_eq!(db.values[&b_id], Value::Integer(15));
	}

	#[test]
	fn with_no_params() {
		let db = setup("main{} proc a() {}");
		assert!(db.proc_db.contains_key(&"a".id()));
	}

	#[test]
	fn with_internal_expressions() {
		let db = setup("main { let a: s8 = 2 + 3; }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_sub_expressions() {
		let db = setup("main { let a: s8 = (2 + 3) * (4 - 5); }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_return() {
		let db = setup("main{} proc a() -> s8 {}");
		assert!(db.proc_db.contains_key(&"a".id()));
	}

	#[test]
	fn with_single_param() {
		let db = setup("main{} proc a(b:s8) {}");
		assert!(db.proc_db.contains_key(&"a".id()));
	}

	#[test]
	fn with_multi_params() {
		let db = setup("main{} proc a(b:s8,c:s8) {}");
		assert!(db.proc_db.contains_key(&"a".id()));
	}

	#[cfg(feature="record")]
	#[test]
	fn with_record_param() {
		let db = setup("main{} record a {} proc b(c:a) {}");
		assert!(db.proc_db.contains_key(&"b".id()));
	}

	#[cfg(feature="record")]
	#[test]
	fn with_out_of_order_record_param() {
		let db = setup("main{} proc b(c:a) {} record a {}");
		assert!(db.proc_db.contains_key(&"b".id()));
	}

	#[cfg(feature="table")]
	#[test]
	fn with_table_param() {
		let db = setup("main{} table a[0] {} proc b(c:a) {}");
		assert!(db.proc_db.contains_key(&"b".id()));
	}

	#[test]
	fn with_basic_for_loop() {
		let db = setup("main { for i in [0..10] {} }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_multi_element_for_loop() {
		let db = setup("main { for i,j in [0..10] {} }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="forloop")]
	#[test]
	fn with_table_for_loop() {
		let db = setup("main { for i in a {} }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="forloop")]
	#[test]
	fn with_table_index_for_loop() {
		let db = setup("main { for i in a[0..10] {} }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="forloop")]
	#[test]
	fn with_table_from_for_loop() {
		let db = setup("main { for i in a[0..] {} }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="forloop")]
	#[test]
	fn with_table_to_for_loop() {
		let db = setup("main { for i in a[..10] {} }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="forloop")]
	#[test]
	fn with_table_full_for_loop() {
		let db = setup("main { for i in a[..] {} }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="index")]
	#[test]
	fn with_internal_table_index() {
		let db = setup("main { return a[10].b; }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="index")]
	#[test]
	fn with_internal_table_expression_indexing() {
		let db = setup("main { return a[2 + 4].b; }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_proc_call() {
		let db = setup("main { return a(3); }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_proc_call_in_subexpression() {
		let db = setup("main { return 3 * a(3); }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_proc_call_with_expression_argument() {
		let db = setup("main { return a(b + 4); }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_proc_call_with_multiple_arguments() {
		let db = setup("main { return a(2, 4 / b, b + 4); }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_while_loop() {
		let db = setup("main { while true { } }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="access")]
	#[test]
	fn with_internal_field_assign() {
		let db = setup("main { a.b = 2; }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="access")]
	#[test]
	fn with_internal_table_assign() {
		let db = setup("main { a[3].b = 2; }");
		assert!(db.proc_db.contains_key(&"main".id()));
	}
}

