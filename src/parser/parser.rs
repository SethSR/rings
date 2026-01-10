
use std::collections::{HashMap, VecDeque};
use std::ops::Range;

use crate::ast::{Block as AstBlock, Id as AstId, Kind as AKind, PathSegment};
use crate::cursor::{Cursor, Error};
use crate::identifier::{Id as IdentId, Map as IdentMap};
use crate::input::Data as InputData;
use crate::lexer::Data as LexData;
use crate::operators::BinaryOp;
use crate::task::Task;
use crate::token::{Id as TokenId, Kind as TKind};
use crate::{ast, identifier, rings_type, Bounds, Target};

use super::discovery::{Data as DscData, RecordMap};

type ParseResult<T> = Result<T, Error>;

#[derive(Debug, Default, Clone)]
pub struct ProcData {
	pub target: Option<Target>,
	pub ast_start: ast::Id,
	pub ast_nodes: ast::KindList,
	pub ast_pos_tok: ast::LocList,
	/* Type Checking */
	pub ast_to_type: HashMap<ast::Id, rings_type::Type>,
	pub ident_to_type: identifier::Map<rings_type::Type>,
}

impl ProcData {
	pub fn add_ast(&mut self, kind: ast::Kind,
		tok_range: Range<TokenId>,
	) -> ast::Id {
		self.ast_nodes.push(kind);
		self.ast_pos_tok.push(tok_range.into());
		ast::Id::new(self.ast_nodes.len() - 1)
	}
}

pub fn print(
	proc_db: &IdentMap<ProcData>,
	input: &InputData,
	lex_data: &LexData,
) {
	if proc_db.is_empty() {
		return;
	}

	for (proc_id, proc_data) in proc_db.iter() {
		let proc_name = crate::text(input, lex_data, proc_id);

		if !proc_data.ast_to_type.is_empty() {
			println!();
			println!("{:<32} | {:<8} | TYPE",
				"PROCEDURE", "AST-ID");
			println!("{:-<32} | {:-<8} | {:-<8}", "", "", "");
			for (ast_id, rings_type) in &proc_data.ast_to_type {
				let ast = ast_id.to_string();
				let ast_type = crate::type_text(input, lex_data, rings_type);
				println!("{proc_name:<32} | {ast:<8} | {ast_type:<8}");
			}
		}

		if !proc_data.ident_to_type.is_empty() {
			println!();
			println!("{:<32} | {:<16} | TYPE",
				"PROCEDURE", "VARIABLE");
			println!("{:-<32} | {:-<16} | {:-<8}", "", "", "");
			for (ident_id, rings_type) in &proc_data.ident_to_type {
				let var_name = crate::text(input, lex_data, ident_id);
				let var_type = crate::type_text(input, lex_data, rings_type);
				println!("{proc_name:<32} | {var_name:<16} | {var_type:<8}");
			}
		}
	}
}

pub fn eval(
	input: &InputData,
	lex_data: &LexData,
	dsc_data: &mut DscData,
	mut task_queue: VecDeque<Task>,
) -> Result<IdentMap<ProcData>, Error> {
	let mut out = IdentMap::<ProcData>::default();

	let queue_start_len = task_queue.len();
	for task in &mut task_queue {
		task.prev_queue_length = Some(queue_start_len);
	}

	while let Some(mut task) = task_queue.pop_front() {
		if let Err((token_id, err)) = parse(input, lex_data, dsc_data, &mut out, &task) {
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
				return Err(err);
			}
		} else {
			out.entry(task.name_id)
				.and_modify(|proc_data| {
					proc_data.target = dsc_data.procedures[&task.name_id].target;
				});
		}
	}

	Ok(out)
}

fn parse(
	input: &InputData,
	lex_data: &LexData,
	dsc_data: &mut DscData,
	proc_db: &mut IdentMap<ProcData>,
	task: &Task,
) -> Result<(), (TokenId, Error)> {
	let mut cursor = Cursor::new(task.tok_start);
	let mut proc_data = ProcData::default();
	let start = AstId::new(proc_data.ast_nodes.len());

	// skip to the procedure body
	while TKind::OBrace != cursor.peek(lex_data, 0) {
		cursor.advance();
	}

	let tok_start = cursor.index();
	let mut block = parse_block(&mut cursor, input, lex_data, &dsc_data.records, &mut proc_data)
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
	proc_db.insert(task.name_id, proc_data);

	Ok(())
}

fn parse_block(
	cursor: &mut Cursor,
	input: &InputData,
	lex_data: &LexData,
	records: &RecordMap,
	proc_data: &mut ProcData,
) -> ParseResult<AstBlock> {
	cursor.expect(lex_data, TKind::OBrace)?;

	let mut block = vec![];
	while ![TKind::Eof, TKind::CBrace].contains(&cursor.current(lex_data)) {
		block.push(match cursor.current(lex_data) {
			TKind::Let => parse_let_statement(cursor, lex_data, records, proc_data)?,
			TKind::Identifier(ident_id) => parse_ident_statement(cursor, lex_data, proc_data, ident_id)?,
			TKind::OBrace => {
				let tok_start = cursor.index();
				let b = parse_block(cursor, input, lex_data, records, proc_data)?;
				let tok_range = tok_start..cursor.index();
				proc_data.add_ast(AKind::Block(b), tok_range)
			}
			TKind::Return => parse_return_statement(cursor, lex_data, proc_data)?,
			TKind::If => parse_if_statement(cursor, input, lex_data, records, proc_data)?,
			TKind::For => parse_for_statement(cursor, input, lex_data, records, proc_data)?,
			TKind::While => parse_while_statement(cursor, input, lex_data, records, proc_data)?,
			_ => return Err(cursor.expected_token("definition, assignment, return, if, or for statement")),
		});
	}

	cursor.expect(lex_data, TKind::CBrace)?;

	Ok(AstBlock(block))
}

fn parse_ident_statement(
	cursor: &mut Cursor,
	lex_data: &LexData,
	proc_data: &mut ProcData,
	ident_id: IdentId,
) -> ParseResult<AstId> {
	let left_id = parse_access(cursor, lex_data, proc_data, ident_id)?;

	match cursor.current(lex_data) {
		TKind::Eq => parse_assignment(cursor, lex_data, proc_data, left_id),
		TKind::PlusEq => parse_op_assignment(cursor, lex_data, proc_data, left_id, BinaryOp::Add),
		TKind::DashEq => parse_op_assignment(cursor, lex_data, proc_data, left_id, BinaryOp::Sub),
		TKind::StarEq => parse_op_assignment(cursor, lex_data, proc_data, left_id, BinaryOp::Mul),
		TKind::SlashEq => parse_op_assignment(cursor, lex_data, proc_data, left_id, BinaryOp::Div),
		_ => Err(cursor.expected_token("definition or assignment statement")),
	}
}

fn parse_let_statement(
	cursor: &mut Cursor,
	lex_data: &LexData,
	records: &RecordMap,
	proc_data: &mut ProcData,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.expect(lex_data, TKind::Let)?;
	let tok_ident_start = cursor.index();
	let ident_id = cursor.expect_identifier(lex_data, "variable identifier")?;
	let tok_ident_range = tok_ident_start..cursor.index();
	cursor.expect(lex_data, TKind::Colon)?;
	let var_type = cursor.expect_type(lex_data, records)?;
	cursor.expect(lex_data, TKind::Eq)?;
	let ast_id = parse_expression(cursor, lex_data, proc_data, &[TKind::Semicolon])?;
	cursor.expect(lex_data, TKind::Semicolon)?;
	let tok_range = tok_start..cursor.index();
	let ident = proc_data.add_ast(AKind::Ident(ident_id), tok_ident_range);
	let define = proc_data.add_ast(AKind::Define(ident, var_type), tok_range.clone());
	Ok(proc_data.add_ast(AKind::Assign(define, ast_id), tok_range))
}

fn parse_access(
	cursor: &mut Cursor,
	lex_data: &LexData,
	proc_data: &mut ProcData,
	ident_id: IdentId,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.advance(); // skip the identifier
	let mut accesses = vec![];
	while [TKind::Dot, TKind::OBracket].contains(&cursor.current(lex_data)) {
		match cursor.current(lex_data) {
			// a.b -> location of 'a' + offset of 'b'
			TKind::Dot => {
				cursor.expect(lex_data, TKind::Dot)?;
				let id = cursor.expect_identifier(lex_data, "field name")?;
				accesses.push(PathSegment::Field(id));
			}
			// a[b].c -> location of 'a' + 'b' * size of 'c'
			TKind::OBracket => {
				cursor.expect(lex_data, TKind::OBracket)?;
				let index = parse_expression(cursor, lex_data, proc_data, &[TKind::CBracket])?;
				cursor.expect(lex_data, TKind::CBracket)?;
				cursor.expect(lex_data, TKind::Dot)?;
				let id = cursor.expect_identifier(lex_data, "field name")?;
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
	lex_data: &LexData,
	proc_data: &mut ProcData,
	lvalue_id: AstId,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.expect(lex_data, TKind::Eq)?;
	let ast_id = parse_expression(cursor, lex_data, proc_data, &[TKind::Semicolon])?;
	cursor.expect(lex_data, TKind::Semicolon)?;
	let tok_range = tok_start..cursor.index();
	Ok(proc_data.add_ast(AKind::Assign(lvalue_id, ast_id), tok_range))
}

fn parse_op_assignment(
	cursor: &mut Cursor,
	lex_data: &LexData,
	proc_data: &mut ProcData,
	lvalue_id: AstId, op: BinaryOp,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.advance(); // skip the operator token
	let ast_id = parse_expression(cursor, lex_data, proc_data, &[TKind::Semicolon])?;
	cursor.expect(lex_data, TKind::Semicolon)?;
	let tok_range = tok_start..cursor.index();
	let op_id = proc_data.add_ast(AKind::BinOp(op, lvalue_id, ast_id),
		tok_range.clone());
	Ok(proc_data.add_ast(AKind::Assign(lvalue_id, op_id), tok_range))
}

fn parse_return_statement(
	cursor: &mut Cursor,
	lex_data: &LexData,
	proc_data: &mut ProcData,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.expect(lex_data, TKind::Return)?;
	let ast_id = match cursor.expect(lex_data, TKind::Semicolon) {
		Ok(_) => None,
		Err(_) => {
			let result = parse_expression(cursor, lex_data, proc_data, &[TKind::Semicolon])?;
			cursor.expect(lex_data, TKind::Semicolon)?;
			Some(result)
		},
	};
	let tok_range = tok_start..cursor.index();
	Ok(proc_data.add_ast(AKind::Return(ast_id), tok_range))
}

fn parse_if_statement(
	cursor: &mut Cursor,
	input: &InputData,
	lex_data: &LexData,
	records: &RecordMap,
	proc_data: &mut ProcData,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.expect(lex_data, TKind::If)?;
	let cond_id = parse_expression(cursor, lex_data, proc_data, &[TKind::OBrace])?;
	let then_block = parse_block(cursor, input, lex_data, records, proc_data)?;
	let else_block = if TKind::Else == cursor.current(lex_data) {
		cursor.advance();
		parse_block(cursor, input, lex_data, records, proc_data)?
	} else {
		AstBlock(vec![])
	};
	let tok_range = tok_start..cursor.index();
	Ok(proc_data.add_ast(AKind::If(cond_id, then_block, else_block), tok_range))
}

fn parse_while_statement(
	cursor: &mut Cursor,
	input: &InputData,
	lex_data: &LexData,
	records: &RecordMap,
	proc_data: &mut ProcData,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.expect(lex_data, TKind::While)?;
	let cond = parse_expression(cursor, lex_data, proc_data, &[TKind::OBrace])?;
	let block = parse_block(cursor, input, lex_data, records, proc_data)?;
	let tok_range = tok_start..cursor.index();
	Ok(proc_data.add_ast(AKind::While(cond, block), tok_range))
}

fn parse_for_statement(
	cursor: &mut Cursor,
	input: &InputData,
	lex_data: &LexData,
	records: &RecordMap,
	proc_data: &mut ProcData,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.expect(lex_data, TKind::For)?;

	let mut vars = vec![];
	let ident_id = cursor.expect_identifier(lex_data, "identifier")?;
	vars.push(ident_id);

	while TKind::Comma == cursor.current(lex_data) {
		cursor.advance();
		let ident_id = cursor.expect_identifier(lex_data, "identifier")?;
		vars.push(ident_id);
	}

	while TKind::In != cursor.current(lex_data) {
		let ident_id = cursor.expect_identifier(lex_data, "identifier")?;
		vars.push(ident_id);
		if TKind::In == cursor.current(lex_data) {
			break;
		}
		cursor.expect(lex_data, TKind::Comma)?;
	}

	cursor.expect(lex_data, TKind::In)?;

	// for x in Table {}
	// for x in Table[..] {}
	// for x in [0..10] {}
	let (table_id, range) = if let Ok(table_id) = cursor.expect_identifier(lex_data, "COMPILER ERROR") {
		// Table loop
		let range = if TKind::OBracket == cursor.current(lex_data) {
			cursor.advance();
			let range_start = cursor.expect_u32(input, lex_data, "COMPILER ERROR").ok();
			cursor.expect(lex_data, TKind::Dot2)?;
			let range_end = cursor.expect_u32(input, lex_data, "COMPILER ERROR").ok();
			cursor.expect(lex_data, TKind::CBracket)?;
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
	} else if TKind::OBracket == cursor.current(lex_data) {
		cursor.advance();
		let start = cursor.expect_u32(input, lex_data, "start (inclusive) value")?;
		cursor.expect(lex_data, TKind::Dot2)?;
		let end = cursor.expect_u32(input, lex_data, "end (exclusive) value")?;
		cursor.expect(lex_data, TKind::CBracket)?;
		(None, Some(Bounds::Full { start, end }))
	} else {
		return Err(cursor.expected_token("table name or bracketed range"));
	};

	let block = parse_block(cursor, input, lex_data, records, proc_data)?;

	let tok_range = tok_start..cursor.index();
	Ok(proc_data.add_ast(AKind::For(vars, table_id, range, block), tok_range))
}

fn parse_call(
	cursor: &mut Cursor,
	lex_data: &LexData,
	proc_data: &mut ProcData,
	ident_id: IdentId,
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	cursor.advance();
	cursor.expect(lex_data, TKind::OParen)?;
	let end_tokens = &[TKind::CParen, TKind::Comma];

	let mut exprs = vec![];
	while TKind::CParen != cursor.current(lex_data) {
		let expr = parse_expression(cursor, lex_data, proc_data, end_tokens)?;
		exprs.push(expr);

		if TKind::Comma == cursor.current(lex_data) {
			cursor.advance();
			if TKind::CParen == cursor.current(lex_data) {
				break;
			}
		} else if TKind::CParen != cursor.current(lex_data) {
			break;
		}
	}

	cursor.expect(lex_data, TKind::CParen)?;
	let tok_range = tok_start..cursor.index();
	Ok(proc_data.add_ast(AKind::Call(ident_id, exprs), tok_range))
}

fn parse_expression(
	cursor: &mut Cursor,
	lex_data: &LexData,
	proc_data: &mut ProcData,
	end_tokens: &[TKind],
) -> ParseResult<AstId> {
	parse_expr_main(cursor, lex_data, proc_data, 0, end_tokens)
}

fn parse_expr_main(
	cursor: &mut Cursor,
	lex_data: &LexData,
	proc_data: &mut ProcData,
	min_binding_power: usize, end_tokens: &[TKind],
) -> ParseResult<AstId> {
	let tok_start = cursor.index();
	let left = parse_primary(cursor, lex_data, proc_data)?;
	parse_expr_sub(cursor, lex_data, proc_data, min_binding_power, tok_start, left, end_tokens)
}

fn parse_expr_sub(
	cursor: &mut Cursor,
	lex_data: &LexData,
	proc_data: &mut ProcData,
	min_binding_power: usize, tok_start: TokenId, left: AstId,
	end_tokens: &[TKind],
) -> ParseResult<AstId> {
	if end_tokens.contains(&cursor.current(lex_data)) {
		return Ok(left);
	}
	let Ok(op) = cursor.expect_bin_op(lex_data) else {
		return Ok(left);
	};
	let op_binding_power = op.binding_power();
	let tok_start_inner = cursor.index();
	let right = parse_primary(cursor, lex_data, proc_data)?;
	let right = if min_binding_power <= op_binding_power {
		parse_expr_sub(cursor, lex_data, proc_data, op_binding_power,
			tok_start_inner, right, end_tokens)?
	} else {
		right
	};

	let tok_range = tok_start..cursor.index();
	Ok(proc_data.add_ast(AKind::BinOp(op, left, right), tok_range))
}

fn parse_primary(
	cursor: &mut Cursor,
	lex_data: &LexData,
	proc_data: &mut ProcData,
) -> ParseResult<AstId> {
	let tok_start_op = cursor.index();
	let unary_op = cursor.expect_unary_op(lex_data);
	let tok_end_op = cursor.index();

	fn primary_node(cursor: &mut Cursor, proc_data: &mut ProcData, kind: AKind) -> AstId {
		let tok_start_expr = cursor.index();
		cursor.advance();
		let tok_range = tok_start_expr..cursor.index();
		proc_data.add_ast(kind, tok_range)
	}

	let node = match cursor.current(lex_data) {
		TKind::Identifier(ident_id) => {
			match cursor.peek(lex_data, 1) {
				TKind::OParen => parse_call(cursor, lex_data, proc_data, ident_id)?,
				TKind::Dot | TKind::OBracket => parse_access(cursor, lex_data, proc_data, ident_id)?,
				_ => primary_node(cursor, proc_data, AKind::Ident(ident_id)),
			}
		}
		TKind::Integer(num) => primary_node(cursor, proc_data, AKind::Int(num)),
		TKind::Decimal(num) => primary_node(cursor, proc_data, AKind::Dec(num)),
		TKind::True => primary_node(cursor, proc_data, AKind::Int(1)),
		TKind::False => primary_node(cursor, proc_data, AKind::Int(0)),
		TKind::OParen => {
			cursor.advance();
			let expr = parse_expression(cursor, lex_data, proc_data, &[TKind::CParen])?;
			cursor.expect(lex_data, TKind::CParen)?;
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
	use crate::{ input, lexer };
	use crate::identifier::Identifier;
	use crate::parser::Value;

	use super::*;

	fn setup(source: &str) -> (DscData, IdentMap<ProcData>) {
		let input = input::eval("parser".to_string(), source.into());

		let lex_data = lexer::eval(&input.source)
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));

		crate::parser::eval(&input, &lex_data, false)
			.unwrap_or_else(|e| panic!("{}", e.display(&input)))
	}

	#[test]
	fn main() {
		let (_, proc_db) = setup("main{}");
		assert_eq!(proc_db[&"main".id()].ast_nodes, [
			AKind::Return(None),
			AKind::Block(AstBlock(vec![0.into()])),
		]);
	}

	#[test]
	fn values_basic() {
		let (dsc_data, _) = setup("value a = 2;");
		let a_id = "a".id();
		assert!(dsc_data.values.contains_key(&a_id));
		assert_eq!(dsc_data.values[&a_id], Value::Integer(2));
	}

	#[test]
	fn values_expressions() {
		let (dsc_data, _) = setup("value a = 2 * 5;");
		let a_id = "a".id();
		assert!(dsc_data.values.contains_key(&a_id));
		assert_eq!(dsc_data.values[&a_id], Value::Integer(10));
	}

	#[test]
	fn values_compound() {
		let (dsc_data, _) = setup("value a = 3; value b = a * 5;");
		let a_id = "a".id();
		let b_id = "b".id();
		assert!(dsc_data.values.contains_key(&a_id));
		assert!(dsc_data.values.contains_key(&b_id));
		assert_eq!(dsc_data.values[&a_id], Value::Integer(3));
		assert_eq!(dsc_data.values[&b_id], Value::Integer(15));
	}

	#[test]
	fn values_compound_inverted() {
		let (dsc_data, _) = setup("value b = a * 5; value a = 3;");
		let a_id = "a".id();
		let b_id = "b".id();
		assert!(dsc_data.values.contains_key(&a_id));
		assert!(dsc_data.values.contains_key(&b_id));
		assert_eq!(dsc_data.values[&a_id], Value::Integer(3));
		assert_eq!(dsc_data.values[&b_id], Value::Integer(15));
	}

	#[test]
	fn with_no_params() {
		let (_, proc_db) = setup("main{} proc a() {}");
		assert!(proc_db.contains_key(&"a".id()));
	}

	#[test]
	fn with_internal_expressions() {
		let (_, proc_db) = setup("main { let a: s8 = 2 + 3; }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_sub_expressions() {
		let (_, proc_db) = setup("main { let a: s8 = (2 + 3) * (4 - 5); }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_return() {
		let (_, proc_db) = setup("main{} proc a() -> s8 {}");
		assert!(proc_db.contains_key(&"a".id()));
	}

	#[test]
	fn with_single_param() {
		let (_, proc_db) = setup("main{} proc a(b:s8) {}");
		assert!(proc_db.contains_key(&"a".id()));
	}

	#[test]
	fn with_multi_params() {
		let (_, proc_db) = setup("main{} proc a(b:s8,c:s8) {}");
		assert!(proc_db.contains_key(&"a".id()));
	}

	#[cfg(feature="record")]
	#[test]
	fn with_record_param() {
		let (_, proc_db) = setup("main{} record a {} proc b(c:a) {}");
		assert!(proc_db.contains_key(&"b".id()));
	}

	#[cfg(feature="record")]
	#[test]
	fn with_out_of_order_record_param() {
		let (_, proc_db) = setup("main{} proc b(c:a) {} record a {}");
		assert!(proc_db.contains_key(&"b".id()));
	}

	#[cfg(feature="table")]
	#[test]
	fn with_table_param() {
		let (_, proc_db) = setup("main{} table a[0] {} proc b(c:a) {}");
		assert!(proc_db.contains_key(&"b".id()));
	}

	#[test]
	fn with_basic_for_loop() {
		let (_, proc_db) = setup("main { for i in [0..10] {} }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_multi_element_for_loop() {
		let (_, proc_db) = setup("main { for i,j in [0..10] {} }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="forloop")]
	#[test]
	fn with_table_for_loop() {
		let (_, proc_db) = setup("main { for i in a {} }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="forloop")]
	#[test]
	fn with_table_index_for_loop() {
		let (_, proc_db) = setup("main { for i in a[0..10] {} }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="forloop")]
	#[test]
	fn with_table_from_for_loop() {
		let (_, proc_db) = setup("main { for i in a[0..] {} }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="forloop")]
	#[test]
	fn with_table_to_for_loop() {
		let (_, proc_db) = setup("main { for i in a[..10] {} }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="forloop")]
	#[test]
	fn with_table_full_for_loop() {
		let (_, proc_db) = setup("main { for i in a[..] {} }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="index")]
	#[test]
	fn with_internal_table_index() {
		let (_, proc_db) = setup("main { return a[10].b; }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="index")]
	#[test]
	fn with_internal_table_expression_indexing() {
		let (_, proc_db) = setup("main { return a[2 + 4].b; }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_proc_call() {
		let (_, proc_db) = setup("main { return a(3); }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_proc_call_in_subexpression() {
		let (_, proc_db) = setup("main { return 3 * a(3); }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_proc_call_with_expression_argument() {
		let (_, proc_db) = setup("main { return a(b + 4); }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_proc_call_with_multiple_arguments() {
		let (_, proc_db) = setup("main { return a(2, 4 / b, b + 4); }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_while_loop() {
		let (_, proc_db) = setup("main { while true { } }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="access")]
	#[test]
	fn with_internal_field_assign() {
		let (_, proc_db) = setup("main { a.b = 2; }");
		assert!(proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="access")]
	#[test]
	fn with_internal_table_assign() {
		let (_, proc_db) = setup("main { a[3].b = 2; }");
		assert!(proc_db.contains_key(&"main".id()));
	}
}

