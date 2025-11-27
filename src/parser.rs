
use std::ops::Range;

use crate::ast::{ Block as AstBlock, Id as AstId, Kind as AKind, PathSegment };
use crate::cursor::Cursor;
use crate::error::{ self, CompilerError };
use crate::identifier::{Id as IdentId};
use crate::token::{ Id as TokenId, Kind as TKind };
use crate::{ BinaryOp, Bounds, Data, ProcData, UnaryOp };

type ParseResult = Result<AstId, CompilerError>;

pub fn eval(data: &mut Data) {
	while let Some(mut task) = data.task_queue.pop_front() {
		let err_len = data.errors.len();

		match parse(data, task.tok_start) {
			Err(token_id) => {
				// We didn't finish. Check if we made progress.
				if token_id > task.prev_furthest_token {
					data.errors.truncate(err_len);

					// We made progress, so we're not stuck yet. Re-queue and try again.
					task.prev_furthest_token = token_id;
					task.prev_ready_proc_count = data.proc_db.len();
					data.task_queue.push_back(task);
				} else if data.proc_db.len() > task.prev_ready_proc_count {
					data.errors.truncate(err_len);

					// Someone else made progress, so maybe a different dependency will finish. Re-queue and
					// try again.
					task.prev_ready_proc_count = data.proc_db.len();
					data.task_queue.push_back(task);
				} else {
					data.task_queue.push_back(task);
					return;
				}
			}

			Ok(proc_data) => {
				// We finished. Add us to the 'done' list, so dependent procedures can progress.
				data.proc_db.insert(task.name_id(), proc_data);
				//data.variable_tables.insert(task.proc_name, var_table);
			}
		}
	}
}

fn parse(data: &mut Data, start_token: TokenId) -> Result<ProcData, TokenId> {
	let mut cursor = Cursor::new(start_token);
	let cursor = &mut cursor;
	let mut proc_data = ProcData::default();
	let start = AstId::new(proc_data.ast_nodes.len());

	// skip to the procedure body
	while TKind::OBrace != cursor.peek(data, 0) {
		cursor.advance();
	}

	let tok_start = cursor.index();
	let mut block = parse_block(cursor, data, &mut proc_data)
		.map_err(|mut e| {
			e.set_kind(error::Kind::Parser);
			data.errors.push(e);
			cursor.index()
		})?;
	let tok_end = cursor.index();

	let end = AstId::new(proc_data.ast_nodes.len());

	let has_return = proc_data.ast_nodes[start..end]
		.iter()
		.any(|kind| matches!(kind, AKind::Return(_)));

	if !has_return {
		let tok_pos = cursor.index();
		let ast_id = new_ast(&mut proc_data, AKind::Return(None),
			tok_pos..tok_pos,
		);
		block.0.push(ast_id);
	}

	proc_data.ast_start = new_ast(&mut proc_data, AKind::Block(block),
		tok_start..tok_end,
	);
	Ok(proc_data)
}

fn new_ast(proc_data: &mut ProcData, kind: AKind,
	tok_range: Range<TokenId>,
) -> AstId {
	proc_data.ast_nodes.push(kind);
	proc_data.ast_pos_tok.push(tok_range);
	AstId::new(proc_data.ast_nodes.len() - 1)
}

fn parse_block(cursor: &mut Cursor, data: &mut Data, proc_data: &mut ProcData,
) -> Result<AstBlock, CompilerError> {
	cursor.expect(data, TKind::OBrace)?;

	let mut block = vec![];
	while ![TKind::Eof, TKind::CBrace].contains(&cursor.current(data)) {
		block.push(match cursor.current(data) {
			TKind::Let => parse_let_statement(cursor, data, proc_data)?,
			TKind::Identifier(ident_id) => parse_ident_statement(cursor, data, proc_data, ident_id)?,
			TKind::OBrace => {
				let tok_start = cursor.index();
				let b = parse_block(cursor, data, proc_data)?;
				let tok_range = tok_start..cursor.index();
				new_ast(proc_data, AKind::Block(b), tok_range)
			}
			TKind::Return => parse_return_statement(cursor, data, proc_data)?,
			TKind::If => parse_if_statement(cursor, data, proc_data)?,
			TKind::For => parse_for_statement(cursor, data, proc_data)?,
			TKind::While => parse_while_statement(cursor, data, proc_data)?,
			_ => return Err(error::expected_token(data,
					"definition, assignment, return, if, or for statement",
			cursor.index())),
		});
	}

	cursor.expect(data, TKind::CBrace)?;

	Ok(AstBlock(block))
}

fn parse_ident_statement(cursor: &mut Cursor, data: &mut Data, proc_data: &mut ProcData,
	ident_id: IdentId,
) -> ParseResult {
	let left_id = parse_access(cursor, data, proc_data, ident_id)?;

	match cursor.current(data) {
		TKind::Eq => parse_assignment(cursor, data, proc_data, left_id),
		TKind::PlusEq => parse_op_assignment(cursor, data, proc_data, left_id, BinaryOp::Add),
		TKind::DashEq => parse_op_assignment(cursor, data, proc_data, left_id, BinaryOp::Sub),
		TKind::StarEq => parse_op_assignment(cursor, data, proc_data, left_id, BinaryOp::Mul),
		TKind::SlashEq => parse_op_assignment(cursor, data, proc_data, left_id, BinaryOp::Div),
		_ => Err(error::expected_token(data,
			"definition or assignment statement",
			cursor.index())),
	}
}

fn parse_let_statement(cursor: &mut Cursor, data: &mut Data, proc_data: &mut ProcData,
) -> ParseResult {
	let tok_start = cursor.index();
	cursor.expect(data, TKind::Let)?;
	let tok_ident_start = cursor.index();
	let ident_id = cursor.expect_identifier(data, "variable identifier")?;
	let tok_ident_range = tok_ident_start..cursor.index();
	cursor.expect(data, TKind::Colon)?;
	let var_type = cursor.expect_type(data)?;
	cursor.expect(data, TKind::Eq)?;
	let ast_id = parse_expression(cursor, data, proc_data, &[TKind::Semicolon])?;
	cursor.expect(data, TKind::Semicolon)?;
	let tok_range = tok_start..cursor.index();
	let ident = new_ast(proc_data, AKind::Ident(ident_id), tok_ident_range);
	Ok(new_ast(proc_data, AKind::Define(ident, var_type, ast_id), tok_range))
}

fn parse_access(cursor: &mut Cursor, data: &mut Data, proc_data: &mut ProcData,
	ident_id: IdentId,
) -> ParseResult {
	let tok_start = cursor.index();
	cursor.advance(); // skip the identifier
	let mut accesses = vec![];
	while [TKind::Dot, TKind::OBracket].contains(&cursor.current(data)) {
		match cursor.current(data) {
			// a.b -> location of 'a' + offset of 'b'
			TKind::Dot => {
				cursor.expect(data, TKind::Dot)?;
				let id = cursor.expect_identifier(data, "field name")?;
				accesses.push(PathSegment::Field(id));
			}
			// a[b].c -> location of 'a' + 'b' * size of 'c'
			TKind::OBracket => {
				cursor.expect(data, TKind::OBracket)?;
				let index = parse_expression(cursor, data, proc_data, &[TKind::CBracket])?;
				cursor.expect(data, TKind::CBracket)?;
				cursor.expect(data, TKind::Dot)?;
				let id = cursor.expect_identifier(data, "field name")?;
				accesses.push(PathSegment::Index(index, id));
			}
			_ => break,
		}
	}
	let tok_range = tok_start..cursor.index();
	let kind = if accesses.is_empty() {
		AKind::Ident(ident_id)
	} else {
		#[cfg(feature="ready")]
		AKind::Access(ident_id, accesses);
		todo!()
	};
	Ok(new_ast(proc_data, kind, tok_range))
}

fn parse_assignment(cursor: &mut Cursor, data: &mut Data, proc_data: &mut ProcData,
	lvalue_id: AstId,
) -> ParseResult {
	let tok_start = cursor.index();
	cursor.expect(data, TKind::Eq)?;
	let ast_id = parse_expression(cursor, data, proc_data, &[TKind::Semicolon])?;
	cursor.expect(data, TKind::Semicolon)?;
	let tok_range = tok_start..cursor.index();
	Ok(new_ast(proc_data, AKind::Assign(lvalue_id, ast_id), tok_range))
}

fn parse_op_assignment(cursor: &mut Cursor, data: &mut Data, proc_data: &mut ProcData,
	lvalue_id: AstId, op: BinaryOp,
) -> ParseResult {
	let tok_start = cursor.index();
	cursor.advance(); // skip the operator token
	let ast_id = parse_expression(cursor, data, proc_data, &[TKind::Semicolon])?;
	cursor.expect(data, TKind::Semicolon)?;
	let tok_range = tok_start..cursor.index();
	let op_id = new_ast(proc_data, AKind::BinOp(op, lvalue_id, ast_id),
		tok_range.clone());
	Ok(new_ast(proc_data, AKind::Assign(lvalue_id, op_id), tok_range))
}

fn parse_return_statement(cursor: &mut Cursor, data: &mut Data, proc_data: &mut ProcData,
) -> ParseResult {
	let tok_start = cursor.index();
	cursor.expect(data, TKind::Return)?;
	let ast_id = match cursor.expect(data, TKind::Semicolon) {
		Ok(_) => None,
		Err(_) => {
			let result = parse_expression(cursor, data, proc_data, &[TKind::Semicolon])?;
			cursor.expect(data, TKind::Semicolon)?;
			Some(result)
		},
	};
	let tok_range = tok_start..cursor.index();
	Ok(new_ast(proc_data, AKind::Return(ast_id), tok_range))
}

fn parse_if_statement(cursor: &mut Cursor, data: &mut Data, proc_data: &mut ProcData,
) -> ParseResult {
	let tok_start = cursor.index();
	cursor.expect(data, TKind::If)?;
	let cond_id = parse_expression(cursor, data, proc_data, &[TKind::OBrace])?;
	let then_block = parse_block(cursor, data, proc_data)?;
	let else_block = if TKind::Else == cursor.current(data) {
		cursor.advance();
		parse_block(cursor, data, proc_data)?
	} else {
		AstBlock(vec![])
	};
	let tok_range = tok_start..cursor.index();
	Ok(new_ast(proc_data, AKind::If(cond_id, then_block, else_block), tok_range))
}

fn parse_while_statement(cursor: &mut Cursor, data: &mut Data, proc_data: &mut ProcData,
) -> ParseResult {
	let tok_start = cursor.index();
	cursor.expect(data, TKind::While)?;
	let cond = parse_expression(cursor, data, proc_data, &[TKind::OBrace])?;
	let block = parse_block(cursor, data, proc_data)?;
	let tok_range = tok_start..cursor.index();
	Ok(new_ast(proc_data, AKind::While(cond, block), tok_range))
}

fn parse_for_statement(cursor: &mut Cursor, data: &mut Data, proc_data: &mut ProcData,
) -> ParseResult {
	let tok_start = cursor.index();
	cursor.expect(data, TKind::For)?;

	let mut vars = vec![];
	let ident_id = cursor.expect_identifier(data, "identifier")?;
	vars.push(ident_id);

	while TKind::Comma == cursor.current(data) {
		cursor.advance();
		let ident_id = cursor.expect_identifier(data, "identifier")?;
		vars.push(ident_id);
	}

	while TKind::In != cursor.current(data) {
		let ident_id = cursor.expect_identifier(data, "identifier")?;
		vars.push(ident_id);
		if TKind::In == cursor.current(data) {
			break;
		}
		cursor.expect(data, TKind::Comma)?;
	}

	cursor.expect(data, TKind::In)?;

	// for x in Table {}
	// for x in Table[..] {}
	// for x in [0..10] {}
	let (table_id, range) = if let Ok(table_id) = cursor.expect_identifier(data, "COMPILER ERROR") {
		// Table loop
		let range = if TKind::OBracket == cursor.current(data) {
			cursor.advance();
			let range_start = cursor.expect_u32(data, "COMPILER ERROR").ok();
			cursor.expect(data, TKind::Dot2)?;
			let range_end = cursor.expect_u32(data, "COMPILER ERROR").ok();
			cursor.expect(data, TKind::CBracket)?;
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
	} else if TKind::OBracket == cursor.current(data) {
		cursor.advance();
		let start = cursor.expect_u32(data, "start (inclusive) value")?;
		cursor.expect(data, TKind::Dot2)?;
		let end = cursor.expect_u32(data, "end (exclusive) value")?;
		cursor.expect(data, TKind::CBracket)?;
		(None, Some(Bounds::Full { start, end }))
	} else {
		return Err(error::expected_token(data,
			"table name or bracketed range",
			cursor.index()));
	};

	let block = parse_block(cursor, data, proc_data)?;

	let tok_range = tok_start..cursor.index();
	Ok(new_ast(proc_data, AKind::For(vars, table_id, range, block), tok_range))
}

fn parse_call(cursor: &mut Cursor, data: &mut Data, proc_data: &mut ProcData,
	ident_id: IdentId,
) -> ParseResult {
	let tok_start = cursor.index();
	cursor.advance();
	cursor.expect(data, TKind::OParen)?;
	let end_tokens = &[TKind::CParen, TKind::Comma];

	let mut exprs = vec![];
	while TKind::CParen != cursor.current(data) {
		let expr = parse_expression(cursor, data, proc_data, end_tokens)?;
		exprs.push(expr);

		if TKind::Comma == cursor.current(data) {
			cursor.advance();
			if TKind::CParen == cursor.current(data) {
				break;
			}
		} else if TKind::CParen != cursor.current(data) {
			break;
		}
	}

	cursor.expect(data, TKind::CParen)?;
	let tok_range = tok_start..cursor.index();
	Ok(new_ast(proc_data, AKind::Call(ident_id, exprs), tok_range))
}

fn parse_expression(cursor: &mut Cursor, data: &mut Data, proc_data: &mut ProcData,
	end_tokens: &[TKind],
) -> ParseResult {
	parse_expr_main(cursor, data, proc_data, 0, end_tokens)
}

fn parse_expr_main(cursor: &mut Cursor, data: &mut Data, proc_data: &mut ProcData,
	min_binding_power: usize, end_tokens: &[TKind],
) -> ParseResult {
	let tok_start = cursor.index();
	let left = parse_primary(cursor, data, proc_data)?;
	parse_expr_sub(cursor, data, proc_data, min_binding_power, tok_start, left, end_tokens)
}

fn parse_expr_sub(cursor: &mut Cursor, data: &mut Data, proc_data: &mut ProcData,
	min_binding_power: usize, tok_start: TokenId, left: AstId,
	end_tokens: &[TKind],
) -> ParseResult {
	if end_tokens.contains(&cursor.current(data)) {
		return Ok(left);
	}
	let Ok(op) = parse_bin_op(cursor, data) else {
		return Ok(left);
	};
	let op_binding_power = binding_power(&op);
	let tok_start_inner = cursor.index();
	let right = parse_primary(cursor, data, proc_data)?;
	let right = if min_binding_power <= op_binding_power {
		parse_expr_sub(cursor, data, proc_data, op_binding_power,
			tok_start_inner, right, end_tokens)?
	} else {
		right
	};

	let tok_range = tok_start..cursor.index();
	Ok(new_ast(proc_data, AKind::BinOp(op, left, right), tok_range))
}

fn parse_primary(cursor: &mut Cursor, data: &mut Data, proc_data: &mut ProcData,
) -> ParseResult {
	let tok_start_op = cursor.index();
	let unary_op = match cursor.current(data) {
		TKind::Dash => { cursor.advance(); Some(UnaryOp::Neg) }
		TKind::Bang => { cursor.advance(); Some(UnaryOp::Not) }
		_ => None,
	};
	let tok_end_op = cursor.index();

	fn primary_node(cursor: &mut Cursor, proc_data: &mut ProcData, kind: AKind) -> AstId {
		let tok_start_expr = cursor.index();
		cursor.advance();
		let tok_range = tok_start_expr..cursor.index();
		new_ast(proc_data, kind, tok_range)
	}

	let node = match cursor.current(data) {
		TKind::Identifier(ident_id) => {
			match cursor.peek(data, 1) {
				TKind::OParen => parse_call(cursor, data, proc_data, ident_id)?,
				TKind::Dot | TKind::OBracket => parse_access(cursor, data, proc_data, ident_id)?,
				_ => primary_node(cursor, proc_data, AKind::Ident(ident_id)),
			}
		}
		TKind::Integer(num) => primary_node(cursor, proc_data, AKind::Int(num)),
		TKind::Decimal(num) => primary_node(cursor, proc_data, AKind::Dec(num)),
		TKind::True => primary_node(cursor, proc_data, AKind::Int(1)),
		TKind::False => primary_node(cursor, proc_data, AKind::Int(0)),
		TKind::OParen => {
			cursor.advance();
			let expr = parse_expression(cursor, data, proc_data, &[TKind::CParen])?;
			cursor.expect(data, TKind::CParen)?;
			expr
		}
		_ => return Err(error::expected_token(data,
			"identifier or number",
			cursor.index())),
	};

	if let Some(op) = unary_op {
		let tok_range = tok_start_op..tok_end_op;
		Ok(new_ast(proc_data, AKind::UnOp(op, node), tok_range))
	} else {
		Ok(node)
	}
}

fn parse_bin_op(cursor: &mut Cursor, data: &mut Data) -> Result<BinaryOp, CompilerError> {
	let op = match cursor.current(data) {
		TKind::Amp      => BinaryOp::BinAnd,
		TKind::Amp2     => BinaryOp::LogAnd,
		TKind::BangEq   => BinaryOp::CmpNE,
		TKind::Bar      => BinaryOp::BinOr,
		TKind::Bar2     => BinaryOp::LogOr,
		TKind::Carrot   => BinaryOp::BinXor,
		TKind::Carrot2  => BinaryOp::LogXor,
		TKind::Dash     => BinaryOp::Sub,
		TKind::Dot      => BinaryOp::Access,
		TKind::Eq2      => BinaryOp::CmpEQ,
		TKind::LArr     => BinaryOp::CmpLT,
		TKind::LArr2    => BinaryOp::ShL,
		TKind::LArrEq   => BinaryOp::CmpLE,
		TKind::OBracket => BinaryOp::Index,
		TKind::OParen   => BinaryOp::Call,
		TKind::Percent  => BinaryOp::Mod,
		TKind::Plus     => BinaryOp::Add,
		TKind::RArr     => BinaryOp::CmpGT,
		TKind::RArr2    => BinaryOp::ShR,
		TKind::RArrEq   => BinaryOp::CmpGE,
		TKind::Slash    => BinaryOp::Div,
		TKind::Star     => BinaryOp::Mul,
		_ => return Err(error::expected_token(data, "binary operator", cursor.index())),
	};
	cursor.advance();
	Ok(op)
}

fn binding_power(op: &BinaryOp) -> usize {
	match op {
		BinaryOp::Call   | BinaryOp::Index => 10,
		BinaryOp::Add    | BinaryOp::Sub   => 20,
		BinaryOp::Mul    | BinaryOp::Div   | BinaryOp::Mod    => 30,
		BinaryOp::ShL    | BinaryOp::ShR   => 40,
		BinaryOp::LogAnd | BinaryOp::LogOr | BinaryOp::LogXor => 50,
		BinaryOp::BinAnd | BinaryOp::BinOr | BinaryOp::BinXor => 60,
		BinaryOp::CmpEQ  | BinaryOp::CmpNE |
		BinaryOp::CmpGE  | BinaryOp::CmpGT |
		BinaryOp::CmpLE  | BinaryOp::CmpLT => 70,
		BinaryOp::Access => 80,
	}
}

#[cfg(test)]
mod can_parse_proc {
	use crate::{ lexer, discovery, ast };
	use crate::identifier::Identifier;

	fn setup(source: &str) -> crate::Data {
		let source_file = "parser".to_string();
		let mut db = crate::Data::new(source_file, source.into());
		db.DEBUG_show_tokens = true;
		lexer::eval(&mut db);
		discovery::eval(&mut db);
		super::eval(&mut db);
		db
	}

	#[test]
	fn main() {
		let db = setup("main{}");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert_eq!(db.proc_db[&"main".id()].ast_nodes, [
			ast::Kind::Return(None),
			ast::Kind::Block(ast::Block(vec![0.into()])),
		]);
	}

	#[test]
	fn with_no_params() {
		let db = setup("main{} proc a() {}");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"a".id()));
	}

	#[test]
	fn with_internal_expressions() {
		let db = setup("main { let a: s8 = 2 + 3; }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_sub_expressions() {
		let db = setup("main { let a: s8 = (2 + 3) * (4 - 5); }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_return() {
		let db = setup("main{} proc a() -> s8 {}");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"a".id()));
	}

	#[test]
	fn with_single_param() {
		let db = setup("main{} proc a(b:s8) {}");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"a".id()));
	}

	#[test]
	fn with_multi_params() {
		let db = setup("main{} proc a(b:s8,c:s8) {}");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"a".id()));
	}

	#[cfg(feature="ready")]
	#[test]
	fn with_record_param() {
		let db = setup("main{} record a {} proc b(c:a) {}");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"b".id()));
	}

	// TODO - srenshaw - Check this once we have out-of-order type resolution.
	// #[test]
	// fn with_out_of_order_record_param() {
	// 	let db = setup("main{} proc b(c:a) {} record a {}");
	// 	assert!(db.errors.is_empty(), "{}", db.errors_to_string());
	// 	assert!(db.proc_db.contains_key(&"b".id()));
	// }

	#[cfg(feature="ready")]
	#[test]
	fn with_table_param() {
		let db = setup("main{} table a[0] {} proc b(c:a) {}");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"b".id()));
	}

	#[test]
	fn with_basic_for_loop() {
		let db = setup("main { for i in [0..10] {} }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_multi_element_for_loop() {
		let db = setup("main { for i,j in [0..10] {} }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="ready")]
	#[test]
	fn with_table_for_loop() {
		let db = setup("main { for i in a {} }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="ready")]
	#[test]
	fn with_table_index_for_loop() {
		let db = setup("main { for i in a[0..10] {} }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="ready")]
	#[test]
	fn with_table_from_for_loop() {
		let db = setup("main { for i in a[0..] {} }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="ready")]
	#[test]
	fn with_table_to_for_loop() {
		let db = setup("main { for i in a[..10] {} }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="ready")]
	#[test]
	fn with_table_full_for_loop() {
		let db = setup("main { for i in a[..] {} }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="ready")]
	#[test]
	fn with_internal_table_index() {
		let db = setup("main { return a[10].b; }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="ready")]
	#[test]
	fn with_internal_table_expression_indexing() {
		let db = setup("main { return a[2 + 4].b; }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_proc_call() {
		let db = setup("main { return a(3); }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_proc_call_in_subexpression() {
		let db = setup("main { return 3 * a(3); }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_proc_call_with_expression_argument() {
		let db = setup("main { return a(b + 4); }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_proc_call_with_multiple_arguments() {
		let db = setup("main { return a(2, 4 / b, b + 4); }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_while_loop() {
		let db = setup("main { while true { } }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="ready")]
	#[test]
	fn with_internal_field_assign() {
		let db = setup("main { a.b = 2; }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}

	#[cfg(feature="ready")]
	#[test]
	fn with_internal_table_assign() {
		let db = setup("main { a[3].b = 2; }");
		assert!(db.errors.is_empty(), "{}", db.errors_to_string());
		assert!(db.proc_db.contains_key(&"main".id()));
	}
}

