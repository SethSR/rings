
use std::ops::Range;

use crate::ast;
use crate::cursor::Cursor;
use crate::error::{self, CompilerError};
use crate::identifier;
use crate::token;
use crate::{BinaryOp, Data, Bounds, SrcPos, UnaryOp};

use ast::Kind as AKind;
use token::Kind as TKind;

#[derive(Debug)]
pub struct Task {
	pub proc_name: identifier::Id,
	pub start_token: token::Id,
	pub prev_furthest_token: token::Id,
	pub prev_ready_proc_count: usize,
}

pub fn eval(data: &mut Data) {
	data.proc_queue = data.procedures.iter()
		.map(|(&proc_name, proc)| Task {
			proc_name,
			start_token: proc.tok_start,
			prev_furthest_token: token::Id::default(),
			prev_ready_proc_count: 0,
		})
		.collect::<std::collections::VecDeque<_>>();

	while let Some(mut task) = data.proc_queue.pop_front() {
		let err_len = data.errors.len();

		match parse(data, &mut task) {
			Err(token_id) => {
				// We didn't finish. Check if we made progress.
				if token_id > task.prev_furthest_token {
					data.errors.truncate(err_len);

					// We made progress, so we're not stuck yet. Re-queue and try again.
					task.prev_furthest_token = token_id;
					task.prev_ready_proc_count = data.completed_procs.len();
					data.proc_queue.push_back(task);
				} else if data.completed_procs.len() > task.prev_ready_proc_count {
					data.errors.truncate(err_len);

					// Someone else made progress, so maybe a different dependency will finish. Re-queue and
					// try again.
					task.prev_ready_proc_count = data.completed_procs.len();
					data.proc_queue.push_back(task);
				} else {
					let name = data.text(task.proc_name).to_string();
					let mut err = error::error_with_notes(data,
						&format!("No progress made since last attempt to parse '{name}'"),
						task.start_token,
						&[("reached this point before failing", task.prev_furthest_token)]
					);
					err.set_kind(error::Kind::Parser);
					data.errors.push(err);
					data.proc_queue.push_back(task);
					return;
				}
			}

			Ok(proc_start) => {
				// We finished. Add us to the 'done' list, so dependent procedures can progress.
				data.completed_procs.insert(task.proc_name, proc_start);
			}
		}
	}
}

fn parse(data: &mut Data, task: &mut Task) -> Result<ast::Id, token::Id> {
	let mut cursor = Cursor::new(task.start_token);
	let cursor = &mut cursor;
	let start = ast::Id::new(data.ast_nodes.len());

	// skip to the procedure body
	while TKind::OBrace != cursor.peek(data, 0) {
		cursor.advance();
	}

	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	let mut block = parse_block(cursor, data)
		.map_err(|mut e| {
			e.set_kind(error::Kind::Parser);
			data.errors.push(e);
			cursor.index()
		})?;
	let src_end = cursor.location(data);
	let tok_end = cursor.index();

	let end = ast::Id::new(data.ast_nodes.len());

	let has_return = data.ast_nodes[start..end]
		.iter()
		.any(|kind| matches!(kind, AKind::Return(_)));

	if !has_return {
		let src_pos = cursor.location(data);
		let tok_pos = cursor.index();
		let ast_id = new_ast(data, AKind::Return(None),
			src_pos..src_pos,
			tok_pos..tok_pos,
		);
		block.0.push(ast_id);
	}

	Ok(new_ast(data, AKind::Block(block),
		src_start..src_end,
		tok_start..tok_end,
	))
}

fn new_ast(data: &mut Data, kind: AKind,
	src_range: Range<SrcPos>,
	tok_range: Range<token::Id>,
) -> ast::Id {
	data.ast_nodes.push(kind);
	data.ast_pos_src.push(src_range);
	data.ast_pos_tok.push(tok_range);
	ast::Id::new(data.ast_nodes.len() - 1)
}

fn parse_block(cursor: &mut Cursor, data: &mut Data,
) -> Result<ast::Block, CompilerError> {
	cursor.expect(data, TKind::OBrace)?;

	let mut block = vec![];
	while ![TKind::Eof, TKind::CBrace].contains(&cursor.current(data)) {
		block.push(match cursor.current(data) {
			TKind::Identifier(ident_id) => parse_ident_statement(cursor, data, ident_id)?,
			TKind::OBrace => {
				let src_start = cursor.location(data);
				let tok_start = cursor.index();
				let b = parse_block(cursor, data)?;
				let src_range = src_start..cursor.location(data);
				let tok_range = tok_start..cursor.index();
				new_ast(data, AKind::Block(b), src_range, tok_range)
			}
			TKind::Return => parse_return_statement(cursor, data)?,
			TKind::If => parse_if_statement(cursor, data)?,
			TKind::For => parse_for_statement(cursor, data)?,
			_ => return Err(error::expected_token(data,
					"definition, assignment, return, if, or for statement",
			cursor.index())),
		});
	}

	Ok(ast::Block(block))
}

fn parse_ident_statement(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id,
) -> Result<ast::Id, CompilerError> {
	match cursor.peek(data, 1) {
		TKind::Colon => parse_definition(cursor, data, ident_id),
		TKind::Equal => parse_assignment(cursor, data, ident_id),
		TKind::ColonEqual => Err(error::error(data,
			"type-inference is not implemented yet, please add a type-specifier",
			cursor.index())),
		TKind::PlusEqual => parse_op_assignment(cursor, data, ident_id, BinaryOp::Add),
		TKind::DashEqual => parse_op_assignment(cursor, data, ident_id, BinaryOp::Sub),
		TKind::StarEqual => parse_op_assignment(cursor, data, ident_id, BinaryOp::Mul),
		TKind::SlashEqual => parse_op_assignment(cursor, data, ident_id, BinaryOp::Div),
		_ => Err(error::expected_token(data,
			"definition or assignment statement",
			cursor.index())),
	}
}

fn parse_definition(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id,
) -> Result<ast::Id, CompilerError> {
	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	cursor.advance();
	cursor.advance();
	let var_type = cursor.expect_type(data)?;
	cursor.expect(data, TKind::Equal)?;
	let ast_id = parse_expression(cursor, data, TKind::Semicolon)?;
	cursor.expect(data, TKind::Semicolon)?;
	let src_range = src_start..cursor.location(data);
	let tok_range = tok_start..cursor.index();
	Ok(new_ast(data, AKind::Define(ident_id, var_type, ast_id), src_range, tok_range))
}

fn parse_assignment(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id,
) -> Result<ast::Id, CompilerError> {
	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	cursor.advance();
	cursor.advance();
	let ast_id = parse_expression(cursor, data, TKind::Semicolon)?;
	cursor.expect(data, TKind::Semicolon)?;
	let src_range = src_start..cursor.location(data);
	let tok_range = tok_start..cursor.index();
	Ok(new_ast(data, AKind::Assign(ident_id, ast_id), src_range, tok_range))
}

fn parse_op_assignment(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id, op: BinaryOp,
) -> Result<ast::Id, CompilerError> {
	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	cursor.advance();
	cursor.advance();
	let var_id = new_ast(data, AKind::Ident(ident_id),
		src_start..cursor.location(data),
		tok_start..cursor.index(),
	);
	let ast_id = parse_expression(cursor, data, TKind::Semicolon)?;
	cursor.expect(data, TKind::Semicolon)?;
	let src_range = src_start..cursor.location(data);
	let tok_range = tok_start..cursor.index();
	let op_id = new_ast(data, AKind::BinOp(op, var_id, ast_id),
		src_range.clone(),
		tok_range.clone());
	Ok(new_ast(data, AKind::Assign(ident_id, op_id), src_range, tok_range))
}

fn parse_return_statement(cursor: &mut Cursor, data: &mut Data) -> Result<ast::Id, CompilerError> {
	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	cursor.expect(data, TKind::Return)?;
	let ast_id = if TKind::Semicolon != cursor.current(data) {
		let id = parse_expression(cursor, data, TKind::Semicolon)?;
		cursor.expect(data, TKind::Semicolon)?;
		Some(id)
	} else {
		None
	};
	let src_range = src_start..cursor.location(data);
	let tok_range = tok_start..cursor.index();
	Ok(new_ast(data, AKind::Return(ast_id), src_range, tok_range))
}

fn parse_if_statement(cursor: &mut Cursor, data: &mut Data) -> Result<ast::Id, CompilerError> {
	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	cursor.expect(data, TKind::If)?;
	let cond_id = parse_expression(cursor, data, TKind::Semicolon)?;
	cursor.expect(data, TKind::Semicolon)?;
	let then_block = parse_block(cursor, data)?;
	let else_block = if TKind::Else == cursor.current(data) {
		cursor.advance();
		parse_block(cursor, data)?
	} else {
		ast::Block(vec![])
	};
	let src_range = src_start..cursor.location(data);
	let tok_range = tok_start..cursor.index();
	Ok(new_ast(data, AKind::If(cond_id, then_block, else_block), src_range, tok_range))
}

fn parse_for_statement(cursor: &mut Cursor, data: &mut Data) -> Result<ast::Id, CompilerError> {
	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	cursor.expect(data, TKind::For)?;

	let mut vars = vec![];
	while TKind::In != cursor.current(data) {
		let ident_id = cursor.expect_identifier(data, "identifier")?;
		cursor.advance();
		vars.push(ident_id);
		if TKind::In == cursor.current(data) {
			break;
		}
		cursor.expect(data, TKind::Comma)?;
	}

	cursor.expect(data, TKind::In)?;

	let table_id = cursor.expect_identifier(data, "COMPILER ERROR").ok();

	let range = if TKind::OBracket == cursor.current(data) {
		cursor.advance();
		let range_start = cursor.expect_integer(data, "COMPILER ERROR").ok();
		cursor.expect(data, TKind::DotDot)?;
		let range_end = cursor.expect_integer(data, "COMPILER ERROR").ok();
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

	let block = parse_block(cursor, data)?;

	let src_range = src_start..cursor.location(data);
	let tok_range = tok_start..cursor.index();
	Ok(new_ast(data, AKind::For(vars, table_id, range, block), src_range, tok_range))
}

fn parse_expression(cursor: &mut Cursor, data: &mut Data,
	end_token: TKind,
) -> Result<ast::Id, CompilerError> {
	parse_expr_main(cursor, data, 0, end_token)
}

fn parse_expr_main(cursor: &mut Cursor, data: &mut Data,
	min_binding_power: usize, end_token: TKind,
) -> Result<ast::Id, CompilerError> {
	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	let left = parse_primary(cursor, data)?;
	parse_expr_sub(cursor, data, min_binding_power, src_start, tok_start, left, end_token)
}

fn parse_expr_sub(cursor: &mut Cursor, data: &mut Data,
	min_binding_power: usize, src_start: usize, tok_start: token::Id, left: ast::Id,
	end_token: TKind,
) -> Result<ast::Id, CompilerError> {
	if end_token == cursor.current(data) {
		return Ok(left);
	}
	let op = parse_bin_op(cursor, data)?;
	let op_binding_power = binding_power(&op);
	cursor.advance();
	let src_start_inner = cursor.location(data);
	let tok_start_inner = cursor.index();
	let right = parse_primary(cursor, data)?;
	let right = if min_binding_power < op_binding_power {
		parse_expr_sub(cursor, data, op_binding_power,
			src_start_inner, tok_start_inner, right, end_token)?
	} else {
		right
	};

	let src_range = src_start..cursor.location(data);
	let tok_range = tok_start..cursor.index();
	Ok(new_ast(data, AKind::BinOp(op, left, right), src_range, tok_range))
}

fn parse_primary(cursor: &mut Cursor, data: &mut Data) -> Result<ast::Id, CompilerError> {
	let src_start_op = cursor.location(data);
	let tok_start_op = cursor.index();
	let unary_op = match cursor.current(data) {
		TKind::Dash => { cursor.advance(); Some(UnaryOp::Neg) }
		TKind::Bang => { cursor.advance(); Some(UnaryOp::Not) }
		_ => None,
	};
	let src_end_op = cursor.location(data);
	let tok_end_op = cursor.index();

	fn primary_node(cursor: &mut Cursor, data: &mut Data, kind: AKind) -> ast::Id {
		let src_start_expr = cursor.location(data);
		let tok_start_expr = cursor.index();
		cursor.advance();
		let src_range = src_start_expr..cursor.location(data);
		let tok_range = tok_start_expr..cursor.index();
		new_ast(data, kind, src_range, tok_range)
	}

	let node = match cursor.current(data) {
		TKind::Identifier(ident_id) => primary_node(cursor, data, AKind::Ident(ident_id)),
		TKind::Integer(num) => primary_node(cursor, data, AKind::Int(num)),
		TKind::Decimal(num) => primary_node(cursor, data, AKind::Dec(num)),
		TKind::OParen => {
			cursor.advance();
			let expr = parse_expression(cursor, data, TKind::CParen)?;
			cursor.expect(data, TKind::CParen)?;
			expr
		}
		_ => return Err(error::expected_token(data,
			"identifier or number",
			cursor.index())),
	};

	if let Some(op) = unary_op {
		let src_range = src_start_op..src_end_op;
		let tok_range = tok_start_op..tok_end_op;
		Ok(new_ast(data, AKind::UnOp(op, node), src_range, tok_range))
	} else {
		Ok(node)
	}
}

fn parse_bin_op(cursor: &mut Cursor, data: &mut Data) -> Result<BinaryOp, CompilerError> {
	match cursor.current(data) {
		TKind::Plus => Ok(BinaryOp::Add),
		TKind::Dash => Ok(BinaryOp::Sub),
		TKind::Star => Ok(BinaryOp::Mul),
		TKind::Slash => Ok(BinaryOp::Div),
		TKind::Percent => Ok(BinaryOp::Mod),
		TKind::Amp => Ok(BinaryOp::BinAnd),
		TKind::AmpAmp => Ok(BinaryOp::LogAnd),
		TKind::Bar => Ok(BinaryOp::BinOr),
		TKind::BarBar => Ok(BinaryOp::LogOr),
		TKind::Carrot => Ok(BinaryOp::BinXor),
		TKind::CarrotCarrot => Ok(BinaryOp::LogXor),
		TKind::EqualEqual => Ok(BinaryOp::CmpEQ),
		TKind::BangEqual => Ok(BinaryOp::CmpNE),
		TKind::Less => Ok(BinaryOp::CmpLT),
		TKind::LessEqual => Ok(BinaryOp::CmpLE),
		TKind::LessLess => Ok(BinaryOp::ShL),
		TKind::Greater => Ok(BinaryOp::CmpGT),
		TKind::GreaterEqual => Ok(BinaryOp::CmpGE),
		TKind::GreaterGreater => Ok(BinaryOp::ShR),
		TKind::Dot => Ok(BinaryOp::Access),
		_ => Err(error::expected_token(data, "binary operator", cursor.index())),
	}
}

fn binding_power(op: &BinaryOp) -> usize {
	match op {
		BinaryOp::Add | BinaryOp::Sub => 10,
		BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 20,
		BinaryOp::ShL | BinaryOp::ShR => 30,
		BinaryOp::LogAnd | BinaryOp::LogOr | BinaryOp::LogXor => 40,
		BinaryOp::BinAnd | BinaryOp::BinOr | BinaryOp::BinXor => 50,
		BinaryOp::CmpEQ | BinaryOp::CmpNE |
		BinaryOp::CmpGE | BinaryOp::CmpGT |
		BinaryOp::CmpLE | BinaryOp::CmpLT => 60,
		BinaryOp::Access => 70,
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
		assert!(db.errors.is_empty(), "{}", db.errors.iter()
			.map(|e| e.display(&db))
			.collect::<Vec<_>>()
			.join("\n"));
		db
	}

	#[test]
	fn main() {
		let db = setup("main{}");
		assert!(db.completed_procs.contains_key(&"main".id()));
		assert_eq!(db.ast_nodes, [
			ast::Kind::Return(None),
			ast::Kind::Block(ast::Block(vec![0.into()])),
		]);
	}

	#[test]
	fn with_no_params() {
		let db = setup("main{} a :: proc() {}");
		assert!(db.completed_procs.contains_key(&"a".id()));
	}

	#[test]
	fn with_internal_expressions() {
		let db = setup("main { a: u8 = 2 + 3; }");
		assert!(db.completed_procs.contains_key(&"main".id()));
	}

	#[test]
	fn with_internal_sub_expressions() {
		let db = setup("main { a: s8 = (2 + 3) * (4 - 5); }");
		assert!(db.completed_procs.contains_key(&"main".id()));
	}

	#[test]
	fn with_return() {
		let db = setup("main{} a :: proc() -> u16 {}");
		assert!(db.completed_procs.contains_key(&"a".id()));
	}

	#[test]
	fn with_single_param() {
		let db = setup("main{} a :: proc(b:s8) {}");
		assert!(db.completed_procs.contains_key(&"a".id()));
	}

	#[test]
	fn with_multi_params() {
		let db = setup("main{} a :: proc(b:s8,c:u32) {}");
		assert!(db.completed_procs.contains_key(&"a".id()));
	}

	#[test]
	fn with_record_param() {
		let db = setup("main{} a :: record{} b :: proc(c:a){}");
		assert!(db.completed_procs.contains_key(&"b".id()));
	}

	// TODO - srenshaw - Check this once we have out-of-order type resolution.
	// #[test]
	// fn with_out_of_order_record_param() {
	// 	let db = setup("main{} b :: proc(c:a){} a :: record{}");
	// 	assert!(db.completed_procs.contains_key(&"b".id()));
	// }

	#[test]
	fn with_table_param() {
		let db = setup("main{} a :: table[0]{} b :: proc(c:a){}");
		assert!(db.completed_procs.contains_key(&"b".id()));
	}
}

