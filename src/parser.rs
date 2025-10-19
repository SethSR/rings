
use std::ops::Range;

use crate::ast;
use crate::cursor::Cursor;
use crate::error;
use crate::identifier;
use crate::token;
use crate::{BinaryOp, Data, RangeType, SrcPos};

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
	data.proc_queue = data.procedures.keys()
		.map(|&proc_name| Task {
			proc_name,
			start_token: data.proc_start[&proc_name],
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
					error::error_with_notes(data,
						&format!("No progress made since last attempt to parse '{name}'"),
						task.start_token,
						&[("reached this point before failing", task.prev_furthest_token)]
					);
					data.proc_queue.push_back(task);
					return;
				}
			}

			Ok(proc_range) => {
				// We finished. Add us to the 'done' list, so dependent procedures can progress.
				data.completed_procs.insert(task.proc_name, proc_range);
			}
		}
	}
}

fn parse(data: &mut Data, task: &mut Task) -> Result<Range<ast::Id>, token::Id> {
	let mut cursor = Cursor::new(task.start_token);
	let cursor = &mut cursor;
	let start = ast::Id::new(data.ast_nodes.len());

	// skip to the procedure body
	while TKind::OBrace != cursor.peek(data, 0) {
		cursor.advance();
	}

	parse_block(cursor, data)
		.ok_or(cursor.index())?;

	let end = ast::Id::new(data.ast_nodes.len());

	Ok(start..end)
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
) -> Option<ast::Block> {
	cursor.expect(data, TKind::OBrace)?;

	let mut block = vec![];
	while ![TKind::Eof, TKind::CBrace].contains(&cursor.current(data)) {
		block.push(match cursor.current(data) {
			TKind::Identifier(ident_id) => parse_ident_statement(cursor, data, ident_id)?,
			TKind::OBrace => {
				let src_start = cursor.location(data);
				let tok_start = cursor.index();
				let b = parse_block(cursor, data)?;
				let src_end = cursor.location(data);
				let tok_end = cursor.index();
				new_ast(data, AKind::Block(b),
					src_start..src_end,
					tok_start..tok_end)
			}
			TKind::Return => parse_return_statement(cursor, data)?,
			TKind::If => parse_if_statement(cursor, data)?,
			TKind::For => parse_for_statement(cursor, data)?,
			_ => {
				error::expected_token(data,
					"definition, assignment, return, if, or for statement", cursor.index());
				return None;
			}
		});
	}

	Some(ast::Block(block))
}

fn parse_ident_statement(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id,
) -> Option<ast::Id> {
	match cursor.peek(data, 1) {
		TKind::Colon => parse_definition(cursor, data, ident_id),
		TKind::Equal => parse_assignment(cursor, data, ident_id),
		TKind::ColonEqual => {
			error::error(data,
				"type-inference is not implemented yet, please add a type-specifier",
				cursor.index());
			None
		}
		TKind::PlusEqual => parse_op_assignment(cursor, data, ident_id, BinaryOp::Add),
		TKind::DashEqual => parse_op_assignment(cursor, data, ident_id, BinaryOp::Sub),
		TKind::StarEqual => parse_op_assignment(cursor, data, ident_id, BinaryOp::Mul),
		TKind::SlashEqual => parse_op_assignment(cursor, data, ident_id, BinaryOp::Div),
		_ => {
			error::expected_token(data, "definition or assignment statement", cursor.index());
			None
		}
	}
}

fn parse_definition(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id,
) -> Option<ast::Id> {
	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	cursor.advance();
	cursor.advance();
	let var_type = cursor.expect_type(data, "type-specifier")?;
	cursor.expect(data, TKind::Equal)?;
	let ast_id = parse_expression(cursor, data)?;
	let src_end = cursor.location(data);
	let tok_end = cursor.index();
	Some(new_ast(data, AKind::Define(ident_id, var_type, ast_id),
		src_start..src_end,
		tok_start..tok_end,
	))
}

fn parse_assignment(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id,
) -> Option<ast::Id> {
	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	cursor.advance();
	cursor.advance();
	let ast_id = parse_expression(cursor, data)?;
	let src_end = cursor.location(data);
	let tok_end = cursor.index();
	Some(new_ast(data, AKind::Assign(ident_id, ast_id),
		src_start..src_end,
		tok_start..tok_end,
	))
}

fn parse_op_assignment(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id, op: BinaryOp,
) -> Option<ast::Id> {
	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	cursor.advance();
	cursor.advance();
	let var_id = new_ast(data, AKind::Ident(ident_id),
		src_start..cursor.location(data),
		tok_start..cursor.index(),
	);
	let ast_id = parse_expression(cursor, data)?;
	let src_end = cursor.location(data);
	let tok_end = cursor.index();
	let op_id = new_ast(data, AKind::BinOp(op, var_id, ast_id),
		src_start..src_end,
		tok_start..tok_end,
	);
	Some(new_ast(data, AKind::Assign(ident_id, op_id),
		src_start..src_end,
		tok_start..tok_end,
	))
}

fn parse_return_statement(cursor: &mut Cursor, data: &mut Data) -> Option<ast::Id> {
	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	cursor.expect(data, TKind::Return)?;
	let ast_id = if TKind::Semicolon != cursor.current(data) {
		Some(parse_expression(cursor, data)?)
	} else {
		None
	};
	let src_end = cursor.location(data);
	let tok_end = cursor.index();
	Some(new_ast(data, AKind::Return(ast_id),
		src_start..src_end,
		tok_start..tok_end,
	))
}

fn parse_if_statement(cursor: &mut Cursor, data: &mut Data) -> Option<ast::Id> {
	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	cursor.expect(data, TKind::If)?;
	let cond_id = parse_expression(cursor, data)?;
	let then_block = parse_block(cursor, data)?;
	let else_block = if TKind::Else == cursor.current(data) {
		cursor.advance();
		parse_block(cursor, data)?
	} else {
		ast::Block(vec![])
	};
	let src_end = cursor.location(data);
	let tok_end = cursor.index();
	Some(new_ast(data, AKind::If(cond_id, then_block, else_block),
		src_start..src_end,
		tok_start..tok_end,
	))
}

fn parse_for_statement(cursor: &mut Cursor, data: &mut Data) -> Option<ast::Id> {
	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	cursor.expect(data, TKind::For)?;

	let mut vars = vec![];
	while TKind::In != cursor.current(data) {
		let TKind::Identifier(ident_id) = cursor.current(data) else {
			error::expected_token(data, "identifier", cursor.index());
			return None;
		};
		cursor.advance();
		vars.push(ident_id);
		if TKind::In == cursor.current(data) {
			break;
		}
		cursor.expect(data, TKind::Comma)?;
	}

	cursor.expect(data, TKind::In)?;

	let table_id = cursor.expect_identifier(data,
		"ERROR: expect-table");

	let range = if TKind::OBracket == cursor.current(data) {
		cursor.advance();
		let range_start = cursor.expect_integer(data,
			"ERROR: expect-range-start");
		cursor.expect(data, TKind::DotDot)?;
		let range_end = cursor.expect_integer(data,
			"ERROR: expect-range-end");
		cursor.expect(data, TKind::CBracket)?;
		match (range_start, range_end) {
			(Some(start), Some(end)) => Some(RangeType::Full { start, end }),
			(Some(start), None) => Some(RangeType::From { start }),
			(None, Some(end)) => Some(RangeType::To { end }),
			(None, None) => None,
		}
	} else {
		None
	};

	let block = parse_block(cursor, data)?;

	let src_end = cursor.location(data);
	let tok_end = cursor.index();
	Some(new_ast(data, AKind::For(vars, table_id, range, block),
		src_start..src_end,
		tok_start..tok_end,
	))
}

fn parse_expression(cursor: &mut Cursor, data: &mut Data) -> Option<ast::Id> {
	parse_expr_main(cursor, data, 0)
}

fn parse_expr_main(cursor: &mut Cursor, data: &mut Data,
	min_binding_power: usize,
) -> Option<ast::Id> {
	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	let left = parse_primary(cursor, data)?;
	parse_expr_sub(cursor, data, min_binding_power, src_start, tok_start, left)
}

fn parse_expr_sub(cursor: &mut Cursor, data: &mut Data,
	min_binding_power: usize, src_start: usize, tok_start: token::Id, left: ast::Id,
) -> Option<ast::Id> {
	if TKind::Semicolon == cursor.current(data) {
		cursor.advance();
		return Some(left);
	}
	let op = parse_bin_op(cursor, data)?;
	let op_binding_power = binding_power(&op);
	let src_start_inner = cursor.location(data);
	let tok_start_inner = cursor.index();
	let right = parse_primary(cursor, data)?;
	let right = if min_binding_power < op_binding_power {
		parse_expr_sub(cursor, data, op_binding_power,
			src_start_inner, tok_start_inner, right)?
	} else {
		right
	};

	let src_end = cursor.location(data);
	let tok_end = cursor.index();
	Some(new_ast(data, AKind::BinOp(op, left, right),
		src_start..src_end,
		tok_start..tok_end,
	))
}

fn parse_primary(cursor: &mut Cursor, data: &mut Data) -> Option<ast::Id> {
	let src_start = cursor.location(data);
	let tok_start = cursor.index();
	let kind = match cursor.current(data) {
		TKind::Identifier(ident_id) => AKind::Ident(ident_id),
		TKind::Integer(num) => AKind::Int(num),
		TKind::Decimal(num) => AKind::Dec(num),
		_ => {
			error::expected_token(data, "identifier or number", cursor.index());
			return None;
		}
	};
	cursor.advance();
	let src_end = cursor.location(data);
	let tok_end = cursor.index();
	Some(new_ast(data, kind,
		src_start..src_end,
		tok_start..tok_end,
	))
}

fn parse_bin_op(cursor: &mut Cursor, data: &mut Data) -> Option<BinaryOp> {
	match cursor.current(data) {
		TKind::Plus => Some(BinaryOp::Add),
		TKind::Dash => Some(BinaryOp::Sub),
		TKind::Star => Some(BinaryOp::Mul),
		TKind::Slash => Some(BinaryOp::Div),
		TKind::Percent => Some(BinaryOp::Mod),
		TKind::Amp => Some(BinaryOp::BinAnd),
		TKind::AmpAmp => Some(BinaryOp::LogAnd),
		TKind::Bar => Some(BinaryOp::BinOr),
		TKind::BarBar => Some(BinaryOp::LogOr),
		TKind::Carrot => Some(BinaryOp::BinXor),
		TKind::CarrotCarrot => Some(BinaryOp::LogXor),
		TKind::EqualEqual => Some(BinaryOp::CmpEQ),
		TKind::BangEqual => Some(BinaryOp::CmpNE),
		TKind::Less => Some(BinaryOp::CmpLT),
		TKind::LessEqual => Some(BinaryOp::CmpLE),
		TKind::LessLess => Some(BinaryOp::ShL),
		TKind::Greater => Some(BinaryOp::CmpGT),
		TKind::GreaterEqual => Some(BinaryOp::CmpGE),
		TKind::GreaterGreater => Some(BinaryOp::ShR),
		_ => {
			error::expected_token(data, "binary operator", cursor.index());
			None
		}
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
	}
}

