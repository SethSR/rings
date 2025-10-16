
use std::ops::Range;

use crate::ast;
use crate::cursor::Cursor;
use crate::error;
use crate::identifier;
use crate::token;
use crate::{BinaryOp, Data, RangeType, SrcPos, Task};

use ast::Kind as AKind;
use token::Kind as TKind;

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
			Err((e, token_id)) => {
				data.errors.push(e);

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
					print!("Tokens:");
					for token in &data.tok_list[task.start_token..task.prev_furthest_token] {
						if let token::Kind::Identifier(ident_id) = token {
							print!(" {}", data.text(*ident_id));
						} else {
							print!(" {token:?}");
						}
					}
					data.proc_queue.push_back(task);
					error::error(data,
						&format!("No progress made since last attempt to parse '{name}'"))
				}
			}

			Ok(proc_range) => {
				// We finished. Add us to the 'done' list, so dependent procedures can progress.
				data.completed_procs.insert(task.proc_name, proc_range);
			}
		}
	}
}

macro_rules! error {
	($expected:expr, $found:expr) => {{
		return Err(format!("Expected {}, found {:?}", $expected, $found));
	}};
}

fn parse(data: &mut Data, task: &mut Task) -> Result<Range<ast::Id>, (String, token::Id)> {
	let mut cursor = Cursor::new(task.start_token);
	let cursor = &mut cursor;
	let start = ast::Id::new(data.ast_nodes.len());

	// skip to the procedure body
	while TKind::OBrace != cursor.peek(data, 0) {
		cursor.advance();
	}

	parse_block(cursor, data)
		.map_err(|e| (e, cursor.index()))?;

	let end = ast::Id::new(data.ast_nodes.len());

	Ok(start..end)
}

fn new_ast(data: &mut Data, kind: AKind, location: Range<SrcPos>) -> ast::Id {
	data.ast_nodes.push(kind);
	data.ast_locations.push(location);
	ast::Id::new(data.ast_nodes.len() - 1)
}

fn parse_block(cursor: &mut Cursor, data: &mut Data,
) -> Result<ast::Block, String> {
	cursor.expect(data, TKind::OBrace)?;

	let mut block = vec![];
	while ![TKind::Eof, TKind::CBrace].contains(&cursor.current(data)) {
		block.push(match cursor.current(data) {
			TKind::Identifier(ident_id) => parse_ident_statement(cursor, data, ident_id)?,
			TKind::OBrace => {
				let start = cursor.location(data);
				let b = parse_block(cursor, data)?;
				let end = cursor.location(data);
				new_ast(data, AKind::Block(b), start..end)
			}
			TKind::Return => parse_return_statement(cursor, data)?,
			TKind::If => parse_if_statement(cursor, data)?,
			TKind::For => parse_for_statement(cursor, data)?,
			kind => {
				error!("definition, assignment, return, if, or for statement", kind)
			}
		});
	}

	Ok(ast::Block(block))
}

fn parse_ident_statement(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id,
) -> Result<ast::Id, String> {
	let start = cursor.location(data);
	cursor.advance();
	match cursor.current(data) {
		TKind::Colon => parse_definition(cursor, data, ident_id, start),
		TKind::Equal => parse_assignment(cursor, data, ident_id, start),
		TKind::ColonEqual => {
			Err(format!("type-inference is not implemented yet: {}", data.text(ident_id)))
		}
		TKind::PlusEqual => parse_op_assignment(cursor, data, ident_id, start, BinaryOp::Add),
		TKind::DashEqual => parse_op_assignment(cursor, data, ident_id, start, BinaryOp::Sub),
		TKind::StarEqual => parse_op_assignment(cursor, data, ident_id, start, BinaryOp::Mul),
		TKind::SlashEqual => parse_op_assignment(cursor, data, ident_id, start, BinaryOp::Div),
		next => error!("definition or assignment statement", next),
	}
}

fn parse_definition(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id, start: usize,
) -> Result<ast::Id, String> {
	cursor.advance();
	let var_type = cursor.expect_type(data, "type-specifier")?;
	cursor.expect(data, TKind::Equal)?;
	let ast_id = parse_expression(cursor, data)?;
	let end = cursor.location(data);
	Ok(new_ast(data, AKind::Define(ident_id, var_type, ast_id), start..end))
}

fn parse_assignment(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id, start: usize,
) -> Result<ast::Id, String> {
	cursor.advance();
	let ast_id = parse_expression(cursor, data)?;
	let end = cursor.location(data);
	Ok(new_ast(data, AKind::Assign(ident_id, ast_id), start..end))
}

fn parse_op_assignment(cursor: &mut Cursor, data: &mut Data,
	ident_id: identifier::Id, start: usize, op: BinaryOp,
) -> Result<ast::Id, String> {
	cursor.advance();
	let var_id = new_ast(data, AKind::Ident(ident_id),
		start..cursor.location(data));
	let ast_id = parse_expression(cursor, data)?;
	let end = cursor.location(data);
	let op_id = new_ast(data, AKind::BinOp(op, var_id, ast_id), start..end);
	Ok(new_ast(data, AKind::Assign(ident_id, op_id), start..end))
}

fn parse_return_statement(cursor: &mut Cursor, data: &mut Data) -> Result<ast::Id, String> {
	let start = cursor.location(data);
	cursor.expect(data, TKind::Return)?;
	let ast_id = if TKind::Semicolon != cursor.current(data) {
		Some(parse_expression(cursor, data)?)
	} else {
		None
	};
	let end = cursor.location(data);
	Ok(new_ast(data, AKind::Return(ast_id), start..end))
}

fn parse_if_statement(cursor: &mut Cursor, data: &mut Data) -> Result<ast::Id, String> {
	let start = cursor.location(data);
	cursor.expect(data, TKind::If)?;
	let cond_id = parse_expression(cursor, data)?;
	let then_block = parse_block(cursor, data)?;
	let else_block = if TKind::Else == cursor.current(data) {
		cursor.advance();
		parse_block(cursor, data)?
	} else {
		ast::Block(vec![])
	};
	let end = cursor.location(data);
	Ok(new_ast(data, AKind::If(cond_id, then_block, else_block), start..end))
}

fn parse_for_statement(cursor: &mut Cursor, data: &mut Data) -> Result<ast::Id, String> {
	let start = cursor.location(data);
	cursor.expect(data, TKind::For)?;

	let mut vars = vec![];
	while TKind::In != cursor.current(data) {
		let TKind::Identifier(ident_id) = cursor.current(data) else {
			error!("identifier", cursor.current(data));
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
		"ERROR: expect-table").ok();

	let range = if TKind::OBracket == cursor.current(data) {
		cursor.advance();
		let range_start = cursor.expect_integer(data,
			"ERROR: expect-range-start").ok();
		cursor.expect(data, TKind::DotDot)?;
		let range_end = cursor.expect_integer(data,
			"ERROR: expect-range-end").ok();
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

	let end = cursor.location(data);
	Ok(new_ast(data, AKind::For(vars, table_id, range, block), start..end))
}

fn parse_expression(cursor: &mut Cursor, data: &mut Data) -> Result<ast::Id, String> {
	parse_expr_main(cursor, data, 0)
}

fn parse_expr_main(cursor: &mut Cursor, data: &mut Data,
	min_binding_power: usize,
) -> Result<ast::Id, String> {
	let start = cursor.location(data);
	let left = parse_primary(cursor, data)?;
	parse_expr_sub(cursor, data, min_binding_power, start, left)
}

fn parse_expr_sub(cursor: &mut Cursor, data: &mut Data,
	min_binding_power: usize, start: usize, left: ast::Id,
) -> Result<ast::Id, String> {
	let op = parse_bin_op(cursor, data)?;
	let op_binding_power = binding_power(&op);
	let start_inner = cursor.location(data);
	let right = parse_primary(cursor, data)?;
	let right = if min_binding_power < op_binding_power {
		parse_expr_sub(cursor, data, op_binding_power, start_inner, right)?
	} else {
		right
	};

	let end = cursor.location(data);
	Ok(new_ast(data, AKind::BinOp(op, left, right), start..end))
}

fn parse_primary(cursor: &mut Cursor, data: &mut Data) -> Result<ast::Id, String> {
	let start = cursor.location(data);
	let kind = match cursor.current(data) {
		TKind::Identifier(ident_id) => AKind::Ident(ident_id),
		TKind::Integer(num) => AKind::Int(num),
		TKind::Decimal(num) => AKind::Dec(num),
		kind => error!("identifier or number", kind),
	};
	cursor.advance();
	let end = cursor.location(data);
	Ok(new_ast(data, kind, start..end))
}

fn parse_bin_op(cursor: &mut Cursor, data: &mut Data) -> Result<BinaryOp, String> {
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
		kind => error!("binary operator", kind),
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

