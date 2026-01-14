
use crate::identifier::IdentId;
use crate::operators::BinaryOp;
use crate::token::Kind as TKind;
use crate::Bounds;

use super::ast::{AstId, AstKind, KindList, PathSegment};
use super::cursor::Cursor;
use super::error::Error;
use super::Data;

pub fn parse_block(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	data: &mut Data,
	proc_id: IdentId,
	depth: u16,
) -> Result<Vec<AstId>, Error> {
	cursor.expect(TKind::OBrace)?;

	let mut block = vec![];
	while ![TKind::Eof, TKind::CBrace].contains(&cursor.current()) {
		block.push(match cursor.current() {
			TKind::Let => parse_let_statement(cursor, nodes, data, proc_id, depth)?,
			TKind::Identifier(ident_id) => parse_ident_statement(cursor, nodes, ident_id)?,
			TKind::OBrace => {
				let b = parse_block(cursor, nodes, data, proc_id, depth + 1)?;
				nodes.push(AstKind::Block(b))
			}
			TKind::Return => parse_return_statement(cursor, nodes)?,
			TKind::If => parse_if_statement(cursor, nodes, data, proc_id, depth)?,
			TKind::For => parse_for_statement(cursor, nodes, data, proc_id, depth)?,
			TKind::While => parse_while_statement(cursor, nodes, data, proc_id, depth)?,
			TKind::Mark => parse_mark_statement(cursor, nodes)?,
			TKind::Free => parse_free_statement(cursor, nodes)?,
			TKind::Use => parse_use_statement(cursor, nodes)?,
			_ => return Err(cursor.expected_token("definition, assignment, return, if, or for statement")),
		});
	}

	cursor.expect(TKind::CBrace)?;

	Ok(block)
}

fn parse_ident_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	ident_id: IdentId,
) -> Result<AstId, Error> {
	let left_id = parse_access(cursor, nodes, ident_id)?;

	match cursor.current() {
		TKind::Eq => parse_assignment(cursor, nodes, left_id),
		TKind::PlusEq => parse_op_assignment(cursor, nodes, left_id, BinaryOp::Add),
		TKind::DashEq => parse_op_assignment(cursor, nodes, left_id, BinaryOp::Sub),
		TKind::StarEq => parse_op_assignment(cursor, nodes, left_id, BinaryOp::Mul),
		TKind::SlashEq => parse_op_assignment(cursor, nodes, left_id, BinaryOp::Div),
		_ => Err(cursor.expected_token("definition or assignment statement")),
	}
}

// TODO - srenshaw - Add local record definitions

// TODO - srenshaw - Add overlay syntax

/// Matches Let statements:
/// - `let <ident>: <type> = <expr>;`
/// - `let <ident>: <type>;` (not implemented)
/// - `let <ident> = <expr>;` (not implemented)
fn parse_let_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	data: &mut Data,
	proc_id: IdentId,
	depth: u16,
) -> Result<AstId, Error> {
	cursor.expect(TKind::Let)?;
	let ident_id = cursor.expect_identifier("variable identifier")?;
	cursor.expect(TKind::Colon)?;
	let var_type = cursor.expect_type(data)?;
	cursor.expect(TKind::Eq)?;
	let ast_id = parse_expression(cursor, nodes, &[TKind::Semicolon])?;
	cursor.expect(TKind::Semicolon)?;
	let ident = nodes.push(AstKind::Ident(ident_id));
	data.types.insert((proc_id, depth, ident_id), var_type);
	Ok(nodes.push(AstKind::Assign(ident, ast_id)))
}

fn parse_access(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	ident_id: IdentId,
) -> Result<AstId, Error> {
	cursor.advance(); // skip the identifier
	let mut accesses = vec![];
	while [TKind::Dot, TKind::OBracket].contains(&cursor.current()) {
		match cursor.current() {
			// a.b -> location of 'a' + offset of 'b'
			TKind::Dot => {
				cursor.expect(TKind::Dot)?;
				let id = cursor.expect_identifier("field name")?;
				accesses.push(PathSegment::Field(id));
			}
			// a[b].c -> location of 'a' + 'b' * size of 'c'
			TKind::OBracket => {
				cursor.expect(TKind::OBracket)?;
				let index = parse_expression(cursor, nodes, &[TKind::CBracket])?;
				cursor.expect(TKind::CBracket)?;
				cursor.expect(TKind::Dot)?;
				let id = cursor.expect_identifier("field name")?;
				accesses.push(PathSegment::Index(index, id));
			}
			_ => break,
		}
	}
	let kind = if accesses.is_empty() {
		AstKind::Ident(ident_id)
	} else {
		AstKind::Access(ident_id, accesses)
	};
	Ok(nodes.push(kind))
}

fn parse_assignment(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	lvalue_id: AstId,
) -> Result<AstId, Error> {
	cursor.expect(TKind::Eq)?;
	let ast_id = parse_expression(cursor, nodes, &[TKind::Semicolon])?;
	cursor.expect(TKind::Semicolon)?;
	Ok(nodes.push(AstKind::Assign(lvalue_id, ast_id)))
}

fn parse_op_assignment(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	lvalue_id: AstId, op: BinaryOp,
) -> Result<AstId, Error> {
	cursor.advance(); // skip the operator token
	let ast_id = parse_expression(cursor, nodes, &[TKind::Semicolon])?;
	cursor.expect(TKind::Semicolon)?;
	let op_id = nodes.push(AstKind::BinOp(op, lvalue_id, ast_id));
	Ok(nodes.push(AstKind::Assign(lvalue_id, op_id)))
}

fn parse_return_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
) -> Result<AstId, Error> {
	cursor.expect(TKind::Return)?;
	let ast_id = match cursor.expect(TKind::Semicolon) {
		Ok(_) => None,
		Err(_) => {
			let result = parse_expression(cursor, nodes, &[TKind::Semicolon])?;
			cursor.expect(TKind::Semicolon)?;
			Some(result)
		},
	};
	Ok(nodes.push(AstKind::Return(ast_id)))
}

fn parse_if_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	data: &mut Data,
	proc_id: IdentId,
	depth: u16,
) -> Result<AstId, Error> {
	cursor.expect(TKind::If)?;
	let cond_id = parse_expression(cursor, nodes, &[TKind::OBrace])?;
	let then_block = parse_block(cursor, nodes, data, proc_id, depth + 1)?;
	let else_block = if TKind::Else == cursor.current() {
		cursor.advance();
		parse_block(cursor, nodes, data, proc_id, depth + 1)?
	} else {
		vec![]
	};
	Ok(nodes.push(AstKind::If(cond_id, then_block, else_block)))
}

fn parse_while_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	data: &mut Data,
	proc_id: IdentId,
	depth: u16,
) -> Result<AstId, Error> {
	cursor.expect(TKind::While)?;
	let cond = parse_expression(cursor, nodes, &[TKind::OBrace])?;
	let block = parse_block(cursor, nodes, data, proc_id, depth + 1)?;
	Ok(nodes.push(AstKind::While(cond, block)))
}

fn parse_for_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	data: &mut Data,
	proc_id: IdentId,
	depth: u16,
) -> Result<AstId, Error> {
	cursor.expect(TKind::For)?;

	let mut vars = vec![];
	let ident_id = cursor.expect_identifier("identifier")?;
	vars.push(ident_id);

	while TKind::Comma == cursor.current() {
		cursor.advance();
		let ident_id = cursor.expect_identifier("identifier")?;
		vars.push(ident_id);
	}

	while TKind::In != cursor.current() {
		let ident_id = cursor.expect_identifier("identifier")?;
		vars.push(ident_id);
		if TKind::In == cursor.current() {
			break;
		}
		cursor.expect(TKind::Comma)?;
	}

	cursor.expect(TKind::In)?;

	// for x in Table {}
	// for x in Table[..] {}
	// for x in [0..10] {}
	let (table_id, range) = if let Ok(table_id) = cursor.expect_identifier("COMPILER ERROR") {
		// Table loop
		let range = if TKind::OBracket == cursor.current() {
			cursor.advance();
			let range_start = cursor.expect_u32("COMPILER ERROR").ok();
			cursor.expect(TKind::Dot2)?;
			let range_end = cursor.expect_u32("COMPILER ERROR").ok();
			cursor.expect(TKind::CBracket)?;
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
	} else if TKind::OBracket == cursor.current() {
		cursor.advance();
		let start = cursor.expect_u32("start (inclusive) value")?;
		cursor.expect(TKind::Dot2)?;
		let end = cursor.expect_u32("end (exclusive) value")?;
		cursor.expect(TKind::CBracket)?;
		(None, Some(Bounds::Full { start, end }))
	} else {
		return Err(cursor.expected_token("table name or bracketed range"));
	};

	let block = parse_block(cursor, nodes, data, proc_id, depth + 1)?;

	Ok(nodes.push(AstKind::For(vars, table_id, range, block)))
}

/// Matches Mark construct:
/// - `mark <ident> in <region>;`
fn parse_mark_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
) -> Result<AstId, Error> {
	cursor.expect(TKind::Mark)?;
	let mark_id = cursor.expect_identifier("mark name")?;
	cursor.expect(TKind::In)?;
	let region_id = cursor.expect_identifier("region name")?;
	cursor.expect(TKind::Semicolon)?;
	Ok(nodes.push(AstKind::new_mark(region_id, mark_id)))
}

/// Matches Free construct:
/// - `free <ident>;`
/// - `free <ident> in <region>;`
fn parse_free_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
) -> Result<AstId, Error> {
	cursor.expect(TKind::Free)?;
	let ident = cursor.expect_identifier("mark or region name")?;
	let (region_id, mark_id) = if cursor.expect(TKind::In).is_ok() {
		let region_id = cursor.expect_identifier("region name")?;
		(region_id, Some(ident))
	} else {
		(ident, None)
	};
	cursor.expect(TKind::Semicolon)?;
	Ok(nodes.push(AstKind::new_free(region_id, mark_id)))
}

/// Matches Use construct:
/// - `use <ident> in <region>;`
fn parse_use_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
) -> Result<AstId, Error> {
	cursor.expect(TKind::Use)?;
	let ident = cursor.expect_identifier("table or record name")?;
	cursor.expect(TKind::In)?;
	let region_id = cursor.expect_identifier("region name")?;
	cursor.expect(TKind::Semicolon)?;
	Ok(nodes.push(AstKind::new_use(region_id, ident)))
}

fn parse_call(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	ident_id: IdentId,
) -> Result<AstId, Error> {
	cursor.advance();
	cursor.expect(TKind::OParen)?;
	let end_tokens = &[TKind::CParen, TKind::Comma];

	let mut exprs = vec![];
	while TKind::CParen != cursor.current() {
		let expr = parse_expression(cursor, nodes, end_tokens)?;
		exprs.push(expr);

		if TKind::Comma == cursor.current() {
			cursor.advance();
			if TKind::CParen == cursor.current() {
				break;
			}
		} else if TKind::CParen != cursor.current() {
			break;
		}
	}

	cursor.expect(TKind::CParen)?;
	Ok(nodes.push(AstKind::Call(ident_id, exprs)))
}

fn parse_expression(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	end_tokens: &[TKind],
) -> Result<AstId, Error> {
	parse_expr_main(cursor, nodes, 0, end_tokens)
}

fn parse_expr_main(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	min_binding_power: usize, end_tokens: &[TKind],
) -> Result<AstId, Error> {
	let left = parse_primary(cursor, nodes)?;
	parse_expr_sub(cursor, nodes, min_binding_power, left, end_tokens)
}

fn parse_expr_sub(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	min_binding_power: usize, left: AstId,
	end_tokens: &[TKind],
) -> Result<AstId, Error> {
	if end_tokens.contains(&cursor.current()) {
		return Ok(left);
	}
	let Ok(op) = cursor.expect_bin_op() else {
		return Ok(left);
	};
	let op_binding_power = op.binding_power();
	let right = parse_primary(cursor, nodes)?;
	let right = if min_binding_power <= op_binding_power {
		parse_expr_sub(cursor, nodes, op_binding_power,
			right, end_tokens)?
	} else {
		right
	};

	Ok(nodes.push(AstKind::BinOp(op, left, right)))
}

fn parse_primary(
	cursor: &mut Cursor,
	nodes: &mut KindList,
) -> Result<AstId, Error> {
	let unary_op = cursor.expect_unary_op();

	fn primary_node(cursor: &mut Cursor, nodes: &mut KindList, kind: AstKind) -> AstId {
		cursor.advance();
		nodes.push(kind)
	}

	let node = match cursor.current() {
		TKind::Identifier(ident_id) => {
			match cursor.peek(1) {
				TKind::OParen => parse_call(cursor, nodes, ident_id)?,
				TKind::Dot | TKind::OBracket => parse_access(cursor, nodes, ident_id)?,
				_ => primary_node(cursor, nodes, AstKind::Ident(ident_id)),
			}
		}
		TKind::Integer(num) => primary_node(cursor, nodes, AstKind::Int(num)),
		TKind::Decimal(num) => primary_node(cursor, nodes, AstKind::Dec(num)),
		TKind::True => primary_node(cursor, nodes, AstKind::Int(1)),
		TKind::False => primary_node(cursor, nodes, AstKind::Int(0)),
		TKind::OParen => {
			cursor.advance();
			let expr = parse_expression(cursor, nodes, &[TKind::CParen])?;
			cursor.expect(TKind::CParen)?;
			expr
		}
		_ => return Err(cursor.expected_token("identifier or number")),
	};

	if let Some(op) = unary_op {
		Ok(nodes.push(AstKind::UnOp(op, node)))
	} else {
		Ok(node)
	}
}

