
use crate::identifier::IdentId;
use crate::operators::BinaryOp;
use crate::token::{Id as TokenId, Kind as TKind};

use super::ast::{Ast, AstId, AstList, Kind as AstKind, PathSegment};
use super::cursor::Cursor;
use super::error::Error;
use super::Data;

type KindList = AstList<AstKind, TokenId>;

pub fn parse_block(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	data: &mut Data<TokenId>,
	proc_id: IdentId,
	depth: u16,
) -> Result<Vec<AstId>, Error> {
	let scope_start = cursor.index();
	cursor.expect(TKind::OBrace)?;
	nodes.push(Ast::scope_begin((scope_start..scope_start).into()));

	let mut block = vec![];
	while ![TKind::Eof, TKind::CBrace].contains(&cursor.current()) {
		block.push(match cursor.current() {
			TKind::Let => parse_let_statement(cursor, nodes, data, proc_id, depth)?,
			TKind::Identifier(ident_id) => parse_ident_statement(cursor, nodes, ident_id)?,
			TKind::OBrace => {
				let tok_start = cursor.index();
				let b = parse_block(cursor, nodes, data, proc_id, depth + 1)?;
				let tok_end = cursor.index();
				nodes.push(Ast::block(b, (tok_start..tok_end).into()))
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

	let scope_end = cursor.index();
	cursor.expect(TKind::CBrace)?;
	nodes.push(Ast::scope_end((scope_end..scope_end).into()));

	Ok(block)
}

fn parse_ident_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	ident_id: IdentId,
) -> Result<AstId, Error> {
	let tok_start = cursor.index();
	let left_id = parse_access(cursor, nodes, ident_id)?;

	match cursor.current() {
		TKind::Eq => parse_assignment(cursor, nodes, tok_start, left_id),
		TKind::PlusEq => parse_op_assignment(cursor, nodes, tok_start, left_id, BinaryOp::Add),
		TKind::DashEq => parse_op_assignment(cursor, nodes, tok_start, left_id, BinaryOp::Sub),
		TKind::StarEq => parse_op_assignment(cursor, nodes, tok_start, left_id, BinaryOp::Mul),
		TKind::SlashEq => parse_op_assignment(cursor, nodes, tok_start, left_id, BinaryOp::Div),
		_ => Err(cursor.expected_token("definition or assignment statement")),
	}
}

// TODO - srenshaw - Add local record construction

// TODO - srenshaw - Add overlay syntax

/// Matches Let statements:
/// - `let <ident>: <type> = <expr>;`
/// - `let <ident>: <type>;` (not implemented)
/// - `let <ident> = <expr>;` (not implemented)
fn parse_let_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	data: &mut Data<TokenId>,
	proc_id: IdentId,
	depth: u16,
) -> Result<AstId, Error> {
	let tok_start = cursor.index();
	cursor.expect(TKind::Let)?;
	let id_start = cursor.index();
	let ident_id = cursor.expect_identifier("variable identifier")?;
	let id_end = cursor.index();
	let var_type = if cursor.expect(TKind::Colon).is_ok() {
		cursor.expect_type(data)?
	} else {
		super::Type::Unknown
	};
	cursor.expect(TKind::Eq)?;
	let ast_id = parse_expression(cursor, nodes, &[TKind::Semicolon])?;
	cursor.expect(TKind::Semicolon)?;
	let tok_end = cursor.index();
	let ident = nodes.push(Ast::ident(ident_id, (id_start..id_end).into()));
	data.types.insert((proc_id, depth, ident_id), var_type);
	Ok(nodes.push(Ast::assign(ident, ast_id, (tok_start..tok_end).into())))
}

fn parse_access(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	ident_id: IdentId,
) -> Result<AstId, Error> {
	let tok_start = cursor.index();
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
	let tok_end = cursor.index();
	let kind = if accesses.is_empty() {
		Ast::ident(ident_id, (tok_start..tok_end).into())
	} else {
		Ast::access(ident_id, accesses, (tok_start..tok_end).into())
	};
	Ok(nodes.push(kind))
}

fn parse_assignment(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	tok_start: TokenId,
	lvalue_id: AstId,
) -> Result<AstId, Error> {
	cursor.expect(TKind::Eq)?;
	let ast_id = parse_expression(cursor, nodes, &[TKind::Semicolon])?;
	cursor.expect(TKind::Semicolon)?;
	let tok_end = cursor.index();
	Ok(nodes.push(Ast::assign(lvalue_id, ast_id, (tok_start..tok_end).into())))
}

fn parse_op_assignment(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	tok_start: TokenId,
	lvalue_id: AstId, op: BinaryOp,
) -> Result<AstId, Error> {
	cursor.advance(); // skip the operator token
	let op_start = cursor.index();
	let ast_id = parse_expression(cursor, nodes, &[TKind::Semicolon])?;
	let op_end = cursor.index();
	cursor.expect(TKind::Semicolon)?;
	let op_id = nodes.push(Ast::bin_op(op, lvalue_id, ast_id, (op_start..op_end).into()));
	let tok_end = cursor.index();
	Ok(nodes.push(Ast::assign(lvalue_id, op_id, (tok_start..tok_end).into())))
}

fn parse_return_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
) -> Result<AstId, Error> {
	let tok_start = cursor.index();
	cursor.expect(TKind::Return)?;
	let ast_id = match cursor.expect(TKind::Semicolon) {
		Ok(_) => None,
		Err(_) => {
			let result = parse_expression(cursor, nodes, &[TKind::Semicolon])?;
			cursor.expect(TKind::Semicolon)?;
			Some(result)
		},
	};
	let tok_end = cursor.index();
	Ok(nodes.push(Ast::return_(ast_id, (tok_start..tok_end).into())))
}

fn parse_if_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	data: &mut Data<TokenId>,
	proc_id: IdentId,
	depth: u16,
) -> Result<AstId, Error> {
	let tok_start = cursor.index();
	cursor.expect(TKind::If)?;
	let cond_id = parse_expression(cursor, nodes, &[TKind::OBrace])?;
	let then_block = parse_block(cursor, nodes, data, proc_id, depth + 1)?;
	let else_block = if TKind::Else == cursor.current() {
		cursor.advance();
		parse_block(cursor, nodes, data, proc_id, depth + 1)?
	} else {
		vec![]
	};
	let tok_end = cursor.index();
	Ok(nodes.push(Ast::if_(cond_id, then_block, else_block, (tok_start..tok_end).into())))
}

fn parse_while_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	data: &mut Data<TokenId>,
	proc_id: IdentId,
	depth: u16,
) -> Result<AstId, Error> {
	let tok_start = cursor.index();
	cursor.expect(TKind::While)?;
	let cond = parse_expression(cursor, nodes, &[TKind::OBrace])?;
	let block = parse_block(cursor, nodes, data, proc_id, depth + 1)?;
	let tok_end = cursor.index();
	Ok(nodes.push(Ast::while_(cond, block, (tok_start..tok_end).into())))
}

fn parse_for_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	data: &mut Data<TokenId>,
	proc_id: IdentId,
	depth: u16,
) -> Result<AstId, Error> {
	let tok_start = cursor.index();
	cursor.expect(TKind::For)?;

	let mut vars = vec![];
	let id_start = cursor.index();
	let ident_id = cursor.expect_identifier("identifier")?;
	let id_end = cursor.index();
	vars.push((ident_id, (id_start..id_end).into()));

	while TKind::In != cursor.current() {
		cursor.expect(TKind::Comma)?;
		let id_start = cursor.index();
		let ident_id = cursor.expect_identifier("identifier")?;
		let id_end = cursor.index();
		vars.push((ident_id, (id_start..id_end).into()));
	}

	let vars = vars.into_iter()
			.map(|(ident_id, span)| {
				data.types.insert((proc_id, depth, ident_id), super::Type::Unknown);
				nodes.push(Ast::ident(ident_id, span))
			})
			.collect();

	cursor.expect(TKind::In)?;

	let range_location = cursor.index();
	let table_id = cursor.expect_identifier("COMPILER ERROR").ok();

	let (range_start, range_end) = if cursor.expect(TKind::OBracket).is_ok() {
		let range_start = parse_expression(cursor, nodes, &[TKind::Dot2]).ok();
		cursor.expect(TKind::Dot2)?;
		let range_end = parse_expression(cursor, nodes, &[TKind::CBracket]).ok();
		cursor.expect(TKind::CBracket)?;
		(range_start, range_end)
	} else {
		(None, None)
	};

	if table_id.is_none() && range_start.is_none() && range_end.is_none() {
		return Err(Error::ExpectedToken {
			expected: "table range or bound range".to_string(),
			found: range_location,
		});
	}

	let block = parse_block(cursor, nodes, data, proc_id, depth + 1)?;

	let tok_end = cursor.index();
	Ok(nodes.push(Ast::for_(vars, table_id, range_start, range_end, block, (tok_start..tok_end).into())))
}

/// Matches Mark construct:
/// - `mark <ident> in <region>;`
fn parse_mark_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
) -> Result<AstId, Error> {
	let tok_start = cursor.index();
	cursor.expect(TKind::Mark)?;
	let mark_id = cursor.expect_identifier("mark name")?;
	cursor.expect(TKind::In)?;
	let region_id = cursor.expect_identifier("region name")?;
	cursor.expect(TKind::Semicolon)?;
	let tok_end = cursor.index();
	Ok(nodes.push(Ast::new_mark(region_id, mark_id, (tok_start..tok_end).into())))
}

/// Matches Free construct:
/// - `free <ident>;`
/// - `free <ident> in <region>;`
fn parse_free_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
) -> Result<AstId, Error> {
	let tok_start = cursor.index();
	cursor.expect(TKind::Free)?;
	let ident = cursor.expect_identifier("mark or region name")?;
	let (region_id, mark_id) = if cursor.expect(TKind::In).is_ok() {
		let region_id = cursor.expect_identifier("region name")?;
		(region_id, Some(ident))
	} else {
		(ident, None)
	};
	cursor.expect(TKind::Semicolon)?;
	let tok_end = cursor.index();
	Ok(nodes.push(Ast::new_free(region_id, mark_id, (tok_start..tok_end).into())))
}

/// Matches Use construct:
/// - `use <ident> in <region>;`
fn parse_use_statement(
	cursor: &mut Cursor,
	nodes: &mut KindList,
) -> Result<AstId, Error> {
	let tok_start = cursor.index();
	cursor.expect(TKind::Use)?;
	let ident = cursor.expect_identifier("table or record name")?;
	cursor.expect(TKind::In)?;
	let region_id = cursor.expect_identifier("region name")?;
	cursor.expect(TKind::Semicolon)?;
	let tok_end = cursor.index();
	Ok(nodes.push(Ast::new_use(region_id, ident, (tok_start..tok_end).into())))
}

fn parse_call(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	ident_id: IdentId,
) -> Result<AstId, Error> {
	let tok_start = cursor.index();
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
	let tok_end = cursor.index();
	Ok(nodes.push(Ast::call(ident_id, exprs, (tok_start..tok_end).into())))
}

fn parse_expression(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	end_tokens: &[TKind],
) -> Result<AstId, Error> {
	let tok_start = cursor.index();
	parse_expr_main(cursor, nodes, tok_start, 0, end_tokens)
}

fn parse_expr_main(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	tok_start: TokenId,
	min_binding_power: usize, end_tokens: &[TKind],
) -> Result<AstId, Error> {
	let left = parse_primary(cursor, nodes)?;
	parse_expr_sub(cursor, nodes, tok_start, min_binding_power, left, end_tokens)
}

fn parse_expr_sub(
	cursor: &mut Cursor,
	nodes: &mut KindList,
	tok_start: TokenId,
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
		parse_expr_sub(cursor, nodes, tok_start, op_binding_power,
			right, end_tokens)?
	} else {
		right
	};
	let tok_end = cursor.index();

	Ok(nodes.push(Ast::bin_op(op, left, right, (tok_start..tok_end).into())))
}

fn parse_primary(
	cursor: &mut Cursor,
	nodes: &mut KindList,
) -> Result<AstId, Error> {
	let tok_start = cursor.index();
	let unary_op = cursor.expect_unary_op();

	let primary_node = |cursor: &mut Cursor, nodes: &mut KindList, ast| {
		cursor.advance();
		nodes.push(ast)
	};

	let tok_loc = cursor.index();
	let node = match cursor.current() {
		TKind::Identifier(ident_id) => {
			match cursor.peek(1) {
				TKind::OParen => parse_call(cursor, nodes, ident_id)?,
				TKind::Dot | TKind::OBracket => parse_access(cursor, nodes, ident_id)?,
				_ => primary_node(cursor, nodes, Ast::ident(ident_id, (tok_loc..tok_loc).into())),
			}
		}
		TKind::Integer(num) => primary_node(cursor, nodes, Ast::int(num, (tok_loc..tok_loc).into())),
		TKind::Decimal(num) => primary_node(cursor, nodes, Ast::dec(num, (tok_loc..tok_loc).into())),
		TKind::True => primary_node(cursor, nodes, Ast::int(1, (tok_loc..tok_loc).into())),
		TKind::False => primary_node(cursor, nodes, Ast::int(0, (tok_loc..tok_loc).into())),
		TKind::OParen => {
			cursor.advance();
			let expr = parse_expression(cursor, nodes, &[TKind::CParen])?;
			cursor.expect(TKind::CParen)?;
			expr
		}
		_ => return Err(cursor.expected_token("identifier or number")),
	};
	let tok_end = cursor.index();

	if let Some(op) = unary_op {
		Ok(nodes.push(Ast::un_op(op, node, (tok_start..tok_end).into())))
	} else {
		Ok(node)
	}
}

