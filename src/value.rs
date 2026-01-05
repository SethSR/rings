
use crate::cursor::{Cursor, Error};
use crate::discovery::{Value, ValueMap};
use crate::lexer::Data as LexData;
use crate::operators::{BinaryOp, UnaryOp};
use crate::token::{Id as TokenId, Kind as TKind};
use crate::Span;

type ValueResult = Result<Expr, Error>;

#[derive(Debug)]
pub enum Kind {
	Int(i64),
	Dec(f64),

	Unfinished,
}

pub struct Expr {
	pub kind: Kind,
	pub loc: Span<TokenId>,
}

pub fn value_expression(cursor: &mut Cursor,
	lex_data: &LexData,
	end_tokens: &[TKind],
	with_values: Option<&ValueMap>,
) -> ValueResult {
	value_expr_main(cursor, lex_data, 0, end_tokens, with_values)
}

fn value_expr_main(cursor: &mut Cursor,
	lex_data: &LexData,
	min_binding_power: usize, end_tokens: &[TKind],
	with_values: Option<&ValueMap>,
) -> ValueResult {
	let left = value_primary(cursor, lex_data, with_values)?;
	value_expr_sub(cursor, lex_data, min_binding_power, left, end_tokens,
		with_values)
}

fn reduce_binary_op(op: BinaryOp, left_kind: Kind, right_kind: Kind) -> Kind {
	match (left_kind, right_kind) {
		(Kind::Unfinished, _) | (_, Kind::Unfinished) => Kind::Unfinished,

		(Kind::Int(a), Kind::Int(b)) => match op {
			BinaryOp::Add => Kind::Int(a + b),
			BinaryOp::Sub => Kind::Int(a - b),
			BinaryOp::Mul => Kind::Int(a * b),
			BinaryOp::Div => Kind::Int(a / b),
			BinaryOp::Mod => Kind::Int(a % b),
			BinaryOp::BinAnd => Kind::Int(a & b),
			BinaryOp::BinOr  => Kind::Int(a | b),
			BinaryOp::BinXor => Kind::Int(a ^ b),
			BinaryOp::CmpEQ => Kind::Int((a == b) as i64),
			BinaryOp::CmpGE => Kind::Int((a >= b) as i64),
			BinaryOp::CmpGT => Kind::Int((a >  b) as i64),
			BinaryOp::CmpLE => Kind::Int((a <= b) as i64),
			BinaryOp::CmpLT => Kind::Int((a <  b) as i64),
			BinaryOp::CmpNE => Kind::Int((a != b) as i64),
			BinaryOp::LogAnd => Kind::Int((a != 0 && b != 0) as i64),
			BinaryOp::LogOr  => Kind::Int((a != 0 || b != 0) as i64),
			BinaryOp::LogXor => Kind::Int(((a != 0) ^ (b != 0)) as i64),
			BinaryOp::ShL => Kind::Int(a << b),
			BinaryOp::ShR => Kind::Int(a >> b),
		}

		(Kind::Dec(a), Kind::Dec(b)) => match op {
			BinaryOp::Add => Kind::Dec(a + b),
			BinaryOp::Sub => Kind::Dec(a - b),
			BinaryOp::Mul => Kind::Dec(a * b),
			BinaryOp::Div => Kind::Dec(a / b),
			BinaryOp::Mod => Kind::Dec(a % b),
			BinaryOp::CmpEQ => Kind::Int((a == b) as i64),
			BinaryOp::CmpGE => Kind::Int((a >= b) as i64),
			BinaryOp::CmpGT => Kind::Int((a >  b) as i64),
			BinaryOp::CmpLE => Kind::Int((a <= b) as i64),
			BinaryOp::CmpLT => Kind::Int((a <  b) as i64),
			BinaryOp::CmpNE => Kind::Int((a != b) as i64),
			BinaryOp::LogAnd => Kind::Int((a != 0. && b != 0.) as i64),
			BinaryOp::LogOr  => Kind::Int((a != 0. || b != 0.) as i64),
			BinaryOp::LogXor => Kind::Int(((a != 0.) ^ (b != 0.)) as i64),
			_ => Kind::Unfinished,
		}

		_ => Kind::Unfinished,
	}
}

fn value_expr_sub(cursor: &mut Cursor,
	lex_data: &LexData,
	min_binding_power: usize, left: Expr,
	end_tokens: &[TKind],
	with_values: Option<&ValueMap>,
) -> ValueResult {
	if end_tokens.contains(&cursor.current(lex_data)) {
		cursor.advance();
		return Ok(left);
	}

	let Ok(op) = cursor.expect_bin_op(lex_data) else {
		return Ok(left);
	};
	let op_binding_power = op.binding_power();

	let right = value_primary(cursor, lex_data, with_values)?;

	if min_binding_power <= op_binding_power {
		let right = value_expr_sub(cursor, lex_data, op_binding_power,
			right, end_tokens, with_values)?;

		let kind = reduce_binary_op(op, left.kind, right.kind);

		Ok(Expr { kind, loc: left.loc + right.loc })
	} else {
		Ok(right)
	}
}

fn reduce_unary_op(op: UnaryOp, kind: Kind) -> Kind {
	match (op, kind) {
		(_, Kind::Unfinished) => Kind::Unfinished,
		(UnaryOp::Neg, Kind::Int(num)) => Kind::Int(-num),
		(UnaryOp::Neg, Kind::Dec(num)) => Kind::Dec(-num),
		(UnaryOp::Not, Kind::Int(num)) => Kind::Int(!num),
		// This is technically an error, but it should be picked up by the Parser phase
		(UnaryOp::Not, Kind::Dec(_)) => Kind::Unfinished,
	}
}

fn value_primary(cursor: &mut Cursor,
	lex_data: &LexData,
	with_values: Option<&ValueMap>,
) -> ValueResult {
	let unary_op = cursor.expect_unary_op(lex_data);

	let tok_start = cursor.index();
	let mut kind = match cursor.current(lex_data) {
		TKind::Identifier(ident_id) => if let Some(values) = with_values {
			cursor.advance();
			match values.get(&ident_id) {
				Some(Value::Integer(num)) => Kind::Int(*num),
				Some(Value::Decimal(num)) => Kind::Dec(*num),
				None => Kind::Unfinished,
			}
		} else {
			cursor.advance();
			Kind::Unfinished
		}
		TKind::Integer(num) => { cursor.advance(); Kind::Int(num) }
		TKind::Decimal(num) => { cursor.advance(); Kind::Dec(num) }
		TKind::True => { cursor.advance(); Kind::Int(1) }
		TKind::False => { cursor.advance(); Kind::Int(0) }
		TKind::OParen => {
			cursor.advance();
			let expr = value_expression(cursor, lex_data, &[TKind::CParen], with_values)?;
			expr.kind
		}
		_ => return Err(cursor.expected_token("identifier or number")),
	};

	if let Some(op) = unary_op {
		kind = reduce_unary_op(op, kind);
	}

	Ok(Expr {
		kind,
		loc: (tok_start..cursor.index()).into(),
	})
}

