
use crate::{Data, TokenKind, ValueKind};

pub fn eval(
	data: &mut Data,
) {
	let mut pos = 0;

	loop {
		let kind = data.tok_list[pos];
		let start = pos;
		pos += 1;

		if let TokenKind::Identifier(ident_id) = kind {
			if &data.source[data.identifiers[ident_id].clone()] == "main" {
				expect(data, &mut pos, TokenKind::OBrace);

				let mut brace_count = 1;
				while brace_count > 0 && data.tok_list[pos] != TokenKind::Eof {
					brace_count += match data.tok_list[pos] {
						TokenKind::OBrace => 1,
						TokenKind::CBrace => -1,
						_ => 0,
					};
					pos += 1;
				}

				data.proc_start.push(start);
			} else if let TokenKind::Integer(value) = data.tok_list[pos + 1] {
				expect(data, &mut pos, TokenKind::ColonColon);
				pos += 1; // increment past the integer, as we already have it
				expect(data, &mut pos, TokenKind::Semicolon);
				data.values.insert(ident_id, ValueKind::Integer(value));
			} else if let TokenKind::Decimal(value) = data.tok_list[pos + 1] {
				expect(data, &mut pos, TokenKind::ColonColon);
				pos += 1; // increment past the decimal, as we already have it
				expect(data, &mut pos, TokenKind::Semicolon);
				data.values.insert(ident_id, ValueKind::Decimal(value));
			} else if let TokenKind::Proc = data.tok_list[pos + 1] {
				expect(data, &mut pos, TokenKind::ColonColon);
				expect(data, &mut pos, TokenKind::Proc);
				expect(data, &mut pos, TokenKind::OParen);
				// TODO - srenshaw - Handle parameter lists in function declarations
				expect(data, &mut pos, TokenKind::CParen);
				// TODO - srenshaw - Handle return type declarations
				expect(data, &mut pos, TokenKind::OBrace);

				let mut brace_count = 1;
				while brace_count > 0 && data.tok_list[pos] != TokenKind::Eof {
					brace_count += match data.tok_list[pos] {
						TokenKind::OBrace => 1,
						TokenKind::CBrace => -1,
						_ => 0,
					};
					pos += 1;
				}

				data.proc_start.push(start);
			} else if data.tok_list[pos + 1] == TokenKind::Record {
				todo!("handle record declaration");
			} else if data.tok_list[pos + 1] == TokenKind::Table {
				todo!("handle table declaration");
			}
		} else if TokenKind::Eof == kind {
			break;
		} else {
			data.error = format!("Expected identifier or EOF, found {kind:?}");
			return;
		}
	}
}

fn expect(data: &mut Data, pos: &mut usize, kind: TokenKind) {
	let token_kind = data.tok_list[*pos];
	if token_kind != kind {
		data.error = format!("Expected {kind:?}, found {token_kind:?}");
		panic!("{data}");
	}
	*pos += 1;
}

#[test]
fn can_parse_constant_values() {
	let source = "
a :: 3;
b :: 4.2;
";
	let mut data = Data::new(source.to_string());
	crate::lexer::eval(&mut data);
	eval(&mut data);

	assert_eq!(data.values.len(), 2);
	assert_eq!(data.values[&0], ValueKind::Integer(3), "{data}");
	assert_eq!(data.values[&1], ValueKind::Decimal(4.2), "{data}");
}

#[test]
fn can_parse_procedure_locations() {
	let source = "
main {}
a :: proc() {}
";
	let mut data = Data::new(source.to_string());
	crate::lexer::eval(&mut data);
	eval(&mut data);

	assert_eq!(data.proc_start.len(), 2);
	assert_eq!(data.proc_start[0], 0, "{data}");
	assert_eq!(data.proc_start[1], 3, "{data}");
}

