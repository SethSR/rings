
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::ops::Range;

use crate::{Data, Identifier, TokenKind};

pub fn eval(data: &mut Data) {
	let Data {
		source,
		tok_list: kinds,
		tok_pos: starts,
		identifiers,
		..
	} = data;
	let mut lexer = Lexer { source, pos: 0 };
	lexer.skip_whitespace_and_comments();
	while lexer.next(kinds, starts, identifiers) {
		lexer.skip_whitespace_and_comments();
	}
}

#[derive(Default)]
struct Lexer<'a> {
	source: &'a str,
	pos: usize
}

impl<'a> Lexer<'a> {
	fn next(&mut self,
		kinds: &mut Vec<TokenKind>,
		starts: &mut Vec<usize>,
		identifiers: &mut HashMap<u64, Range<usize>>,
	) -> bool {
		let start = self.pos;

		let kind = match self.peek(0) {
			None => {
				kinds.push(TokenKind::Eof);
				starts.push(start);
				return false;
			}

			Some(c) if c.is_alphabetic() || c == '_' => {
				self.advance();
				while let Some(c) = self.peek(0) {
					if !c.is_alphanumeric() && c != '_' {
						break;
					}
					self.advance();
				}
				match &self.source[start..self.pos] {
					"return" => TokenKind::Return,
					"record" => TokenKind::Record,
					"table" => TokenKind::Table,
					"proc" => TokenKind::Proc,
					"bool" => TokenKind::Bool,
					"true" => TokenKind::True,
					"false" => TokenKind::False,
					"in" => TokenKind::In,
					"if" => TokenKind::If,
					"else" => TokenKind::Else,
					"for" => TokenKind::For,
					"u8" => TokenKind::U8,
					"s8" => TokenKind::S8,
					"u16" => TokenKind::U16,
					"s16" => TokenKind::S16,
					"u32" => TokenKind::U32,
					"s32" => TokenKind::S32,

					_ => {
						let ident_id = self.source[start..self.pos].id();
						if let Entry::Vacant(e) = identifiers.entry(ident_id) {
							e.insert(start..self.pos);
						}
						TokenKind::Identifier(ident_id)
					}
				}
			}

			Some(c) if c.is_numeric() => {
				self.advance();
				while let Some(c) = self.peek(0) {
					if !c.is_numeric() && c != '_' {
						break;
					}
					self.advance();
				}
				let mut is_decimal = false;
				if self.peek(0) == Some('.') {
					is_decimal = true;
					self.advance();
					while let Some(c) = self.peek(0) {
						if !c.is_numeric() && c != '_' {
							break;
						}
						self.advance();
					}
				}
				let text = &self.source[start..self.pos];
				if is_decimal {
					let num = text.replace('_', "")
						.parse::<f64>()
						.expect("unable to parse decimal");
					TokenKind::Decimal(num)
				} else {
					let num = text.replace('_', "")
						.parse::<i64>()
						.expect("unable to parse integer");
					TokenKind::Integer(num)
				}
			}

			Some('-') => {
				self.advance();
				if self.expect('=') {
					TokenKind::DashEqual
				} else if self.expect('>') {
					TokenKind::Arrow
				} else {
					TokenKind::Dash
				}
			}

			Some(':') => {
				self.advance();
				if self.expect(':') {
					TokenKind::ColonColon
				} else if self.expect('=') {
					TokenKind::ColonEqual
				} else {
					TokenKind::Colon
				}
			}

			Some('+') => {
				self.advance();
				if self.expect('=') { TokenKind::PlusEqual } else { TokenKind::Plus }
			}

			Some('*') => {
				self.advance();
				if self.expect('=') { TokenKind::StarEqual } else { TokenKind::Star }
			}

			Some('/') => {
				self.advance();
				if self.expect('=') { TokenKind::SlashEqual } else { TokenKind::Slash }
			}

			Some('=') => {
				self.advance();
				if self.expect('=') { TokenKind::EqualEqual } else { TokenKind::Equal }
			}

			Some('!') => {
				self.advance();
				if self.expect('=') { TokenKind::BangEqual } else { TokenKind::Eof }
			}

			Some('<') => {
				self.advance();
				if self.expect('=') { TokenKind::LessEqual } else { TokenKind::Less }
			}

			Some('>') => {
				self.advance();
				if self.expect('=') { TokenKind::GreaterEqual } else { TokenKind::Greater }
			}

			Some('(') => { self.advance(); TokenKind::OParen }

			Some(')') => { self.advance(); TokenKind::CParen }

			Some('{') => { self.advance(); TokenKind::OBrace }

			Some('}') => { self.advance(); TokenKind::CBrace }

			Some('[') => { self.advance(); TokenKind::OBracket }

			Some(']') => { self.advance(); TokenKind::CBracket }

			Some(';') => { self.advance(); TokenKind::Semicolon }

			Some(',') => { self.advance(); TokenKind::Comma }

			Some('@') => { self.advance(); TokenKind::At }

			Some(_) => { self.advance(); TokenKind::Eof }
		};

		kinds.push(kind);
		starts.push(start);
		true
	}

	fn skip_whitespace_and_comments(&mut self) {
		loop {
			match self.peek(0) {
				Some(c) if c.is_whitespace() => {
					self.advance();
				}
				Some('-') if self.peek(1) == Some('-') => {
					self.advance();
					self.advance();
					while let Some(c) = self.peek(0) {
						if c == '\n' {
							break;
						}
						self.advance();
					}
				}
				_ => break,
			}
		}
	}

	fn peek(&self, offset: usize) -> Option<char> {
		self.source[self.pos..].chars().nth(offset)
	}

	fn expect(&mut self, ch: char) -> bool {
		if self.peek(0) == Some(ch) {
			self.advance();
			true
		} else {
			false
		}
	}

	fn advance(&mut self) {
		if let Some(c) = self.peek(0) {
			self.pos += c.len_utf8();
		}
	}
}

#[test]
fn can_lex_a_simple_program() {
	let source = "main { return 3; }";
	let mut data = Data::new(source.into());
	eval(&mut data);

	let hash_main = "main".id();

	assert_eq!(data.tok_list, [
		TokenKind::Identifier(hash_main),
		TokenKind::OBrace,
		TokenKind::Return,
		TokenKind::Integer(3),
		TokenKind::Semicolon,
		TokenKind::CBrace,
		TokenKind::Eof,
	]);
	assert_eq!(data.tok_pos, [ 0, 5, 7, 14, 15, 17, 18 ]);
	assert_eq!(data.identifiers.len(), 1);
	assert_eq!(data.identifiers[&hash_main], 0..4);
}

#[test]
fn can_lex_decimal_numbers() {
	let source = "5.6 4. 2_3.4_5";
	let mut data = Data::new(source.into());
	eval(&mut data);

	assert_eq!(data.tok_list, [
		TokenKind::Decimal(5.6),
		TokenKind::Decimal(4.),
		TokenKind::Decimal(23.45),
		TokenKind::Eof,
	]);
}

