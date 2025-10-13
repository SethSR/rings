
use std::collections::hash_map::Entry;
use std::ops::Range;

use crate::token;
use crate::identifier::{self, Identifier};
use crate::{Data, SrcPos};

pub fn eval(data: &mut Data) {
	let Data {
		source,
		tok_list: kinds,
		tok_pos: starts,
		identifiers,
		..
	} = data;
	let mut lexer = Lexer {
		source,
		..Default::default()
	};

	lexer.skip_whitespace_and_comments();
	while lexer.next(kinds, starts, identifiers) {
		lexer.skip_whitespace_and_comments();
	}
}

#[derive(Default)]
struct Lexer<'a> {
	source: &'a str,
	pos: SrcPos,
}

impl<'a> Lexer<'a> {
	fn next(&mut self,
		kinds: &mut token::KindList,
		starts: &mut token::PosList,
		identifiers: &mut identifier::Map<Range<SrcPos>>,
	) -> bool {
		let start = self.pos;

		let kind = match self.peek(0) {
			None => {
				kinds.push(token::Kind::Eof);
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
					"region" => token::Kind::Region,
					"return" => token::Kind::Return,
					"record" => token::Kind::Record,
					"table" => token::Kind::Table,
					"proc" => token::Kind::Proc,
					"bool" => token::Kind::Bool,
					"true" => token::Kind::True,
					"false" => token::Kind::False,
					"at" => token::Kind::At,
					"in" => token::Kind::In,
					"if" => token::Kind::If,
					"else" => token::Kind::Else,
					"for" => token::Kind::For,
					"u8" => token::Kind::U8,
					"s8" => token::Kind::S8,
					"u16" => token::Kind::U16,
					"s16" => token::Kind::S16,
					"u32" => token::Kind::U32,
					"s32" => token::Kind::S32,

					text => {
						let ident_id = text.id();
						if let Entry::Vacant(e) = identifiers.entry(ident_id) {
							e.insert(start..self.pos);
						}
						token::Kind::Identifier(ident_id)
					}
				}
			}

			Some(c) if c.is_numeric() => {
				let mut inner_start = start;
				self.advance();

				#[derive(PartialEq)]
				enum NumType { Bin, Hex, Dec }

				let num_type = if c == '0' {
					match self.peek(0) {
						Some('b') => NumType::Bin,
						Some('x') => NumType::Hex,
						_ => NumType::Dec,
					}
				} else {
					NumType::Dec
				};

				if num_type != NumType::Dec {
					self.advance();
					inner_start = self.pos;
				}

				while let Some(c) = self.peek(0) {
					if !c.is_numeric() && c != '_' {
						break;
					}
					self.advance();
				}
				let mut is_fractional = false;
				if self.peek(0) == Some('.') {
					is_fractional = true;
					self.advance();
					while let Some(c) = self.peek(0) {
						if !c.is_numeric() && c != '_' {
							break;
						}
						self.advance();
					}
				}

				let text = &self.source[inner_start..self.pos];
				if is_fractional {
					match num_type {
						NumType::Bin => panic!("parsing binary fixed-point numbers not implemented"),
						NumType::Hex => panic!("parsing hexadecimal fixed-point numbers not implemented"),
						NumType::Dec => {
							let num = text.replace('_', "")
								.parse::<f64>()
								.expect("unable to parse decimal fixed-point number");
							token::Kind::Decimal(num)
						}
					}
				} else {
					let text = text.replace('_', "");
					let num = match num_type {
						NumType::Bin => i64::from_str_radix(&text, 2)
							.expect("unable to parse binary integer"),
						NumType::Hex => i64::from_str_radix(&text, 16)
							.expect("unable to parse hexadecimal integer"),
						NumType::Dec => text.parse::<i64>()
							.expect("unable to parse decimal integer"),
					};
					token::Kind::Integer(num)
				}
			}

			Some('-') => {
				self.advance();
				if self.expect('=') {
					token::Kind::DashEqual
				} else if self.expect('>') {
					token::Kind::Arrow
				} else {
					token::Kind::Dash
				}
			}

			Some(':') => {
				self.advance();
				if self.expect(':') {
					token::Kind::ColonColon
				} else if self.expect('=') {
					token::Kind::ColonEqual
				} else {
					token::Kind::Colon
				}
			}

			Some('+') => {
				self.advance();
				if self.expect('=') { token::Kind::PlusEqual } else { token::Kind::Plus }
			}

			Some('*') => {
				self.advance();
				if self.expect('=') { token::Kind::StarEqual } else { token::Kind::Star }
			}

			Some('/') => {
				self.advance();
				if self.expect('=') { token::Kind::SlashEqual } else { token::Kind::Slash }
			}

			Some('^') => {
				self.advance();
				if self.expect('=') { token::Kind::CarrotEqual } else { token::Kind::Carrot }
			}

			Some('=') => {
				self.advance();
				if self.expect('=') { token::Kind::EqualEqual } else { token::Kind::Equal }
			}

			Some('!') => {
				self.advance();
				if self.expect('=') { token::Kind::BangEqual } else { token::Kind::Eof }
			}

			Some('<') => {
				self.advance();
				if self.expect('=') { token::Kind::LessEqual } else { token::Kind::Less }
			}

			Some('>') => {
				self.advance();
				if self.expect('=') { token::Kind::GreaterEqual } else { token::Kind::Greater }
			}

			Some('(') => { self.advance(); token::Kind::OParen }

			Some(')') => { self.advance(); token::Kind::CParen }

			Some('{') => { self.advance(); token::Kind::OBrace }

			Some('}') => { self.advance(); token::Kind::CBrace }

			Some('[') => { self.advance(); token::Kind::OBracket }

			Some(']') => { self.advance(); token::Kind::CBracket }

			Some(';') => { self.advance(); token::Kind::Semicolon }

			Some(',') => { self.advance(); token::Kind::Comma }

			Some(_) => { self.advance(); token::Kind::Eof }
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

#[cfg(test)]
mod can_lex {
	use super::*;
	use super::token::Kind;

	fn setup(source: &str) -> Data {
		let mut data = Data::new(source.into());
		eval(&mut data);
		data
	}

	#[test]
	fn a_simple_program() {
		let data = setup("main { return 3; }");
		assert_eq!(data.tok_list, [
			Kind::Identifier("main".id()),
			Kind::OBrace,
			Kind::Return,
			Kind::Integer(3),
			Kind::Semicolon,
			Kind::CBrace,
			Kind::Eof,
		]);
		assert_eq!(data.tok_pos, [ 0, 5, 7, 14, 15, 17, 18 ]);
		assert_eq!(data.identifiers.len(), 1);
		assert_eq!(data.identifiers[&"main".id()], 0..4);
	}

	#[test]
	fn a_region_declaration() {
		let data = setup("wram_high :: region[2*1024^3] at 0x0020_0000;");
		assert_eq!(data.tok_list, [
			Kind::Identifier("wram_high".id()),
			Kind::ColonColon,
			Kind::Region,
			Kind::OBracket,
			Kind::Integer(2),
			Kind::Star,
			Kind::Integer(1024),
			Kind::Carrot,
			Kind::Integer(3),
			Kind::CBracket,
			Kind::At,
			Kind::Integer(0x0020_0000),
			Kind::Semicolon,
			Kind::Eof,
		]);
	}

	#[test]
	fn decimal_numbers() {
		let data = setup("5.6 4. 2_3.4_5");
		assert_eq!(data.tok_list, [
			Kind::Decimal(5.6),
			Kind::Decimal(4.),
			Kind::Decimal(23.45),
			Kind::Eof,
		]);
	}
}

