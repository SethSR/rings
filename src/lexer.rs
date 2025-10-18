
use std::collections::hash_map::Entry;

use crate::token;
use crate::identifier::Identifier;
use crate::{Data, SrcPos};

pub fn eval(data: &mut Data) {
	let mut lexer = Lexer { pos: 0 };

	lexer.skip_whitespace_and_comments(data);
	while lexer.next(data) {
		lexer.skip_whitespace_and_comments(data);
	}
}

macro_rules! parse_error {
	($data:expr, $msg:expr) => {{
		crate::error::error($data, $msg, crate::token::Id::new($data.tok_list.len()));
		return false;
	}}
}

#[derive(Default)]
struct Lexer {
	pos: SrcPos,
}

impl Lexer {
	fn next(&mut self, data: &mut Data) -> bool {
		let start = self.pos;

		let kind = match self.peek(data, 0) {
			None => {
				data.tok_list.push(token::Kind::Eof);
				data.tok_pos.push(start);
				return false;
			}

			Some(c) if c.is_alphabetic() || c == '_' => {
				self.advance(data);
				while let Some(c) = self.peek(data, 0) {
					if !c.is_alphanumeric() && c != '_' {
						break;
					}
					self.advance(data);
				}
				match &data.source[start..self.pos] {
					"region" => token::Kind::Region,
					"return" => token::Kind::Return,
					"record" => token::Kind::Record,
					"table" => token::Kind::Table,
					"index" => token::Kind::Index,
					"proc" => token::Kind::Proc,
					"bool" => token::Kind::Bool,
					"true" => token::Kind::True,
					"false" => token::Kind::False,
					"at" => token::Kind::At,
					"in" => token::Kind::In,
					"if" => token::Kind::If,
					"else" => token::Kind::Else,
					"for" => token::Kind::For,
					"where" => token::Kind::Where,
					"u8" => token::Kind::U8,
					"s8" => token::Kind::S8,
					"u16" => token::Kind::U16,
					"s16" => token::Kind::S16,
					"u32" => token::Kind::U32,
					"s32" => token::Kind::S32,

					text => {
						let ident_id = text.id();
						if let Entry::Vacant(e) = data.identifiers.entry(ident_id) {
							e.insert(start..self.pos);
						}
						token::Kind::Identifier(ident_id)
					}
				}
			}

			Some(c) if c.is_numeric() => {
				let mut inner_start = start;
				self.advance(data);

				#[derive(PartialEq)]
				enum NumType { Bin, Hex, Dec }

				let num_type = if c == '0' {
					match self.peek(data, 0) {
						Some('b') => NumType::Bin,
						Some('x') => NumType::Hex,
						_ => NumType::Dec,
					}
				} else {
					NumType::Dec
				};

				if num_type != NumType::Dec {
					self.advance(data);
					inner_start = self.pos;
				}

				while let Some(c) = self.peek(data, 0) {
					if !c.is_numeric() && c != '_' {
						break;
					}
					self.advance(data);
				}
				let mut is_fractional = false;
				if self.peek(data, 0) == Some('.') {
					is_fractional = true;
					self.advance(data);
					while let Some(c) = self.peek(data, 0) {
						if !c.is_numeric() && c != '_' {
							break;
						}
						self.advance(data);
					}
				}

				let text = &data.source[inner_start..self.pos];
				if is_fractional {
					match num_type {
						NumType::Bin => parse_error!(data,
							"parsing binary fixed-point numbers not implemented yet"),
						NumType::Hex => parse_error!(data,
							"parsing hexadecimal fixed-point numbers not implemented yet"),
						NumType::Dec => {
							match text.replace('_', "").parse::<f64>() {
								Ok(num) => token::Kind::Decimal(num),
								Err(_) => parse_error!(data, "unable to parse decimal fixed-point number"),
							}
						}
					}
				} else {
					let text = text.replace('_', "");
					let num = match num_type {
						NumType::Bin => match i64::from_str_radix(&text, 2) {
							Ok(num) => num,
							Err(_) => parse_error!(data, "unable to parse binary integer"),
						},
						NumType::Hex => match i64::from_str_radix(&text, 16) {
							Ok(num) => num,
							Err(_) => parse_error!(data, "unable to parse hexadecimal integer"),
						},
						NumType::Dec => match text.parse::<i64>() {
							Ok(num) => num,
							Err(_) => parse_error!(data, "unable to parse decimal integer"),
						},
					};
					token::Kind::Integer(num)
				}
			}

			Some('-') => {
				self.advance(data);
				if self.expect(data, '=') {
					token::Kind::DashEqual
				} else if self.expect(data, '>') {
					token::Kind::Arrow
				} else {
					token::Kind::Dash
				}
			}

			Some(':') => {
				self.advance(data);
				if self.expect(data, ':') {
					token::Kind::ColonColon
				} else if self.expect(data, '=') {
					token::Kind::ColonEqual
				} else {
					token::Kind::Colon
				}
			}

			Some('<') => {
				self.advance(data);
				if self.expect(data, '<') {
					token::Kind::LessLess
				} else if self.expect(data, '=') {
					token::Kind::LessEqual
				} else {
					token::Kind::Less
				}
			}

			Some('>') => {
				self.advance(data);
				if self.expect(data, '>') {
					token::Kind::GreaterGreater
				} else if self.expect(data, '=') {
					token::Kind::GreaterEqual
				} else {
					token::Kind::Greater
				}
			}

			Some('^') => {
				self.advance(data);
				if self.expect(data, '^') {
					token::Kind::CarrotCarrot
				} else if self.expect(data, '=') {
					token::Kind::CarrotEqual
				} else {
					token::Kind::Carrot
				}
			}

			Some('+') => {
				self.advance(data);
				if self.expect(data, '=') { token::Kind::PlusEqual } else { token::Kind::Plus }
			}

			Some('*') => {
				self.advance(data);
				if self.expect(data, '=') { token::Kind::StarEqual } else { token::Kind::Star }
			}

			Some('/') => {
				self.advance(data);
				if self.expect(data, '=') { token::Kind::SlashEqual } else { token::Kind::Slash }
			}

			Some('%') => {
				self.advance(data);
				if self.expect(data, '=') { token::Kind::PercentEqual } else { token::Kind::Percent }
			}

			Some('=') => {
				self.advance(data);
				if self.expect(data, '=') { token::Kind::EqualEqual } else { token::Kind::Equal }
			}

			Some('!') => {
				self.advance(data);
				if self.expect(data, '=') { token::Kind::BangEqual } else { token::Kind::Eof }
			}

			Some('.') => {
				self.advance(data);
				if self.expect(data, '.') { token::Kind::DotDot } else { token::Kind::Dot }
			}

			Some('&') => {
				self.advance(data);
				if self.expect(data, '&') { token::Kind::AmpAmp } else { token::Kind::Amp }
			}

			Some('|') => {
				self.advance(data);
				if self.expect(data, '|') { token::Kind::BarBar } else { token::Kind::Bar }
			}

			Some('(') => { self.advance(data); token::Kind::OParen }

			Some(')') => { self.advance(data); token::Kind::CParen }

			Some('{') => { self.advance(data); token::Kind::OBrace }

			Some('}') => { self.advance(data); token::Kind::CBrace }

			Some('[') => { self.advance(data); token::Kind::OBracket }

			Some(']') => { self.advance(data); token::Kind::CBracket }

			Some(';') => { self.advance(data); token::Kind::Semicolon }

			Some(',') => { self.advance(data); token::Kind::Comma }

			Some(_) => { self.advance(data); token::Kind::Eof }
		};

		data.tok_list.push(kind);
		data.tok_pos.push(start);
		true
	}

	fn skip_whitespace_and_comments(&mut self, data: &mut Data) {
		loop {
			match self.peek(data, 0) {
				Some('\n') => {
					data.line_pos.push(self.pos);
					self.advance(data);
				}
				Some(c) if c.is_whitespace() => {
					self.advance(data);
				}
				Some('-') if self.peek(data, 1) == Some('-') => {
					self.advance(data);
					self.advance(data);
					while let Some(c) = self.peek(data, 0) {
						if c == '\n' {
							break;
						}
						self.advance(data);
					}
				}
				_ => break,
			}
		}
	}

	fn peek(&self, data: &Data, offset: usize) -> Option<char> {
		data.source[self.pos..].chars().nth(offset)
	}

	fn expect(&mut self, data: &Data, ch: char) -> bool {
		if self.peek(data, 0) == Some(ch) {
			self.advance(data);
			true
		} else {
			false
		}
	}

	fn advance(&mut self, data: &Data) {
		if let Some(c) = self.peek(data, 0) {
			self.pos += c.len_utf8();
		}
	}
}

#[cfg(test)]
mod can_lex {
	use super::*;
	use super::token::Kind;

	fn setup(source: &str) -> Data {
		let mut data = Data::new(file!().to_string(), source.into());
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

