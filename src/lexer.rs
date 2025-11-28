
use std::collections::hash_map::Entry;

use crate::error::{self, Error};
use crate::identifier::Identifier;
use crate::token;
use crate::{Data, SrcPos};

pub fn eval(data: &mut Data) {
	let mut lexer = Lexer { pos: 0 };

	// Add the initial line
	data.line_pos.push(lexer.pos);

	lexer.skip_whitespace_and_comments(data);
	loop {
		match lexer.next(data) {
			Ok(true) => {}
			Ok(false) => return, // all done!
			Err(mut e) => {
				e.set_kind(error::Kind::Lexer);
				data.errors.push(e);
				return;
			}
		}

		lexer.skip_whitespace_and_comments(data);
	}
}

#[derive(Default)]
struct Lexer {
	pos: SrcPos,
}

impl Lexer {
	fn next(&mut self, data: &mut Data) -> Result<bool, Error> {
		let start = self.pos;

		let kind = match self.peek(data, 0) {
			None => {
				data.tok_list.push(token::Kind::Eof);
				data.tok_pos.push(start);
				return Ok(false);
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
					// Top Level Stmts
					"index" => token::Kind::Index,
					"overlay" => token::Kind::Overlay,
					"proc" => token::Kind::Proc,
					"record" => token::Kind::Record,
					"region" => token::Kind::Region,
					"table" => token::Kind::Table,
					"value" => token::Kind::Value,

					// Types
					"bool" => token::Kind::Bool,
					"s16" => token::Kind::S16,
					"s32" => token::Kind::S32,
					"s8" => token::Kind::S8,
					"u16" => token::Kind::U16,
					"u32" => token::Kind::U32,
					"u8" => token::Kind::U8,

					// Keywords
					"else" => token::Kind::Else,
					"false" => token::Kind::False,
					"for" => token::Kind::For,
					"if" => token::Kind::If,
					"in" => token::Kind::In,
					"let" => token::Kind::Let,
					"return" => token::Kind::Return,
					"true" => token::Kind::True,
					"where" => token::Kind::Where,
					"while" => token::Kind::While,

					text => {
						let ident_id = text.id();
						if let Entry::Vacant(e) = data.identifiers.entry(ident_id) {
							e.insert((start..self.pos).into());
						}
						match text {
							"main" => token::Kind::Main,
							"sub" => token::Kind::Sub,
							_ => token::Kind::Identifier(ident_id),
						}
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
				if self.peek(data, 0) == Some('.') && self.peek(data, 1) != Some('.') {

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
						NumType::Bin => return Err(error::error(data,
							"parsing binary fixed-point numbers not implemented yet",
							self.pos.into())),
						NumType::Hex => return Err(error::error(data,
							"parsing hexadecimal fixed-point numbers not implemented yet",
							self.pos.into())),
						NumType::Dec => {
							match text.replace('_', "").parse::<f64>() {
								Ok(num) => token::Kind::Decimal(num),
								Err(_) => return Err(error::error(data,
									"unable to parse decimal fixed-point number",
									self.pos.into())),
							}
						}
					}
				} else {
					let text = text.replace('_', "");
					let num = match num_type {
						NumType::Bin => match i64::from_str_radix(&text, 2) {
							Ok(num) => num,
							Err(_) => return Err(error::error(data,
								"unable to parse binary integer",
								self.pos.into())),
						},
						NumType::Hex => match i64::from_str_radix(&text, 16) {
							Ok(num) => num,
							Err(_) => return Err(error::error(data,
								"unable to parse hexadecimal integer",
								self.pos.into())),
						},
						NumType::Dec => match text.parse::<i64>() {
							Ok(num) => num,
							Err(_) => return Err(error::error(data,
								"unable to parse decimal integer",
								self.pos.into())),
						},
					};
					token::Kind::Integer(num)
				}
			}

			Some('-') => {
				self.advance(data);
				self.item(data, &[
					('=', token::Kind::DashEq),
					('>', token::Kind::Arrow),
				], token::Kind::Dash)
			}

			Some(':') => {
				self.advance(data);
				self.item(data, &[
					(':', token::Kind::Colon2),
					('=', token::Kind::ColonEq),
				], token::Kind::Colon)
			}

			Some('<') => {
				self.advance(data);
				self.item(data, &[
					('<', token::Kind::LArr2),
					('=', token::Kind::LArrEq),
				], token::Kind::LArr)
			}

			Some('>') => {
				self.advance(data);
				self.item(data, &[
					('>', token::Kind::RArr2),
					('=', token::Kind::RArrEq),
				], token::Kind::RArr)
			}

			Some('^') => {
				self.advance(data);
				self.item(data, &[
					('^', token::Kind::Carrot2),
					('=', token::Kind::CarrotEq),
				], token::Kind::Carrot)
			}

			Some('+') => {
				self.advance(data);
				self.item(data, &[('=', token::Kind::PlusEq)], token::Kind::Plus)
			}

			Some('*') => {
				self.advance(data);
				self.item(data, &[('=', token::Kind::StarEq)], token::Kind::Star)
			}

			Some('/') => {
				self.advance(data);
				self.item(data, &[('=', token::Kind::SlashEq)], token::Kind::Slash)
			}

			Some('%') => {
				self.advance(data);
				self.item(data, &[('=', token::Kind::PercentEq)], token::Kind::Percent)
			}

			Some('=') => {
				self.advance(data);
				self.item(data, &[('=', token::Kind::Eq2)], token::Kind::Eq)
			}

			Some('!') => {
				self.advance(data);
				self.item(data, &[('=', token::Kind::BangEq)], token::Kind::Bang)
			}

			Some('.') => {
				self.advance(data);
				self.item(data, &[('.', token::Kind::Dot2)], token::Kind::Dot)
			}

			Some('&') => {
				self.advance(data);
				self.item(data, &[('&', token::Kind::Amp2)], token::Kind::Amp)
			}

			Some('|') => {
				self.advance(data);
				self.item(data, &[('|', token::Kind::Bar2)], token::Kind::Bar)
			}

			Some('(') => { self.advance(data); token::Kind::OParen }

			Some(')') => { self.advance(data); token::Kind::CParen }

			Some('{') => { self.advance(data); token::Kind::OBrace }

			Some('}') => { self.advance(data); token::Kind::CBrace }

			Some('[') => { self.advance(data); token::Kind::OBracket }

			Some(']') => { self.advance(data); token::Kind::CBracket }

			Some('@') => { self.advance(data); token::Kind::At }

			Some(';') => { self.advance(data); token::Kind::Semicolon }

			Some(',') => { self.advance(data); token::Kind::Comma }

			Some(_) => { self.advance(data); token::Kind::Eof }
		};

		data.tok_list.push(kind);
		data.tok_pos.push(start);
		Ok(true)
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

	fn item(&mut self, db: &Data,
		pairs: &[(char, token::Kind)],
		end: token::Kind,
	) -> token::Kind {
		for &(ch, kind) in pairs {
			if self.expect(db, ch) {
				return kind;
			}
		}
		end
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
		data.DEBUG_show_tokens = true;
		eval(&mut data);
		data
	}

	#[test]
	fn a_simple_program() {
		let data = setup("main { return 3; }");
		assert_eq!(data.tok_list, [
			Kind::Main,
			Kind::OBrace,
			Kind::Return,
			Kind::Integer(3),
			Kind::Semicolon,
			Kind::CBrace,
			Kind::Eof,
		]);
		assert_eq!(data.tok_pos, [ 0, 5, 7, 14, 15, 17, 18 ]);
	}

	#[test]
	fn a_region_declaration() {
		let data = setup("wram_high :: region[2*1024^3] @ 0x0020_0000;");
		assert_eq!(data.tok_list, [
			Kind::Identifier("wram_high".id()),
			Kind::Colon2,
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

