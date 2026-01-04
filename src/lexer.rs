
use std::collections::hash_map::Entry;

use crate::error::Error;
use crate::identifier::{Identifier, Map as IdentMap};
use crate::token::{Kind, KindList, PosList};
use crate::{Span, SrcPos};

pub fn eval(
	source: &str,
	identifiers: &mut IdentMap<Span<SrcPos>>,
	tok_list: &mut KindList,
	tok_pos: &mut PosList,
	line_pos: &mut PosList,
) -> Result<(), Error> {
	let mut lexer = Lexer { pos: 0 };

	// Add the initial line
	line_pos.push(lexer.pos);

	lexer.skip_whitespace_and_comments(source, line_pos);
	loop {
		match lexer.next(source, identifiers, tok_list, tok_pos) {
			Ok(true) => {}
			Ok(false) => return Ok(()), // all done!
			Err(e) => return Err(e),
		}

		lexer.skip_whitespace_and_comments(source, line_pos);
	}
}

#[derive(Default)]
struct Lexer {
	pos: SrcPos,
}

impl Lexer {
	fn next(&mut self,
		source: &str,
		identifiers: &mut IdentMap<Span<SrcPos>>,
		tok_list: &mut KindList,
		tok_pos: &mut PosList,
	) -> Result<bool, Error> {
		let start = self.pos;

		let kind = match self.peek(source, 0) {
			None => {
				tok_list.push(Kind::Eof);
				tok_pos.push(start);
				return Ok(false);
			}

			Some(c) if c.is_alphabetic() || c == '_' => {
				self.advance(source);
				while let Some(c) = self.peek(source, 0) {
					if !c.is_alphanumeric() && c != '_' {
						break;
					}
					self.advance(source);
				}
				match &source[start..self.pos] {
					// Top Level Stmts
					"index" => Kind::Index,
					"overlay" => Kind::Overlay,
					"proc" => Kind::Proc,
					"record" => Kind::Record,
					"region" => Kind::Region,
					"table" => Kind::Table,
					"value" => Kind::Value,

					// Types
					"bool" => Kind::Bool,
					"s16" => Kind::S16,
					"s32" => Kind::S32,
					"s8" => Kind::S8,
					"u16" => Kind::U16,
					"u32" => Kind::U32,
					"u8" => Kind::U8,

					// Keywords
					"else" => Kind::Else,
					"false" => Kind::False,
					"for" => Kind::For,
					"free" => Kind::Free,
					"if" => Kind::If,
					"in" => Kind::In,
					"let" => Kind::Let,
					"m68k" => Kind::M68k,
					"mark" => Kind::Mark,
					"return" => Kind::Return,
					"sh2" => Kind::SH2,
					"true" => Kind::True,
					"where" => Kind::Where,
					"while" => Kind::While,
					"x64" => Kind::X64,
					"z80" => Kind::Z80,

					text => {
						let ident_id = text.id();
						if let Entry::Vacant(e) = identifiers.entry(ident_id) {
							e.insert((start..self.pos).into());
						}
						match text {
							"main" => Kind::Main,
							"sub" => Kind::Sub,
							_ => Kind::Identifier(ident_id),
						}
					}
				}
			}

			Some(c) if c.is_numeric() => {
				let mut inner_start = start;
				self.advance(source);

				#[derive(PartialEq)]
				enum NumType { Bin, Hex, Dec }

				let num_type = if c == '0' {
					match self.peek(source, 0) {
						Some('b') => NumType::Bin,
						Some('x') => NumType::Hex,
						_ => NumType::Dec,
					}
				} else {
					NumType::Dec
				};

				if num_type != NumType::Dec {
					self.advance(source);
					inner_start = self.pos;
				}

				while let Some(c) = self.peek(source, 0) {
					let valid = match num_type {
						NumType::Bin => ['0', '1', '_'].contains(&c),
						NumType::Hex => c.is_ascii_hexdigit() || c == '_',
						NumType::Dec => c.is_ascii_digit() || c == '_',
					};
					if !valid { break; }
					self.advance(source);
				}
				let mut is_fractional = false;
				if self.peek(source, 0) == Some('.') && self.peek(source, 1) != Some('.') {

					is_fractional = true;
					self.advance(source);
					while let Some(c) = self.peek(source, 0) {
						if !c.is_numeric() && c != '_' {
							break;
						}
						self.advance(source);
					}
				}

				let text = &source[inner_start..self.pos];
				let tok_src = (inner_start..self.pos).into();
				if is_fractional {
					match num_type {
						NumType::Bin => return Err(Error::new(tok_src,
							"parsing binary fixed-point numbers not implemented yet")),
						NumType::Hex => return Err(Error::new(tok_src,
							"parsing hexadecimal fixed-point numbers not implemented yet")),
						NumType::Dec => text.replace('_', "").parse::<f64>()
							.map_err(|_| Error::new(tok_src,
								"unable to parse decimal fixed-point number"))
							.map(Kind::Decimal)?,
					}
				} else {
					let text = text.replace('_', "");
					let num = match num_type {
						NumType::Bin => i64::from_str_radix(&text, 2).map_err(|_| Error::new(tok_src,
							"unable to parse binary integer"))?,
						NumType::Hex => i64::from_str_radix(&text, 16).map_err(|_| Error::new(tok_src,
							"unable to parse hexadecimal integer"))?,
						NumType::Dec => text.parse::<i64>().map_err(|_| Error::new(tok_src,
							"unable to parse decimal integer"))?,
					};
					Kind::Integer(num)
				}
			}

			Some('-') => {
				self.advance(source);
				self.item(source, &[
					('=', Kind::DashEq),
					('>', Kind::Arrow),
				], Kind::Dash)
			}

			Some(':') => {
				self.advance(source);
				self.item(source, &[
					(':', Kind::Colon2),
					('=', Kind::ColonEq),
				], Kind::Colon)
			}

			Some('<') => {
				self.advance(source);
				self.item(source, &[
					('<', Kind::LArr2),
					('=', Kind::LArrEq),
				], Kind::LArr)
			}

			Some('>') => {
				self.advance(source);
				self.item(source, &[
					('>', Kind::RArr2),
					('=', Kind::RArrEq),
				], Kind::RArr)
			}

			Some('^') => {
				self.advance(source);
				self.item(source, &[
					('^', Kind::Carrot2),
					('=', Kind::CarrotEq),
				], Kind::Carrot)
			}

			Some('+') => {
				self.advance(source);
				self.item(source, &[('=', Kind::PlusEq)], Kind::Plus)
			}

			Some('*') => {
				self.advance(source);
				self.item(source, &[('=', Kind::StarEq)], Kind::Star)
			}

			Some('/') => {
				self.advance(source);
				self.item(source, &[('=', Kind::SlashEq)], Kind::Slash)
			}

			Some('%') => {
				self.advance(source);
				self.item(source, &[('=', Kind::PercentEq)], Kind::Percent)
			}

			Some('=') => {
				self.advance(source);
				self.item(source, &[('=', Kind::Eq2)], Kind::Eq)
			}

			Some('!') => {
				self.advance(source);
				self.item(source, &[('=', Kind::BangEq)], Kind::Bang)
			}

			Some('.') => {
				self.advance(source);
				self.item(source, &[('.', Kind::Dot2)], Kind::Dot)
			}

			Some('&') => {
				self.advance(source);
				self.item(source, &[('&', Kind::Amp2)], Kind::Amp)
			}

			Some('|') => {
				self.advance(source);
				self.item(source, &[('|', Kind::Bar2)], Kind::Bar)
			}

			Some('(') => { self.advance(source); Kind::OParen }

			Some(')') => { self.advance(source); Kind::CParen }

			Some('{') => { self.advance(source); Kind::OBrace }

			Some('}') => { self.advance(source); Kind::CBrace }

			Some('[') => { self.advance(source); Kind::OBracket }

			Some(']') => { self.advance(source); Kind::CBracket }

			Some('@') => { self.advance(source); Kind::At }

			Some(';') => { self.advance(source); Kind::Semicolon }

			Some(',') => { self.advance(source); Kind::Comma }

			Some(_) => { self.advance(source); Kind::Eof }
		};

		tok_list.push(kind);
		tok_pos.push(start);
		Ok(true)
	}

	fn skip_whitespace_and_comments(&mut self,
		source: &str,
		line_pos: &mut PosList,
	) {
		loop {
			match self.peek(source, 0) {
				Some('\n') => {
					line_pos.push(self.pos);
					self.advance(source);
				}
				Some(c) if c.is_whitespace() => {
					self.advance(source);
				}
				Some('-') if self.peek(source, 1) == Some('-') => {
					self.advance(source);
					self.advance(source);
					while let Some(c) = self.peek(source, 0) {
						if c == '\n' {
							break;
						}
						self.advance(source);
					}
				}
				_ => break,
			}
		}
	}

	fn item(&mut self,
		source: &str,
		pairs: &[(char, Kind)],
		end: Kind,
	) -> Kind {
		for &(ch, kind) in pairs {
			if self.expect(source, ch) {
				return kind;
			}
		}
		end
	}

	fn peek(&self,
		source: &str,
		offset: usize,
	) -> Option<char> {
		source[self.pos..].chars().nth(offset)
	}

	fn expect(&mut self, source: &str, ch: char) -> bool {
		if self.peek(source, 0) == Some(ch) {
			self.advance(source);
			true
		} else {
			false
		}
	}

	fn advance(&mut self, source: &str) {
		if let Some(c) = self.peek(source, 0) {
			self.pos += c.len_utf8();
		}
	}
}

#[cfg(test)]
mod can_lex {
	use crate::error;
	use crate::Data;

	use super::*;

	fn setup(source: &str) -> Data {
		let mut data = Data::new(file!().to_string(), source.into());
		data.DEBUG_show_tokens = true;
		eval(
			&data.source,
			&mut data.identifiers,
			&mut data.tok_list,
			&mut data.tok_pos,
			&mut data.line_pos,
		).map_err(|e| e.with_kind(error::Kind::Lexer)
				.display(&data.source_file, &data.source, &data.line_pos))
			.map(|_| data)
			.unwrap_or_else(|msg| panic!("{msg}"))
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

