
use std::collections::hash_map::Entry;

use crate::error::{Error, Kind as ErrorKind};
use crate::identifier::{Identifier, Map as IdentMap};
use crate::token::{Kind, KindList, PosList};
use crate::{identifier, input, Span, SrcPos};

pub fn eval(source: &str) -> Result<Data, Error> {
	let mut lexer = Lexer::default();

	lexer.skip_whitespace_and_comments(source);
	loop {
		match lexer.next(source) {
			Ok(true) => {}
			Ok(false) => return Ok(lexer.out), // all done!
			Err(e) => return Err(e.with_kind(ErrorKind::Lexer)),
		}

		lexer.skip_whitespace_and_comments(source);
	}
}

#[derive(Debug, Default)]
pub struct Data {
	identifiers: IdentMap<Span<SrcPos>>,
	pub tok_list: KindList,
	pub tok_pos: PosList,
}

impl Data {
	pub fn text<'a>(&self,
		input: &'a input::Data,
		ident_id: &identifier::IdentId,
	) -> &'a str {
		let Span { start, end } =  self.identifiers[ident_id];
		&input.source[start..end]
	}
	
	pub fn location(&self,
		ident_id: &identifier::IdentId,
	) -> Span<SrcPos> {
		self.identifiers[ident_id]
	}

	pub fn print(&self, input: &input::Data, with_tokens: bool) {
		if with_tokens {
			print!("Tokens:\n ");
			for (token, start) in self.tok_list.iter().zip(self.tok_pos.iter()) {
				match token {
					Kind::Identifier(ident_id) => {
						print!(" Identifier({})[{start}]", self.text(input, ident_id));
					}
					_ => print!(" {token:?}[{start}]"),
				}
			}
			println!();
			println!();
		}

		let mut identifiers = self.identifiers.keys()
			.map(|id| (id, self.text(input, id)))
			.collect::<Vec<_>>();
		identifiers.sort_by(|(_,a),(_,b)| a.cmp(b));

		println!("{:<32} | HASH-VALUE",
			"IDENTIFIER");
		println!("{:-<32} | {:-<16}", "", "");
		for (ident_id, ident) in identifiers {
			println!("{ident:<32} | {ident_id}");
		}
		println!()
	}
}

#[derive(Default)]
struct Lexer {
	pos: SrcPos,
	out: Data,
}

impl Lexer {
	fn next(&mut self, source: &str) -> Result<bool, Error> {
		let start = self.pos;

		let kind = match self.peek(source, 0) {
			None => {
				self.out.tok_list.push(Kind::Eof);
				self.out.tok_pos.push(start);
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

					// Targets
					"m68k" => Kind::M68k,
					"sh2" => Kind::SH2,
					"x64" => Kind::X64,
					"z80" => Kind::Z80,

					// Keywords
					"else" => Kind::Else,
					"false" => Kind::False,
					"for" => Kind::For,
					"free" => Kind::Free,
					"if" => Kind::If,
					"in" => Kind::In,
					"let" => Kind::Let,
					"mark" => Kind::Mark,
					"return" => Kind::Return,
					"true" => Kind::True,
					"use" => Kind::Use,
					"where" => Kind::Where,
					"while" => Kind::While,

					text => {
						let ident_id = text.id();
						if let Entry::Vacant(e) = self.out.identifiers.entry(ident_id) {
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
				self.lex_number(source, start, c, false)?
			}

			Some('-') => {
				self.advance(source);
				if let Some(c) = self.peek(source, 0).filter(|c| c.is_numeric()) {
					self.lex_number(source, self.pos, c, true)?
				} else {
					self.item(source, &[
						('=', Kind::DashEq),
						('>', Kind::Arrow),
					], Kind::Dash)
				}
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

			Some(ch) =>  return Err(Error::new((start..self.pos).into(),
				format!("unknown character '{ch}'"))),
		};

		self.out.tok_list.push(kind);
		self.out.tok_pos.push(start);
		Ok(true)
	}

	fn lex_number(&mut self,
		source: &str,
		mut start: usize,
		c: char,
		is_negative: bool,
	) -> Result<Kind, Error> {
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
			start = self.pos;
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

		let text = &source[start..self.pos];
		let tok_src = (start..self.pos).into();
		if is_fractional {
			match num_type {
				NumType::Bin => Err(Error::new(tok_src,
					"parsing binary fixed-point numbers not implemented yet")),
				NumType::Hex => Err(Error::new(tok_src,
					"parsing hexadecimal fixed-point numbers not implemented yet")),
				NumType::Dec => text.replace('_', "").parse::<f64>()
						.map_err(|_| Error::new(tok_src,
							"unable to parse decimal fixed-point number"))
						.map(|n| Kind::Decimal(if is_negative { -n } else { n })),
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
			Ok(Kind::Integer(if is_negative { -num } else { num }))
		}
	}

	fn skip_whitespace_and_comments(&mut self, source: &str) {
		loop {
			match self.peek(source, 0) {
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
	use super::*;

	fn setup(source: &str) -> Data {
		let input = input::eval(file!().to_string(), source.into());

		eval(&source)
			.unwrap_or_else(|e| panic!("{}", e.display(&input)))
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
	fn integer_numbers() {
		let data = setup("4 0b110 0x45");
		assert_eq!(data.tok_list, [
			Kind::Integer(4),
			Kind::Integer(0b110),
			Kind::Integer(0x45),
			Kind::Eof,
		]);
	}

	#[test]
	fn negative_integer_numbers() {
		let data = setup("-4 -0b110 -0x45");
		assert_eq!(data.tok_list, [
			Kind::Integer(-4),
			Kind::Integer(-0b110),
			Kind::Integer(-0x45),
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

	#[test]
	#[should_panic="unknown character"]
	fn invalid_characters() {
		let data = setup("table#users");
		assert_eq!(data.tok_list, []);
	}
}

