
use std::ops::Range;

use crate::token;
use crate::Data;

pub fn error(data: &Data, message: &str, token_id: token::Id) -> CompilerError {
	CompilerError::new(get_location(data, token_id), message)
}

#[cfg(feature="ready")]
pub fn error_with_notes(data: &Data, message: &str, token_id: token::Id,
	notes: &[(&str, token::Id)],
) -> CompilerError {
	let mut err = CompilerError::new(get_location(data, token_id), message);
	for (note_msg, note_id) in notes {
		err.with_note_at(get_location(data, *note_id), *note_msg);
	}
	err
}

pub fn expected(span: Range<usize>, expected: &str, found: &str) -> CompilerError {
	let span = Span { start: span.start, end: span.end };
	CompilerError::new(span, format!("Expected {expected}, found {found}"))
}

pub fn expected_token(data: &Data, expected: &str, token_id: token::Id) -> CompilerError {
	let found = data.tok_list[token_id];
	if let token::Kind::Identifier(ident_id) = found {
		error(data, &format!("Expected {expected}, found '{}'", data.text(&ident_id)), token_id)
	} else {
		error(data, &format!("Expected {expected}, found {found:?}"), token_id)
	}
}

fn get_location(data: &Data, token_id: token::Id) -> Span {
	let kind = data.tok_list[token_id];
	let start = data.tok_pos[token_id];
	Span { start, end: start + kind.size(data) }
}

pub struct Span {
	pub start: usize,
	pub end: usize,
}

pub struct Note {
	pub location: Option<Span>,
	pub message: String,
}

// Color codes
const RED   : &str = "\x1b[31m";
// const YELLOW: &str = "\x1b[33m";
const BLUE  : &str = "\x1b[34m";
const BOLD  : &str = "\x1b[1m";
const RESET : &str = "\x1b[0m";

#[derive(Debug)]
pub enum Kind {
	Lexer,
	Discovery,
	Parser,
	Checker,
	#[cfg(feature="ready")]
	LoweringTAC,
	#[cfg(feature="ready")]
	LoweringMachine,
	#[cfg(feature="ready")]
	Assembler,
}

pub struct CompilerError {
	kind: Option<Kind>,
	location: Span,
	message: String,
	notes: Vec<Note>,
}

impl CompilerError {
	pub fn new(
		location: Span,
		message: impl Into<String>,
	) -> Self {
		Self {
			kind: None,
			location,
			message: message.into(),
			notes: Vec::new(),
		}
	}

	pub fn set_kind(&mut self, kind: Kind) {
		self.kind = Some(kind);
	}

	#[cfg(feature="ready")]
	pub fn with_note(&mut self, message: impl Into<String>) {
		self.notes.push(Note {
			location: None,
			message: message.into(),
		});
	}

	#[cfg(feature="ready")]
	pub fn with_note_at(
		&mut self,
		location: Span,
		message: impl Into<String>,
	) {
		self.notes.push(Note {
			location: Some(location),
			message: message.into(),
		});
	}

	pub fn display(&self, data: &Data) -> String {
		let mut output = vec![];

		// Main error header
		let file = &data.source_file;
		let (line, col) = data.lookup_position(self.location.start);
		let line_num_digit_count = line.to_string().len();

		let message = if let Some(kind) = &self.kind {
			format!("{kind:?} : {}", self.message)
		} else {
			self.message.to_string()
		};

		output.push(header(&error_lead(), &message));
		output.push(location(line_num_digit_count,
			file, line, col));
		output.push(vbar_empty(line_num_digit_count));
		output.push(vbar_text(line_num_digit_count,
			line, &data.get_line(line)));

		// Highlight underline
		let (end_line, end_col) = data.lookup_position(self.location.end);
		let underline_len = if line == end_line {
			end_col.saturating_sub(col).max(1) // Single line highlight
		} else {
			1 // Multi-line highlight
		};
		output.push(vbar_highlight(line_num_digit_count,
			col - 1, underline_len));

		// Notes
		for note in &self.notes {
			if let Some(note_loc) = &note.location {
				// TODO - srenshaw - Eventually, we'll want to change this to allow more than one source
				// file.
				let note_file = &data.source_file;
				let (note_line, note_col) = data.lookup_position(note_loc.start);

				output.push(header(&note_lead(), &note.message));
				output.push(location(line_num_digit_count,
					note_file, note_line, note_col));
				output.push(vbar_empty(line_num_digit_count));
				output.push(vbar_text(line_num_digit_count,
					note_line, &data.get_line(note_line)));
				output.push(vbar_highlight(line_num_digit_count,
					note_col - 1, 1));
			} else {
				// Note without location
				output.push(header(&note_lead(), &note.message));
			}
		}

		output.join("\n")
	}
}

fn error_lead() -> String {
	format!("{RED}error")
}

fn note_lead() -> String {
	format!("{BLUE}note")
}

fn header(lead: &str, msg: &str) -> String {
	format!("{BOLD}{lead}{RESET}{BOLD}: {msg}")
}

fn location(gutter_width: usize, file: &str, line: usize, col: usize) -> String {
	format!("{:gutter_width$}{BLUE}-->{RESET}{file}:{line}:{col}", "")
}

fn vbar<T: std::fmt::Display>(gutter_width: usize, text: T) -> String {
	format!("{BLUE}{text:gutter_width$} |{RESET}")
}

fn vbar_empty(gutter_width: usize) -> String {
	vbar(gutter_width, "")
}

fn vbar_text(gutter_width: usize, line: usize, text: &str) -> String {
	format!("{} {text}", vbar(gutter_width, line))
}

fn vbar_highlight(gutter_width: usize, padding: usize, length: usize) -> String {
	let padding = " ".repeat(padding);
	let arrows = "^".repeat(length);
	format!("{}{padding}{RED}{arrows}{RESET}", vbar(gutter_width, ""))
}

