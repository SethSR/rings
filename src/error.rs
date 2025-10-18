
use std::ops::Range;

use crate::token;
use crate::Data;

pub fn error(data: &mut Data, span: Range<usize>, msg: &str) {
	let span = Span { start: span.start, end: span.end };
	let err = CompilerError::new(span, msg);
	data.errors.push(err);
}

pub fn expected(data: &mut Data, span: Range<usize>, expected: &str, found: &str) {
	error(data, span, &format!("Expected {expected}, found {found}"))
}

pub fn expected_token(data: &mut Data, expected: &str, id: token::Id) {
	let found = data.tok_list[id];
	let start = data.tok_pos[id];
	let span = start..start + found.size(data);
	if let token::Kind::Identifier(ident_id) = found {
		error(data, span, &format!("Expected {expected}, found '{}'", data.text(ident_id)))
	} else {
		error(data, span, &format!("Expected {expected}, found {found:?}"))
	}
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

pub struct CompilerError {
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
			location,
			message: message.into(),
			notes: Vec::new(),
		}
	}

	pub fn with_note(&mut self, message: impl Into<String>) {
		self.notes.push(Note {
			location: None,
			message: message.into(),
		});
	}

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

		output.push(header(&error_lead(), &self.message));
		output.push(location(line_num_digit_count,
			file, line, col));
		output.push(vbar_empty(line_num_digit_count));
		output.push(vbar_text(line_num_digit_count,
			line, data.get_line(line)));

		// Highlight underline
		let (end_line, end_col) = data.lookup_position(self.location.end);
		let underline_len = if line == end_line {
			end_col.saturating_sub(col).max(1) // Single line highlight
		} else {
			1 // Multi-line highlight
		};
		output.push(vbar_highlight(line_num_digit_count,
			col, underline_len));

		// Notes
		for note in &self.notes {
			output.push(String::from('\n'));

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
					note_line, data.get_line(note_line)));
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
	format!("{} {padding}{RED}{arrows}{RESET}", vbar(gutter_width, ""))
}

