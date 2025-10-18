
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
const YELLOW: &str = "\x1b[33m";
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
		let line_num_width = line.to_string().len();

		// Error header: "error message"
		output.push(header(&error_lead(), &self.message));

		// Location: "  --> file.rings:10:5"
		output.push(location(file, line, col, line_num_width));

		// Line number and separator
		output.push(vbar_empty(line_num_width));

		// The actual line
		output.push(vbar_text(line_num_width, line, data.get_line(line)));

		// Highlight underline
		let (end_line, end_col) = data.lookup_position(self.location.end);

		if line == end_line {
			// Single line highlight
			let underline_len = (end_col - col).max(1);
			output.push(vbar_highlight(line_num_width, col, underline_len));
		} else {
			// Multi-line highlight
			output.push(vbar_highlight(line_num_width, col, 1));
		}

		// Notes
		for note in &self.notes {
			output.push(String::from('\n'));

			if let Some(note_loc) = &note.location {
				// TODO - srenshaw - Eventually, we'll want to change this to allow more than one source
				// file.
				let note_file = &data.source_file;
				let (note_line, note_col) = data.lookup_position(note_loc.start);

				output.push(header(&note_lead(), &note.message));
				output.push(location(note_file, note_line, note_col, line_num_width));

				// Show note context
				let note_line_text = data.get_line(note_line);
				output.push(vbar_empty(line_num_width));
				output.push(vbar_text(line_num_width, note_line, note_line_text));
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

fn location(file: &str, line: usize, col: usize, width: usize) -> String {
	format!("{:width$}{BLUE}-->{RESET}{file}:{line}:{col}", "")
}

fn vbar<T: std::fmt::Display>(width: usize, text: T) -> String {
	format!("{BLUE}{text:width$} |{RESET}")
}

fn vbar_empty(width: usize) -> String {
	vbar(width, "")
}

fn vbar_text(width: usize, line: usize, text: &str) -> String {
	format!("{} {text}", vbar(width, line))
}

fn vbar_highlight(width: usize, padding: usize, length: usize) -> String {
	let padding = " ".repeat(padding);
	let arrows = "^".repeat(length);
	format!("{} {padding}{RED}{arrows}{RESET}", vbar(width, ""))
}

