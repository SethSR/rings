
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
	let span = data.tok_pos[id]..data.tok_pos[id+1];
	error(data, span, &format!("Expected {expected}, found {found:?}"))
}

pub struct Span {
	pub start: usize,
	pub end: usize,
}

// File tracking
pub type FileId = u32;

pub struct SourceFile {
	pub id: FileId,
	pub name: String,
	pub contents: String,
	pub line_starts: Vec<u32>, // Byte offset of each line start
}

pub struct SourceManager {
	files: Vec<SourceFile>,
}

pub struct Note {
	pub location: Option<Span>,
	pub message: String,
}

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

	pub fn with_note(mut self, message: impl Into<String>) -> Self {
		self.notes.push(Note {
			location: None,
			message: message.into(),
		});
		self
	}

	pub fn with_note_at(
		mut self,
		location: Span,
		message: impl Into<String>,
	) -> Self {
		self.notes.push(Note {
			location: Some(location),
			message: message.into(),
		});
		self
	}

	pub fn display(&self, data: &Data) -> String {
		let mut output = String::new();

		// Main error header
		let file = &data.source_file;
		let (line, col) = data.lookup_position(self.location.start);

		// Color codes
		const RED   : &str = "\x1b[31m";
		const YELLOW: &str = "\x1b[33m";
		const BLUE  : &str = "\x1b[34m";
		const BOLD  : &str = "\x1b[1m";
		const RESET : &str = "\x1b[0m";

		// Error header: "error message"
		output.push_str(&format!(
			"{BOLD}{RED}error{RESET}{BOLD}: {}\n",
			self.message,
		));
		output.push_str(&format!("{RESET}  {BOLD}\n"));

		// Location: "  --> file.rings:10:5"
		output.push_str(&format!(
			"   {BLUE}-->{RESET}{file}:{line}:{col}\n",
		));

		// Show the source line with highlight
		let line_text = data.get_line(line);
		let line_num_width = line.to_string().len().max(3);

		// Line number and separator
		output.push_str(&format!(
			"{:width$} {BLUE}|{RESET}\n", "",
			width = line_num_width,
		));

		// The actual line
		output.push_str(&format!(
			"{BLUE}{line:width$} |{RESET} {line_text}\n",
			width = line_num_width,
		));

		// Highlight underline
		let (_, start_col) = data.lookup_position(self.location.start);
		let (end_line, end_col) = data.lookup_position(self.location.end);

		if line == end_line {
			// Single line highlight
			let underline_len = (end_col - start_col).max(1);
			output.push_str(&format!(
				"{:width$} {BLUE}|{RESET} {}{RED}{}{RESET}\n",
				"",
				" ".repeat(start_col - 1),
				"^".repeat(underline_len),
				width = line_num_width,
			));
		} else {
			// Multi-line highlight
			output.push_str(&format!(
				"{:width$} {BLUE}|{RESET} {}{RED}^{RESET} ...\n",
				"",
				" ".repeat(start_col - 1),
				width = line_num_width,
			));
		}

		// Notes
		for note in &self.notes {
			if let Some(note_loc) = &note.location {
				// TODO - srenshaw - Eventually, we'll want to change this to allow more than one source
				// file.
				let note_file = &data.source_file;
				let (note_line, note_col) = data.lookup_position(note_loc.start);

				output.push_str(&format!(
					"\n{BOLD}{BLUE}note{RESET}{BOLD}: {}\n",
					note.message,
				));
				output.push_str(&format!(
					"   {BLUE}-->{RESET}{note_file}:{note_line}:{note_col}\n",
				));

				// Show note context
				let note_line_text = data.get_line(note_line);
				output.push_str(&format!(
					"{:width$} {BLUE}|{RESET}\n",
					"",
					width = line_num_width,
				));
				output.push_str(&format!(
					"{BLUE}{note_line:width$} |{RESET} {note_line_text}\n",
					width = line_num_width,
				));
			} else {
				// Note without location
				output.push_str(&format!(
					"\n{BOLD}{BLUE}note{RESET}{BOLD}: {}\n",
					note.message,
				));
			}
		}

		output
	}
}
