
use crate::ast::Id as AstId;
use crate::identifier::Id as IdentId;
use crate::token::Id as TokenId;
use crate::{Data, Span};

pub fn error(data: &Data, message: &str, token_id: TokenId) -> Error {
	Error::new(data.token_source(token_id), message)
}

#[cfg(feature="ready")]
pub fn error_with_notes(data: &Data, message: &str, token_id: TokenId,
	notes: &[(&str, TokenId)],
) -> Error {
	let mut err = Error::new(get_location(data, token_id), message);
	for (note_msg, note_id) in notes {
		err.with_note_at(get_location(data, *note_id), *note_msg);
	}
	err
}

#[derive(Debug, Default, Clone)]
pub struct Note {
	pub location: Option<Span<usize>>,
	pub message: String,
}

// Color codes
const RED   : &str = "\x1b[31m";
// const YELLOW: &str = "\x1b[33m";
const BLUE  : &str = "\x1b[34m";
const BOLD  : &str = "\x1b[1m";
const RESET : &str = "\x1b[0m";

#[derive(Debug, Clone, Copy)]
pub enum Kind {
	// User facing Errors
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

	// Internal Errors
	NoType { msg: &'static str, ast_id: AstId },
	MissingAstNode { proc_id: IdentId, ast_id: AstId },
	MissingLoopBounds { proc_id: IdentId },
}

#[derive(Debug, Default, Clone)]
pub struct Error {
	kind: Option<Kind>,
	location: Span<usize>,
	message: String,
	notes: Vec<Note>,
}

impl Error {
	pub fn new(
		location: Span<usize>,
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

