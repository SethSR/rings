
use crate::ast;
use crate::error;
use crate::identifier::Id as IdentId;
use crate::{BinaryOp, Data, Bounds, UnaryOp};


type TempId = u32; // Temporary variable ID

#[derive(Debug, Clone)]
pub enum Tac {
	// Basic operations
	Copy { src: Location,  dst: Location },

	// Arithmetic
	BinOp { op: BinaryOp, left: Location, right: Location, dst: Location },
	UnOp { op: UnaryOp, src: Location, dst: Location },

	// Memory access (for table/variable access)
	Load { address: Location, offset: i32, dst: TempId },
	Store { src: Location, address: Location, offset: i32 },

	// Control Flow
	Label(LabelId),
	Jump(LabelId),
	JumpIf { cond: Location, target: LabelId },
	JumpIfNot { cond: Location, target: LabelId },

	// Procedure related
	Call { name: IdentId, args: Vec<Location>, dst: Option<TempId> },
	Return(Option<Location>),

	// Comments/debug
	Comment(String),
}

#[derive(Debug, Clone)]
pub enum Location {
	Temp(TempId),      // Temporary variable
	Variable(IdentId), // Named variable
	Constant(i64),     // Immediate constant
	TableElement {     // Table[index] access
		table: IdentId,
		index: Box<Location>,
	},
}

pub type LabelId = u32;

pub fn eval(data: &mut Data) {
	let mut tac_functions = data.completed_procs.keys()
		.map(|proc_id| TacFunction {
			name: *proc_id,
			locals: vec![],
			instructions: vec![],
			next_temp: 0,
			next_label: 0,
		})
		.collect::<Vec<_>>();

	for tac_function in &mut tac_functions {
		let name = data.text(tac_function.name).to_string();

		if !tac_function.lower_procedure(data) {
			eprintln!("failed to lower '{name}'");
		}

		println!("{name}:");
		for tac in &tac_function.instructions {
			println!("{tac:?}");
		}
	}
}

pub struct TacFunction {
	pub name: IdentId,
	// pub params: Vec<IdentId>,
	pub locals: Vec<(IdentId, crate::Type)>,
	pub instructions: Vec<Tac>,
	pub next_temp: TempId,
	pub next_label: LabelId,
}

impl TacFunction {
	pub fn lower_procedure(&mut self, data: &mut Data) -> bool {
		let proc_start = data.completed_procs[&self.name];

		let kind = &data.ast_nodes[proc_start];
		let range = &data.ast_pos_tok[proc_start];
		if let Err(err_msg) = self.lower_node(kind, data) {
			error::error(data, &err_msg, range.start);
			return false;
		}

		true
	}

	fn lower_node(&mut self, kind: &ast::Kind, data: &Data) -> Result<Option<Location>, String> {
		match kind {
			ast::Kind::Int(value) => {
				Ok(Some(Location::Constant(*value)))
			}

			ast::Kind::Dec(_value) => {
				todo!()
			}

			ast::Kind::Ident(ident_id) => {
				Ok(Some(Location::Variable(*ident_id)))
			}

			ast::Kind::Define(var_id, type_kind, expr_id) => {
				// Get initializer value
				let Some(init_value) = self.lower_node(&data.ast_nodes[*expr_id], data)? else {
					return Ok(None);
				};

				// Add to locals
				self.locals.push((*var_id, *type_kind));

				// Emit assignment
				self.emit(Tac::Copy {
					src: init_value,
					dst: Location::Variable(*var_id),
				});

				Ok(None)
			}

			ast::Kind::Assign(var_id, expr_id) => {
				let Some(value) = self.lower_node(&data.ast_nodes[*expr_id], data)? else {
					return Ok(None);
				};

				self.emit(Tac::Copy {
					src: value,
					dst: Location::Variable(*var_id),
				});

				Ok(None)
			}

			ast::Kind::BinOp(op, left_id, right_id) => {
				let Some(left) = self.lower_node(&data.ast_nodes[*left_id], data)? else {
					return Ok(None);
				};
				let Some(right) = self.lower_node(&data.ast_nodes[*right_id], data)? else {
					return Ok(None);
				};

				// Allocation temporary for result
				let temp = self.alloc_temp();

				self.emit(Tac::BinOp {
					op: *op,
					left,
					right,
					dst: Location::Temp(temp),
				});

				Ok(Some(Location::Temp(temp)))
			}

			ast::Kind::UnOp(op, right_id) => {
				let Some(node) = self.lower_node(&data.ast_nodes[*right_id], data)? else {
					return Ok(None);
				};

				let temp = self.alloc_temp();

				self.emit(Tac::UnOp {
					op: *op,
					src: node,
					dst: Location::Temp(temp),
				});

				Ok(Some(Location::Temp(temp)))
			}

			ast::Kind::Return(maybe_expr) => {
				let value = if let Some(expr_id) = maybe_expr {
					self.lower_node(&data.ast_nodes[*expr_id], data)?
				} else {
					None
				};

				self.emit(Tac::Return(value));
				Ok(None)
			}

			ast::Kind::Block(stmts) => {
				for stmt_id in &stmts.0 {
					self.lower_node(&data.ast_nodes[*stmt_id], data)?;
				}
				Ok(None)
			}

			ast::Kind::If(cond_id, then_block, else_block) => {
				let Some(cond) = self.lower_node(&data.ast_nodes[*cond_id], data)? else {
					return Ok(None);
				};

				let else_label = self.alloc_label();
				let end_label = self.alloc_label();

				// if !cond goto else
				self.emit(Tac::JumpIfNot { cond, target: else_label });

				// then block
				for stmt_id in &then_block.0 {
					self.lower_node(&data.ast_nodes[*stmt_id], data)?;
				}
				self.emit(Tac::Jump(end_label));

				// else block
				self.emit(Tac::Label(else_label));
				for stmt_id in &else_block.0 {
					self.lower_node(&data.ast_nodes[*stmt_id], data)?;
				}

				// end
				self.emit(Tac::Label(end_label));
				Ok(None)
			}

			ast::Kind::While(cond_id, body_block) => {
				let start_label = self.alloc_label();
				let end_label = self.alloc_label();

				// start:
				self.emit(Tac::Label(start_label));

				// if !cond goto end
				let Some(cond) = self.lower_node(&data.ast_nodes[*cond_id], data)? else {
					return Ok(None);
				};
				self.emit(Tac::JumpIfNot { cond, target: end_label });

				// body
				for stmt_id in &body_block.0 {
					self.lower_node(&data.ast_nodes[*stmt_id], data)?;
				}

				// goto start
				self.emit(Tac::Jump(start_label));

				// end:
				self.emit(Tac::Label(end_label));
				Ok(None)
			}

			ast::Kind::For(
				iter_vars,
				table_id,
				bounds,
				body_block,
			) => {
				// For now, simple version: lower to while loop
				// for i in 0..N becomes:
				// i = 0
				// while i < N:
				//   body
				//   i = i + 1

				self.lower_for_loop(data, iter_vars, table_id, bounds, body_block)?;

				Ok(None)
			}
		}
	}

	fn lower_for_loop(
		&mut self,
		data: &Data,
		iter_vars: &[IdentId],
		table_id: &Option<IdentId>,
		bounds: &Option<Bounds>,
		body_block: &ast::Block,
	) -> Result<(), String> {
		// Simple case: for i in 0..N
		if iter_vars.len() == 1 && table_id.is_none() {
			let index_var = iter_vars[0];

			let start_label = self.alloc_label();
			let end_label = self.alloc_label();

			let Some(Bounds::Full { start, end }) = bounds else {
				return Err("L - non-table loop ranges require a min and max value".to_string());
			};

			// i = start
			self.emit(Tac::Copy {
				src: Location::Constant(*start),
				dst: Location::Variable(index_var),
			});

			// start:
			self.emit(Tac::Label(start_label));

			// if i >= end goto end
			let temp = self.alloc_temp();
			println!("binop2");
			self.emit(Tac::BinOp {
				op: BinaryOp::CmpGE,
				left: Location::Variable(index_var),
				right: Location::Constant(*end),
				dst: Location::Temp(temp),
			});

			// body
			for stmt_id in &body_block.0 {
				self.lower_node(&data.ast_nodes[*stmt_id], data)?;
			}

			self.emit(Tac::JumpIf {
				cond: Location::Temp(temp),
				target: end_label,
			});

			// i = i + 1
			let temp2 = self.alloc_temp();
			println!("binop3");
			self.emit(Tac::BinOp {
				op: BinaryOp::Add,
				left: Location::Variable(index_var),
				right: Location::Constant(1),
				dst: Location::Temp(temp2),
			});
			self.emit(Tac::Copy {
				src: Location::Temp(temp2),
				dst: Location::Variable(index_var),
			});

			// goto start
			self.emit(Tac::Jump(start_label));

			// end:
			self.emit(Tac::Label(end_label));
		}

		// TODO - srenshaw - Handle table iteration, multiple variables, etc.
		Ok(())
	}

	fn emit(&mut self, instr: Tac) {
		self.instructions.push(instr);
	}

	fn alloc_temp(&mut self) -> TempId {
		self.next_temp += 1;
		self.next_temp - 1
	}

	fn alloc_label(&mut self) -> LabelId {
		self.next_label += 1;
		self.next_label - 1
	}
}

