
use crate::operators::{BinaryOp, UnaryOp};
use crate::vsmc::{Section, Vsmc};

use super::inner_label;

mod ins;
use ins::Reg;
pub use ins::Asm;

fn build_constant(data: &mut Vec<Asm>, c: &i64) {
	if (i8::MIN as i64..u8::MAX as i64).contains(c) {
		data.push(Asm::MovI(*c as i8, 0));
		if *c > i8::MAX as i64 {
			data.push(Asm::ExtUB(0, 0));
		}
	} else if (i16::MIN as i64..u16::MAX as i64).contains(c) {
		data.push(Asm::MovI((c >> 8) as i8, 0));
		data.push(Asm::ShLL8(0));
		data.push(Asm::OrI(*c as u8));
		if *c > i16::MAX as i64 {
			data.push(Asm::ExtUW(0, 0));
		}
	// NOTE - srenshaw - This explicitly checks from i32 to u32, because the output is the same
	} else if (i32::MIN as i64..u32::MAX as i64).contains(c) {
		data.push(Asm::MovI((c >> 24) as i8, 0));
		data.push(Asm::ShLL8(0));
		data.push(Asm::OrI((c >> 16) as u8));
		data.push(Asm::ShLL8(0));
		data.push(Asm::OrI((c >> 8) as u8));
		data.push(Asm::ShLL8(0));
		data.push(Asm::OrI(*c as u8));
	}
}

pub fn lower(proc_name: &str, section: Section) -> Vec<Asm> {
	let Section {
		instructions,
		next_label: mut label_id,
		..
	} = section;

	const R0: Reg = 0;
	const FP: Reg = 13; // Data stack address
	const SP: Reg = 15; // Call stack address

	let mut data = vec![
		Asm::Label(proc_name.to_owned()),
	];

	data.push(Asm::Mov(SP, FP));

	build_constant(&mut data, &(section.locals.len() as i64));
	data.push(Asm::Sub(R0, FP));

	for tac in instructions {
		match tac {
			Vsmc::Push(value) => {
				build_constant(&mut data, &(value as i64));
				data.push(Asm::MovLM(R0, FP));
			}

			Vsmc::Label(id) => {
				let label = format!("{proc_name}_{id}");
				data.push(Asm::Label(label))
			}

			Vsmc::Return(true) => {
				// Return through R0
				data.push(Asm::Rts);
				data.push(Asm::MovLP(FP, R0));
			}
			Vsmc::Return(false) => {
				data.push(Asm::Rts);
				data.push(Asm::Nop);
			}

			Vsmc::Jump(id) => {
				let label = format!("{proc_name}_{id}");
				data.push(Asm::Bra(label));
				data.push(Asm::Nop);
			}

			Vsmc::JumpIf(id) => {
				let label = format!("{proc_name}_{id}");
				data.push(Asm::MovLP(FP, R0));
				data.push(Asm::CmpPL(R0));
				data.push(Asm::BT(label));
				data.push(Asm::Nop);
			}

			Vsmc::UnOp(op) => {
				data.push(Asm::MovLP(FP, R0));
				match op {
					UnaryOp::Not => data.push(Asm::Not(R0, R0)),
					UnaryOp::Neg => data.push(Asm::Neg(R0, R0)),
				}
				data.push(Asm::MovLM(R0, FP));
			}

			Vsmc::BinOp(op) => {
				data.push(Asm::MovLP(FP, 1));
				data.push(Asm::MovLP(FP, R0));
				match op {
					BinaryOp::Add => data.push(Asm::Add(1, R0)),
					BinaryOp::BinAnd => data.push(Asm::And(1, R0)),
					BinaryOp::BinOr => data.push(Asm::Or(1, R0)),
					BinaryOp::BinXor => data.push(Asm::Xor(1, R0)),
					BinaryOp::CmpEQ => {
						data.push(Asm::CmpEQ(1, R0));
						data.push(Asm::MovT(R0));
					}
					BinaryOp::CmpGE => {
						data.push(Asm::CmpGE(1, R0));
						data.push(Asm::MovT(R0));
					}
					BinaryOp::CmpGT => {
						data.push(Asm::CmpGT(1, R0));
						data.push(Asm::MovT(R0));
					}
					BinaryOp::CmpLE => {
						data.push(Asm::CmpGT(R0, 1));
						data.push(Asm::MovT(R0));
					}
					BinaryOp::CmpLT => {
						data.push(Asm::CmpGE(R0, 1));
						data.push(Asm::MovT(R0));
					}
					BinaryOp::CmpNE => {
						data.push(Asm::CmpEQ(R0, 1));
						data.push(Asm::MovT(0));
						data.push(Asm::CmpIM(0));
						data.push(Asm::MovT(R0));
					}
					BinaryOp::Div => {
						todo!("finish implementing division once we we have more types")
					}
					BinaryOp::LogAnd => {
						data.push(Asm::CmpPL(1));
						data.push(Asm::MovT(1));
						data.push(Asm::CmpPL(R0));
						data.push(Asm::MovT(R0));
						data.push(Asm::And(1, R0));
					}
					BinaryOp::LogOr => {
						data.push(Asm::CmpPL(1));
						data.push(Asm::MovT(1));
						data.push(Asm::CmpPL(R0));
						data.push(Asm::MovT(R0));
						data.push(Asm::Or(1, R0));
					}
					BinaryOp::LogXor => {
						data.push(Asm::CmpPL(1));
						data.push(Asm::MovT(1));
						data.push(Asm::CmpPL(R0));
						data.push(Asm::MovT(R0));
						data.push(Asm::Xor(1, R0));
					},
					BinaryOp::Mod => {
						todo!("finish implementing modulation once we we have more types")
					}
					BinaryOp::Mul => {
						data.push(Asm::MulL(1, R0));
						data.push(Asm::StMacl(R0));
					}
					BinaryOp::ShL => {
						let loop_label = inner_label(proc_name, &mut label_id);
						let check_label = inner_label(proc_name, &mut label_id);
						data.push(Asm::Bra(check_label.clone()));
						data.push(Asm::Label(loop_label.clone()));
						data.push(Asm::ShAL(R0));
						data.push(Asm::Label(check_label));
						data.push(Asm::DT(1));
						data.push(Asm::BF(loop_label));
					}
					BinaryOp::ShR => {
						let loop_label = inner_label(proc_name, &mut label_id);
						let check_label = inner_label(proc_name, &mut label_id);
						data.push(Asm::Bra(check_label.clone()));
						data.push(Asm::Label(loop_label.clone()));
						data.push(Asm::ShAR(R0));
						data.push(Asm::Label(check_label));
						data.push(Asm::DT(1));
						data.push(Asm::BF(loop_label));
					}
					BinaryOp::Sub => data.push(Asm::Sub(1, R0)),
				}
				data.push(Asm::MovLM(R0, FP));
			}

			Vsmc::Load(offset) => {
				data.push(Asm::Mov(SP, 1));
				data.push(Asm::MovLL4(offset as u8, 1, R0));
				data.push(Asm::AddI(-4, FP));
			}

			Vsmc::Store(offset) => {
				data.push(Asm::Mov(SP, 1));
				data.push(Asm::MovLS4(R0, offset as u8, 1));
				data.push(Asm::AddI(4, FP));
			}

			#[cfg(feature="call")]
			Vsmc::Call { name, args, dst } => {
				todo!("call convention is not implemented yet: {}({}){}",
					db.text(name),
					args.iter().map(|loc| match loc {
						Location::Constant(c) => c.to_string(),
						Location::Temp(t) => format!("?{t}"),
						Location::Variable(v) => db.text(&v).to_string(),
					}).collect::<Vec<_>>().join(", "),
					dst.map(|x| format!(" -> {x}")).unwrap_or("".to_string()),
				);
			}
		}
	}

	data
}

