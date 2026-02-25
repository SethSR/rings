
use std::collections::HashMap;

use crate::operators::{BinaryOp, UnaryOp};
use crate::tac::{Data as TacData, Location, TAC};
use crate::parser::Type;

mod ins;
use ins::Reg;
pub use ins::Asm;

fn build_constant(data: &mut Vec<Asm>, typ: Type, c: i64) {
	match typ {
		Type::S8 => {
			data.push(Asm::MovI(c as i8, R0));
		}
		Type::U8 => {
			data.push(Asm::MovI(c as i8, R0));
			data.push(Asm::ExtUB(R0, R0));
		}
		Type::S16 => {
			data.push(Asm::MovI((c >> 8) as i8, R0));
			data.push(Asm::ShLL8(R0));
			data.push(Asm::OrI(c as u8));
		}
		Type::U16 => {
			data.push(Asm::MovI((c >> 8) as i8, R0));
			data.push(Asm::ShLL8(R0));
			data.push(Asm::OrI(c as u8));
			data.push(Asm::ExtUW(R0, R0));
		}
		Type::S32 | Type::U32 => {
			data.push(Asm::MovI((c >> 24) as i8, R0));
			data.push(Asm::ShLL8(R0));
			data.push(Asm::OrI((c >> 16) as u8));
			data.push(Asm::ShLL8(R0));
			data.push(Asm::OrI((c >> 8) as u8));
			data.push(Asm::ShLL8(R0));
			data.push(Asm::OrI(c as u8));
		}
		_ => unreachable!("cannot build immediates from non-integer types"),
	}
}

/// Utility and return register
const R0: Reg = 0;
/// Call stack address
//const FP: Reg = 14;
/// Data stack address
const SP: Reg = 15;

const TL: Reg = 12;
const TR: Reg = 13;

pub fn lower(
	proc_name: &str,
	tac_data: TacData,
	stack_addr: u32,
	ret_type: Type,
) -> Vec<Asm> {
	let registers = super::allocate(
		&[ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ],
		&tac_data.instructions,
	);

	let mut lbl_gen = super::LabelGenerator(tac_data.next_label);

	let mut data = vec![
		Asm::Label(proc_name.to_owned()),
	];

	if proc_name == "main" {
		data.push(Asm::Comment("Setup stack pointer".into()));
		build_constant(&mut data, Type::U32, stack_addr as i64);
		data.push(Asm::Mov(R0, SP));
	}

	for tac in &tac_data.instructions {
		match tac {
			TAC::Move { src, dst } => {
				data.push(Asm::Comment(format!("Move {src:?} -> {dst:?}")));

				move_src_to_target(&mut data, &registers, src, TL);
				move_target_to_dst(&mut data, &registers, TL, dst);
			}

			TAC::Label(lbl) => {
				data.push(Asm::Label(format!("{proc_name}_{lbl}")));
			}

			TAC::Return(with_value) => {
				data.push(Asm::Comment(format!("Return {:?}", with_value)));

				data.push(Asm::Rts);
				if let Some(vr) = with_value {
					// Return through R0
					data.push(Asm::Mov(registers[vr], R0));
				} else {
					data.push(Asm::Nop);
				}
			}

			TAC::Jump(lbl) => {
				let label = format!("{proc_name}_{lbl}");
				data.push(Asm::Comment(format!("Jump {label}")));

				data.push(Asm::Bra(label));
				data.push(Asm::Nop);
			}

			TAC::JumpIf { lbl, vr } => {
				let label = format!("{proc_name}_{lbl}");
				data.push(Asm::Comment(format!("JumpIf VReg({vr},{ret_type:?}) > 0 -> {label}")));

				data.push(Asm::CmpPL(registers[vr]));
				data.push(Asm::BT(label));
				data.push(Asm::Nop);
			}

			TAC::UnOp { op, rhs, dst } => {
				data.push(Asm::Comment(format!("UnOp {op} {rhs:?} -> {dst:?}")));

				move_src_to_target(&mut data, &registers, rhs, TL);
				match op {
					UnaryOp::Not => data.push(Asm::Not(TL, TL)),
					UnaryOp::Neg => data.push(Asm::Neg(TL, TL)),
				}
				move_target_to_dst(&mut data, &registers, TL, dst);
			}

			TAC::BinOp { op, lhs, rhs, dst } => {
				data.push(Asm::Comment(format!("BinOp {lhs:?} {op} {rhs:?} -> {dst:?}")));

				let ltyp = move_src_to_target(&mut data, &registers, lhs, TL);
				let rtyp = move_src_to_target(&mut data, &registers, rhs, TR);

				match op {
					BinaryOp::Add => data.push(Asm::Add(TR, TL)),
					BinaryOp::Sub => data.push(Asm::Sub(TR, TL)),

					BinaryOp::BinAnd => data.push(Asm::And(TR, TL)),
					BinaryOp::BinOr => data.push(Asm::Or(TR, TL)),
					BinaryOp::BinXor => data.push(Asm::Xor(TR, TL)),

					BinaryOp::CmpEQ => {
						data.push(Asm::CmpEQ(TR, TL));
						data.push(Asm::MovT(TL));
					}
					BinaryOp::CmpGE => {
						data.push(Asm::CmpGE(TR, TL));
						data.push(Asm::MovT(TL));
					}
					BinaryOp::CmpGT => {
						data.push(Asm::CmpGT(TR, TL));
						data.push(Asm::MovT(TL));
					}
					BinaryOp::CmpLE => {
						data.push(Asm::CmpGT(TL, TR));
						data.push(Asm::MovT(TL));
					}
					BinaryOp::CmpLT => {
						data.push(Asm::CmpGE(TL, TR));
						data.push(Asm::MovT(TL));
					}
					BinaryOp::CmpNE => {
						data.push(Asm::CmpEQ(TR, TL));
						data.push(Asm::MovT(TL));
						data.push(Asm::AddI(-1, TL));
						data.push(Asm::Neg(TL, TL));
					}

					BinaryOp::LogAnd => {
						data.push(Asm::CmpPL(TL));
						data.push(Asm::MovT(TL));
						data.push(Asm::CmpPL(TR));
						data.push(Asm::MovT(TR));
						data.push(Asm::And(TR, TL));
					}
					BinaryOp::LogOr => {
						data.push(Asm::CmpPL(TL));
						data.push(Asm::MovT(TL));
						data.push(Asm::CmpPL(TR));
						data.push(Asm::MovT(TR));
						data.push(Asm::Or(TR, TL));
					}
					BinaryOp::LogXor => {
						data.push(Asm::CmpPL(TL));
						data.push(Asm::MovT(TL));
						data.push(Asm::CmpPL(TR));
						data.push(Asm::MovT(TR));
						data.push(Asm::Xor(TR, TL));
					}

					BinaryOp::Mul => {
						data.push(Asm::MulL(TR, TL));
						data.push(Asm::StMacl(TL));
					}

					BinaryOp::Div | BinaryOp::Mod => {
						// TODO - srenshaw - Finish setting up the division header.

						if ltyp.is_signed_integer() {
							data.push(Asm::Div0S(TR, TL));
						} else {
							data.push(Asm::Div0U);
						}

						match rtyp {
							Type::S8 | Type::U8 => {
								data.push(Asm::ShLL16(TR));
								data.push(Asm::ShLL8(TR));
								for _ in 0..8 {
									data.push(Asm::Div1(TR, TL));
								}
							}
							Type::S16 | Type::U16 => {
								data.push(Asm::ShLL16(TR));
								for _ in 0..16 {
									data.push(Asm::Div1(TR, TL));
								}
							}
							Type::S32 | Type::U32 => for _ in 0..32 {
								data.push(Asm::RotCL(R0));
								data.push(Asm::Div1(TR, TL));
							}
							_ => unreachable!(),
						}

						if *op == BinaryOp::Div {
							if matches!(rtyp, Type::S32 | Type::U32) {
								data.push(Asm::Mov(R0, TL));
							}
						} else {
							// TODO - srenshaw - Finish converting quotient, dividend, and divisor into remainder.
						}
					}

					BinaryOp::ShL => {
						let loop_label = lbl_gen.next(proc_name);
						let check_label = lbl_gen.next(proc_name);
						data.push(Asm::Bra(check_label.clone()));
						data.push(Asm::Label(loop_label.clone()));
						if ltyp.is_signed_integer() {
							data.push(Asm::ShAL(TL));
						} else {
							data.push(Asm::ShLL(TL));
						}
						data.push(Asm::Label(check_label));
						data.push(Asm::DT(TR));
						data.push(Asm::BF(loop_label));
					}

					BinaryOp::ShR => {
						let loop_label = lbl_gen.next(proc_name);
						let check_label = lbl_gen.next(proc_name);
						data.push(Asm::Bra(check_label.clone()));
						data.push(Asm::Label(loop_label.clone()));
						if ltyp.is_signed_integer() {
							data.push(Asm::ShAR(TL));
						} else {
							data.push(Asm::ShLR(TL));
						}
						data.push(Asm::Label(check_label));
						data.push(Asm::DT(TR));
						data.push(Asm::BF(loop_label));
					}
				}

				move_target_to_dst(&mut data, &registers, TL, dst);
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

fn extend_register(data: &mut Vec<Asm>, reg: Reg, typ: Type) {
	match typ {
		Type::Bool => {}
		Type::S8 => data.push(Asm::ExtSB(reg, reg)),
		Type::U8 => data.push(Asm::ExtUB(reg, reg)),
		Type::S16 => data.push(Asm::ExtSW(reg, reg)),
		Type::U16 => data.push(Asm::ExtUW(reg, reg)),
		Type::S32 | Type::U32 => {}
		_ => unreachable!("{typ:?}"),
	}
}

/// This will rewrite R0 for address, constant, and stack locations.
fn move_src_to_target(
	data: &mut Vec<Asm>,
	registers: &HashMap<u32,Reg>,
	loc: &Location,
	target: Reg,
) -> Type {
	match loc {
		Location::Addr(adr,typ) => {
			build_constant(data, Type::U32, *adr as i64);
			match typ {
				Type::S8 | Type::U8 => data.push(Asm::MovBL(R0, target)),
				Type::S16 | Type::U16 => data.push(Asm::MovWL(R0, target)),
				Type::S32 | Type::U32 => data.push(Asm::MovLL(R0, target)),
				_ => unreachable!(),
			}
			*typ
		}
		Location::Const(val,typ) => {
			build_constant(data, *typ, *val);
			data.push(Asm::Mov(R0, target));
			*typ
		}
		Location::Stack(idx,typ) => {
			build_constant(data, Type::U8, *idx as i64);
			data.push(Asm::ShLL2(R0));
			match typ {
				Type::S8 | Type::U8 => data.push(Asm::MovBL0(SP, target)),
				Type::S16 | Type::U16 => data.push(Asm::MovWL0(SP, target)),
				Type::S32 | Type::U32 => data.push(Asm::MovLL0(SP, target)),
				_ => unreachable!(),
			}
			*typ
		}
		Location::VReg(vr,typ) => {
			data.push(Asm::Mov(registers[vr], target));
			extend_register(data, target, *typ);
			*typ
		}
	}
}

/// This will rewrite R0 for address and stack locations.
fn move_target_to_dst(
	data: &mut Vec<Asm>,
	registers: &HashMap<u32,Reg>,
	target: Reg,
	loc: &Location,
) -> Type {
	match loc {
		Location::Addr(adr,typ) => {
			build_constant(data, Type::U32, *adr as i64);
			match typ {
				Type::S8 | Type::U8 => data.push(Asm::MovBS(target, R0)),
				Type::S16 | Type::U16 => data.push(Asm::MovWS(target, R0)),
				Type::S32 | Type::U32 => data.push(Asm::MovLS(target, R0)),
				_ => unreachable!(),
			}
			*typ
		}
		Location::Const(..) => panic!("cannot have a constant as a destination"),
		Location::Stack(idx,typ) => {
			build_constant(data, Type::U8, *idx as i64);
			data.push(Asm::ShLL2(R0));
			match typ {
				Type::S8 | Type::U8 => data.push(Asm::MovBS0(target, SP)),
				Type::S16 | Type::U16 => data.push(Asm::MovWS0(target, SP)),
				Type::S32 | Type::U32 => data.push(Asm::MovLS0(target, SP)),
				_ => unreachable!(),
			}
			*typ
		}
		Location::VReg(vr,typ) => {
			let r = registers[vr];
			data.push(Asm::Mov(target, r));
			extend_register(data, r, *typ);
			*typ
		}
	}
}
