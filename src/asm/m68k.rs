
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result};
use crate::asm::LabelGenerator;
use crate::operators::{BinaryOp, UnaryOp};
use crate::parser::Type;
use crate::tac::{Data as TacData, Location, TAC, VRegId};
use crate::Span;

fn get_arithmetic_size(typ: &Type) -> Sz {
	match typ {
		Type::S8 | Type::U8 => Sz::B,
		Type::S16 | Type::U16 => Sz::W,
		Type::S32 | Type::U32 => Sz::L,
		Type::Bool => Sz::B,
		_ => panic!("unexpected ADD type: {typ:?}"),
	}
}

pub fn lower(proc_name: &str, tac_data: TacData) -> Vec<Asm> {
	fn update_interval(
		intervals: &mut HashMap<VRegId, Span<usize>>,
		vr: VRegId,
		idx: usize,
	) {
		intervals.entry(vr)
				.and_modify(|interval| interval.start = idx)
				.or_insert(Span::point(idx));
	}

	let mut intervals = HashMap::<VRegId, Span<usize>>::new();
	for (idx, tac) in tac_data.instructions.iter().enumerate().rev() {
		match tac {
			TAC::Load { vr, loc: Location::VReg(vr1,_) } |
			TAC::Store { vr, loc: Location::VReg(vr1,_) } => {
				update_interval(&mut intervals, *vr, idx);
				update_interval(&mut intervals, *vr1, idx);
			}
			TAC::Load { vr, ..} |
			TAC::Store { vr, ..} |
			TAC::Return(Some(vr)) => {
				update_interval(&mut intervals, *vr, idx);
			}
			TAC::UnOp { vr0, vr1, ..} => {
				update_interval(&mut intervals, *vr0, idx);
				update_interval(&mut intervals, *vr1, idx);
			}
			TAC::BinOp { vr0, vr1, vr2, ..} => {
				update_interval(&mut intervals, *vr0, idx);
				update_interval(&mut intervals, *vr1, idx);
				update_interval(&mut intervals, *vr2, idx);
			}
			_ => {}
		}
	}

	let mut reg_allocator = super::Allocator::new(&[
		Data::D0, Data::D1, Data::D2, Data::D3, Data::D4, Data::D5, Data::D6,
	]);
	reg_allocator.eval(intervals.values().cloned().collect());

	// The base address for the variable stack
	const VAR_SP: Addr = Addr::A6;

	let mut label_gen = LabelGenerator::new(tac_data.next_label);
	let mut data = vec![
		Asm::Label(proc_name.to_string()),
	];

	for tac in &tac_data.instructions {
		match tac {
			TAC::BinOp { op, typ, vr0, vr1, vr2 } => {
				let sz = Sz::L; //get_arithmetic_size(typ);
				// TODO - srenshaw - These can fail, so we'll need to check for memory locations as well
				let r0_int = &intervals[vr0];
				let r0 = reg_allocator.registers[r0_int];
				let r1_int = &intervals[vr1];
				let r1 = reg_allocator.registers[r1_int];
				let r2_int = &intervals[vr2];
				let r2 = reg_allocator.registers[r2_int];

				match op {
					BinaryOp::Add => {
						if r0 == r1 && r0 == r2 {
							// All equal
							// r += r
							data.push(Asm::add(sz, r2, r2));
						} else if r0 == r1 {
							// r2 varies
							// r2 = r0 + r0
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(Asm::add(sz, r2, r2));
						} else if r0 == r2 {
							// r1 varies
							// r2 += r1
							data.push(Asm::add(sz, r1, r2));
						} else if r1 == r2 {
							// r0 varies
							// r2 += r0
							data.push(Asm::add(sz, r0, r2));
						} else {
							// All vary
							// r2 = r0
							// r2 += r1
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(Asm::add(sz, r1, r2));
						}
					}

					BinaryOp::Sub => {
						if r0 == r1 {
							// r2 varies or all equal
							data.push(Asm::Clr(sz, EA::Dat(r2)));
						} else if r0 == r2 {
							// r1 varies
							data.push(Asm::sub(sz, r1, r2));
						} else if r1 == r2 {
							// TODO - srenshaw - If r0 dies here, we can optimize this
							// r0 varies
							data.push(Asm::Move(sz, EA::Dat(r1), EA::Dat(Data::D7)));
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(Asm::sub(sz, Data::D7, r2));
						} else {
							// All vary
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(Asm::sub(sz, r1, r2));
						}
					}

					// 15*23
					// = (10 + 5)*(20 + 3)
					// = 10*(20 + 3) + 5*(20 + 3)
					// = 10*20 + 10*3 + 5*20 + 5*3
					// = 1*10*2*10 + 1*10*3 + 5*2*10 + 5*3
					// = (1*2)*100 + (1*3)*10 + (5*2)*10 + 5*3
					// = (1*2)*100 + (1*3 + 5*2)*10 + 5*3
					// -> (h1*h2)<<32 + (h1*l2 + l1*h2)<<16 + l1*l2
					BinaryOp::Mul => {
						let mul = if typ.is_signed_integer() { Asm::MulS } else { Asm::MulU };

						if r0 == r1 && r0 == r2 {
							// All equal
							// r = r * r
							data.push(mul(EA::Dat(r2), r2));
						} else if r0 == r1 {
							// r2 varies
							// r2 = r0 * r0
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(mul(EA::Dat(r2), r2));
						} else if r0 == r2 {
							// r1 varies
							// r2 *= r1
							data.push(mul(EA::Dat(r1), r2));
						} else if r1 == r2 {
							// r0 varies
							// r2 *= r0
							data.push(mul(EA::Dat(r0), r2));
						} else {
							// All vary
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(mul(EA::Dat(r1), r2));
						}
					}

					BinaryOp::Div | BinaryOp::Mod => {
						let div = if typ.is_signed_integer() { Asm::DivS } else { Asm::DivU };

						if r0 == r1 && r0 == r2 {
							// All equal
							// r = r / r
							let trap_label = label_gen.next(proc_name);
							data.push(Asm::Tst(sz, EA::Dat(r2)));
							data.push(Asm::Bcc(Cond::NE, trap_label.clone()));
							// Force a div-by-zero
							data.push(Asm::Clr(sz, EA::Dat(r2)));
							data.push(Asm::DivS(EA::Dat(r2), r2));
							data.push(Asm::Label(trap_label));
							data.push(Asm::Move(sz, EA::Imm(1), EA::Dat(r2)));
						} else if r0 == r1 {
							// r2 varies
							// r2 = r0 / r0
							let trap_label = label_gen.next(proc_name);
							data.push(Asm::Tst(sz, EA::Dat(r0)));
							data.push(Asm::Bcc(Cond::NE, trap_label.clone()));
							// Force a div-by-zero
							data.push(Asm::Clr(sz, EA::Dat(r2)));
							data.push(Asm::DivS(EA::Dat(r2), r2));
							data.push(Asm::Label(trap_label));
							data.push(Asm::Move(sz, EA::Imm(1), EA::Dat(r2)));
						} else if r0 == r2 {
							// r1 varies
							// r2 /= r1
							data.push(div(EA::Dat(r1), r2));
						} else if r1 == r2 {
							// r0 varies
							// r2 = r0 / r2
							data.push(Asm::Move(sz, EA::Dat(r2), EA::Dat(Data::D7)));
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(div(EA::Dat(Data::D7), r2));
						} else {
							// All vary
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(div(EA::Dat(r1), r2));
						}

						// r2 = rem
						if *op == BinaryOp::Mod {
							data.push(Asm::Swap(r2));
						}
					}

					BinaryOp::ShL => {
						let shl = if typ.is_signed_integer() { Asm::Asl } else { Asm::Lsl };

						// (A & B) | B
						// A B ?
						// (A | B) & B
						if (r0 == r1 && r0 == r2) || (r0 == r2) {
							// All equal
							// r <<= r
							data.push(shl(r1, r2));
						} else if r0 == r1 {
							// r2 varies
							// r2 = r0
							// r2 <<= r2
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(shl(r2, r2));
						} else if r1 == r2 {
							// r0 varies
							// r2 = r0 << r2
							data.push(Asm::Move(sz, EA::Dat(r2), EA::Dat(Data::D7)));
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(shl(Data::D7, r2));
						} else {
							// All varies
							// r2 = r0
							// r2 <<= r1
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(shl(r1, r2));
						}
					}

					BinaryOp::ShR => {
						// r2 >>= r1
						let shr = if typ.is_signed_integer() { Asm::Asr } else { Asm::Lsr };

						if r0 == r1 && r0 == r2 {
							// All equal
							// r >>= r
							data.push(shr(r1, r2));
						} else if r0 == r1 {
							// r2 varies
							// r2 = r0 >> r0
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(shr(r2, r2));
						} else if r0 == r2 {
							// r1 varies
							// r2 >>= r1
							data.push(shr(r1, r2));
						} else if r1 == r2 {
							// r0 varies
							// r2 = r0 >> r2
							data.push(Asm::Move(sz, EA::Dat(r1), EA::Dat(Data::D7)));
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(shr(Data::D7, r2));
						} else {
							// All varies
							// r2 = r0 >> r1
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(shr(r1, r2));
						}
					}

					BinaryOp::BinAnd => {
						if r0 == r1 && r0 == r2 {
							// r = r & r
							// do nothing
						} else if r0 == r1 {
							// r2 = r0 & r0
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
						} else if r0 == r2 {
							// r2 = r2 & r1
							data.push(Asm::and(sz, r1, r2));
						} else if r1 == r2 {
							// r2 = r0 & r2
							data.push(Asm::and(sz, r0, r2));
						} else {
							// r2 = r0 & r1
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(Asm::and(sz, r1, r2));
						}
					}

					BinaryOp::BinOr => {
						if r0 == r1 && r0 == r2 {
							// r = r | r
							// do nothing
						} else if r0 == r1 {
							// r2 = r0 | r0
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
						} else if r0 == r2 {
							// r2 = r0 | r1
							data.push(Asm::or(sz, r1, r2));
						} else if r1 == r2 {
							// r2 = r0 | r2
							data.push(Asm::or(sz, r0, r2));
						} else {
							// r2 = r0 | r1
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(Asm::or(sz, r1, r2));
						}
					}

					BinaryOp::BinXor => {
						if r0 == r1 && r0 == r2 {
							// r = r ^ r
							data.push(Asm::Clr(sz, EA::Dat(r2)));
						} else if r0 == r1 {
							// r2 = r0 ^ r0
							data.push(Asm::Clr(sz, EA::Dat(r2)));
						} else if r0 == r2 {
							// r2 = r2 ^ r1
							data.push(Asm::eor(sz, r1, r2));
						} else if r1 == r2 {
							// r2 = r0 ^ r2
							data.push(Asm::eor(sz, r0, r2));
						} else {
							// r2 = r0 ^ r1
							data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r2)));
							data.push(Asm::eor(sz, r1, r2));
						}
					}

					BinaryOp::LogAnd => {
						if r0 == r1 && r0 == r2 {
							// r2 = r2 && r2
							data.push(Asm::Tst(sz, EA::Dat(r2)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(r2)));
						} else if r0 == r1 {
							// r2 = r0 && r0
							data.push(Asm::Tst(sz, EA::Dat(r0)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(r2)));
						} else if r0 == r2 {
							// r2 = r2 && r1
							data.push(Asm::Tst(sz, EA::Dat(r2)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(r2)));
							data.push(Asm::Tst(sz, EA::Dat(r1)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(Data::D7)));
							data.push(Asm::and(Sz::L, Data::D7, r2));
							data.push(Asm::Neg(Sz::B, EA::Dat(r2)));
						} else if r1 == r2 {
							// r2 = r0 && r2
							data.push(Asm::Tst(sz, EA::Dat(r2)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(r2)));
							data.push(Asm::Tst(sz, EA::Dat(r0)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(Data::D7)));
							data.push(Asm::and(Sz::L, Data::D7, r2));
							data.push(Asm::Neg(Sz::B, EA::Dat(r2)));
						} else {
							data.push(Asm::Tst(sz, EA::Dat(r0)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(r2)));
							data.push(Asm::Tst(sz, EA::Dat(r1)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(Data::D7)));
							data.push(Asm::and(Sz::L, Data::D7, r2));
							data.push(Asm::Neg(Sz::B, EA::Dat(r2)));
						}
					}

					BinaryOp::LogOr => {
						if r0 == r1 && r0 == r2 {
							// r2 = r2 || r2
							data.push(Asm::Tst(sz, EA::Dat(r2)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(r2)));
						} else if r0 == r1 {
							// r2 = r0 || r0
							data.push(Asm::Tst(sz, EA::Dat(r0)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(r2)));
						} else if r0 == r2 {
							// r2 = r2 || r1
							data.push(Asm::Tst(sz, EA::Dat(r2)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(r2)));
							data.push(Asm::Tst(sz, EA::Dat(r1)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(Data::D7)));
							data.push(Asm::or(Sz::L, Data::D7, r2));
							data.push(Asm::Neg(Sz::B, EA::Dat(r2)));
						} else if r1 == r2 {
							// r2 = r2 || r0
							data.push(Asm::Tst(sz, EA::Dat(r2)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(r2)));
							data.push(Asm::Tst(sz, EA::Dat(r0)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(Data::D7)));
							data.push(Asm::or(Sz::L, Data::D7, r2));
							data.push(Asm::Neg(Sz::B, EA::Dat(r2)));
						} else {
							// r2 = r0 || r1
							data.push(Asm::Tst(sz, EA::Dat(r0)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(r2)));
							data.push(Asm::Tst(sz, EA::Dat(r1)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(Data::D7)));
							data.push(Asm::or(Sz::L, Data::D7, r2));
							data.push(Asm::Neg(Sz::B, EA::Dat(r2)));
						}
					}

					BinaryOp::LogXor => {
						if r0 == r1 && r0 == r2 {
							// r2 = r2 ^^ r2
							data.push(Asm::Clr(sz, EA::Dat(r2)));
						} else if r0 == r1 {
							// r2 = r0 ^^ r0
							data.push(Asm::Clr(sz, EA::Dat(r2)));
						} else if r0 == r2 {
							// r2 = r2 ^^ r1
							data.push(Asm::Tst(sz, EA::Dat(r2)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(r2)));
							data.push(Asm::Tst(sz, EA::Dat(r1)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(Data::D7)));
							data.push(Asm::eor(Sz::L, Data::D7, r2));
							data.push(Asm::Neg(Sz::B, EA::Dat(r2)));
						} else if r1 == r2 {
							// r2 = r0 ^^ r2
							data.push(Asm::Tst(sz, EA::Dat(r2)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(Data::D7)));
							data.push(Asm::Tst(sz, EA::Dat(r0)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(r2)));
							data.push(Asm::eor(Sz::L, Data::D7, r2));
							data.push(Asm::Neg(Sz::B, EA::Dat(r2)));
						} else {
							// r2 = r0 ^^ r1
							data.push(Asm::Tst(sz, EA::Dat(r0)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(r2)));
							data.push(Asm::Tst(sz, EA::Dat(r1)));
							data.push(Asm::Scc(Cond::NE, EA::Dat(Data::D7)));
							data.push(Asm::eor(Sz::L, Data::D7, r2));
							data.push(Asm::Neg(Sz::B, EA::Dat(r2)));
						}
					}

					BinaryOp::CmpEQ => {
						output_cmp(r0, r1, r2, sz, Cond::EQ, Cond::T, &mut data);
					}

					BinaryOp::CmpNE => {
						output_cmp(r0, r1, r2, sz, Cond::NE, Cond::F, &mut data);
					}

					BinaryOp::CmpGE => {
						let cc = if typ.is_signed_integer() { Cond::GE } else { Cond::CC };
						output_cmp(r0, r1, r2, sz, cc, Cond::T, &mut data);
					}

					BinaryOp::CmpGT => {
						let cc = if typ.is_signed_integer() { Cond::GT } else { Cond::HI };
						output_cmp(r0, r1, r2, sz, cc, Cond::F, &mut data);
					}

					BinaryOp::CmpLE => {
						let cc = if typ.is_signed_integer() { Cond::LE } else { Cond::LS };
						output_cmp(r0, r1, r2, sz, cc, Cond::T, &mut data);
					}

					BinaryOp::CmpLT => {
						let cc = if typ.is_signed_integer() { Cond::LT } else { Cond::CS };
						output_cmp(r0, r1, r2, sz, cc, Cond::F, &mut data);
					}
				}
			}

			TAC::UnOp { op, typ, vr0, vr1 } => {
				let sz = Sz::L; //get_arithmetic_size(typ);
				let r0 = reg_allocator.registers[&intervals[vr0]];
				let r1 = reg_allocator.registers[&intervals[vr1]];

				data.push(Asm::Move(sz, EA::Dat(r0), EA::Dat(r1)));
				match op {
					UnaryOp::Neg => data.push(Asm::Neg(sz, EA::Dat(r1))),
					UnaryOp::Not => data.push(Asm::Not(sz, EA::Dat(r1))),
				}
			}

			TAC::Load { loc, vr } => {
				let r = reg_allocator.registers[&intervals[vr]];
				match loc {
					Location::Addr(addr, typ) => {
						let sz = Sz::L;//get_arithmetic_size(typ);
						data.push(Asm::Move(sz, EA::AbL(*addr as i32), EA::Dat(r)));
					}
					Location::Const(val, typ) => {
						let sz = Sz::L;//get_arithmetic_size(typ);
						data.push(Asm::Move(sz, EA::Imm(*val as i32), EA::Dat(r)));
					}
					Location::Stack(idx, typ) => {
						let sz = Sz::L;//get_arithmetic_size(typ);
						data.push(Asm::Move(sz, EA::Dsp((idx * 4) as i16, VAR_SP), EA::Dat(r)));
					}
					Location::VReg(vr1, typ) => {
						let sz = Sz::L;//get_arithmetic_size(typ);
						let r1 = reg_allocator.registers[&intervals[vr1]];
						data.push(Asm::Move(sz, EA::Dat(r1), EA::Dat(r)));
					}
				}
			}

			TAC::Store { vr, loc } => {
				let r = reg_allocator.registers[&intervals[vr]];
				match loc {
					Location::Addr(addr, typ) => {
						let sz = Sz::L;//get_arithmetic_size(typ);
						data.push(Asm::Move(sz, EA::Dat(r), EA::AbL(*addr as i32)));
					}
					Location::Const(..) => panic!("Can't store into an immediate"),
					Location::Stack(idx, typ) => {
						let sz = Sz::L;//get_arithmetic_size(typ);
						let r = reg_allocator.registers[&intervals[vr]];
						data.push(Asm::Move(sz, EA::Dat(r), EA::Dsp((idx * 4) as i16, VAR_SP)));
					}
					Location::VReg(vr1, typ) => {
						let sz = Sz::L;//get_arithmetic_size(typ);
						let r1 = reg_allocator.registers[&intervals[vr1]];
						data.push(Asm::Move(sz, EA::Dat(r), EA::Dat(r1)));
					}
				}
			}

			TAC::Label(id) => {
				data.push(Asm::Label(format!("{proc_name}_{id}")));
			}

			TAC::Jump(id) => {
				data.push(Asm::Bra(format!("{proc_name}_{id}")));
			}

			TAC::JumpIf { lbl, vr } => {
				let r = reg_allocator.registers[&intervals[vr]];
				data.push(Asm::Tst(Sz::L, EA::Dat(r)));
				data.push(Asm::Bcc(Cond::EQ, format!("{proc_name}_{lbl}")));
			}

			TAC::Return(with_value) => {
				if let Some(vr) = with_value {
					let r = reg_allocator.registers[&intervals[vr]];
					if r != Data::D0 {
						data.push(Asm::Move(Sz::W, EA::Dat(r), EA::Dat(Data::D0)));
					}
				}
				data.push(Asm::Rts);
			}
		}
	}

	data
}

fn output_cmp(
	r0: Data, r1: Data, r2: Data,
	sz: Sz, cc: Cond, def: Cond,
	data: &mut Vec<Asm>,
) {
	if r0 == r1 && r0 == r2 {
		// r2 = r2 > r2
		data.push(Asm::Scc(def, EA::Dat(r2)));
	} else if r0 == r1 {
		// r2 = r0 > r0
		data.push(Asm::Scc(def, EA::Dat(r2)));
	} else if r0 == r2 {
		// r2 = r2 > r1
		data.push(Asm::Cmp(sz, EA::Dat(r1), r2));
		data.push(Asm::Scc(cc, EA::Dat(r2)));
	} else if r1 == r2 {
		// r2 = r0 > r2
		data.push(Asm::Cmp(sz, EA::Dat(r2), r0));
		data.push(Asm::Scc(cc, EA::Dat(r2)));
	} else {
		// r2 = r0 > r1
		data.push(Asm::Cmp(sz, EA::Dat(r1), r0));
		data.push(Asm::Scc(cc, EA::Dat(r2)));
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sz { B, W, L }
impl Display for Sz {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::B => write!(f, "b"),
			Self::W => write!(f, "w"),
			Self::L => write!(f, "l"),
		}
	}
}
impl From<&Sz> for u32 {
	fn from(sz: &Sz) -> u32 {
		match sz {
			Sz::B => 1,
			Sz::W => 2,
			Sz::L => 4,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Data { D0, D1, D2, D3, D4, D5, D6, D7 }
impl Display for Data {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::D0 => write!(f, "d0"),
			Self::D1 => write!(f, "d1"),
			Self::D2 => write!(f, "d2"),
			Self::D3 => write!(f, "d3"),
			Self::D4 => write!(f, "d4"),
			Self::D5 => write!(f, "d5"),
			Self::D6 => write!(f, "d6"),
			Self::D7 => write!(f, "d7"),
		}
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum Addr { A0, A1, A2, A3, A4, A5, A6, A7 }
impl Display for Addr {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::A0 => write!(f, "a0"),
			Self::A1 => write!(f, "a1"),
			Self::A2 => write!(f, "a2"),
			Self::A3 => write!(f, "a3"),
			Self::A4 => write!(f, "a4"),
			Self::A5 => write!(f, "a5"),
			Self::A6 => write!(f, "a6"),
			Self::A7 => write!(f, "a7"),
		}
	}
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum EA {
	/// Data Register Direct
	/// - Dn
	Dat(Data),
	/// Address Register Direct
	/// - An
	Adr(Addr),
	/// Address Register Indirect
	/// - (An)
	Ind(Addr),
	/// Address Register Indirect with Postincrement
	/// - (An)+
	Pst(Addr),
	/// Address Register Indirect with Predecrement
	/// - -(An)
	Pre(Addr),
	/// Address Register Indirect with Displacement
	/// - (d16,An)
	Dsp(i16,Addr),
	/// Address Register Indirect with Index (8-Bit Displacement)
	/// - (d8,An,Xn)
	Idx(i8,Addr,Addr),
	/// Absolute Short Addressing
	/// - (xxx).W
	AbW(i16),
	/// Absolute Long Addressing
	/// - (xxx).L
	AbL(i32),
	/// Immediate Data
	/// - #<xxx>
	Imm(i32),
}
impl Display for EA {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::Dat(d) => write!(f, "{d}"),
			Self::Adr(a) => write!(f, "{a}"),
			Self::Ind(a) => write!(f, "({a})"),
			Self::Pst(a) => write!(f, "({a})+"),
			Self::Pre(a) => write!(f, "-({a})"),
			Self::Dsp(d,a) => write!(f, "{d}({a})"),
			Self::Idx(d,a,x) => write!(f, "{d}({a},{x})"),
			Self::AbW(a) => write!(f, "(#${a:04X})"),
			Self::AbL(a) => write!(f, "(#${a:08X})"),
			Self::Imm(a) => write!(f, "#{a}"),
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Cond { CC, CS, EQ, F, GE, GT, HI, LE, LS, LT, NE, T }
impl Display for Cond {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::CC => write!(f, "cc"),
			Self::CS => write!(f, "cs"),
			Self::EQ => write!(f, "eq"),
			Self::F => write!(f, "f"),
			Self::GE => write!(f, "ge"),
			Self::GT => write!(f, "gt"),
			Self::HI => write!(f, "hi"),
			Self::LE => write!(f, "le"),
			Self::LS => write!(f, "ls"),
			Self::LT => write!(f, "lt"),
			Self::NE => write!(f, "ne"),
			Self::T => write!(f, "t"),
		}
	}
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Asm {
	Label(String),

	Add1(Sz,EA,Data),
	/// Syntax: `ADD.<sz> Dn,<ea>`
	///
	/// Size: (Byte, Word, Long)
	Add2(Sz,Data,EA),
	/// Syntax: `AND.<sz> <ea>,Dn`
	///
	/// Size: (Byte, Word, Long)
	And1(Sz,EA,Data),
	/// Syntax: `AND.<sz> Dn,<ea>`
	///
	/// Size: (Byte, Word, Long)
	And2(Sz,Data,EA),
	Asl(Data,Data),
	Asr(Data,Data),
	Bcc(Cond,String),
	Bra(String),
	Clr(Sz,EA),
	Cmp(Sz,EA,Data),
	CmpI(Sz,i32,EA),
	DivS(EA,Data),
	DivU(EA,Data),
	Eor1(Sz,EA,Data),
	Eor2(Sz,Data,EA),
	Lsl(Data,Data),
	Lsr(Data,Data),
	Move(Sz,EA,EA),
	MulS(EA,Data),
	MulU(EA,Data),
	Neg(Sz,EA),
	Nop,
	Not(Sz,EA),
	Or1(Sz,EA,Data),
	Or2(Sz,Data,EA),
	Rts,
	Scc(Cond,EA),
	Sub1(Sz,EA,Data),
	/// Syntax: `SUB.<sz> Dn,<ea>`
	///
	/// Size: (Byte, Word, Long)
	Sub2(Sz,Data,EA),
	Swap(Data),
	Trap(u8),
	Tst(Sz,EA),
}

impl Asm {
	fn add(sz: Sz, dx: Data, dy: Data) -> Self {
		Self::Add1(sz, EA::Dat(dx), dy)
	}
	fn and(sz: Sz, dx: Data, dy: Data) -> Self {
		Self::And1(sz, EA::Dat(dx), dy)
	}

	fn or(sz: Sz, dx: Data, dy: Data) -> Self {
		Self::Or1(sz, EA::Dat(dx), dy)
	}

	fn eor(sz: Sz, dx: Data, dy: Data) -> Self {
		Self::Eor1(sz, EA::Dat(dx), dy)
	}

	fn sub(sz: Sz, dx: Data, dy: Data) -> Self {
		Self::Sub1(sz, EA::Dat(dx), dy)
	}
}

impl Display for Asm {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::Label(label) => write!(f, "{label}:"),

			Self::Add1(sz,ea,d) => write!(f, "\tadd.{sz} {ea},{d}"),
			Self::Add2(sz,d,ea) => write!(f, "\tadd.{sz} {d},{ea}"),
			Self::And1(sz,ea,d) => write!(f, "\tand.{sz} {ea},{d}"),
			Self::And2(sz,d,ea) => write!(f, "\tand.{sz} {d},{ea}"),
			Self::Asl(dx,dy) => write!(f, "\tasl {dx},{dy}"),
			Self::Asr(dx,dy) => write!(f, "\tasr {dx},{dy}"),
			Self::Bcc(cond,label) => write!(f, "\tb{cond} {label}"),
			Self::Bra(label) => write!(f, "\tbra {label}"),
			Self::Clr(sz,ea) => write!(f, "\tcls.{sz} {ea}"),
			Self::Cmp(sz,ea,d) => write!(f, "\tcmp.{sz} {ea},{d}"),
			Self::CmpI(sz,i,ea) => write!(f, "\tcmpi.{sz} #{i},{ea}"),
			Self::DivS(ea,d) => write!(f, "\tdivs {ea},{d}"),
			Self::DivU(ea,d) => write!(f, "\tdivu {ea},{d}"),
			Self::Eor1(sz,ea,d) => write!(f, "\teor.{sz} {ea},{d}"),
			Self::Eor2(sz,d,ea) => write!(f, "\teor.{sz} {d},{ea}"),
			Self::Lsl(dx,dy) => write!(f, "\tlsl {dx},{dy}"),
			Self::Lsr(dx,dy) => write!(f, "\tlsr {dx},{dy}"),
			Self::Move(sz,src,dst) => write!(f, "\tmove.{sz} {src},{dst}"),
			Self::MulS(ea,d) => write!(f, "\tmuls {ea},{d}"),
			Self::MulU(ea,d) => write!(f, "\tmulu {ea},{d}"),
			Self::Neg(sz,reg) => write!(f, "\tneg.{sz} {reg}"),
			Self::Nop => write!(f, "\tnop"),
			Self::Not(sz,reg) => write!(f, "\tnot.{sz} {reg}"),
			Self::Or1(sz,ea,d) => write!(f, "\tor.{sz} {ea},{d}"),
			Self::Or2(sz,d,ea) => write!(f, "\tor.{sz} {d},{ea}"),
			Self::Rts => write!(f, "\trts"),
			Self::Scc(cond,ea) => write!(f, "\ts{cond} {ea}"),
			Self::Sub1(sz,ea,d) => write!(f, "\tsub.{sz} {ea},{d}"),
			Self::Sub2(sz,d,ea) => write!(f, "\tsub.{sz} {d},{ea}"),
			Self::Swap(d) => write!(f, "\tswap {d}"),
			Self::Trap(v) => write!(f, "\ttrap #{v}"),
			Self::Tst(sz,ea) => write!(f, "\ttst.{sz} {ea}"),
		}
	}
}

#[cfg(test)]
mod tests {
	use crate::{input, layout, lexer, packing, parser, tac, type_checker};
	use crate::identifier::{Identifier, Map as IdentMap};
	use super::*;

	#[derive(Debug, Default)]
	struct M68kEmu {
		pc: usize,
		d: [u32;8],
		a: [u32;8],
		x: bool,
		n: bool,
		z: bool,
		v: bool,
		c: bool,
		mem: HashMap<u32, u32>,
		labels: HashMap<String, usize>,
	}

	impl M68kEmu {
		fn flags(&self, cc: Cond) -> bool {
			match cc {
				Cond::CC => !self.c,
				Cond::CS => self.c,
				Cond::EQ => self.z,
				Cond::F => false,
				Cond::GE => self.n && self.v || !self.n && !self.v,
				Cond::GT => self.n && self.v && !self.z || !self.v && !self.v && !self.z,
				Cond::HI => !self.c && !self.z,
				Cond::LE => self.z || self.n && !self.v || !self.n && self.v,
				Cond::LS => self.c || self.z,
				Cond::LT => self.n && !self.v || !self.n && self.v,
				Cond::NE => !self.z,
				Cond::T => true,
			}
		}

		fn set_n(&mut self, sz: &Sz, res: u32) {
			self.n = match sz {
				Sz::B => res & 0x80,
				Sz::W => res & 0x8000,
				Sz::L => res & 0x80000000,
			} != 0;
		}

		fn get_src(&mut self, sz: &Sz, ea: &EA) -> u32 {
			match ea {
				EA::Dat(d) => self.d[*d as usize],
				EA::Adr(_) => unreachable!(),
				EA::Ind(a) => {
					let adr = self.a[*a as usize];
					self.mem[&adr]
				}
				EA::Pst(a) => {
					let offset: u32 = sz.into();
					let adr = &mut self.a[*a as usize];
					let src = self.mem[adr];
					*adr += offset;
					src
				}
				EA::Pre(a) => {
					let offset: u32 = sz.into();
					let adr = &mut self.a[*a as usize];
					*adr -= offset;
					self.mem[adr]
				}
				EA::Dsp(dsp,a) => {
					let adr = self.a[*a as usize];
					self.mem[&adr.wrapping_add_signed(*dsp as i32)]
				}
				EA::Idx(dsp,a,x) => {
					let adr = self.a[*a as usize];
					let loc = adr.wrapping_add_signed(*dsp as i32)
							.wrapping_add(self.a[*x as usize]);
					self.mem[&loc]
				}
				EA::AbL(imm) => self.mem[&(*imm as u32)],
				EA::AbW(imm) => self.mem[&(*imm as u32)],
				EA::Imm(imm) => *imm as u32,
			}
		}

		fn get_dst(&mut self, sz: &Sz, ea: &EA) -> &mut u32 {
			match ea {
				EA::Dat(_) => unreachable!(),
				EA::Adr(_) => unreachable!(),
				EA::Ind(a) => {
				let adr = self.a[*a as usize];
				self.mem.entry(adr).or_default()
			}
				EA::Pst(a) => {
				let offset: u32 = sz.into();
				let adr = &mut self.a[*a as usize];
				let dst = self.mem.entry(*adr)
						.or_default();
				*adr += offset;
				dst
			}
				EA::Pre(a) => {
				let offset: u32 = sz.into();
				let adr = &mut self.a[*a as usize];
				*adr -= offset;
				self.mem.entry(*adr).or_default()
			}
				EA::Dsp(dsp,a) => {
				let adr = self.a[*a as usize];
				self.mem.entry(adr.wrapping_add_signed(*dsp as i32))
						.or_default()
			}
				EA::Idx(dsp,a,x) => {
				let adr = self.a[*a as usize];
				let loc = adr.wrapping_add_signed(*dsp as i32)
						.wrapping_add(self.a[*x as usize]);
				self.mem.entry(loc).or_default()
			}
				EA::AbL(imm) => self.mem
					.entry(*imm as u32)
					.or_default(),
				EA::AbW(imm) => self.mem
					.entry(*imm as u32)
					.or_default(),
				EA::Imm(_) => unreachable!(),
			}
		}

		fn get_mv_src(&mut self, sz: &Sz, ea: &EA) -> u32 {
			match ea {
				EA::Dat(d) => self.d[*d as usize],
				EA::Adr(a) => self.a[*a as usize],
				EA::Ind(a) => {
					let adr = self.a[*a as usize];
					self.mem[&adr]
				}
				EA::Pst(a) => {
					let offset: u32 = sz.into();
					let adr = &mut self.a[*a as usize];
					let src = self.mem[adr];
					*adr += offset;
					src
				}
				EA::Pre(a) => {
					let offset: u32 = sz.into();
					let adr = &mut self.a[*a as usize];
					*adr -= offset;
					self.mem[adr]
				}
				EA::Dsp(dsp,a) => {
					let adr = self.a[*a as usize];
					self.mem[&adr.wrapping_add_signed(*dsp as i32)]
				}
				EA::Idx(dsp,a,x) => {
					let adr = self.a[*a as usize];
					let loc = adr.wrapping_add_signed(*dsp as i32)
							.wrapping_add(self.a[*x as usize]);
					self.mem[&loc]
				}
				EA::AbL(imm) => self.mem[&(*imm as u32)],
				EA::AbW(imm) => self.mem[&(*imm as u32)],
				EA::Imm(imm) => *imm as u32,
			}
		}

		fn get_mv_dst(&mut self, sz: &Sz, ea: &EA) -> &mut u32 {
			match ea {
				EA::Dat(d) => &mut self.d[*d as usize],
				EA::Adr(a) => &mut self.a[*a as usize],
				EA::Ind(a) => {
					let adr = self.a[*a as usize];
					self.mem.entry(adr).or_default()
				}
				EA::Pst(a) => {
					let offset: u32 = sz.into();
					let adr = &mut self.a[*a as usize];
					let dst = self.mem.entry(*adr)
							.or_default();
					*adr += offset;
					dst
				}
				EA::Pre(a) => {
					let offset: u32 = sz.into();
					let adr = &mut self.a[*a as usize];
					*adr -= offset;
					self.mem.entry(*adr).or_default()
				}
				EA::Dsp(dsp,a) => {
					let adr = self.a[*a as usize];
					self.mem.entry(adr.wrapping_add_signed(*dsp as i32))
							.or_default()
				}
				EA::Idx(dsp,a,x) => {
					let adr = self.a[*a as usize];
					let loc = adr.wrapping_add_signed(*dsp as i32)
							.wrapping_add(self.a[*x as usize]);
					self.mem.entry(loc).or_default()
				}
				EA::AbL(imm) => self.mem
						.entry(*imm as u32)
						.or_default(),
				EA::AbW(imm) => self.mem
						.entry(*imm as u32)
						.or_default(),
				EA::Imm(_) => unreachable!(),
			}
		}

		fn get_scc_dst(&mut self, sz: &Sz, ea: &EA) -> &mut u32 {
			match ea {
				EA::Dat(d) => &mut self.d[*d as usize],
				EA::Adr(_) => unreachable!(),
				EA::Ind(a) => {
					let adr = self.a[*a as usize];
					self.mem.entry(adr).or_default()
				}
				EA::Pst(a) => {
					let offset: u32 = sz.into();
					let adr = &mut self.a[*a as usize];
					let dst = self.mem.entry(*adr)
							.or_default();
					*adr += offset;
					dst
				}
				EA::Pre(a) => {
					let offset: u32 = sz.into();
					let adr = &mut self.a[*a as usize];
					*adr -= offset;
					self.mem.entry(*adr).or_default()
				}
				EA::Dsp(dsp,a) => {
					let adr = self.a[*a as usize];
					self.mem.entry(adr.wrapping_add_signed(*dsp as i32))
							.or_default()
				}
				EA::Idx(dsp,a,x) => {
					let adr = self.a[*a as usize];
					let loc = adr.wrapping_add_signed(*dsp as i32)
							.wrapping_add(self.a[*x as usize]);
					self.mem.entry(loc).or_default()
				}
				EA::AbL(imm) => self.mem
						.entry(*imm as u32)
						.or_default(),
				EA::AbW(imm) => self.mem
						.entry(*imm as u32)
						.or_default(),
				EA::Imm(_) => unreachable!(),
			}
		}
	}

	fn interpret(data: &[Asm]) -> M68kEmu {
		let mut emu = M68kEmu::default();
		emu.labels.extend(data.iter()
				.enumerate()
				.filter_map(|(idx, asm)| match asm {
					Asm::Label(label) => Some((label.to_owned(), idx)),
					_ => None,
				}));

		loop {
			let asm = &data[emu.pc];
			print!("\t{asm}\t");
			emu.pc += 1;
			match asm {
				Asm::Label(_) => {}
				Asm::Add1(sz,ea,d) => {
					let src = emu.get_src(sz, ea);
					let dst = &mut emu.d[*d as usize];
					let (res,c) = dst.overflowing_add(src);
					*dst = res;
					emu.x = c;
					emu.set_n(sz, res);
					emu.z = res == 0;
					emu.v = c;
					emu.c = c;
					print!("; {d} = {res}");
				}
				Asm::Add2(sz,d,ea) => {
					let src = emu.d[*d as usize];
					let dst = emu.get_dst(sz, ea);
					let (res,c) = dst.overflowing_add(src);
					*dst = res;
					emu.x = c;
					emu.set_n(sz, res);
					emu.z = res == 0;
					emu.v = c;
					emu.c = c;
					print!("; {ea} = {res}");
				}
				Asm::And1(sz,ea,d) => {
					let src = emu.get_src(sz, ea);
					let dst = &mut emu.d[*d as usize];
					let res = *dst & src;
					*dst = res;
					emu.set_n(sz, res);
					emu.z = res == 0;
					emu.v = false;
					emu.c = false;
					print!("; {d} = {res}");
				}
				Asm::And2(sz,d,ea) => {
					let src = emu.d[*d as usize];
					let dst = emu.get_dst(sz, ea);
					let res = *dst & src;
					*dst = res;
					emu.set_n(sz, res);
					emu.z = res == 0;
					emu.v = false;
					emu.c = false;
					print!("; {ea} = {res}");
				}
				Asm::Asl(s,d) => {
					let src = emu.d[*s as usize] & 0x3F;
					let dst = &mut emu.d[*d as usize];
					let res = *dst << src;
					emu.x = if src != 0 {
						(*dst & 0x80000000) != 0
					} else {
						emu.x
					};
					emu.n = (res & 0x80000000) != 0;
					emu.z = res == 0;
					// overflow bit: set if the MSB is changed at any time during the shift, cleared otherwise
					emu.v = (*dst & !(0xFFFFFFFFu32 >> src)) != 0;
					emu.c = src != 0 && (*dst & 0x80000000) != 0;
					*dst = res;
					print!("; {d} = {res}");
				}
				Asm::Asr(s,d) => {
					let src = emu.d[*s as usize] & 0x3F;
					let dst = &mut emu.d[*d as usize];
					let res = *dst >> src;
					emu.x = if src != 0 {
						(*dst & 0x80000000) != 0
					} else {
						emu.x
					};
					emu.n = (res & 0x80000000) != 0;
					emu.z = res == 0;
					emu.v = false;
					emu.c = src != 0 && (*dst & 0x80000000) != 0;
					*dst = res;
					print!("; {d} = {res}");
				}
				Asm::Bcc(cc,label) => {
					if emu.flags(*cc) {
						emu.pc = emu.labels[label];
					}
				}
				Asm::Bra(label) => {
					emu.pc = emu.labels[label];
				}
				Asm::Clr(sz,ea) => {
					*emu.get_dst(sz, ea) = 0;
					emu.n = false;
					emu.z = true;
					emu.v = false;
					emu.c = false;
					print!("; {ea} = 0");
				}
				Asm::Cmp(sz,ea,d) => {
					let src = emu.get_src(sz, ea);
					let dst = emu.d[*d as usize];
					let (res,c) = dst.overflowing_sub(src);
					emu.set_n(sz, res);
					emu.z = res == 0;
					emu.v = c;
					emu.c = c;
				}
				Asm::CmpI(sz,imm,ea) => {
					let src = *imm;
					let dst = *emu.get_dst(sz, ea);
					let (res,c) = dst.overflowing_sub_signed(src);
					emu.set_n(sz, res);
					emu.z = res == 0;
					emu.v = c;
					emu.c = c;
				}
				Asm::DivS(ea,d) => {
					let src = emu.get_src(&Sz::L, ea);
					let dst = &mut emu.d[*d as usize];
					let (res, c) = dst.overflowing_div(src);
					*dst = res;
					emu.set_n(&Sz::L, res);
					emu.z = res == 0;
					emu.v = c;
					emu.c = false;
					print!("; {d} = {res}");
				}
				Asm::DivU(ea,d) => {
					let src = emu.get_src(&Sz::L, ea);
					let dst = &mut emu.d[*d as usize];
					let (res, c) = dst.overflowing_div(src);
					*dst = res;
					emu.set_n(&Sz::L, res);
					emu.z = res == 0;
					emu.v = c;
					emu.c = false;
					print!("; {d} = {res}");
				}
				Asm::Eor1(sz,ea,d) => {
					let src = emu.get_src(sz, ea);
					let dst = &mut emu.d[*d as usize];
					let res = *dst ^ src;
					*dst = res;
					emu.set_n(sz, res);
					emu.z = res == 0;
					emu.v = false;
					emu.c = false;
					print!("; {d} = {res}");
				}
				Asm::Eor2(sz,d,ea) => {
					let src = emu.d[*d as usize];
					let dst = emu.get_dst(sz, ea);
					let res = *dst ^ src;
					*dst = res;
					emu.set_n(sz, res);
					emu.z = res == 0;
					emu.v = false;
					emu.c = false;
					print!("; {ea} = {res}");
				}
				Asm::Lsl(s,d) => {
					let src = emu.d[*s as usize] & 0x3F;
					let dst = &mut emu.d[*d as usize];
					let res = *dst << src;
					*dst = res;
					let bit = *dst >> (32 - src);
					emu.x = if src != 0 { bit != 0 } else { emu.x };
					emu.set_n(&Sz::L, res);
					emu.z = res == 0;
					emu.v = false;
					emu.c = src != 0 && bit != 0;
					print!("; {d} = {res}");
				}
				Asm::Lsr(s,d) => {
					let src = emu.d[*s as usize] & 0x3F;
					let dst = &mut emu.d[*d as usize];
					let res = *dst >> src;
					*dst = res;
					let bit = *dst << (32 - src);
					emu.x = if src != 0 { bit != 0 } else { emu.x };
					emu.set_n(&Sz::L, res);
					emu.z = res == 0;
					emu.v = false;
					emu.c = src != 0 && bit != 0;
					print!("; {d} = {res}");
				}
				Asm::Move(sz,eas,ead) => {
					// TODO - srenshaw - `get_mv_src` doesn't fully match MOVE addressing modes
					let src = emu.get_mv_src(sz, eas);
					let dst = emu.get_mv_dst(sz, ead);
					*dst = src;
					emu.set_n(sz, src);
					emu.z = src == 0;
					emu.v = false;
					emu.c = false;
					print!("; {ead} = {src}");
				}
				Asm::MulS(ea,d) => {
					let src = emu.get_src(&Sz::L, ea);
					let dst = &mut emu.d[*d as usize];
					let res = *dst * src;
					*dst = res;
					emu.set_n(&Sz::L, res);
					emu.z = res == 0;
					emu.v = false;
					emu.c = false;
					print!("; {d} = {res}");
				}
				Asm::MulU(ea,d) => {
					let src = emu.get_src(&Sz::L, ea);
					let dst = &mut emu.d[*d as usize];
					let res = *dst * src;
					*dst = res;
					emu.set_n(&Sz::L, res);
					emu.z = res == 0;
					emu.v = false;
					emu.c = false;
					print!("; {d} = {res}");
				}
				Asm::Neg(sz,ea) => {
					let dst = emu.get_dst(sz, ea);
					let (res, c) = 0u32.overflowing_sub(*dst);
					*dst = res;
					emu.x = res != 0;
					emu.set_n(sz, res);
					emu.z = res == 0;
					emu.v = c;
					emu.c = res != 0;
					print!("; {ea} = {res}");
				}
				Asm::Not(sz,ea) => {
					let dst = emu.get_dst(sz, ea);
					*dst = !*dst;
					let res = *dst;
					emu.set_n(sz, res);
					emu.z = res == 0;
					emu.v = false;
					emu.c = false;
					print!("; {ea} = {res}");
				}
				Asm::Nop => {}
				Asm::Or1(sz,ea,d) => {
					let src = emu.get_src(sz, ea);
					let dst = &mut emu.d[*d as usize];
					let res = *dst | src;
					*dst = res;
					emu.set_n(sz, res);
					emu.z = res == 0;
					emu.v = false;
					emu.c = false;
					print!("; {d} = {res}");
				}
				Asm::Or2(sz,d,ea) => {
					let src = emu.d[*d as usize];
					let dst = emu.get_dst(sz, ea);
					let res = *dst | src;
					*dst = res;
					emu.set_n(sz, res);
					emu.z = res == 0;
					emu.v = false;
					emu.c = false;
					print!("; {ea} = {res}");
				}
				Asm::Rts => {
					println!();
					break
				}
				Asm::Scc(cc,ea) => {
					let c = emu.flags(*cc);
					let dst = emu.get_scc_dst(&Sz::B, ea);
					*dst = if c { 0xFF } else { 0x00 };
					print!("; {ea} = {dst}");
				}
				Asm::Sub1(sz,ea,d) => {
					let src = emu.get_src(sz, ea);
					let dst = &mut emu.d[*d as usize];
					let (res, c) = dst.overflowing_sub(src);
					*dst = res;
					emu.x = c;
					emu.set_n(sz, res);
					emu.z = res == 0;
					emu.v = c;
					emu.c = c;
					print!("; {d} = {res}");
				}
				Asm::Sub2(sz,d,ea) => {
					let src = emu.d[*d as usize];
					let dst = emu.get_dst(sz, ea);
					let (res,c) = dst.overflowing_sub(src);
					*dst = res;
					emu.x = c;
					emu.set_n(sz, res);
					emu.z = res == 0;
					emu.v = c;
					emu.c = c;
					print!("; {ea} = {res}");
				}
				Asm::Swap(d) => {}
				Asm::Tst(sz,ea) => {}
				Asm::Trap(v) => {}
			}
			println!();
		}

		emu
	}

	fn setup(source: &str) -> IdentMap<Vec<Asm>> {
		let mut source_str = String::new();
		source_str.push_str("region Stack[0] @ 0;");
		source_str.push_str(source);

		let input = input::eval(file!().to_string(), source_str.into());

		let lex_data = lexer::eval(&input.source)
				.unwrap_or_else(|e| panic!("{}", e.display(&input)));

		let prs_data = parser::eval(&input, &lex_data, false)
				.unwrap_or_else(|e| panic!("{}", e.display(&input)));

		let typ_data = type_checker::eval(&input, &lex_data, &prs_data)
				.unwrap_or_else(|e| panic!("{}", e.display(&input)));

		let pak_data = packing::eval(&prs_data);

		let loc_data = layout::eval(&prs_data, &pak_data)
				.unwrap_or_else(|e| panic!("{}", e.display(&input, &lex_data)));

		let tac_data = tac::eval(&prs_data, &typ_data, &pak_data, &loc_data)
				.map_err(|e| e.into_comp_error(&input, &lex_data, &prs_data.procedures))
				.unwrap_or_else(|e| panic!("{}", e.display(&input)));

		let mut out = IdentMap::<Vec<Asm>>::default();

		for (proc_id, tac) in tac_data {
			let proc_name = lex_data.text(&input, &proc_id).to_owned();
			out.insert(proc_id, lower(&proc_name, tac));
		}

		out
	}

	#[test]
	fn set_stack_value() {
		let data = setup("main {
			let a: s8 = (2 + 3) * (3 - 1);
		}");
		let main_proc = &data[&"main".id()];
		let emu = interpret(main_proc);
		assert_eq!(emu.mem.len(), 1);
		assert_eq!(emu.mem[&0], 10);
	}

	#[test]
	fn for_loop() {
		let data = setup("main {
			let b: s8 = 4;
			let c: s8 = 0;
			for i in [0..10] {
				c += b * 2;
			}
		}");
		let main_proc = &data[&"main".id()];
		let emu = interpret(main_proc);
		assert_eq!(emu.mem.len(), 2);
		assert_eq!(emu.mem[&0], 4);
		assert_eq!(emu.mem[&4], 80);
	}
}
