
use std::collections::HashMap;

use super::{Asm, Ins};
use super::{setup_div16, setup_div32};

#[derive(Debug, Default)]
pub struct Sh2Emu {
	r: [u32;16],
	m: bool,
	q: bool,
	s: bool,
	t: bool,
	gbr: u32,
	vbr: u32,
	mach: u32,
	macl: u32,
	pr: u32,
	pc: usize,

	pub mem: HashMap<u32, u32>,
	labels: HashMap<String, usize>,
}

impl Sh2Emu {
	pub fn interpret(&mut self, data: &[Asm]) {
		self.labels.extend(data.iter()
				.enumerate()
				.filter_map(|(idx, asm)| match asm {
					Asm::Label(label) => Some((label.to_owned(), idx)),
					_ => None,
				}));

		let mut pipeline = &data[0];
		self.pc += 2;

		loop {
			if (self.pc >> 1) > data.len() + 20 {
				panic!("segmentation fault! attempting to run code at {}", self.pc);
			}

			let asm = pipeline;
			pipeline = data.get(self.pc >> 1)
					.unwrap_or(&Asm::Ins(Ins::Nop));
			eprint!("[{:4}]\t{asm}\t", self.pc >> 1);

			match asm {
				Asm::MovWI(..) |
				Asm::MovLI(..) |
				Asm::BF(_) |
				Asm::BFS(_) |
				Asm::BT(_) |
				Asm::BTS(_) |
				Asm::Bra(_) |
				Asm::Bsr(_) |
				Asm::Table |
				Asm::Label(_) => panic!("{asm}"),

				Asm::Comment(_) => {}

				Asm::Ins(ins) => match ins {
					Ins::Word(_) => panic!("{asm}"),

					Ins::Add(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						self.r[*n as usize] = rn.wrapping_add(rm);
					}

					Ins::AddI(i, n) => {
						let rn = self.r[*n as usize];
						let i = if *i < 0 {
							*i as i32
						} else {
							*i as u32 as i32
						};
						self.r[*n as usize] = rn.wrapping_add_signed(i);
					}

					Ins::AddC(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						let tmp1 = rn.wrapping_add(rm);
						let tmp0 = rn;
						self.r[*n as usize] = tmp1.wrapping_add(self.t as u32);
						self.t = tmp0 > tmp1;
						if tmp1 > self.r[*n as usize] {
							self.t = true;
						}
					}

					Ins::AddV(m, n) => {
						let mut rn = self.r[*n as usize];
						let rm = self.r[*m as usize];

						let dst = (rn as i32) < 0;
						let src = (rm as i32) < 0;
						rn = rm.wrapping_add(rm);
						let ans = (rn as i32) < 0;
						self.t = !(src ^ dst) && (ans ^ dst);
						self.r[*n as usize] = rn;
					}

					Ins::And(m, n) => {
						let rm = self.r[*m as usize];
						self.r[*n as usize] &= rm;
					}

					Ins::AndI(i) => {
						self.r[0] &= *i as u32;
					}

					Ins::AndM(i) => {
						let addr = self.gbr + self.r[0];
						self.mem.insert(addr, self.mem[&addr] & *i as u32);
					}

					Ins::BF(d) => {
						if !self.t {
							let disp = *d as isize;
							self.pc = self.pc.wrapping_add_signed(disp);
							pipeline = &data[(self.pc + 2) >> 1];
							continue;
						}
					}

					Ins::BFS(d) => {
						if !self.t {
							let disp = *d as isize;
							self.pc = self.pc.wrapping_add_signed(disp);
							continue;
						}
					}

					Ins::Bra(d) => {
						let disp = if (*d & 0x800) > 0 {
							0xF000 | *d as u16
						} else {
							0x0FFF & *d as u16
						} as usize;
						self.pc += disp << 1;
					}

					Ins::BraF(m) => {
						self.pc += self.r[*m as usize] as usize;
					}

					Ins::Bsr(d) => {
						let disp = if (*d & 0x800) > 0 {
							0xF000 | *d as u16
						} else {
							0x0FFF & *d as u16
						} as usize;
						self.pc += disp << 1;
					}

					Ins::BsrF(m) => {
						self.pr = self.pc as u32;
						self.pc += self.r[*m as usize] as usize;
					}

					Ins::BT(d) => {
						if self.t {
							let disp = *d as isize;
							self.pc = self.pc.wrapping_add_signed(disp);
							pipeline = &data[(self.pc + 2) >> 1];
							continue;
						}
					}

					Ins::BTS(d) => {
						if self.t {
							let disp = *d as isize;
							self.pc = self.pc.wrapping_add_signed(disp);
							continue;
						}
					}

					Ins::ClrMac => {
						self.mach = 0;
						self.macl = 0;
					}

					Ins::ClrT => {
						self.t = false;
					}

					Ins::CmpEQ(m, n) => {
						self.t = self.r[*n as usize] == self.r[*m as usize];
					}

					Ins::CmpGE(m, n) => {
						self.t = self.r[*n as usize] as i32 >= self.r[*m as usize] as i32;
					}

					Ins::CmpGT(m, n) => {
						self.t = self.r[*n as usize] as i32 > self.r[*m as usize] as i32;
					}

					Ins::CmpHI(m, n) => {
						self.t = self.r[*n as usize] > self.r[*m as usize];
					}

					Ins::CmpHS(m, n) => {
						self.t = self.r[*n as usize] >= self.r[*m as usize];
					}

					Ins::CmpPL(n) => {
						self.t = self.r[*n as usize] as i32 > 0;
					}

					Ins::CmpPZ(n) => {
						self.t = self.r[*n as usize] as i32 >= 0;
					}

					Ins::CmpStr(m, n) => {
						let temp = self.r[*n as usize] ^ self.r[*m as usize];
						let hh = ((temp >> 24) & 0xFF) == 0;
						let hl = ((temp >> 16) & 0xFF) == 0;
						let lh = ((temp >> 8) & 0xFF) == 0;
						let ll = (temp & 0xFF) == 0;
						self.t = hh || hl || lh || ll;
					}

					Ins::CmpIM(i) => {
						let imm = if *i < 0 {
							0xFFFFFF00 | *i as u32
						} else {
							0x000000FF & *i as u32
						};
						self.t = self.r[0] == imm;
					}

					Ins::Div0S(m, n) => {
						self.q = (self.r[*n as usize] as i32) < 0;
						self.m = (self.r[*m as usize] as i32) < 0;
						self.t = self.m != self.q;
					}

					Ins::Div0U => {
						self.q = false;
						self.m = false;
						self.t = false;
					}

					Ins::Div1(m, n) => {
						let mut rn = self.r[*n as usize];
						let rm = self.r[*m as usize];

						let old_q = self.q;
						self.q = (rn as i32) < 0;
						rn <<= 1;
						rn |= self.t as u32;

						let tmp0 = rn;
						let tmp1 = if old_q == self.m {
							rn = rn.wrapping_sub(rm);
							rn > tmp0
						} else {
							rn = rn.wrapping_add(rm);
							rn < tmp0
						};

						self.q ^= self.m ^ tmp1;

						self.r[*n as usize] = rn;
						self.t = self.q == self.m;
					}

					Ins::DMulS(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];

						let tn = rn as i32;
						let tm = rm as i32;
						let tn = if tn < 0 { -tn } else { tn } as u32;
						let tm = if tm < 0 { -tm } else { tm } as u32;
						let f_nl_ml = if ((rn ^ rm) as i32) < 0 { -1 } else { 0 };

						let rnl = tn & 0x0000FFFF;
						let rnh = (tn >> 16) & 0x0000FFFF;
						let rml = tm & 0x0000FFFF;
						let rmh = (tm >> 16) & 0x0000FFFF;

						let temp0 = rml * rnl;
						let temp1 = rmh * rnl;
						let temp2 = rml * rnh;
						let temp3 = rmh * rnh;

						let res1 = temp1 + temp2;
						let res2 = if res1 < temp1 { 0x00010000 } else { 0 };

						let temp1 = (res1 << 16) & 0xFFFF0000;
						let res0 = temp0 + temp1;
						let res2 = res2 + if res0 < temp0 { 1 } else { 0 };

						let res2 = res2 + ((res1 >> 16) & 0x0000FFFF) + temp3;

						let (res2, res0) = if f_nl_ml < 0 {
							if res0 == 0 {
								(!res2 + 1, res0)
							} else {
								(!res2, !res0 + 1)
							}
						} else {
							(res2, res0)
						};

						self.mach = res2;
						self.macl = res0;
					}

					Ins::DMulU(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];

						let rnl = rn & 0x0000FFFF;
						let rnh = (rn >> 16) & 0x0000FFFF;

						let rml = rm & 0x0000FFFF;
						let rmh = (rm >> 16) & 0x0000FFFF;

						let temp0 = rml * rnl;
						let temp1 = rmh * rnl;
						let temp2 = rml * rnh;
						let temp3 = rmh * rnh;

						let res1 = temp1 + temp2;
						let res2 = if res1 < temp1 { 0x00010000 } else { 0 };

						let temp1 = (res1 << 16) & 0xFFFF0000;
						let res0 = temp0 + temp1;
						let res2 = res2 + if res0 < temp0 { 1 } else { 0 };

						let res2 = res2 + ((res1 >> 16) & 0x0000FFFF) + temp3;

						self.mach = res2;
						self.macl = res0;
					}

					Ins::DT(n) => {
						let mut rn = self.r[*n as usize];
						rn = rn.wrapping_sub(1);
						self.r[*n as usize] = rn;
						self.t = rn == 0;
					}

					Ins::ExtSB(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						self.r[*n as usize] = if (rm as i8) < 0 {
							rn | 0xFFFFFF00
						} else {
							rn & 0x000000FF
						};
					}

					Ins::ExtSW(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						self.r[*n as usize] = if (rm as i16) < 0 {
							rn | 0xFFFF0000
						} else {
							rn & 0x0000FFFF
						};
					}

					Ins::ExtUB(m, n) => {
						self.r[*n as usize] = self.r[*m as usize] & 0xFF;
					}

					Ins::ExtUW(m, n) => {
						self.r[*n as usize] = self.r[*m as usize] & 0xFFFF;
					}

					Ins::Jmp(m) => {
						self.pc = self.r[*m as usize] as usize;
						continue;
					}

					Ins::Jsr(m) => {
						self.pr = self.pc as u32;
						self.pc = self.r[*m as usize] as usize;
						continue;
					}

					Ins::LdSr(m) => {
						let rm = self.r[*m as usize];
						self.set_sr(rm);
					}

					Ins::LdGbr(m) => {
						self.gbr = self.r[*m as usize];
					}

					Ins::LdVbr(m) => {
						self.vbr = self.r[*m as usize];
					}

					Ins::LdMSr(m) => {
						let rm = self.r[*m as usize];
						self.set_sr(self.mem[&rm]);
					}

					Ins::LdMGbr(m) => {
						let rm = self.r[*m as usize];
						self.gbr = self.mem[&rm];
						self.r[*m as usize] += 4;
					}

					Ins::LdMVbr(m) => {
						let rm = self.r[*m as usize];
						self.vbr = self.mem[&rm];
						self.r[*m as usize] += 4;
					}

					Ins::LdMach(m) => {
						self.mach = self.r[*m as usize];
					}

					Ins::LdMacl(m) => {
						self.macl = self.r[*m as usize];
					}

					Ins::LdPr(m) => {
						self.pr = self.r[*m as usize];
					}

					Ins::LdMMach(m) => {
						let rm = self.r[*m as usize];
						self.mach = self.mem[&rm];
						self.r[*m as usize] += 4;
					}

					Ins::LdMMacl(m) => {
						let rm = self.r[*m as usize];
						self.macl = self.mem[&rm];
						self.r[*m as usize] += 4;
					}

					Ins::LdMPr(m) => {
						let rm = self.r[*m as usize];
						self.pr = self.mem[&rm];
						self.r[*m as usize] += 4;
					}

					Ins::MacL(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];

						let tempn = self.mem[&rn] as i32;
						self.r[*n as usize] += 4;
						let tempm = self.mem[&rm] as i32;
						self.r[*m as usize] += 4;

						let f_nl_ml = if (tempn ^ tempm) < 0 { -1 } else { 0 };
						let tempn = if tempn < 0 { -tempn } else { tempn };
						let tempm = if tempm < 0 { -tempm } else { tempm };

						let temp1 = tempn as u32;
						let temp2 = tempm as u32;

						let rnl = temp1 & 0x0000FFFF;
						let rnh = (temp1 >> 16) & 0x0000FFFF;
						let rml = temp2 & 0x0000FFFF;
						let rmh = (temp2 >> 16) & 0x0000FFFF;

						let temp0 = rml * rnl;
						let temp1 = rmh * rnl;
						let temp2 = rml * rnh;
						let temp3 = rmh * rnh;

						let res1 = temp1 + temp2;
						let res2 = if res1 < temp1 { 0x00010000 } else { 0 };

						let temp1 = (res1 << 16) & 0xFFFF0000;
						let res0 = temp0 + temp1;
						let res2 = res2 + if res0 < temp0 { 1 } else { 0 };

						let res2 = res2 + ((res1 >> 16) & 0x0000FFFF) + temp3;

						let (res2, res0) = if f_nl_ml < 0 {
							(!res2, (-(res0 as i32)) as u32)
						} else {
							(res2, res0)
						};

						if self.s {
							let res0 = self.macl + res0;
							let res2 = res2 + if self.macl > res0 { 1 } else { 0 };
							let res2 = res2 + (self.mach & 0x0000FFFF);

							let (res2, res0) = if ((res2 as i32) < 0) && (res2 < 0xFFFF8000) {
								(0x00008000, 0x00000000)
							} else {
								(res2, res0)
							};
							let (res2, res0) = if ((res2 as i32) > 0) && (res2 > 0x00007FFF) {
								(0x00007FFF, 0xFFFFFFFF)
							} else {
								(res2, res0)
							};

							self.mach = res2;
							self.macl = res0;
						} else {
							let res0 = self.macl.wrapping_add(res0);
							let res2 = res2 + if self.macl > res0 { 1 } else { 0 };
							let res2 = res2.wrapping_add(self.mach);

							self.mach = res2;
							self.macl = res0;
						}
					}

					Ins::MacW(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						let tempn = self.mem[&rn] as i32;
						self.r[*n as usize] += 2;
						let tempm = self.mem[&rm] as i32;
						self.r[*m as usize] += 2;
						let temp1 = self.macl;
						let tempm = tempn as i16 as i32 * tempm as i16 as i32;
						let dest = if (self.macl as i32) < 0 { 1 } else { 0 };
						let (src, tempn) = if tempm < 0 {
							(1, 0xFFFFFFFFu32)
						} else {
							(0, 0)
						};
						let src = src + dest;
						self.macl = self.macl.wrapping_add_signed(tempm);
						let ans = if (self.macl as i32) < 0 { 1 } else { 0 };
						let ans = ans + dest;
						if self.s {
							if ans == 1 {
								if src == 0 { self.macl = 0x7FFFFFFF; }
								if src == 2 { self.macl = 0x80000000; }
							}
						} else {
							self.mach += tempn;
							if temp1 > self.macl { self.mach = self.mach.wrapping_add(1); }
						}
					}

					Ins::Mov(m, n) => {
						self.r[*n as usize] = self.r[*m as usize];
					}

					Ins::MovBS(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						self.mem.insert(rn, rm as u8 as u32);
					}

					Ins::MovWS(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						self.mem.insert(rn, rm as u16 as u32);
					}

					Ins::MovLS(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						self.mem.insert(rn, rm);
					}

					Ins::MovBL(m, n) => {
						let rm = self.r[*m as usize];
						self.r[*n as usize] = self.mem[&rm] as i8 as i32 as u32;
					}

					Ins::MovWL(m, n) => {
						let rm = self.r[*m as usize];
						self.r[*n as usize] = self.mem[&rm] as i16 as i32 as u32;
					}

					Ins::MovLL(m, n) => {
						let rm = self.r[*m as usize];
						self.r[*n as usize] = *self.mem.get(&rm)
								.unwrap_or(&0xFFFFFFFFu32);
					}

					Ins::MovBM(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						self.mem.insert(rn - 1, rm as i8 as i32 as u32);
						self.r[*n as usize] -= 1;
					}

					Ins::MovWM(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						self.mem.insert(rn - 2, rm as i16 as i32 as u32);
						self.r[*n as usize] -= 2;
					}

					Ins::MovLM(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						self.mem.insert(rn - 4, rm);
						self.r[*n as usize] -= 4;
					}

					Ins::MovBP(m, n) => {
						let rm = self.r[*m as usize];
						self.r[*n as usize] = self.mem[&rm] as i8 as i32 as u32;
						if n != m {
							self.r[*m as usize] += 1;
						}
					}

					Ins::MovWP(m, n) => {
						let rm = self.r[*m as usize];
						self.r[*n as usize] = self.mem[&rm] as i16 as i32 as u32;
						if n != m {
							self.r[*m as usize] += 2;
						}
					}

					Ins::MovLP(m, n) => {
						let rm = self.r[*m as usize];
						self.r[*n as usize] = self.mem[&rm];
						if n != m {
							self.r[*m as usize] += 4;
						}
					}

					Ins::MovBS0(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						self.mem.insert(rn + self.r[0], rm as u8 as u32);
					}

					Ins::MovWS0(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						self.mem.insert(rn + self.r[0], rm as u16 as u32);
					}

					Ins::MovLS0(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						self.mem.insert(rn + self.r[0], rm);
					}

					Ins::MovBL0(m, n) => {
						let rm = self.r[*m as usize];
						self.r[*n as usize] = self.mem[&(rm + self.r[0])] as i8 as i32 as u32;
					}

					Ins::MovWL0(m, n) => {
						let rm = self.r[*m as usize];
						self.r[*n as usize] = self.mem[&(rm + self.r[0])] as i16 as i32 as u32;
					}

					Ins::MovLL0(m, n) => {
						let rm = self.r[*m as usize];
						self.r[*n as usize] = self.mem[&(rm + self.r[0])];
					}

					Ins::MovI(i, n) => {
						self.r[*n as usize] = *i as i32 as u32;
					}

					Ins::MovWI(d, n) => {
						let disp = (*d as u32) << 1;
						let addr = (self.pc - 1 + disp as usize) >> 1;
						let Asm::Ins(Ins::Word(word)) = data[addr] else {
							panic!("expected word value @ 0x{:X}, found {}", addr << 1, data[addr])
						};
						self.r[*n as usize] = word as i16 as i32 as u32;
					}

					Ins::MovLI(d, n) => {
						let disp = (*d as u32) << 2; // disp = 27 << 2 = 108;
						let addr = ((self.pc & 0xFFFFFFFC) + disp as usize) >> 1; // addr = ((770 & 0xFC) + 108) >> 1 = (768 + 108) >> 1 = 876 >> 1 = 438
						let Asm::Ins(Ins::Word(w0)) = data[addr] else {
							panic!("expected high word value @ 0x{:}, found {}", addr, data[addr])
						};
						let Asm::Ins(Ins::Word(w1)) = data[addr + 1] else {
							panic!("expected low word value @ 0x{:X}, found {}", addr << 1, data[addr])
						};
						let long = (w0 as u32) << 16 | w1 as u32;
						self.r[*n as usize] = long;
					}

					Ins::MovBLG(d) => {
						let disp = *d as u32;
						self.r[0] = self.mem[&(self.gbr + disp)] as i8 as i32 as u32;
					}

					Ins::MovWLG(d) => {
						let disp = (*d as u32) << 1;
						self.r[0] = self.mem[&(self.gbr + disp)] as i16 as i32 as u32;
					}

					Ins::MovLLG(d) => {
						let disp = (*d as u32) << 2;
						self.r[0] = self.mem[&(self.gbr + disp)];
					}

					Ins::MovBSG(d) => {
						let disp = *d as u32;
						self.mem.insert(self.gbr + disp, self.r[0]);
					}

					Ins::MovWSG(d) => {
						let disp = (*d as u32) << 1;
						self.mem.insert(self.gbr + disp, self.r[0]);
					}

					Ins::MovLSG(d) => {
						let disp = (*d as u32) << 2;
						self.mem.insert(self.gbr + disp, self.r[0]);
					}

					Ins::MovBS4(d, n) => {
						let rn = self.r[*n as usize];
						let disp = (*d & 0xF) as u32;
						self.mem.insert(rn + disp, self.r[0]);
					}

					Ins::MovWS4(d, n) => {
						let rn = self.r[*n as usize];
						let disp = ((*d & 0xF) as u32) << 1;
						self.mem.insert(rn + disp, self.r[0]);
					}

					Ins::MovLS4(m, d, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						let disp = ((*d & 0xF) as u32) << 2;
						self.mem.insert(rn + disp, rm);
					}

					Ins::MovBL4(d, m) => {
						let rm = self.r[*m as usize];
						let disp = (*d & 0xF) as u32;
						self.r[0] = self.mem[&(rm + disp)] as i8 as i32 as u32;
					}

					Ins::MovWL4(d, m) => {
						let rm = self.r[*m as usize];
						let disp = ((*d & 0xF) as u32) << 1;
						self.r[0] = self.mem[&(rm + disp)] as i16 as i32 as u32;
					}

					Ins::MovLL4(d, m, n) => {
						let rm = self.r[*m as usize];
						let disp = ((*d & 0xF) as u32) << 2;
						self.r[*n as usize] = self.mem[&(rm + disp)];
					}

					Ins::MovA(d) => {
						let disp = *d as u32;
						self.r[0] = (self.pc as u32 & 0xFFFFFFFC) | (disp << 2);
					}

					Ins::MovT(n) => {
						self.r[*n as usize] = self.t as u32;
					}

					Ins::MulL(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						self.macl = rn * rm;
					}

					Ins::MulS(m, n) => {
						let rn = self.r[*n as usize] as i16 as i32;
						let rm = self.r[*m as usize] as i16 as i32;
						self.macl = (rn * rm) as u32;
					}

					Ins::MulU(m, n) => {
						let rn = self.r[*n as usize] as u16 as u32;
						let rm = self.r[*m as usize] as u16 as u32;
						self.macl = rn * rm;
					}

					Ins::Neg(m, n) => {
						let rm = self.r[*m as usize];
						self.r[*n as usize] = 0i32.wrapping_sub_unsigned(rm) as u32;
					}

					Ins::NegC(m, n) => {
						let rm = self.r[*m as usize];
						let (temp, c0) = 0i32.overflowing_sub(rm as i32);
						let (rn, c1) = temp.overflowing_sub(self.t as i32);
						self.r[*n as usize] = rn as u32;
						self.t = c0 || c1;
					}

					Ins::Nop => {}

					Ins::Not(m, n) => {
						self.r[*n as usize] = !self.r[*m as usize];
					}

					Ins::Or(m, n) => {
						self.r[*n as usize] |= self.r[*m as usize];
					}

					Ins::OrI(i) => {
						self.r[0] |= *i as u32;
					}

					Ins::OrM(i) => {
						let mut temp = self.mem[&(self.gbr + self.r[0])] as u8;
						temp |= *i;
						self.r[0] = temp as u32;
					}

					Ins::RotCL(n) => {
						let mut rn = self.r[*n as usize];
						let temp = (rn as i32) < 0;
						rn <<= 1;
						if self.t { rn |= 1; } else { rn &= 0xFFFFFFFE; }
						self.r[*n as usize] = rn;
						self.t = temp;
					}

					Ins::RotCR(n) => {
						let mut rn = self.r[*n as usize];
						let temp = (rn & 1) > 0;
						rn >>= 1;
						if self.t { rn |= 0x80000000; } else { rn &= 0x7FFFFFFF; }
						self.r[*n as usize] = rn;
						self.t = temp;
					}

					Ins::RotL(n) => {
						let rn = self.r[*n as usize];
						self.t = (rn as i32) < 0;
						self.r[*n as usize] = rn.rotate_left(1);
					}

					Ins::RotR(n) => {
						let rn = self.r[*n as usize];
						self.t = (rn & 1) > 0;
						self.r[*n as usize] = rn.rotate_right(1);
					}

					Ins::Rte => {
						self.pc = (self.mem[&self.r[15]] + 4) as usize;
						self.r[15] += 4;
						let sr = self.mem[&self.r[15]] & 0x3F3;
						self.r[15] += 4;
						self.m = (sr & 0x200) > 0;
						self.q = (sr & 0x100) > 0;
						self.s = (sr & 0x002) > 0;
						self.t = (sr & 0x001) > 0;
						break;
					}

					Ins::Rts => {
						self.pc = (self.pr + 4) as usize;
						eprintln!();
						break;
					}

					Ins::SetT => {
						self.t = true;
					}

					Ins::ShAL(n) | Ins::ShLL(n) => {
						let rn = self.r[*n as usize];
						self.t = (rn as i32) < 0;
						self.r[*n as usize] <<= 1;
					}

					Ins::ShAR(n) => {
						let rn = self.r[*n as usize];
						self.t = (rn & 1) > 0;
						self.r[*n as usize] = ((rn as i32) >> 1) as u32;
					}

					Ins::ShLR(n) => {
						let rn = self.r[*n as usize];
						self.t = (rn & 1) > 0;
						self.r[*n as usize] >>= 1;
					}

					Ins::ShLL2(n) => {
						self.r[*n as usize] <<= 2;
					}

					Ins::ShLL8(n) => {
						self.r[*n as usize] <<= 8;
					}

					Ins::ShLL16(n) => {
						self.r[*n as usize] <<= 16;
					}

					Ins::ShLR2(n) => {
						self.r[*n as usize] >>= 2;
					}

					Ins::ShLR8(n) => {
						self.r[*n as usize] >>= 8;
					}

					Ins::ShLR16(n) => {
						self.r[*n as usize] >>= 16;
					}

					Ins::Sleep => {
						eprintln!();
						break;
					}

					Ins::StSr(n) => {
						self.r[*n as usize] = self.get_sr();
					}

					Ins::StGbr(n) => {
						self.r[*n as usize] = self.gbr;
					}

					Ins::StVbr(n) => {
						self.r[*n as usize] = self.vbr;
					}

					Ins::StMSr(n) => {
						let mut rn = self.r[*n as usize];
						rn = rn.wrapping_sub(4);
						self.mem.insert(rn, self.get_sr());
						self.r[*n as usize] = rn;
					}

					Ins::StMGbr(n) => {
						let mut rn = self.r[*n as usize];
						rn = rn.wrapping_sub(4);
						self.mem.insert(rn, self.gbr);
						self.r[*n as usize] = rn;
					}

					Ins::StMVbr(n) => {
						let mut rn = self.r[*n as usize];
						rn = rn.wrapping_sub(4);
						self.mem.insert(rn, self.vbr);
						self.r[*n as usize] = rn;
					}

					Ins::StMach(n) => {
						self.r[*n as usize] = self.mach;
					}

					Ins::StMacl(n) => {
						self.r[*n as usize] = self.macl;
					}

					Ins::StPr(n) => {
						self.r[*n as usize] = self.pr;
					}

					Ins::StMMach(n) => {
						let mut rn = self.r[*n as usize];
						rn = rn.wrapping_sub(4);
						self.mem.insert(rn, self.mach);
						self.r[*n as usize] = rn;
					}

					Ins::StMMacl(n) => {
						let mut rn = self.r[*n as usize];
						rn = rn.wrapping_sub(4);
						self.mem.insert(rn, self.macl);
						self.r[*n as usize] = rn;
					}

					Ins::StMPr(n) => {
						let mut rn = self.r[*n as usize];
						rn = rn.wrapping_sub(4);
						self.mem.insert(rn, self.pr);
						self.r[*n as usize] = rn;
					}

					Ins::Sub(m, n) => {
						let mut rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						rn = rn.wrapping_sub(rm);
						self.r[*n as usize] = rn;
					}

					Ins::SubC(m, n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						let (rn, c0) = rn.overflowing_sub(rm);
						let (rn, c1) = rn.overflowing_sub(self.t as u32);
						self.r[*n as usize] = rn;
						self.t = c0 || c1;
					}

					Ins::SubV(m, n) => {
						let mut rn = self.r[*n as usize];
						let rm = self.r[*m as usize];

						let dst = (rn as i32) < 0;
						let src = (rm as i32) < 0;
						rn = rm.wrapping_sub(rm);
						let ans = (rn as i32) < 0;
						self.t = !(src ^ dst) && (ans ^ dst);
						self.r[*n as usize] = rn;
					}

					Ins::SwapB(m, n) => {
						let rm = self.r[*m as usize];
						self.r[*n as usize] = (rm & 0xFFFF0000) | (rm & 0xFF00) >> 8 | (rm & 0x00FF) << 8;
					}

					Ins::SwapW(m, n) => {
						let rm = self.r[*m as usize];
						self.r[*n as usize] = (rm >> 16) | (rm << 16);
					}

					Ins::Tas(n) => {
						let rn = self.r[*n as usize];
						let temp = self.mem[&rn];
						self.t = temp == 0;
						self.mem.insert(rn, temp | 0x80);
					}

					Ins::TrapA(i) => {
						let imm = (0xFF & *i) as u32;
						self.r[15] -= 4;
						self.mem.insert(self.r[15], self.get_sr());
						self.r[15] -= 4;
						self.mem.insert(self.r[15], self.pc as u32 - 2);
						self.pc = (self.mem[&(self.vbr + imm << 2)] + 4) as usize;
					}

					Ins::Tst(m,n) => {
						self.t = (self.r[*n as usize] & self.r[*m as usize]) == 0;
					}

					Ins::TstI(i) => {
						let temp = self.r[0] & 0xFF & *i as u32;
						self.t = temp == 0;
					}

					Ins::TstM(i) => {
						let mut temp = self.mem[&(self.gbr + self.r[0])];
						temp &= 0xFF & *i as u32;
						self.t = temp == 0;
					}

					Ins::Xor(m,n) => {
						self.r[*n as usize] ^= self.r[*m as usize];
					}

					Ins::XorI(i) => {
						self.r[0] ^= *i as u32;
					}

					Ins::XorM(i) => {
						let addr = self.gbr + self.r[0];
						self.mem.insert(addr, self.mem[&addr] ^ *i as u32);
					}

					Ins::Xtrct(m,n) => {
						let rn = self.r[*n as usize];
						let rm = self.r[*m as usize];
						self.r[*n as usize] = (rm << 16) | (rn >> 16);
					}
				}
			}

			eprintln!("\n{self}");
			self.pc += 2;
		}
	}
}

impl std::fmt::Display for Sh2Emu {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		writeln!(f, "[ R0  {:08X} R1  {:08X} R2  {:08X} R3  {:08X} ]", self.r[0], self.r[1], self.r[2], self.r[3])?;
		writeln!(f, "[ R4  {:08X} R5  {:08X} R6  {:08X} R7  {:08X} ]", self.r[4], self.r[5], self.r[6], self.r[7])?;
		writeln!(f, "[ R8  {:08X} R9  {:08X} R10 {:08X} R11 {:08X} ]", self.r[8], self.r[9], self.r[10], self.r[11])?;
		writeln!(f, "[ R12 {:08X} R13 {:08X} R14 {:08X} R15 {:08X} ]", self.r[12], self.r[13], self.r[14], self.r[15])?;
		writeln!(f, "[ GBR {:08X} VBR {:08X} SR  {:08X}              ]", self.gbr, self.vbr, self.get_sr())?;
		writeln!(f, "[ MAH {:08X} MAL {:08X} PR  {:08X}              ]", self.mach, self.macl, self.pr)?;
		writeln!(f, "[ PC  {:08X}                                        ]", self.pc)
	}
}

impl Sh2Emu {
	fn get_sr(&self) -> u32 {
		(self.m as u32) << 9 | (self.q as u32) << 8 | (self.s as u32) << 1 | (self.t as u32)
	}

	fn set_sr(&mut self, sr: u32) {
		self.m = (sr & 0x200) > 0;
		self.q = (sr & 0x100) > 0;
		self.s = (sr & 0x002) > 0;
		self.t = (sr & 0x001) > 0;
	}
}

#[test]
fn div32_by_16u() {
	let mut emu = Sh2Emu::default();
	emu.r[0] = 3;
	emu.r[1] = 0x3322;
	let mut data = setup_div16(
		0, 1, 2, false,
	);
	data.push(Asm::Ins(Ins::Rts));
	emu.interpret(&data);

	assert_eq!(emu.r[1], 0x3322 / 3);
}

#[test]
fn div16_by_16s() {
	let mut emu = Sh2Emu::default();
	emu.r[0] = 3;
	emu.r[1] = (-51i32) as u32;
	let mut data = setup_div16(
		0, 1, 2, true,
	);
	data.push(Asm::Ins(Ins::Rts));
	emu.interpret(&data);

	assert_eq!(emu.r[1], (-51 / 3) as u32);
}

#[test]
fn div32_by_32u() {
	let mut emu = Sh2Emu::default();
	emu.r[0] = 0x300;
	emu.r[1] = 0x3322;
	let mut data = setup_div32(
		0, 1, 2, 3, false,
	);
	data.push(Asm::Ins(Ins::Rts));
	emu.interpret(&data);

	assert_eq!(emu.r[1], (0x3322u64 / 0x300) as u32);
}

#[test]
fn div32_by_32s() {
	let mut emu = Sh2Emu::default();
	emu.r[0] = 3;
	emu.r[1] = (-49i32) as u32;
	let mut data = setup_div32(
		0, 1, 2, 3, true,
	);
	data.push(Asm::Ins(Ins::Rts));
	emu.interpret(&data);

	assert_eq!(emu.r[1], -16i32 as u32);
}

#[test]
fn div8_by_8u() {
	let lhs = 4u8;
	let rhs = 5u8;
	let dst = 0u8;

	let mut emu = Sh2Emu::default();
	emu.r[lhs as usize] = 123;
	emu.r[rhs as usize] = 7;
	let mut data = vec![];

	// Not needed if these values are known to be zero extended.
	data.push(Asm::Ins(Ins::ExtUB(lhs, lhs)));
	data.push(Asm::Ins(Ins::ExtUB(rhs, rhs)));

	data.push(Asm::Ins(Ins::ShLL8(rhs)));

	data.push(Asm::Ins(Ins::Div0U));

	for _ in 0..8 {
		data.push(Asm::Ins(Ins::Div1(rhs, lhs)));
	}

	data.push(Asm::Ins(Ins::RotCL(lhs)));
	data.push(Asm::Ins(Ins::ExtUB(lhs, dst)));

	data.push(Asm::Ins(Ins::Rts));

	emu.interpret(&data);

	assert_eq!(emu.r[dst as usize], 123 / 7);
}
