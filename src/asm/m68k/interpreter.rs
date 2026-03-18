
use std::collections::HashMap;

use super::{Addr, Asm, Block, Cond, Sz, EA};

pub fn interpret(data: &[Asm], blocks: &[Block]) -> M68kEmu {
	let mut emu = M68kEmu::default();
	emu.blocks = blocks.into();

	loop {
		let asm = &data[emu.pc];
		emu.pc += 1;
		match asm {
			Asm::Comment(_) => {}
			Asm::Label(_) => {}
			Asm::Add1(sz,ea,d) => {
				let src = emu.get_src(sz, ea, 0xFFF);
				let dst = &mut emu.d[*d as usize];
				let (res, c) = add(*sz, src, *dst);
				*dst = calc_result(*sz, res, *dst);
				emu.set_flags(c, is_neg(*sz, res), res == 0, c, c);
			}
			Asm::Add2(sz,d,ea) => {
				let src = emu.d[*d as usize];
				let dst = emu.get_dst(sz, ea, 0x3F8);
				let (res, c) = add(*sz, src, *dst);
				*dst = calc_result(*sz, res, *dst);
				emu.set_flags(c, is_neg(*sz, res), res == 0, c, c);
			}
			Asm::And1(sz,ea,d) => {
				let src = emu.get_src(sz, ea, 0xBFF);
				let dst = &mut emu.d[*d as usize];
				let res = and(*sz, src, *dst);
				*dst = calc_result(*sz, res, *dst);
				emu.set_flags(emu.x, is_neg(*sz, res), res == 0, false, false);
			}
			Asm::And2(sz,d,ea) => {
				let src = emu.d[*d as usize];
				let dst = emu.get_dst(sz, ea, 0x3F8);
				let res = and(*sz, src, *dst);
				*dst = calc_result(*sz, res, *dst);
				emu.set_flags(emu.x, is_neg(*sz, res), res == 0, false, false);
			}
			Asm::Asl(sz,s,d) => {
				let src = emu.d[*s as usize] & 0x3F;
				let dst = &mut emu.d[*d as usize];
				let dneg = is_neg(*sz, *dst);
				let x = if src != 0 { dneg } else { emu.x };
				let (res, v) = match sz {
					Sz::B => {
						let dst = *dst as u8;
						let res = (dst << src) as u32;
						let v = if dneg { dst.leading_ones() } else { dst.leading_zeros() } < src + 1;
						(res, v)
					}
					Sz::W => {
						let dst = *dst as u16;
						let res = (dst << src) as u32;
						let v = if dneg { dst.leading_ones() } else { dst.leading_zeros() } < src + 1;
						(res, v)
					}
					Sz::L => {
						let res = *dst << src;
						let v = if dneg { dst.leading_ones() } else { dst.leading_zeros() } < src + 1;
						(res, v)
					}
				};
				*dst = calc_result(*sz, res, *dst);
				emu.set_flags(x, is_neg(*sz, res), res == 0, v, src != 0 && dneg);
			}
			Asm::Asr(sz,s,d) => {
				let src = emu.d[*s as usize] & 0x3F;
				let dst = &mut emu.d[*d as usize];
				let dneg  = is_neg(*sz, *dst);
				let x = if src != 0 { dneg } else { emu.x };
				let res = match sz {
					Sz::B => ((*dst as u8) >> src) as u32,
					Sz::W => ((*dst as u16) >> src) as u32,
					Sz::L => *dst >> src,
				};
				*dst = calc_result(*sz, res, *dst);
				emu.set_flags(x, is_neg(*sz, res), res == 0, false, src != 0 && dneg);
			}
			Asm::Bcc(cc,label) => {
				if emu.get_cond(*cc) {
					emu.pc = emu.blocks[*label as usize].span.start;
				}
			}
			Asm::Bra(label) => {
				emu.pc = emu.blocks[*label as usize].span.start;
			}
			Asm::Clr(sz,ea) => {
				let dst = emu.get_dst(sz, ea, 0xBF8);
				*dst = calc_result(*sz, 0, *dst);
				emu.set_flags(emu.x, false, true, false, false);
			}
			Asm::Cmp(sz,ea,d) => {
				let src = emu.get_src(sz, ea, 0xFFF);
				let dst = emu.d[*d as usize];
				let (res, c) = sub(*sz, src, dst);
				emu.set_flags(emu.x, is_neg(*sz, res), res == 0, c, c);
			}
			Asm::CmpI(sz,imm,ea) => {
				let dst = *emu.get_dst(sz, ea, 0xBFB);
				let (res, c) = sub(*sz, *imm as u32, dst);
				emu.set_flags(emu.x, is_neg(*sz, res), res == 0, c, c);
			}
			Asm::DivS(ea,d) => {
				let src = emu.get_src(&Sz::L, ea, 0xBFF);
				let dst = &mut emu.d[*d as usize];
				let (res_div, c) =  (*dst as i32).overflowing_div(src as i16 as i32);
				let res_mod = (*dst as i32) % (src as i16 as i32);
				*dst = ((res_mod << 16) | res_div) as u32;
				emu.set_flags(emu.x, is_neg(Sz::W, res_div as u32), res_div == 0, c, false);
			}
			Asm::DivU(ea,d) => {
				let src = emu.get_src(&Sz::L, ea, 0xBFF);
				let dst = &mut emu.d[*d as usize];
				let (res_div, c) = dst.overflowing_div(src as u16 as u32);
				let res_mod = *dst % (src as u16 as u32);
				*dst = (res_mod << 16) | res_div;
				emu.set_flags(emu.x, is_neg(Sz::W, res_div), res_div == 0, c, false);
			}
			Asm::Eor(sz,d,ea) => {
				let src = emu.d[*d as usize];
				let dst = emu.get_dst(sz, ea, 0xBF8);
				let res = match sz {
					Sz::B => ((*dst as u8) ^ (src as u8)) as u32,
					Sz::W => ((*dst as u16) ^ (src as u16)) as u32,
					Sz::L => *dst ^ src,
				};
				*dst = calc_result(*sz, res, *dst);
				emu.set_flags(emu.x, is_neg(*sz, res), res == 0, false, false);
			}
			Asm::Ext(sz,d) => {
				let dst = &mut emu.d[*d as usize];
				let mut res = *dst;
				match sz {
					Sz::B => unreachable!("cannot extend to Byte size"),
					Sz::W => {
						res &= 0xFFFF0000;
						res |= *dst as i8 as i16 as u32;
					}
					Sz::L => {
						res = *dst as i16 as i32 as u32;
					}
				}
				*dst = res;
				emu.set_flags(emu.x, is_neg(*sz, res), res == 0, false, false);
			}
			Asm::Lsl(sz,s,d) => {
				let src = emu.d[*s as usize] & 0x3F;
				let dst = &mut emu.d[*d as usize];
				let (res, bit) = match sz {
					Sz::B => {
						let res = (*dst as u8) << src;
						let bit = (*dst as u8).rotate_left(src) & 1 > 0;
						(res as u32, bit)
					}
					Sz::W => {
						let res = (*dst as u16) << src;
						let bit = (*dst as u16).rotate_left(src) & 1 > 0;
						(res as u32, bit)
					}
					Sz::L => {
						let res = *dst << src;
						let bit = dst.rotate_left(src) & 1 > 0;
						(res, bit)
					}
				};
				*dst = calc_result(*sz, res, *dst);
				let x = if src != 0 { bit } else { emu.x };
				emu.set_flags(x, is_neg(Sz::L, res), res == 0, false, src != 0 && bit);
			}
			Asm::Lsr(sz,s,d) => {
				let src = emu.d[*s as usize] & 0x3F;
				let dst = &mut emu.d[*d as usize];
				let (res, bit) = match sz {
					Sz::B => {
						let res = (*dst as u8) >> src;
						let bit = (*dst as u8).rotate_right(src) & 0x80 > 0;
						(res as u32, bit)
					}
					Sz::W => {
						let res = (*dst as u16) >> src;
						let bit = (*dst as u16).rotate_right(src) & 0x8000 > 0;
						(res as u32, bit)
					}
					Sz::L => {
						let res = *dst >> src;
						let bit = dst.rotate_right(src) & 0x80000000 > 0;
						(res, bit)
					}
				};
				*dst = calc_result(*sz, res, *dst);
				let x = if src != 0 { bit } else { emu.x };
				emu.set_flags(x, is_neg(Sz::L, res), res == 0, false, src != 0 && bit);
			}
			Asm::Move(sz,eas,ead) => {
				let src = emu.get_src(sz, eas, 0xFFF);
				let dst = emu.get_dst(sz, ead, 0xFF8);
				*dst = calc_result(*sz, src, *dst);
				emu.set_flags(emu.x, is_neg(*sz, src), src == 0, false, false);
			}
			Asm::MulS(ea,d) => {
				let src = emu.get_src(&Sz::W, ea, 0xBFF);
				let dst = &mut emu.d[*d as usize];
				let res = (*dst as i16 as i32) * src as i16 as i32;
				*dst = res as u32;
				emu.set_flags(emu.x, is_neg(Sz::L, res as u32), res == 0, false, false);
			}
			Asm::MulU(ea,d) => {
				let src = emu.get_src(&Sz::W, ea, 0xBFF);
				let dst = &mut emu.d[*d as usize];
				let res = (*dst as u16 as u32) * src as u16 as u32;
				*dst = res;
				emu.set_flags(emu.x, is_neg(Sz::L, res), res == 0, false, false);
			}
			Asm::Neg(sz,ea) => {
				let dst = emu.get_dst(sz, ea, 0xBF8);
				let (res, c) = 0u32.overflowing_sub(*dst);
				*dst = calc_result(*sz, res, *dst);
				emu.set_flags(res != 0, is_neg(*sz, res), res == 0, c, res != 0);
			}
			Asm::Not(sz,ea) => {
				let dst = emu.get_dst(sz, ea, 0xBF8);
				let res = match sz {
					Sz::B => !(*dst as u8) as u32,
					Sz::W => !(*dst as u16) as u32,
					Sz::L => !*dst,
				};
				*dst = calc_result(*sz, res, *dst);
				emu.set_flags(emu.x, is_neg(*sz, res), res == 0, false, false);
			}
			Asm::Nop => {}
			Asm::Or1(sz,ea,d) => {
				let src = emu.get_src(sz, ea, 0xBFF);
				let dst = &mut emu.d[*d as usize];
				let res = or(*sz, src, *dst);
				*dst = calc_result(*sz, res, *dst);
				emu.set_flags(emu.x, is_neg(*sz, res), res == 0, false, false);
			}
			Asm::Or2(sz,d,ea) => {
				let src = emu.d[*d as usize];
				let dst = emu.get_dst(sz, ea, 0x3F8);
				let res = or(*sz, src, *dst);
				*dst = calc_result(*sz, res, *dst);
				emu.set_flags(emu.x, is_neg(*sz, res), res == 0, false, false);
			}
			Asm::Rts => {
				break
			}
			Asm::Scc(cc,ea) => {
				let c = emu.get_cond(*cc);
				let dst = emu.get_dst(&Sz::B, ea, 0xBF8);
				*dst = calc_result(Sz::B, if c { 0xFF } else { 0x00 }, *dst);
			}
			Asm::Sub1(sz,ea,d) => {
				let src = emu.get_src(sz, ea, 0xFFF);
				let dst = &mut emu.d[*d as usize];
				let (res, c) = sub(*sz, src, *dst);
				*dst = calc_result(*sz, res, *dst);
				emu.set_flags(c, is_neg(*sz, res), res == 0, c, c);
			}
			Asm::Sub2(sz,d,ea) => {
				let src = emu.d[*d as usize];
				let dst = emu.get_dst(sz, ea, 0x3F8);
				let (res,c) = sub(*sz, src, *dst);
				*dst = calc_result(*sz, res, *dst);
				emu.set_flags(c, is_neg(*sz, res), res == 0, c, c);
			}
			Asm::Swap(d) => {
				let dst = &mut emu.d[*d as usize];
				let res = (*dst << 16) | (*dst >> 16);
				*dst = res;
				emu.set_flags(emu.x, is_neg(Sz::L, res), res == 0, false, false);
			}
			Asm::Trap(v) => todo!("Trap({v})"),
			Asm::Tst(sz,ea) => {
				let dst = emu.get_src(sz, ea, 0xFFF);
				emu.set_flags(emu.x, is_neg(*sz, dst), dst == 0, false, false);
			}
		}
	}

	emu
}

fn is_neg(sz: Sz, res: u32) -> bool {
	(res & match sz {
		Sz::B => 0x00000080,
		Sz::W => 0x00008000,
		Sz::L => 0x80000000,
	}) > 0
}

fn calc_result(sz: Sz, res: u32, dst: u32) -> u32 {
	res | (dst & match sz {
		Sz::B => 0xFFFFFF00,
		Sz::W => 0xFFFF0000,
		Sz::L => 0x00000000,
	})
}

fn add(sz: Sz, src: u32, dst: u32) -> (u32,bool) {
	match sz {
		Sz::B => {
			let (res,c) = (dst as u8).overflowing_add(src as u8);
			(res as u32, c)
		}
		Sz::W => {
			let (res,c) = (dst as u16).overflowing_add(src as u16);
			(res as u32, c)
		}
		Sz::L => dst.overflowing_add(src),
	}
}

fn sub(sz: Sz, src: u32, dst: u32) -> (u32,bool) {
	match sz {
		Sz::B => {
			let (res, c) = (dst as u8).overflowing_sub(src as u8);
			(res as u32, c)
		}
		Sz::W => {
			let (res, c) = (dst as u16).overflowing_sub(src as u16);
			(res as u32, c)
		}
		Sz::L => dst.overflowing_sub(src),
	}
}

fn and(sz: Sz, src: u32, dst: u32) -> u32 {
	match sz {
		Sz::B => (dst as u8 & src as u8) as u32,
		Sz::W => (dst as u16 & src as u16) as u32,
		Sz::L => dst & src,
	}
}

fn or(sz: Sz, src: u32, dst: u32) -> u32 {
	match sz {
		Sz::B => (dst as u8 | src as u8) as u32,
		Sz::W => (dst as u16 | src as u16) as u32,
		Sz::L => dst | src,
	}
}

#[derive(Debug, Default)]
pub struct M68kEmu {
	pc: usize,
	d: [u32;8],
	a: [u32;8],
	x: bool,
	n: bool,
	z: bool,
	v: bool,
	c: bool,
	pub mem: HashMap<u32, u32>,
	blocks: Vec<Block>,
}

impl M68kEmu {
	fn get_cond(&self, cc: Cond) -> bool {
		match cc {
			Cond::CC => !self.c,
			Cond::CS => self.c,
			Cond::EQ => self.z,
			Cond::GE => self.n && self.v || !self.n && !self.v,
			Cond::GT => self.n && self.v && !self.z || !self.v && !self.v && !self.z,
			Cond::HI => !self.c && !self.z,
			Cond::LE => self.z || self.n && !self.v || !self.n && self.v,
			Cond::LS => self.c || self.z,
			Cond::LT => self.n && !self.v || !self.n && self.v,
			Cond::NE => !self.z,
		}
	}

	fn set_flags(&mut self, x: bool, n: bool, z: bool, v: bool, c: bool) {
		self.x = x;
		self.n = n;
		self.z = z;
		self.v = v;
		self.c = c;
	}

	fn get_src(&mut self, sz: &Sz, ea: &EA, mask: u16) -> u32 {
		match ea {
			EA::Dat(d) => {
				if mask & 0x800 > 0 {
					self.d[*d as usize]
				} else {
					panic!("direct data source is invalid")
				}
			}
			EA::Adr(a) => {
				if mask & 0x400 > 0 {
					self.a[*a as usize]
				} else {
					panic!("direct address source is invalid")
				}
			}
			EA::Ind(a) => {
				if mask & 0x200 > 0 {
					let adr = self.a[*a as usize];
					self.mem[&adr]
				} else {
					panic!("indirect address source is invalid")
				}
			}
			EA::Pst(a) => {
				if mask & 0x100 > 0 {
					let offset: u32 = sz.into();
					let adr = &mut self.a[*a as usize];
					let src = self.mem[adr];
					*adr += offset.max(2 * (*a == Addr::A7) as u32);
					src
				} else {
					panic!("post-increment address source is invalid")
				}
			}
			EA::Pre(a) => {
				if mask & 0x080 > 0 {
					let offset: u32 = sz.into();
					let adr = &mut self.a[*a as usize];
					*adr -= offset.max(2 * (*a == Addr::A7) as u32);
					self.mem[adr]
				} else {
					panic!("pre-decrement address source is invalid")
				}
			}
			EA::Dsp(dsp,a) => {
				if mask & 0x040 > 0 {
					let adr = self.a[*a as usize];
					self.mem[&adr.wrapping_add_signed(*dsp as i32)]
				} else {
					panic!("indirect address w/ displacement source is invalid")
				}
			}
			EA::Idx(dsp,a,x) => {
				if mask & 0x020 > 0 {
					let adr = self.a[*a as usize];
					let loc = adr.wrapping_add_signed(*dsp as i32)
							.wrapping_add(self.a[*x as usize]);
					self.mem[&loc]
				} else {
					panic!("indirect address w/ indexed displacement source is invalid")
				}
			}
			EA::AbL(imm) => {
				if mask & 0x010 > 0 {
					self.mem[&(*imm as u32)]
				} else {
					panic!("indirect long absolute source is invalid")
				}
			}
			EA::AbW(imm) => {
				if mask & 0x008 > 0 {
					self.mem[&(*imm as u32)]
				} else {
					panic!("indirect word absolute source is invalid")
				}
			}
			EA::Imm(imm) => {
				if mask & 0x004 > 0 {
					*imm as u32
				} else {
					panic!("immediate source is invalid")
				}
			}
		}
	}

	fn get_dst(&mut self, sz: &Sz, ea: &EA, mask: u16) -> &mut u32 {
		match ea {
			EA::Dat(d) => {
				if mask & 0x800 > 0 {
					&mut self.d[*d as usize]
				} else {
					panic!("direct data destination is invalid")
				}
			}
			EA::Adr(a) => {
				if mask & 0x400 > 0 {
					&mut self.a[*a as usize]
				} else {
					panic!("direct address destination is invalid")
				}
			}
			EA::Ind(a) => {
				if mask & 0x200 > 0 {
					let adr = self.a[*a as usize];
					self.mem.entry(adr).or_default()
				} else {
					panic!("indirect address destination is invalid")
				}
			}
			EA::Pst(a) => {
				if mask & 0x100 > 0 {
					let offset: u32 = sz.into();
					let adr = &mut self.a[*a as usize];
					let dst = self.mem.entry(*adr)
							.or_default();
					*adr += offset.max(2 * (*a == Addr::A7) as u32);
					dst
				} else {
					panic!("post-increment address destination is invalid")
				}
			}
			EA::Pre(a) => {
				if mask & 0x080 > 0 {
					let offset: u32 = sz.into();
					let adr = &mut self.a[*a as usize];
					*adr -= offset.max(2 * (*a == Addr::A7) as u32);
					self.mem.entry(*adr).or_default()
				} else {
					panic!("pre-increment address destination is invalid")
				}
			}
			EA::Dsp(dsp,a) => {
				if mask & 0x040 > 0 {
					let adr = self.a[*a as usize];
					self.mem.entry(adr.wrapping_add_signed(*dsp as i32))
							.or_default()
				} else {
					panic!("indirect address w/ displacement destination is invalid")
				}
			}
			EA::Idx(dsp,a,x) => {
				if mask & 0x020 > 0 {
					let adr = self.a[*a as usize];
					let loc = adr.wrapping_add_signed(*dsp as i32)
							.wrapping_add(self.a[*x as usize]);
					self.mem.entry(loc).or_default()
				} else {
					panic!("indirect address w/ indexed displacement destination is invalid")
				}
			}
			EA::AbL(imm) => {
				if mask & 0x010 > 0 {
					self.mem
							.entry(*imm as u32)
							.or_default()
				} else {
					panic!("indirect long absolute destination is invalid")
				}
			}
			EA::AbW(imm) => {
				if mask & 0x008 > 0 {
					self.mem
							.entry(*imm as u32)
							.or_default()
				} else {
					panic!("indirect word absolute destination is invalid")
				}
			}
			EA::Imm(_) => unreachable!(),
		}
	}
}
