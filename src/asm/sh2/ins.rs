
use std::fmt::{Display, Formatter, Result};

pub type Reg = u8;

#[allow(dead_code)]
#[derive(Debug, Default, PartialEq, Eq)]
pub enum Asm {
	Label(String),
	/* Data Transfer */

	/* Move Immediate Data */

	// mov #S,Rn
	MovI(i8, Reg),
	// mov.w @(disp,PC),Rn
	MovWI(u8, Reg),
	// mov.l @(disp,PC),Rn
	MovLI(u8, Reg),

	/* Move Data */

	// mov Rm,Rn
	Mov(Reg, Reg),
	// mov.b Rm,@Rn
	MovBS(Reg, Reg),
	// mov.w Rm,@Rn
	MovWS(Reg, Reg),
	// mov.l Rm,@Rn
	MovLS(Reg, Reg),
	// mov.b @Rm,Rn
	MovBL(Reg, Reg),
	// mov.w @Rm,Rn
	MovWL(Reg, Reg),
	// mov.l @Rm,Rn
	MovLL(Reg, Reg),
	// mov.b Rm,@-Rn
	MovBM(Reg, Reg),
	// mov.w Rm,@-Rn
	MovWM(Reg, Reg),
	// mov.l Rm,@-Rn
	MovLM(Reg, Reg),
	// mov.b @Rm+,Rn
	MovBP(Reg, Reg),
	// mov.w @Rm+,Rn
	MovWP(Reg, Reg),
	// mov.l @Rm+,Rn
	MovLP(Reg, Reg),
	// mov.b Rm,@(R0,Rn)
	MovBS0(Reg, Reg),
	// mov.w Rm,@(R0,Rn)
	MovWS0(Reg, Reg),
	// mov.l Rm,@(R0,Rn)
	MovLS0(Reg, Reg),
	// mov.b @(R0,Rm),Rn
	MovBL0(Reg, Reg),
	// mov.w @(R0,Rm),Rn
	MovWL0(Reg, Reg),
	// mov.l @(R0,Rm),Rn
	MovLL0(Reg, Reg),

	/* Move Peripheral Data */

	// mov.b @(disp,GBR),R0
	MovBLG(u8),
	// mov.w @(disp,GBR),R0
	MovWLG(u8),
	// mov.l @(disp,GBR),R0
	MovLLG(u8),
	// mov.b R0,@(disp,GBR)
	MovBSG(u8),
	// mov.w R0,@(disp,GBR)
	MovWSG(u8),
	// mov.l R0,@(disp,GBR)
	MovLSG(u8),

	/* Move Structure Data */

	// mov.b R0,@(disp,Rn)
	MovBS4(u8, Reg),
	// mov.w R0,@(disp,Rn)
	MovWS4(u8, Reg),
	// mov.l Rm,@(disp,Rn)
	MovLS4(Reg, u8, Reg),
	// mov.b @(disp,Rm),R0
	MovBL4(u8, Reg),
	// mov.w @(disp,Rm),R0
	MovWL4(u8, Reg),
	// mov.l @(disp,Rm),Rn
	MovLL4(u8, Reg, Reg),

	/* Move Effective Address */

	// mova @(disp,PC),R0
	MovA(u8),

	// movt Rn
	MovT(Reg),
	// swap.b Rm,Rn
	SwapB(Reg, Reg),
	// swap.w Rm,Rn
	SwapW(Reg, Reg),
	// Xtrct Rm,Rn
	Xtrct(Reg, Reg),

	/* Arithmetic Instructions */

	// add Rm,Rn
	Add(Reg, Reg),
	// add #S,Rn
	AddI(i8, Reg),
	// addc Rm,Rn
	AddC(Reg, Reg),
	// addv Rm,Rn
	AddV(Reg, Reg),
	// cmp/eq #S,R0
	CmpIM(i8),
	// cmp/eq Rm,Rn
	CmpEQ(Reg, Reg),
	// cmp/hs Rm,Rn
	CmpHS(Reg, Reg),
	// cmp/ge Rm,Rn
	CmpGE(Reg, Reg),
	// cmp/hi Rm,Rn
	CmpHI(Reg, Reg),
	// cmp/gt Rm,Rn
	CmpGT(Reg, Reg),
	// cmp/pl Rn
	CmpPL(Reg),
	// cmp/pz Rn
	CmpPZ(Reg),
	// cmp/str Rm,Rn
	CmpStr(Reg, Reg),
	// div1 Rm,Rn
	Div1(Reg, Reg),
	// div0s Rm,Rn
	Div0S(Reg, Reg),
	// div0u
	Div0U,
	// dmuls.l Rm,Rn
	DMulS(Reg, Reg),
	// dmulu.l Rm,Rn
	DMulU(Reg, Reg),
	// dt Rn
	DT(Reg),
	// exts.b Rm,Rn
	ExtSB(Reg, Reg),
	// exts.w Rm,Rn
	ExtSW(Reg, Reg),
	// extu.b Rm,Rn
	ExtUB(Reg, Reg),
	// extu.w Rm,Rn
	ExtUW(Reg, Reg),
	// mac.l @Rm+,@Rn+
	MacL(Reg, Reg),
	// mac.w @Rm+,@Rn+
	MacW(Reg, Reg),
	// mul.l Rm,Rn
	MulL(Reg, Reg),
	// muls.w Rm,Rn
	MulS(Reg, Reg),
	// mulu.w Rm,Rn
	MulU(Reg, Reg),
	// neg Rm,Rn
	Neg(Reg, Reg),
	// negc Rm,Rn
	NegC(Reg, Reg),
	// sub Rm,Rn
	Sub(Reg, Reg),
	// subc Rm,Rn
	SubC(Reg, Reg),
	// subv Rm,Rn
	SubV(Reg, Reg),

	/* Logic Operation Instructions */

	// and Rm,Rn
	And(Reg, Reg),
	// and #U,R0
	AndI(u8),
	// and.b #U,@(R0,GBR)
	AndM(u8),
	// not Rm,Rn
	Not(Reg, Reg),
	// or Rm,Rn
	Or(Reg, Reg),
	// or #U,R0
	OrI(u8),
	// or.b #U,@(R0,GBR)
	OrM(u8),
	// tas.b @Rn
	Tas(Reg),
	// tst Rm,Rn
	Tst(Reg, Reg),
	// tst #U,R0
	TstI(u8),
	// tst.b #U,@(R0,GBR)
	TstM(u8),
	// xor Rm,Rn
	Xor(Reg, Reg),
	// xor #U,R0
	XorI(u8),
	// xor.b #U,@(R0,GBR)
	XorM(u8),

	/* Shift Instructions */

	// rotl Rn
	RotL(Reg),
	// rotr Rn
	RotR(Reg),
	// rotcl Rn
	RotCL(Reg),
	// rotcr Rn
	RotCR(Reg),
	// shal Rn
	ShAL(Reg),
	// shar Rn
	ShAR(Reg),
	// shll Rn
	ShLL(Reg),
	// shlr Rn
	ShLR(Reg),
	// shll2 Rn
	ShLL2(Reg),
	// shlr2 Rn
	ShLR2(Reg),
	// shll8 Rn
	ShLL8(Reg),
	// shlr8 Rn
	ShLR8(Reg),
	// shll16 Rn
	ShLL16(Reg),
	// shlr16 Rn
	ShLR16(Reg),

	/* Branch Instructions */

	/// Instruction | Code   | State
	/// ----------- | ------ | -----
	/// BF label    | 0x8Bss | 3/1
	///
	/// - When T = 0, disp * 2 + PC -> PC
	/// - When T = 1, nop
	///
	/// Reads the T bit, and conditionally branches. If T = 1, BF executes the next instruction. If T
	/// = 0, it branches. The branch destination is an address specified by PC + displacement. The PC
	/// points to the starting address of the second instruction after the branch instruction. The
	/// 8-bit displacement is sign-extended and doubled. Consequently, the relative interval from the
	/// branch destination is -256 to +254 bytes. If the displacement is too short to reach the branch
	/// destination, use BF with the BRA instruction or the like.
	BF(String),

	// BF/S label
	BFS(String),

	// bt label
	BT(String),

	// bt/s label
	BTS(String),

	/// Instruction | Code   | State
	/// ----------- | ------ | -----
	/// BRA label   | 0xAsss | 2
	///
	/// - disp * 2 + PC -> PC
	///
	/// Branches unconditionally after executing the instruction following this BRA instruction. The
	/// branch destination is an address specified by PC + displacement. The PC points to the starting
	/// address of the second instruction after this BRA instruction. The 12-bit displacement is
	/// sign-extended and doubled. Consequently, the relative interval from the branch destination is
	/// -4096 to +4094 bytes. If the displacement is too short to reach the branch destination, this
	/// instruction must be changed to the JMP instruction. Here, a MOV instruction must be used to
	/// transfer the destination address to a register.
	Bra(String),

	// braf Rm
	BraF(Reg),
	// bsr label
	Bsr(String),
	// bsrf Rm
	BsrF(Reg),
	// jmp @Rm
	Jmp(Reg),
	// jsr @Rm
	Jsr(Reg),
	// rts
	Rts,

	/* System Control Instructions */

	// clrt
	ClrT,
	// clrmac
	ClrMac,
	// ldc Rm,SR
	LdSr(Reg),
	// ldc Rm,GBR
	LdGbr(Reg),
	// ldc Rm,VBR
	LdVbr(Reg),
	// ldc.l @Rm+,SR
	LdMSr(Reg),
	// ldc.l @Rm+,GBR
	LdMGbr(Reg),
	// ldc.l @Rm+,VBR
	LdMVbr(Reg),
	// lds Rm,MACH
	LdMach(Reg),
	// lds Rm,MACL
	LdMacl(Reg),
	// lds Rm,PR
	LdPr(Reg),
	// lds.l @Rm+,MACH
	LdMMach(Reg),
	// lds.l @Rm+,MACL
	LdMMacl(Reg),
	// lds.l @Rm+,PR
	LdsMPr(Reg),
	// nop
	#[default]
	Nop,
	// rte
	Rte,
	// sett
	SetT,
	// sleep
	Sleep,
	// stc SR,Rn
	StSr(Reg),
	// stc GBR,Rn
	StGbr(Reg),
	// stc VBR,Rn
	StVbr(Reg),
	// stc.l SR,@-Rn
	StMSr(Reg),
	// stc.l GBR,@-Rn
	StMGbr(Reg),
	// stc.l VBR,@-Rn
	StMVbr(Reg),
	// sts MACH,Rn
	StMach(Reg),
	// sts MACL,Rn
	StMacl(Reg),
	// sts PR,Rn
	StPr(Reg),
	// sts.l MACH,@-Rn
	StMMach(Reg),
	// sts.l MACL,@-Rn
	StMMacl(Reg),
	// sts.l PR,@-Rn
	StMPr(Reg),
	// trapa #U
	TrapA(u8),
}

impl Display for Asm {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Self::Label(label)  => write!(f, "{label}:"),
			Self::MovI(s,n)     => write!(f, "\tmovb   #{s},r{n}"),
			Self::MovWI(u,n)    => write!(f, "\tmovw   ({u},pc),r{n}"),
			Self::MovLI(u,n)    => write!(f, "\tmovl   ({u},pc),r{n}"),
			Self::Mov(m,n)      => write!(f, "\tmov    r{m},r{n}"),
			Self::MovBS(m,n)    => write!(f, "\tmovb   r{m},(r{n})"),
			Self::MovWS(m,n)    => write!(f, "\tmovw   r{m},(r{n})"),
			Self::MovLS(m,n)    => write!(f, "\tmovl   r{m},(r{n})"),
			Self::MovBL(m,n)    => write!(f, "\tmovb   (r{m}),r{n}"),
			Self::MovWL(m,n)    => write!(f, "\tmovw   (r{m}),r{n}"),
			Self::MovLL(m,n)    => write!(f, "\tmovl   (r{m}),r{n}"),
			Self::MovBM(m,n)    => write!(f, "\tmovb   r{m},-(r{n})"),
			Self::MovWM(m,n)    => write!(f, "\tmovw   r{m},-(r{n})"),
			Self::MovLM(m,n)    => write!(f, "\tmovl   r{m},-(r{n})"),
			Self::MovBP(m,n)    => write!(f, "\tmovb   (r{m})+,r{n}"),
			Self::MovWP(m,n)    => write!(f, "\tmovw   (r{m})+,r{n}"),
			Self::MovLP(m,n)    => write!(f, "\tmovl   (r{m})+,r{n}"),
			Self::MovBS0(m,n)   => write!(f, "\tmovb   r{m},(r0,r{n})"),
			Self::MovWS0(m,n)   => write!(f, "\tmovw   r{m},(r0,r{n})"),
			Self::MovLS0(m,n)   => write!(f, "\tmovl   r{m},(r0,r{n})"),
			Self::MovBL0(m,n)   => write!(f, "\tmovb   (r0,r{m}),r{n}"),
			Self::MovWL0(m,n)   => write!(f, "\tmovw   (r0,r{m}),r{n}"),
			Self::MovLL0(m,n)   => write!(f, "\tmovl   (r0,r{m}),r{n}"),
			Self::MovBLG(u)     => write!(f, "\tmovb   ({u},gbr),r0"),
			Self::MovWLG(u)     => write!(f, "\tmovw   ({u},gbr),r0"),
			Self::MovLLG(u)     => write!(f, "\tmovl   ({u},gbr),r0"),
			Self::MovBSG(u)     => write!(f, "\tmovb   r0,({u},gbr)"),
			Self::MovWSG(u)     => write!(f, "\tmovw   r0,({u},gbr)"),
			Self::MovLSG(u)     => write!(f, "\tmovl   r0,({u},gbr)"),
			Self::MovBS4(u,n)   => write!(f, "\tmovb   r0,({u},r{n})"),
			Self::MovWS4(u,n)   => write!(f, "\tmovw   r0,({u},r{n})"),
			Self::MovLS4(m,u,n) => write!(f, "\tmovl   r{m},({u},r{n})"),
			Self::MovBL4(u,m)   => write!(f, "\tmovb   ({u},r{m}),r0"),
			Self::MovWL4(u,m)   => write!(f, "\tmovw   ({u},r{m}),r0"),
			Self::MovLL4(u,m,n) => write!(f, "\tmovl   ({u},r{m}),r{n}"),
			Self::MovA(u)       => write!(f, "\tmova   ({u},pc),r0"),
			Self::MovT(n)       => write!(f, "\tmovt   r{n}"),
			Self::SwapB(m,n)    => write!(f, "\tswapb  r{m},r{n}"),
			Self::SwapW(m,n)    => write!(f, "\tswapw  r{m},r{n}"),
			Self::Xtrct(m,n)    => write!(f, "\txtrct  r{m},r{n}"),
			Self::Add(m,n)      => write!(f, "\tadd    r{m},r{n}"),
			Self::AddI(s,n)     => write!(f, "\tadd    #{s},r{n}"),
			Self::AddC(m,n)     => write!(f, "\taddc   r{m},r{n}"),
			Self::AddV(m,n)     => write!(f, "\taddv   r{m},r{n}"),
			Self::CmpIM(s)      => write!(f, "\tcmpeq  #{s},r0"),
			Self::CmpEQ(m,n)    => write!(f, "\tcmpeq  r{m},r{n}"),
			Self::CmpHS(m,n)    => write!(f, "\tcmphs  r{m},r{n}"),
			Self::CmpGE(m,n)    => write!(f, "\tcmpge  r{m},r{n}"),
			Self::CmpHI(m,n)    => write!(f, "\tcmphi  r{m},r{n}"),
			Self::CmpGT(m,n)    => write!(f, "\tcmpgt  r{m},r{n}"),
			Self::CmpPL(n)      => write!(f, "\tcmppl  r{n}"),
			Self::CmpPZ(n)      => write!(f, "\tcmppz  r{n}"),
			Self::CmpStr(m,n)   => write!(f, "\tcmpstr r{m},r{n}"),
			Self::Div1(m,n)     => write!(f, "\tdiv1   r{m},r{n}"),
			Self::Div0S(m,n)    => write!(f, "\tdiv0s  r{m},r{n}"),
			Self::Div0U         => write!(f, "\tdiv0u"),
			Self::DMulS(m,n)    => write!(f, "\tdmuls  r{m},r{n}"),
			Self::DMulU(m,n)    => write!(f, "\tdmulu  r{m},r{n}"),
			Self::DT(n)         => write!(f, "\tdt     r{n}"),
			Self::ExtSB(m,n)    => write!(f, "\textsb  r{m},r{n}"),
			Self::ExtSW(m,n)    => write!(f, "\textsw  r{m},r{n}"),
			Self::ExtUB(m,n)    => write!(f, "\textub  r{m},r{n}"),
			Self::ExtUW(m,n)    => write!(f, "\textuw  r{m},r{n}"),
			Self::MacL(m,n)     => write!(f, "\tmacl   (r{m})+,(r{n})+"),
			Self::MacW(m,n)     => write!(f, "\tmacw   (r{m})+,(r{n})+"),
			Self::MulL(m,n)     => write!(f, "\tmull   r{m},r{n}"),
			Self::MulS(m,n)     => write!(f, "\tmulsw  r{m},r{n}"),
			Self::MulU(m,n)     => write!(f, "\tmuluw  r{m},r{n}"),
			Self::Neg(m,n)      => write!(f, "\tneg    r{m},r{n}"),
			Self::NegC(m,n)     => write!(f, "\tnegc   r{m},r{n}"),
			Self::Sub(m,n)      => write!(f, "\tsub    r{m},r{n}"),
			Self::SubC(m,n)     => write!(f, "\tsubc   r{m},r{n}"),
			Self::SubV(m,n)     => write!(f, "\tsubv   r{m},r{n}"),
			Self::And(m,n)      => write!(f, "\tand    r{m},r{n}"),
			Self::AndI(u)       => write!(f, "\tand    #{u},r0"),
			Self::AndM(u)       => write!(f, "\tand    #{u},(r0,gbr)"),
			Self::Not(m,n)      => write!(f, "\tnot    r{m},r{n}"),
			Self::Or(m,n)       => write!(f, "\tor     r{m},r{n}"),
			Self::OrI(u)        => write!(f, "\tor     #{u},r0"),
			Self::OrM(u)        => write!(f, "\tor     #{u},(r0,gbr)"),
			Self::Tas(n)        => write!(f, "\ttas    (r{n})"),
			Self::Tst(m,n)      => write!(f, "\ttst    r{m},r{n}"),
			Self::TstI(u)       => write!(f, "\ttst    #{u},r0"),
			Self::TstM(u)       => write!(f, "\ttst    #{u},(r0,gbr)"),
			Self::Xor(m,n)      => write!(f, "\txor    r{m},r{n}"),
			Self::XorI(u)       => write!(f, "\txor    #{u},r0"),
			Self::XorM(u)       => write!(f, "\txor    #{u},(r0,gbr)"),
			Self::RotL(n)       => write!(f, "\trotl   r{n}"),
			Self::RotR(n)       => write!(f, "\trotr   r{n}"),
			Self::RotCL(n)      => write!(f, "\trotcl  r{n}"),
			Self::RotCR(n)      => write!(f, "\trotcr  r{n}"),
			Self::ShAL(n)       => write!(f, "\tshal   r{n}"),
			Self::ShAR(n)       => write!(f, "\tshar   r{n}"),
			Self::ShLL(n)       => write!(f, "\tshll   r{n}"),
			Self::ShLR(n)       => write!(f, "\tshlr   r{n}"),
			Self::ShLL2(n)      => write!(f, "\tshll2  r{n}"),
			Self::ShLR2(n)      => write!(f, "\tshlr2  r{n}"),
			Self::ShLL8(n)      => write!(f, "\tshll8  r{n}"),
			Self::ShLR8(n)      => write!(f, "\tshlr8  r{n}"),
			Self::ShLL16(n)     => write!(f, "\tshll16 r{n}"),
			Self::ShLR16(n)     => write!(f, "\tshlr16 r{n}"),
			Self::BF(s)         => write!(f, "\tbf     {s}"),
			Self::BFS(s)        => write!(f, "\tbfs    {s}"),
			Self::BT(s)         => write!(f, "\tbt     {s}"),
			Self::BTS(s)        => write!(f, "\tbts    {s}"),
			Self::Bra(s)        => write!(f, "\tbra    {s}"),
			Self::BraF(m)       => write!(f, "\tbraf   r{m}"),
			Self::Bsr(s)        => write!(f, "\tbsr    {s}"),
			Self::BsrF(m)       => write!(f, "\tbsrf   r{m}"),
			Self::Jmp(m)        => write!(f, "\tjmp    (r{m})"),
			Self::Jsr(m)        => write!(f, "\tjsr    (r{m})"),
			Self::Rts           => write!(f, "\trts"),
			Self::ClrT          => write!(f, "\tclrt"),
			Self::ClrMac        => write!(f, "\tclrmac"),
			Self::LdSr(m)       => write!(f, "\tldc    r{m},sr"),
			Self::LdGbr(m)      => write!(f, "\tldc    r{m},gbr"),
			Self::LdVbr(m)      => write!(f, "\tldc    r{m},vbr"),
			Self::LdMSr(m)      => write!(f, "\tldc    (r{m})+,sr"),
			Self::LdMGbr(m)     => write!(f, "\tldc    (r{m})+,gbr"),
			Self::LdMVbr(m)     => write!(f, "\tldc    (r{m})+,vbr"),
			Self::LdMach(m)     => write!(f, "\tlds    r{m},mach"),
			Self::LdMacl(m)     => write!(f, "\tlds    r{m},macl"),
			Self::LdPr(m)       => write!(f, "\tlds    r{m},pr"),
			Self::LdMMach(m)    => write!(f, "\tlds    (r{m})+,mach"),
			Self::LdMMacl(m)    => write!(f, "\tlds    (r{m})+,macl"),
			Self::LdsMPr(m)     => write!(f, "\tlds    (r{m})+,pr"),
			Self::Nop           => write!(f, "\tnop"),
			Self::Rte           => write!(f, "\trte"),
			Self::SetT          => write!(f, "\tsett"),
			Self::Sleep         => write!(f, "\tsleep"),
			Self::StSr(n)       => write!(f, "\tstc    sr,r{n}"),
			Self::StGbr(n)      => write!(f, "\tstc    gbr,r{n}"),
			Self::StVbr(n)      => write!(f, "\tstc    vbr,r{n}"),
			Self::StMSr(n)      => write!(f, "\tstc    sr,-(r{n})"),
			Self::StMGbr(n)     => write!(f, "\tstc    gbr,-(r{n})"),
			Self::StMVbr(n)     => write!(f, "\tstc    vbr,-(r{n})"),
			Self::StMach(n)     => write!(f, "\tsts    mach,r{n}"),
			Self::StMacl(n)     => write!(f, "\tsts    macl,r{n}"),
			Self::StPr(n)       => write!(f, "\tsts    pr,r{n}"),
			Self::StMMach(n)    => write!(f, "\tsts    mach,-(r{n})"),
			Self::StMMacl(n)    => write!(f, "\tsts    macl,-(r{n})"),
			Self::StMPr(n)      => write!(f, "\tsts    pr,-(r{n})"),
			Self::TrapA(u)      => write!(f, "\ttrapa  #{u}"),
		}
	}
}

