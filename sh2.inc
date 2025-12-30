
macro ALIGN? num
	local offset
	offset = num - ($ mod num)
	if offset <> num
		db offset dup 0
	end if
end macro

element R
repeat 16, i:0
	R#i? := R + i
end repeat

macro ADD? args&
	local m, n
	match rm =, rn, args
		n = rn - R
		match #imm, rm
			if imm eqtype byte
				db $70 + n, imm
			else
				err "invalid Imm"
			end if
		else
			m = rm - R
			db $30 + n, $0C + m shl 4
		end match
	else
		err "invalid args: ", `args
	end match
end macro

macro ADDC? args&
	match rm =, rn, args
		db $30 + (rn-R), $0E + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro ADDV? args&
	match rm =, rn, args
		db $30 + (rn-R), $0F + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro AND? args&
	match #imm =, =R0?, args
		if imm eqtype byte
			db $C9, imm
		end if
	else match #imm =, (=R0?=,=GBR?), args
		if imm eqtype byte
			db $CD, imm
		end if
	else match rm =, rn, args
		db $20 + (rn-R), $09 + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro BF? arg
	local target
	target = (arg-($+4)) shr 1
	if target eqtype byte
		db $8B, target
	end if
end macro

macro BFS? arg
	local target
	target = (arg-($+4)) shr 1
	if target eqtype byte
		db $8F, target
	end if
end macro

macro BRA? arg
	local target
	target = (arg-($+4)) shr 1
	if target < $800 & target >= -$800
		db $A0 + target shr 8, target and $FF
	else
		err "target out of range"
	end if
end macro

macro BRAF? arg
	match (rm), arg
		db $00 + (rm-R), $23
	else
		err "invalid arg:", arg
	end match
end macro

macro BSR? arg
	local target
	target = (arg-($+4)) shr 1
	if target < $800 & target >= -$800
		db $B0 + target shr 8, target and $FF
	else
		err "target out of range"
	end if
end macro

macro BSRF? arg
	match (rm), arg
		db $00 + (rm-R), $03
	else
		err "invalid arg:", arg
	end match
end macro

macro BT? arg
	local target
	target = (arg-($+4)) shr 1
	if target eqtype byte
		db $89, target
	end if
end macro

macro BTS? arg
	local target
	target = (arg-($+4)) shr 1
	if target eqtype byte
		db $8D, target
	end if
end macro

macro CLRMAC?
	db $00, $28
end macro

macro CLRT?
	db $00,$08
end macro

macro CMPEQ? args&
	match #imm =, =R0?, args
		db $88, imm
	else match rm =, rn, args
		db $30 + (rn-R), $00 + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro CMPGE? args&
	match rm =, rn, args
		db $30 + (rn-R), $03 + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro CMPGT? args&
	match rm =, rn, args
		db $30 + (rn-R), $07 + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro CMPHI? args&
	match rm =, rn, args
		db $30 + (rn-R), $06 + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro CMPHS? args&
	match rm =, rn, args
		db $30 + (rn-R), $02 + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro CMPPL? rn
	db $40 + (rn-R), $15
end macro

macro CMPPZ? rn
	db $40 + (rn-R), $11
end macro

macro CMPSTR? args&
	match rm =, rn, args
		db $20 + (rn-R), $0C + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro DIV0S? args&
	match rm =, rn, args
		db $20 + (rn-R), $07 + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro DIV0U?
	db $00, $19
end macro

macro DIV1? args&
	match rm =, rn, args
		db $30 + (rn-R), $04 + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro DMULS? args&
	match rm =, rn, args
		db $30 + (rn-R), $0D + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro DMULU? args&
	match rm =, rn, args
		db $30 + (rn-R), $05 + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro DT? rn
	db $40 + (rn-R), $10
end macro

macro EXTSB? args&
	match rm =, rn, args
		db $60 + (rn-R), $0E + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro EXTSW? args&
	match rm =, rn, args
		db $60 + (rn-R), $0F + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro EXTUB? args&
	match rm =, rn, args
		db $60 + (rn-R), $0C + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro EXTUW? args&
	match rm =, rn, args
		db $60 + (rn-R), $0D + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro JMP? arg
	match (rm), arg
		db $40 + (rm-R), $2B
	else match rm, arg
		err "expected: (", `arg, "); found: ", `arg
	end match
end macro

macro JSR? arg
	match (rm), arg
		db $40 + (rm-R), $0B
	else match rm, arg
		err "expected: (", `arg, "); found: ", `arg
	end match
end macro

macro LDC? args&
	match (rm)+ =, =SR?, args
		db $40 + (rm-R), $07
	else match (rm=)+ =, =GBR?, args
		db $40 + (rm-R), $17
	else match (rm)+ =, =VBR?, args
		db $40 + (rm-R), $27
	else match (rm) =, rn, args
		err "LDC.L requires post-increment", 10, "found: ", `args, "; expected: (", `rm, ")+,", `rn
	else match rm =, =SR?, args
		db $40 + (rm-R), $0E
	else match rm =, =GBR?, args
		db $40 + (rm-R), $1E
	else match rm =, =VBR?, args
		db $40 + (rm-R), $2E
	else
		err "invalid args: ", `args
	end match
end macro

macro LDS? args&
	match (rm)+ =, =MACH?, args
		db $40 + (rm-R), $06
	else match (rm=)+ =, =MACL?, args
		db $40 + (rm-R), $16
	else match (rm)+ =, =PR?, args
		db $40 + (rm-R), $26
	else match (rm) =, rn, args
		err "LDS.L requires post-increment", 10, "found: ", `args, "; expected: (", `rm, ")+,", `rn
	else match rm =, =MACH?, args
		db $40 + (rm-R), $0A
	else match rm =, =MACL?, args
		db $40 + (rm-R), $1A
	else match rm =, =PR?, args
		db $40 + (rm-R), $2A
	else
		err "invalid args: ", `args
	end match
end macro

macro MACL? args&
	match (rm)+ =, (rn)+, args
		db $00 + (rn-R), $0F + (rm-R) shl 4
	else match (rm)+ =, (rn), args
		err "requires post-increment on source"
	else match (rm) =, (rn)+, args
		err "requires post-increment on destination"
	end match
end macro

macro MACW? args&
	match (rm)+ =, (rn)+, args
		db $40 + (rn - R), $0F + (rm - R) shl 4
	else match (rm)+ =, (rn), args
		err "requires post-increment on source"
	else match (rm) =, (rn)+, args
		err "requires post-increment on destination"
	end match
end macro

macro MOV? rm, rn
	db $60 + (rn-R), $03 + (rm-R) shl 4
end macro

macro MOVB? args&
	match rm =, -(rn), args
		db $20 + (rn-R), $04 + (rm-R) shl 4
	else match (rm)+ =, rn, args
		db $60 + (rn-R), $04 + (rm-R) shl 4
	else match rm =, (=R0? =, rn), args
		db $00 + (rn-R), $04 + (rm-R) shl 4
	else match (=R0? =, rm) =, rn, args
		db $00 + (rn-R), $0C + (rm-R) shl 4
	else match #imm =, rn, args
		if imm eqtype byte
			db $E0 + (rn-R), imm
		else
			err "displacement must be 0-255, found: ", `d
		end if
	else match (d =, =GBR?) =, =R0?, args
		if d eqtype byte
			db $C4, d
		else
			err "displacement must be 0-255, found: ", `d
		end if
	else match =R0? =, (d =, =GBR?), args
		if d eqtype byte
			db $C0, d
		else
			err "displacement must be 0-255, found: ", `d
		end if
	else match =R0? =, (d =, rn), args
		if d < 16 & d >= 0
			db $80, (rn-R) shl 4 + d
		else
			err "displacement must be 0-16, found: ", `d
		end if
	else match (d =, rm) =, =R0?, args
		if d < 16 & d >= 0
			db $54, (rm-R) shl 4 + d
		else
			err "displacement must be 0-16, found: ", `d
		end if
	else match rm =, (rn), args
		db $20 + (rn-R), $00 + (rm-R) shl 4
	else match (rm) =, rn, args
		db $60 + (rn-R), $00 + (rm-R) shl 4
	end match
end macro

macro MOVW? args&
	match rm =, -(rn), args
		db $20 + (rn-R), $05 + (rm-R) shl 4
	else match (rm)+ =, rn, args
		db $60 + (rn-R), $05 + (rm-R) shl 4
	else match rm =, (=R0? =, rn), args
		db $00 + (rn-R), $05 + (rm-R) shl 4
	else match (=R0? =, rm) =, rn, args
		db $00 + (rn-R), $0D + (rm-R) shl 4
	else match (d =, =PC?) =, rn, args
		if d eqtype byte
			db $90 + (rn-R), d
		else
			err "displacement must be 0-255, found: ", `d
		end if
	else match (d =, =GBR?) =, =R0?, args
		if d eqtype byte
			db $C5, d
		else
			err "displacement must be 0-255, found: ", `d
		end if
	else match =R0? =, (d =, =GBR?), args
		if d eqtype byte
			db $C1, d
		else
			err "displacement must be 0-255, found: ", `d
		end if
	else match =R0? =, (d =, rn), args
		if d < 16 & d >= 0
			db $81, (rn-R) shl 4 + d
		else
			err "displacement must be 0-16, found: ", `d
		end if
	else match (d =, rm) =, =R0?, args
		if d < 16 & d >= 0
			db $85, (rm-R) shl 4 + d
		else
			err "displacement must be 0-16, found: ", `d
		end if
	else match rm =, (rn), args
		db $20 + (rn-R), $01 + (rm-R) shl 4
	else match (rm) =, rn, args
		db $60 + (rn-R), $01 + (rm-R) shl 4
	end match
end macro

macro MOVL? args&
	match rm =, -(rn), args
		db $20 + (rn-R), $06 + (rm-R) shl 4
	else match (rm)+ =, rn, args
		db $60 + (rn-R), $06 + (rm-R) shl 4
	else match rm =, (=R0? =, rn), args
		db $00 + (rn-R), $06 + (rm-R) shl 4
	else match (=R0? =, rm) =, rn, args
		db $00 + (rn-R), $0E + (rm-R) shl 4
	else match (d =, =PC?) =, rn, args
		if d eqtype byte
			db $D0 + (rn-R), d
		else
			err "displacement must be 0-255, found: ", `d
		end if
	else match (d =, =GBR?) =, =R0?, args
		if d eqtype byte
			db $C6, d
		else
			err "displacement must be 0-255, found: ", `d
		end if
	else match =R0? =, (d =, =GBR?), args
		if d eqtype byte
			db $C2, d
		else
			err "displacement must be 0-255, found: ", `d
		end if
	else match rm =, (d =, rn), args
		if d < 16 & d >= 0
			db $10 + (rn-R), (rm-R) shl 4 + d
		else
			err "displacement must be 0-16, found: ", `d
		end if
	else match (d =, rm) =, rn, args
		if d < 16 & d >= 0
			db $50 + (rn-R), (rm-R) shl 4 + d
		else
			err "displacement must be 0-16, found: ", `d
		end if
	else match rm =, (rn), args
		db $20 + (rn-R), $02 + (rm-R) shl 4
	else match (rm) =, rn, args
		db $60 + (rn-R), $02 + (rm-R) shl 4
	end match
end macro

macro MOVA? args&
	match (disp =, =PC?) =, =R0?, args
		if disp eqtype byte
			db $C7, disp
		end if
	else match (disp =, =PC?) =, rn, args
		err "requires R0 destination; found: ", `rn
	else match label =, =R0?, args
		local disp
		disp = (label-($+4)) shr 2
		if disp eqtype byte
			db $C7, disp
		end if
	else match label =, rn, args
		err "requires R0 destination; found: ", `rn
	else
		err "invalid args: ", `args
	end match
end macro

macro MOVT? rn
	db $00 + (rn - R), $29
end macro

macro MULL? rm, rn
	db $00 + (rn-R), $07 + (rm-R) shl 4
end macro

macro MULSW? rm, rn
	db $20 + (rn-R), $0F + (rm-R) shl 4
end macro

macro MULUW? rm, rn
	db $20 + (rn-R), $0E + (rm-R) shl 4
end macro

macro NEG? rm, rn
	db $60 + (rn-R), $0B + (rm-R) shl 4
end macro

macro NEGC? rm, rn
	db $60 + (rn-R), $0A + (rm-R) shl 4
end macro

macro NOP?
	db $00, $09
end macro

macro NOT? rm, rn
	db $60 + (rn-R), $07 + (rm-R) shl 4
end macro

macro OR? args&
	match #imm =, =R0?, args
		if imm eqtype byte
			db $CB, imm
		end if
	else match #imm =, (=R0?=,=GBR?), args
		if imm eqtype byte
			db $CF, imm
		end if
	else match rm =, rn, args
		db $20 + (rn-R), $0B + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro ROTCL? rn
	db $40 + (rn-R), $24
end macro

macro ROTCR? rn
	db $40 + (rn-R), $25
end macro

macro ROTL? rn
	db $40 + (rn-R), $04
end macro

macro ROTR? rn
	db $40 + (rn-R), $05
end macro

macro RTE?
	db $00, $2B
end macro

macro RTS?
	db $00, $0B
end macro

macro SETT?
	db $00, $18
end macro

macro SHAL? rn
	db $40 + (rn-R), $20
end macro

macro SHAR? rn
	db $40 + (rn-R), $21
end macro

macro SHLL? rn
	db $40 + (rn-R), $00
end macro

macro SHLL2? rn
	db $40 + (rn-R), $08
end macro

macro SHLL8? rn
	db $40 + (rn-R), $18
end macro

macro SHLL16? rn
	db $40 + (rn-R), $28
end macro

macro SHLR? rn
	db $40 + (rn-R), $01
end macro

macro SHLR2? rn
	db $40 + (rn-R), $09
end macro

macro SHLR8? rn
	db $40 + (rn-R), $19
end macro

macro SHLR16? rn
	db $40 + (rn-R), $29
end macro

macro SLEEP?
	db $00, $1B
end macro

macro STC? args&
	match =SR? =, -(rn), args
		db $40 + (rn-R), $03
	else match =GBR? =, -(rn), args
		db $40 + (rn-R), $13
	else match =VBR? =, -(rn), args
		db $40 + (rn-R), $23
	else match =SR? =, rn, args
		db $00 + (rn-R), $02
	else match =GBR? =, rn, args
		db $00 + (rn-R), $12
	else match =VBR? =, rn, args
		db $00 + (rn-R), $22
	else
		err "invalid args: ", `args
	end match
end macro

macro STS? args&
	match =MACH? =, -(rn), args
		db $40 + (rn-R), $02
	else match =MACL? =, -(rn), args
		db $40 + (rn-R), $12
	else match =PR? =, -(rn), args
		db $40 + (rn-R), $22
	else match =MACH? =, rn, args
		db $00 + (rn-R), $0A
	else match =MACL? =, rn, args
		db $00 + (rn-R), $1A
	else match =PR? =, rn, args
		db $00 + (rn-R), $2A
	else
		err "invalid args: ", `args
	end match
end macro

macro SUB? rm, rn
	db $30 + (rn-R), $08 + (rm-R) shl 4
end macro

macro SUBC? rm, rn
	db $30 + (rn-R), $0A + (rm-R) shl 4
end macro

macro SUBV? rm, rn
	db $30 + (rn-R), $0B + (rm-R) shl 4
end macro

macro SWAPB? rm, rn
	db $60 + (rn-R), $08 + (rm-R) shl 4
end macro

macro SWAPW? rm, rn
	db $60 + (rn-R), $09 + (rm-R) shl 4
end macro

macro TAS? arg
	match (rn), arg
		db $40 + (rn-R), $1B
	else
		err "expect: (", `arg, "), found: ", `arg
	end match
end macro

macro TRAPA? arg
	match #imm, arg
		if imm eqtype byte
			db $C3, imm
		end if
	end match
end macro

macro TST? args&
	match #imm =, =R0?, args
		if imm eqtype byte
			db $C8, imm
		end if
	else match #imm =, (=R0?=,=GBR?), args
		if imm eqtype byte
			db $CC, imm
		end if
	else match rm =, rn, args
		db $20 + (rn-R), $08 + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro XOR? args&
	match #imm =, =R0?, args
		if imm eqtype byte
			db $CA, imm
		end if
	else match #imm =, (=R0?=,=GBR?), args
		if imm eqtype byte
			db $CE, imm
		end if
	else match rm =, rn, args
		db $20 + (rn-R), $0A + (rm-R) shl 4
	else
		err "invalid args: ", `args
	end match
end macro

macro XTRCT? rm, rn
	db $20 + (rn-R), $0D + (rm-R) shl 4
end macro

