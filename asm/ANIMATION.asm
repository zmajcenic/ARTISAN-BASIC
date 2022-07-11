; sprite animation routines

; number of animation items and pointer
ANIMITEMNUM:
 DB 0
ANIMITEMPTR:
 DW EXT_END
; number of animation definitions and pointer
ANIMDEFNUM:
 DB 0
ANIMDEFPTR:
 DW EXT_END
; number of links between sprite and animation definitions
ANIMSPRNUM:
 DB 0
ANIMSPRPTR:
 DW EXT_END

; *******************************************************************************************************
; function to handle CALL MAXANIMITEMS basic extension
; MAXANIMITEMS BYTE number
; sets new number and moves memory buffers as needed
MAXANIMITEMS:
	; opening (
	CALL CHKCHAR
	DB '('
	; get value
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
	; ending )
	CALL CHKCHAR
	DB ')'
    POP AF

	; save position
	PUSH HL
MAXANIMITEMS.ENTRY:
    LD B,A
    LD A,(ANIMITEMNUM)
    SUB B
    JR Z, .EXIT; same value as before
    LD IX,ANIMITEMPTR
    LD IY,ANIMDEFPTR
    JP M, .INCREASE
    ; new value is lower than previous one
    CALL .SIZEDIFF
    CALL .DECREASE_COMMON
    LD HL,(ANIMSPRPTR)
    XOR A
    SBC HL,BC
    LD (ANIMSPRPTR),HL
    LD HL,(FREEMEMPTR)
    XOR A
    SBC HL,BC
    LD (FREEMEMPTR),HL
.EXIT:
    EI
	POP HL
	RET
.INCREASE:
    NEG
    CALL .SIZEDIFF
    CALL .INCREASE_COMMON
    LD HL,(ANIMSPRPTR)
    ADD HL,BC
    LD (ANIMSPRPTR),HL
    LD HL,(FREEMEMPTR)
    ADD HL,BC
    LD (FREEMEMPTR),HL
    JR .EXIT
.SIZEDIFF:
    LD H,0
    LD L,A
    ADD HL,HL
    ADD HL,HL
    LD A,B
    LD (ANIMITEMNUM),A
    LD B,H
    LD C,L
    RET ; BC=size difference in bytes
.SIZETOMOVE:
    PUSH DE
    LD HL,(FREEMEMPTR)
    LD E,(IY)
    LD D,(IY+1)
    XOR A
    SBC HL,DE
    LD B,H
    LD C,L
    POP DE
    RET 
.DECREASE_COMMON:
    LD L,(IY)
    LD H,(IY+1)
    XOR A
    SBC HL,BC
    EX DE,HL
    PUSH BC
    CALL .SIZETOMOVE
    DI
    LD A,B
    OR C
    JR Z,.L1
    LD L,(IY)
    LD H,(IY+1)
    LD (IX),E
    LD (IX+1),D
    LDIR
.L1:
    POP BC
    LD L,(IY)
    LD H,(IY+1)
    XOR A
    SBC HL,BC
    LD (IY),L
    LD (IY+1),H
    RET 
.INCREASE_COMMON:
    LD HL,(FREEMEMPTR)
    DEC HL
    XOR A
    SBC HL,BC
    EX DE,HL
    PUSH BC
    CALL .SIZETOMOVE
    DI
    LD A,B
    OR C
    JR Z,.L2
    LD HL,(FREEMEMPTR)
    DEC HL
    LDDR
.L2:
    POP BC
    LD L,(IY)
    LD H,(IY+1)
    ADD HL,BC
    LD (IY),L
    LD (IY+1),H
    RET
; *******************************************************************************************************