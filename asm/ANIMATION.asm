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

; ANIMATION ITEM
; byte type = [0 - pattern and color change
;              1 - pattern definition change ]
; word ticks - number of ticks to hold this state
; for type = 0
;   byte pattern;
;   byte color;
; for type = 1
;   work data_pointer;
; total size = 5b

; ANIMATION DEFINITION
; byte number of items 1-15
; byte[15] anim_item;

; *******************************************************************************************************
; helper function HL=A*5
; changes HL,DE;
Ax5:
    LD H,0
    LD L,A
    LD D,H
    LD E,L
    ADD HL,HL
    ADD HL,HL
    ADD HL,DE
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL MAXANIMITEMS basic extension
; MAXANIMITEMS (BYTE number)
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
.ENTRY:
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
.E1:
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
.E2:
    LD HL,(FREEMEMPTR)
    ADD HL,BC
    LD (FREEMEMPTR),HL
    JR .EXIT
.SIZEDIFF:
    CALL Ax5
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
    ;LD (IX),E
    ;LD (IX+1),D
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

; *******************************************************************************************************
; function to handle CALL ANIMITEMPAT basic extension
; ANIMITEMPAT ( BYTE id,
;               INT ticks,
;               BYTE pattern,
;               BYTE color )
; fills animation item data, returns an error if out of bounds
ANIMITEMPAT:
    ; opening (
	CALL CHKCHAR
	DB '('
	; get id
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
    ; check if out of bounds
    INC A
    LD C,A
    LD A,(ANIMITEMNUM)
    CP C
    JP C,SUBSCRIPT_OUT_OF_RANGE
	; comma
	CALL CHKCHAR
	DB ','
	; get ticks
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get pattern
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
	; comma
	CALL CHKCHAR
	DB ','
	; get color
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
	; ending )
	CALL CHKCHAR
	DB ')'
.ENTRY:
    PUSH HL
    POP IX
    EXX
    POP BC ; color
    POP DE ; pattern
    POP HL ; ticks
    EXX
    POP AF
    CALL Ax5
    LD DE,(ANIMITEMPTR)
    ADD HL,DE
    PUSH HL
    POP IY
    EXX
    LD (IY),0 ; type=0
    LD (IY+1),L
    LD (IY+2),H
    LD (IY+3),D
    LD (IY+4),B
    
    PUSH IX
    POP HL
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL ANIMITEMPTR basic extension
; ANIMITEMPTR ( BYTE id,
;               INT ticks,
;               INT pointer,
; fills animation item data, returns an error if out of bounds
ANIMITEMPTR_CMD:
    ; opening (
	CALL CHKCHAR
	DB '('
	; get id
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
    ; check if out of bounds
    INC A
    LD C,A
    LD A,(ANIMITEMNUM)
    CP C
    JP C,SUBSCRIPT_OUT_OF_RANGE
	; comma
	CALL CHKCHAR
	DB ','
	; get ticks
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get pointer
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; ending )
	CALL CHKCHAR
	DB ')'
.ENTRY:
    PUSH HL
    POP IX
    EXX
    POP DE ; pointer
    POP HL ; ticks
    EXX
    POP AF
    CALL Ax5
    LD DE,(ANIMITEMPTR)
    ADD HL,DE
    PUSH HL
    POP IY
    EXX
    LD (IY),1 ; type=1
    LD (IY+1),L
    LD (IY+2),H
    LD (IY+3),E
    LD (IY+4),D
    
    PUSH IX
    POP HL
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL MAXANIMDEFS basic extension
; MAXANIMDEFS (BYTE number)
; sets new number and moves memory buffers as needed
MAXANIMDEFS:
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
.ENTRY:
    LD B,A
    LD A,(ANIMDEFNUM)
    SUB B
    JP Z, MAXANIMITEMS.EXIT; same value as before
    LD IX,ANIMDEFPTR
    LD IY,ANIMSPRPTR
    JP M, .INCREASE
    ; new value is lower than previous one
    CALL .SIZEDIFF
    CALL MAXANIMITEMS.DECREASE_COMMON
    JP MAXANIMITEMS.E1
.INCREASE:
    NEG
    CALL .SIZEDIFF
    CALL MAXANIMITEMS.INCREASE_COMMON
    JP MAXANIMITEMS.E2
.SIZEDIFF:
    LD H,0
    LD L,A
    CALL HLx16
    LD A,B
    LD (ANIMDEFNUM),A
    LD B,H
    LD C,L
    RET ; BC=size difference in bytes
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL ANIMDEF basic extension
; ANIMITEMPAT ( BYTE id,
;               BYTE size,
;               INT[] list )
; fills animation definition data, returns an error if out of bounds, or invalid type
ANIMDEF:
    ; opening (
	CALL CHKCHAR
	DB '('
	; get id
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
    ; check if out of bounds
    INC A
    LD C,A
    LD A,(ANIMDEFNUM)
    CP C
    JP C,SUBSCRIPT_OUT_OF_RANGE
	; comma
	CALL CHKCHAR
	DB ','
	; get size
	LD IX, GETBYT
	CALL CALBAS
    CP 16
    JP NC, OVERFLOW
    OR A
    JP Z, OVERFLOW
	PUSH AF
	; comma
	CALL CHKCHAR
	DB ','
	; get pointer to a list of animation items in integer array format
    LD A,1
    LD (SUBFLG),A ; search for arrays only
	LD IX, PTRGET
	CALL CALBAS
    ; contrary to documentation we get a pointer to array dimension in BC
    ; and type in VALTYP
    LD A,(VALTYP)
    CP 2
    JP NZ,TYPE_MISMATCH
    LD A,(BC)
    CP 1
    JP NZ,TYPE_MISMATCH
    INC BC
    LD A,(BC)
    POP DE
    PUSH DE
    INC A
    CP D
    JP C,SUBSCRIPT_OUT_OF_RANGE
    .2 INC BC
    PUSH BC
	; ending )
	CALL CHKCHAR
	DB ')'
.ENTRY:
    PUSH HL
    POP IX
    POP DE ; pointer to INT array
    POP BC ; B=item number
    POP AF ; id
    LD H,0
    LD L,A
    CALL HLx16
    PUSH DE
    LD DE,(ANIMDEFPTR)
    ADD HL,DE
    POP DE
    LD (HL),B
.L1:
    INC HL
    LD A,(DE)
    .2 INC DE
    LD (HL),A
    DJNZ .L1
    PUSH IX
    POP HL
    RET
; *******************************************************************************************************
