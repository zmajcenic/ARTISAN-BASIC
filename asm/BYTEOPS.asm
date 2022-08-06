; *******************************************************************************************************
; function to return low byte of input parameter
; LO (INT variable, INT number)
LO:
	; opening (
	CALL CHKCHAR
	DB '('
	; get variable pointer
	LD IX, PTRGET
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get number
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
 	; ending )
	CALL CHKCHAR
	DB ')'

	POP DE
	POP IX
	LD (IX),E
	LD (IX+1),0

    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to return high byte of input parameter
; HI (INT variable, INT number)
HI:
	; opening (
	CALL CHKCHAR
	DB '('
	; get variable pointer
	LD IX, PTRGET
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get number
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
 	; ending )
	CALL CHKCHAR
	DB ')'

	POP DE
	POP IX
	LD (IX),D
	LD (IX+1),0

    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to return WORD from two BYTE parameters
; W (INT variable, BYTE hi byte, BYTE low byte)
WORD:
	; opening (
	CALL CHKCHAR
	DB '('
	; get variable pointer
	LD IX, PTRGET
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get number1
	LD IX, GETBYT
	CALL CALBAS
	PUSH AF
	; comma
	CALL CHKCHAR
	DB ','
	; get number2
	LD IX, GETBYT
	CALL CALBAS
	PUSH AF
 	; ending )
	CALL CHKCHAR
	DB ')'

	POP DE
	POP BC
	POP IX
	LD (IX),D
	LD (IX+1),B

    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to cycle a variable between min and max value
; RANGE ( INT value variable, 
;		  INT delta variable,
;		  INT minimum,
;		  INT maximum )
; if below minimum or maximum delta variable is negated and value set to corresponding limit
RANGE:
	; opening (
	CALL CHKCHAR
	DB '('
	; get value variable pointer
	LD IX, PTRGET
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get delta variable pointer
	LD IX, PTRGET
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get minimum
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get maximum
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
 	; ending )
	CALL CHKCHAR
	DB ')'

	EXX
	POP DE ; maximum
	POP BC ; minimum
	POP IX ; delta pointer
	POP IY ; value pointer

	PUSH DE
	LD L,(IY)
	LD H,(IY+1)
	LD E,(IX)
	LD D,(IX+1)
	ADD HL,DE
	POP DE
	PUSH HL
	AND A
	SBC HL,DE ; new value-MAX
	JP P,.SETMAX
	POP HL
	PUSH HL
	AND A
	SBC HL,BC ; new value-MIN
	JP M,.SETMIN
	LD A,H
	OR L
	JR Z,.SETMIN
	POP HL
	LD (IY),L
	LD (IY+1),H
	EXX
	RET	
.SETMAX:
	LD (IY),E
	LD (IY+1),D
	JR .INVERT
.SETMIN:
	LD (IY),C
	LD (IY+1),B
.INVERT:
	POP HL
	LD HL,0
	LD E,(IX)
	LD D,(IX+1)
	AND A
	SBC HL,DE
	LD (IX),L
	LD (IX+1),H
	EXX
	RET
; *******************************************************************************************************


