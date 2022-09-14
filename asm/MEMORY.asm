; *******************************************************************************************************
; function to handle CALL MEMCPY basic extension
; _MEMCPY ( INT source, 
;			INT destination, 
;			INT count, 
; will put ram in page 0 also, page 1 is already there
MEMCPY:
	; opening (
	CALL CHKCHAR
	DB '('
	; get source address
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get destination address
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get length
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; ending )
	CALL CHKCHAR
	DB ')'

	; save position
	PUSH HL
	POP IX

	POP BC ; count
	POP DE ; destination
	POP HL ; source
	EXX
	; enable page 0
	DI
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	EXX
	LDIR
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
	PUSH IX
	POP HL
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL FILRAM basic extension
; FILRAM ( INT start address, 
;		   INT count, 
;		   BYTE value )
; will put ram in page 0 also, page 1 is already there
FILRAM:
	; opening (
	CALL CHKCHAR
	DB '('
	; get start address
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get count
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get value
	LD IX, GETBYT
	CALL CALBAS
	PUSH AF
	; ending )
	CALL CHKCHAR
	DB ')'

	; save position
	PUSH HL
	POP IX

	POP DE ; actually AF
	POP BC ; count
	POP HL ; start address
	LD A, B
	OR A
	JR NZ, .L1 ; >=256 bytes to fill
	OR C
	JR Z, .EXIT ; 0 bytes to fill, skip
	LD A, C
	DEC A
	JR NZ, .L1 ; ; >1 byte to fill
	; one byte to fill
	LD (HL), D
	JR .EXIT
.L1:
	EXX
	; enable page 0
	DI
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	EXX
	CALL .FILLVALUE
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
.EXIT:
	PUSH IX
	POP HL
	RET

.FILLVALUE:
    LD (HL), D
    LD D, H
    LD E, L
    INC DE
    DEC BC
    LDIR
    RET
; *******************************************************************************************************

