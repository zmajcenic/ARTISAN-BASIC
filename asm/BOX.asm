; *******************************************************************************************************
; generic function to implement rectangle data copy
; should be modified to call appropriate function for memory or vram
; input IX=pointer to following structure
; +00 source data pointer
; +02 num bytes in a row
; +04 number of rows
; +06 source add-to value till next row
; +08 destination address
; +10 destination add-to value till next row
; modifies AF, BC, DE, HL
RECTANGLE_COPY:
	LD L, (IX+0)
	LD H, (IX+1) ; source address
	LD E, (IX+8)
	LD D, (IX+9) ; destination
	LD B, (IX+4) ; row number
.L1:
	PUSH BC
		PUSH HL
			PUSH DE
				LD C, (IX+2)
				LD B, (IX+3) ; num bytes in a row
.CALL1:
				CALL 0 ; set destination address from DE
.CALL2:
				CALL 0 ; copy data fn
			POP HL
			LD C, (IX+10)
			LD B, (IX+11) ; destination add-to
			ADD HL, BC
			EX DE, HL
		POP HL
		LD C, (IX+6)
		LD B, (IX+7) ; src add-to
		ADD HL, BC
	POP BC
	DJNZ .L1
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL BOXMEMCPY basic extension
; copies data with window like boundaries to ram
; BOXMEMCPY ( INT request_data_ptr )
; request_data_ptr described in RECTANGLE_COPY
; will put ram in page 0 also, page 1 is already there
BOXMEMCPY:
	; opening (
	CALL CHKCHAR
	DB '('
	; get pointer to request struct
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; ending )
	CALL CHKCHAR
	DB ')'

	POP IX ; pointer to request struct

	PUSH HL ; save position in BASIC buffer

	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	; set RAM functions to call
	LD HL, 0
	LD (RECTANGLE_COPY.CALL1), HL ; NOP NOP
	LD (RECTANGLE_COPY.CALL1+2), HL ; NOP NOP
	LD HL, #B0ED ; LDIR
	LD (RECTANGLE_COPY.CALL1+4), HL
	CALL RECTANGLE_COPY

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP HL
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL BOXMEMVRM basic extension
; copies data with window like boundaries to Vram
; BOXMEMVRM ( INT request_data_ptr )
; request_data_ptr described in RECTANGLE_COPY
; will put ram in page 0 also, page 1 is already there
BOXMEMVRM:
	; opening (
	CALL CHKCHAR
	DB '('
	; get pointer to request struct
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; ending )
	CALL CHKCHAR
	DB ')'

	POP IX ; pointer to request struct

	PUSH HL ; save position in BASIC buffer

	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	; set RAM functions to call
	LD HL, .SETDEST
	LD (RECTANGLE_COPY.CALL1+1), HL
	LD HL, .COPYDATA
	LD (RECTANGLE_COPY.CALL2+1), HL
	LD A, #CD ; CALL
	LD (RECTANGLE_COPY.CALL1), A
	LD (RECTANGLE_COPY.CALL2), A
	;LD A,1
	LD (VRAM_UPDATE_IN_PROGRESS),A
	CALL RECTANGLE_COPY
	XOR A
	LD (VRAM_UPDATE_IN_PROGRESS),A

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP HL
	RET
.SETDEST:
	EX DE, HL
	DI
	CALL SETWRT_LOCAL
	EI
	EX DE, HL
	RET	
.COPYDATA:
	LD B, C
	JP BBYTECOPY
; *******************************************************************************************************