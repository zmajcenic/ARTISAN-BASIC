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
; copies data with window like boundaries within ram
; BOXMEMCPY ( INT source data pointer,
;			  INT source number of bytes in a row,
;			  INT number of rows,
;			  INT source add-to value till next row,
; 			  INT destination pointer,
;			  INT destination add-to value till next row )
; request_data_ptr described in RECTANGLE_COPY
; will put ram in page 0 also, page 1 is already there
BOXMEMCPY:
	LD DE,BOXMEMCPY.RET
	LD (BOXCOMMON.ADDR+2), DE
	JP BOXCOMMON
.RET:
	EI
	; set RAM functions to call
	LD HL, 0
	LD (RECTANGLE_COPY.CALL1), HL ; NOP NOP
	LD (RECTANGLE_COPY.CALL1+2), HL ; NOP NOP
	LD HL, #B0ED ; LDIR
	LD (RECTANGLE_COPY.CALL1+4), HL
	JP BOXCOMMON.CALL
; *******************************************************************************************************

; *******************************************************************************************************
; common parts of BOX commands
BOXCOMMON:
	; opening (
	CALL CHKCHAR
	DB '('
	; get source data pointer
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+0), DE
	; comma
	CALL CHKCHAR
	DB ','
	; source number of bytes in a row
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+2), DE
	; comma
	CALL CHKCHAR
	DB ','
	; number of rows
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+4), DE
	; comma
	CALL CHKCHAR
	DB ','
	; source add-to value till next row
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+6), DE
	; comma
	CALL CHKCHAR
	DB ','
	; destination pointer
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+8), DE
	; comma
	CALL CHKCHAR
	DB ','
	; destination add-to value till next row
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+10), DE
	; ending )
	CALL CHKCHAR
	DB ')'

	PUSH HL ; save position in BASIC buffer
.ADDR:
	LD IY, 0
	JP ENABLE_PAGE0
.CALL:
	LD IX,BLIT_STRUCT
	CALL RECTANGLE_COPY
	XOR A
	LD (VRAM_UPDATE_IN_PROGRESS),A

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP HL
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL BOXMEMVRM basic extension
; copies data with window like boundaries from ram to Vram
; BOXMEMVRM ( INT source data pointer,
;			  INT source number of bytes in a row,
;			  INT number of rows,
;			  INT source add-to value till next row,
; 			  INT destination pointer,
;			  INT destination add-to value till next row )
; request_data_ptr described in RECTANGLE_COPY
; will put ram in page 0 also, page 1 is already there
BOXMEMVRM:
	LD DE,BOXMEMVRM.RET
	LD (BOXCOMMON.ADDR+2), DE
	JP BOXCOMMON
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
	JP BOXCOMMON.CALL
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