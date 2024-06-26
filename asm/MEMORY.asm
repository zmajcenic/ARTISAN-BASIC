 IF (BASIC_EXTENSION == 1)
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
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	EXX
	LDIR
	JP COMMON_EXIT_CODE_IX
; *******************************************************************************************************
 ENDIF

 IF (DEFUSR_EXTENSION == 1)
; *******************************************************************************************************
; same as MEMCPY but for DEFUSR approach
; input IX=pointer to input array, real data from +2
; +2 = source address
; +4 = destination address
; +6 = lenght
MEMCPY_DEFUSR:
	; enable page 0
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	LD L,(IX+2)
	LD H,(IX+3)
	LD E,(IX+4)
	LD D,(IX+5)
	LD C,(IX+6)
	LD B,(IX+7)
	LDIR
    JP COMMON_EXIT_CODE
; *******************************************************************************************************
 ENDIF

 IF (BASIC_EXTENSION == 1)
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
	EXX
	; enable page 0
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	EXX
	CALL FILVRM_FILLVALUE
	JP COMMON_EXIT_CODE_IX
; *******************************************************************************************************
 ENDIF

 IF (DEFUSR_EXTENSION == 1)
; *******************************************************************************************************
; same as FILVRM but for DEFUSR approach
; input IX=pointer to input array, real data from +2
; +2 = start address
; +4 = count
; +6 = value 
FILRAM_DEFUSR:
	; enable page 0
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	LD L,(IX+2)
	LD H,(IX+3)
	LD C,(IX+4)
	LD B,(IX+5)
	LD D,(IX+6)
	CALL FILVRM_FILLVALUE
    JP COMMON_EXIT_CODE
; *******************************************************************************************************
 ENDIF	

; *******************************************************************************************************
; common function to fill RAM
FILVRM_FILLVALUE:
	LD A,B
	OR C
	RET Z ; zero size
    LD (HL), D
    LD D, H
    LD E, L
    INC DE
    DEC BC
	LD A,B
	OR C
	RET Z ; if count was 1
    LDIR
    RET
; *******************************************************************************************************