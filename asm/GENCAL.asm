; *******************************************************************************************************
; function to handle CALL GENCAL basic extension
; GENCAL ( INT fn_addr, = address of the function to call
;		   INT[] reg_list_ptr, = array holding register values (AF,BC,DE,HL,IX,IY)
; output values of registers will also be stored at reg_list_ptr
GENCAL:
	; opening (
	CALL CHKCHAR
	DB '('
	; get function address
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get pointer to register list
    LD A,2
    LD B,1
    LD DE,#0500
    CALL GET_BASIC_ARRAY_DATA_POINTER
	PUSH BC
	; ending )
	CALL CHKCHAR
	DB ')'

	; save BASIC token position
	PUSH HL
    EXX
	POP HL ; HL'=next basic token
    EXX

    POP HL ; get pointer to register values
	DI
    LD (BLIT_STRUCT), SP
    LD SP, HL
    POP AF
    POP BC
    POP DE
    POP HL
    POP IX
    POP IY
    EXX
    LD (BLIT_STRUCT+2), SP
    LD SP, (BLIT_STRUCT)
    EI
    POP DE ; get function to call
    PUSH HL
    CALL .EXXDECALL
    DI
    LD (BLIT_STRUCT), SP
    LD SP, (BLIT_STRUCT+2)
    PUSH IY
    PUSH IX
    PUSH HL
    PUSH DE
    PUSH BC
    PUSH AF
    LD SP, (BLIT_STRUCT)
    EI
    POP HL
	RET 

.EXXDECALL:
    PUSH DE
    EXX
    RET
; *******************************************************************************************************