 IF (DEFUSR_EXTENSION == 1)
; *******************************************************************************************************
; same as GENCAL but for DEFUSR approach
; input IX=pointer to input array, real data from +2
; +2 = function address to call
; +4 = register list array pointer
GENCAL_DEFUSR:
    LD L,(IX+2)
    LD H,(IX+3)
    PUSH HL
    LD L,(IX+4)
    LD H,(IX+5)
    PUSH HL
    JR GENCAL.COMMON
; *******************************************************************************************************
 ENDIF

; *******************************************************************************************************
; function to handle CALL GENCAL basic extension
; GENCAL ( INT fn_addr, = address of the function to call
;		   INT[] reg_list_ptr, = array holding register values (AF,BC,DE,HL,IX,IY)
; output values of registers will also be stored at reg_list_ptr
GENCAL:
 IF (BASIC_EXTENSION == 1)
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
 ENDIF
.COMMON:
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