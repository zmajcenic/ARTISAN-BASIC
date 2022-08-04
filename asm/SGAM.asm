; Sprite Group Animate and Move

; *******************************************************************************************************
; function to handle CALL SGAM basic extension
; sets position of a group of sprites as described in SPRGRPMOV
; and manually animate a list of animations
; _SGAM ( INT x, 
;	      INT y, 
;		  BYTE count, 
;		  INT[2][count] data_ptr,
;         BYTE item_number,
;         INT[] sprite_animations )        
; will put ram in page 0 also, page 1 is already there
SGAM:
	LD A, (SPRATR_INIT_STATUS)
	OR A
	JP Z,ILLEGAL_FUNCTION
	; opening (
	CALL CHKCHAR
	DB '('
	; get x
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT),DE
	; comma
	CALL CHKCHAR
	DB ','
	; get y
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+2),DE
	; comma
	CALL CHKCHAR
	DB ','
	; get count
	LD IX, GETBYT
	CALL CALBAS
    OR A
    JP Z,SUBSCRIPT_OUT_OF_RANGE
	LD (BLIT_STRUCT+4),A
	; comma
	CALL CHKCHAR
	DB ','
	; get sprite group definition array data pointer
    LD A,(BLIT_STRUCT+4)
	LD E,A
	LD D,3
	LD A,2
	LD B,A
	CALL GET_BASIC_ARRAY_DATA_POINTER
	LD (BLIT_STRUCT+5),BC
	; comma
	CALL CHKCHAR
	DB ','
	; get sprite animation array size
	LD IX,GETBYT
	CALL CALBAS  
    LD (BLIT_STRUCT+7),A
    OR A
    JP Z,SUBSCRIPT_OUT_OF_RANGE
	; comma
	CALL CHKCHAR
	DB ','
    ; get array pointer
    LD A,(BLIT_STRUCT+7)
    LD D,A
    LD A,2
    LD B,1
    CALL GET_BASIC_ARRAY_DATA_POINTER
    LD (BLIT_STRUCT+8),BC
	; ending )
	CALL CHKCHAR
	DB ')'
 
    PUSH HL
    DI

	; enable page 0
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
    EXX
    LD DE,(BLIT_STRUCT) ; initial x
    LD BC,(BLIT_STRUCT+2) ; initial y
    EXX
    LD HL,(BLIT_STRUCT+5) ; pointer to data
    LD A,(BLIT_STRUCT+4) ; number of entries
    LD B,A
    CALL SPRGRPMOV.UPDATE_LOC

    LD HL,.STEP
    LD (ANIMSTARTSTOP_COMMON.FN+1),HL
    LD A,(BLIT_STRUCT+7) ; anim number
    LD B,A
    LD DE,(BLIT_STRUCT+8) ; anim list
.L1:
    PUSH BC
    LD A,(DE)
    .2 INC DE
    PUSH DE
    CALL ANIMSTARTSTOP_COMMON.SETVALUE
    POP DE
    POP BC
    DJNZ .L1

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
    EI
    POP HL
    RET
.STEP:
    LD B,1
    JP PROCESS_SINGLE_ANIMATION.INACTIVE_TOO
; *******************************************************************************************************
