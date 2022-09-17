; Sprite Group Animate and Move

; *******************************************************************************************************
; shared function to process a list of animations
; input B=list size
; input DE=list pointer
SGAM_PROCESS_ANIM_LIST:
    LD HL,.STEP
    LD (ANIMSTARTSTOP_COMMON.FN+1),HL
.L1:
    PUSH BC
    LD A,(DE)
    .2 INC DE
    PUSH DE
    CALL ANIMSTARTSTOP_COMMON.SETVALUE
    POP DE
    POP BC
    DJNZ .L1
	RET
.STEP:
    LD B,1
    JP PROCESS_SINGLE_ANIMATION.INACTIVE_TOO
; *******************************************************************************************************

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
.ENTRY:
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

    LD A,(BLIT_STRUCT+7) ; anim number
    LD B,A
    LD DE,(BLIT_STRUCT+8) ; anim list
	CALL SGAM_PROCESS_ANIM_LIST

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
    EI
    POP HL
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; handles automatic move and animate sprite groups during interrupt
PROCESS_AUTOSGAMS:
	LD A,(AUTOSGAMNUM)
	OR A
	RET Z
	LD B,A
	LD IX,(AUTOSGAMPTR)
.L1:
	PUSH BC
	LD A,(IX+19) ; active flag
	OR A
	JR Z,.LOOPEND
	; active, check timer
	LD L,(IX+22)
	LD H,(IX+23) ; timer
	DEC HL
	LD A,H
	OR L
	JR Z,.L2
	; not expired
	LD (IX+22),L
	LD (IX+23),H
	JR .LOOPEND
.L2:
	; expired, process

    ; set initial timer
    LD A,(IX+20)
    LD (IX+22),A
    LD A,(IX+21)
    LD (IX+23),A 

	CALL .MOVE
	CALL .UPDATELOC
	CALL .PROCESS_ANIM_LIST

.LOOPEND:
	LD DE,24
	ADD IX,DE
	POP BC
	DJNZ .L1
	RET	

.MOVE:
	; process movement
	LD A,(IX+10) ; direction
	OR A
	JR Z, .MOVE_L1
	; vertical
	LD L,(IX+2)
	LD H,(IX+3) ; vertical variable pointer
	JR .MOVE_L2
.MOVE_L1:
	; horizontal
	LD L,(IX+0)
	LD H,(IX+1) ; horizontal variable pointer
.MOVE_L2:
	PUSH HL
	POP IY
	LD L,(IY+0)
	LD H,(IY+1)
	LD E,(IX+8)
	LD D,(IX+9) ; delta value
	ADD HL,DE
	PUSH HL
	LD E,(IX+4)
	LD D,(IX+5) ; minimum value
	AND A
	SBC HL,DE
	JP M,.MOVE_L3 ; below minimum
	POP HL
	PUSH HL
	LD E,(IX+6)
	LD D,(IX+7) ; maximum value
	EX DE,HL
	AND A
	SBC HL,DE	
	JP M,.MOVE_L4 ; above maximum
	POP HL
	; within bounds
.MOVE_L5:
	LD (IY+0),L
	LD (IY+1),H
	RET	
.MOVE_L3:
	POP HL
	CALL .INVERSE_DELTA
	LD L,E
	LD H,D
	JR .MOVE_L5
.MOVE_L4:
	POP HL
	CALL .INVERSE_DELTA
	LD L,(IX+6)
	LD H,(IX+7) ; maximum
	JR .MOVE_L5
.INVERSE_DELTA:
	XOR A
	SUB (IX+8)
	LD (IX+8),A
	SBC A,A
	SUB (IX+9)
	LD (IX+9),A
	RET

.UPDATELOC:
	PUSH IX
	EXX
	LD L,(IX+0)
	LD H,(IX+1)
	LD E,(HL)
	INC HL
	LD D,(HL)
	LD L,(IX+2)
	LD H,(IX+3)
	LD C,(HL)
	INC HL
	LD B,(HL)
	EXX
	LD L,(IX+12)
	LD H,(IX+13) ; pointer to sprite group data
	LD B,(IX+11) ; sprite group size
	CALL SPRGRPMOV.UPDATE_LOC
	POP IX
	RET

.PROCESS_ANIM_LIST:
	PUSH IX
    LD B,(IX+14) ; anim list size
	BIT 7,(IX+9)
	JR Z,.PROCESS_ANIM_LIST_L1
	; negative direction
	LD E,(IX+15)
	LD D,(IX+16)
	JR .PROCESS_ANIM_LIST_L2
.PROCESS_ANIM_LIST_L1:
	; positive direction
	LD E,(IX+17)
	LD D,(IX+18)
.PROCESS_ANIM_LIST_L2:
	CALL SGAM_PROCESS_ANIM_LIST
	POP IX
	RET
; *******************************************************************************************************
