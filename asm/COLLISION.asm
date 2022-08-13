; ************************************************************************************************
; quick test if HL<=DE<=HL+BC 
; input BC=width, DE=x, HL=min
; if not true flag C set
; modifies AF
GENERIC_INNER_CHECK:
    PUSH HL
    AND A 
    SBC HL, DE 
    JP P, .GENERIC_INNER_CHECK_NOT
    AND A
    ADC HL, BC 
    JP M, .GENERIC_INNER_CHECK_NOT
    AND A
    JR .EXIT
.GENERIC_INNER_CHECK_NOT:
    SCF 
.EXIT:
    POP HL
    RET
; ************************************************************************************************

; ************************************************************************************************
; function to check if rectangles are overlapping
; input IX=pointer to struct
;  +00 active flag
;  +02 x coordinate
;  +04 y coordinate
;  +06 x offset where rectangle begins
;  +08 y offset where rectangle begins
;  +10 width
;  +12 height
; input BLIT_STRUCT data
;  +00 x coordinate
;  +02 y coordinate
;  +04 width
;  +06 height
; returns CF=1 if not overlapping, CF=0 if overlapping
RECTANGLE_OVERLAP_CHECK:
    ; first check which rectanlge is higher
    LD L,(IX+12)
    LD H,(IX+13)
    LD DE,(BLIT_STRUCT+6)
    AND A
    SBC HL,DE
    LD L,(IX+4)
    LD H,(IX+5)
    LD E,(IX+8)
    LD D,(IX+9)
    JP M,.L1
    ; equally high or IX defined one higher
    ; check upper boundary
    ADD HL,DE
    LD DE,(BLIT_STRUCT+2)
    LD C,(IX+12)
    LD B,(IX+13)
    CALL GENERIC_INNER_CHECK
    JR NC,.INSIDE
    ; check lower boundary
    PUSH HL
    LD HL,(BLIT_STRUCT+6)
    ADD HL,DE
    EX DE,HL
    POP HL
    CALL GENERIC_INNER_CHECK
    JR NC,.INSIDE
    RET ; not overlapping
.L1:
    ; rectangle defined in BLIT_STRUCT is higher
    ADD HL,DE
    EX DE,HL
    LD HL,(BLIT_STRUCT+2)
    LD BC,(BLIT_STRUCT+6)
    CALL GENERIC_INNER_CHECK
    JR NC,.INSIDE
    PUSH HL
    LD L,(IX+12)
    LD H,(IX+13)
    ADD HL,DE
    EX DE,HL
    POP HL
    CALL GENERIC_INNER_CHECK
    RET C
.INSIDE:
    ; check x coordinate
    ; first check which rectangle is wider
    LD L,(IX+10)
    LD H,(IX+11)
    LD DE,(BLIT_STRUCT+4)
    AND A
    SBC HL,DE
    LD L,(IX+2)
    LD H,(IX+3)
    LD E,(IX+6)
    LD D,(IX+7)
    JP M,.L2
    ; equally wide or IX defined one wider
    ; check left boundary
    ADD HL,DE
    LD DE,(BLIT_STRUCT+0)
    LD C,(IX+10)
    LD B,(IX+11)
    CALL GENERIC_INNER_CHECK
    RET NC ; overlap
    ; check right boundary
    PUSH HL
    LD HL,(BLIT_STRUCT+4)
    ADD HL,DE
    EX DE,HL
    POP HL
    JP GENERIC_INNER_CHECK ; CF and result set by fn call
.L2:
    ; rectangle defined in BLIT_STRUCT is higher
    ADD HL,DE
    EX DE,HL
    LD HL,(BLIT_STRUCT+0)
    LD BC,(BLIT_STRUCT+4)
    CALL GENERIC_INNER_CHECK
    RET NC ; overlap
    PUSH HL
    LD L,(IX+10)
    LD H,(IX+11)
    ADD HL,DE
    EX DE,HL
    POP HL
    JP GENERIC_INNER_CHECK
; ************************************************************************************************

; ************************************************************************************************
; function tries to find rectangle overlap and returns an index if found
; input BLIT_STRUCT data
;  +00 x coordinate
;  +02 y coordinate
;  +04 width
;  +06 height
;  +08 number of items in a list, described under RECTANGLE_OVERLAP_CHECK
;  +09 pointer to first element
;  +11 pointer to INT result variable
; returns CF=1 if not overlapping
; returns A=list index and CF=0 if overlapping
FIND_OVERLAP:
    LD A,(BLIT_STRUCT+8)
    LD B,A
    LD IX,(BLIT_STRUCT+9)
.L1:
    PUSH BC
    LD A,(IX)
    OR (IX+1)
    JR Z,.NEXT
    CALL RECTANGLE_OVERLAP_CHECK
    JR C,.NEXT
    ; found
    POP BC
    LD A,(BLIT_STRUCT+8)
    SUB B
    AND A
    RET
.NEXT:
    LD DE,14
    ADD IX,DE
    POP BC
    DJNZ .L1
    SCF
    RET 
; ************************************************************************************************

; ************************************************************************************************
; function to handle CALL COLL basic extension
; checks for collision between player and other rectangles
; COLL ( INT result variable, 
;	     INT player x value,
;	     INT player y value,
;	     INT player width,
;	     INT player height,
;	     INT number of items in a list,
;		 INT[6][n] rectangle struct )
; will fill result variable with index or -1 if no collision
; rectangle struct described under RECTANGLE_OVERLAP_CHECK
COLL:
	; opening (
	CALL CHKCHAR
	DB '('
	; get address of result variable
	LD IX, PTRGET
	CALL CALBAS
	LD (BLIT_STRUCT+11),DE
	; comma
	CALL CHKCHAR
	DB ','
	; get x
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+0),DE
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
	; get width
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+4),DE
	; comma
	CALL CHKCHAR
	DB ','
	; get height
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+6),DE
	; comma
	CALL CHKCHAR
	DB ','
	; get number of items in a list
	LD IX, GETBYT
	CALL CALBAS
	LD (BLIT_STRUCT+8),A
	; comma
	CALL CHKCHAR
	DB ','
	; get address of rectangle structure array DIM R%(6,n)
	LD A,(BLIT_STRUCT+8)
    LD E,A
    LD A,2
	LD B,A
	LD D,7
	CALL GET_BASIC_ARRAY_DATA_POINTER
	LD (BLIT_STRUCT+9),BC
	; ending )
	CALL CHKCHAR
	DB ')'

    PUSH HL
    CALL FIND_OVERLAP
    LD HL,(BLIT_STRUCT+11)
    JR C,.NOTFOUND
    LD (HL),A
    INC HL
    LD (HL),0
    POP HL
    RET 
.NOTFOUND:
    LD (HL),#FF
    INC HL
    LD (HL),#FF
    POP HL
    RET 
; ************************************************************************************************
