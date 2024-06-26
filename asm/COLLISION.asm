; generic collision checking routines
; in BASIC we use rectangle structure array DIM R%(7,n) of the format
; R%(0,n) is active flag, <>0 active / 0 skipped in checks
; R%(1,n) is either upper left x coordinate or sprite id (0-31) depending on R(7,n)
; R%(2,n) is either upper left y coordinate or unused depending on R(7,n)
; R%(3,n) x offset where rectangle begins
; R%(4,n) y offset where rectangle begins
; R%(5,n) is width
; R%(6,n) is height
; R%(7,0) is type, 0=generic where R%(1,0) contains x coordinate
;                  <>0 sprite where R%(1,0) contains sprite id 
; for type sprite, upper left coordinates are taken from sprite attribute array

; ************************************************************************************************
; quick test if HL<=DE<=HL+BC 
; input BC=width, DE=x, HL=min
; if not true flag C set
; modifies AF
GENERIC_INNER_CHECK:
    PUSH HL
    PUSH DE
    EX DE,HL
    AND A 
    SBC HL, DE 
    JP M, .GENERIC_INNER_CHECK_NOT
    AND A
    SBC HL, BC 
    JR Z, .L2
    JP P, .GENERIC_INNER_CHECK_NOT
.L2:
    AND A
    JR .EXIT
.GENERIC_INNER_CHECK_NOT:
    SCF 
.EXIT:
    POP DE
    POP HL
    RET
; ************************************************************************************************

; ************************************************************************************************
; function to check if rectangles are overlapping
; input IX=IY=pointer to struct
;  +00 active flag
;  +02 x coordinate
;  +04 y coordinate
;  +06 x offset where rectangle begins
;  +08 y offset where rectangle begins
;  +10 width
;  +12 height
; where IY is used to read +2 and +4, and IX to read +6, +8, +10 and +12
; this is a hack to allow location being taken from sprite attributes table
; input TMP_STRUCT data
;  +00 x coordinate
;  +02 y coordinate
;  +04 width
;  +06 height
; returns CF=1 if not overlapping, CF=0 if overlapping
RECTANGLE_OVERLAP_CHECK:
    ; first check which rectangle is higher
    LD L,(IX+12)
    LD H,(IX+13)
    LD DE,(TMP_STRUCT+6)
    AND A
    SBC HL,DE
    LD L,(IY+4)
    LD H,(IY+5)
    LD E,(IX+8)
    LD D,(IX+9)
    JP M,.L1
    ; equally high or IX defined one higher
    ; check upper boundary
    ADD HL,DE
    LD DE,(TMP_STRUCT+2)
    LD C,(IX+12)
    LD B,(IX+13)
    CALL GENERIC_INNER_CHECK
    JR NC,.INSIDE
    ; check lower boundary
    PUSH HL
    LD HL,(TMP_STRUCT+6)
    ADD HL,DE
    EX DE,HL
    POP HL
    CALL GENERIC_INNER_CHECK
    JR NC,.INSIDE
    RET ; not overlapping
.L1:
    ; rectangle defined in TMP_STRUCT is higher
    ADD HL,DE
    EX DE,HL
    LD HL,(TMP_STRUCT+2)
    LD BC,(TMP_STRUCT+6)
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
    LD DE,(TMP_STRUCT+4)
    AND A
    SBC HL,DE
    LD L,(IY+2)
    LD H,(IY+3)
    LD E,(IX+6)
    LD D,(IX+7)
    JP M,.L2
    ; equally wide or IX defined one wider
    ; check left boundary
    ADD HL,DE
    LD DE,(TMP_STRUCT+0)
    LD C,(IX+10)
    LD B,(IX+11)
    CALL GENERIC_INNER_CHECK
    RET NC ; overlap
    ; check right boundary
    PUSH HL
    LD HL,(TMP_STRUCT+4)
    ADD HL,DE
    EX DE,HL
    POP HL
    JP GENERIC_INNER_CHECK ; CF and result set by fn call
.L2:
    ; rectangle defined in TMP_STRUCT is higher
    ADD HL,DE
    EX DE,HL
    LD HL,(TMP_STRUCT+0)
    LD BC,(TMP_STRUCT+4)
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
; input TMP_STRUCT data
;  +00 x coordinate
;  +02 y coordinate
;  +04 width
;  +06 height
;  +08 number of items in a list, described under RECTANGLE_OVERLAP_CHECK
;  +09 pointer to first element of R%(7,n)
;  +11 pointer to INT result variable
; returns CF=1 if not overlapping
; returns A=list index and CF=0 if overlapping
FIND_OVERLAP:
    LD A,(TMP_STRUCT+8)
    LD B,A
    LD IX,(TMP_STRUCT+9)
.L1:
    PUSH BC
    ; check active flag
    LD A,(IX)
    OR (IX+1)
    JR Z,.NEXT
    ; check type
    LD A,(IX+14)
    OR (IX+15)
    JR NZ,.L2
    PUSH IX
    POP IY
.L3:
    CALL RECTANGLE_OVERLAP_CHECK
    JR C,.NEXT
    ; found
    POP BC
    LD A,(TMP_STRUCT+8)
    SUB B
    AND A
    RET
.NEXT:
    LD DE,16
    ADD IX,DE
    POP BC
    DJNZ .L1
    SCF
    RET 
.L2:
    ; sprite, need to build a temporary data struct since x and y values are inversed
    ; at TMP_STRUCT+13
    LD A,(IX+2) ; sprite ID
    CALL GETnthSPRATTR
    LD IY,TMP_STRUCT+11
    LD A,(HL)
    LD (IY+4),A
    INC HL
    LD A,(HL)
    LD (IY+5),A
    INC HL
    LD A,(HL)
    LD (IY+2),A
    INC HL
    LD A,(HL)
    LD (IY+3),A
    JR .L3
; ************************************************************************************************

 IF (BASIC_EXTENSION == 1)
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
	LD (TMP_STRUCT+11),DE
	; comma
	CALL CHKCHAR
	DB ','
	; get x
	LD IX, FRMQNT
	CALL CALBAS
	LD (TMP_STRUCT+0),DE
	; comma
	CALL CHKCHAR
	DB ','
	; get y
	LD IX, FRMQNT
	CALL CALBAS
	LD (TMP_STRUCT+2),DE
	; comma
	CALL CHKCHAR
	DB ','
	; get width
	LD IX, FRMQNT
	CALL CALBAS
	LD (TMP_STRUCT+4),DE
	; comma
	CALL CHKCHAR
	DB ','
	; get height
	LD IX, FRMQNT
	CALL CALBAS
	LD (TMP_STRUCT+6),DE
	; comma
	CALL CHKCHAR
	DB ','
	; get number of items in a list
	LD IX, GETBYT
	CALL CALBAS
	LD (TMP_STRUCT+8),A
	; comma
	CALL CHKCHAR
	DB ','
	; get address of rectangle structure array DIM R%(7,n)
	LD A,(TMP_STRUCT+8)
    LD E,A
    LD A,2
	LD B,A
	LD D,7
	CALL GET_BASIC_ARRAY_DATA_POINTER
	LD (TMP_STRUCT+9),BC
	; ending )
	CALL CHKCHAR
	DB ')'

    PUSH HL
    CALL FIND_OVERLAP
    LD HL,(TMP_STRUCT+11)
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
 ENDIF

 IF (DEFUSR_EXTENSION == 1)
; *******************************************************************************************************
; same as COLL but for DEFUSR approach
; input IX=pointer to input array, real data from +2
; +02 = pointer to result variable
; +04 = player X
; +06 = player Y
; +08 = player width
; +10 = player height
; +12 = number of list items
; +14 = pointer to list of collidable objects
COLL_DEFUSR:
    PUSH IX
    POP HL
    .4 INC HL ; skip over to player x
    LD DE,TMP_STRUCT
    LD BC,9
    LDIR ; copy over x,y,w,h,list item number
    LD A,(IX+14)
    LD (TMP_STRUCT+9),A
    LD A,(IX+15)
    LD (TMP_STRUCT+10),A ; address to collidable objects array
    LD A,(IX+2)
    LD (TMP_STRUCT+11),A
    LD A,(IX+3)
    LD (TMP_STRUCT+12),A ; address to results variable
    PUSH IX
    CALL FIND_OVERLAP
    POP IX
    LD L,(IX+2)
    LD H,(IX+3)
    JR C,.NOTFOUND
    LD (HL),A
    INC HL
    LD (HL),0
.EXIT:
    XOR A ; success
    RET 
.NOTFOUND:
    LD (HL),#FF
    INC HL
    LD (HL),#FF    
    JR .EXIT 
; *******************************************************************************************************
 ENDIF
 