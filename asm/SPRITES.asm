; commands and variables related to sprites

SPRATR_INIT_STATUS:
 DB 0
SPRATR_UPDATE_FLAG:
 DW 0
SPRATR_DATA:
 DW 0
SPRFLICKER_ENABLED:
 DB 0
; to support sprite flicker
FLICKER:
 DB 0

; to temporarily store stack pointer
TMPSP:
 DW 0

; *******************************************************************************************************
; helper function gets pointer to n-th entry in sprite attributes
; changes HL,DE;
GETnthSPRATTR:
    LD H,0
    LD L,A
    CALL HLx8
    LD DE,(SPRATR_DATA)
    ADD HL,DE
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function updates sprite attribute table in VRAM based on buffer of the form with rotating for flicker
; struct {
; DW y
; DW x
; DW pattern (0-63)
; DW color
; } [32]
; will hide sprites whose location is outside of visible area
; works in screen 1 and 2
; triggered by value in (SPRATR_UPDATE_FLAG) != 0 and after being done resets it to 0
; modifies AF, AF', BC, DE, HL
SPRATR_UPDATE:
	; check if initialized
	LD A, (SPRATR_INIT_STATUS)
	OR A
	RET Z
	; check if update requested
	LD HL, (SPRATR_UPDATE_FLAG)
	LD A, (HL)
	OR A
	RET Z
	; check screen mode
	LD A, (SCRMOD)
	DEC A
	JR Z, .L0 ; screen 1
	DEC A
	RET NZ ; not screen 2
.L0:
	LD B, 32 ; sprite number
	LD C, #98 ; register for vdp data output
	; set VDP address
	LD HL, (ATRBAS)
	LD A, (SPRFLICKER_ENABLED)
	OR A
	JR Z, .L3
	LD A, (FLICKER)
.L3:
	LD E, A
	EX AF, AF'
	LD A, E
	ADD A, A
	ADD A, A
	LD D, 0
	LD E, A
	ADD HL, DE
	CALL SETWRT_LOCAL
	LD (TMPSP), SP
	LD SP, (SPRATR_DATA)

.LOOP:
	POP HL
	INC H
	JR Z, .L1 ; negative number above -256
	DEC H
	JR NZ, .OUT3 ; sprite verticall can't be visible
	LD A, L
	CP 192
	JR NC, .OUT3
	DEC A ; due to VDP rule that top of screen is -1
	LD D, A
	JP .X
.L1:
	LD A, L
	ADD 16
	JP M, .OUT3 ; below -16
	DEC L ; due to VDP rule that top of screen is -1
	LD D, L
	JP .X
.OUT3:
	POP HL ; skip x value
.OUT2:
	POP HL ; skip pattern
	POP HL ; skip color
	LD A, #D1
	OUT (#98), A ; sprite hidden
	OUT (#98), A ; value unimportant
	OUT (#98), A ; value unimportant
	OUT (#98), A ; value unimportant
	JP .NEXT
.X:
	POP HL
	INC H
	JR Z, .L2
	DEC H
	JR NZ, .OUT2
	LD E, 0 ; EC bit
	JP .XY
.L2:
	LD A, L
	ADD 32
	JP M, .OUT2
	LD L, A
	LD E, #80
.XY:
	OUT (C), D
	OUT (C), L
	POP HL ; pattern
	LD A, (REG1SAV)
	AND 2
	LD A, L
	JR Z, .SMALLSPRITES
	ADD A, A
	ADD A, A ; needs to go at 4x
.SMALLSPRITES:
	OUT (#98), A
	POP HL ; color
	LD A, L
	OR E
	OUT (#98), A
.NEXT:
	EX AF, AF'
	INC A
	AND 31
	JP NZ, .NEXT2
	EX AF, AF'
	LD HL, (ATRBAS)
	; CALL SETWRT_LOCAL not allowed as SP modified
	LD	A, L
	OUT	(099H), A
	LD	A, H
	AND	03FH
	OR	040H
	OUT	(099H), A
	JP .NEXT3
.NEXT2:
	EX AF, AF'
.NEXT3:
	DJNZ .LOOP
	EX AF, AF'
	INC A
	LD (FLICKER), A

	LD SP, (TMPSP)
	LD HL, (SPRATR_UPDATE_FLAG)
	LD (HL), 0 ; zero out update flag
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL SPRENABLE basic extension
; initializes sprites handler
; _SPRENABLE ( INT sprites_attributes_data, 
;			   INT update_variable_location,
;			   INT sprite_flicker_enabled )
; expects both locations to be in range #8000+ or throws an error
; since these should be BASIC variables
; sets variables SPRATR_INIT_STATUS, SPRATR_UPDATE_FLAG, SPRATR_DATA and SPRFLICKER_ENABLED
SPRENABLE:
	; opening (
	CALL CHKCHAR
	DB '('
	; get address of sprite attribute table DIM SA%(3,31)
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get address of sprite update flag location
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get flicker enabled flag
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; ending )
	CALL CHKCHAR
	DB ')'

	POP DE ; get flicker flag
	LD A, D
	OR E
	LD (SPRFLICKER_ENABLED), A

	POP DE ; update variable location
	BIT 7, D ; is address >= &h8000
	JR NZ, .L1
	LD E, 5 ; illegal function call
	JP THROW_ERROR
.L1:
	LD (SPRATR_UPDATE_FLAG), DE
	POP DE ; address of sprite attribute table
	BIT 7, D ; is address >= &h8000
	JR NZ, .L2
	LD E, 5 ; illegal function call
	JP THROW_ERROR
.L2:
	LD (SPRATR_DATA), DE
	LD A, 1
	LD (SPRATR_INIT_STATUS), A
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL SPRDISABLE basic extension
; disables sprites handling
; _SPRDISABLE
; resets variable SPRATR_INIT_STATUS 
SPRDISABLE:
	XOR A
	LD (SPRATR_INIT_STATUS), A
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL SPRSET basic extension
; sets position, and optionally pattern and color of sprite
; _SPRSET ( BYTE sprite_num , valid 0-31
;			INT x, 
;			INT y, 
;			INT pattern, valid 0-63, otherwise ignored
;			INT color, valid 0-15, otherwise ignored
SPRSET:
	LD A, (SPRATR_INIT_STATUS)
	OR A
	JR NZ, .L1
	LD E, 5 ; illegal function call
	JP THROW_ERROR
.L1:
	; opening (
	CALL CHKCHAR
	DB '('
	; get sprite number
	LD IX, GETBYT
	CALL CALBAS
	PUSH AF
	; comma
	CALL CHKCHAR
	DB ','
	; get x
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get y
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get pattern
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get color
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; ending )
	CALL CHKCHAR
	DB ')'

    ; save position in BASIC text
	PUSH HL
	POP IX

	POP BC ; color
	POP DE ; pattern
	EXX
	POP BC ; y
	POP DE ; x
	POP AF ; sprite number
	CP 32
	JR C, .L2
	LD E, 5 ; illegal function call
	JP THROW_ERROR
.L2:
	; find location in sprite attributes table
	PUSH DE
	CALL GETnthSPRATTR
	POP DE
	; set y
	LD (HL), C
	INC HL
	LD (HL), B
	INC HL
	; set x
	LD (HL), E
	INC HL
	LD (HL), D
	INC HL
	PUSH HL
	EXX
	POP HL
	; check if 0<=pattern<64
	LD A, D
	OR A
	JR NZ, .L3
	LD A, E
	CP 64
	JR NC, .L3
	; set pattern
	;ADD A, A
	;ADD A, A
	;ADD A, A
	LD (HL), A
	INC HL
	LD (HL), D
	INC HL
	JR .L4
.L3:
	; skip pattern
	.2 INC HL
.L4:
	; check if 0<=color<16
	LD A, B
	OR A
	JR NZ, .L5
	LD A, C
	CP 16
	JR NC, .L5
	; set color
	LD (HL), C
	INC HL
	LD (HL), B

.L5:
	PUSH IX
	POP HL
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function sets sprite location based on initial coordinates and offset provided
; input A=sprite number in SPRATR_DATA , 0-31
; input DE=initial x
; input BC=initial y
; input IY=location where delta y,x are located
; modifies AF, HL, IX
SPRSET_DELTA_POS:
	PUSH DE
	CALL GETnthSPRATTR
	PUSH HL
	POP IX
	POP DE
	; IX=sprite's y location
	LD L, (IY)
	LD H, (IY+1)
	ADD HL, BC
	LD (IX), L
	LD (IX+1), H
	LD L, (IY+2)
	LD H, (IY+3)
	ADD HL, DE
	LD (IX+2), L
	LD (IX+3), H
	RET
; *******************************************************************************************************	

; *******************************************************************************************************
; function to handle CALL SPRGRPMOV basic extension
; sets position of a group of sprites described with
; { int sprite_num, int delta_y, int delta_x } [count]
; _SPRGRPMOV ( INT x, 
;			   INT y, 
;			   INT data_ptr, 
;			   BYTE count, 
; will put ram in page 0 also, page 1 is already there
SPRGRPMOV:
	LD A, (SPRATR_INIT_STATUS)
	OR A
	JR NZ, .L1
	LD E, 5 ; illegal function call
	JP THROW_ERROR
.L1:
	; opening (
	CALL CHKCHAR
	DB '('
	; get x
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get y
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get data pointer
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get count
	LD IX, GETBYT
	CALL CALBAS
	PUSH AF
	; ending )
	CALL CHKCHAR
	DB ')'

	PUSH HL
	POP IX

	POP BC ; count
	POP HL ; data pointer
	EXX
	POP BC ; y
	POP DE ; x
	EXX
	
	PUSH IX ; save position in BASIC buffer

	PUSH BC
	PUSH HL
    XOR A
    CALL GET_PAGE_INFO
	EXX
	POP HL
	POP AF
	EXX
    PUSH BC
    PUSH DE
	EXX
	PUSH AF
	PUSH HL
	EXX
    LD A, (RAMAD0)
    LD H, 0
    CALL LOCAL_ENASLT
	EI
	POP HL
	POP BC
	CALL .UPDATE_LOC
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
	POP HL
	RET

.UPDATE_LOC:
.L4:
	LD A, (HL)
	INC HL
	INC HL
	PUSH HL
	POP IY
	EXX
	CALL SPRSET_DELTA_POS
	EXX
	.4 INC HL
	DJNZ .L4
	RET
; *******************************************************************************************************

