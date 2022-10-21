; *******************************************************************************************************
; function rotates mask and data of several characters and applies to background data
; this handles x-shift from 0 to 4
; contains self-modifying code that is set-up from external function
; input HL=pointer to mask data
; input HL'=pointer to character data
; input DE=output buffer containing background data
; input BC=DE+8
; input A=number of characters to process
; modifies AF, AF', HL, HL', DE, DE', BC, BC'
SHIFT04:
	EX AF, AF'
	LD A, (HL) ; get mask
	EXX
	LD D, A
	LD E, #FF
	SCF
.M1:
	JR .M1 ; this is self-modifying part
	RR D
	RR E
	RR D
	RR E
	RR D
	RR E
	RR D
	RR E

	LD B, (HL) ; get data
	LD C, 0
.M2:
	JR .M2 ; also self-modifying part
	SRL B
	RR C
	SRL B
	RR C
	SRL B
	RR C
	SRL B
	RR C

	EXX
	LD A, (DE) ; background
	EXX
	AND D
	OR B
	EXX
	LD (DE), A
	
	LD A, (BC)
	EXX
	AND E
	OR C
	INC HL
	EXX
	LD (BC), A
	
	INC HL
	INC DE
	INC BC
	
	EX AF, AF'
	DEC A
	JP NZ, SHIFT04
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function rotates mask and data of several characters and applies to background data
; this handles x-shift from 5 to 8
; contains self-modifying code that is set-up from external function
; input HL=pointer to mask data
; input HL'=pointer to character data
; input DE=output buffer containing background data
; input BC=DE+8
; input A=number of characters to process
; modifies AF, AF', HL, HL', DE, DE', BC, BC'
SHIFT58:
	EX AF, AF'
	LD A, (HL) ; get mask
	EXX
	LD D, A
	LD E, #FF
	SCF
.M1:
	JR .M1 ; this is self-modifying part
	RL D
	RL E
	RL D
	RL E
	RL D
	RL E

	LD B, (HL)
	LD C, 0
.M2:
	JR .M2 ; also self-modifying part
	SLA B
	RL C
	SLA B
	RL C
	SLA B
	RL C

	EXX
	LD A, (DE) ; background
	EXX
	AND E
	OR C
	EXX
	LD (DE), A
	
	LD A, (BC)
	EXX
	AND D
	OR B
	INC HL
	EXX
	LD (BC), A
	
	INC HL
	INC DE
	INC BC
	
	EX AF, AF'
	DEC A
	JP NZ, SHIFT58
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; routine that shifts one row of characters
; contains self-modifying code that is set-up from external function
; input HL=pointer to mask data
; input HL'=pointer to character data
; input DE=output buffer containing background data
; input A=number of characters to process
; input IX=pointer to structure describing input data
; modifies AF, AF', HL, HL', DE, DE', BC, BC'
SHIFT_ROW:
	PUSH AF
		LD (BLIT_TMP1), DE
		PUSH HL
			CALL .ADDYSHIFT
		POP HL
		LD (BLIT_TMP2), DE ; DE+vertical shift
.L1:
		LD A, 8
		SUB (IX+2) ; y shift
.CALL1:
		CALL 0
		LD A, (IX+2); y shift
		OR A
		JR Z, .DONE
		LD DE, (BLIT_TMP1)
		PUSH HL
			CALL .DETONEXTROW
		POP HL
.CALL2:
		CALL 0
		LD DE, (BLIT_TMP1)
		PUSH HL
			CALL .ADD8
		POP HL
		LD (BLIT_TMP1), DE
		LD DE, (BLIT_TMP2)
		PUSH HL
			CALL .ADD8
		POP HL
		LD (BLIT_TMP2), DE ; DE+vertical shift
.DONE:
	POP AF
	DEC A
	RET Z
	PUSH AF
	JP .L1
.ADDYSHIFT:
	EX DE, HL
	LD D, 0
	LD E, (IX+2); y shift
	JR .MOVDEBC
.ADD8:
	LD HL, 8
	JP .MOVDEBC
.DETONEXTROW:
	LD L, (IX+6)
	LD H, (IX+7) ; bkg add to value
.MOVDEBC:
	ADD HL, DE
	LD D, H
	LD E, L
	LD BC, 8
	ADD HL, BC
	LD B, H
	LD C, L
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function rotates mask and character data and applies it to background
; input IX=pointer to structure describing input data
; +0  DW horizontal shift count 0-7 (low byte used)
; +2  DW vertical shift count 0-7 (low byte used)
; +4  DW background data start;
; +6  DW background add to value to next row of background data
; +8  DW mask data start;
; +10  DW character data start;
; +12 DW character&mask add to value to next row of data
; +14 DW columns (low byte used)
; +16 DW rows (low byte used)
SHIFT_MERGE_CHARACTER:
	LD A, (IX) ; shift
	CP 5
	JR C, .RIGHT
	; shifts 5-7, use rotate towards left 1-3
	LD HL, SHIFT58
	LD (SHIFT_ROW.CALL1+1), HL ; modify fn used
	LD (SHIFT_ROW.CALL2+1), HL ; modify fn used
	SUB 5
	JR Z, .L1
	ADD A, A
	ADD A, A
	LD H, A
	LD L, #18 ; JR opcode
	LD (SHIFT58.M1), HL
	LD (SHIFT58.M2), HL
	JR .DO
.L1:
	LD HL, 0 ; 2xNOP opcode
	LD (SHIFT58.M1), HL
	LD (SHIFT58.M2), HL
	JR .DO
.RIGHT:
	; shifts 0-4, rotate towards right
	LD HL, SHIFT04
	LD (SHIFT_ROW.CALL1+1), HL ; modify fn used
	LD (SHIFT_ROW.CALL2+1), HL ; modify fn used
	CP 4
	JR Z, .R1
	SUB 4
	NEG
	ADD A, A
	ADD A, A
	LD H, A
	LD L, #18 ; JR opcode
	LD (SHIFT04.M1), HL
	LD (SHIFT04.M2), HL
	JR .DO
.R1:
	LD HL, 0 ; 2xNOP opcode
	LD (SHIFT04.M1), HL
	LD (SHIFT04.M2), HL
.DO:
	LD B, (IX+16) ; rows
	LD L, (IX+8)
	LD H, (IX+9) ; mask data
	LD E, (IX+4)
	LD D, (IX+5) ; background data
	EXX
	LD L, (IX+10)
	LD H, (IX+11) ; character data
	EXX
.LOOP:
	PUSH BC
		PUSH HL
			PUSH DE
				EXX
				PUSH HL
					EXX
					LD A, (IX+14) ; columns
.CALL:
					CALL SHIFT_ROW
				POP HL
				LD E, (IX+12)
				LD D, (IX+13) ; char data to next row
				ADD HL, DE
				EXX
			POP HL
			LD E, (IX+6)
			LD D, (IX+7) ; background to next row
			ADD HL, DE
			EX DE, HL
		POP HL
		LD C, (IX+12)
		LD B, (IX+13) ; char data to next row
		ADD HL, BC
	POP BC
	DJNZ .LOOP
	RET	
; *******************************************************************************************************

 IFNDEF CMDS_WITH_PARAMETERS
; *******************************************************************************************************
; function to handle CALL BLIT basic extension
; rotates 1-bit character drawing horizontally with mask and character data and
; fuses with background data and applies vertical shift too
; BLIT ( INT request_data_ptr )
; request_data_ptr described in SHIFT_MERGE_CHARACTER
; will put ram in page 0 also, page 1 is already there
BLIT:
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
	CALL SHIFT_MERGE_CHARACTER

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP HL
	RET
; *******************************************************************************************************
 ENDIF

 IFDEF CMDS_WITH_PARAMETERS
; *******************************************************************************************************
; function to handle CALL BLIT basic extension
; rotates 1-bit character drawing horizontally with mask and character data and
; fuses with background data and applies vertical shift too
; in form without pointers
; BLIT ( INT x,
;		 INT y,
;		 INT char_data_pointer,
;		 INT mask_data_pointer,
;		 INT width (in characters),
;		 INT height (in characters),
;		 INT background_pointer (top left), 
;		 INT background_width (in characters),
; will put ram in page 0 also, page 1 is already there
BLIT:
	; opening (
	CALL CHKCHAR
	DB '('
	; get x coordinate
	LD IX, FRMQNT
	CALL CALBAS
	LD A, E
	AND 7
	LD (BLIT_STRUCT+0), A
	CALL .DAdiv8
	LD (BLIT_TMP+0),A
	; comma
	CALL CHKCHAR
	DB ','
	; get y coordinate
	LD IX, FRMQNT
	CALL CALBAS
	LD A, E
	AND 7
	LD (BLIT_STRUCT+2), A
	CALL .DAdiv8
	LD (BLIT_TMP+1),A
	; comma
	CALL CHKCHAR
	DB ','
	; get char data pointer
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+10), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get mask data pointer
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+8), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get width
	LD IX, FRMQNT
	CALL CALBAS
	LD A, E
	LD (BLIT_STRUCT+14), A
	; comma
	CALL CHKCHAR
	DB ','
	; get height
	LD IX, FRMQNT
	CALL CALBAS
	LD A, E
	LD (BLIT_STRUCT+16), A
	; comma
	CALL CHKCHAR
	DB ','
	; get background pointer
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+4), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get background width
	LD IX, FRMQNT
	CALL CALBAS
	LD A, E
	LD (BLIT_TMP+2), A
	; ending )
	CALL CHKCHAR
	DB ')'

	PUSH HL ; save position in BASIC buffer

	; calculate char&mask add to value
	LD H, 0
	LD A, (BLIT_STRUCT+14)
	LD L, A
	CALL HLx8
	LD (BLIT_STRUCT+12), HL
	; calculate background add to value
	LD H, 0
	LD A, (BLIT_TMP+2)
	LD L, A
	CALL HLx8
	LD (BLIT_STRUCT+6), HL
	; calculate pointer to background location
	LD HL, 0
	LD A,(BLIT_TMP+1)
	OR A
	JR Z, .L1
	LD B,A
	LD DE,(BLIT_STRUCT+6)
.L0:
	ADD HL, DE
	DJNZ .L0
.L1:
	EX DE,HL
	LD H,0
	LD A,(BLIT_TMP+0)
	LD L,A
	CALL HLx8
	ADD HL,DE
	LD DE,(BLIT_STRUCT+4)
	ADD HL,DE
	LD (BLIT_STRUCT+4),HL

	DI
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	LD IX, BLIT_STRUCT
	CALL SHIFT_MERGE_CHARACTER

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP HL
	RET
.DAdiv8:
	LD A,E
	SRA D 
    RR  A
    SRA D 
    RR  A
    SRA D 
    RR  A
	RET
; *******************************************************************************************************
 ENDIF