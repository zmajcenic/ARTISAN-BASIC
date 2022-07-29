; *******************************************************************************************************
; generic function to implement tiling
; should be modified to call appropriate function for memory or vram
; input IX=pointer to following structure
; +00 tile_data_ptr
; +02 tile_rows
; +04 tile_columns
; +06 destination_address
; +08 dest_to_next_row_add_to_value
; +10 num_horizontal_tiles
; +12 num_vertical_tiles
; modifies AF, BC, DE, HL
TILE:
	LD L, (IX+6)
	LD H, (IX+7) ; destination address
	LD (TILETMP1), HL
	LD B, (IX+12) ; vertical tile number
.L1:
	PUSH BC
		LD L, (IX+0)
		LD H, (IX+1) ; tile address
		LD (TILETMP2), HL
		LD B, (IX+2) ; tile rows
.L2:
		PUSH BC
.CALL1:		CALL 0
			LD B, (IX+10) ; horizontal tile num
.L3:
			PUSH BC
				LD HL, (TILETMP2)
				LD B, (IX+4) ; tile columns
.L4:
				PUSH BC
.CALL2:
					CALL 0
				POP BC
				DJNZ .L4
			POP BC
			DJNZ .L3
			LD (TILETMP2), HL
			LD HL, (TILETMP1)
			LD E, (IX+8)
			LD D, (IX+9) ; add to value for dest next row
			ADD HL, DE
			LD (TILETMP1), HL
		POP BC
		DJNZ .L2
	POP BC
	DJNZ .L1
	RET
; *******************************************************************************************************

 IFNDEF CMDS_WITH_PARAMETERS
; *******************************************************************************************************
; function to handle CALL TILERAM basic extension
; fills memory with tiles
; TILERAM ( INT request_data_ptr )
; request_data_ptr described in TILE
; will put ram in page 0 also, page 1 is already there
TILERAM:
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
	; set RAM functions to call
	LD HL, .TILECOPY
	LD (TILE.CALL2+1), HL
	LD HL, .SETDESTROW
	LD (TILE.CALL1+1), HL
	LD A,1
	LD (VRAM_UPDATE_IN_PROGRESS),A
	CALL TILE
	XOR A
	LD (VRAM_UPDATE_IN_PROGRESS),A

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP HL
	RET
.TILECOPY:
	.8 LDI
	RET	
.SETDESTROW:
	LD DE, (TILETMP1)
	RET
; *******************************************************************************************************
 ENDIF

 IFDEF CMDS_WITH_PARAMETERS
; *******************************************************************************************************
; function to handle CALL TILERAM basic extension
; fills memory with tiles
; TILERAM ( INT tile_data_pointer,
;			INT tile_columns,
;			INT tile_rows,
;			INT destination_pointer,
;			INT destination_columns,
;			INT destination_rows,
;			INT destination_begin_column,
;			INT destination_begin_row,
;			INT number_of_tiles_horizontally,
;			INT	number_of_tiles_vertically )
; will put ram in page 0 also, page 1 is already there
TILERAM:
	; opening (
	CALL CHKCHAR
	DB '('
	; get tile data pointer coordinate
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+0), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get tile columns
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+4), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get tile columns
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+2), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get destintion pointer
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+6), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get destination columns
	LD IX, FRMQNT
	CALL CALBAS
	LD A, E
	LD (BLIT_TMP+0), A
	; comma
	CALL CHKCHAR
	DB ','
	; get destination rows
	LD IX, FRMQNT
	CALL CALBAS
	LD A, E
	LD (BLIT_TMP+1), A
	; comma
	CALL CHKCHAR
	DB ','
	; get destination begin column
	LD IX, FRMQNT
	CALL CALBAS
	LD A, E
	LD (BLIT_TMP+2), A
	; comma
	CALL CHKCHAR
	DB ','
	; get destination begin row
	LD IX, FRMQNT
	CALL CALBAS
	LD A, E
	LD (BLIT_TMP+3), A
	; comma
	CALL CHKCHAR
	DB ','
	; get number of tiles horizontally
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+10), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get number of tiles vertically
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+12), DE
	; ending )
	CALL CHKCHAR
	DB ')'

	PUSH HL ; save position in BASIC buffer

	; calculate destination add to value
	LD H, 0
	LD A, (BLIT_TMP+0)
	LD L, A
	CALL HLx8
	LD (BLIT_STRUCT+8), HL
	; calculate pointer to background location
	LD HL, 0
	LD A,(BLIT_TMP+3)
	OR A
	JR Z, .L1
	LD B,A
	LD DE,(BLIT_STRUCT+8)
.L0:
	ADD HL, DE
	DJNZ .L0
.L1:
	EX DE,HL
	LD H,0
	LD A,(BLIT_TMP+2)
	LD L,A
	CALL HLx8
	ADD HL,DE
	LD DE,(BLIT_STRUCT+6)
	ADD HL,DE
	LD (BLIT_STRUCT+6),HL

	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	; set RAM functions to call
	LD HL, .TILECOPY
	LD (TILE.CALL2+1), HL
	LD HL, .SETDESTROW
	LD (TILE.CALL1+1), HL
	LD IX,BLIT_STRUCT
	CALL TILE

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP HL
	RET
.TILECOPY:
	.8 LDI
	RET	
.SETDESTROW:
	LD DE, (TILETMP1)
	RET
; *******************************************************************************************************
 ENDIF

 IFDEF CMDS_WITH_PARAMETERS
; *******************************************************************************************************
; function to handle CALL TILEVRM basic extension
; fills vram with tiles
; TILEVRM ( INT tile_data_pointer,
;			INT tile_columns,
;			INT tile_rows,
;			INT destination_begin_column,
;			INT destination_begin_row,
;			INT number_of_tiles_horizontally,
;			INT	number_of_tiles_vertically )
; will put ram in page 0 also, page 1 is already there
; for destination uses address of SCREEN 2 pattern buffer and 32x24 size
TILEVRM:
	; opening (
	CALL CHKCHAR
	DB '('
	; get tile data pointer coordinate
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+0), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get tile columns
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+4), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get tile columns
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+2), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get destination begin column
	LD IX, FRMQNT
	CALL CALBAS
	LD A, E
	LD (BLIT_TMP+2), A
	; comma
	CALL CHKCHAR
	DB ','
	; get destination begin row
	LD IX, FRMQNT
	CALL CALBAS
	LD A, E
	LD (BLIT_TMP+3), A
	; comma
	CALL CHKCHAR
	DB ','
	; get number of tiles horizontally
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+10), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get number of tiles vertically
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+12), DE
	; ending )
	CALL CHKCHAR
	DB ')'

	PUSH HL ; save position in BASIC buffer

	; calculate destination add to value
	LD HL, 256
	LD (BLIT_STRUCT+8), HL
	; calculate pointer to background location
	LD A,(BLIT_TMP+3)
	LD H,A
	LD L,0
	EX DE,HL
	LD H,0
	LD A,(BLIT_TMP+2)
	LD L,A
	CALL HLx8
	ADD HL,DE
	LD DE,(GRPCGP)
	ADD HL,DE
	LD (BLIT_STRUCT+6),HL

	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	; set RAM functions to call
	LD HL, .TILECOPY
	LD (TILE.CALL2+1), HL
	LD HL, .SETDESTROW
	LD (TILE.CALL1+1), HL
	LD IX,BLIT_STRUCT
	CALL TILE

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP HL
	RET
.TILECOPY:
	LD BC, #0898
	JP BBYTECOPY_NO_C	
.SETDESTROW:
	LD HL, (TILETMP1)
	DI
	CALL SETWRT_LOCAL
	EI
	RET
; *******************************************************************************************************
 ENDIF

 IFNDEF CMDS_WITH_PARAMETERS
; *******************************************************************************************************
; function to handle CALL TILEVRM basic extension
; fills vram with tiles
; TILEVRM ( INT request_data_ptr )
; request_data_ptr described in TILE
; will put ram in page 0 also, page 1 is already there
TILEVRM:
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
	; set RAM functions to call
	LD HL, .TILECOPY
	LD (TILE.CALL2+1), HL
	LD HL, .SETDESTROW
	LD (TILE.CALL1+1), HL
	CALL TILE

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP HL
	RET
.TILECOPY:
	LD BC, #0898
	JP BBYTECOPY_NO_C	
.SETDESTROW:
	LD HL, (TILETMP1)
	DI
	CALL SETWRT_LOCAL
	EI
	RET
; *******************************************************************************************************
 ENDIF