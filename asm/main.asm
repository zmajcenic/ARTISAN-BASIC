 ORG 04000h

; control variables to include/exclude parts of code
SOUND_CMDS	 	EQU 1
RAM_CMDS 		EQU 1
VRAM_CMDS 		EQU 1
BLIT_CMDS		EQU 1
SPRITE_CMDS 	EQU 1
GENCAL_CMD		EQU 1
TILE_CMDS		EQU 1
BOX_CMDS		EQU 1
ANIM_CMDS		EQU 1

 DEFINE CMDS_WITH_PARAMETERS

CHPUT   EQU     #A2
CALBAS	EQU		#159
ERRHAND EQU     #406F
FRMEVL  EQU     #4C64
FRESTR	EQU		#67D0
; FRMQNT = formula quantificator
; input HL=pointer to current program expression
; output HL=next address
; output DE=integer datum
FRMQNT	EQU		#542F
; GETBYT = get byte parameter
; input HL=pointer to current program expression
; output HL=next address
; output A=E=byte read
GETBYT		EQU		#521C
CHRGTR  	EQU     #4666
PTRGET		EQU 	#5EA4
SUBFLG		EQU		#F6A5
SYNCHR		EQU		#558C
VALTYP  	EQU     #F663
USR     	EQU     #F7F8
PROCNM		EQU		#FD89
BIOS_FILVRM EQU     #0056
CLIKSW		EQU		#F3DB
ATRBAS		EQU		#F928
GRPCGP		EQU		#F3CB

RAMAD0	EQU	#F341	; Main-RAM Slot (00000h~03FFFh)
RAMAD1	EQU	#F342	; Main-RAM Slot (04000h~07FFFh)
RAMAD2	EQU	#F343	; Main-RAM Slot (08000h~0BFFFh)
RAMAD3	EQU	#F344	; Main-RAM Slot (0C000h~0FFFFh)
EXPTBL	EQU #FCC1
SCRMOD	EQU #FCAF ; current screen mode
REG1SAV EQU #F3E0 ; VDP(1)

; BASIC error codes
;01 NEXT without FOR 
;02 Syntax error 
;03 RETURN without GOSUB 
;04 Out of DATA 
;05 Illegal function call 
;06 Overflow 
;07 Out of memory 
;08 Undefined line number 
;09 Subscript out of range 
;10 Redimensioned array 
;11 Division by zero 
;12 Illegal direct 
;13 Type mismatch 
;14 Out of string space 
;15 String too long 
;16 String formula too complex 
;17 Can't CONTINUE 
;18 Undefined user function
;19 Device I/O error
;20 Verify error
;21 No RESUME
;22 RESUME without error
;23 Unprintable error
;24 Missing operand
;25 Line buffer overflow
;50 FIELD overflow
;51 Internal error
;52 Bad file number
;53 File not found
;54 File already open
;55 Input past end
;56 Bad file name
;57 Direct statement in file
;58 Sequential I/O only
;59 File not OPEN


 ; simulate cartridge with BASIC extension
 DW 04241H, 0, CALLHAND, 0, 0, 0, 0, 0

; this location #4010 stores last location used by basic extension
; free memory after that point
FREEMEMPTR:
 DW EXT_END 

; this location #4012 stores extension version in DAA format
; first byte is major version and second minor
VERSION:
 DB #00, #70

; binary included AKG player compiled at #4014
 IF (SOUND_CMDS == 1)
	INCBIN "bin/AKG.bin"
	INCLUDE "symbol/AKG.sym"
 ENDIF

ORIG.HTIMI:
	DB 0, 0, 0, 0, 0
 EXPORT ORIG.HTIMI

 IF (SOUND_CMDS == 1)
MUSIC_INIT_STATUS:
 DB 0
SFX_INIT_STATUS:
 DB 0
SOUND_ENABLED:
 DB 0
 ENDIF

 IF (SPRITE_CMDS == 1)
 INCLUDE "SPRITES.asm"
 ENDIF

 IF (ANIM_CMDS == 1)
 INCLUDE "ANIMATION.asm"
 ENDIF

; temp variables for BLIT, TILE functions
 IF (BLIT_CMDS + TILE_CMDS > 0)
BLIT_TMP:
TILETMP1:
BLIT_TMP1:
 DW 0
TILETMP2:
BLIT_TMP2:
 DW 0
  IFDEF CMDS_WITH_PARAMETERS
BLIT_STRUCT:
 DS 17
  ENDIF
 ENDIF

; List of pointers to available instructions (as ASCIIZ) and execute address (as word)
; per starting letter, if no commands with this letter, NULL value
CMDS:
 IF (ANIM_CMDS == 1)
	DW CMDS_A ;
 ELSE
    DW 0 ; A
 ENDIF
 IF (BLIT_CMDS + BOX_CMDS > 0)
    DW CMDS_B ; B
 ELSE
	DW 0
 ENDIF
    DW 0 ; C
    DW 0 ; D
    DW 0 ; E
 IF (VRAM_CMDS + RAM_CMDS > 0)
    DW CMDS_F; F
 ELSE
	DW 0
 ENDIF
 IF (GENCAL_CMD > 0)
    DW CMDS_G; G
 ELSE
	DW 0
 ENDIF
    DW 0 ; H
    DW 0 ; I
    DW 0 ; J
    DW 0 ; K
    DW 0 ; L
 IF (VRAM_CMDS + RAM_CMDS + ANIM_CMDS > 0)
    DW CMDS_M ; M
 ELSE
	DW 0
 ENDIF
    DW 0 ; N
    DW 0 ; O
    DW 0 ; P
    DW 0 ; Q
    DW 0 ; R
 IF (SOUND_CMDS + SPRITE_CMDS > 0)
    DW CMDS_S ; S
 ELSE
	DW 0
 ENDIF
 IF (TILE_CMDS > 0)
    DW CMDS_T ; T
 ELSE
	DW 0
 ENDIF
    DW 0 ; U
 IF (VRAM_CMDS > 0)
    DW CMDS_V ; V
 ELSE
	DW 0
 ENDIF
    DW 0 ; W
    DW 0 ; X
    DW 0 ; Y
    DW 0 ; Z

CMDS_M:
 IF (VRAM_CMDS == 1)
    DB "MEMVRM", 0
    DW MEMVRM
 ENDIF
 IF (RAM_CMDS == 1)
	DB "MEMCPY", 0
	DW MEMCPY
 ENDIF
 IF (ANIM_CMDS == 1)
	DB "MAXANIMITEMS",0
	DW MAXANIMITEMS
	DB "MAXANIMDEFS",0
	DW MAXANIMDEFS
	DB "MAXANIMSPRS",0
	DW MAXANIMSPRS
 ENDIF
 IF (VRAM_CMDS + RAM_CMDS + ANIM_CMDS > 0)
	DB 0
 ENDIF
CMDS_F:
 IF (VRAM_CMDS == 1)
    DB "FILVRM", 0
    DW FILVRM
 ENDIF
 IF (RAM_CMDS == 1)
    DB "FILRAM", 0
    DW FILRAM
 ENDIF
 IF (VRAM_CMDS + RAM_CMDS > 0)
    DB 0
 ENDIF
CMDS_G:
 IF (GENCAL_CMD == 1)
    DB "GENCAL", 0
    DW GENCAL
 ENDIF
 IF (GENCAL_CMD > 0)
	DB	0
 ENDIF
CMDS_V:
 IF (VRAM_CMDS == 1)
	DB "VRMMEM", 0
	DW VRMMEM
 ENDIF
 IF (VRAM_CMDS > 0)
	DB 0
 ENDIF
CMDS_S:
 IF (SPRITE_CMDS == 1)
	DB "SPRSET", 0
	DW SPRSET
	DB "SPRGRPMOV", 0
	DW SPRGRPMOV
 ENDIF
 IF (SOUND_CMDS == 1)
	DB "SNDSFX", 0
	DW SNDSFX
	DB "SNDPLYON", 0
	DW SNDPLYON
	DB "SNDPLYOFF", 0
	DW SNDPLYOFF
	DB "SNDPLYINI", 0
	DW SNDPLYINIT
 ENDIF
 IF (SPRITE_CMDS == 1)
	DB "SPRENABLE", 0
	DW SPRENABLE
	DB "SPRDISABLE", 0
	DW SPRDISABLE
 ENDIF
 IF (SOUND_CMDS + SPRITE_CMDS > 0)
	DB 0
 ENDIF
CMDS_B:
 IF (BLIT_CMDS == 1)
	DB "BLIT", 0
	DW BLIT
 ENDIF
 IF (BOX_CMDS == 1)
	DB "BOXMEMCPY", 0
	DW BOXMEMCPY
	DB "BOXMEMVRM", 0
	DW BOXMEMVRM
 ENDIF
 IF (BLIT_CMDS + BOX_CMDS > 0)
	DB 0
 ENDIF
CMDS_T:
 IF (TILE_CMDS == 1)
	DB "TILERAM", 0
	DW TILERAM
	DB "TILEVRM", 0
	DW TILEVRM
 ENDIF
 IF (TILE_CMDS > 0)
	DB 0
 ENDIF
CMDS_A:
 IF (ANIM_CMDS == 1)
	DB "ANIMITEMPAT",0
	DW ANIMITEMPAT
	DB "ANIMITEMPTR",0
	DW ANIMITEMPTR_CMD
	DB "ANIMDEF",0
	DW ANIMDEF
	DB "ANIMSPRITE",0
	DW ANIMSPRITE
	DB 0
 ENDIF

 IF (VRAM_CMDS + TILE_CMDS + SPRITE_CMDS > 0)
; ****************************************************************************************************
; function sets VRAM address
; input HL=address
; modifies AF
SETWRT_LOCAL:
	LD	A, L
	OUT	(099H), A
	LD	A, H
	AND	03FH
	OR	040H
	OUT	(099H), A
	RET
; ****************************************************************************************************
 ENDIF

 IF (VRAM_CMDS + TILE_CMDS > 0)
; ****************************************************************************************************
; function copies data from RAM to VRAM
; input HL=address in RAM
; input B=count
; modifies AF
BBYTECOPY:
	OUTI
	JP	NZ, BBYTECOPY
	RET
; ****************************************************************************************************
 ENDIF

; ****************************************************************************************************
; function multiplies HL by 16
HLx16:
	ADD HL,HL
; ****************************************************************************************************
; function multiplies HL by 8
HLx8:
	.3 ADD HL, HL
	RET
; ****************************************************************************************************

; ****************************************************************************************************
; function gets slot and subslot data for specific page
; input A=page (0, 1 or 2)
; output B = 0A8H register value
; output D = 0 is no subslots, 1 if yes
; output C = 0A8H value when page 3 slot equals to requested page slot
; output E = subslot value if present
; modifies AF, BC, DE, HL
GET_PAGE_INFO:
    LD L, A
    ADD A, low (EXPTBL)
    LD (GET_PAGE_INFO_L1+1), A
    IN A, (0A8H)
    LD B, A
    AND 03FH
    LD C, A
GET_PAGE_INFO_L1:
    LD A, (EXPTBL) ; modified by code above
    AND 080H
    JR Z, GET_PAGE_INFO_L2
    ; expanded
    DEC L
    JP M, GET_PAGE_INFO_L3
    DEC L
    JP M, GET_PAGE_INFO_L4
    ; page 2
    RLCA
    RLCA
GET_PAGE_INFO_L5:
    AND 0C0H
    OR C
    OUT (0A8H), A ; slot 3 = slot of page requested
    LD C, A
    LD A, (0FFFFH)
    CPL
    LD E, A
    LD D, 1
    LD A, B ; return stack
    OUT (0A8H), A
    RET 
GET_PAGE_INFO_L2:
    ; not expanded
    LD D, 0
    RET 
GET_PAGE_INFO_L4:
    ; page 1
    RRCA
    RRCA
GET_PAGE_INFO_L3:
    ; page 0
    RRCA
    RRCA
    JR GET_PAGE_INFO_L5
; ****************************************************************************************************

; ****************************************************************************************************
; function returns original slot and subslot info
; input B = 0A8H register value
; input D = 0 is no subslots, 1 if yes
; input C = 0A8H value when page 3 slot equals to requested page slot
; input E = subslot value if present
; modifies AF, disables interrupts
RESTORE_PAGE_INFO:
    LD A, D
    OR A
    JR Z, RESTORE_PAGE_INFO_L1
    LD A, C
	DI
    OUT (0A8H), A
    LD A, E
    LD (0FFFFH), A
RESTORE_PAGE_INFO_L1:
    LD A, B
    OUT (0A8H), A
    RET 
; ****************************************************************************************************

; *******************************************************************************************************
; SELECTS A SLOT IN THE PAGE SPECIFIED BY AN ADDRESS.
; INPUT:  A = SLOT ID: EXXXSSPP
; E = EXPANDED FLAG
; SS = SECONDARY SLOT NUMBER (ONLY IF EXPANDED)
; PP = PRIMARY SLOT NUMBER
;     HL = ADDRESS INSIDE THE PAGE TO CHANGE
; CHANGES: AF, BC, DE

LOCAL_ENASLT:
    CALL L0353 
    JP M, L0340 
    IN A, (0A8H) 
    AND C 
    OR B 
    OUT (0A8H), A 
    RET 
L0340:
    PUSH HL 
    CALL L0378 
    LD C, A 
    LD B, 0 
    LD A, L 
    AND H 
    OR D 
    LD HL, 0FCC5H 
    ADD HL, BC 
    LD (HL), A 
    POP HL 
    LD A, C 
    JR LOCAL_ENASLT
L0353:
    DI 
    PUSH AF 
    LD A, H 
    RLCA 
    RLCA 
    AND 3 
    LD E, A 
    LD A, 0C0H 
L035D:
    RLCA 
    RLCA 
    DEC E 
    JP P, L035D
    LD E, A 
    CPL 
    LD C, A 
    POP AF 
    PUSH AF 
    AND 3 
    INC A 
    LD B, A 
    LD A, 0ABH 
L036E:
    ADD A, 055H 
    DJNZ L036E
    LD D, A 
    AND E 
    LD B, A 
    POP AF 
    AND A 
    RET
L0378:
    PUSH AF
    LD A, D 
    AND 0C0H 
    LD C, A 
    POP AF 
    PUSH AF 
    LD D, A 
    IN A, (0A8H) 
    LD B, A 
    AND 03FH 
    OR C 
    OUT (0A8H), A 
    LD A, D 
    RRCA 
    RRCA 
    AND 3 
    LD D, A 
    LD A, 0ABH 
L0390:
    ADD A, 055H 
    DEC D 
    JP P, L0390
    AND E 
    LD D, A 
    LD A, E 
    CPL 
    LD H, A 
    LD A, (0FFFFH) 
    CPL 
    LD L, A 
    AND H 
    OR D 
    LD (0FFFFH), A 
    LD A, B 
    OUT (0A8H), A 
    POP AF 
    AND 3 
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; some common code to activate page 0 and place values needed to restore original page on stack
; input IY=return address
ENABLE_PAGE0:
    XOR A
    CALL GET_PAGE_INFO
    PUSH BC
    PUSH DE
    LD A, (RAMAD0)
    LD H, 0
    CALL LOCAL_ENASLT
	JP (IY)
; *******************************************************************************************************

; General BASIC CALL-instruction handler
CALLHAND:
	PUSH HL
	LD	HL, CMDS ; pointer table based on starting letter
    LD A, (PROCNM)
    SUB 'A'
    ADD A, A
    LD D, 0
    LD E, A
    ADD HL, DE
    LD E, (HL)
    INC HL
    LD D, (HL)
    LD A, D
    OR E
    JR Z, .CMDNOTRECOGNIZED
    EX DE, HL
.CHKCMD:
	LD	DE, PROCNM
.LOOP:	LD	A,(DE)
	CP	(HL)
	JR	NZ,.TONEXTCMD	; Not equal
	INC	DE
	INC	HL
	AND	A
	JR	NZ,.LOOP	; No end of instruction name, go checking
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	POP	HL		; routine address
	CALL	GETPREVCHAR
	CALL	.CALLDE		; Call routine
	AND	A
	RET
 
.TONEXTCMD:
	LD	C,0FFH
	XOR	A
	CPIR			; Skip to end of instruction name
	INC	HL
	INC	HL		; Skip address
	CP	(HL)
	JR	NZ,.CHKCMD	; Not end of table, go checking
.CMDNOTRECOGNIZED:
	POP	HL
    SCF
	RET
 
.CALLDE:
	PUSH	DE
	RET
 
;---------------------------
 
;GETSTRPNT:
; OUT:
; HL = String Address
; B  = Lenght
;        LD      HL,(USR)
;        LD      B,(HL)
;        INC     HL
;        LD      E,(HL)
;        INC     HL
;        LD      D,(HL)
;        EX      DE,HL
;        RET
 
;EVALTXTPARAM:
;	CALL	CHKCHAR
;	DEFB	"("             ; Check for (
;	LD	IX,FRMEVL
;	CALL	CALBAS		; Evaluate expression
;       LD      A,(VALTYP)
;        CP      3               ; Text type?
;        JP      NZ,TYPE_MISMATCH
;        PUSH	HL
;        LD	IX,FRESTR         ; Free the temporary string
;        CALL	CALBAS
;        POP	HL
;	CALL	CHKCHAR
;	DEFB	")"             ; Check for )
;        RET
 
 
CHKCHAR:
	CALL	GETPREVCHAR	; Get previous basic char
	EX	(SP),HL
	CP	(HL) 	        ; Check if good char
	JR	NZ,SYNTAX_ERROR	; No, Syntax error
	INC	HL
	EX	(SP),HL
	INC	HL		; Get next basic char
 
GETPREVCHAR:
	DEC	HL
	LD	IX,CHRGTR
	JP      CALBAS
 
 
TYPE_MISMATCH:
    LD E, 13 ; Type mismatch 
    JR THROW_ERROR
SUBSCRIPT_OUT_OF_RANGE:
    LD E,9 ; subscript out of range
	JR THROW_ERROR
OVERFLOW:
	LD E,6
	JR THROW_ERROR
SYNTAX_ERROR:
    LD E, 2 ; Syntax error 
THROW_ERROR:
	LD	IX,ERRHAND	; Call the Basic error handler
	JP	CALBAS
 
;---------------------------
 
 IF (RAM_CMDS == 1)
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
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
	PUSH IX
	POP HL
	RET
; *******************************************************************************************************
 ENDIF

 IF (VRAM_CMDS == 1)
; *******************************************************************************************************
; function to handle CALL FILVRM basic extension
; FILVRM ( INT offset, 
;		   INT count, 
;		   BYTE value,
;		   BYTE wait_vsync) >0 = true
; wait_vsync will issue HALT before copying
FILVRM:
	; opening (
	CALL CHKCHAR
	DB '('
	; get offset address
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
	; comma
	CALL CHKCHAR
	DB ','
	; get vsync wait
	LD IX, GETBYT
	CALL CALBAS
	PUSH AF
	; ending )
	CALL CHKCHAR
	DB ')'

    EI
	; save position
	PUSH HL
	POP IX

	; syntax ok
	; wait for vsync if needed
	POP AF
	OR A
	JR Z, .L1
	HALT

.L1:
    POP AF ; value
    POP BC ; count
    POP HL ; offset
    CALL BIOS_FILVRM

.L3:
	PUSH IX
	POP HL
	RET 
; *******************************************************************************************************
 ENDIF

 IF (RAM_CMDS == 1)
; *******************************************************************************************************
; function to handle CALL FILRAM basic extension
; FILRAM ( INT start address, 
;		   INT count, 
;		   BYTE value,
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
	LD A, B
	OR A
	JR NZ, .L1 ; >=256 bytes to fill
	OR C
	JR Z, .EXIT ; 0 bytes to fill, skip
	LD A, C
	DEC A
	JR NZ, .L1 ; ; >1 byte to fill
	; one byte to fill
	LD (HL), D
	JR .EXIT
.L1:
	EXX
	; enable page 0
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	EXX
	CALL .FILLVALUE
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
.EXIT:
	PUSH IX
	POP HL
	RET

.FILLVALUE:
    LD (HL), D
    LD D, H
    LD E, L
    INC DE
    DEC BC
    LDIR
    RET
; *******************************************************************************************************
 ENDIF

 IF (GENCAL_CMD == 1)
; *******************************************************************************************************
; function to handle CALL GENCAL basic extension
; GENCAL ( INT fn_addr, = address of the function to call
;		   INT reg_list_ptr, = pointer to array holding register values (AF,BC,DE,HL,IX,IY)
; output values of reristers will also be stored at reg_list_ptr
GENCAL_VAR_SP:
    DW 0
GENCAL_VAR_SP2:
    DW 0
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
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
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
    LD (GENCAL_VAR_SP), SP
    LD SP, HL
    POP AF
    POP BC
    POP DE
    POP HL
    POP IX
    POP IY
    EXX
    LD (GENCAL_VAR_SP2), SP
    LD SP, (GENCAL_VAR_SP)
    EI
    POP DE ; get function to call
    PUSH HL
    CALL .EXXDECALL
    DI
    LD (GENCAL_VAR_SP), SP
    LD SP, (GENCAL_VAR_SP2)
    PUSH IY
    PUSH IX
    PUSH HL
    PUSH DE
    PUSH BC
    PUSH AF
    LD SP, (GENCAL_VAR_SP)
    EI
    POP HL
	RET 

.EXXDECALL:
    PUSH DE
    EXX
    RET
; *******************************************************************************************************
 ENDIF

 IF (VRAM_CMDS == 1)
; *******************************************************************************************************
; function to handle CALL MEMVRM basic extension
; copies from RAM to VRAM
; _MEMVRM ( INT source, 
;			INT destination, 
;			INT count, 
;			BYTE wait_vsync) >0 = true
; will put ram in page 0 also, page 1 is already there
; wait_vsync will issue HALT before copying
MEMVRM:
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
	; comma
	CALL CHKCHAR
	DB ','
	; get vsync wait
	LD IX, GETBYT
	CALL CALBAS
	PUSH AF
	; ending )
	CALL CHKCHAR
	DB ')'

    ; save position in BASIC text
	PUSH HL
	POP IX

	POP AF ; wait vsync
	OR A
	JR Z, .L1
    EI
	HALT
	DI
.L1:
	; pop LDIR parameters and store away for later
	POP BC ; count
	POP DE ; vram destination
	POP HL ; ram source
	EXX
 	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	EXX
	CALL .LDIRVM
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
	PUSH IX
	POP HL
	RET

.LDIRVM:
	EX DE, HL
	DI
	CALL SETWRT_LOCAL
	EI
	EX DE, HL
	LD A, B
	OR A
	JR Z, .L3
	PUSH BC
	LD C, #98
.L2:
	LD D, B
	LD B, 0
	CALL BBYTECOPY
	LD B, D
	DJNZ .L2
	POP BC
.L3:
	LD A, C
	OR A
	RET Z
	LD B, C
	LD C, #98
	JP BBYTECOPY
; *******************************************************************************************************
 ENDIF

 IF (VRAM_CMDS == 1)
; *******************************************************************************************************
; function to handle CALL VRMMEM basic extension
; copies from RAM to VRAM
; _VRMMEM ( INT source, 
;			INT destination, 
;			INT count
; will put ram in page 0 also, page 1 is already there
VRMMEM:
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

    ; save position in BASIC text
	PUSH HL
	POP IX

	POP BC ; count
	POP DE ; destination
	POP HL ; source
	EXX
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:	
	EI
	EXX
	CALL .LDIRMV
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
	PUSH IX
	POP HL
	RET

.LDIRMV:
	; set VRAM address *exactly* as in ROM, otherwise corruption
	LD	A, L
	DI
	OUT	(099H), A
	LD	A, H
	AND	03FH
	OUT	(099H), A
	EI
	;EX (SP), HL
	;EX (SP), HL
	;NOP
	;NOP
.L4:
    IN A, (#98)
	LD (DE), A
    INC DE
    DEC BC
    LD A, C
    OR B
    JR NZ, .L4
    RET
; *******************************************************************************************************
 ENDIF

; *******************************************************************************************************
; H.TIMI function
MBGE_HTIMI:
 EXPORT MBGE_HTIMI
	PUSH AF
	
 IF (SPRITE_CMDS == 1)
	CALL SPRATR_UPDATE
 ENDIF

 IF (SOUND_CMDS == 1)
	LD A, (SOUND_ENABLED)
	OR A
	JR Z, .EXIT

	; enable page 2
    LD A, 2
    CALL GET_PAGE_INFO
    PUSH BC
    PUSH DE
    LD A, (RAMAD2)
    LD H, 080H
    CALL LOCAL_ENASLT
	; enable page 0
    XOR A
    CALL GET_PAGE_INFO
    PUSH BC
    PUSH DE
    LD A, (RAMAD0)
    LD H, 0
    CALL LOCAL_ENASLT

	CALL PLY_AKG_PLAY

	; restore page 0
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
	; restore page 2
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
 ENDIF

.EXIT:
	POP AF
	JP ORIG.HTIMI
; *******************************************************************************************************

; *******************************************************************************************************
; interrupt handler when page 0 enabled
VBLANK:
	EXPORT VBLANK

    PUSH AF
	; is VDP originator ?
	IN	A, (099H)
	AND	A
	JP P, .EXIT

 IF (SOUND_CMDS == 1)
	LD A, (SOUND_ENABLED)
	OR A
	JR Z, .EXIT

    PUSH BC
    PUSH DE
    PUSH HL
    EX AF, AF'
    EXX
    PUSH AF
    PUSH BC
    PUSH DE
    PUSH HL
    PUSH IX
    PUSH IY

	CALL PLY_AKG_PLAY

    POP IY
    POP IX
    POP HL
    POP DE
    POP BC
    POP AF
    EX AF, AF'
    EXX
    POP HL
    POP DE
    POP BC
 ENDIF

.EXIT:
	POP AF
	EI
	RETI
; *******************************************************************************************************

 IF (SOUND_CMDS == 1)
; *******************************************************************************************************
; function to handle CALL SNDPLYINIT basic extension
; initializes sound player
; _SNDPLYINIT ( INT music_offset, 
;				INT sfx_offset, can be -1 if no SFX
; will put ram in page 0 also, page 1 is already there
; sets variables MUSIC_INIT_STATUS and SFX_INIT_STATUS
SNDPLYINIT:
	; opening (
	CALL CHKCHAR
	DB '('
	; get music address
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get sfx address
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; ending )
	CALL CHKCHAR
	DB ')'

    ; save position in BASIC text
	LD B, H
	LD C, L

	; pop LDIR parameters and store away for later
	POP DE ; sfx address
	POP HL ; music address
	PUSH BC ; basic text location
	EXX
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EXX

	PUSH DE
	XOR A
	; HL = music location
	CALL PLY_AKG_INIT
	LD A, 1
	LD (MUSIC_INIT_STATUS), A

	POP HL ; SFX
	; check if SFX address -1
	INC HL
	LD A, L
	OR H
	JR Z,.L1
	DEC HL
	CALL PLY_AKG_INITSOUNDEFFECTS
	LD A, 1
	LD (SFX_INIT_STATUS), A
.L1:
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP HL
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL SNDPLYON basic extension
; enables sound player
; _SNDPLYON
; sets SOUND_ENABLED variable to 1 if init call was done
; if not throws out of data error
SNDPLYON:
	LD A, (MUSIC_INIT_STATUS)
	OR A
	JR NZ, .L1
	; player not initialized, throw error
	LD E, 04 ; Out of DATA 
	JP THROW_ERROR
.L1:
	LD (SOUND_ENABLED), A
	; disable key click
	XOR A
	LD (CLIKSW), A
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL SNDPLYOFF basic extension
; disables sound player
; _SNDPLYOFF
; sets SOUND_ENABLED variable to 0
; calls AKG to stop music and SFX on all channels if initialized
SNDPLYOFF:
	LD A, (SOUND_ENABLED)
	OR A
	RET Z ; already stopped
	XOR A
	LD (SOUND_ENABLED), A
	PUSH HL
	CALL PLY_AKG_STOP
	LD A, (SFX_INIT_STATUS)
	OR A
	JR Z, .EXIT ; SFX not in use
	XOR A
	CALL PLY_AKG_STOPSOUNDEFFECTFROMCHANNEL
	LD A, 1
	CALL PLY_AKG_STOPSOUNDEFFECTFROMCHANNEL
	LD A, 2
	CALL PLY_AKG_STOPSOUNDEFFECTFROMCHANNEL
.EXIT:
	POP HL
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL SNDSFX basic extension
; plays a sound effect
; _SNDSFX ( BYTE sfx_number, >0
;			BYTE channel, = 0,1 or 2
;			BYTE inverted_volume = 0-16, 0 being full volume
; will put ram in page 0 also, page 1 is already there
; if sound off throws illegal function call
; if sfx not initialized, throws out of data
SNDSFX:
	; opening (
	CALL CHKCHAR
	DB '('
	; get sfx_number
	LD IX, GETBYT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get sfx address
	LD IX, GETBYT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get inverted volume
	LD IX, GETBYT
	CALL CALBAS
	PUSH DE
	; ending )
	CALL CHKCHAR
	DB ')'

	LD A, (SOUND_ENABLED)
	OR A
	JR NZ, .L1
	; sound disabled, throw illegal function call
	LD E, 5
	JP THROW_ERROR
.L1:
	LD A, (SFX_INIT_STATUS)
	OR A
	JR NZ, .L2
	; sfx data not initialized, throw out of data
	LD E, 4
	JP THROW_ERROR
.L2:
	; pop  parameters and store away for later
	POP DE ; inverted volume
	LD B, E
	POP DE ; channel
	LD C, E
	POP DE
	LD A, E
	EX AF, AF'
	PUSH HL ; basic text location
	EXX
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EXX
	EX AF, AF'
	CALL PLY_AKG_PLAYSOUNDEFFECT

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP HL
	RET
; *******************************************************************************************************
 ENDIF

 IF (BLIT_CMDS == 1)
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
;		 INT background_height (in characters))
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
	; comma
	CALL CHKCHAR
	DB ','
	; get background height
	LD IX, FRMQNT
	CALL CALBAS
	LD A, E
	LD (BLIT_TMP+3), A
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
 ENDIF

 IF (TILE_CMDS == 1)
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
.CALL1:
			CALL 0
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
	JP BBYTECOPY	
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
	JP BBYTECOPY	
.SETDESTROW:
	LD HL, (TILETMP1)
	DI
	CALL SETWRT_LOCAL
	EI
	RET
; *******************************************************************************************************
 ENDIF
 ENDIF

 IF (BOX_CMDS == 1)
; *******************************************************************************************************
; generic function to implement rectangle data copy
; should be modified to call appropriate function for memory or vram
; input IX=pointer to following structure
; +00 source data pointer
; +02 num bytes in a row
; +04 number of rows
; +06 source add-to value till next row
; +08 destination address
; +10 destination add-to value till next row
; modifies AF, BC, DE, HL
RECTANGLE_COPY:
	LD L, (IX+0)
	LD H, (IX+1) ; source address
	LD E, (IX+8)
	LD D, (IX+9) ; destination
	LD B, (IX+4) ; row number
.L1:
	PUSH BC
		PUSH HL
			PUSH DE
				LD C, (IX+2)
				LD B, (IX+3) ; num bytes in a row
.CALL1:
				CALL 0 ; set destination address from DE
.CALL2:
				CALL 0 ; copy data fn
			POP HL
			LD C, (IX+10)
			LD B, (IX+11) ; destination add-to
			ADD HL, BC
			EX DE, HL
		POP HL
		LD C, (IX+6)
		LD B, (IX+7) ; src add-to
		ADD HL, BC
	POP BC
	DJNZ .L1
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL BOXMEMCPY basic extension
; copies data with window like boundaries to ram
; BOXMEMCPY ( INT request_data_ptr )
; request_data_ptr described in RECTANGLE_COPY
; will put ram in page 0 also, page 1 is already there
BOXMEMCPY:
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
	LD HL, 0
	LD (RECTANGLE_COPY.CALL1), HL ; NOP NOP
	LD (RECTANGLE_COPY.CALL1+2), HL ; NOP NOP
	LD HL, #B0ED ; LDIR
	LD (RECTANGLE_COPY.CALL1+4), HL
	CALL RECTANGLE_COPY

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP HL
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL BOXMEMVRM basic extension
; copies data with window like boundaries to ram
; BOXMEMVRM ( INT request_data_ptr )
; request_data_ptr described in RECTANGLE_COPY
; will put ram in page 0 also, page 1 is already there
BOXMEMVRM:
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
	LD HL, .SETDEST
	LD (RECTANGLE_COPY.CALL1+1), HL
	LD HL, .COPYDATA
	LD (RECTANGLE_COPY.CALL2+1), HL
	LD A, #CD ; CALL
	LD (RECTANGLE_COPY.CALL1), A
	LD (RECTANGLE_COPY.CALL2), A
	CALL RECTANGLE_COPY

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP HL
	RET
.SETDEST:
	EX DE, HL
	DI
	CALL SETWRT_LOCAL
	EI
	EX DE, HL
	RET	
.COPYDATA:
	LD B, C
	LD C, #98
	JP BBYTECOPY
; *******************************************************************************************************
 ENDIF

EXT_END:
