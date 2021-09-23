 ORG 04000h

; INCLUDE "system_variables.inc"
; INCLUDE "system_hooks.inc"
; INCLUDE "bios_calls.inc"

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
GETBYT	EQU		#521C
CHRGTR  EQU     #4666
SYNCHR	EQU		#558C
VALTYP  EQU     #F663
USR     EQU     #F7F8
PROCNM	EQU		#FD89
BIOS_FILVRM  EQU     #56
CLIKSW	EQU		#F3DB
ATRBAS	EQU		#F928

RAMAD0	EQU	0F341h	; Main-RAM Slot (00000h~03FFFh)
RAMAD1	EQU	0F342h	; Main-RAM Slot (04000h~07FFFh)
RAMAD2	EQU	0F343h	; Main-RAM Slot (08000h~0BFFFh)
RAMAD3	EQU	0F344h	; Main-RAM Slot (0C000h~0FFFFh)
EXPTBL	EQU #FCC1
SCRMOD	EQU #FCAF ; current screen mode

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

; this location #400A stores last location used by basic extension
; free memory after that point
 DW EXT_END 

; binary included AKG player compiled at #4012
 INCBIN "bin/AKG.bin"
 INCLUDE "symbol/AKG.sym"

ORIG.HTIMI:
	DB 0, 0, 0, 0, 0
 EXPORT ORIG.HTIMI

MUSIC_INIT_STATUS:
 DB 0
SFX_INIT_STATUS:
 DB 0
SOUND_ENABLED:
 DB 0

SPRATR_INIT_STATUS:
 DB 0
SPRATR_UPDATE_FLAG:
 DW 0
SPRATR_DATA:
 DW 0

; to temporarily store stack pointer
TMPSP:
 DW 0
; to support sprite flicker
FLICKER:
 DB 0

; temp variables for BLIT functions
BLIT_TMP1:
 DW 0
BLIT_TMP2:
 DW 0

; List of pointers to available instructions (as ASCIIZ) and execute address (as word)
; per starting letter, if no commands with this letter, NULL value
CMDS:
    DW 0 ; A
    DW CMDS_B ; B
    DW 0 ; C
    DW 0 ; D
    DW 0 ; E
    DW CMDS_F; F
    DW CMDS_G; G
    DW 0 ; H
    DW 0 ; I
    DW 0 ; J
    DW 0 ; K
    DW CMDS_L ; L
    DW CMDS_M ; M
    DW 0 ; N
    DW 0 ; O
    DW 0 ; P
    DW 0 ; Q
    DW 0 ; R
    DW CMDS_S ; S
    DW 0 ; T
    DW CMDS_U ; U
    DW CMDS_V ; V
    DW 0 ; W
    DW 0 ; X
    DW 0 ; Y
    DW 0 ; Z

CMDS_U:
	DEFB	"UPRINT",0      ; Print upper case string
	DEFW	UPRINT
    DB 0
CMDS_L:
	DEFB	"LPRINT",0      ; Print lower case string
	DEFW	LPRINT
    DB 0
CMDS_M:
    DB "MEMVRM", 0
    DW MEMVRM
	DB "MEMCPY", 0
	DW MEMCPY
	DB 0
CMDS_F:
    DB "FILVRM", 0
    DW FILVRM
    DB "FILRAM", 0
    DW FILRAM
    DB 0
CMDS_G:
    DB "GENCAL", 0
    DW GENCAL
	DB	0
CMDS_V:
	DB "VRMMEM", 0
	DW VRMMEM
	DB 0
CMDS_S:
	DB "SPRSET", 0
	DW SPRSET
	DB "SPRGRPMOV", 0
	DW SPRGRPMOV
	DB "SNDSFX", 0
	DW SNDSFX
	DB "SNDPLYON", 0
	DW SNDPLYON
	DB "SNDPLYOFF", 0
	DW SNDPLYOFF
	DB "SNDPLYINI", 0
	DW SNDPLYINIT
	DB "SPRATRINI", 0
	DW SPRATRINI
	DB 0
CMDS_B:
	DB "BLIT", 0
	DW BLIT
	DB 0

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

; *******************************************************************************************************
; function updates sprite attribute table in VRAM based on buffer of the form with rotating for flicker
; struct {
; DW y
; DW x
; DW pattern (0-31)
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
	LD C, #98 ; register for vpd data output
	; set VDP address
	LD HL, (ATRBAS)
	LD A, (FLICKER)
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
	LD A, L
	ADD A, A
	ADD A, A ; needs to go at 4x
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
	POP	HL
.CMDNOTRECOGNIZED:
    SCF
	RET
 
.CALLDE:
	PUSH	DE
	RET
 
;---------------------------
 
;---------------------------
UPRINT:
	CALL	EVALTXTPARAM	; Evaluate text parameter
	PUSH	HL
        CALL    GETSTRPNT
.LOOP
        LD      A,(HL)
        CALL    .UCASE
        CALL    CHPUT  ;Print
        INC     HL
        DJNZ    .LOOP
 
	POP	HL
	OR      A
	RET
 
.UCASE:
        CP      "a"
        RET     C
        CP      "z"+1
        RET     NC
        AND     %11011111
        RET
;---------------------------
LPRINT:
	CALL	EVALTXTPARAM	; Evaluate text parameter
	PUSH	HL
        CALL    GETSTRPNT
.LOOP
        LD      A,(HL)
        CALL    .LCASE
        CALL    CHPUT  ;Print
        INC     HL
        DJNZ    .LOOP
 
	POP	HL
	OR      A
	RET
 
.LCASE:
        CP      "A"
        RET     C
        CP      "Z"+1
        RET     NC
        OR      %00100000
        RET
;---------------------------
 
GETSTRPNT:
; OUT:
; HL = String Address
; B  = Lenght
 
        LD      HL,(USR)
        LD      B,(HL)
        INC     HL
        LD      E,(HL)
        INC     HL
        LD      D,(HL)
        EX      DE,HL
        RET
 
EVALTXTPARAM:
	CALL	CHKCHAR
	DEFB	"("             ; Check for (
	LD	IX,FRMEVL
	CALL	CALBAS		; Evaluate expression
        LD      A,(VALTYP)
        CP      3               ; Text type?
        JP      NZ,TYPE_MISMATCH
        PUSH	HL
        LD	IX,FRESTR         ; Free the temporary string
        CALL	CALBAS
        POP	HL
	CALL	CHKCHAR
	DEFB	")"             ; Check for )
        RET
 
 
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
 
SYNTAX_ERROR:
    LD E, 2 ; Syntax error 
THROW_ERROR:
	LD	IX,ERRHAND	; Call the Basic error handler
	JP	CALBAS
 
;---------------------------
 
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
    LD (GENCAL_VAR_SP), SP
    DI
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

; *******************************************************************************************************
; function to handle CALL MEMVRM basic extension
; copies from RAM to VRAM
; _MEMVRM ( INT source, 
;			INT destination, 
;			INT count, 
;			BYTE wait_vsync) >0 = treu
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
	CALL SETWRT_LOCAL
	EX DE, HL
	LD A, B
	OR A
	JR Z, .L3
	PUSH BC
	LD C, #98
.L2:
	LD D, B
	LD B, 0
	CALL .BBYTECOPY
	LD B, D
	DJNZ .L2
	POP BC
.L3:
	LD A, C
	OR A
	RET Z
	LD B, C
	LD C, #98
.BBYTECOPY:
	OUTI
	JP	NZ, .BBYTECOPY
	RET
; *******************************************************************************************************

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
	CALL SETWRT_LOCAL
.L4:
    IN A, (#98)
	LD (DE), A
    INC DE
    DEC BC
    LD A, C
    OR B
    JP NZ, .L4
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; H.TIMI function
MBGE_HTIMI:
 EXPORT MBGE_HTIMI
	PUSH AF
	
	CALL SPRATR_UPDATE

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
.EXIT:
	POP AF
	EI
	RETI
; *******************************************************************************************************

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

; *******************************************************************************************************
; function to handle CALL SPRATRINI basic extension
; initializes sprites handler
; _SPRATRINI ( INT sprites_attributes_data, 
;			   INT update_variable_location )
; expects both locations to be in range #8000+ or throws an error
; since these should be BASIC variables
; sets variables SPRATR_INIT_STATUS, SPRATR_UPDATE_FLAG, SPRATR_DATA
SPRATRINI:
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
	; get update variable location SU%
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; ending )
	CALL CHKCHAR
	DB ')'

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
; function to handle CALL SPRSET basic extension
; sets position, and optionally pattern and color of sprite
; _SPRSET ( BYTE sprite_num , valid 0-31
;			INT x, 
;			INT y, 
;			INT pattern, valid 0-31, otherwise ignored
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
	.3 ADD A, A
	PUSH DE
	LD D, 0
	LD E, A
	LD HL, (SPRATR_DATA)
	ADD HL, DE
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
	; check if 0<=pattern<32
	LD A, D
	OR A
	JR NZ, .L3
	LD A, L
	CP 32
	JR NC, .L3
	; set pattern
	LD (HL), E
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
	.3 ADD A, A
	PUSH DE
	LD D, 0
	LD E, A
	LD IX, (SPRATR_DATA)
	ADD IX, DE
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

	LD B, (HL)
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
; +0  DW horizontal shift count 0-7
; +2  DW vertical shift count 0-7
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

; *******************************************************************************************************
; function to handle CALL BLIT basic extension
; rotates 1-bit character drawing horizontally with mask and character data and
; fuses with background data
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


EXT_END:
