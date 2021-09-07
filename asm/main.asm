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

RAMAD0	EQU	0F341h	; Main-RAM Slot (00000h~03FFFh)
RAMAD1	EQU	0F342h	; Main-RAM Slot (04000h~07FFFh)
RAMAD2	EQU	0F343h	; Main-RAM Slot (08000h~0BFFFh)
RAMAD3	EQU	0F344h	; Main-RAM Slot (0C000h~0FFFFh)
EXPTBL	EQU #FCC1

 ; simulate cartridge with BASIC extension
 DW 04241H, 0, CALLHAND, 0, 0, 0, 0, 0

; this location #400A stores last location used by basic extension
; free memory after that point
 DW EXT_END 

; List of pointers to available instructions (as ASCIIZ) and execute address (as word)
; per starting letter, if no commands with this letter, NULL value
CMDS:
    DW 0 ; A
    DW 0 ; B
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
    DW 0 ; S
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
; modifies AF
RESTORE_PAGE_INFO:
    LD A, D
    OR A
    JR Z, RESTORE_PAGE_INFO_L1
    LD A, C
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
        LD      E,13
        DB      1
 
SYNTAX_ERROR:
        LD      E,2
	LD	IX,ERRHAND	; Call the Basic error handler
	JP	CALBAS
 
;---------------------------
 
; *******************************************************************************************************
; function to handle CALL MEMCPY basic extension
; _MEMCPY ( INT source, 
;			INT destination, 
;			INT count, 
;			BYTE enable_ram, >0 = true
;			BYTE wait_vsync) >0 = treu
; enable_ram will put ram in page 0 also, page 1 is already there
; wait_vsync will issue HALT before copying
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
	; comma
	CALL CHKCHAR
	DB ','
	; get ROM/RAM
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
	; enable RAM in page 0 if needed
	POP AF
	OR A
	; pop LDIR parameters and store away for later
	POP BC
	POP DE
	POP HL
	JR Z, .L2
	EXX
    XOR A
    CALL GET_PAGE_INFO
    PUSH BC
    PUSH DE
    LD A, (RAMAD0)
    LD H, 0
	DI
    CALL LOCAL_ENASLT
	EXX
	LDIR
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
	JR .L3

.L2:
	LDIR

.L3:
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
;	   	   BYTE enable_ram, >0 = true
;		   BYTE wait_vsync) >0 = true
; enable_ram will put ram in page 0 also, page 1 is already there
; wait_vsync will issue HALT before copying
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
	; comma
	CALL CHKCHAR
	DB ','
	; get ROM/RAM
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
	; enable RAM in page 0 if needed
	POP AF
	OR A
	; pop LDIR parameters and store away for later
	POP DE ; actually AF
	POP BC ; count
	POP HL ; start address
	JR Z, .L2
	EXX
    XOR A
    CALL GET_PAGE_INFO
    PUSH BC
    PUSH DE
    LD A, (RAMAD0)
    LD H, 0
	DI
    CALL LOCAL_ENASLT
	EXX
	CALL .FILLVALUE
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
	JR .L3

.L2:
	CALL .FILLVALUE

.L3:
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
;			BYTE enable_ram, >0 = true
;			BYTE wait_vsync) >0 = treu
; enable_ram will put ram in page 0 also, page 1 is already there
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
	; get ROM/RAM
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

    ; save position in BASIC text
	PUSH HL
	POP IX

	; syntax ok
	; wait for vsync if needed
	POP AF
	OR A
	JR Z, .L1
    EI
	HALT
    DI ; since interrupt can modify vram address

.L1:
	; enable RAM in page 0 if needed
	POP AF
	OR A
	; pop LDIR parameters and store away for later
	POP BC
	POP DE
	POP HL
	JR Z, .L2
	EXX
    XOR A
    CALL GET_PAGE_INFO
    PUSH BC
    PUSH DE
    LD A, (RAMAD0)
    LD H, 0
    CALL LOCAL_ENASLT
	EXX
	CALL .LDIRVM
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
	JR .L3

.L2:
	CALL .LDIRVM

.L3:
	PUSH IX
	POP HL
	RET

.LDIRVM:
	LD	A, E
	OUT	(099H), A
	LD	A, D
	AND	03FH
	OR	040H
	OUT	(099H), A
    
.L4:
    LD A, (HL)
    OUT (#98), A
    INC HL
    DEC BC
    LD A, C
    OR B
    JP NZ, .L4
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL VRMMEM basic extension
; copies from RAM to VRAM
; _VRMMEM ( INT source, 
;			INT destination, 
;			INT count, 
;			BYTE enable_ram, >0 = true
;			BYTE wait_vsync) >0 = true
; enable_ram will put ram in page 0 also, page 1 is already there
; wait_vsync will issue HALT before copying
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
	; comma
	CALL CHKCHAR
	DB ','
	; get ROM/RAM
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

    ; save position in BASIC text
	PUSH HL
	POP IX

	; syntax ok
	; wait for vsync if needed
	POP AF
	OR A
	JR Z, .L1
    EI
	HALT
    DI ; since interrupt can modify vram address

.L1:
	; enable RAM in page 0 if needed
	POP AF
	OR A
	; pop LDIR parameters and store away for later
	POP BC
	POP DE
	POP HL
	JR Z, .L2
	EXX
    XOR A
    CALL GET_PAGE_INFO
    PUSH BC
    PUSH DE
    LD A, (RAMAD0)
    LD H, 0
    CALL LOCAL_ENASLT
	EXX
	CALL .LDIRMV
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
	JR .L3

.L2:
	CALL .LDIRMV

.L3:
	PUSH IX
	POP HL
	RET

.LDIRMV:
	LD	A, L
	OUT	(099H), A
	LD	A, H
	AND	03FH
	OR	040H
	OUT	(099H), A
    
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


EXT_END:
