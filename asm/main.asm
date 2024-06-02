 ORG 04000h

; control variables to include/exclude parts of code
SOUND_CMDS	 	EQU 1
RAM_CMDS 		EQU 1
VRAM_CMDS 		EQU 1
BLIT_CMDS		EQU 1
SPRITE_CMDS 	EQU 1
GENCAL_CMD		EQU 1
TILE_CMDS		EQU 1
BOX_CMDS		   EQU 1
ANIM_CMDS		EQU 1
COLL_CMD       EQU 1
DECOMP_CMDS    EQU 1

; what to compile, provided in sjasmplus command line
;BASIC_EXTENSION   EQU 1
;DEFUSR_EXTENSION  EQU 0

CHPUT   EQU    #A2
CALBAS  EQU		#159
ERRHAND EQU    #406F
FRMEVL  EQU    #4C64
FRESTR  EQU		#67D0
; FRMQNT = formula quantificator
; input HL=pointer to current program expression
; output HL=next address
; output DE=integer datum
FRMQNT	EQU		#542F
; GETBYT = get byte parameter
; input HL=pointer to current program expression
; output HL=next address
; output A=E=byte read
GETBYT		EQU	#521C
CHRGTR  	   EQU   #4666
PTRGET		EQU 	#5EA4
SUBFLG		EQU	#F6A5
SYNCHR		EQU	#558C
VALTYP  	   EQU   #F663
DAC         EQU   #F7F6
USR     	   EQU   #F7F8
PROCNM		EQU	#FD89
BIOS_FILVRM EQU   #0056
CLIKSW		EQU	#F3DB

RAMAD0	   EQU	#F341	; Main-RAM Slot (00000h~03FFFh)
RAMAD1	   EQU	#F342	; Main-RAM Slot (04000h~07FFFh)
RAMAD2	   EQU	#F343	; Main-RAM Slot (08000h~0BFFFh)
RAMAD3	   EQU	#F344	; Main-RAM Slot (0C000h~0FFFFh)
EXPTBL	   EQU   #FCC1
SCRMOD	   EQU   #FCAF ; current screen mode
REG1SAV     EQU   #F3E0 ; VDP(1)
JIFFY	      EQU   #FC9E 
GRPPAT	   EQU   #F3CF ; SCREEN 2 sprite generator table address 
GRPCGP		EQU	#F3CB ; SCREEN 2 pattern generator table address
GRPATR      EQU   #F3CD ; SCREEN 2 sprite attribute table address 
T32PAT	   EQU   #F3C5 ; SCREEN 1 sprite generator table address 
T32CGP      EQU   #F3C1 ; SCREEN 1 pattern ganarator table address
T32ATR      EQU   #F3C3 ; SCREEN 1 sprite attribute table address 

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
 DB #00, #91

; this location #4014 contains a jump to entry point for DEFUSR approach
; if excluded it contains 3xRET so that sound player can be at aspecific spot
 IF (DEFUSR_EXTENSION == 1)
   JP DEFUSR_ENTRY
 ELSE
   .3 RET
 ENDIF

; binary included AKG player compiled at #4017
 IF (SOUND_CMDS == 1)
	INCBIN "bin/AKG.bin"
	INCLUDE "symbol/AKG.sym"
 ENDIF

 INCLUDE "VBLANK.asm"

 IF (SPRITE_CMDS == 1)
 INCLUDE "SPRITES.asm"
 ENDIF

 IF (ANIM_CMDS == 1)
 INCLUDE "ANIMATION.asm"
 INCLUDE "SGAM.asm"
 ENDIF

 IF (RAM_CMDS == 1)
 INCLUDE "MEMORY.asm"
 ENDIF

 IF (SOUND_CMDS == 1)
 INCLUDE "SOUND.asm"
 ENDIF

 IF (VRAM_CMDS == 1)
 INCLUDE "VRAM.asm"
 ENDIF 

 IF (GENCAL_CMD == 1)
 INCLUDE "GENCAL.asm"
 ENDIF 

 IF (BOX_CMDS == 1)
 INCLUDE "BOX.asm"
 ENDIF 

 IF (BLIT_CMDS == 1)
 INCLUDE "BLIT.asm"
 ENDIF 

 IF (TILE_CMDS == 1)
 INCLUDE "TILE.asm"
 ENDIF 

 IF (COLL_CMD == 1)
 INCLUDE "COLLISION.asm"
 ENDIF

 IF (DECOMP_CMDS == 1)
 INCLUDE "decomp.asm"
 ENDIF

 IF (DEFUSR_EXTENSION == 1)
 INCLUDE "DEFUSR.asm"
 ENDIF

; temp variables for BLIT, TILE functions
 IF (BLIT_CMDS + TILE_CMDS + BOX_CMDS + SPRITE_CMDS + ANIM_CMDS + COLL_CMD > 0)
BLIT_TMP:
TILETMP1:
BLIT_TMP1:
 DW 0
TILETMP2:
BLIT_TMP2:
 DW 0
BLIT_STRUCT:
 DS 17
 ENDIF

 IF (VRAM_CMDS + TILE_CMDS + BOX_CMDS + SPRITE_CMDS + ANIM_CMDS > 0)
VRAM_UPDATE_IN_PROGRESS:
 DB 0
 ENDIF

; List of pointers to available instructions (as ASCIIZ) and execute address (as word)
; per starting letter, if no commands with this letter, NULL value
CMDS:
	DW CMDS_A ; always present due to ARTINFO
 IF (BLIT_CMDS + BOX_CMDS > 0) && (BASIC_EXTENSION == 1)
    DW CMDS_B ; B
 ELSE
	DW 0
 ENDIF
 IF (COLL_CMD == 1) && (BASIC_EXTENSION == 1)
	DW CMDS_C ;
 ELSE
    DW 0 ; C
 ENDIF
    DW 0 ; D
    DW 0 ; E
 IF (VRAM_CMDS + RAM_CMDS > 0) && (BASIC_EXTENSION == 1)
    DW CMDS_F; F
 ELSE
	DW 0
 ENDIF
 IF (GENCAL_CMD > 0) && (BASIC_EXTENSION == 1)
    DW CMDS_G; G
 ELSE
	DW 0
 ENDIF
    DW 0 ; H
    DW 0 ; I
    DW 0 ; J
    DW 0 ; K
    DW 0 ; L
 IF (VRAM_CMDS + RAM_CMDS + ANIM_CMDS > 0) && (BASIC_EXTENSION == 1)
    DW CMDS_M ; M
 ELSE
	DW 0
 ENDIF
    DW 0 ; N
    DW 0 ; O
    DW 0 ; P
    DW 0 ; Q
    DW 0 ; R
 IF (SOUND_CMDS + SPRITE_CMDS > 0) && (BASIC_EXTENSION == 1)
    DW CMDS_S ; S
 ELSE
	DW 0
 ENDIF
 IF (TILE_CMDS > 0) && (BASIC_EXTENSION == 1)
    DW CMDS_T ; T
 ELSE
	DW 0
 ENDIF
 IF (DECOMP_CMDS > 0) && (BASIC_EXTENSION == 1)
    DW CMDS_U ; U
 ELSE
	DW 0
 ENDIF
 IF ((VRAM_CMDS > 0) || (VRAM_CMDS + DECOMP_CMDS > 1)) && (BASIC_EXTENSION == 1)
    DW CMDS_V ; V
 ELSE
	DW 0
 ENDIF
    DW 0 ; W
    DW 0 ; X
    DW 0 ; Y
    DW 0 ; Z

 IF (BASIC_EXTENSION == 1)
 IF (VRAM_CMDS + RAM_CMDS + ANIM_CMDS > 0)
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
   DB "MAXAUTOSGAMS",0
   DW MAXAUTOSGAMS
 ENDIF
 ENDIF

 IF (VRAM_CMDS + RAM_CMDS > 0)
CMDS_F:
 IF (VRAM_CMDS == 1)
    DB "FILVRM", 0
    DW FILVRM
 ENDIF
 IF (RAM_CMDS == 1)
    DB "FILRAM", 0
    DW FILRAM
 ENDIF
 ENDIF

 IF (GENCAL_CMD > 0)
CMDS_G:
 IF (GENCAL_CMD == 1)
    DB "GENCAL", 0
    DW GENCAL
 ENDIF
 ENDIF

 IF (VRAM_CMDS > 0)
CMDS_V:
 IF (VRAM_CMDS == 1)
	DB "VRMMEM", 0
	DW VRMMEM
 ENDIF
 IF (DECOMP_CMDS == 1)
	DB "VUNPACK", 0
	DW VUNPACK
 ENDIF
 ENDIF

 IF (SOUND_CMDS + SPRITE_CMDS + ANIM_CMDS > 0)
CMDS_S:
 IF (SPRITE_CMDS + ANIM_CMDS > 0)
    DB "SGAM",0
    DW SGAM
 ENDIF
 IF (SPRITE_CMDS == 1)
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
 ENDIF

 IF (BLIT_CMDS + BOX_CMDS > 0)
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
 ENDIF

 IF (TILE_CMDS > 0)
CMDS_T:
 IF (TILE_CMDS == 1)
	DB "TILERAM", 0
	DW TILERAM
	DB "TILEVRM", 0
	DW TILEVRM
 ENDIF
 ENDIF

 IF (COLL_CMD > 0)
CMDS_C:
 IF (COLL_CMD == 1)
    DB "COLL", 0
    DW COLL
 ENDIF
 ENDIF

 IF (DECOMP_CMDS > 0)
CMDS_U:
 IF (DECOMP_CMDS == 1)
    DB "UNPACK", 0
    DW UNPACK
 ENDIF
 ENDIF

CMDS_A:
 IF (ANIM_CMDS == 1)
   DB "ANIMSTEP",0
   DW ANIMSTEP
	DB "ANIMSTART",0
	DW ANIMSTART
	DB "ANIMSTOP",0
	DW ANIMSTOP
	DB "ANIMITEMPAT",0
	DW ANIMITEMPAT
	DB "ANIMITEMPTR",0
	DW ANIMITEMPTR_CMD
	DB "ANIMDEF",0
	DW ANIMDEF
	DB "ANIMSPRITE",0
	DW ANIMSPRITE
   DB "ANIMCHAR",0
   DW ANIMCHAR
   DB "AUTOSGAMDEF",0
   DW AUTOSGAMDEF
   DB "AUTOSGAMSTART",0
   DW AUTOSGAMSTART
   DB "AUTOSGAMSTOP",0
   DW AUTOSGAMSTOP
 ENDIF
   DB "ARTINFO",0
   DW ARTINFO
	DB 0
 ELSE // if not BASIC extension
CMDS_A:
   DB "ARTINFO",0
   DW ARTINFO
	DB 0
 ENDIF

 IF (VRAM_CMDS + TILE_CMDS + SPRITE_CMDS > 0)
; ****************************************************************************************************
; function sets VRAM address
; input HL=address
; modifies AF
SETWRT_LOCAL:
	LD	A,L
	OUT (099H),A
	LD	A,H
	AND 03FH
	OR	040H
	OUT (099H),A
	RET
; ****************************************************************************************************
 ENDIF

 IF (VRAM_CMDS + TILE_CMDS > 0)
; ****************************************************************************************************
; function copies data from RAM to VRAM
; input HL=address in RAM
; input B=count
; modifies AF, BC, HL
BBYTECOPY:
	LD C,#98
BBYTECOPY_NO_C:
	OUTI
	JP	NZ, BBYTECOPY_NO_C
	RET
; ****************************************************************************************************
 ENDIF

; ****************************************************************************************************
; function multiplies HL by 32
HLx32:
	ADD HL,HL
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
   DI
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
NOACTION_DEFUSR: ; just a safe RET for use when DEFUSR table has an empty entry
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
   DI
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
   EI
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
.LOOP:	
   LD	A,(DE)
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
	DEC HL
	LD	IX,CHRGTR
	CALL CALBAS
   EI
   RET
 
 
TYPE_MISMATCH:
    LD E, 13 ; Type mismatch 
    JR THROW_ERROR
SUBSCRIPT_OUT_OF_RANGE:
    LD E,9 ; subscript out of range
	JR THROW_ERROR
OVERFLOW:
	LD E,6
	JR THROW_ERROR
ILLEGAL_FUNCTION:
    LD E, 5 ; illegal function call
    JR THROW_ERROR
SYNTAX_ERROR:
    LD E, 2 ; Syntax error 
THROW_ERROR:
	LD	IX,ERRHAND	; Call the Basic error handler
	JP	CALBAS
 
;---------------------------

 IF (BASIC_EXTENSION == 1)
; *******************************************************************************************************
; helper function to get pointer to BASIC array data
; input A=data type (2=INT,4=SINGLE,8=DOUBLE)
; input B=dimensions (1 or 2)
; input D=minimal first dimension
; input E=minimal second dimension, if applicable
; returns BC=pointer to first data element
; throws BASIC error if invalid type
GET_BASIC_ARRAY_DATA_POINTER:
	PUSH DE
	PUSH BC
	PUSH AF
   LD A,1
   LD (SUBFLG),A ; search for arrays only
	LD IX, PTRGET
	CALL CALBAS
   XOR A
   LD (SUBFLG),A ; if not reset will cause syntax errors
	LD A,(VALTYP)
	POP DE ; required type
	CP D
	JP NZ,TYPE_MISMATCH
	LD A,(BC)
	INC BC
	POP DE ; required number of dimensions
	CP D
	JP NZ,TYPE_MISMATCH
	POP DE ; required minimal array dimensions
	DEC A
	JR Z,.ONE_DIMENSION
	; 2-dimension array
	LD A,(BC)
	.2 INC BC
	CP E
	JP C,SUBSCRIPT_OUT_OF_RANGE
.ONE_DIMENSION:
	LD A,(BC)
	.2 INC BC
	CP D
	JP C,SUBSCRIPT_OUT_OF_RANGE
	RET	
; ******************************************************************************************************* 
 ENDIF

; *******************************************************************************************************
; function to handle CALL ARTINFO basic extension
; returns info about the extension
; _ARTINFO ( INT variable version,
;			    INT variable build_flags,
;			    INT variable free_memory_ptr )
; this function is always available and can be used to test if the extension is active
ARTINFO:
	; opening (
	CALL CHKCHAR
	DB '('
	; get address of version variable
	LD IX, PTRGET
	CALL CALBAS
	LD A,(VERSION)
   LD (DE),A
   INC DE
   LD A,(VERSION+1)
   LD (DE),A
	; comma
	CALL CHKCHAR
	DB ','
	; get address of build flags variable
	LD IX, PTRGET
	CALL CALBAS
   PUSH HL
   LD HL,SOUND_CMDS+2*RAM_CMDS+4*VRAM_CMDS+8*BLIT_CMDS+16*SPRITE_CMDS+32*GENCAL_CMD+64*TILE_CMDS+128*BOX_CMDS+256*ANIM_CMDS+512*COLL_CMD+1024*BASIC_EXTENSION+2048*DEFUSR_EXTENSION
   EX DE,HL
   LD (HL),E
   INC HL
   LD (HL),D
   POP HL
	; comma
	CALL CHKCHAR
	DB ','
	; get address of free memory variable
	LD IX, PTRGET
	CALL CALBAS
	LD A,(FREEMEMPTR)
   LD (DE),A
   INC DE
   LD A,(FREEMEMPTR+1)
   LD (DE),A
	; ending )
	CALL CHKCHAR
	DB ')'
	RET
; *******************************************************************************************************

EXT_END:
