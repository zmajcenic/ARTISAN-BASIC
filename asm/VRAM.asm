 IF (BASIC_EXTENSION == 1)
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
	LD A,1
	LD (VRAM_UPDATE_IN_PROGRESS),A
    POP AF ; value
    POP BC ; count
    POP HL ; offset
    CALL BIOS_FILVRM
	XOR A
	LD (VRAM_UPDATE_IN_PROGRESS),A

.L3:
	PUSH IX
	POP HL
	RET 
; *******************************************************************************************************
 ENDIF

 IF (DEFUSR_EXTENSION == 1)
; *******************************************************************************************************
; same as FILVRM but for DEFUSR approach
; input IX=pointer to input array, real data from +2
; +2 = offset
; +4 = count
; +6 = value
; +8 = halt flag
FILVRM_DEFUSR:
	LD A,(IX+8)
	OR A
	JR Z,.L0
	HALT
.L0:
	LD A,1
	LD (VRAM_UPDATE_IN_PROGRESS),A
	LD L,(IX+2)
	LD H,(IX+3)
	LD C,(IX+4)
	LD B,(IX+5)
	LD A,(IX+6)
	CALL BIOS_FILVRM
	XOR A
	LD (VRAM_UPDATE_IN_PROGRESS),A
	RET
; *******************************************************************************************************
 ENDIF

 IF (BASIC_EXTENSION == 1)
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
	HALT
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
	CALL VRAM_LDIRVM
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
	PUSH IX
	POP HL
	RET
; *******************************************************************************************************
 ENDIF

 IF (DEFUSR_EXTENSION == 1)
; *******************************************************************************************************
; same as MEMVRM but for DEFUSR approach
; input IX=pointer to input array, real data from +2
; +2 = source address
; +4 = destination address
; +6 = lenght
; +8 = vsync wait flag
MEMVRM_DEFUSR:
	LD A,(IX+8)
	OR A
	JR Z,.L0
	HALT
.L0:
	; enable page 0
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	LD L,(IX+2)
	LD H,(IX+3)
	LD E,(IX+4)
	LD D,(IX+5)
	LD C,(IX+6)
	LD B,(IX+7)
	CALL VRAM_LDIRVM
    POP DE
    POP BC
    JP RESTORE_PAGE_INFO
; *******************************************************************************************************
 ENDIF

; *******************************************************************************************************
; common code to copy from memory to VRAM
; input HL=RAM source
; input DE=VRAM destination
; BC=count
VRAM_LDIRVM:
	LD A,1
	LD (VRAM_UPDATE_IN_PROGRESS),A
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
	CALL BBYTECOPY_NO_C
	LD B, D
	DJNZ .L2
	POP BC
.L3:
	LD A, C
	OR A
	JR Z,.L4
	LD B, C
	CALL BBYTECOPY
.L4:
	XOR A
	LD (VRAM_UPDATE_IN_PROGRESS),A
	RET
; *******************************************************************************************************

 IF (BASIC_EXTENSION == 1)
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
	CALL VRAM_LDIRMV
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
	PUSH IX
	POP HL
	RET
; *******************************************************************************************************
 ENDIF

 IF (DEFUSR_EXTENSION == 1)
; *******************************************************************************************************
; same as VRMMEM but for DEFUSR approach
; input IX=pointer to input array, real data from +2
; +2 = source address
; +4 = destination address
; +6 = count
VRMMEM_DEFUSR:
	; enable page 0
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	LD L,(IX+2)
	LD H,(IX+3)
	LD E,(IX+4)
	LD D,(IX+5)
	LD C,(IX+6)
	LD B,(IX+7)
	CALL VRAM_LDIRMV
    POP DE
    POP BC
    JP RESTORE_PAGE_INFO
; *******************************************************************************************************
 ENDIF

; *******************************************************************************************************
; common code to copy from VRAM to RAM
; input HL=VRAM source
; input DE=RAM destination
; BC=count
VRAM_LDIRMV:
	LD A,1
	LD (VRAM_UPDATE_IN_PROGRESS),A
	LD	A, L
	DI
	OUT	(099H), A
	LD	A, H
	AND	03FH
	NOP ; too fast VDP access per openMSX
	OUT	(099H), A
	EI
	NOP
	NOP
	NOP ; too fast VDP access per openMSX
.L4:
    IN A, (#98)
	LD (DE), A
    INC DE
    DEC BC
    LD A, C
    OR B
    JR NZ, .L4
	XOR A
	LD (VRAM_UPDATE_IN_PROGRESS),A
    RET
; *******************************************************************************************************