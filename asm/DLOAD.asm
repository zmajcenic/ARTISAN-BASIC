BDOS_SETDTA     EQU #1A
BDOS_OPEN       EQU #0F
BDOS_CLOSE      EQU #10
BDOS_RDBLK      EQU #27    

; *******************************************************************************************************
; function processes file name
; filenames supported are D:FILENAME.EXT, FILENAME.EXT, D:FILENAME, FILENAME
; FCB 0 will be zeroed out
; FCB 0 will be populated with drive info 0=default or 1-8 for drives A-H
; FCB 0 will be populated with file name
; input HL=pointer to expression
; input B=length
; returns CF=1 if bad filename
DLOAD_PROCESS_FILENAME:
    ; check for zero size
    LD A,B
    OR A
    JR Z, .BADFILENAME
    ; check for more than 2+8+1+3=14
    CP 15
    JR NC, .BADFILENAME
    ; check if more than 2 letters
    CP 3
    JR C, .L7 ; no drive
    ; check for : at proper place
    LD E, (HL)
    INC HL
    LD A, (HL)
    CP ':'
    JR NZ, .L1
    ; so we have : , check for letters A-H
    LD A,E
    CALL UPPER
    SUB 'A'
    CP 9
    JR NC, .BADFILENAME 
    INC HL
    DEC B ; consume two characters
    DEC B
.L2:
    LD DE, (FCB0)
    LD (DE), A
    JR .PROCESS_FILENAME
.L1:
    ; no drive specified
    DEC HL
.L7:
    XOR A
    JR .L2
.PROCESS_FILENAME:
    ; HL is pointing to rest of the name
    INC DE ; 8-character filename location, needs to be padded with blanks
    LD C,8 ; filename length
.L4:
    CALL .GETCHAR
    CP '.'
    JR Z, .L6 ; if dot, fill rest with blanks
    LD (DE),A
    INC DE
    DEC C
    JR NZ, .L4
    ; so we cleared filename part
    LD A,B ; no more letters. just fill extension with blanks
    OR A
    JR Z, .L8
    CALL .GETCHAR ; here we must have . for a valid name
    CP '.'
    JR NZ, .BADFILENAME
.L8:
    LD C,3
.L5:
    CALL .GETCHAR
    LD (DE),A
    INC DE
    DEC C
    JR NZ, .L5
    LD A,B ; nothing must be left in buffer
    OR A
    JR NZ, .BADFILENAME
    ; file name correct, now zero out the rest of FCB 0
    LD HL,(FCB0)
    LD DE,12
    ADD HL,DE
    LD (HL),0
    LD D,H
    LD E,L
    INC DE
    LD BC,37-12-1
    LDIR
    XOR A ; clear carry flag
    RET    
.L6:
    LD A,' '
.L9:
    LD (DE),A
    INC DE
    DEC C
    JR NZ, .L9
    JR .L8
.BADFILENAME:
    SCF
    RET
.GETCHAR: ; gets a character, returns blank if we read past input
    LD A,B
    OR A
    JR Z, .BLANK
    LD A,(HL)
    CALL UPPER
    INC HL
    DEC B
    RET
.BLANK:
    LD A,' '
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function sets disk buffer from NULBUF
; input none
; returns ZF!=1 on error
DLOAD_SETDTA:
    LD DE,(NULBUF)
    LD C, BDOS_SETDTA
    JR BDOS_CALL
; *******************************************************************************************************

; *******************************************************************************************************
; function opens a file using FCB 0
; input none
; returns ZF!=1 on error
DLOAD_OPENFILE:
    LD DE,(FCB0)
    LD C, BDOS_OPEN
    JR BDOS_CALL
; *******************************************************************************************************

; *******************************************************************************************************
; function makes a file seek and sets record size to 1 byte
; input none
; output node
DLOAD_SEEK:
    LD HL,(TMP_STRUCT)
    LD IX,(FCB0)
    LD (IX+33),L
    LD (IX+34),H
    LD (IX+14),1
    LD (IX+15),0
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function reads a number of bytes from a file using FCB 0 to (NULBUF) buffer
; input HL=number of bytes to read
; returns HL=number of bytes read
; returns ZF!=1 on error
DLOAD_READ:
    LD DE,(FCB0)
    LD C,BDOS_RDBLK
BDOS_CALL:
    CALL BDOS
    OR A
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function closes a file FCB 0
; input none
; returns ZF!=1 on error
DLOAD_CLOSE:
    XOR A
    LD DE,(FCB0)
    LD C,BDOS_CLOSE
    JR BDOS_CALL
; *******************************************************************************************************

; *******************************************************************************************************
; function copies data from (NULBUF) to destination, enables RAM in page 0
; input BC=number of bytes to copy
; output none
DLOAD_TRANSFERBLOCK:
    EXX ; save BC
	; enable page 0
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
    EXX
    LD DE,(TMP_STRUCT+2)
    LD HL,(NULBUF)
	LDIR
    LD (TMP_STRUCT+2),DE
    POP DE
    POP BC
    JP RESTORE_PAGE_INFO
; *******************************************************************************************************	

; *******************************************************************************************************
; function opens and loads a file in FCB 0
; file name needs to be already set in FCB 0
; input none
; returns ZF!=1 on error
DLOAD_LOADFILE:
    CALL DLOAD_SETDTA
    CALL DLOAD_OPENFILE
    RET NZ
    CALL DLOAD_SEEK
    LD BC,(TMP_STRUCT+4)
.L1:
    LD A,B
    OR A
    JR Z,.REST
    DEC B
    LD HL,256
.L2:
    PUSH BC
    PUSH HL
    CALL DLOAD_READ
    POP BC
    JR NZ,.ERRREAD
    CALL DLOAD_TRANSFERBLOCK
    POP BC
    JR .L1
.REST:
    LD A,C
    OR A
    JR Z,.EXIT
    LD H,0
    LD L,C
    LD C,H
    JR .L2
.ERRREAD:
    POP BC
.EXIT:
    PUSH AF
    CALL DLOAD_CLOSE
    POP AF
    RET
; *******************************************************************************************************

 IF (BASIC_EXTENSION == 1)

; *******************************************************************************************************
; function to handle CALL DLOAD basic extension
; _DLOAD ( STRING filename, 
;		   INT offset, 
;		   INT destination,
;          INT size ) 
; will put ram in page 0 also, page 1 is already there
DLOAD:
	; opening (
	CALL CHKCHAR
	DB '('
    CALL EVALTXTPARAM
    PUSH HL
    CALL GETSTRPNT
    CALL DLOAD_PROCESS_FILENAME
    JP C, BAD_FILENAME
    POP HL
	; comma
	CALL CHKCHAR
	DB ','
	; get offset
	LD IX, FRMQNT
	CALL CALBAS
	LD (TMP_STRUCT), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get destination
	LD IX, FRMQNT
	CALL CALBAS
	LD (TMP_STRUCT+2), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get size
	LD IX, FRMQNT
	CALL CALBAS
	LD (TMP_STRUCT+4), DE
	; ending )
	CALL CHKCHAR
	DB ')'

    PUSH HL
    CALL DLOAD_LOADFILE
    JP NZ, DISKIOERR
    POP HL
    RET
; *******************************************************************************************************

 ENDIF

 IF (DEFUSR_EXTENSION == 1)
; *******************************************************************************************************
; same as DLOAD but for DEFUSR approach
; input IX=pointer to input array, real data from +2
; +2 = string pointer as provided by MSX-BASIC: length, followed by a pointer to ASCII data
; +4 = offset
; +6 = destination
; +8 = size
; output A=0 on success
; NOTE: this call will fail if called under X-BASIC as strings are handled differently in memory: length followed by ASCII data
; *******************************************************************************************************
DLOAD_DEFUSR:
    LD L,(IX+2)
    LD H,(IX+3)
    LD B,(HL) ; string length
    INC HL
    LD E,(HL)
    INC HL
    LD D,(HL)
    EX DE,HL ; pointer to ASCIIZ text
    CALL DLOAD_PROCESS_FILENAME
    JR C,.ERR ; exit on error
    LD L,(IX+4)
    LD H,(IX+5)
    LD (TMP_STRUCT),HL ; offset
    LD L,(IX+6)
    LD H,(IX+7)
    LD (TMP_STRUCT+2),HL ; destination
    LD L,(IX+8)
    LD H,(IX+9)
    LD (TMP_STRUCT+4),HL ; size
    CALL DLOAD_LOADFILE
    JR NZ,.ERR
    XOR A
    RET
.ERR:
    LD A,1
    RET
; *******************************************************************************************************    

 ENDIF

