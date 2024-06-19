; *******************************************************************************************************
; function processes file name
; filenames supported are D:FILENAME.EXT, FILENAME.EXT, D:FILENAME, FILENAME
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
	LD (BLIT_STRUCT), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get destination
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+2), DE
	; comma
	CALL CHKCHAR
	DB ','
	; get size
	LD IX, FRMQNT
	CALL CALBAS
	LD (BLIT_STRUCT+4), DE
	; ending )
	CALL CHKCHAR
	DB ')'
    RET
; *******************************************************************************************************

 ENDIF
