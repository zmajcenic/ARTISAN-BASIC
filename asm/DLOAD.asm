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
    ; check for more than 13 letters
    CP 14
    JR NC, .BADFILENAME
    ; check if more than 2 letters
    CP 3
    JR C, .PROCESS_FILENAME
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
    XOR A
    DEC HL
    JR .L2
.PROCESS_FILENAME:
    ; HL is pointing to rest of the name
    INC DE ; 8-character filename location, needs to be padded with blanks
    LD C,8 ; filename length
.L4:
    CALL .GETCHAR
    CP '.'
    JR Z, .L6 ; if dot, stay here until we fill up filename part
.L3:
    LD (DE),A
    INC DE
    DEC C
    JR NZ, .L4
    ; so we cleared filename part, DE at extension part
    ; we are either at . or past 8 chars
    INC HL ; skip potential . and don't care
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
    LD A,' ' ; case where we stay at first .
    DEC HL
    JR .L3
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