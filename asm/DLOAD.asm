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
    