 ORG 08800H

 INCLUDE "system_variables.inc"
 INCLUDE "system_hooks.inc"
 INCLUDE "bios_calls.inc"

RAMAD0	equ	0F341h	; Main-RAM Slot (00000h~03FFFh)
RAMAD1	equ	0F342h	; Main-RAM Slot (04000h~07FFFh)
RAMAD2	equ	0F343h	; Main-RAM Slot (08000h~0BFFFh)
RAMAD3	equ	0F344h	; Main-RAM Slot (0C000h~0FFFFh)

 DB 0FEH ; header
 DW LoaderBegin 
 DW LoaderEnd-1
 DW LoaderStart

LoaderBegin:

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

LoaderStart:
    DI
    XOR A
    CALL GET_PAGE_INFO
    PUSH BC
    PUSH DE
    LD A, (RAMAD0)
    LD H, 0
    CALL LOCAL_ENASLT

    LD A, 1
    CALL GET_PAGE_INFO
    PUSH BC
    PUSH DE
    LD A, (RAMAD1)
    LD H, 040H
    CALL LOCAL_ENASLT

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

    EI
    RET

LoaderEnd:
