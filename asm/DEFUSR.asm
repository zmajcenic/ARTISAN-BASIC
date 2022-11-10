; entry function that handles call using CALLF
; pointer to data structure is expected at DAC+2
; first entry must be function id followed by function specific parameters

DEFUSR_TABLE_ENTRIES    EQU 4

DEFUSR_JUMP_TABLE:
 IF (SPRITE_CMDS == 1)
 DW SPRENABLE_DEFUSR        ; 0
 ELSE
 DW NOACTION_DEFUSR
 ENDIF

 IF (SPRITE_CMDS == 1)
 DW SPRDISABLE_DEFUSR        ; 1
 ELSE
 DW NOACTION_DEFUSR
 ENDIF

 IF (RAM_CMDS == 1)
 DW MEMCPY_DEFUSR           ; 2
 ELSE
 DW NOACTION_DEFUSR
 ENDIF

 IF (VRAM_CMDS == 1)        ; 3
 DW MEMVRM_DEFUSR
 ELSE
 DW NOACTION_DEFUSR
 ENDIF

DEFUSR_ENTRY:
    EI
    LD IX,(DAC+2)
    LD A,(IX)
    CP DEFUSR_TABLE_ENTRIES
    RET NC ; return if an undefined function requested
    LD H,0
    LD L,A
    ADD HL,HL
    LD DE,DEFUSR_JUMP_TABLE
    ADD HL,DE
    EX DE,HL
    LD A,(DE)
    LD L,A
    INC DE
    LD A,(DE)
    LD H,A
    JP (HL) ; call function with IX=pointer to data array
    