; entry function that handles call using CALLF
; pointer to data structure is expected at DAC+2
; first entry must be function id followed by function specific parameters

DEFUSR_TABLE_ENTRIES    EQU 29

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

 IF (BLIT_CMDS == 1)        ; 4
 DW BLIT_DEFUSR
 ELSE
 DW NOACTION_DEFUSR
 ENDIF

 IF (ANIM_CMDS == 1)        ; 5
 DW SGAM_DEFUSR
 ELSE
 DW NOACTION_DEFUSR
 ENDIF

 IF (SPRITE_CMDS == 1)
 DW SPRGRPMOV_DEFUSR        ; 6
 ELSE
 DW NOACTION_DEFUSR
 ENDIF

 IF (COLL_CMD == 1)
 DW COLL_DEFUSR             ; 7
 ELSE
 DW NOACTION_DEFUSR
 ENDIF 

 IF (SOUND_CMDS == 1)
 DW SNDSFX_DEFUSR             ; 8
 ELSE
 DW NOACTION_DEFUSR
 ENDIF 

 IF (ANIM_CMDS == 1)
 DW ANIMSTEP_SINGLE_DEFUSR      ; 9
 DW ANIMSTEP_MULTI_DEFUSR       ; 10
 DW ANIMSTART_SINGLE_DEFUSR     ; 11
 DW ANIMSTART_MULTI_DEFUSR      ; 12
 DW ANIMSTOP_SINGLE_DEFUSR      ; 13
 DW ANIMSTOP_MULTI_DEFUSR       ; 14
 ELSE
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
 ENDIF

 IF (BOX_CMDS == 1)
 DW BOXMEMCPY_DEFUSR            ; 15
 DW BOXMEMVRM_DEFUSR            ; 16
 ELSE
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
 ENDIF

 IF (ANIM_CMDS == 1)
 DW MAXANIMITEMS_DEFUSR         ; 17
 DW MAXANIMDEFS_DEFUSR          ; 18
 DW MAXANIMSPRS_DEFUSR          ; 19
 DW MAXAUTOSGAMS_DEFUSR         ; 20
 DW ANIMITEMPAT_DEFUSR          ; 21
 DW ANIMITEMPTR_DEFUSR          ; 22
 DW ANIMDEF_DEFUSR              ; 23
 DW ANIMSPRITE_DEFUSR           ; 24
 DW ANIMCHAR_DEFUSR             ; 25
 DW AUTOSGAMDEF_DEFUSR          ; 26
 DW AUTOSGAMSTART_DEFUSR        ; 27
 DW AUTOSGAMSTOP_DEFUSR         ; 28
 ELSE
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
 DW NOACTION_DEFUSR
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
    