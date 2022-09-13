ORIG.HTIMI:
	DB 0, 0, 0, 0, 0
 EXPORT ORIG.HTIMI

; *******************************************************************************************************
; interrupt handler when page 0 enabled
; we are here only if one of the extended basic commands have paged in page 0
; which means we arrived from BASIC so page 2 is already good
VBLANK:
	EXPORT VBLANK

    PUSH AF
	; is VDP originator ?
	IN	A, (099H)
	AND	A
	JP P, .EXIT

 IF (SOUND_CMDS + SPRITE_CMDS > 0)
    PUSH BC
    PUSH DE
    PUSH HL
    EX AF, AF'
    EXX
    PUSH AF
    PUSH BC
    PUSH DE
    PUSH HL
    PUSH IX
    PUSH IY

 IF (SPRITE_CMDS == 1)
	CALL PROCESS_SPRITES_AND_ANIMATIONS
 ENDIF

 IF (SOUND_CMDS == 1)
	LD A, (SOUND_ENABLED)
	OR A
	CALL NZ,PLY_AKG_PLAY
 ENDIF

    ; increase JIFFY
    ;LD HL,(JIFFY)
    ;INC HL
    ;LD (JIFFY),HL

    POP IY
    POP IX
    POP HL
    POP DE
    POP BC
    POP AF
    EX AF, AF'
    EXX
    POP HL
    POP DE
    POP BC
 ENDIF

.EXIT:
	POP AF
	EI
	RETI
; *******************************************************************************************************

; *******************************************************************************************************
; H.TIMI function
; we can end up here from anywhere so page in both page 0 and 2
MBGE_HTIMI:
 EXPORT MBGE_HTIMI

 IF (SOUND_CMDS + SPRITE_CMDS > 0)

	PUSH AF

	; enable page 2
    LD A, 2
    CALL GET_PAGE_INFO
    PUSH BC
    PUSH DE
    LD A, (RAMAD2)
    LD H, 080H
    CALL LOCAL_ENASLT
	; enable page 0
    XOR A
    CALL GET_PAGE_INFO
    PUSH BC
    PUSH DE
    LD A, (RAMAD0)
    LD H, 0
    CALL LOCAL_ENASLT
	
 IF (SPRITE_CMDS == 1)
	CALL PROCESS_SPRITES_AND_ANIMATIONS
 ENDIF

 IF (SOUND_CMDS == 1)
	LD A, (SOUND_ENABLED)
	OR A
	CALL NZ,PLY_AKG_PLAY
 ENDIF

	; restore page 0
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO
	; restore page 2
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP AF
 ENDIF
	JP ORIG.HTIMI
; *******************************************************************************************************

; *******************************************************************************************************
; function checks if the sprite system is initialized and what screen mode we are running
; also checks if some VRAM modifying command is running
; when that checks out calls sprite updates and animation processing
; if in an unsupported mode disables sprite handling
PROCESS_SPRITES_AND_ANIMATIONS:
	; check if initialized
	LD A, (SPRATR_INIT_STATUS)
	OR A
	RET Z
	; check screen mode
	LD A, (SCRMOD)
	DEC A
	JR Z, .L0 ; screen 1
	DEC A
    JR Z, .L0 ; screen 2
	; unsupported screen mode, disable 
    XOR A
    LD (SPRATR_INIT_STATUS),A
    RET
.L0:
    ; check if anyone else is working with VRAM
    LD A,(VRAM_UPDATE_IN_PROGRESS)
    OR A
    RET NZ

    CALL SPRATR_UPDATE

 IF (ANIM_CMDS == 1)
    CALL PROCESS_ANIMATIONS
    CALL PROCESS_AUTOSGAMS
 ENDIF
    RET
; *******************************************************************************************************