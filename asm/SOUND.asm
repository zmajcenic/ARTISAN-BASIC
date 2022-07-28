MUSIC_INIT_STATUS:
 DB 0
SFX_INIT_STATUS:
 DB 0
SOUND_ENABLED:
 DB 0

; *******************************************************************************************************
; function to handle CALL SNDPLYINIT basic extension
; initializes sound player
; _SNDPLYINIT ( INT music_offset, 
;				INT sfx_offset, can be -1 if no SFX
; will put ram in page 0 also, page 1 is already there
; sets variables MUSIC_INIT_STATUS and SFX_INIT_STATUS
SNDPLYINIT:
	; opening (
	CALL CHKCHAR
	DB '('
	; get music address
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get sfx address
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; ending )
	CALL CHKCHAR
	DB ')'

    ; save position in BASIC text
	LD B, H
	LD C, L

	; pop LDIR parameters and store away for later
	POP DE ; sfx address
	POP HL ; music address
	PUSH BC ; basic text location
	EXX
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EXX

	PUSH DE
	XOR A
	; HL = music location
	CALL PLY_AKG_INIT
	LD A, 1
	LD (MUSIC_INIT_STATUS), A

	POP HL ; SFX
	; check if SFX address -1
	INC HL
	LD A, L
	OR H
	JR Z,.L1
	DEC HL
	CALL PLY_AKG_INITSOUNDEFFECTS
	LD A, 1
	LD (SFX_INIT_STATUS), A
.L1:
    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP HL
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL SNDPLYON basic extension
; enables sound player
; _SNDPLYON
; sets SOUND_ENABLED variable to 1 if init call was done
; if not throws out of data error
SNDPLYON:
	LD A, (MUSIC_INIT_STATUS)
	OR A
	JR NZ, .L1
	; player not initialized, throw error
	LD E, 04 ; Out of DATA 
	JP THROW_ERROR
.L1:
	LD (SOUND_ENABLED), A
	; disable key click
	XOR A
	LD (CLIKSW), A
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL SNDPLYOFF basic extension
; disables sound player
; _SNDPLYOFF
; sets SOUND_ENABLED variable to 0
; calls AKG to stop music and SFX on all channels if initialized
SNDPLYOFF:
	LD A, (SOUND_ENABLED)
	OR A
	RET Z ; already stopped
	XOR A
	LD (SOUND_ENABLED), A
	PUSH HL
	CALL PLY_AKG_STOP
	LD A, (SFX_INIT_STATUS)
	OR A
	JR Z, .EXIT ; SFX not in use
	XOR A
	CALL PLY_AKG_STOPSOUNDEFFECTFROMCHANNEL
	LD A, 1
	CALL PLY_AKG_STOPSOUNDEFFECTFROMCHANNEL
	LD A, 2
	CALL PLY_AKG_STOPSOUNDEFFECTFROMCHANNEL
.EXIT:
	POP HL
	RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL SNDSFX basic extension
; plays a sound effect
; _SNDSFX ( BYTE sfx_number, >0
;			BYTE channel, = 0,1 or 2
;			BYTE inverted_volume = 0-16, 0 being full volume
; will put ram in page 0 also, page 1 is already there
; if sound off throws illegal function call
; if sfx not initialized, throws out of data
SNDSFX:
	; opening (
	CALL CHKCHAR
	DB '('
	; get sfx_number
	LD IX, GETBYT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get sfx address
	LD IX, GETBYT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get inverted volume
	LD IX, GETBYT
	CALL CALBAS
	PUSH DE
	; ending )
	CALL CHKCHAR
	DB ')'

	LD A, (SOUND_ENABLED)
	OR A
	JR NZ, .L1
	; sound disabled, throw illegal function call
	LD E, 5
	JP THROW_ERROR
.L1:
	LD A, (SFX_INIT_STATUS)
	OR A
	JR NZ, .L2
	; sfx data not initialized, throw out of data
	LD E, 4
	JP THROW_ERROR
.L2:
	; pop  parameters and store away for later
	POP DE ; inverted volume
	LD B, E
	POP DE ; channel
	LD C, E
	POP DE
	LD A, E
	EX AF, AF'
	PUSH HL ; basic text location
	EXX
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EXX
	EX AF, AF'
	CALL PLY_AKG_PLAYSOUNDEFFECT

    POP DE
    POP BC
    CALL RESTORE_PAGE_INFO

	POP HL
	RET
; *******************************************************************************************************