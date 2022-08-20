; sprite animation routines

; number of animation items and pointer
ANIMITEMNUM:
 DB 0
ANIMITEMPTR:
 DW EXT_END
; number of animation definitions and pointer
ANIMDEFNUM:
 DB 0
ANIMDEFPTR:
 DW EXT_END
; number of links between sprite and animation definitions
ANIMSPRNUM:
 DB 0
ANIMSPRPTR:
 DW EXT_END

; ANIMATION ITEM
; byte type = [0 - pattern and color change
;              1 - pattern definition change ]
; word ticks - number of ticks to hold this state
; for type = 0
;   byte pattern;
;   byte color;
; for type = 1
;   work data_pointer;
; total size = 5b

; ANIMATION DEFINITION
; byte number of items 1-15
; byte[15] anim_item;
; total size = 16b

; SPRITE/CHAR ANIMATION
; +00 byte sprite/char number;
; +01 word time;
; +03 byte current item;
; +04 byte animation definition;
; +05 byte cyclic;
; +06 byte active;
; +07 byte 0=sprite, 1-3 character bank
; total size = 8b

; *******************************************************************************************************
; helper function HL=A*5
; changes HL,DE;
Ax5:
    LD H,0
    LD L,A
    LD D,H
    LD E,L
    ADD HL,HL
    ADD HL,HL
    ADD HL,DE
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; helper function gets pointer to n-th animation item
; changes HL,DE;
GETnthANIMITEM:
    CALL Ax5
    LD DE,(ANIMITEMPTR)
    ADD HL,DE
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; helper function gets pointer to n-th entry in animation definition
; changes HL,DE;
GETnthANIMDEF:
    LD H,0
    LD L,A
    CALL HLx16
    LD DE,(ANIMDEFPTR)
    ADD HL,DE
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; helper function gets pointer to n-th entry in sprite animation
; changes HL,DE;
GETnthSPRANIM:
    LD H,0
    LD L,A
    CALL HLx8
    LD DE,(ANIMSPRPTR)
    ADD HL,DE
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL MAXANIMITEMS basic extension
; MAXANIMITEMS (BYTE number)
; sets new number and moves memory buffers as needed
MAXANIMITEMS:
	; opening (
	CALL CHKCHAR
	DB '('
	; get value
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
	; ending )
	CALL CHKCHAR
	DB ')'
    POP AF

	; save position
	PUSH HL
.ENTRY:
    LD B,A
    LD A,(ANIMITEMNUM)
    SUB B
    JR Z, .EXIT; same value as before
    LD IY,ANIMDEFPTR
    JP M, .INCREASE
    ; new value is lower than previous one
    CALL .SIZEDIFF
    CALL .DECREASE_COMMON
    LD HL,(ANIMSPRPTR)
    XOR A
    SBC HL,BC
    LD (ANIMSPRPTR),HL
.E1:
    LD HL,(FREEMEMPTR)
    XOR A
    SBC HL,BC
    LD (FREEMEMPTR),HL
.EXIT:
    EI
	POP HL
	RET
.INCREASE:
    NEG
    CALL .SIZEDIFF
    CALL .INCREASE_COMMON
    LD HL,(ANIMSPRPTR)
    ADD HL,BC
    LD (ANIMSPRPTR),HL
.E2:
    LD HL,(FREEMEMPTR)
    ADD HL,BC
    LD (FREEMEMPTR),HL
    JR .EXIT
.SIZEDIFF:
    CALL Ax5
    LD A,B
    LD (ANIMITEMNUM),A
    LD B,H
    LD C,L
    RET ; BC=size difference in bytes
.SIZETOMOVE:
    PUSH DE
    LD HL,(FREEMEMPTR)
    LD E,(IY)
    LD D,(IY+1)
    XOR A
    SBC HL,DE
    LD B,H
    LD C,L
    POP DE
    RET 
.DECREASE_COMMON:
    LD L,(IY)
    LD H,(IY+1)
    XOR A
    SBC HL,BC
    EX DE,HL
    PUSH BC
    CALL .SIZETOMOVE
    DI
    LD A,B
    OR C
    JR Z,.L1
    LD L,(IY)
    LD H,(IY+1)
    LDIR
.L1:
    POP BC
    LD L,(IY)
    LD H,(IY+1)
    XOR A
    SBC HL,BC
    LD (IY),L
    LD (IY+1),H
    RET 
.INCREASE_COMMON:
    LD HL,(FREEMEMPTR)
    DEC HL
    XOR A
    SBC HL,BC
    EX DE,HL
    PUSH BC
    CALL .SIZETOMOVE
    DI
    LD A,B
    OR C
    JR Z,.L2
    LD HL,(FREEMEMPTR)
    DEC HL
    LDDR
.L2:
    POP BC
    LD L,(IY)
    LD H,(IY+1)
    ADD HL,BC
    LD (IY),L
    LD (IY+1),H
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL ANIMITEMPAT basic extension
; ANIMITEMPAT ( BYTE id,
;               INT ticks,
;               BYTE pattern,
;               BYTE color )
; fills animation item data, returns an error if out of bounds
ANIMITEMPAT:
    ; opening (
	CALL CHKCHAR
	DB '('
	; get id
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
    ; check if out of bounds
    INC A
    LD C,A
    LD A,(ANIMITEMNUM)
    CP C
    JP C,SUBSCRIPT_OUT_OF_RANGE
	; comma
	CALL CHKCHAR
	DB ','
	; get ticks
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get pattern
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
	; comma
	CALL CHKCHAR
	DB ','
	; get color
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
	; ending )
	CALL CHKCHAR
	DB ')'
.ENTRY:
    PUSH HL
    POP IX
    EXX
    POP BC ; color
    POP DE ; pattern
    POP HL ; ticks
    EXX
    POP AF
    CALL GETnthANIMITEM
    PUSH HL
    POP IY
    EXX
    LD (IY),0 ; type=0
    LD (IY+1),L
    LD (IY+2),H
    LD (IY+3),D
    LD (IY+4),B
    
    PUSH IX
    POP HL
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL ANIMITEMPTR basic extension
; ANIMITEMPTR ( BYTE id,
;               INT ticks,
;               INT pointer,
; fills animation item data, returns an error if out of bounds
ANIMITEMPTR_CMD:
    ; opening (
	CALL CHKCHAR
	DB '('
	; get id
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
    ; check if out of bounds
    INC A
    LD C,A
    LD A,(ANIMITEMNUM)
    CP C
    JP C,SUBSCRIPT_OUT_OF_RANGE
	; comma
	CALL CHKCHAR
	DB ','
	; get ticks
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get pointer
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; ending )
	CALL CHKCHAR
	DB ')'
.ENTRY:
    PUSH HL
    POP IX
    EXX
    POP DE ; pointer
    POP HL ; ticks
    EXX
    POP AF
    CALL GETnthANIMITEM
    PUSH HL
    POP IY
    EXX
    LD (IY),1 ; type=1
    LD (IY+1),L
    LD (IY+2),H
    LD (IY+3),E
    LD (IY+4),D
    
    PUSH IX
    POP HL
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL MAXANIMDEFS basic extension
; MAXANIMDEFS (BYTE number)
; sets new number and moves memory buffers as needed
MAXANIMDEFS:
	; opening (
	CALL CHKCHAR
	DB '('
	; get value
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
	; ending )
	CALL CHKCHAR
	DB ')'
    POP AF

	; save position
	PUSH HL
.ENTRY:
    LD B,A
    LD A,(ANIMDEFNUM)
    SUB B
    JP Z, MAXANIMITEMS.EXIT; same value as before
    LD IY,ANIMSPRPTR
    JP M, .INCREASE
    ; new value is lower than previous one
    CALL .SIZEDIFF
    CALL MAXANIMITEMS.DECREASE_COMMON
    JP MAXANIMITEMS.E1
.INCREASE:
    NEG
    CALL .SIZEDIFF
    CALL MAXANIMITEMS.INCREASE_COMMON
    JP MAXANIMITEMS.E2
.SIZEDIFF:
    LD H,0
    LD L,A
    CALL HLx16
    LD A,B
    LD (ANIMDEFNUM),A
    LD B,H
    LD C,L
    RET ; BC=size difference in bytes
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL ANIMDEF basic extension
; ANIMITEMPAT ( BYTE id,
;               BYTE size,
;               INT[] list )
; fills animation definition data, returns an error if out of bounds, or invalid type
ANIMDEF:
    ; opening (
	CALL CHKCHAR
	DB '('
	; get id
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
    ; check if out of bounds
    INC A
    LD C,A
    LD A,(ANIMDEFNUM)
    CP C
    JP C,SUBSCRIPT_OUT_OF_RANGE
	; comma
	CALL CHKCHAR
	DB ','
	; get size
	LD IX, GETBYT
	CALL CALBAS
    CP 16
    JP NC, OVERFLOW
    OR A
    JP Z, OVERFLOW
	PUSH AF
	; comma
	CALL CHKCHAR
	DB ','
	; get pointer to a list of animation items in integer array format
    ; get array pointer
    POP DE
    PUSH DE
    LD A,2
    LD B,1
    CALL GET_BASIC_ARRAY_DATA_POINTER
    PUSH BC
	; ending )
	CALL CHKCHAR
	DB ')'
.ENTRY:
    PUSH HL
    POP IX
    POP DE ; pointer to INT array
    POP BC ; B=item number
    POP AF ; id
    PUSH DE
    CALL GETnthANIMDEF
    POP DE
    LD (HL),B
.L1:
    INC HL
    LD A,(DE)
    .2 INC DE
    LD (HL),A
    DJNZ .L1
    PUSH IX
    POP HL
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL MAXANIMSPRS basic extension
; MAXANIMSPRS (BYTE number)
; sets new number and moves memory buffers as needed
MAXANIMSPRS:
	; opening (
	CALL CHKCHAR
	DB '('
	; get value
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
	; ending )
	CALL CHKCHAR
	DB ')'
    POP AF

	; save position
	PUSH HL
.ENTRY:
    LD B,A
    LD A,(ANIMSPRNUM)
    SUB B
    JP Z, MAXANIMITEMS.EXIT; same value as before
    LD IY,FREEMEMPTR
    JP M, .INCREASE
    ; new value is lower than previous one
    CALL .SIZEDIFF
    CALL MAXANIMITEMS.DECREASE_COMMON
    JP MAXANIMITEMS.EXIT
.INCREASE:
    NEG
    PUSH AF; save difference for later to set active flag to 0 of new entires
    CALL .SIZEDIFF
    CALL MAXANIMITEMS.INCREASE_COMMON
    XOR A
    SBC HL,BC ; location of new stuff
    POP AF
    LD B,A
    LD DE,8
    PUSH HL
    POP IX
.L1:
    LD (IX+6),0
    ADD IX,DE
    DJNZ .L1
    JP MAXANIMITEMS.EXIT
.SIZEDIFF:
    LD H,0
    LD L,A
    CALL HLx8
    LD A,B
    LD (ANIMSPRNUM),A
    LD B,H
    LD C,L
    RET ; BC=size difference in bytes
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL ANIMSPRITE basic extension
; ANIMSPRITE ( BYTE id,
;              BYTE sprite_number,
;              BYTE animation_definition_id,
;              BYTE cyclic_flag )
; fills sprite animation data, returns an error if out of bounds, or invalid type
ANIMSPRITE:
    ; opening (
	CALL CHKCHAR
	DB '('
	; get sprite animation id
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
    INC A
    LD C,A
    LD A,(ANIMSPRNUM)
    CP C
    JP C,SUBSCRIPT_OUT_OF_RANGE
	; comma
	CALL CHKCHAR
	DB ','
	; get sprite number
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
    ; check if out of bounds
    CP 32
    JP NC, SUBSCRIPT_OUT_OF_RANGE
	; comma
	CALL CHKCHAR
	DB ','
	; get animation definition id
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
    INC A
    LD C,A
    LD A,(ANIMDEFNUM)
    CP C
    JP C,SUBSCRIPT_OUT_OF_RANGE
	; comma
	CALL CHKCHAR
	DB ','
	; get cyclic flag
	LD IX, GETBYT
    CALL CALBAS
	PUSH AF
	; ending )
	CALL CHKCHAR
	DB ')'
.ENTRY:
    PUSH HL
    POP IX
    EXX
    POP DE ; cyclic
    POP BC ; animation definition id
    POP HL ; sprite number
    EXX
    POP AF ; sprite animation id
    CALL GETnthSPRANIM
    PUSH HL
    POP IY
    EXX
    LD (IY),H
    LD (IY+4),B
    LD (IY+5),D
    ;LD (IY+6),0 -- not needed as set in MAXANIMSPRS
    ; following will do preparation for ANIMSTEP situation
    ; current item set to above limit and timer to 1
    ; any call to ANIMSTEP will switch and setup to first item for cyclic
    LD (IY+3),255
    LD (IY+1),1
    LD (IY+2),0
    ; mark as sprite animation
    LD (IY+7),0
    PUSH IX
    POP HL
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL ANIMCHAR basic extension
; ANIMCHAR ( BYTE id,
;            INT character number 0-767,
;            BYTE animation_definition_id,
;            BYTE cyclic_flag )
; fills sprite animation data, returns an error if out of bounds, or invalid type
ANIMCHAR:
    ; opening (
	CALL CHKCHAR
	DB '('
	; get sprite animation id
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
    INC A
    LD C,A
    LD A,(ANIMSPRNUM)
    CP C
    JP C,SUBSCRIPT_OUT_OF_RANGE
	; comma
	CALL CHKCHAR
	DB ','
	; get character number
	LD IX, FRMQNT
	CALL CALBAS
    PUSH DE
    ; check if out of bounds
    LD A,D
    CP 3
    JP NC, SUBSCRIPT_OUT_OF_RANGE
	; comma
	CALL CHKCHAR
	DB ','
	; get animation definition id
	LD IX, GETBYT
	CALL CALBAS
    PUSH AF
    INC A
    LD C,A
    LD A,(ANIMDEFNUM)
    CP C
    JP C,SUBSCRIPT_OUT_OF_RANGE
	; comma
	CALL CHKCHAR
	DB ','
	; get cyclic flag
	LD IX, GETBYT
    CALL CALBAS
	PUSH AF
	; ending )
	CALL CHKCHAR
	DB ')'
.ENTRY:
    PUSH HL
    POP IX
    EXX
    POP DE ; cyclic
    POP BC ; animation definition id
    POP HL ; character number
    EXX
    POP AF ; sprite animation id
    CALL GETnthSPRANIM
    PUSH HL
    POP IY
    EXX
    LD (IY),L
    INC H ; save character bank+1
    LD (IY+7),H
    LD (IY+4),B
    LD (IY+5),D
    ;LD (IY+6),0 -- not needed as set in MAXANIMSPRS
    ; following will do preparation for ANIMSTEP situation
    ; current item set to above limit and timer to 1
    ; any call to ANIMSTEP will switch and setup to first item for cyclic
    LD (IY+3),255
    LD (IY+1),1
    LD (IY+2),0
    PUSH IX
    POP HL
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function to handle CALL ANIMSTEP basic extension
; two forms
; ANIMSTEP ( BYTE id )
; or
; ANIMSTEP ( BYTE item_number,
;            INT[] sprite_animations )
; sets active flag to 1
ANIMSTEP:
    LD DE,ANIMSTARTSTOP_COMMON.STEP
    JR ANIMSTARTSTOP_COMMON
; *******************************************************************************************************
; *******************************************************************************************************
; function to handle CALL ANIMSTART basic extension
; two forms
; ANIMSTART ( BYTE id )
; or
; ANIMSTART ( BYTE item_number,
;             INT[] sprite_animations )
; sets active flag to 1
ANIMSTART:
    LD DE,ANIMSTARTSTOP_COMMON.START
    JR ANIMSTARTSTOP_COMMON
; *******************************************************************************************************
; *******************************************************************************************************
; function to handle CALL ANIMSTOP basic extension
; two forms
; ANIMSTOP ( BYTE id )
; or
; ANIMSTOP ( BYTE item_number,
;            INT[] sprite_animations )
; sets active flag to 1
ANIMSTOP:
    LD DE,ANIMSTARTSTOP_COMMON.STOP
; *******************************************************************************************************
ANIMSTARTSTOP_COMMON:
    LD (ANIMSTARTSTOP_COMMON.FN+1),DE
    ; opening (
	CALL CHKCHAR
	DB '('
	; get sprite animation id or array size
	LD IX,GETBYT
	CALL CALBAS
    PUSH AF
    ; check if comma present
    CALL GETPREVCHAR
    INC HL
    CP ','
    JR Z,.L1
    CP ')'
    JP NZ,SYNTAX_ERROR
    ; ok so single argument variant
    POP AF
    PUSH HL
    CALL .SETVALUE
    POP HL
    RET 
.L1:
    ; get array pointer
    POP DE
    PUSH DE
    LD A,2
    LD B,1
    CALL GET_BASIC_ARRAY_DATA_POINTER
    PUSH BC
	; ending )
	CALL CHKCHAR
	DB ')'
    POP DE ; array pointer
    POP BC ; number of items
    LD A,B
    OR A
    JP Z,SUBSCRIPT_OUT_OF_RANGE
    PUSH HL
    DI
.L2:
    PUSH BC
    LD A,(DE)
    .2 INC DE
    PUSH DE
    CALL .SETVALUE
    POP DE
    POP BC
    DJNZ .L2
    EI
    POP HL
    RET

.SETVALUE:
    LD B,A
    INC A
    LD C,A
    LD A,(ANIMSPRNUM)
    CP C
    JP C,SUBSCRIPT_OUT_OF_RANGE
    LD A,B
    CALL GETnthSPRANIM
    PUSH HL
    POP IX
.FN:
    JP 0
.START:
    LD (IX+6),1 ; active flag
    LD (IX+3),0 ; current item
    LD B,0 ; setup timer
    JP SETUP_ANIM_STEP 
.STOP:
    LD (IX+6),0 ; active flag
    RET 
.STEP:
    LD B,0
    JP PROCESS_SINGLE_ANIMATION.INACTIVE_TOO
; *******************************************************************************************************

; *******************************************************************************************************
; function processes animations during vblank period
PROCESS_ANIMATIONS:
    LD A,(ANIMSPRNUM)
    OR A
    RET Z; no animations defined
    LD B,A
    LD IX,(ANIMSPRPTR)
.L1:
    PUSH BC
    LD B,0 ; normal mode, change on timer expiry only
    CALL PROCESS_SINGLE_ANIMATION
    LD DE,8
    ADD IX,DE
    POP BC
    DJNZ .L1
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; processes single sprite animation
; skips inactive ones, but this can be skipped by calling .INACTIVE_TOO entry point
; on timer expiry goes to next animation item
; input IX=sprite animation pointer
; input B=1 force mode, activate animation action regardless of expired timer
PROCESS_SINGLE_ANIMATION:
    LD A,(IX+6); active
    OR A
    RET Z ; inactive animation
.INACTIVE_TOO:
    LD L,(IX+1)
    LD H,(IX+2) ; HL=end time
    DEC HL
    LD (IX+1),L
    LD (IX+2),H
    LD A,L
    OR H
    JR Z,.STEP
    DEC B
    INC B
    RET Z ; not forced mode, return
    JP SETUP_ANIM_STEP; call function with flag to skip timer setup
.STEP:
    LD B,0; setup timer
    INC (IX+3) ; current animation item
    JP SETUP_ANIM_STEP
; *******************************************************************************************************

; *******************************************************************************************************
; function will setup sprite animation after current item change
; input A=current animation definition
; input IX=pointer to sprite animation
; input B=1 skip timer setup
; output IY=pointer to animation item
; CF=1 error or non-cyclic animation ended, in both cases set active flag to 0
; basically sets new end time for current animation
INIT_CURRENT_ANIMATION:
    CALL GETnthANIMDEF
    LD A,(IX+3) ; current animation item
    CP (HL) ; number of animation items in the animation definition
    JR C,.L3 ; last item not reached
    ; last item reached
    LD A,(IX+5) ; cyclic flag
    OR A
    JR Z,.ERROR ; non-cyclic animation
    ; cyclic animation, restart
    LD (IX+3),0; current item
.L3:
    ; HL = animation definition
    INC HL ; skip animation definition size field
    LD D,0
    LD E,(IX+3); current item
    ADD HL,DE
    LD C,(HL) ; current animation item
    INC C
    LD A,(ANIMITEMNUM)
    CP C
    JR C,.ERROR ; invalid animation item, stop animation
    DEC C
    LD A,C
    CALL GETnthANIMITEM
    PUSH HL
    POP IY ; IY=animation item
    DEC B
    JR Z,.EXIT
    LD E,(IY+1)
    LD D,(IY+2) ; duration
    LD (IX+1),E
    LD (IX+2),D
.EXIT:
    XOR A
    RET 
.ERROR:
    SCF
    RET
; *******************************************************************************************************

; *******************************************************************************************************
; function will display currect item and set up expiry time
; it will also stop the animation if expired
; sets sprite update flag if any changes in sprite data made
; input IX=current sprite animation
; input B=1 skip timer setup
SETUP_ANIM_STEP:
    LD C,(IX+4) ; animation definition ID
    INC C
    LD A,(ANIMDEFNUM)
    CP C
    JR NC,.L2
    ; given animation item is outside of bounds, deactivate animation
.STOPANIM:
    LD (IX+6),0
    RET
.L2:
    DEC C
    LD A,C
    CALL INIT_CURRENT_ANIMATION
    JR C, .STOPANIM
    LD A,(IY) ; type of animation item
    OR A
    JR Z,.L4 ; change pattern and/or color
.PAT:
    ; change pattern definition
    ; check if sprite or character
    LD A,(IX+7)
    OR A
    JR NZ,.CHAR
    LD A,(IX) ; sprite number
    CALL GETnthSPRATTR
    .4 INC HL ; skip y and x
    LD A,(HL); current pattern
    LD H,0
    LD L,A
    LD A,(REG1SAV)
    AND 2
    JR NZ,.L6
    ; 8x8 sprite
    CALL HLx8
    LD B,8
    JR .L5
.L6:
    CALL HLx32
    LD B,32
.L5:
    LD A,(SCRMOD)
    DEC A
    JR NZ,.L10
    LD DE,(T32PAT)
    JR .L7
.L10:
    LD DE,(GRPPAT)
.L7:
    ADD HL,DE
    CALL SETWRT_LOCAL
    LD L,(IY+3)
    LD H,(IY+4) ; pointer to sprite pattern data
    JP BBYTECOPY 
.L4:
    ; change pattern and color in sprite attributes table
    LD A,(IX) ; sprite number
    CALL GETnthSPRATTR
    .4 INC HL ; skip y and x
    LD A,(IY+3) ; new pattern
    LD (HL),A
    .2 INC HL
    LD A,(IY+4) ; new color
    LD (HL),A
    LD HL,(SPRATR_UPDATE_FLAG)
    LD (HL),1
    RET
.CHAR:
    LD L,(IX)
    DEC A
    LD H,A
    CALL HLx8
    LD A,(SCRMOD)
    DEC A
    JR NZ,.L8
    LD DE,(T32CGP)
    JR .L9
.L8:
    LD DE,(GRPCGP)
.L9:
    LD B,8
    JR .L7
; *******************************************************************************************************
