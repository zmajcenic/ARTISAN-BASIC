; -----------------------------------------------------------------------------
; ZX0 decoder by Einar Saukas & Urusergi
; "Standard" version (68 bytes only)
; -----------------------------------------------------------------------------
; Parameters:
;   HL: source address (compressed data)
;   DE: destination address (decompressing)
; -----------------------------------------------------------------------------

dzx0_standard:
        ld      bc, $ffff               ; preserve default offset 1
        push    bc
        inc     bc
        ld      a, $80
dzx0s_literals:
        call    dzx0s_elias             ; obtain length
dzx0_ldir_1:
        ldir                            ; copy literals
        nop                             ; placeholder for call
        add     a, a                    ; copy from last offset or new offset?
        jr      c, dzx0s_new_offset
        call    dzx0s_elias             ; obtain length
dzx0s_copy:
        ex      (sp), hl                ; preserve source, restore offset
        push    hl                      ; preserve offset
        add     hl, de                  ; calculate destination - offset
dzx0_ldir_2:
        ldir                            ; copy from offset
        nop                             ; placeholder for call
        pop     hl                      ; restore offset
        ex      (sp), hl                ; preserve offset, restore source
        add     a, a                    ; copy from literals or new offset?
        jr      nc, dzx0s_literals
dzx0s_new_offset:
        pop     bc                      ; discard last offset
        ld      c, $fe                  ; prepare negative offset
        call    dzx0s_elias_loop        ; obtain offset MSB
        inc     c
        ret     z                       ; check end marker
        ld      b, c
        ld      c, (hl)                 ; obtain offset LSB
        inc     hl
        rr      b                       ; last offset bit becomes first length bit
        rr      c
        push    bc                      ; preserve new offset
        ld      bc, 1                   ; obtain length
        call    nc, dzx0s_elias_backtrack
        inc     bc
        jr      dzx0s_copy
dzx0s_elias:
        inc     c                       ; interlaced Elias gamma coding
dzx0s_elias_loop:
        add     a, a
        jr      nz, dzx0s_elias_skip
        ld      a, (hl)                 ; load another group of 8 bits
        inc     hl
        rla
dzx0s_elias_skip:
        ret     c
dzx0s_elias_backtrack:
        add     a, a
        rl      c
        rl      b
        jr      dzx0s_elias_loop
; -----------------------------------------------------------------------------

; *******************************************************************************************************
; helper function for VRAM unpack to save AF prior to calling VRAM copy fn
DXZ0s_VRAM_LDIR:
        PUSH AF ; save AF used by algorithm
        PUSH DE
        EX DE, HL
        ADD HL, BC
        PUSH HL
        POP IY
        EX DE, HL
        POP DE
        CALL VRAM_LDIRVM
        LD C,0
        PUSH IY
        POP DE
        POP AF
        RET
; *******************************************************************************************************

 IF (BASIC_EXTENSION == 1)
; *******************************************************************************************************
; function to handle CALL VUNPACK basic extension
; _VUNPACK ( INT source, 
;			 INT destination )
; will put ram in page 0 also, page 1 is already there
; *******************************************************************************************************
VUNPACK:
    LD A, #CD ; CALL
    LD (dzx0_ldir_1), A
    LD (dzx0_ldir_2), A
    LD DE, DXZ0s_VRAM_LDIR
    LD (dzx0_ldir_1 + 1), DE
    LD (dzx0_ldir_2 + 1), DE
    JR UNPACK_COMMON
; function to handle CALL UNPACK basic extension
; _UNPACK ( INT source, 
;			INT destination )
; will put ram in page 0 also, page 1 is already there
UNPACK:
    LD DE, #B0ED ; LDIR
    LD (dzx0_ldir_1), DE
    LD (dzx0_ldir_2), DE
    XOR A ; NOP
    LD (dzx0_ldir_1 + 2), A
    LD (dzx0_ldir_2 + 2), A
UNPACK_COMMON:
	; opening (
	CALL CHKCHAR
	DB '('
	; get source address
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; comma
	CALL CHKCHAR
	DB ','
	; get destination address
	LD IX, FRMQNT
	CALL CALBAS
	PUSH DE
	; ending )
	CALL CHKCHAR
	DB ')'

	; save position
	PUSH HL
	POP IX

	POP DE ; destination
	POP HL ; source
	EXX
	; enable page 0
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	EXX
	CALL dzx0_standard
        POP DE
        POP BC
        CALL RESTORE_PAGE_INFO
	PUSH IX
	POP HL
	RET
; *******************************************************************************************************
 ENDIF

 IF (DEFUSR_EXTENSION == 1)
; *******************************************************************************************************
; same as VUNPACK but for DEFUSR approach
; input IX=pointer to input array, real data from +2
; +2 = source address
; +4 = destination address
; *******************************************************************************************************
VUNPACK_DEFUSR:
    LD A, #CD ; CALL
    LD (dzx0_ldir_1), A
    LD (dzx0_ldir_2), A
    LD HL, DXZ0s_VRAM_LDIR
    LD (dzx0_ldir_1 + 1), HL
    LD (dzx0_ldir_2 + 1), HL
    JR UNPACK_DEFUSR_COMMON
; same as UNPACK but for DEFUSR approach
; input IX=pointer to input array, real data from +2
; +2 = source address
; +4 = destination address
UNPACK_DEFUSR:
    LD HL, #B0ED ; LDIR
    LD (dzx0_ldir_1), HL
    LD (dzx0_ldir_2), HL
    XOR A ; NOP
    LD (dzx0_ldir_1 + 2), A
    LD (dzx0_ldir_2 + 2), A
UNPACK_DEFUSR_COMMON:
	; enable page 0
	LD IY, .RET
	JP ENABLE_PAGE0
.RET:
	EI
	LD L,(IX+2)
	LD H,(IX+3)
	LD E,(IX+4)
	LD D,(IX+5)
	CALL dzx0_standard
    POP DE
    POP BC
    JP RESTORE_PAGE_INFO
; *******************************************************************************************************
 ENDIF