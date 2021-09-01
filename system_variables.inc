;+------------------------------------------------------------------+ 
;|                         MSX System Variables                     |
;+------------------------------------------------------------------+ 
;|                                                                  |
;|  Taken from MSX RED BOOK  (c) 1985 by Avalon Software            |
;|                           (c) 1988 by McGraw Hill                |
;|                                                                  |
;|  Formatted as Z80 Assembly include by Daniel Jose Viana, 2014    |
;|                                                                  |
;+------------------------------------------------------------------+
;
;    The section of the Workspace Area from F380H to FD99H holds
;the BIOS/Interpreter variables. These are listed on the
;following pages in standard assembly language form:
;

 IFNDEF SYSTEM_VARIABLES
 DEFINE SYSTEM_VARIABLES

RDPRIM: equ  0F380H  ;      OUT (0A8H),A ; Set new Primary Slot
;            0F382H         LD E,(HL)    ; Read memory
;            0F383H         JR WRPRM1    ; Restore old Primary Slot
;
;    This routine is used by the RDSLT standard routine to switch
;Primary Slots and read a byte from memory. The new Primary Slot
;Register setting is supplied in register A, the old setting in
;register D and the byte read returned in register E.
;
WRPRIM: equ 0F385H ;       OUT (0A8H),A ; Set new Primary Slot
;           0F387H         LD (HL),E    ; Write to memory
;           0F388H WRPRM1: LD A,D       ; Get old setting
;           0F389H         OUT (0A8H),A ; Restore old Primary Slot
;           0F38BH         RET
;
;    This routine is used by the WRSLT standard routine to switch
;Primary Slots and write a byte to memory. The new Primary Slot
;Register setting is supplied in register A, the old setting in
;register D and the byte to write in register E.
;
CLPRIM: equ 0F38CH ;       OUT (0A8H),A ; Set new Primary Slot
;           0F38EH         EX AF,AF'    ; Swap to AF for call
;           0F38FH         CALL CLPRM1  ; Do it
;           0F392H         EX AF,AF'    ; Swap to AF
;           0F393H         POP AF       ; Get old setting
;           0F394H         OUT (0A8H),A ; Restore old Primary Slot
;           0F396H         EX AF,AF'    ; Swap to AF
;           0F397H         RET
;           0F398H CLPRM1: JP (IX)
;
;    This routine is used by the CALSLT standard routine to switch
;Primary Slots and call an address. The new Primary Slot
;Register setting is supplied in register A, the old setting on
;the Z80 stack and the address to call in register pair IX.
;
USRTAB: equ 0F39AH ;       DEFW 475AH   ; USR 0
;           0F39CH         DEFW 475AH   ; USR 1
;           0F39EH         DEFW 475AH   ; USR 2
;           0F3A0H         DEFW 475AH   ; USR 3
;           0F3A2H         DEFW 475AH   ; USR 4
;           0F3A4H         DEFW 475AH   ; USR 5
;           0F3A6H         DEFW 475AH   ; USR 6
;           0F3A8H         DEFW 475AH   ; USR 7
;           0F3AAH         DEFW 475AH   ; USR 8
;           0F3ACH         DEFW 475AH   ; USR 9
;
;    These ten variables contain the "USR" function addresses. Their
;values are set to the Interpreter "Illegal function call" error
;generator at power-up and thereafter only altered by the
;"DEFUSR" statement.
;
LINL40: equ 0F3AEH ; DEFB 37
;
;    This variable contains the 40x24 Text Mode screen width. Its
;value is set at power-up and thereafter only altered by the
;"WIDTH" statement.
;
LINL32: equ 0F3AFH ; DEFB 29
;
;    This variable contains the 32x24 Text Mode screen width. Its
;value is set at power-up and thereafter only altered by the
;"WIDTH" statement.
;
LINLEN: equ 0F3B0H ;  DEFB 37
;
;    This variable contains the current text mode screen width. Its
;value is set from LINL40 or LINL32 whenever the VDP is
;initialized to a text mode via the INITXT or INIT32 standard
;routines.
;
CRTCNT: equ 0F3B1H ;  DEFB 24
;
;    This variable contains the number of rows on the screen. Its
;value is set at power-up and thereafter unaltered.
;
CLMLST: equ 0F3B2H  ; DEFB 14
;
;    This variable contains the minimum number of columns that must
;still be available on a line for a data item to be "PRINT"ed,
;if less space is available a CR,LF is issued first. Its value
;is set at power-up and thereafter only altered by the "WIDTH"
;and "SCREEN" statements.
;
TXTNAM: equ 0F3B3H  ; DEFW 0000H   ; Name Table Base
TXTCOL: equ 0F3B5H  ; DEFW 0000H   ; Colour Table Base
TXTCGP: equ 0F3B7H  ; DEFW 0800H   ; Character Pattern Base
TXTATR: equ 0F3B9H  ; DEFW 0000H   ; Sprite Attribute Base
TXTPAT: equ 0F3BBH  ; DEFW 0000H   ; Sprite Pattern Base
;
;    These five variables contain the 40x24 Text Mode VDP base
;addresses. Their values are set at power-up and thereafter only
;altered by the "BASE" statement.
;
T32NAM: equ 0F3BDH  ; DEFW 1800H   ; Name Table Base
T32COL: equ 0F3BFH  ; DEFW 2000H   ; Colour Table Base
T32CGP: equ 0F3C1H  ; DEFW 0000H   ; Character Pattern Base
T32ATR: equ 0F3C3H  ; DEFW 1B00H   ; Sprite Attribute Base
T32PAT: equ 0F3C5H  ; DEFW 3800H   ; Sprite Pattern Base
;
;    These five variables contain the 32x24 Text Mode VDP base
;addresses. Their values are set at power-up and thereafter only
;altered by the "BASE" statement.
;
GRPNAM: equ 0F3C7H  ; DEFW 1800H   ; Name Table Base
GRPCOL: equ 0F3C9H  ; DEFW 2000H   ; Colour Table Base
GRPCGP: equ 0F3CBH  ; DEFW 0000H   ; Character Pattern Base
GRPATR: equ 0F3CDH  ; DEFW 1B00H   ; Sprite Attribute Base
GRPPAT: equ 0F3CFH  ; DEFW 3800H   ; Sprite Pattern Base
;
;    These five variables contain the Graphics Mode VDP base
;addresses. Their values are set at power-up and thereafter only
;altered by the "BASE" statement.
;
MLTNAM: equ 0F3D1H  ; DEFW 0800H   ; Name Table Base
MLTCOL: equ 0F3D3H  ; DEFW 0000H   ; Colour Table Base
MLTCGP: equ 0F3D5H  ; DEFW 0000H   ; Character Pattern Base
MLTATR: equ 0F3D7H  ; DEFW 1B00H   ; Sprite Attribute Base
MLTPAT: equ 0F3D9H  ; DEFW 3800H   ; Sprite Pattern Base
;
;    These five variables contain the Multicolour Mode VDP base
;addresses. Their values are set at power-up and thereafter only
;altered by the "BASE" statement.
;
CLIKSW: equ 0F3DBH  ; DEFB 01H
;
;    This variable controls the interrupt handler key click:
;00H=Off, NZ=On. Its value is set at power-up and thereafter
;only altered by the "SCREEN" statement.
;
CSRY: equ 0F3DCH  ; DEFB 01H
;
;    This variable contains the row coordinate (from 1 to CTRCNT) of
;the text mode cursor.
;
CSRX: equ 0F3DDH  ; DEFB 01H
;
;    This variable contains the column coordinate (from 1 to LINLEN)
;of the text mode cursor. Note that the BIOS cursor coordinates
;for the home position are 1,1 whatever the screen width.
;
CNSDFG: equ 0F3DEH  ; DEFB FFH
;
;    This variable contains the current state of the function key
;display: 00H=Off, NZ=On.
;
RG0SAV: equ 0F3DFH  ; DEFB 00H
RG1SAV: equ 0F3E0H  ; DEFB F0H
RG2SAV: equ 0F3E1H  ; DEFB 00H
RG3SAV: equ 0F3E2H  ; DEFB 00H
RG4SAV: equ 0F3E3H  ; DEFB 01H
RG5SAV: equ 0F3E4H  ; DEFB 00H
RG6SAV: equ 0F3E5H  ; DEFB 00H
RG7SAV: equ 0F3E6H  ; DEFB F4H
;
;    These eight variables mimic the state of the eight write-only
;VDP Mode Registers. The values shown are for 40x24 Text Mode.
;
STATFL: equ 0F3E7H  ; DEFB CAH
;
;    This variable is continuously updated by the interrupt handler
;with the contents of the VDP Status Register.
;
TRGFLG: equ 0F3E8H  ; DEFB F1H
;
;    This variable is continuously updated by the interrupt handler
;with the state of the four joystick trigger inputs and the
;space key.
;
FORCLR: equ 0F3E9H  ; DEFB 0FH     ; White
;
;    This variable contains the current foreground colour. Its value
;is set at power-up and thereafter only altered by the "COLOR"
;statement. The foreground colour is used by the CLRSPR standard
;routine to set the sprite colour and by the CHGCLR standard
;routine to set the 1 pixel colour in the text modes. It also
;functions as the graphics ink colour as it is copied to ATRBYT
;by the GRPPRT standard routine and used throughout the
;Interpreter as the default value for any optional colour
;operand.
;
BAKCLR: equ 0F3EAH  ; DEFB 04H     ; Dark blue
;
;    This variable contains the current background colour. Its value
;is set at power-up and thereafter only altered by the "COLOR"
;statement. The background colour is used by the CLS standard
;routine to clear the screen in the graphics modes and by the
;CHGCLR standard routine to set the 0 pixel colour in the text
;modes.
;
BDRCLR: equ 0F3EBH  ; DEFB 04H     ; Dark blue
;
;    This variable contains the current border colour. Its value is
;set at power-up and thereafter only altered by the "COLOR"
;statement. The border colour is used by the CHGCLR standard
;routine in 32x24 Text Mode, Graphics Mode and Multicolour Mode
;to set the border colour.
;
MAXUPD: equ 0F3ECH ;       DEFB C3H
;           0F3EDH         DEFW 0000H
;
;    These two bytes are filled in by the "LINE" statement handler
;to form a Z80 JP to the RIGHTC, LEFTC, UPC or DOWNC standard
;routines.
;
MINUPD: equ 0F3EFH  ;      DEFB C3H
;           0F3F0H         DEFW 0000H
;
;    These two bytes are filled in by the "LINE" statement handler
;to form a Z80 JP to the RIGHTC, LEFTC, UPC or DOWNC standard
;routines.
;
ATRBYT: equ 0F3F2H  ; DEFB 0FH
;
;    This variable contains the graphics ink colour used by the SETC
;and NSETCX standard routines.
;
QUEUES: equ 0F3F3H  ; DEFW F959H
;
;    This variable contains the address of the control blocks for
;the three music queues. Its value is set at power-up and
;thereafter unaltered.
;
FRCNEW: equ 0F3F5H  ; DEFB FFH
;
;    This variable contains a flag to distinguish the two statements
;in the "CLOAD/CLOAD?" statement handler: 00H=CLOAD, FFH=CLOAD?.
;
SCNCNT: equ 0F3F6H  ; DEFB 01H
;
;    This variable is used as a counter by the interrupt handler to
;control the rate at which keyboard scans are performed.
;
REPCNT: equ 0F3F7H  ; DEFB 01H
;
;    This variable is used as a counter by the interrupt handler to
;control the key repeat rate.
;
PUTPNT: equ 0F3F8H  ; DEFW FBF0H
;
;    This variable contains the address of the put position in
;KEYBUF.
;
GETPNT: equ 0F3FAH  ; DEFW FBF0H
;
;    This variable contains the address of the get position in
;KEYBUF.
;
CS1200: equ 0F3FCH  ; DEFB 53H     ; LO cycle 1st half
;    F3FDH         DEFB 5CH     ; LO cycle 2nd half
;    F3FEH         DEFB 26H     ; HI cycle 1st half
;    F3FFH         DEFB 2DH     ; HI cycle 2nd half
;    F400H         DEFB 0FH     ; Header cycle count
;
;    These five variables contain the 1200 baud cassette parameters.
;Their values are set at power-up and thereafter unaltered.
;
CS2400: equ 0F401H  ; DEFB 25H     ; LO cycle 1st half
;    F402H         DEFB 2DH     ; LO cycle 2nd half
;    F403H         DEFB 0EH     ; HI cycle 1st half
;    F404H         DEFB 16H     ; HI cycle 2nd half
;    F405H         DEFB 1FH     ; Header cycle count
;
;    These five variables contain the 2400 baud cassette parameters.
;Their values are set at power-up and thereafter unaltered.
;
;LOW: equ 0F406H    ; DEFB 53H     ; LO cycle 1st half
;    F407H         DEFB 5CH     ; LO cycle 2nd half
;HIGH: equ 0F408H  ;  DEFB 26H     ; HI cycle 1st half
;    F409H         DEFB 2DH     ; HI cycle 2nd half
HEADER: equ 0F40AH  ; DEFB 0FH     ; Header cycle count
;
;    These five variables contain the current cassette parameters.
;Their values are set to 1200 baud at power-up and thereafter
;only altered by the "CSAVE" and "SCREEN" statements.
;
ASPCT1: equ 0F40BH  ; DEFW 0100H
;
;    This variable contains the reciprocal of the default "CIRCLE"
;aspect ratio multiplied by 256. Its value is set at power-up
;and thereafter unaltered.
;
ASPCT2: equ 0F40DH  ; DEFW 01C0H
;
;    This variable contains the default "CIRCLE" aspect ratio
;multiplied by 256. Its value is set at power-up and thereafter
;unaltered. The aspect ratio is present in two forms so that the
;"CIRCLE" statement handler can select the appropriate one
;immediately rather than needing to examine and possibly
;reciprocate it as is the case with an operand in the program
;text.
;
ENDPRG: equ 0F40FH  ; DEFB ":"
;           0F410H    DEFB 00H
;           0F411H    DEFB 00H
;           0FE12H    DEFB 00H
;           0F413H    DEFB 00H
;
;    These five bytes form a dummy program line. Their values are
;set at power-up and thereafter unaltered. The line exists in
;case an error occurs in the Interpreter Mainloop before any
;tokenized text is available in KBUF. If an "ON ERROR GOTO" is
;active at this time then it provides some text for the "RESUME"
;statement to terminate on.
;
ERRFLG: equ 0F414H  ; DEFB 00H
;
;    This variable is used by the Interpreter error handler to save
;the error number.
;
LPTPOS: equ 0F415H  ; DEFB 00H
;
;    This variable is used by the "LPRINT" statement handler to hold
;the current position of the printer head.
;
PRTFLG: equ 0F416H  ; DEFB 00H
;
;    This variable determines whether the OUTDO standard routine
;directs its output to the screen or to the printer: 00H=Screen,
;01H=Printer.
;
NTMSXP: equ 0F417H  ; DEFB 00H
;
;    This variable determines whether the OUTDO standard routine
;will replace headered graphics characters directed to the
;printer with spaces: 00H=Graphics, NZ=Spaces. Its value is set
;at power-up and thereafter only altered by the "SCREEN"
;statement.
;
RAWPRT: equ 0F418H  ; DEFB 00H
;
;    This variable determines whether the OUTDO standard routine
;will modify control and headered graphics characters directed
;to the printer: 00H=Modify, NZ=Raw. Its value is set at power-
;up and thereafter unaltered.
;
VLZADR: equ 0F419H  ; DEFW 0000H
VLZDAT: equ 0F41BH  ; DEFB 00H
;
;    These variables contain the address and value of any character
;temporarily removed by the "VAL" function.
;
CURLIN: equ 0F41CH  ; DEFW FFFFH
;
;    This variable contains the current Interpreter line number. A
;value of FFFFH denotes direct mode.
;
KBFMIN: equ 0F41EH  ; DEFB ":"
;
;    This byte provides a dummy prefix to the tokenized text
;contained in KBUF. Its function is similar to that of ENDPRG
;but is used for the situation where an error occurs within a
;direct statement.
;
KBUF: equ 0F41FH    ; DEFS 318
;
;    This buffer contains the tokenized form of the input line
;collected by the Interpreter Mainloop. When a direct statement
;is executed the contents of this buffer form the program text.
;
BUFMIN: equ 0F55DH  ; DEFB ","
;
;    This byte provides a dummy prefix to the text contained in BUF.
;It is used to synchronize the "INPUT" statement handler as it
;starts to analyze the input text.
;
BUF: equ 0F55EH     ; DEFS 259
;
;    This buffer contains the text collected from the console by the
;INLIN standard routine.
;
TTYPOS: equ 0F661H  ; DEFB 00H
;
;    This variable is used by the "PRINT" statement handler to hold
;the current screen position (Teletype!).
;
DIMFLG: equ 0F662H  ; DEFB 00H
;
;    This variable is normally zero but is set by the "DIM"
;statement handler to control the operation of the variable
;search routine.
;
VALTYP: equ 0F663H  ; DEFB 02H
;
;    This variable contains the type code of the operand currently
;contained in DAC: integer, 3=String, 4=Single Precision,
;8=Double Precision.
;
DORES: equ 0F664H  ; DEFB 00H
;
;    This variable is normally zero but is set to prevent the
;tokenization of unquoted keywords following a "DATA" token.
;
DONUM: equ 0F665H  ; DEFB 00H
;
;    This variable is normally zero but is set when a numeric
;constant follows one of the keywords GOTO, GOSUB, THEN, etc.,
;and must be tokenized to the special line number operand form.
;
CONTXT: equ 0F666H  ; DEFW 0000H
;
;    This variable is used by the CHRGTR standard routine to save
;the address of the character following a numeric constant in
;the program text.
;
CONSAV: equ 0F668H  ; DEFB 00H
;
;    This variable is used by the CHRGTR standard routine to save
;the token of a numeric constant found in the program text.
;
CONTYP: equ 0F669H  ; DEFB 00H
;
;    This variable is used by the CHRGTR standard routine to save
;the type of a numeric constant found in the program text.
;
CONLO: equ 0F66AH   ; DEFS 8
;
;    This buffer is used by the CHRGTR standard routine to save the
;value of a numeric constant found in the program text.
;
MEMSIZ: equ 0F672H  ; DEFW F168H
;
;    This variable contains the address of the top of the String
;Storage Area. Its value is set at power-up and thereafter only
;altered by the "CLEAR" and "MAXFILES" statements.
;
STKTOP: equ 0F674H  ; DEFW F0A0H
;
;    This variable contains the address of the top of the Z80 stack.
;Its value is set at power-up to MEMSIZ-200 and thereafter only
;altered by the "CLEAR" and "MAXFILES" statements.
;
TXTTAB: equ 0F676H  ; DEFW 8001H
;
;    This variable contains the address of the first byte of the
;Program Text Area. Its value is set at power-up and thereafter
;unaltered.
;
TEMPPT: equ 0F678H  ; DEFW F67AH
;
;    This variable contains the address of the next free location in
;TEMPST.
;
TEMPST: equ 0F67AH  ; DEFS 30
;
;    This buffer is used to store string descriptors. It functions
;as a stack with string producers pushing their results and
;string consumers popping them.
;
DSCTMP: equ 0F698H  ; DEFS 3
;
;    This buffer is used by the string functions to hold a result
;descriptor while it is being constructed.
;
FRETOP: equ 0F69BH  ; DEFW F168H
;
;    This variable contains the address of the next free location in
;the String Storage Area. When the area is empty FRETOP is equal
;to MEMSIZ.
;
TEMP3: equ 0F69DH  ; DEFW 0000H
;
;    This variable is used for temporary storage by various parts of
;the Interpreter.
;
TEMP8: equ 0F69FH  ; DEFW 0000H
;
;    This variable is used for temporary storage by various parts of
;the Interpreter.
;
ENDFOR: equ 0F6A1H  ; DEFW 0000H
;
;    This variable is used by the "FOR" statement handler to hold
;the end of statement address during construction of a parameter
;block.
;
DATLIN: equ 0F6A3H  ; DEFW 0000H
;
;    This variable contains the line number of the current "DATA"
;item in the program text.
;
SUBFLG: equ 0F6A5H  ; DEFB 00H
;
;    This variable is normally zero but is set by the "ERASE",
;"FOR", "FN" and "DEF FN" handlers to control the processing of
;subscripts by the variable search routine.
;
FLGINP: equ 0F6A6H  ; DEFB 00H
;
;    This variable contains a flag to distinguish the two statements
;in the "READ/INPUT" statement handler: 00H=INPUT, NZ=READ.
;
TEMP: equ 0F6A7H    ; DEFW 0000H
;
;    This variable is used for temporary storage by various parts of
;the Interpreter.
;
PTRFLG: equ 0F6A9H  ; DEFB 00H
;
;    This variable is normally zero but is set if any line number
;operands in the Program Text Area have been converted to
;pointers.
;
AUTFLG: equ 0F6AAH  ; DEFB 00H
;
;    This variable is normally zero but is set when "AUTO" mode is
;turned on.
;
AUTLIN: equ 0F6ABH  ; DEFW 0000H
;
;    This variable contains the current "AUTO" line number.
;
AUTINC: equ 0F6ADH  ; DEFW 0000H
;
;    This variable contains the current "AUTO" line number
;increment.
;
SAVTXT: equ 0F6AFH  ; DEFW 0000H
;
;    This variable is updated by the Runloop at the start of every
;statement with the current location in the program text. It is
;used during error recovery to set ERRTXT for the "RESUME"
;statement handler and OLDTXT for the "CONT" statement handler.
;
SAVSTK: equ 0F6B1H  ; DEFW F09EH
;
;    This variable is updated by the Runloop at the start of every
;statement with the current Z80 SP for error recovery purposes.
;
ERRLIN: equ 0F6B3H  ; DEFW 0000H
;
;    This variable is used by the error handler to hold the line
;number of the program line generating an error.
;
DOT: equ 0F6B5H  ; DEFW 0000H
;
;    This variable is updated by the Mainloop and the error handler
;with the current line number for use with the "." parameter.
;
ERRTXT: equ 0F6B7H  ; DEFW 0000H
;
;    This variable is updated from SAVTXT by the error handler for
;use by the "RESUME" statement handler.
;
ONELIN: equ 0F6B9H  ; DEFW 0000H
;
;    This variable is set by the "ON ERROR GOTO" statement handler
;with the address of the program line to execute when an error
;occurs.
;
ONEFLG: equ 0F6BBH  ; DEFB 00H
;
;    This variable is normally zero but is set by the error handler
;when control transfers to an "ON ERROR GOTO" statement. This is
;to prevent a loop developing if an error occurs inside the
;error recovery statements.
;
TEMP2: equ 0F6BCH   ; DEFW 0000H
;
;    This variable is used for temporary storage by various parts of
;the Interpreter.
;
OLDLIN: equ 0F6BEH  ; DEFW 0000H
;
;    This variable contains the line number of the terminating
;program line. It is set by the "END" and "STOP" statement
;handlers for use with the "CONT" statement.
;
OLDTXT: equ 0F6C0H  ; DEFW 0000H
;
;    This variable contains the address of the terminating program
;statement.
;
VARTAB: equ 0F6C2H  ; DEFW 8003H
;
;    This variable contains the address of the first byte of the
;Variable Storage Area.
;
ARYTAB: equ 0F6C4H  ; DEFW 8003H
;
;    This variable contains the address of the first byte of the
;Array Storage Area.
;
STREND: equ 0F6C6H  ; DEFW 8003H
;
;    This variable contains the address of the byte following the
;Array Storage Area.
;
DATPTR: equ 0F6C8H  ; DEFW 8000H
;
;    This variable contains the address of the current "DATA" item
;in the program text.
;
DEFTBL: equ 0F6CAH  ; DEFB 08H     ; A
;           0F6CBH    DEFB 08H     ; B
;           0F6CCH    DEFB 08H     ; C
;           0F6CDH    DEFB 08H     ; D
;           0F6CEH    DEFB 08H     ; E
;           0F6CFH    DEFB 08H     ; F
;           0F6D0H    DEFB 08H     ; G
;           0F6D1H    DEFB 08H     ; H
;           0F6D2H    DEFB 08H     ; I
;           0F6D3H    DEFB 08H     ; J
;           0F6D4H    DEFB 08H     ; K
;           0F6D5H    DEFB 08H     ; L
;           0F6D6H    DEFB 08H     ; M
;           0F6D7H    DEFB 08H     ; N
;           0F6D8H    DEFB 08H     ; O
;           0F6D9H    DEFB 08H     ; P
;           0F6DAH    DEFB 08H     ; Q
;           0F6DBH    DEFB 08H     ; R
;           0F6DCH    DEFB 08H     ; S
;           0F6DDH    DEFB 08H     ; T
;           0F6DEH    DEFB 08H     ; U
;           0F6DFH    DEFB 08H     ; V
;           0F6E0H    DEFB 08H     ; W
;           0F6E1H    DEFB 08H     ; X
;           0F6E2H    DEFB 08H     ; Y
;           0F6E3H    DEFB 08H     ; Z
;
;    These twenty-six variables contain the default type for each
;group of BASIC Variables. Their values are set to double
;precision at power-up, "NEW" and "CLEAR" and thereafter altered
;only by the "DEF" group of statements.
;
PRMSTK: equ 0F6E4H  ; DEFW 0000H
;
;    This variable contains the base address of the previous "FN"
;parameter block on the Z80 stack. It is used during string
;garbage collection to travel from block to block on the stack.
;
PRMLEN: equ 0F6E6H  ; DEFW 0000H
;
;    This variable contains the length of the current "FN" parameter
;block in PARM1.
;
PARM1: equ 0F6E8H  ; DEFS 100
;
;    This buffer contains the local Variables belonging to the "FN"
;function currently being evaluated.
;
PRMPRV: equ 0F74CH  ; DEFW F6E4H
;
;    This variable contains the address of the previous "FN"
;parameter block. It is actually a constant used to ensure that
;string garbage collection commences with the current parameter
;block before proceeding to those on the stack.
;
PRMLN2: equ 0F74EH  ; DEFW 0000H
;
;    This variable contains the length of the "FN" parameter block
;being constructed in PARM2
;
PARM2: equ 0F750H  ; DEFS 100
;
;    This buffer is used to construct the local Variables owned by
;the current "FN" function.
;
PRMFLG: equ 0F7B4H  ; DEFB 00H
;
;    This variable is used during a Variable search to indicate
;whether local or global Variables are being examined.
;
ARYTA2: equ 0F7B5H  ; DEFW 0000H
;
;    This variable is used during a Variable search to hold the
;termination address of the storage area being examined.
;
NOFUNS: equ 0F7B7H  ; DEFB 00H
;
;    This variable is normally zero but is set by the "FN" function
;handler to indicate to the variable search routine that local
;Variables are present.
;
TEMP9: equ 0F7B8H   ; DEFW 0000H
;
;    This variable is used for temporary storage by various parts of
;the Interpreter.
;
FUNACT: equ 0F7BAH  ; DEFW 0000H
;
;    This variable contains the number of currently active "FN
;functions.
;
SWPTMP: equ 0F7BCH  ; DEFS 8
;
;    This buffer is used to hold the first operand in a "SWAP"
;statement.
;
TRCFLG: equ 0F7C4H  ; DEFB 00H
;
;    This variable is normally zero but is set by the "TRON"
;statement handler to turn on the trace facility.
;
FBUFFR: equ 0F7C5H  ; DEFS 43
;
;    This buffer is used to hold the text produced during numeric
;output conversion.
;
DECTMP: equ 0F7F0H  ; DEFW 0000H
;
;    This variable is used for temporary storage by the double
;precision division routine.
;
DECTM2: equ 0F7F2H  ; DEFW 0000H
;
;    This variable is used for temporary storage by the double
;precision division routine.
;
DECCNT: equ 0F7F4H  ; DEFB 00H
;
;    This variable is used by the double precision division routine
;to hold the number of non-zero bytes in the mantissa of the
;second operand.
;
DAC: equ 0F7F6H     ; DEFS 16
;
;    This buffer functions as the Interpreter's primary accumulator
;during expression evaluation.
;
HOLD8: equ 0F806H   ; DEFS 65
;
;    This buffer is used by the double precision multiplication
;routine to hold the multiples of the first operand.
;
ARG: equ 0F847H     ; DEFS 16
;
;    This buffer functions as the Interpreter's secondary
;accumulator during expression evaluation.
;
RNDX: equ 0F857H    ; DEFS 8
;
;    This buffer contains the current double precision random
;number.
;
MAXFIL: equ 0F85FH  ; DEFB 01H
;
;    This variable contains the number of currently allocated user
;I/O buffers. Its value is set to 1 at power-up and thereafter
;only altered by the "MAXFILES" statement.
;
FILTAB: equ 0F860H  ; DEFW F16AH
;
;    This variable contains the address of the pointer table for the
;I/O buffer FCBs.
;
NULBUF: equ 0F862H  ; DEFW F177H
;
;    This variable contains the address of the first byte of the
;data buffer belonging to I/O buffer 0.
;
PTRFIL: equ 0F864H  ; DEFW 0000H
;
;    This variable contains the address of the currently active I/O
;buffer FCB.
;
FILNAM: equ 0F866H  ; DEFS 11
;
;    This buffer holds a user-specified filename. It is eleven
;characters long to allow for disc file specs such as
;"FILENAME.BAS".
;
FILNM2: equ 0F871H  ; DEFS 11
;
;    This buffer holds a filename read from an I/O device for
;comparison with the contents of FILNAM.
;
NLONLY: equ 0F87CH  ; DEFB 00H
;
;    This variable is normally zero but is set during a program
;"LOAD". Bit 0 is used to prevent I/O buffer 0 being closed
;during loading and bit 7 to prevent the user I/O buffers being
;closed if auto-run is required.
;
SAVEND: equ 0F87DH  ; DEFW 0000H
;
;    This variable is used by the "BSAVE" statement handler to hold
;the end address of the memory block to be saved.
;
FNKSTR: equ 0F87FH  ; DEFS 160
;
;    This buffer contains the ten sixteen-character function key
;strings. Their values are set at power-up and thereafter only
;altered by the "KEY" statement.
;
CGPNT: equ 0F91FH  ; DEFB 00H     ; Slot ID
;    F920H         DEFW 1BBFH   ; Address
;
;    These variables contain the location of the character set
;copied to the VDP by the INITXT and INIT32 standard routines.
;Their values are set to the MSX ROM character set at power-up
;and thereafter unaltered.
;
NAMBAS: equ 0F922H  ; DEFW 0000H
;
;    This variable contains the current text mode VDP Name Table
;base address. Its value is set from TXTNAM or T32NAM whenever
;the VDP is initialized to a text mode via the INITXT or INIT32
;standard routines.
;
CGPBAS: equ 0F924H  ; DEFW 0800H
;
;    This variable contains the current text mode VDP Character
;Pattern Table base address. Its value is set from TXTCGP or
;T32CGP whenever the VDP is initialized to a text mode via the
;INITXT or INIT32 standard routines.
;
PATBAS: equ 0F926H  ; DEFW 3800H
;
;    This variable contains the current VDP Sprite Pattern Table
;base address. Its value is set from T32PAT, GRPPAT or MLTPAT
;whenever the VDP is initialized via the INIT32, INIGRP or
;INIMLT standard routines.
;
ATRBAS: equ 0F928H  ; DEFW 1B00H
;
;    This variable contains the current VDP Sprite Attribute Table
;base address. Its value is set from T32ATR, GRPATR or MLTATR
;whenever the VDP is initialized via the INIT32, INIGRP or
;INIMLT standard routines.
;
CLOC:  equ 0F92AH ; DEFW 0000H   ; Pixel location
CMASK: equ 0F92CH  ; DEFB 80H     ; Pixel Mask
;
;    These variables contain the current pixel physical address used
;by the RIGHTC, LEFTC, UPC, TUPC, DOWNC, TDOWNC, FETCHC, STOREC,
;READC, SETC, NSETCX, SCANR and SCANL standard routines. CLOC
;holds the address of the byte containing the current pixel and
;CMASK defines the pixel within that byte.
;
MINDEL: equ 0F92DH  ; DEFW 0000H
;
;    This variable is used by the "LINE" statement handler to hold
;the minimum difference between the end points of the line.
;
MAXDEL: equ 0F92FH  ; DEFW 0000H
;
;    This variable is used by the "LINE" statement handler to hold
;the maximum difference between the end points of the line.
;
ASPECT: equ 0F931H  ; DEFW 0000H
;
;    This variable is used by the "CIRCLE" statement handler to hold
;the current aspect ratio. This is stored as a single byte
;binary fraction so an aspect ratio of 0.75 would become 00C0H.
;The MSB is only required if the aspect ratio is exactly 1.00,
;that is 0100H.
;
CENCNT: equ 0F933H  ; DEFW 0000H
;
;    This variable is used by the "CIRCLE" statement handler to hold
;the point count of the end angle.
;
CLINEF: equ 0F935H  ; DEFB 00H
;
;    This variable is used by the "CIRCLE" statement handler to hold
;the two line flags. Bit 0 is set if a line is required from the
;start angle to the centre and bit 7 set if one is required from
;the end angle.
;
CNPNTS: equ 0F936H  ; DEFW 0000H
;
;    This variable is used by the "CIRCLE" statement handler to hold
;the number of points within a forty-five degree segment.
;
CPLOTF: equ 0F938H  ; DEFB 00H
;
;    This variable is normally zero but is set by the "CIRCLE"
;statement handler if the end angle is smaller than the start
;angle. It is used to determine whether the pixels should be set
;"inside" the angles or "outside" them.
;
CPCNT: equ 0F939H   ; DEFW 0000H
;
;    This variable is used by the 'CIRCLE" statement handler to hold
;the point count within the current forty-five degree segment,
;this is in fact the Y coordinate.
;
CPCNT8: equ 0F93BH  ; DEFW 0000H
;
;    This variable is used by the "CIRCLE" statement handler to hold
;the total point count of the present position.
;
CRCSUM: equ 0F93DH  ; DEFW 0000H
;
;    This variable is used by the "CIRCLE" statement handler as the
;point computation counter.
;
CSTCNT: equ 0F93FH  ; DEFW 0000H
;
;    This variable is used by the "CIRCLE" statement handler to hold
;the point count of the start angle.
;
CSCLXY: equ 0F941H  ; DEFB 00H
;
;    This variable is used by the "CIRCLE" statement handler as a
;flag to determine in which direction the elliptic squash is to
;be applied: 00H=Y, 01H=X.
;
CSAVEA: equ 0F942H  ; DEFW 0000H
;
;    This variable is used for temporary storage by the SCANR
;standard routine.
;
CSAVEM: equ 0F944H  ; DEFB 00h
;
;    This variable is used for temporary storage by the SCANR
;standard routine.
;
CXOFF: equ 0F945H  ; DEFW 0000H
;
;    This variable is used for temporary storage by the "CIRCLE"
;statement handler.
;
CYOFF: equ 0F947H  ; DEFW 0000H
;
;    This variable is used for temporary storage by the "CIRCLE"
;statement handler.
;
LOHMSK: equ 0F949H  ; DEFB 00H
;
;    This variable is used by the "PAINT" statement handler to hold
;the leftmost position of a LH excursion.
;
LOHDIR: equ 0F94AH  ; DEFB 00H
;
;    This variable is used by the "PAINT" statement handler to hold
;the new paint direction required by a LH excursion.
;
LOHADR: equ 0F94BH  ; DEFW 0000H
;
;    This variable is used by the "PAINT" statement handler to hold
;the leftmost position of a LH excursion.
;
LOHCNT: equ 0F94DH  ; DEFW 0000H
;
;    This variable is used by the "PAINT" statement handler to hold
;the size of a LH excursion.
;
SKPCNT: equ 0F94FH  ; DEFW 0000H
;
;    This variable is used by the "PAINT" statement handler to hold
;the skip count returned by the SCANR standard routine.
;
MOVCNT: equ 0F951H  ; DEFW 0000H
;
;    This variable is used by the "PAINT" statement handler to hold
;the movement count returned by the SCANR standard routine.
;
PDIREC: equ 0F953H  ; DEFB 00H
;
;    This variable is used by the "PAINT" statement handler to hold
;the current paint direction: 40H=Down, C0H=Up, 00H=Terminate.
;
LFPROG: equ 0F954H  ; DEFB 00H
;
;    This variable is normally zero but is set by the "PAINT"
;statement handler if there has been any leftwards progress.
;
RTPROG: equ 0F955H  ; DEFB 00H
;
;    This variable is normally zero but is set by the "PAINT"
;statement handler if there has been any rightwards progress.
;
MCLTAB: equ 0F956H  ; DEFW 0000H
;
;    This variable contains the address of the command table to be
;used by the macro language parser. The "DRAW" table is at 5D83H
;and the "PLAY" table at 752EH.
;
MCLFLG: equ 0F958H  ; DEFB 00H
;
;    This variable is zero if the macro language parser is being
;used by the "DRAW", statement handler and non-zero if it is
;being used by "PLAY".
;
QUETAB: equ 0F959H  ; DEFB 00H     ; AQ Put position
;           0F95AH    DEFB 00H     ; AQ Get position
;           0F95BH    DEFB 00H     ; AQ Putback flag
;           0F95CH    DEFB 7FH     ; AQ Size
;           0F95DH    DEFW F975H   ; AQ Address
;                     
;           0F95FH    DEFB 00H     ; BQ Put position
;           0F960H    DEFB 00H     ; BQ Get position
;           0F961H    DEFB 00H     ; BQ Putback flag
;           0F962H    DEFB 7FH     ; BQ Size
;           0F963H    DEFW F9F5H   ; BQ Address
;                     
;           0F965H    DEFB 00H     ; CQ Put position
;           0F966H    DEFB 00H     ; CQ Get position
;           0F967H    DEFB 00H     ; CQ Putback flag
;           0F968H    DEFB 7FH     ; CQ Size
;           0F969H    DEFW FA75H   ; CQ Address
;                     
;           0F96BH    DEFB 00H     ; RQ Put position
;           0F96CH    DEFB 00H     ; RQ Get position
;           0F96DH    DEFB 00H     ; RQ Putback flag
;           0F96EH    DEFB 00H     ; RQ Size
;           0F96FH    DEFW 0000H   ; RQ Address
;
;    These twenty-four variables form the control blocks for the
;three music queues (VOICAQ, VOICBQ and VOICCQ) and the RS232
;queue. The three music control blocks are initialized by the
;GICINI standard routine and thereafter maintained by the
;interrupt handler and the PUTQ standard routine. The RS232
;control block is unused in the current MSX ROM.
;
QUEBAK: equ 0F971H  ; DEFB 00H     ; AQ Putback character
;           0F972H    DEFB 00H     ; BQ Putback character
;           0F973H    DEFB 00H     ; CQ Putback character
;           0F974H    DEFB 00H     ; RQ Putback character
;
;    These four variables are used to hold any unwanted character
;returned to the associated queue. Although the putback facility
;is implemented in the MSX ROM it is currently unused.
;
VOICAQ: equ 0F975H  ; DEFS 128     ; Voice A queue
VOICBQ: equ 0F9F5H  ; DEFS 128     ; Voice B queue
VOICCQ: equ 0FA75H  ; DEFS 128     ; Voice C queue
RS2IQ:  equ 0FAF5H  ; DEFS 64      ; RS232 queue
;
;    These four buffers contain the three music queues and the RS232
;queue, the latter is unused.
;
PRSCNT: equ 0FB35H  ; DEFB 00H
;
;    This variable is used by the "PLAY" statement handler to count
;the number of completed operand strings. Bit 7 is also set
;after each of the three operands has been parsed to prevent
;repeated activation of the STRTMS standard routine.
;
SAVSP: equ 0FB36H  ; DEFW 0000H
;
;    This variable is used by the "PLAY" statement handler to save
;the Z80 SP before control transfers to the macro language
;parser. Its value is compared with the SP on return to
;determine whether any data has been left on the stack because
;of a queue-full termination by the parser.
;
VOICEN: equ 0FB38H  ; DEFB 00H
;
;    This variable contains the current voice number being processed
;by the "PLAY" statement handler. The values 0, 1 and 2
;correspond to PSG channels A, B and C.
;
SAVVOL: equ 0FB39H  ; DEFW 0000H
;
;    This variable is used by the "PLAY" statement "R" command
;handler to save the current volume setting while a zero-
;amplitude rest is generated.
;
MCLLEN: equ 0FB3BH  ; DEFB 00H
;
;    This variable is used by the macro language parser to hold the
;length of the string operand being parsed.
;
MCLPTR: equ 0FB3CH  ; DEFW 0000H
;
;    This variable is used by the macro language parser to hold the
;address of the string operand being parsed.
;
QUEUEN: equ 0FB3EH  ; DEFB 00H
;
;    This variable is used by the interrupt handler to hold the
;number of the music queue currently being processed. The values
;0, 1 and 2 correspond to PSG channels A, B and C.
;
MUSICF: equ 0FB3FH  ; DEFB 00H
;
;    This variable contains three bit flags set by the STRTMS
;standard routine to initiate processing of a music queue by the
;interrupt handler. Bits 0, 1 and 2 correspond to VOICAQ, VOICBQ
;and VOICCQ.
;
PLYCNT: equ 0FB40H  ; DEFB 00H
;
;    This variable is used by the STRTMS standard routine to hold
;the number of "PLAY" statement sequences currently held in the
;music queues. It is examined when all three end of queue marks
;have been found for one sequence to determine whether
;dequeueing should be restarted.
;
VCBA: equ 0FB41H  ; DEFW 0000H   ; Duration counter
;         0FB43H    DEFB 00H     ; String length
;         0FB44H    DEFW 0000H   ; String address
;         0FB46H    DEFW 0000H   ; Stack data address
;         0FB48H    DEFB 00H     ; Music packet length
;         0FB49H    DEFS 7       ; Music packet
;         0FB50H    DEFB 04H     ; Octave
;         0FB51H    DEFB 04H     ; Length
;         0FB52H    DEFB 78H     ; Tempo
;         0FB53H    DEFB 88H     ; Volume
;         0FB54H    DEFW 00FFH   ; Envelope period
;         0FB56H    DEFS 16      ; Space for stack data
;
;    This thirty-seven byte buffer is used by the "PLAY" statement
;handler to hold the current parameters for voice A.
;
VCBB: equ 0FB66H  ; DEFS 37
;
;    This buffer is used by the "PLAY" statement handler to hold the
;current parameters for voice B, its structure is the same as
;VCBA.
;
VCBC: equ 0FB8BH  ; DEFS 37
;
;    This buffer is used by the "PLAY" statement handler to hold the
;current parameters for voice C, its structure is the same as
;VCBA.
;
ENSTOP: equ 0FBB0H  ; DEFB 00H
;
;    This variable determines whether the interrupt handler will
;execute a warm start to the Interpreter upon detecting the keys
;CODE, GRAPH, CTRL and SHIFT depressed together: 00H=Disable,
;NZ=Enable.
;
BASROM: equ 0FBB1H  ; DEFB 00H
;
;    This variable determines whether the ISCNTC and INLIN standard
;routines will respond to the CTRL-STOP key: 00H=Enable,
;NZ=Disable. It is used to prevent termination of a BASIC ROM
;located during the power-up ROM search.
;
LINTTB: equ 0FBB2H  ; DEFS 24
;
;    Each of these twenty-four variables is normally non-zero but is
;zeroed if the contents of the corresponding screen row have
;overflowed onto the next row. They are maintained by the BIOS
;but only actually used by the INLIN standard routine (the
;screen editor) to discriminate between logical and physical
;lines.
;
FSTPOS: equ 0FBCAH  ; DEFW 0000H
;
;    This variable is used to hold the cursor coordinates upon entry
;to the INLIN standard routine. Its function is to restrict the
;extent of backtracking performed when the text is collected
;from the screen at termination.
;
CURSAV: equ 0FBCCH  ; DEFB 00H
;
;    This variable is used to hold the screen character replaced by
;the text cursor.
;
FNKSWI: equ 0FBCDH  ; DEFB 00H
;
;    This variable is used by the CHSNS standard routine to
;determine whether the shifted or unshifted function keys are
;currently displayed: 00H=Shifted, 01H=Unshifted.
;
FNKFLG: equ 0FBCEH  ; DEFS 10
;
;    Each of these ten variables is normally zero but is set to 01H
;if the associated function key has been turned on by a "KEY(n)
;ON" statement. They are used by the interrupt handler to
;determine whether, in program mode only, it should return a
;character string or update the associated entry in TRPTBL.
;
ONGSBF: equ 0FBD8H  ; DEFB 00H
;
;    This variable is normally zero but is incremented by the
;interrupt handler whenever a device has achieved the conditions
;necessary to generate a program interrupt. It is used by the
;Runloop to determine whether any program interrupts are pending
;without having to search TRPTBL.
;
CLIKFL: equ 0FBD9H  ; DEFB 00H
;
;    This variable is used internally by the interrupt handler to
;prevent spurious key clicks when returning multiple characters
;from a single key depression such as a function key.
;
OLDKEY: equ 0FBDAH  ; DEFS 11
;
;    This buffer is used by the interrupt handler to hold the
;previous state of the keyboard matrix, each byte contains one
;row of keys starting with row 0.
;
NEWKEY: equ 0FBE5H  ; DEFS 11
;
;    This buffer is used by the interrupt handler to hold the
;current state of the keyboard matrix. Key transitions are
;detected by comparison with the contents of OLDKEY after which
;OLDKEY is updated with the current state.
;
KEYBUF: equ 0FBF0H  ; DEFS 40
;
;    This buffer contains the decoded keyboard characters produced
;by the interrupt handler. Note that the buffer is organized as
;a circular queue driven by GETPNT and PUTPNT and consequently
;has no fixed starting point.
;
LINWRK: equ 0FC18H  ; DEFS 40
;
;    This buffer is used by the BIOS to hold a complete line of
;screen characters.
;
PATWRK: equ 0FC40H  ; DEFS 8
;
;    This buffer is used by the BIOS to hold an 8x8 pixel pattern.
;
BOTTOM: equ 0FC48H  ; DEFW 8000H
;
;    This variable contains the address of the lowest RAM location
;used by the Interpreter. Its value is set at power-up and
;thereafter unaltered.
;
HIMEM: equ 0FC4AH  ; DEFW F380H
;
;    This variable contains the address of the byte following the
;highest RAM location used by the Interpreter. Its value is set
;at power-up and thereafter only altered by the "CLEAR"
;statement.
;
TRPTBL: equ 0FC4CH  ; DEFS 3       ; KEY 1
;           0FC4FH    DEFS 3       ; KEY 2
;           0FC52H    DEFS 3       ; KEY 3
;           0FC55H    DEFS 3       ; KEY 4
;           0FC58H    DEFS 3       ; KEY 5
;           0FC5BH    DEFS 3       ; KEY 6
;           0FC5EH    DEFS 3       ; KEY 7
;           0FC61H    DEFS 3       ; KEY 8
;           0FC64H    DEFS 3       ; KEY 9
;           0FC67H    DEFS 3       ; KEY 10
;           0FC6AH    DEFS 3       ; STOP
;           0FC6DH    DEFS 3       ; SPRITE
;           0FC70H    DEFS 3       ; STRIG 0
;           0FC73H    DEFS 3       ; STRIG 1
;           0FC76H    DEFS 3       ; STRIG 2
;           0FC79H    DEFS 3       ; STRIG 3
;           0FC7CH    DEFS 3       ; STRIG 4
;           0FC7FH    DEFS 3       ; INTERVAL
;           0FC82H    DEFS 3       ; Unused
;           0FC85H    DEFS 3       ; Unused
;           0FC88H    DEFS 3       ; Unused
;           0FC8BH    DEFS 3       ; Unused
;           0FC8EH    DEFS 3       ; Unused
;           0FC91H    DEFS 3       ; Unused
;           0FC94H    DEFS 3       ; Unused
;           0FC97H    DEFS 3       ; Unused
;
;    These twenty-six three byte variables hold the current state of
;the interrupt generating devices. The first byte of each entry
;contains the device status (bit 0=On, bit 1=Stop, bit 2=Event
;active) and is updated by the interrupt handler, the Runloop
;interrupt processor and the "DEVICE 0=ON/OFF/STOP" and "RETURN"
;statement handlers. The remaining two bytes of each entry are
;set by the "ON DEVICE GOSUB" statement handler and contain the
;address of the program line to execute upon a program
;interrupt.
;
RTYCNT: equ 0FC9AH  ; DEFB 00H
;
;    This variable is unused by the current MSX ROM.
;
INTFLG: equ 0FC9BH  ; DEFB 00H
;
;    This variable is normally zero but is set to 03H or 04H if the
;CTRL-STOP or STOP keys are detected by the interrupt handler.
;
PADY: equ 0FC9CH  ; DEFB 00H
;
;    This variable contains the Y coordinate of the last point
;detected by a touchpad.
;
PADX: equ 0FC9DH  ; DEFB 00H
;
;    This variable contains the X coordinate of the last point
;detected by a touchpad.
;
JIFFY: equ 0FC9EH  ; DEFW 0000H
;
;    This variable is continually incremented by the interrupt
;handler. Its value may be set or read by the "TIME" statement
;or function.
;
INTVAL: equ 0FCA0H  ; DEFW 0000H
;
;    This variable holds the interval duration set by the "ON
;INTERVAL" statement handler.
;
INTCNT: equ 0FCA2H  ; DEFW 0000H
;
;    This variable is continually decremented by the interrupt
;handler. When zero is reached its value is reset from INTVAL
;and, if applicable, a program interrupt generated. Note that
;this variable always counts irrespective of whether an
;"INTERVAL ON" statement is active.
;
LOWLIM: equ 0FCA4H  ; DEFB 31H
;
;    This variable is used to hold the minimum allowable start bit
;duration as determined by the TAPION standard routine.
;
WINWID: equ 0FCA5H  ; DEFB 22H
;
;    This variable is used to hold the LO/HI cycle discrimination
;duration as determined by the TAPION standard routine.
;
GRPHED: equ 0FCA6H  ; DEFB 00H
;
;    This variable is normally zero but is set to 01H by the CNVCHR
;standard routine upon detection of a graphic header code.
;
ESCCNT: equ 0FCA7H  ; DEFB 00H
;
;    This variable is used by the CHPUT standard routine ESC
;sequence processor to count escape parameters.
;
INSFLG: equ 0FCA8H  ; DEFB 00H
;
;    This variable is normally zero but is set to FFH by the INLIN
;standard routine when insert mode is on.
;
CSRSW: equ 0FCA9H  ; DEFB 00H
;
;    If this variable is zero the cursor is only displayed while the
;CHGET standard routine is waiting for a keyboard character. If
;it is non-zero the cursor is permanently displayed via the
;CHPUT standard routine.
;
CSTYLE: equ 0FCAAH  ; DEFB 00H
;
;    This variable determines the cursor style: 00H=Block,
;NZ=Underline.
;
CAPST: equ 0FCABH  ; DEFB 00H
;
;    This variable is used by   the interrupt handler to hold the
;current caps lock status: 00H=Off, NZ=On.
;
KANAST: equ 0FCACH  ; DEFB 00H
;
;    This variable is used to hold the keyboard Kana lock status on
;Japanese machines and the DEAD key status on European machines.
;
KANAMD: equ 0FCADH  ; DEFB 00H
;
;    This variable holds a keyboard mode on Japanese machines only.
;
FLBMEM: equ 0FCAEH  ; DEFB 00H
;
;    This variable is set by the file I/O error generators but is
;otherwise unused.
;
SCRMOD: equ 0FCAFH  ; DEFB 00H
;
;    This variable contains the current screen mode: 0=40x24 Text
;Mode, 1=32x24 Text Mode, 2=Graphics Mode, 3=Multicolour Mode.
;
OLDSCR: equ 0FCB0H  ; DEFB 00H
;
;    This variable holds the screen mode of the last text mode set.
;
CASPRV: equ 0FCB1H  ; DEFB 00H
;
;    This variable is used to hold any character returned to an I/O
;buffer by the cassette putback function.
;
BDRATR: equ 0FCB2H  ; DEFB 00H
;
;    This variable contains the boundary colour for the "PAINT"
;statement handler. Its value is set by the PNTINI standard
;routine and used by the SCANR and SCANL standard routines.
;
GXPOS: equ 0FCB3H  ; DEFW 0000H
;
;    This variable is used for temporary storage of a graphics X
;coordinate.
;
GYPOS: equ 0FCB5H  ; DEFW 0000H
;
;    This variable is used for temporary storage of a graphics Y
;coordinate.
;
GRPACX: equ 0FCB7H  ; DEFW 0000H
;
;    This variable contains the current graphics X coordinate for
;the GRPPRT standard routine.
;
GRPACY: equ 0FCB9H  ; DEFW 0000H
;
;    This variable contains the current graphics Y coordinate for
;the GRPPRT standard routine.
;
DRWFLG: equ 0FCBBH  ; DEFB 00H
;
;    Bits 6 and 7 of this variable are set by the "DRAW" statement
;"N" and "B" command handlers to turn the associated mode on.
;
DRWSCL: equ 0FCBCH  ; DEFB 00H
;
;    This variable is used by the "DRAW" statement "S" command
;handler to hold the current scale factor.
;
DRWANG: equ 0FCBDH  ; DEFB 00H
;
;    This variable is used by the "DRAW" statement "A" command
;handler to hold the current angle.
;
RUNBNF: equ 0FCBEH  ; DEFB 00H
;
;    This variable is normally zero but is set by the "BLOAD"
;statement handler when an auto-run "R" parameter is specified.
;
SAVENT: equ 0FCBFH  ; DEFW 0000H
;
;    This variable contains the "BSAVE" and "BLOAD" entry address.
;
EXPTBL: equ 0FCC1H  ; DEFB 00H     ; Primary Slot 0
;           0FCC2H    DEFB 00H     ; Primary Slot 1
;           0FCC3H    DEFB 00H     ; Primary Slot 2
;           0FCC4H    DEFB 00H     ; Primary Slot 3
;
;    Each of these four variables is normally zero but is set to 80H
;during the power-up RAM search if the associated Primary Slot
;is found to be expanded.
;
SLTTBL:  equ 0FCC5H ; DEFB 00H     ; Primary Slot 0
;            0FCC6H   DEFB 00H     ; Primary Slot 1
;            0FCC7H   DEFB 00H     ; Primary Slot 2
;            0FCC8H   DEFB 00H     ; Primary Slot 3
;
;    These four variables duplicate the contents of the four
;possible Secondary Slot Registers. The contents of each
;variable should only be regarded as valid if EXPTBL shows the
;associated Primary Slot to be expanded.
;
SLTATR: equ 0FCC9H  ; DEFS 4       ; PS0, SS0
;           0FCCDH    DEFS 4       ; PS0, SS1
;           0FCD1H    DEFS 4       ; PS0, SS2
;           0FCD5H    DEFS 4       ; PS0, SS3
;           
;           0FCD9H    DEFS 4       ; PS1, SS0
;           0FCDDH    DEFS 4       ; PS1, SS1
;           0FCE1H    DEFS 4       ; PS1, SS2
;           0FCE5H    DEFS 4       ; PS1, SS3
;           
;           0FCE9H    DEFS 4       ; PS2, SS0
;           0FCEDH    DEFS 4       ; PS2, SS1
;           0FCF1H    DEFS 4       ; PS2, SS2
;           0FCF5H    DEFS 4       ; PS2, SS3
;           
;           0FCF9H    DEFS 4       ; PS3, SS0
;           0FCFDH    DEFS 4       ; PS3, SS1
;           0FD01H    DEFS 4       ; PS3, SS2
;           0FD05H    DEFS 4       ; PS3, SS3
;
;    These sixty-four variables contain the attributes of any
;extension ROMs found during the power-up ROM search. The
;characteristics of each 16 KB ROM are encoded into a single byte
;so four bytes are required for each possible slot. The encoding
;is:
;
;
;        Bit 7 set=BASIC program
;        Bit 6 set=Device handler
;        Bit 5 set=Statement handler
;
;
;    Note that the entries for page 0 (0000H to 3FFFH) and page 3
;(C000H to FFFFH) will always be zero as only page 1 (4000H to
;7FFFH) and page 2 (8000H to BFFFH) are actually examined. The
;MSX convention is that machine code extension ROMs are placed
;in page 1 and BASIC program ROMs in page 2.
;
SLTWRK: equ 0FD09H  ; DEFS 128
;
;    This buffer provides two bytes of local workspace for each of
;the sixty-four possible extension ROMs.
;
PROCNM: equ 0FD89H  ; DEFS 16
;
;    This buffer is used to hold a device or statement name for
;examination by an extension ROM.
;
DEVICE: equ 0FD99H ;  DEFB 00H
;
;    This variable is used to pass a device code, from 0 to 3, to an
;extension ROM.
;
;
 ENDIF