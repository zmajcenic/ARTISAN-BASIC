;+------------------------------------------------------------------+ 
;|                         MSX Bios CALLS                           |
;+------------------------------------------------------------------+ 
;|                                                                  |
;|  Taken from MSX RED BOOK  (c) 1985 by Avalon Software            |
;|                           (c) 1988 by McGraw Hill                |
;|                                                                  |
;|  Formatted as Z80 Assembly include by Daniel Jose Viana, 2014    |
;|                                                                  |
;+------------------------------------------------------------------+
;
; Terminology
; 
;
;     Reference is frequently made in this chapter to the standard
; routines and to Workspace Area variables. Whenever this is done
; the Microsoft-recommended name is used in upper case letters,
; for example "the FILVRM standard routine" and "SCRMOD is set".
; Subroutines which are not named are referred to by a
; parenthesized address, "the screen is cleared (0777H)" for
; example. When reference is made to the Z80 status flags
; assembly language conventions are used, for example "Flag C"
; would mean that the carry flag is set while "Flag NZ" means
; that the zero flag is reset. The terms "EI" and "DI" mean
; enabled interrupts and disabled interrupts respectively.

 IFNDEF BIOS_CALLS
 DEFINE BIOS_CALLS

;NAME    ADDR.      CALL TO  FUNCTION
;-------------------------------------------------------------
CHKRAM equ 0000H ; 02D7H   Power-up, check RAM
SYNCHR equ 0008H ; 2683H   Check BASIC program character
RDSLT  equ 000CH ; 01B6H   Read RAM in any slot
CHRGTR equ 0010H ; 2686H   Get next BASIC program character
WRSLT  equ 0014H ; 01D1H   Write to RAM in any slot
OUTDO  equ 0018H ; 1B45H   Output to current device
CALSLT equ 001CH ; 0217H   Call routine in any slot
DCOMPR equ 0020H ; 146AH   Compare register pairs HL and DE
ENASLT equ 0024H ; 025EH   Enable any slot permanently
GETYPR equ 0028H ; 2689H   Get BASIC operand type
CALLF  equ 0030H ; 0205H   Call routine in any slot
KEYINT equ 0038H ; 0C3CH   Interrupt handler, keyboard scan
INITIO equ 003BH ; 049DH   Initialize I/O devices
INIFNK equ 003EH ; 139DH   Initialize function key strings
DISSCR equ 0041H ; 0577H   Disable screen
ENASCR equ 0044H ; 0570H   Enable screen
WRTVDP equ 0047H ; 057FH   Write to any VDP register
RDVRM  equ 004AH ; 07D7H   Read byte from VRAM
WRTVRM equ 004DH ; 07CDH   Write byte to VRAM
SETRD  equ 0050H ; 07ECH   Set up VDP for read
SETWRT equ 0053H ; 07DFH   Set up VDP for write
FILVRM equ 0056H ; 0815H   Fill block of VRAM with data byte
LDIRMV equ 0059H ; 070FH   Copy block to memory from VRAM
LDIRVM equ 005CH ; 0744H   Copy block to VRAM, from memory
CHGMOD equ 005FH ; 084FH   Change VDP mode
CHGCLR equ 0062H ; 07F7H   Change VDP colours
NMI    equ 0066H ; 1398H   Non Maskable Interrupt handler
CLRSPR equ 0069H ; 06A8H   Clear all sprites
INITXT equ 006CH ; 050EH   Initialize VDP to 40x24 Text Mode
INIT32 equ 006FH ; 0538H   Initialize VDP to 32x24 Text Mode
INIGRP equ 0072H ; 05D2H   Initialize VDP to Graphics Mode
INIMLT equ 0075H ; 061FH   Initialize VDP to Multicolour Mode
SETTXT equ 0078H ; 0594H   Set VDP to 40x24 Text Mode
SETT32 equ 007BH ; 05B4H   Set VDP to 32x24 Text Mode
SETGRP equ 007EH ; 0602H   Set VDP to Graphics Mode
SETMLT equ 0081H ; 0659H   Set VDP to Multicolour Mode
CALPAT equ 0084H ; 06E4H   Calculate address of sprite pattern
CALATR equ 0087H ; 06F9H   Calculate address of sprite attribute
GSPSIZ equ 008AH ; 0704H   Get sprite size
GRPPRT equ 008DH ; 1510H   Print character on graphic screen
GICINI equ 0090H ; 04BDH   Initialize PSG (GI Chip)
WRTPSG equ 0093H ; 1102H   Write to any PSG register
RDPSG  equ 0096H ; 110EH   Read from any PSG register
STRTMS equ 0099H ; 11C4H   Start music dequeueing
CHSNS  equ 009CH ; 0D6AH   Sense keyboard buffer for character
CHGET  equ 009FH ; 10CBH   Get character from keyboard buffer (wait)
CHPUT  equ 00A2H ; 08BCH   Screen character output
LPTOUT equ 00A5H ; 085DH   Line printer character output
LPTSTT equ 00A8H ; 0884H   Line printer status test
CNVCHR equ 00ABH ; 089DH   Convert character with graphic header
PINLIN equ 00AEH ; 23BFH   Get line from console (editor)
INLIN  equ 00B1H ; 23D5H   Get line from console (editor)
QINLIN equ 00B4H ; 23CCH   Display "?", get line from console (editor)
BREAKX equ 00B7H ; 046FH   Check CTRL-STOP key directly
ISCNTC equ 00BAH ; 03FBH   Check CRTL-STOP key
CKCNTC equ 00BDH ; 10F9H   Check CTRL-STOP key
BEEP   equ 00C0H ; 1113H   Go beep
CLS    equ 00C3H ; 0848H   Clear screen
POSIT  equ 00C6H ; 088EH   Set cursor position
FNKSB  equ 00C9H ; 0B26H   Check if function key display on
ERAFNK equ 00CCH ; 0B15H   Erase function key display
DSPFNK equ 00CFH ; 0B2BH   Display function keys
TOTEXT equ 00D2H ; 083BH   Return VDP to text mode
GTSTCK equ 00D5H ; 11EEH   Get joystick status
GTTRIG equ 00D8H ; 1253H   Get trigger status
GTPAD  equ 00DBH ; 12ACH   Get touch pad status
GTPDL  equ 00DEH ; 1273H   Get paddle status
TAPION equ 00E1H ; 1A63H   Tape input ON
TAPIN  equ 00E4H ; 1ABCH   Tape input
TAPIOF equ 00E7H ; 19E9H   Tape input OFF
TAPOON equ 00EAH ; 19F1H   Tape output ON
TAPOUT equ 00EDH ; 1A19H   Tape output
TAPOOF equ 00F0H ; 19DDH   Tape output OFF
STMOTR equ 00F3H ; 1384H   Turn motor ON/OFF
LFTQ   equ 00F6H ; 14EBH   Space left in music queue
PUTQ   equ 00F9H ; 1492H   Put byte in music queue
RIGHTC equ 00FCH ; 16C5H   Move current pixel physical address right
LEFTC  equ 00FFH ; 16EEH   Move current pixel physical address left
UPC    equ 0102H ; 175DH   Move current pixel physical address up
TUPC   equ 0105H ; 173CH   Test then UPC if legal
DOWNC  equ 0108H ; 172AH   Move current pixel physical address down
TDOWNC equ 010BH ; 170AH   Test then DOWNC if legal
SCALXY equ 010EH ; 1599H   Scale graphics coordinates
MAPXYC equ 0111H ; 15DFH   Map graphic coordinates to physical address
FETCHC equ 0114H ; 1639H   Fetch current pixel physical address
STOREC equ 0117H ; 1640H   Store current pixel physical address
SETATR equ 011AH ; 1676H   Set attribute byte
READC  equ 011DH ; 1647H   Read attribute of current pixel
SETC   equ 0120H ; 167EH   Set attribute of current pixel
NSETCX equ 0123H ; 1809H   Set attribute of number of pixels
GTASPC equ 0126H ; 18C7H   Get aspect ratio
PNTINI equ 0129H ; 18CFH   Paint initialize
SCANR  equ 012CH ; 18E4H   Scan pixels to right
SCANL  equ 012FH ; 197AH   Scan pixels to left
CHGCAP equ 0132H ; 0F3DH   Change Caps Lock LED
CHGSND equ 0135H ; 0F7AH   Change Key Click sound output
RSLREG equ 0138H ; 144CH   Read Primary Slot Register
WSLREG equ 013BH ; 144FH   Write to Primary Slot Register
RDVDP  equ 013EH ; 1449H   Read VDP Status Register
SNSMAT equ 0141H ; 1452H   Read row of keyboard matrix
PHYDIO equ 0144H ; 148AH   Disk, no action
FORMAT equ 0147H ; 148EH   Disk, no action
ISFLIO equ 014AH ; 145FH   Check for file I/O
OUTDLP equ 014DH ; 1B63H   Formatted output to line printer
GETVCP equ 0150H ; 1470H   Get music voice pointer
GETVC2 equ 0153H ; 1474H   Get music voice pointer
KILBUF equ 0156H ; 0468H   Clear keyboard buffer
CALBAS equ 0159H ; 01FFH   Call to BASIC from any slot

 ENDIF