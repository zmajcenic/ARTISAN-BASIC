;+------------------------------------------------------------------+ 
;|                         MSX System Hooks                         |
;+------------------------------------------------------------------+ 
;|                                                                  |
;|  Taken from MSX RED BOOK  (c) 1985 by Avalon Software            |
;|                           (c) 1988 by McGraw Hill                |
;|                                                                  |
;|  Formatted as Z80 Assembly include by Daniel Jose Viana, 2014    |
;|                                                                  |
;+------------------------------------------------------------------+
;
;    The section of the Workspace Area from FD9AH to FFC9H
;contains one hundred and twelve hooks, each of which is filled
;with five Z80 RET opcodes at power-up. These are called from
;strategic locations within the BIOS/Interpreter so that the ROM
;can be extended, particularly so that it can be upgraded to
;Disk BASIC. Each hook has sufficient room to hold a far call to
;any slot:
;
;        RST 30H
;        DEFB Slot ID
;        DEFW Address
;        RET
;
;    The hooks are listed on the following pages together with the
;address they are called from and a brief note as to their
;function.
;

 IFNDEF SYSTEM_HOOKS
 DEFINE SYSTEM_HOOKS

HKEYI: equ 0FD9AH  ; DEFS 5   -> 0C4AH Interrupt handler
HTIMI: equ 0FD9FH  ; DEFS 5   -> 0C53H Interrupt handler
HCHPU: equ 0FDA4H  ; DEFS 5   -> 08C0H CHPUT standard routine
HDSPC: equ 0FDA9H  ; DEFS 5   -> 09E6H Display cursor
HERAC: equ 0FDAEH  ; DEFS 5   -> 0A33H Erase cursor
HDSPF: equ 0FDB3H  ; DEFS 5   -> 0B2BH DSPFNK standard routine
HERAF: equ 0FDB8H  ; DEFS 5   -> 0B15H ERAFNK standard routine
HTOTE: equ 0FDBDH  ; DEFS 5   -> 0842H TOTEXT standard routine
HCHGE: equ 0FDC2H  ; DEFS 5   -> 10CEH CHGET standard routine
HINIP: equ 0FDC7H  ; DEFS 5   -> 071EH Copy character set to VDP
HKEYC: equ 0FDCCH  ; DEFS 5   -> 1025H Keyboard decoder
HKYEA: equ 0FDD1H  ; DEFS 5   -> 0F10H Keyboard decoder
HNMI:  equ 0FDD6H  ; DEFS 5   -> 1398H NMI standard routine
HPINL: equ 0FDDBH  ; DEFS 5   -> 23BFH PINLIN standard routine
HQINL: equ 0FDE0H  ; DEFS 5   -> 23CCH QINLIN standard routine
HINLI: equ 0FDE5H  ; DEFS 5   -> 23D5H INLIN standard routine
HONGO: equ 0FDEAH  ; DEFS 5   -> 7810H "ON DEVICE GOSUB"
HDSKO: equ 0FDEFH  ; DEFS 5   -> 7C16H "DSKO$"
HSETS: equ 0FDF4H  ; DEFS 5   -> 7C1BH "SET"
HNAME: equ 0FDF9H  ; DEFS 5   -> 7C20H "NAME"
HKILL: equ 0FDFEH  ; DEFS 5   -> 7C25H "KILL"
HIPL:  equ 0FE03H  ; DEFS 5   -> 7C2AH "IPL"
HCOPY: equ 0FE08H  ; DEFS 5   -> 7C2FH "COPY"
HCMD:  equ 0FE0DH  ; DEFS 5   -> 7C34H "CMD"
HDSKF: equ 0FE12H  ; DEFS 5   -> 7C39H "DSKF"
HDSKI: equ 0FE17H  ; DEFS 5   -> 7C3EH "DSKI$"
HATTR: equ 0FE1CH  ; DEFS 5   -> 7C43H "ATTR$"
HLSET: equ 0FE21H  ; DEFS 5   -> 7C48H "LSET"
HRSET: equ 0FE26H  ; DEFS 5   -> 7C4DH "RSET"
HFIEL: equ 0FE2BH  ; DEFS 5   -> 7C52H "FIELD"
HMKIS: equ 0FE30H  ; DEFS 5   -> 7C57H "MKI$"
HMKSS: equ 0FE35H  ; DEFS 5   -> 7C5CH "MKS$"
HMKDS: equ 0FE3AH  ; DEFS 5   -> 7C61H "MKD$"
HCVI:  equ 0FE3FH  ; DEFS 5   -> 7C66H "CVI"
HCVS:  equ 0FE44H  ; DEFS 5   -> 7C6BH "CVS"
HCVD:  equ 0FE49H  ; DEFS 5   -> 7C70H "CVD"
HGETP: equ 0FE4EH  ; DEFS 5   -> 6A93H Locate FCB
HSETF: equ 0FE53H  ; DEFS 5   -> 6AB3H Locate FCB
HNOFO: equ 0FE58H  ; DEFS 5   -> 6AF6H "OPEN"
HNULO: equ 0FE5DH  ; DEFS 5   -> 6B0FH "OPEN"
HNTFL: equ 0FE62H  ; DEFS 5   -> 6B3BH Close I/O buffer 0
HMERG: equ 0FE67H  ; DEFS 5   -> 6B63H "MERGE/LOAD"
HSAVE: equ 0FE6CH  ; DEFS 5   -> 6BA6H "SAVE"
HBINS: equ 0FE71H  ; DEFS 5   -> 6BCEH "SAVE"
HBINL: equ 0FE76H  ; DEFS 5   -> 6BD4H "MERGE/LOAD"
HFILE: equ 0FE7BH  ; DEFS 5   -> 6C2FH "FILES"
HDGET: equ 0FE80H  ; DEFS 5   -> 6C3BH "GET/PUT"
HFILO: equ 0FE85H  ; DEFS 5   -> 6C51H Sequential output
HINDS: equ 0FE8AH  ; DEFS 5   -> 6C79H Sequential input
HRSLF: equ 0FE8FH  ; DEFS 5   -> 6CD8H "INPUT$"
HSAVD: equ 0FE94H  ; DEFS 5   -> 6D03H "LOC", 6D14H "LOF",
	 	  		 ;          -> 6D25H "EOF", 6D39H "FPOS"
HLOC:  equ 0FE99H  ; DEFS 5   -> 6D0FH "LOC"
HLOF:  equ 0FE9EH  ; DEFS 5   -> 6D20H "LOF"
HEOF:  equ 0FEA3H  ; DEFS 5   -> 6D33H "EOF"
HFPOS: equ 0FEA8H  ; DEFS 5   -> 6D43H "FPOS"
HBAKU: equ 0FEADH  ; DEFS 5   -> 6E36H "LINE INPUT#"
HPARD: equ 0FEB2H  ; DEFS 5   -> 6F15H Parse device name
HNODE: equ 0FEB7H  ; DEFS 5   -> 6F33H Parse device name
HPOSD: equ 0FEBCH  ; DEFS 5   -> 6F37H Parse device name
HDEVN: equ 0FEC1H  ; DEFS 5   -> This hook is not used.
HGEND: equ 0FEC6H  ; DEFS 5   -> 6F8FH I/O function dispatcher
HRUNC: equ 0FECBH  ; DEFS 5   -> 629AH Run-clear
HCLEA: equ 0FED0H  ; DEFS 5   -> 62A1H Run-clear
HLOPD: equ 0FED5H  ; DEFS 5   -> 62AFH Run-clear
HSTKE: equ 0FEDAH  ; DEFS 5   -> 62F0H Reset stack
HISFL: equ 0FEDFH  ; DEFS 5   -> 145FH ISFLIO standard routine
HOUTD: equ 0FEE4H  ; DEFS 5   -> 1B46H OUTDO standard routine
HCRDO: equ 0FEE9H  ; DEFS 5   -> 7328H CR,LF to OUTDO
HDSKC: equ 0FEEEH  ; DEFS 5   -> 7374H Mainloop line input
HDOGR: equ 0FEF3H  ; DEFS 5   -> 593CH Line draw
HPRGE: equ 0FEF8H  ; DEFS 5   -> 4039H Program end
HERRP: equ 0FEFDH  ; DEFS 5   -> 40DCH Error handler
HERRF: equ 0FF02H  ; DEFS 5   -> 40FDH Error handler
HREAD: equ 0FF07H  ; DEFS 5   -> 4128H Mainloop "OK"
HMAIN: equ 0FF0CH  ; DEFS 5   -> 4134H Mainloop
HDIRD: equ 0FF11H  ; DEFS 5   -> 41A8H Mainloop direct statement
HFINI: equ 0FF16H  ; DEFS 5   -> 4237H Mainloop finished
HFINE: equ 0FF1BH  ; DEFS 5   -> 4247H Mainloop finished
HCRUN: equ 0FF20H  ; DEFS 5   -> 42B9H Tokenize
HCRUS: equ 0FF25H  ; DEFS 5   -> 4353H Tokenize
HISRE: equ 0FF2AH  ; DEFS 5   -> 437CH Tokenize
HNTFN: equ 0FF2FH  ; DEFS 5   -> 43A4H Tokenize
HNOTR: equ 0FF34H  ; DEFS 5   -> 44EBH Tokenize
HSNGF: equ 0FF39H  ; DEFS 5   -> 45D1H "FOR"
HNEWS: equ 0FF3EH  ; DEFS 5   -> 4601H Runloop new statement
HGONE: equ 0FF43H  ; DEFS 5   -> 4646H Runloop execute
HCHRG: equ 0FF48H  ; DEFS 5   -> 4666H CHRGTR standard routine
HRETU: equ 0FF4DH  ; DEFS 5   -> 4821H "RETURN"
HPRTF: equ 0FF52H  ; DEFS 5   -> 4A5EH "PRINT"
HCOMP: equ 0FF57H  ; DEFS 5   -> 4A54H "PRINT"
HFINP: equ 0FF5CH  ; DEFS 5   -> 4AFFH "PRINT"
HTRMN: equ 0FF61H  ; DEFS 5   -> 4B4DH "READ/INPUT" error
HFRME: equ 0FF66H  ; DEFS 5   -> 4C6DH Expression Evaluator
HNTPL: equ 0FF6BH  ; DEFS 5   -> 4CA6H Expression Evaluator
HEVAL: equ 0FF70H  ; DEFS 5   -> 4DD9H Factor Evaluator
HOKNO: equ 0FF75H  ; DEFS 5   -> 4F2CH Factor Evaluator
HFING: equ 0FF7AH  ; DEFS 5   -> 4F3EH Factor Evaluator
HISMI: equ 0FF7FH  ; DEFS 5   -> 51C3H Runloop execute
HWIDT: equ 0FF84H  ; DEFS 5   -> 51CCH "WIDTH"
HLIST: equ 0FF89H  ; DEFS 5   -> 522EH "LIST"
HBUFL: equ 0FF8EH  ; DEFS 5   -> 532DH Detokenize
HFRQI: equ 0FF93H  ; DEFS 5   -> 543FH Convert to integer
HSCNE: equ 0FF98H  ; DEFS 5   -> 5514H Line number to pointer
HFRET: equ 0FF9DH  ; DEFS 5   -> 67EEH Free descriptor
HPTRG: equ 0FFA2H  ; DEFS 5   -> 5EA9H Variable search
HPHYD: equ 0FFA7H  ; DEFS 5   -> 148AH PHYDIO standard routine
HFORM: equ 0FFACH  ; DEFS 5   -> 148EH FORMAT standard routine
HERRO: equ 0FFB1H  ; DEFS 5   -> 406FH Error handler
HLPTO: equ 0FFB6H  ; DEFS 5   -> 085DH LPTOUT standard routine
HLPTS: equ 0FFBBH  ; DEFS 5   -> 0884H LPTSTT standard routine
HSCRE: equ 0FFC0H  ; DEFS 5   -> 79CCH "SCREEN"
HPLAY: equ 0FFC5H  ; DEFS 5   -> 73E5H "PLAY" statement
;
 ENDIF