; ------------------------------------------------------------------------
; Compiled by MSX-BACON from DEMO2USR.ASC
; ------------------------------------------------------------------------
; 
WORK_H_TIMI                     = 0X0FD9F
WORK_H_ERRO                     = 0X0FFB1
WORK_HIMEM                      = 0X0FC4A
WORK_MAXFIL                     = 0X0F85F
WORK_TXTTAB                     = 0X0F676
WORK_VARTAB                     = 0X0F6C2
WORK_USRTAB                     = 0X0F39A
BIOS_NEWSTT                     = 0X04601
FILE_INFO_SIZE                  = 37 + 3 * 16
BLIB_INIT_NCALBAS               = 0X0404E
BIOS_SYNTAX_ERROR               = 0X4055
BIOS_CALSLT                     = 0X001C
BIOS_ENASLT                     = 0X0024
WORK_MAINROM                    = 0XFCC1
WORK_BLIBSLOT                   = 0XF3D3
SIGNATURE                       = 0X4010
BIOS_ERRHAND_REDIM              = 0X0405E
BIOS_IMULT                      = 0X03193
BIOS_ERRHAND                    = 0X0406F
BIOS_UMULT                      = 0X0314A
WORK_VALTYP                     = 0X0F663
WORK_DAC                        = 0X0F7F6
BIOS_FRCINT                     = 0X02F8A
BIOS_CHGCLR                     = 0X00062
WORK_FORCLR                     = 0X0F3E9
WORK_BAKCLR                     = 0X0F3EA
WORK_BDRCLR                     = 0X0F3EB
WORK_GRPACX                     = 0XFCB7
WORK_GRPACY                     = 0XFCB9
WORK_ROMVER                     = 0X0002D
BIOS_CHGMOD                     = 0X0005F
BIOS_CHGMODP                    = 0X001B5
BIOS_EXTROM                     = 0X0015F
WORK_RG1SV                      = 0X0F3E0
BIOS_WRTVDP                     = 0X00047
BIOS_ICOMP                      = 0X02F4D
WORK_JIFFY                      = 0X0FC9E
BIOS_IDIV                       = 0X031E6
BIOS_VMOVFM                     = 0X02F08
BIOS_NEG                        = 0X02E8D
BIOS_FRCSNG                     = 0X0303A
BLIB_INKEY                      = 0X0402A
BLIB_STRCMP                     = 0X04027
BLIB_FILE_PUTS                  = 0X040ED
WORK_PRTFLG                     = 0X0F416
WORK_PTRFIL                     = 0X0F864
BIOS_FOUT                       = 0X03425
BLIB_PUT_DIGITS                 = 0X040F6
WORK_FILTAB                     = 0XF860
WORK_ERRFLG                     = 0X0F414
; BSAVE header -----------------------------------------------------------
        DEFB        0xfe
        DEFW        start_address
        DEFW        end_address
        DEFW        start_address
        ORG         0X8010
START_ADDRESS:
        LD          HL, 0X8001
        LD          [WORK_TXTTAB], HL
        LD          HL, 0
        LD          [0X8001], HL
        LD          [0X8002], HL
        LD          HL, [WORK_USRTAB + 0]
        LD          [SVARI_USR0_BACKUP], HL
        LD          HL, HEAP_START
        LD          [WORK_VARTAB], HL
        LD          HL, _BASIC_START
        CALL        BIOS_NEWSTT
_BASIC_START_RET:
        LD          HL, [SVARI_USR0_BACKUP]
        LD          [WORK_USRTAB + 0], HL
        LD          A, 1
        LD          [WORK_MAXFIL], A
        LD          SP, [WORK_HIMEM]
        CALL        CHECK_BLIB
        JP          NZ, BIOS_SYNTAX_ERROR
        LD          IX, BLIB_INIT_NCALBAS
        CALL        CALL_BLIB
        LD          HL, WORK_H_TIMI
        LD          DE, H_TIMI_BACKUP
        LD          BC, 5
        LDIR        
        DI          
        CALL        SETUP_H_TIMI
        LD          DE, PROGRAM_START
        JP          PROGRAM_RUN
SETUP_H_TIMI:
        LD          HL, H_TIMI_HANDLER
        LD          [WORK_H_TIMI + 1], HL
        LD          A, 0XC3
        LD          [WORK_H_TIMI], A
        RET         
SETUP_H_ERRO:
        LD          HL, WORK_H_ERRO
        LD          DE, H_ERRO_BACKUP
        LD          BC, 5
        LDIR        
        LD          HL, H_ERRO_HANDLER
        LD          [WORK_H_ERRO + 1], HL
        LD          A, 0XC3
        LD          [WORK_H_ERRO], A
        RET         
JP_HL:
        JP          HL
_BASIC_START:
        DEFB        ':', 0xCD, 0xB7, 0xEF, 0x11, ':', 0x97, 0xDD, 0xEF, 0x0C
        DEFW        _BASIC_START_RET
        DEFB        ':', 'A', 0xEF, 0xDD, '(', 0x11, ')', 0x00
PROGRAM_START:
LINE_10:
; DEFUSR DEMO SMALL ENOUGH TO _RUN UNDER X-BASIC
LINE_20:
LINE_30:
; DEFUSR PREPARE STUFF
LINE_40:
        LD          HL, [VARIA_ZZ]
        LD          A, L
        OR          A, H
        JP          NZ, BIOS_ERRHAND_REDIM
        LD          HL, 10
        INC         HL
        PUSH        HL
        ADD         HL, HL
        LD          DE, 5
        ADD         HL, DE
        PUSH        HL
        LD          C, L
        LD          B, H
        CALL        ALLOCATE_HEAP
        LD          [VARIA_ZZ], HL
        POP         BC
        DEC         BC
        DEC         BC
        LD          [HL], C
        INC         HL
        LD          [HL], B
        INC         HL
        LD          A, 1
        LD          [HL], A
        INC         HL
        POP         BC
        LD          [HL], C
        INC         HL
        LD          [HL], B
        LD          HL, [VARIA_JR]
        LD          A, L
        OR          A, H
        JP          NZ, BIOS_ERRHAND_REDIM
        LD          HL, 2
        INC         HL
        PUSH        HL
        ADD         HL, HL
        LD          DE, 5
        ADD         HL, DE
        PUSH        HL
        LD          C, L
        LD          B, H
        CALL        ALLOCATE_HEAP
        LD          [VARIA_JR], HL
        POP         BC
        DEC         BC
        DEC         BC
        LD          [HL], C
        INC         HL
        LD          [HL], B
        INC         HL
        LD          A, 1
        LD          [HL], A
        INC         HL
        POP         BC
        LD          [HL], C
        INC         HL
        LD          [HL], B
        LD          HL, [VARIA_PT]
        LD          A, L
        OR          A, H
        JP          NZ, BIOS_ERRHAND_REDIM
        LD          HL, 3
        INC         HL
        PUSH        HL
        ADD         HL, HL
        LD          DE, 5
        ADD         HL, DE
        PUSH        HL
        LD          C, L
        LD          B, H
        CALL        ALLOCATE_HEAP
        LD          [VARIA_PT], HL
        POP         BC
        DEC         BC
        DEC         BC
        LD          [HL], C
        INC         HL
        LD          [HL], B
        INC         HL
        LD          A, 1
        LD          [HL], A
        INC         HL
        POP         BC
        LD          [HL], C
        INC         HL
        LD          [HL], B
LINE_50:
; RST #30, SLOT ID
LINE_60:
        LD          HL, VARIA_JR
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 247
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_JR
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 1
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          A, [62274]
        LD          L, A
        LD          H, 0
        LD          A, L
        POP         HL
        LD          [HL], A
LINE_70:
; ADDRESS
LINE_80:
        LD          HL, VARIA_JR
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 1
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 16404
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_90:
; EI, RET
LINE_100:
        LD          HL, VARIA_JR
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 2
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 51707
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_110:
        LD          HL, VARI_PB
        PUSH        HL
        LD          HL, 256
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_SB
        PUSH        HL
        LD          HL, 8192
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_X1
        PUSH        HL
        LD          HL, 112
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_Y1
        PUSH        HL
        LD          HL, 114
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_V1
        PUSH        HL
        LD          HL, 1
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_V2
        PUSH        HL
        LD          HL, 1
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_XL
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_XR
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_YU
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_YD
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_CX
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_CY
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_T
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARS_A
        PUSH        HL
        LD          HL, STR_0
        POP         DE
        EX          DE, HL
        LD          C, [HL]
        LD          [HL], E
        INC         HL
        LD          B, [HL]
        LD          [HL], D
        LD          L, C
        LD          H, B
        CALL        FREE_STRING
        LD          HL, VARI_TM
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_MB
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_I
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_120:
        LD          HL, VARIA_PT
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 18834
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_PT
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 1
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 37412
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_PT
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 2
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 9289
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_PT
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 3
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 18834
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_130:
; DETECT X-BASIC
LINE_140:
        LD          HL, VARI_XB
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_170:
; _MEMCPY(&H4010,VARPTR(MB),2)
LINE_180:
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 2
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 1
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 16400
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 2
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, VARI_MB
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 3
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 2
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_JR
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        LD          [WORK_USRTAB + 0], HL
        LD          HL, VARI_I
        PUSH        HL
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        LD          [WORK_DAC + 2], HL
        LD          A, 2
        LD          [WORK_VALTYP], A
        LD          HL, _PT0
        PUSH        HL
        LD          HL, [WORK_USRTAB + 0]
        PUSH        HL
        LD          HL, WORK_DAC
        RET         
_PT0:
        CALL        BIOS_FRCINT
        LD          HL, [WORK_DAC + 2]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_200:
        LD          HL, 15
        LD          A, L
        LD          [WORK_FORCLR], A
        LD          HL, 1
        LD          A, L
        LD          [WORK_BAKCLR], A
        LD          HL, 1
        LD          A, L
        LD          [WORK_BDRCLR], A
        CALL        BIOS_CHGCLR
        LD          HL, 2
        LD          A, [WORK_ROMVER]
        OR          A, A
        LD          A, L
        JR          NZ, _PT1
        CALL        BIOS_CHGMOD
        JR          _PT2
_PT1:
        LD          IX, BIOS_CHGMODP
        CALL        BIOS_EXTROM
        EI          
_PT2:
        LD          HL, 2
        LD          A, L
        AND         A, 3
        LD          L, A
        LD          A, [WORK_RG1SV]
        AND         A, 0XFC
        OR          A, L
        LD          B, A
        LD          C, 1
        CALL        BIOS_WRTVDP
LINE_210:
; _TILERAM(VARPTR(PT(0)),1,1,SB,32,24,0,0,32,24)
LINE_220:
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 34
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 1
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, VARIA_PT
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 2
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 1
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 3
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 1
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 4
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_SB]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 5
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 32
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 6
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 24
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 7
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 8
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 9
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 32
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 10
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 24
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_I
        PUSH        HL
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        LD          [WORK_DAC + 2], HL
        LD          A, 2
        LD          [WORK_VALTYP], A
        LD          HL, _PT3
        PUSH        HL
        LD          HL, [WORK_USRTAB + 0]
        PUSH        HL
        LD          HL, WORK_DAC
        RET         
_PT3:
        CALL        BIOS_FRCINT
        LD          HL, [WORK_DAC + 2]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_230:
; _MEMCPY(SB,PB,6144)
LINE_240:
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 2
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 1
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_SB]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 2
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_PB]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 3
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 6144
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_I
        PUSH        HL
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        LD          [WORK_DAC + 2], HL
        LD          A, 2
        LD          [WORK_VALTYP], A
        LD          HL, _PT4
        PUSH        HL
        LD          HL, [WORK_USRTAB + 0]
        PUSH        HL
        LD          HL, WORK_DAC
        RET         
_PT4:
        CALL        BIOS_FRCINT
        LD          HL, [WORK_DAC + 2]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_250:
; _TILEVRM(VARPTR(PT(0)),1,1,0,0,32,24)
LINE_260:
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 35
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 1
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, VARIA_PT
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 2
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 1
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 3
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 1
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 4
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 5
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 6
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 32
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 7
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 24
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_I
        PUSH        HL
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        LD          [WORK_DAC + 2], HL
        LD          A, 2
        LD          [WORK_VALTYP], A
        LD          HL, _PT5
        PUSH        HL
        LD          HL, [WORK_USRTAB + 0]
        PUSH        HL
        LD          HL, WORK_DAC
        RET         
_PT5:
        CALL        BIOS_FRCINT
        LD          HL, [WORK_DAC + 2]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_270:
; _FILVRM(8192,6144,&H4E,0)
LINE_280:
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 36
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 1
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 8192
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 2
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 6144
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 3
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 78
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 4
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_I
        PUSH        HL
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        LD          [WORK_DAC + 2], HL
        LD          A, 2
        LD          [WORK_VALTYP], A
        LD          HL, _PT6
        PUSH        HL
        LD          HL, [WORK_USRTAB + 0]
        PUSH        HL
        LD          HL, WORK_DAC
        RET         
_PT6:
        CALL        BIOS_FRCINT
        LD          HL, [WORK_DAC + 2]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_300:
; _DLOAD ("MSXLOGOH.BIN",7,MB,2048)
LINE_310:
        LD          HL, VARS_A
        PUSH        HL
        LD          HL, STR_1
        POP         DE
        EX          DE, HL
        LD          C, [HL]
        LD          [HL], E
        INC         HL
        LD          B, [HL]
        LD          [HL], D
        LD          L, C
        LD          H, B
        CALL        FREE_STRING
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 40
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 1
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, VARS_A
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 2
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 7
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 3
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_MB]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 4
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 2048
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 5
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_XB]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_I
        PUSH        HL
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        LD          [WORK_DAC + 2], HL
        LD          A, 2
        LD          [WORK_VALTYP], A
        LD          HL, _PT7
        PUSH        HL
        LD          HL, [WORK_USRTAB + 0]
        PUSH        HL
        LD          HL, WORK_DAC
        RET         
_PT7:
        CALL        BIOS_FRCINT
        LD          HL, [WORK_DAC + 2]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_320:
        LD          HL, [VARI_I]
        PUSH        HL
        LD          HL, 0
        POP         DE
        CALL        BIOS_ICOMP
        AND         A, 1
        DEC         A
        CPL         
        LD          H, A
        LD          L, A
        LD          A, L
        OR          A, H
        JP          Z, _PT9
        JP          LINE_1500
_PT9:
_PT8:
LINE_1000:
        LD          HL, 0
        LD          [WORK_JIFFY], HL
LINE_1010:
        LD          HL, VARI_CX
        PUSH        HL
        LD          HL, [VARI_X1]
        PUSH        HL
        LD          HL, 8
        POP         DE
        CALL        BIOS_IDIV
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARI_CY
        PUSH        HL
        LD          HL, [VARI_Y1]
        PUSH        HL
        LD          HL, 8
        POP         DE
        CALL        BIOS_IDIV
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_1020:
; _BLIT(X1,Y1,MB,MB+&H400,16,8,PB,32)
LINE_1030:
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 4
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 1
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_X1]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 2
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_Y1]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 3
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_MB]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 4
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_MB]
        PUSH        HL
        LD          HL, 1024
        POP         DE
        ADD         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 5
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 16
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 6
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 8
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 7
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_PB]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 8
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 32
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_JR
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        LD          [WORK_USRTAB + 0], HL
        LD          HL, VARI_I
        PUSH        HL
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        LD          [WORK_DAC + 2], HL
        LD          A, 2
        LD          [WORK_VALTYP], A
        LD          HL, _PT10
        PUSH        HL
        LD          HL, [WORK_USRTAB + 0]
        PUSH        HL
        LD          HL, WORK_DAC
        RET         
_PT10:
        CALL        BIOS_FRCINT
        LD          HL, [WORK_DAC + 2]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_1040:
        LD          HL, VARI_X1
        PUSH        HL
        LD          HL, [VARI_X1]
        PUSH        HL
        LD          HL, [VARI_V1]
        POP         DE
        ADD         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, [VARI_X1]
        PUSH        HL
        LD          HL, 1
        POP         DE
        EX          DE, HL
        XOR         A, A
        SBC         HL, DE
        JP          P, _PT13
        DEC         A
_PT13:
        LD          H, A
        LD          L, A
        PUSH        HL
        LD          HL, [VARI_X1]
        PUSH        HL
        LD          HL, 128
        POP         DE
        EX          DE, HL
        XOR         A, A
        SBC         HL, DE
        JP          M, _PT14
        DEC         A
_PT14:
        LD          H, A
        LD          L, A
        POP         DE
        LD          A, L
        OR          A, E
        LD          L, A
        LD          A, H
        OR          A, D
        LD          H, A
        LD          A, L
        OR          A, H
        JP          Z, _PT12
        LD          HL, VARI_V1
        PUSH        HL
        LD          HL, [VARI_V1]
        EX          DE, HL
        LD          HL, 0
        OR          A, A
        SBC         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        JP          _PT11
_PT12:
_PT11:
LINE_1050:
        LD          HL, VARI_Y1
        PUSH        HL
        LD          HL, [VARI_Y1]
        PUSH        HL
        LD          HL, [VARI_V2]
        POP         DE
        ADD         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, [VARI_Y1]
        PUSH        HL
        LD          HL, 1
        POP         DE
        EX          DE, HL
        XOR         A, A
        SBC         HL, DE
        JP          P, _PT17
        DEC         A
_PT17:
        LD          H, A
        LD          L, A
        PUSH        HL
        LD          HL, [VARI_Y1]
        PUSH        HL
        LD          HL, 128
        POP         DE
        EX          DE, HL
        XOR         A, A
        SBC         HL, DE
        JP          M, _PT18
        DEC         A
_PT18:
        LD          H, A
        LD          L, A
        POP         DE
        LD          A, L
        OR          A, E
        LD          L, A
        LD          A, H
        OR          A, D
        LD          H, A
        LD          A, L
        OR          A, H
        JP          Z, _PT16
        LD          HL, VARI_V2
        PUSH        HL
        LD          HL, [VARI_V2]
        EX          DE, HL
        LD          HL, 0
        OR          A, A
        SBC         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        JP          _PT15
_PT16:
_PT15:
LINE_1060:
        LD          HL, VARI_XL
        PUSH        HL
        LD          HL, [VARI_CX]
        PUSH        HL
        LD          HL, 1
        POP         DE
        EX          DE, HL
        OR          A, A
        SBC         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, [VARI_XL]
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        XOR         A, A
        SBC         HL, DE
        JP          P, _PT21
        DEC         A
_PT21:
        LD          H, A
        LD          L, A
        LD          A, L
        OR          A, H
        JP          Z, _PT20
        LD          HL, VARI_XL
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        JP          _PT19
_PT20:
_PT19:
LINE_1070:
        LD          HL, VARI_YU
        PUSH        HL
        LD          HL, [VARI_CY]
        PUSH        HL
        LD          HL, 1
        POP         DE
        EX          DE, HL
        OR          A, A
        SBC         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, [VARI_YU]
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        XOR         A, A
        SBC         HL, DE
        JP          P, _PT24
        DEC         A
_PT24:
        LD          H, A
        LD          L, A
        LD          A, L
        OR          A, H
        JP          Z, _PT23
        LD          HL, VARI_YU
        PUSH        HL
        LD          HL, 0
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        JP          _PT22
_PT23:
_PT22:
LINE_1080:
        LD          HL, VARI_XR
        PUSH        HL
        LD          HL, [VARI_CX]
        PUSH        HL
        LD          HL, 18
        POP         DE
        ADD         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, [VARI_XR]
        PUSH        HL
        LD          HL, 32
        POP         DE
        XOR         A, A
        SBC         HL, DE
        JP          P, _PT27
        DEC         A
_PT27:
        LD          H, A
        LD          L, A
        LD          A, L
        OR          A, H
        JP          Z, _PT26
        LD          HL, VARI_XR
        PUSH        HL
        LD          HL, 32
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        JP          _PT25
_PT26:
_PT25:
LINE_1090:
        LD          HL, VARI_YD
        PUSH        HL
        LD          HL, [VARI_CY]
        PUSH        HL
        LD          HL, 10
        POP         DE
        ADD         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, [VARI_YD]
        PUSH        HL
        LD          HL, 24
        POP         DE
        XOR         A, A
        SBC         HL, DE
        JP          P, _PT30
        DEC         A
_PT30:
        LD          H, A
        LD          L, A
        LD          A, L
        OR          A, H
        JP          Z, _PT29
        LD          HL, VARI_YD
        PUSH        HL
        LD          HL, 24
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        JP          _PT28
_PT29:
_PT28:
LINE_1100:
        LD          HL, VARI_T
        PUSH        HL
        LD          HL, [VARI_YU]
        PUSH        HL
        LD          HL, 256
        POP         DE
        CALL        BIOS_IMULT
        PUSH        HL
        LD          HL, [VARI_XL]
        PUSH        HL
        LD          HL, 8
        POP         DE
        CALL        BIOS_IMULT
        POP         DE
        ADD         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_1110:
; _BOXMEMVRM(PB+T,(XR-XL)*8,YD-YU,256,BASE(12)+T,256)
LINE_1120:
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 16
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 1
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_PB]
        PUSH        HL
        LD          HL, [VARI_T]
        POP         DE
        ADD         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 2
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_XR]
        PUSH        HL
        LD          HL, [VARI_XL]
        POP         DE
        EX          DE, HL
        OR          A, A
        SBC         HL, DE
        PUSH        HL
        LD          HL, 8
        POP         DE
        CALL        BIOS_IMULT
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 3
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_YD]
        PUSH        HL
        LD          HL, [VARI_YU]
        POP         DE
        EX          DE, HL
        OR          A, A
        SBC         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 4
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 256
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 5
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_T]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 6
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 256
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_JR
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        LD          [WORK_USRTAB + 0], HL
        LD          HL, VARI_I
        PUSH        HL
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        LD          [WORK_DAC + 2], HL
        LD          A, 2
        LD          [WORK_VALTYP], A
        LD          HL, _PT31
        PUSH        HL
        LD          HL, [WORK_USRTAB + 0]
        PUSH        HL
        LD          HL, WORK_DAC
        RET         
_PT31:
        CALL        BIOS_FRCINT
        LD          HL, [WORK_DAC + 2]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_1130:
; _BOXMEMCPY(SB+T,(XR-XL)*8,YD-YU,256,PB+T,256)
LINE_1140:
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 15
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 1
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_SB]
        PUSH        HL
        LD          HL, [VARI_T]
        POP         DE
        ADD         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 2
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_XR]
        PUSH        HL
        LD          HL, [VARI_XL]
        POP         DE
        EX          DE, HL
        OR          A, A
        SBC         HL, DE
        PUSH        HL
        LD          HL, 8
        POP         DE
        CALL        BIOS_IMULT
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 3
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_YD]
        PUSH        HL
        LD          HL, [VARI_YU]
        POP         DE
        EX          DE, HL
        OR          A, A
        SBC         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 4
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 256
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 5
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, [VARI_PB]
        PUSH        HL
        LD          HL, [VARI_T]
        POP         DE
        ADD         HL, DE
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 6
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        PUSH        HL
        LD          HL, 256
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, VARIA_JR
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        LD          [WORK_USRTAB + 0], HL
        LD          HL, VARI_I
        PUSH        HL
        LD          HL, VARIA_ZZ
        LD          D, 1
        LD          BC, 27
        CALL        CHECK_ARRAY
        CALL        CALC_ARRAY_TOP
        LD          HL, 0
        ADD         HL, HL
        POP         DE
        ADD         HL, DE
        LD          [WORK_DAC + 2], HL
        LD          A, 2
        LD          [WORK_VALTYP], A
        LD          HL, _PT32
        PUSH        HL
        LD          HL, [WORK_USRTAB + 0]
        PUSH        HL
        LD          HL, WORK_DAC
        RET         
_PT32:
        CALL        BIOS_FRCINT
        LD          HL, [WORK_DAC + 2]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
LINE_1200:
        LD          HL, VARI_T
        PUSH        HL
        LD          HL, [WORK_JIFFY]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        LD          HL, [VARI_TM]
        PUSH        HL
        LD          HL, [VARI_T]
        POP         DE
        EX          DE, HL
        XOR         A, A
        SBC         HL, DE
        JP          P, _PT35
        DEC         A
_PT35:
        LD          H, A
        LD          L, A
        LD          A, L
        OR          A, H
        JP          Z, _PT34
        LD          HL, VARI_TM
        PUSH        HL
        LD          HL, [VARI_T]
        POP         DE
        EX          DE, HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        JP          _PT33
_PT34:
_PT33:
LINE_1210:
        LD          HL, VARS_A
        PUSH        HL
        LD          IX, BLIB_INKEY
        CALL        CALL_BLIB
        CALL        COPY_STRING
        POP         DE
        EX          DE, HL
        LD          C, [HL]
        LD          [HL], E
        INC         HL
        LD          B, [HL]
        LD          [HL], D
        LD          L, C
        LD          H, B
        CALL        FREE_STRING
        LD          HL, [VARS_A]
        CALL        COPY_STRING
        PUSH        HL
        LD          HL, STR_0
        POP         DE
        EX          DE, HL
        PUSH        HL
        PUSH        DE
        LD          IX, BLIB_STRCMP
        CALL        CALL_BLIB
        POP         HL
        PUSH        AF
        CALL        FREE_STRING
        POP         AF
        POP         HL
        PUSH        AF
        CALL        FREE_STRING
        POP         AF
        LD          HL, 0
        JR          NZ, _PT38
        DEC         HL
_PT38:
        LD          A, L
        OR          A, H
        JP          Z, _PT37
        JP          LINE_1000
_PT37:
_PT36:
LINE_1500:
        LD          HL, 0
        LD          A, [WORK_ROMVER]
        OR          A, A
        LD          A, L
        JR          NZ, _PT39
        CALL        BIOS_CHGMOD
        JR          _PT40
_PT39:
        LD          IX, BIOS_CHGMODP
        CALL        BIOS_EXTROM
        EI          
_PT40:
        XOR         A, A
        LD          [WORK_PRTFLG], A
        LD          H, A
        LD          L, A
        LD          [WORK_PTRFIL], HL
        LD          HL, [VARI_TM]
        CALL        PUT_INTEGER
        LD          HL, STR_2
        CALL        PUTS
PROGRAM_TERMINATION:
        CALL        SUB_TERMINATION
        LD          SP, [WORK_HIMEM]
        LD          HL, _BASIC_END
        JP          BIOS_NEWSTT
SUB_TERMINATION:
        XOR         A, A
        LD          [WORK_MAXFIL], A
        LD          HL, [WORK_HIMEM]
        LD          DE, 267
        SBC         HL, DE
        LD          [WORK_FILTAB], HL
        LD          L, E
        LD          H, D
        INC         DE
        INC         DE
        LD          [HL], E
        INC         HL
        CALL        RESTORE_H_ERRO
        CALL        RESTORE_H_TIMI
        LD          HL, 0X8001
        LD          [WORK_TXTTAB], HL
        RET         
_BASIC_END:
        DEFB        ':', 0x92, ':', 0x94, ':', 0x81, 0x00
ERR_RETURN_WITHOUT_GOSUB:
        LD          E, 3
        JP          BIOS_ERRHAND
CHECK_BLIB:
        LD          A, [WORK_BLIBSLOT]
        LD          H, 0X40
        CALL        BIOS_ENASLT
        LD          BC, 8
        LD          HL, SIGNATURE
        LD          DE, SIGNATURE_REF
_CHECK_BLIB_LOOP:
        LD          A, [DE]
        INC         DE
        CPI         
        JR          NZ, _CHECK_BLIB_EXIT
        JP          PE, _CHECK_BLIB_LOOP
_CHECK_BLIB_EXIT:
        PUSH        AF
        LD          A, [WORK_MAINROM]
        LD          H, 0X40
        CALL        BIOS_ENASLT
        EI          
        POP         AF
        RET         
SIGNATURE_REF:
        DEFB        "BACONLIB"
CALL_BLIB:
        LD          IY, [WORK_BLIBSLOT - 1]
        CALL        BIOS_CALSLT
        EI          
        RET         
ALLOCATE_HEAP:
        LD          HL, [HEAP_NEXT]
        PUSH        HL
        ADD         HL, BC
        JR          C, _ALLOCATE_HEAP_ERROR
        LD          DE, [HEAP_END]
        RST         0X20
        JR          NC, _ALLOCATE_HEAP_ERROR
        LD          [HEAP_NEXT], HL
        POP         HL
        PUSH        HL
        DEC         BC
        LD          E, L
        LD          D, H
        INC         DE
        LD          [HL], 0
        LDIR        
        POP         HL
        RET         
_ALLOCATE_HEAP_ERROR:
        LD          E, 7
        JP          BIOS_ERRHAND
CHECK_ARRAY:
        LD          A, [HL]
        INC         HL
        OR          A, [HL]
        DEC         HL
        RET         NZ
        PUSH        DE
        PUSH        HL
        PUSH        BC
        CALL        ALLOCATE_HEAP
        POP         BC
        POP         DE
        POP         AF
        EX          DE, HL
        PUSH        HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        EX          DE, HL
        DEC         BC
        DEC         BC
        LD          [HL], C
        INC         HL
        LD          [HL], B
        INC         HL
        LD          [HL], A
        INC         HL
        LD          B, A
        LD          DE, 11
_CHECK_ARRAY_LOOP:
        LD          [HL], E
        INC         HL
        LD          [HL], D
        INC         HL
        DJNZ        _CHECK_ARRAY_LOOP
        POP         HL
        RET         
CALC_ARRAY_TOP:
        LD          E, [HL]
        INC         HL
        LD          D, [HL]
        EX          DE, HL
        INC         HL
        INC         HL
        LD          E, [HL]
        INC         HL
        LD          D, 0
        ADD         HL, DE
        ADD         HL, DE
        LD          A, E
        POP         DE
        PUSH        HL
        JR          _CALC_ARRAY_TOP_L2
_CALC_ARRAY_TOP_L1:
        DEC         HL
        LD          B, [HL]
        DEC         HL
        LD          C, [HL]
        PUSH        BC
_CALC_ARRAY_TOP_L2:
        DEC         A
        JR          NZ, _CALC_ARRAY_TOP_L1
        PUSH        DE
        RET         
FREE_STRING:
        LD          DE, HEAP_START
        RST         0X20
        RET         C
        LD          DE, [HEAP_NEXT]
        RST         0X20
        RET         NC
        LD          C, [HL]
        LD          B, 0
        INC         BC
        JP          FREE_HEAP
FREE_HEAP:
        PUSH        HL
        ADD         HL, BC
        LD          [HEAP_MOVE_SIZE], BC
        LD          [HEAP_REMAP_ADDRESS], HL
        EX          DE, HL
        LD          HL, [HEAP_NEXT]
        SBC         HL, DE
        LD          C, L
        LD          B, H
        POP         HL
        EX          DE, HL
        LD          A, C
        OR          A, B
        JR          Z, _FREE_HEAP_LOOP0
        LDIR        
_FREE_HEAP_LOOP0:
        LD          [HEAP_NEXT], DE
        LD          HL, VARS_AREA_START
_FREE_HEAP_LOOP1:
        LD          DE, VARSA_AREA_END
        RST         0X20
        JR          NC, _FREE_HEAP_LOOP1_END
        LD          E, [HL]
        INC         HL
        LD          D, [HL]
        PUSH        HL
        LD          HL, [HEAP_REMAP_ADDRESS]
        EX          DE, HL
        RST         0X20
        JR          C, _FREE_HEAP_LOOP1_NEXT
        LD          DE, [HEAP_MOVE_SIZE]
        SBC         HL, DE
        POP         DE
        EX          DE, HL
        DEC         HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        PUSH        HL
_FREE_HEAP_LOOP1_NEXT:
        POP         HL
        INC         HL
        JR          _FREE_HEAP_LOOP1
_FREE_HEAP_LOOP1_END:
        LD          HL, VARSA_AREA_START
_FREE_HEAP_LOOP2:
        LD          DE, VARSA_AREA_END
        RST         0X20
        RET         NC
        LD          E, [HL]
        LD          A, E
        INC         HL
        LD          D, [HL]
        OR          A, D
        INC         HL
        JR          Z, _FREE_HEAP_LOOP2
        PUSH        HL
        EX          DE, HL
        LD          E, [HL]
        INC         HL
        LD          D, [HL]
        INC         HL
        LD          C, [HL]
        INC         HL
        LD          B, 0
        ADD         HL, BC
        ADD         HL, BC
        EX          DE, HL
        SBC         HL, BC
        SBC         HL, BC
        RR          H
        RR          L
        LD          C, L
        LD          B, H
        EX          DE, HL
_FREE_HEAP_SARRAY_ELEMENTS:
        LD          E, [HL]
        INC         HL
        LD          D, [HL]
        PUSH        HL
        LD          HL, [HEAP_REMAP_ADDRESS]
        EX          DE, HL
        RST         0X20
        JR          C, _FREE_HEAP_LOOP2_NEXT
        LD          DE, [HEAP_MOVE_SIZE]
        SBC         HL, DE
        POP         DE
        EX          DE, HL
        DEC         HL
        LD          [HL], E
        INC         HL
        LD          [HL], D
        PUSH        HL
_FREE_HEAP_LOOP2_NEXT:
        POP         HL
        INC         HL
        DEC         BC
        LD          A, C
        OR          A, B
        JR          NZ, _FREE_HEAP_SARRAY_ELEMENTS
        POP         HL
        JR          _FREE_HEAP_LOOP2
; Allocate memory for strings. A: Length
ALLOCATE_STRING:
        LD          HL, [HEAP_NEXT]
        PUSH        HL
        LD          E, A
        LD          C, A
        LD          D, 0
        ADD         HL, DE
        INC         HL
        LD          DE, [HEAP_END]
        RST         0X20
        JR          NC, _ALLOCATE_STRING_ERROR
        LD          [HEAP_NEXT], HL
        POP         HL
        LD          [HL], C
        RET         
_ALLOCATE_STRING_ERROR:
        LD          E, 7
        JP          BIOS_ERRHAND
COPY_STRING:
        LD          A, [HL]
        PUSH        HL
        CALL        ALLOCATE_STRING
        POP         DE
        PUSH        HL
        EX          DE, HL
        LD          C, [HL]
        LD          B, 0
        INC         BC
        LDIR        
        POP         HL
        RET         
STR:
        CALL        BIOS_FOUT
FOUT_ADJUST:
        DEC         HL
        PUSH        HL
        XOR         A, A
        LD          B, A
_STR_LOOP:
        INC         HL
        CP          A, [HL]
        JR          Z, _STR_LOOP_EXIT
        INC         B
        JR          _STR_LOOP
_STR_LOOP_EXIT:
        POP         HL
        LD          [HL], B
        RET         
PUT_INTEGER:
        LD          [WORK_DAC + 2], HL
        LD          A, 2
        LD          [WORK_VALTYP], A
        CALL        STR
        LD          IX, BLIB_PUT_DIGITS
        JP          CALL_BLIB
PUTS:
        LD          B, [HL]
        INC         B
        DEC         B
        RET         Z
_PUTS_LOOP:
        INC         HL
        LD          A, [HL]
        RST         0X18
        DJNZ        _PUTS_LOOP
        RET         
PROGRAM_RUN:
        LD          HL, HEAP_START
        LD          [HEAP_NEXT], HL
        LD          SP, [WORK_HIMEM]
        LD          HL, ERR_RETURN_WITHOUT_GOSUB
        PUSH        HL
        PUSH        DE
        LD          DE, 256
        XOR         A, A
        LD          HL, [WORK_HIMEM]
        SBC         HL, DE
        LD          [HEAP_END], HL
        DI          
        CALL        SETUP_H_ERRO
        EI          
        LD          HL, VAR_AREA_START
        LD          DE, VAR_AREA_START + 1
        LD          BC, VARSA_AREA_END - VAR_AREA_START - 1
        LD          [HL], 0
        LDIR        
        LD          HL, STR_0
        LD          [VARS_AREA_START], HL
        RET         
; H.TIMI PROCESS -----------------
H_TIMI_HANDLER:
        JP          H_TIMI_BACKUP
RESTORE_H_TIMI:
        DI          
        LD          HL, H_TIMI_BACKUP
        LD          DE, WORK_H_TIMI
        LD          BC, 5
        LDIR        
        EI          
        RET         
RESTORE_H_ERRO:
        DI          
        LD          HL, H_ERRO_BACKUP
        LD          DE, WORK_H_ERRO
        LD          BC, 5
        LDIR        
        EI          
_CODE_RET:
        RET         
H_ERRO_HANDLER:
        PUSH        HL
        PUSH        DE
        PUSH        BC
        CALL        RESTORE_H_TIMI
        CALL        RESTORE_H_ERRO
        CALL        SUB_TERMINATION
        POP         BC
        POP         DE
        POP         HL
        JP          WORK_H_ERRO
STR_0:
        DEFB        0X00
STR_1:
        DEFB        0X0C, 0X4D, 0X53, 0X58, 0X4C, 0X4F, 0X47, 0X4F, 0X48, 0X2E, 0X42, 0X49, 0X4E
STR_2:
        DEFB        0X02, 0X0D, 0X0A
HEAP_NEXT:
        DEFW        0
HEAP_END:
        DEFW        0
HEAP_MOVE_SIZE:
        DEFW        0
HEAP_REMAP_ADDRESS:
        DEFW        0
VAR_AREA_START:
SVARI_USR0_BACKUP:
        DEFW        0
VARI_CX:
        DEFW        0
VARI_CY:
        DEFW        0
VARI_I:
        DEFW        0
VARI_MB:
        DEFW        0
VARI_PB:
        DEFW        0
VARI_SB:
        DEFW        0
VARI_T:
        DEFW        0
VARI_TM:
        DEFW        0
VARI_V1:
        DEFW        0
VARI_V2:
        DEFW        0
VARI_X1:
        DEFW        0
VARI_XB:
        DEFW        0
VARI_XL:
        DEFW        0
VARI_XR:
        DEFW        0
VARI_Y1:
        DEFW        0
VARI_YD:
        DEFW        0
VARI_YU:
        DEFW        0
VAR_AREA_END:
VARS_AREA_START:
VARS_A:
        DEFW        0
VARS_AREA_END:
VARA_AREA_START:
VARIA_JR:
        DEFW        0
VARIA_PT:
        DEFW        0
VARIA_ZZ:
        DEFW        0
VARA_AREA_END:
VARSA_AREA_START:
VARSA_AREA_END:
H_TIMI_BACKUP:
        DEFB        0, 0, 0, 0, 0
H_ERRO_BACKUP:
        DEFB        0, 0, 0, 0, 0
HEAP_START:
END_ADDRESS:
