FILE: $EXECSWT ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01787000
         $EXECSWT                                                       01788000
$INEX    EQU   X'01'          INEXECUTION                               01789000
$DUMP    EQU   X'02'          DUMP ON PROG INT.                         01790000
$NONX    EQU   X'04'          NON-EXECUTABLE                            01791000
$LOAD    EQU   X'08'          IN LOADER                                 01792000
$JTERM   EQU   X'10'          DURING JOB TERMINATION                    01793000
$BAT     EQU   X'20'   BATCH MONITOR IS RUNNING                         01794000
$EOS     EQU   X'40'              END OF INPUT STREAM                   01795000
         MEND                                                           01796000


FILE: ADT      ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          00773000
         ADT                                                            00774000
*                                                                       00775000
* ACTIVE DISK TABLE BLOCK                                               00776000
*                                                                       00777000
ADTSECT  DSECT                                                          00778000
*                                                                       00779000
*        NEEDED FOR READ-ONLY DISKS AND READ-WRITE DISKS                00780000
*                                                                       00781000
ADTID    DS    CL6            DISK-IDENTIFIER (LABEL)                   00782000
ADTFLG3  DS    1X             -RESERVED FOR FUTURE USE-                 00783000
ADTFTYP  DS    1X             FILETYPE FLAG-BYTE                        00784000
ADTPTR   DS    1A             POINTER TO NEXT ADT BLOCK IN CHAIN        00785000
ADTDTA   DS    1A             DEVICE TABLE ADDRESS IN NUCON             00786000
ADTFDA   DS    1A             FILE DIRECTORY (PSTAT) ADDRESS            00787000
ADTMFDN  DS    1F             (1) NUMBER DBL-WORDS IN MFD               00788000
ADTMFDA  DS    1A             (2) MASTER FILE DIRECTORY ADDRESS         00789000
ADTHBCT  DS    1F             FST HYPERBLOCK COUNT                      00790000
ADTFSTC  DS    1F             NUMBER OF FST 40-BYTE ENTRIES (FILES)     00791000
ADTCHBA  DS    1A          (1) POINTER TO CURRENT FST HYPERBLOCK        00792000
ADTCFST  DS    1F          (2) DISPLACEMENT OF CURRENT FST ENTRY        00793000
ADT1ST   DS    1F         (1) DISP. OF 1ST WORD IN BIT-MASK WITH 'HOLE' 00794000
ADTNUM   DS    1F         (2) NUMBER OF RECORDS (NUMTRKS)               00795000
ADTUSED  DS    1F         (3) NUMBER OF RECORDS IN USE (QTUSEDP)        00796000
ADTLEFT  DS    1F         (4) NUMBER OF RECORDS LEFT (QTLEFTP)          00797000
ADTLAST  DS    1F         (5) DISP. OF LAST NONZERO BYTE IN BIT-MASK    00798000
ADTCYL   DS    1F         (6) NUMBER OF CYLINDERS ON DISK (NUMCYLP)     00799000
ADTM     DS    1C             MODE LETTER (P,T,S,A,B,C, ETC.)           00800000
ADTMX    DS    1C             EXTENSION-OF-MODE LETTER (P,T,S, ETC.)    00801000
ADTFLG1  DS    1X             FIRST FLAG-BYTE                           00802000
ADTFLG2  DS    1X             SECOND FLAG BYTE                          00803000
*                                                                       00804000
ADT2ND   DS    0D                                                       00805000
*                                                                       00806000
*        NEEDED JUST FOR READ-WRITE DISKS                               00807000
*                                                                       00808000
ADTMSK   DS    1A             800-BYTE (PQMSK) BIT-MASK ADDRESS         00809000
ADTQQM   DS    1A             200-BYTE (PQQMSK) BIT-MASK ADDRESS        00810000
ADTPQM1  DS    1F              (1) PQMSIZ = NO. BYTES IN PQMSK > 215    00811000
ADTPQM2  DS    1F              (2) PQMNUM = NO. 800 BYTE-REC FOR PQMSK  00812000
ADTPQM3  DS    1F         (3) R0NUM = NO. DBL-WORDS IN PQMSK            00813000
ADTLHBA  DS    1A         (1) POINTER TO LAST FST HYPER-BLOCK           00814000
ADTLFST  DS    1F         (2) DISP. OF LAST FST IN LAST HYPER-BLOCK     00815000
ADTNACW  DS    1H             NUMBER OF ACTIVE WRITE FILES - HALFWORD   00816000
ADTRES   DS    1H             RESERVE-COUNT (RESRVCNT) - HALFWORD       00817000
*                                                                       00818000
ADTLBM   EQU   ADT2ND-ADTSECT LENGTH OF MINIMUM ADT BLOCK (BYTES)       00819000
ADTLDM   EQU   ADTLBM/8       LENGTH OF MINIMUM ADT BLOCK IN DBL-WORDS  00820000
*                                                                       00821000
ADTLB    EQU   *-ADTSECT      LENGTH OF FULL ADT BLOCK (BYTES)          00822000
ADTLD    EQU   (ADTLB+7)/8    LENGTH OF FULL ADT BLOCK IN DBL-WORDS     00823000
*                                                                       00824000
*        FIRST FLAG-BYTE (ADTFLG1) DEFINITIONS                          00825000
*                                                                       00826000
ADTFSF   EQU   X'80'          ADT BLOCK IN FREE STORAGE                 00827000


FILE: ADT      ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 002


ADTFRO   EQU   X'40'          READ-ONLY DISK (ATTACHED & READY)         00828000
ADTFRW   EQU   X'20'          READ-WRITE DISK (ATTACHED & READY)        00829000
ADTFFSTF EQU   X'10'         1ST FST HYPERBLOCK IS IN FREE STORAGE      00830000
ADTFFSTV EQU   X'08'          FST HYPERBLOCKS ARE OF VARYING LENGTH     00831000
ADTFQQF  EQU   X'04'          200-BYTE QQMSK IS IN FREE STORAGE         00832000
ADTROX   EQU   X'02'          THIS DISK HAS READ-ONLY EXTENSION(S)      00833000
ADTFMIN  EQU   X'01'          ADT BLOCK IS MINIMUM SIZE                 00834000
*                                                                       00835000
*        SECOND FLAG-BYTE (ADTFLG2) DEFINITIONS                         00836000
*                                                                       00837000
ADTFMFD  EQU   X'80'          MFD IS IN CORE                            00838000
ADTFALNM EQU   X'40'          ALL FILENAMES ARE IN CORE                 00839000
ADTFALTY EQU   X'20'          ALL FILETYPES ARE IN CORE                 00840000
ADTFMDRO EQU   X'10'          MODES 1-6 ARE IN CORE                     00841000
ADTFALMD EQU   ADTFMDRO+X'08'  ALL MODES (0-6) ARE IN CORE              00842000
ADTFALUF EQU   ADTFMFD+ADTFALNM+ADTFALTY+ADTFALMD  ALL UFD IS IN CORE   00843000
ADTWMSG  EQU   X'04' READ-ONLY WARNING MESSAGE HAS BEEN GIVEN BY WRBUF  00844000
*                                                                       00845000
*        OTHER PARAMETERS                                               00846000
*                                                                       00847000
ADTRL    EQU   800            LOGICAL RECORD LENGTH                     00848000
ADTML    EQU   5              MAXIMUM BIT MASK LENGTH - IN RECORDS      00849000
*                                                                       00850000
*        NUCON DEVICE TABLE OFFSETS                                     00851000
*                                                                       00852000
DTAD     EQU   0              DEVICE NUMBER                             00853000
DTADT    EQU   3              DEVICE TYPE BYTE                          00854000
DTAS     EQU   4              SYMBOLIC DEVICE NAME                      00855000
         MEND                                                           00856000


FILE: AFT      ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          00000010
         AFT                                                            00000020
*                                                                       00000030
* ACTIVE FILE TABLE BLOCK                                               00000040
*                                                                       00000050
AFTSECT  DSECT                                                          00000060
AFTCLD   DS    H              DISK ADDRESS OF CURRENT CHAIN LINk - 0    00000070
AFTCLN   DS    H              NUMBER OF CURRENT CHAIN LINK - 2          00000080
AFTCLA   DS    F              CORE ADDRESS OF CHAIN LINK BUFFER - 4     00000090
AFTDBD   DS    H              DISK ADDRESS OF CURRENT DATA BLOCK - 8    00000100
AFTDBN   DS    H              NUMBER OF CURRENT DATA BLOCK - 10         00000110
AFTDBA   DS    F              CORE ADDRESS OF CURRENT DATA BLOCK - 12   00000120
AFTCLB   DS    XL80          CHAIN LINK BUFFER FROM 1ST CHAIN LINK - 16 00000130
AFTFLG   DS    X              FLAG BYTE - 96                            00000140
AFTPFST  DS    3X             POINTER TO (STATIC) FST-ENTRY - 97        00000150
AFTIN    DS    H              CURRENT ITEM NUMBER - 100                 00000160
AFTID    DS    H         DISPLACEMENT OF CURRENT ITEM IN DATA BLK - 102 00000170
AFTFCLA  DS    F              CORE ADDRESS OF FIRST CHAIN LINK          00000180
AFTFCLX  DS    H              DISK ADDRESS OF SWAPPED FCL               00000190
AFTCLDX  DS    H              DISK ADDRESS OF SWAPPED CHAIN LINK        00000200
AFTFLG2  DS    X              SECOND FLAG-BYTE                          00000210
         DS    3X             ***UNUSED***                              00000220
*                                                                       00000230
*        FLAG BYTE (AFTFLG) DEFINITIONS                                 00000240
*                                                                       00000250
AFTUSED  EQU   X'80'          ACTIVE FILE TABLE BLOCK IN USE            00000260
***      EQU   X'40'                                                    00000270
AFTICF   EQU   X'20'          FIRST CHAIN LINK IN CORE FLAG             00000280
AFTFBA   EQU   X'10'          FULL BUFFER ASSIGNED                      00000290
AFTDBF   EQU   X'08'          DATA BLOCK IN CORE FLAG                   00000300
AFTWRT   EQU   X'04'          ACTIVE WRITE                              00000310
AFTRD    EQU   X'02'          ACTIVE READ                               00000320
*                                                                       00000330
AFTFULD  EQU   X'01'          FULL-DISK SPECIAL CASE                    00000340
*                                                                       00000350
*        SECOND FLAG-BYTE (AFTFLG2) DEFINITIONS                         00000360
*                                                                       00000370
AFTNEW   EQU   X'80'          BRAND NEW FILE                            00000380
AFTOLDCL EQU   X'40'          CURRENT CHAIN LINK EXISTED PREVIOUSLY     00000390
AFTCLX   EQU   X'20'          ALTERNATE CHAIN-LINK ASSIGNED/IMPLIED     00000400
*                                                                       00000410
* COPY OF FST BLOCK IMBEDDED IN AFT BLOCK                               00000420
*                                                                       00000430
AFTFST   DS    0D             - 104                                     00000440
AFTN     DS    D              FILE NAME                                 00000450
AFTT     DS    D              FILE TYPE                                 00000460
AFTD     DS    F              DATE/TIME LAST WRITTEN                    00000470
AFTWP    DS    H              WRITE POINTER (ITEM NO.)                  00000480
AFTRP    DS    H              READ POINTER (ITEM NO.)                   00000490
AFTM     DS    H              FILE MODE                                 00000500
AFTIC    DS    H              ITEM COUNT                                00000510
AFTFCL   DS    H              FIRST CHAIN LINK                          00000520
AFTFV    DS    C              FIXED(F)/VARIABLE(V) FLAG                 00000530
AFTFB    DS    X              FST FLAG BYTE                             00000540
AFTIL    DS    F              (MAXIMUM) ITEM LENGTH                     00000550


FILE: AFT      ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 002


AFTDBC   DS    H              800-BYTE DATA BLOCK COUNT                 00000560
AFTYR    DS    H              YEAR                                      00000570
AFTADT   DS    F              POINTER TO ACTIVE DISK TABLE - 144        00000580
AFTPTR   DS    F              POINTER TO NEXT AFT BLOCK IN CHAIN - 148  00000590
*                                                                       00000600
AFTFSF   EQU   X'40'          BIT IN AFTPTR INDICATES IN FREE STORAGE   00000610
*                                                                       00000620
         DS    0D             END OF DSECT                              00000630
AFTLB    EQU   *-AFTSECT      LENGTH OF AFT BLOCK IN BYTES              00000640
AFTLD    EQU   AFTLB/8        LENGTH OF AFT BLOCK IN DOUBLE WORDS       00000650
         MEND                                                           00000660


FILE: BCLOSE   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01780000
&LABEL   BCLOSE  &CCLST                                                 01781000
&LABEL   LA    1,&CCLST                                                 01782000
         L   15,=V(SYSCTL)                                              01783000
         BALR  14,15                                                    01784000
         DC    AL4(*+4)                                                 01785000
         MEND                                                           01786000


FILE: BRETURN  ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01766000
&LABEL   BRETURN  &SAVE,&ERR                                            01767000
&LABEL   SR    15,15                                                    01768000
         IC    15,&ERR                                                  01769000
         LTR   15,15                                                    01770000
         BZ    L&SYSNDX                                                 01771000
         L     14,&SAVE+56                                              01772000
         MVC   &SAVE+56(4),0(14)                                        01773000
         LM    0,14,&SAVE                                               01774000
         BR    14                                                       01775000
L&SYSNDX LM    0,14,&SAVE                                               01776000
         BC    15,4(14)                                                 01777000
         SPACE 1                                                        01778000
         MEND                                                           01779000


FILE: CALLING  ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01473000
&LABEL   CALLING   &ROUTE                                               01474000
&LABEL   L     15,=V(&ROUTE)                                            01475000
         BALR  14,15                                                    01476000
         MEND                                                           01477000


FILE: CKEOF    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01357000
&LABEL   CKEOF &ERROR                                                   01358000
&LABEL   CH    15,=H'12'                                                01359000
         BNE   &ERROR                                                   01360000
         MEND                                                           01361000


FILE: CMLST    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01729000
&LABEL   CMLST &A,&B,&C,&D,&E                                           01730000
&LABEL   DS    0D                                                       01731000
         DC    CL8'&A '                                                 01732000
         DC    CL8'&B '                                                 01733000
         AIF   (N'&C EQ 0).SEQ2                                         01734000
         DC    CL8'&C '                                                 01735000
         AIF   (N'&D EQ 0).SEQ2                                         01736000
         DC    CL8'&D '                                                 01737000
         AIF   (N'&E EQ 0).SEQ2                                         01738000
         DC    CL8'&E '                                                 01739000
.SEQ2    DC    XL8'FFFFFFFFFFFFFFFF'                                    01740000
         SPACE 1                                                        01741000
         MEND                                                           01742000


FILE: CMS      ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01712000
&LABEL   CMS   &LIST,&COM,&ERROR=                                       01713000
&LABEL   LA    1,&LIST                                                  01714000
         AIF   (N'&COM EQ 0).SEQ1                                       01715000
         MVC   0(8,1),=CL8'&COM '                                       01716000
.SEQ1    ANOP                                                           01717000
         AIF   ('&COM' EQ 'PRESTORE' OR '&COM' EQ 'LIST').SEQ2          01718000
         SVC   X'CA'                                                    01719000
         AGO   .SEQ3                                                    01720000
.SEQ2    L     15,=V(&COM)                                              01721000
         BALR  14,15                                                    01722000
.SEQ3    AIF   (N'&ERROR EQ 0).SEQ4                                     01723000
         DC    AL4(&ERROR)                                              01724000
         AGO   .SEQ5                                                    01725000
.SEQ4    DC    AL4(*+4)                                                 01726000
.SEQ5    SPACE 1                                                        01727000
         MEND                                                           01728000


FILE: CMSAVE   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01162000
         CMSAVE                                                         01163000
*                                                                       01164000
*   SUPERVISOR INTERRUPT HANDLER BLOCK STORAGE DISPLACEMENTS            01165000
*                                                                       01166000
BASE     EQU   0                  IMAGE OF SVCINT BLOCK SAVEAREA        01167000
JSIND    EQU   0                  OVERRIDE INDICATOR: X'0,1,2,3'        01168000
OSFLAG   EQU   1                  SVC TYPE: X'00'=CMS, X'01'=OS         01169000
CHWORD   EQU   2                  --- NOT USED ---                      01170000
CALLER   EQU   4                  A(CALLER SVC INSTRUCTION)             01171000
CALLEE   EQU   8                  CL8' CALLEE '                         01172000
OLDPSW   EQU   16                 D' SVC OLD PSW '                      01173000
NRMRET   EQU   24                 A( NORMAL RETURN )                    01174000
ERRET    EQU   28                 A( ERROR RETUEN )                     01175000
GPRS     EQU   32             GENERAL REGISTERS AT ENTRY TO SVC.        01176000
GPR0     EQU   32                                                       01177000
GPR1     EQU   36             R1                                        01178000
GPR2     EQU   40             R2                                        01179000
GPR3     EQU   44             R3                                        01180000
GPR4     EQU   48             R4                                        01181000
GPR5     EQU   52             R5                                        01182000
GPR6     EQU   56             R6                                        01183000
GPR7     EQU   60             R7                                        01184000
GPR8     EQU   64             R8                                        01185000
GPR9     EQU   68             R9                                        01186000
GPR10    EQU   72             R10                                       01187000
GPR11    EQU   76             R11                                       01188000
GPR12    EQU   80             R12                                       01189000
GPR13    EQU   84             R13                                       01190000
GPR14    EQU   88             R14                                       01191000
GPR15    EQU   92             R15                                       01192000
FPRS     EQU   96             FLOATING-POINT REGISTERS AT ENTRY TO SVC  01193000
FPR0     EQU   96             FR0                                       01194000
FPR2     EQU   104            FR2                                       01195000
FPR4     EQU   112            FR4                                       01196000
FPR6     EQU   120            FR6                                       01197000
*                                                                       01198000
NGPRS    EQU   128                REGS UPON RETURN; SCRATCH AREA        01199000
NGPR0    EQU   128                                                      01200000
NGPR1    EQU   132                                                      01201000
NGPR2    EQU   136                                                      01202000
NGPR3    EQU   140                                                      01203000
NGPR4    EQU   144                                                      01204000
NGPR5    EQU   148                                                      01205000
NGPR6    EQU   152                                                      01206000
NGPR7    EQU   156                                                      01207000
NGPR8    EQU   160                                                      01208000
NGPR9    EQU   164                                                      01209000
NGPR10   EQU   168                                                      01210000
NGPR11   EQU   172                                                      01211000
NGPR12   EQU   176                                                      01212000
NGPR13   EQU   180                                                      01213000
NGPR14   EQU   184                                                      01214000
NGPR15   EQU   188                                                      01215000
NFPRS    EQU   192                                                      01216000


FILE: CMSAVE   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 002


NFPR0    EQU   192                                                      01217000
NFPR2    EQU   200                                                      01218000
NFPR4    EQU   208                                                      01219000
NFPR6    EQU   216                                                      01220000
         MEND                                                           01221000


FILE: CMSCB    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          00001000
         CMSCB                                                          00002000
*** PTF 'CMSCB A28946CA' HAS BEEN APPLIED                               00003000
*** PTF 'CMSCB A41336CA' HAS BEEN APPLIED
*                                                                       00004000
*   FCB HEADER CONTROL WORDS                                            00005000
*                                                                       00006000
FCBHEAD  DSECT                                                          00007000
FCBFIRST DC    A(0)               A(FIRST FCB IN CHAIN)                 00008000
FCBNUM   DC    H'0'               NUMBER OF FCB BLOCKS CHAIN            00009000
         DC    H'0'               -NOT USED-                            00010000
*                                                                       00011000
*   SIMULATED OS CONTROL BLOCKS                                         00012000
*                                                                       00013000
FCBSECT  DSECT                                                          00014000
FCBINIT  DS    0X                 INTERESTING TIDBITS                   00015000
FCBOPCB  EQU   X'08'              OPEN ACQUIRED THIS CMS BLOCK          00016000
FCBPERM  EQU   X'04'              PERMANENT CONTROL BLOCK               00017000
FCBBATCH EQU   X'02'              SPECIAL BATCH DATA SET                00018000
FCBNEXT  DS    A                  AL3(NEXT CMSCB)                       00019000
FCBPROC  DS    A                  A(SPECIAL PROCESSING ROUTINE)         00020000
FCBDD    DS    CL8                 DATA DEFINITION NAME                 00021000
FCBOP    DS    CL8                CMS OPERATION                         00022000
IHAJFCB  DS    0D                 *** JOB FILE CONTROL BLOCK ***        00023000
JFCBDSNM DS    0X                 44 BYTES, DATA SET NAME               00024000
FCBTAPID DS    0X                 TAPE IDENTIFICATION                   00025000
FCBDSNAM DS    CL8                 DATA SET NAME                        00026000
FCBDSTYP DS    CL8                 DATA SET TYPE                        00027000
FCBPRPU  EQU   FCBDSTYP+4          PRINTER/PUNCH COMMAND LIST           00028000
FCBDSMD  DS    CL2                 DATA SET MODE                        00029000
FCBITEM  DS    H                  ITEM IDENTIFICATION NUMBER            00030000
FCBBUFF  DS    F                  A(INPUT-OUTPUT BUFFER)                00031000
FCBBYTE  DS    F                  DATA COUNT                            00032000
FCBFORM  DS    CL2                FILE FORMAT: FIXED/VARIABLE RECORDS   00033000
FCBCOUT  DS    H                  RECORDS PER CMS PHYSICAL BLOCK        00034000
FCBREAD  DS    F                  N'BYTES ACTUALLY READ                 00035000
FCBDEV   DS    X                  DEVICE TYPE CODE                      00036000
FCBDUM   EQU   0                   DUMMY DEVICE                         00037000
FCBPTR   EQU   4                   PRINTER                              00038000
FCBRDR   EQU   8                   READER                               00039000
FCBCON   EQU   12                  CONSOLE TERMINAL                     00040000
FCBTAP   EQU   16                  TAPE                                 00041000
FCBDSK   EQU   20                  DISK                                 00042000
FCBPCH   EQU   24                  PUNCH                                00043000
FCBCRT   EQU   28                  CRT                                  00044000
FCBMODE  DS    X                  MODE: 1,2,3,4,5                       00045000
FCBXTENT DS    H                  NUMBER OF ITEMS IN EXTENT             00046000
FCBRECL  DS    H                  DCB LRECL AT OPEN TIME                00047000
IOBIOFLG DS    X                  IO FLAG.. IOBIN OR IOBOUT
         DS    X                  ---NOT USED---                        00048000
         DS    F                  ---NOT USED---                        00049000
         DS    F                  ---NOT USED---                        00050000
         DS    F                  ---NOT USED---                        00051000
         DS    F                  ---NOT USED---                        00052000
FCBR13   DS    F                  SAVEAREA VECTOR R13                   00053000


FILE: CMSCB    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 002


FCBKEYS  DS    A                  A(DDS IN'CORE KEY TABLE)              00054000
FCBPDS   DS    A                  A(PDS IN-CORE DIRECTORY)              00055000
JFCBMASK DS    8X                 VARIOUS MASK BITS                     00056000
JFCBCRDT DS    3C                 DATA SET CREATION DATE (YDD)          00057000
JFCBXPDT DS    3C                 DATA SET EXPIRATION DATE (YDD)        00058000
JFCBIND1 DS    X                  INDICATOR ONE                         00059000
JFCBIND2 DS    X                  INDICATOR TWO                         00060000
JFCBUFNO DS    X                  NUMBER OF BUFFERS                     00061000
JFCBFTEK DS    0X                 BUFFERING TECHNIQUE                   00062000
JFCBFALN DS    X                  BUFFER ALIGNMENT                      00063000
JFCBUFL  DS    H                  BUFFER LENGTH                         00064000
JFCEROPT DS    X                  ERROR OPTION                          00065000
JFCKEYLE DS    X                  KEYLENGTH                             00066000
         DS    X                  ---NOT USED---                        00067000
JFCLIMCT DS    3X                 BDAM SEARCH LIMIT                     00068000
FCBDSORG DS    0X                 DATA SET ORGANIZATION                 00069000
JFCDSORG DS    2X                                                       00070000
FCBRECFM DS    0X                 RECORD FORMAT                         00071000
JFCRECFM DS    X                                                        00072000
JFCOPTCD DS    X                  OPTION CODES                          00073000
FCBBLKSZ DS    0H                 BLOCK SIZE                            00074000
JFCBLKSI DS    H                                                        00075000
FCBLRECL DS    0H                 LOGICAL RECORD LENGTH                 00076000
JFCLRECL DS    H                                                        00077000
FCBIOSW  DS    X                  I/O OPERATION INDICATOR               00078000
FCBIORD  EQU   X'01'              READ/GET                              00079000
FCBIOWR  EQU   X'02'              WRITE/PUT                             00080000
FCBCLOSE EQU   X'80'              DURING "CLOSE"                        00081000
FCBCLEAV EQU   X'40'              DISP=LEAVE DURING CLOSE               00082000
FCBPVMB  EQU   X'04'              PUT-MOVE-VAR-BLK                      00083000
FCBCASE  EQU   X'08'              ON=LOWER CASE CONSOLE I/O             00084000
         DS    1X                 ---NOT USED---                        00085000
DEBLNGTH DS    0X                 L'DEB IN DBLW WORDS                   00086000
         DS    F                  ---NOT USED---                        00087000
IHADEB   DS    0D                 *** DATA EXTENT BLOCK ***             00088000
DEBTCBAD DS    A                  A(MOVE-MODE USER BUFFER)              00089000
         DS    F                  ---NOT USED---                        00090000
DEBOFLGS DS    4X                 DATA SET STAUS FLAGS                  00091000
DEBOPATB DS    4X                 OPEN/CLOSE OPTION BYTE                00092000
IOBFLG   DS    0X                 (START OF IOB PREFIX FOR NORMAL SCH)  00093000
IOBBFLG  EQU   0                  DISPLACEMENT OF IOB FLAG IN IOB
IOBOUT   EQU   X'40'              "WRITE,PUT" IN PROGRESS               00094000
IOBIN    EQU   X'20'              "READ,GET" IN PROGRESS                00095000
IOBNXTAD DS    A                  A(NEXT BUFFER TO BE USED)             00096000
IOBECB   DS    F                  ECB FOR QSAM NORMAL SCHEDULING        00097000
IHAIOB   DS    0F                 *** INPUT/OUTPUT BLOCK ***            00098000
DEBDEBID DS    0X                 DEB IDENTIFICATION                    00099000
DEBDCBAD DS    A                  A(DATA CONTROL BLOCK)                 00100000
IOBECBCC DS    0X                 ECB COMPLETION CODE                   00101000
IOBBECBC EQU   12                 DISPLACEMENT OF ECB CODE IN IOB
IOBBECBP EQU   12                 DISPLACEMENT OF ECB PTR IN IOB
IOBECBPT DS    A                  A(EVENT CONTROL BLOCK)                00102000
IOBFLAG3 DS    0X                 I/O ERROR FLAG                        00103000
IOBBCSW  EQU   16                 DISPLACEMENT OF CSW IN IOB
IOBCSW   DS    8X                 LAST CCW STORED(I.E., RESIDUAL COUNT) 00104000


FILE: CMSCB    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 003


IOBSTART DS    A                  X'ID-NEXT BUFFER',AL3(INITIAL BUFFER) 00105000
IOBDCBPT DS    A                  A(DATA CONTROL BLOCK)                 00106000
IOBEND   DS    0X                 END-OF-INPUT/OUTPUT BLOCK             00107000
FCBEND   DS    0D                 END-OF FCB,JFCB,DEB,IOB BLOCKS        00108000
FCBENSIZ EQU   (*-FCBSECT)/8      SIZE OF FCB ENTRY, DOUBLEWORDS        00109000
         SPACE 3                                                        00110000
         ORG   FCBDSTYP+4                                               00111000
FCBIOOUT DS    CL8                SPECIAL I/O COMMAND LIST              00112000
FCBIOBUF DS    A                  A(DATA BUFFER)                        00113000
FCBCONCR DS    C                  CONSOLE COLOR CODE                    00114000
FCBCONMS DS    X                  CONSOLE MISCELLANEOUS INFO            00115000
FCBIOCNT DS    H                  L'DATA BUFFER                         00116000
         SPACE 3                                                        00117000
*                                                                       00118000
*   DATA EVENT CONTROL BLOCK                                            00119000
*                                                                       00120000
IHADECB  DSECT                                                          00121000
DECSDECB DS    F                  EVENT CONTROL BLOCK                   00122000
DECTYPE  DS    H                  TYPE OF I/O REQUEST                   00123000
DECBRD   EQU   X'80'              READ SF                               00124000
DECBWR   EQU   X'20'              WRITE SF                              00125000
DECLNGTH DS    H                  LENGTH OF KEY & DATA                  00126000
DECDCBAD DS    A                  V(DATA CONTROL BLOCK)                 00127000
DECAREA  DS    A                  V(KEY & DATA, BUFFER)                 00128000
DECIOBPT DS    A                  V(IOB)                                00129000
*              BDAM EXTENSION                                           00130000
DECKYADR DS    A                  V(KEY)                                00131000
DECRECPT DS    A                  V(BLOCK REFERENCE FIELD)              00132000
         SPACE 3                                                        00133000
*                                                                       00134000
*   SOME FREQUENTLY USED EQUATES                                        00135000
*                                                                       00136000
DDNAM    EQU   FCBDSTYP           FILETYPE = DATA SET NAME              00137000
BLK      EQU   X'10'              RECFM=BLOCKED RECORDS                 00138000
BS       EQU   X'20'              MACRF=BSAM                            00139000
DA       EQU   X'20'              DSORG=DIRECT ACCESS                   00140000
FXD      EQU   X'80'              RECFM=FIXED LENGTH RECORDS            00141000
IS       EQU   X'80'              DSORG=INDEXED SEQUENTIAL              00142000
LOC      EQU   X'08'              MACRF=LOCATE MODE                     00143000
MOV      EQU   X'10'              MACRF=MOVE MODE                       00144000
PS       EQU   X'40'              DSORG=PHYSICAL SEQUENTIAL             00145000
PO       EQU   X'02'              DSORG=PARTIONED ORGANIZATION          00146000
PREVIOUS EQU   X'80'              OFLGS=PREVIOUS I/O OPERATION          00147000
QS       EQU   X'40'              MACRF=QSAM                            00148000
UND      EQU   X'C0'              RECFM=UNDEFIN FORMAT RECORDS          00149000
VAR      EQU   X'40'              RECFM=VARIABLE LENGTH RECORDS         00150000
         MEND                                                           00151000


FILE: CMSREG   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01222000
         CMSREG                                                         01223000
*                                                                       01224000
*SYMBOLIC NAMES FOR REGISTERS.                                          01225000
*                                                                       01226000
* GENERAL REGISTERS.                                                    01227000
R0       EQU   0                                                        01228000
R1       EQU   1                                                        01229000
R2       EQU   2                                                        01230000
R3       EQU   3                                                        01231000
R4       EQU   4                                                        01232000
R5       EQU   5                                                        01233000
R6       EQU   6                                                        01234000
R7       EQU   7                                                        01235000
R8       EQU   8                                                        01236000
R9       EQU   9                                                        01237000
R10      EQU   10                                                       01238000
R11      EQU   11                                                       01239000
R12      EQU   12                                                       01240000
R13      EQU   13                                                       01241000
R14      EQU   14                                                       01242000
R15      EQU   15                                                       01243000
         SPACE 1                                                        01244000
* FLOATING POINT REGISTERS.                                             01245000
FR0      EQU   0                                                        01246000
FR2      EQU   2                                                        01247000
FR4      EQU   4                                                        01248000
FR6      EQU   6                                                        01249000
         MEND                                                           01250000


FILE: CMSTYPE  ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01152000
&LABEL   CMSTYPE &MESSAGE,&COLOR=B                                      01153000
&LABEL   DS      0H
         CNOP  0,4                                                      01156000
         BAL   1,SVC&SYSNDX
TYP&SYSNDX DC  CL8'TYPLIN'                                              01157000
         DC    A(MES&SYSNDX),C'&COLOR',AL3(L'MES&SYSNDX)                01158000
MES&SYSNDX DC  C&MESSAGE                                                01159000
SVC&SYSNDX SVC X'CA'                                                    01160000
         MEND                                                           01161000


FILE: CMSYSREF ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          00400010
         CMSYSREF  &OPTION                                              00400020
         AIF   (N'&OPTION NE 0).CSECT                                   00400030
*                                                                       00400040
*   GENERALIZED EQUATES FOR HARD-CORE QUANTITIES                        00400050
*                                                                       00400060
INSTAL   EQU   X'D0'              INSTALLATION HEADING                  00400070
VERIDENT EQU   X'B0'              VERSION IDENTIFICATION                00400080
DATEREAL EQU   X'98'              DATE: MM/DD/YY                        00400090
TIMEREAL EQU   X'A0'              TIME: HH.MM.SS                        00400100
CPSAVE   EQU   X'84'              CP "SAVESYS" HOOKS                    00400110
COMMVECT EQU   X'10'              V(COMMUNICATION VECTOR TABLE)         00400120
VSYSREF  EQU   X'14'              V(CMS SYSTEM REFERENCE TABLE)         00400130
COMMNAME EQU   X'90'              TERMINAL-ENTERED COMMAND NAME         00400140
PSWEXTO  EQU   X'18'              EXTERNAL OLD PSW                      00400150
PSWEXTN  EQU   X'58'              EXTERNAL NEW PSW                      00400160
PSWSVCO  EQU   X'20'              SUPERVISOR OLD PSW                    00400170
PSWSVCN  EQU   X'60'              SUPERVISOR NEW PSW                    00400180
PSWPRGO  EQU   X'28'              PROGRAM OLD PSW                       00400190
PSWPRGN  EQU   X'68'              PROGRAM NEW PSW                       00400200
PSWMACO  EQU   X'30'              MACHINE OLD PSW                       00400210
PSWMACN  EQU   X'70'              MACHINE NEW PSW                       00400220
PSWIOTO  EQU   X'38'              IN-/OUT-PUT OLD PSW                   00400230
PSWIOTN  EQU   X'78'              IN-/OUT-PUT NEW PSW                   00400240
CSW      EQU   X'40'              CHANNEL STATUS WORD                   00400250
NAMESYS  EQU   X'F8'              NAME OF IPLED SYSTEM                  00400260
*   STATUS BITS:                                                        00400270
*              -CSW + 4                                                 00400280
ATTN     EQU   X'80'              ATTENTION                             00400290
STMD     EQU   X'40'              STATUS MODIFIER                       00400300
CUE      EQU   X'20'              CONTROL UNIT END                      00400310
BUSY     EQU   X'10'              BUSY                                  00400320
CHE      EQU   X'08'              CHANNEL END                           00400330
DVE      EQU   X'04'              DEVICE END                            00400340
UNCK     EQU   X'02'              UNIT CHECK                            00400350
UNEX     EQU   X'01'              UNIT EXCEPTION                        00400360
*              -CSW + 5                                                 00400370
PCI      EQU   X'80'              PROGRAM-CONTROLLED INTERRUPTION       00400380
ICL      EQU   X'40'              INCORRECT LENGTH                      00400390
PGCK     EQU   X'20'              PROGRAM CHECK                         00400400
PTCK     EQU   X'10'              PROTECTION CHECK                      00400410
CHDK     EQU   X'08'              CHANNEL DATA CHECK                    00400420
CHCK     EQU   X'04'              CHANNEL CONTROL CHECK                 00400430
INCK     EQU   X'02'              INTERFACE CONTROL CHECK               00400440
CNCK     EQU   X'01'              CHAINING CHECK                        00400450
CAW      EQU   X'48'              CHANNEL ADDRESS WORD                  00400460
TIMER    EQU   X'50'              TIMER                                 00400470
*                                                                       00400480
*   DISPLACEMENTS WITHIN "NUCONSCT"                                     00400490
*                                                                       00400500
DCMSAREA EQU   X'1C'                                                    00400510
DCONSOLE EQU   X'300'                                                   00400520
DCORESIZ EQU   X'4'                                                     00400530
DERRINF  EQU   X'1C'                                                    00400540
DHIMAIN  EQU   X'DC'                                                    00400550


FILE: CMSYSREF ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 002


DIPLDEV  EQU   X'E0'                                                    00400560
DLDADDR  EQU   X'CC'                                                    00400570
DLDRTBL  EQU   X'14'                                                    00400580
DLOCCNT  EQU   X'C8'                                                    00400590
DLOWEXT  EQU   X'D8'                                                    00400600
DLSTADR  EQU   X'C4'
DLSTSVC  EQU   X'0'                                                     00400610
DNRMINF  EQU   X'1C'                                                    00400620
DPDISK   EQU   X'318'                                                   00400630
DPSW     EQU   X'D0'                                                    00400640
DSDISK   EQU   X'30C'                                                   00400650
DSTADDR  EQU   X'10'                                                    00400660
DSYSDEV  EQU   X'E2'                                                    00400670
DTBLNG   EQU   X'18'                                                    00400680
DTDISK   EQU   X'324'                                                   00400690
DUSFL    EQU   X'8'                                                     00400700
*                                                                       00400710
*   DISPLACEMENT WITHIN "SYSREF" --- COMMUNICATION VECTOR REGION        00400720
*                                                                       00400730
DADTLKP  EQU   156                                                      00400740
DADTLKW  EQU   192                                                      00400750
DADTP    EQU   40                                                       00400760
DADTS    EQU   76                                                       00400770
DADTT    EQU   64                                                       00400780
DBTYPLIN EQU   80                                                       00400790
DBUFFER  EQU   4                                                        00400800
DCMSOP   EQU   8                                                        00400810
DCMSRET  EQU   172                                                      00400820
DCOMBUF  EQU   188                                                      00400830
DDEVTAB  EQU   12                                                       00400840
DDIOSECT EQU   132                                                      00400850
DDMPEXEC EQU   124                                                      00400860
DDMPLIST EQU   100                                                      00400870
DEXEC    EQU   180                                                      00400880
DEXISECT EQU   200                                                      00400890
DFCBTAB  EQU   240                                                      00400900
DFREDBUF EQU   84                                                       00400910
DFREE    EQU   104                                                      00400920
DFRET    EQU   108                                                      00400930
DFSTLKP  EQU   16                                                       00400940
DFSTLKW  EQU   28                                                       00400950
DFVS     EQU   0                                                        00400960
DGETCLK  EQU   20                                                       00400970
DGFLST   EQU   24                                                       00400980
DIADT    EQU   36                                                       00400990
DIOERRSP EQU   48                                                       00401000
DIONTABL EQU   232                                                      00401010
DLNKLST  EQU   92                                                       00401020
DMACLIBL EQU   144                                                      00401030
DMACSECT EQU   148                                                      00401040
DNOTRKST EQU   176                                                      00401050
DNUMTRKS EQU   120                                                      00401060
DOPSECT  EQU   8                                                        00401070
DOSRET   EQU   168                                                      00401080
DOSVECT  EQU   100                                                      00401090


FILE: CMSYSREF ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 003


DOSTABLE EQU   136                                                      00401100
DPIE     EQU   32                                                       00401110
DPRTCLK  EQU   44                                                       00401120
DRDTK    EQU   52                                                       00401130
DSCAN    EQU   56                                                       00401140
DSCBPTR  EQU   212                                                      00401150
DSETCLK  EQU   112                                                      00401160
DSSTAT   EQU   60                                                       00401170
DSTART   EQU   184                                                      00401180
DSTATEXT EQU   164                                                      00401190
DSTRINIT EQU   96                                                       00401200
DSVCSECT EQU   152                                                      00401210
DSWITCH  EQU   68                                                       00401220
DSYSCTL  EQU   236                                                      00401230
DTABEND  EQU   72                                                       00401240
DTBL2311 EQU   204                                                      00401250
DTBL2314 EQU   208                                                      00401260
DTXTLIBS EQU   116                                                      00401270
DUPUFD   EQU   160                                                      00401280
DUSABRV  EQU   196                                                      00401290
DUSER1   EQU   216                                                      00401300
DUSER2   EQU   220                                                      00401310
DUSER3   EQU   224                                                      00401320
DUSER4   EQU   228                                                      00401330
DUSVCTBL EQU   140                                                      00401340
DWRTK    EQU   88                                                       00401350
*                                                                       00401360
         MEXIT                                                          00401370
.CSECT   ANOP                                                           00401380
*                                                                       00401390
*   SYSTEM COMMUNICATION VECTOR REGION                                  00401400
*                                                                       00401410
SYSREF   DS    0F                 TABLE OF EXTERNAL REFERENCES          00401420
*   NOTATION: ** = NOT USED, RESERVED   -- = DYNAMICALLY SET            00401430
*                                                                       00401440
         DC    V(FVS) .           0                                     00401450
         DC    V(BUFFER) .        4                                     00401460
         DC    V(CMSOP) .         8                                     00401470
         DC    V(DEVTAB) .        12                                    00401480
         DC    V(FSTLKP) .        16                                    00401490
         DC    V(GETCLK) .        20                                    00401500
         DC    A(GFLST) .         24                                    00401510
         DC    V(FSTLKW) .        28                                    00401520
         DC    V(PIE) .           32                                    00401530
         DC    V(IADT) .          36                                    00401540
         DC    V(ADTP) .          40                                    00401550
         DC    V(PRTCLK) .        44                                    00401560
         DC    V(IOERRSUP) .      48                                    00401570
         DC    V(RDTK) .          52                                    00401580
         DC    V(SCAN) .          56                                    00401590
         DC    A(0) SSTAT         --                                    00401600
         DC    V(ADTT) .          64                                    00401610
         DC    V(SWITCH) .        68                                    00401620
         DC    V(TABEND) .        72                                    00401630
         DC    V(ADTS) .          76                                    00401640


FILE: CMSYSREF ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 004


         DC    V(BTYPLIN) .       80                                    00401650
         DC    V(FREEDBUF) .      84                                    00401660
         DC    V(WRTK) .          88                                    00401670
         DC    V(LNKLST) .        92                                    00401680
         DC    V(STRINIT) .       96                                    00401690
         DC    V(DUMPLIST) .      100                                   00401700
         DC    V(FREE) .          104                                   00401710
         DC    V(FRET) .          108                                   00401720
         DC    V(SETCLK) .        112                                   00401730
         DC    V(TXTLIBS) .       116                                   00401740
         DC    V(NUMTRKS) .       120                                   00401750
         DC    V(DMPEXEC) .       124                                   00401760
         DC    V(FEIBM) .         128                                   00401770
         DC    V(DIOSECT) .       132                                   00401780
         DC    V(OSTABLE) .       136                                   00401790
         DC    V(USVCTBL) .       140                                   00401800
         DC    V(MACLIBL) .       144                                   00401810
         DC    V(MACSECT) .       148                                   00401820
         DC    V(SVCSECT) .       152                                   00401830
         DC    V(ADTLKP) .        156                                   00401840
         DC    V(UPUFD) .         160                                   00401850
         DC    A(0) SSTATEXT      --                                    00401860
         DC    V(OSRET) .         168                                   00401870
         DC    V(CMSRET) .        172                                   00401880
         DC    V(NOTRKST) .       176                                   00401890
         DC    V(EXEC) .          180                                   00401900
         DC    V(START) .         184                                   00401910
         DC    V(COMBUF) .        188                                   00401920
         DC    V(ADTLKW) .        192                                   00401930
         DC    V(USABRV) .        196                                   00401940
         DC    V(EXISECT) .       200                                   00401950
         DC    V(TBL2311) .       204                                   00401960
         DC    V(TBL2314) .       208                                   00401970
         DC    V(SCBPTR) .        212                                   00401980
         DC    A(3) USER1         **                                    00401990
         DC    A(3) USER2         **                                    00402000
         DC    A(3) USER3         **                                    00402010
         DC    A(3) USER4         **                                    00402020
         DC    V(IONTABL) .       232                                   00402030
         DC    V(SYSCTL) .        236                                   00402040
         DC    V(FCBTAB) .        240                                   00402050
*                                                                       00402060
         MEND                                                           00402070


FILE: DEVTABEX ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          DEV00010
         DEVTABEX &PREFIX=,&ADDINFO=NO                                  DEV00020
*        DATED:                   6 JUNE 1971                           DEV00030
*** PTF 'DEVTABEX AM3636CA' HAS BEEN APPLIED ***                        DEV00040
&PREFIX.EXTD DSECT                                                      DEV00050
         DS    0F                                                       DEV00060
         DS    CL2                     LOAD DEV EXT TABLE ADDRESS       DEV00070
         DS    CL4                     LOAD INST                        DEV00080
         DS    CL2                     BR INST                          DEV00090
&PREFIX.ENT  DS    0C                                                   DEV00100
&PREFIX.PSW  DS    CL8                     PSW OF ERROR  OPERATION      DEV00110
&PREFIX.CSW  DS    CL8                     CSW AT ERROR TIME            DEV00120
&PREFIX.ERBY DS    CL1                     ERROR STATUS                 DEV00130
*                                      C'Y' = I/O ERROR                 DEV00140
*                                      C'N' = NO I/O ERROR              DEV00150
*                                      C'R' = RETRY IOERR INTERNAL-     DEV00160
*                                             I/O OPERATION             DEV00170
*                                      C'E' = END OF FILE               DEV00180
*                                      C'U' = INTERVEN REQ DURING       DEV00190
*                                      C'I' = INCOR LEN                 DEV00200
*                                      ERROR RECOVERY                   DEV00210
&PREFIX.STAT        DS    CL1                     STATUS FIELD          DEV00220
*                                      EQU'S                            DEV00230
*                                      X'80' = SENSE OPERATION          DEV00240
*                                      X'40' = CE ONLY                  DEV00250
*                                      X'20' = DE ONLY                  DEV00260
*                                      X'10' = IGNORE INTERRUPTS        DEV00270
*                                      X'08' = WAIT SATISFIED           DEV00280
*                                      X'04' = CLOSIO ON                DEV00290
*                                      X'02' = READ 2-N TIMES           DEV00300
*                                      X'01' = RESERVED FOR FUTURE USE  DEV00310
&PREFIX.ERR  DS    XL2                 NUMBER OF LATEST-                DEV00320
*                                      I/O ERROR                        DEV00330
* NOTE: SEE IODEFTAB  MACRO IN IOERR FOR MATCHING NUMBER IN             DEV00340
*       ABOVE FEILD TO AN ERROR ENTRY. ALSO, THE FOLLOWING              DEV00350
*       NUMBERS HAVE SPECIAL MEANINGS:                                  DEV00360
*       X'AF00' = SIO OR DIAG BUSY CONDITION                            DEV00370
*       X'CF00' = INVALID CALLING LIST FROM I/O ROUTINT TO IOERR        DEV00380
&PREFIX.CT0  DS   H                     GENERAL COUNTER0                DEV00390
&PREFIX.CT   DS    H                       GENERAL COUNTER              DEV00400
&PREFIX.RT   DS    F                       INTERRUPT RT ADDRESS         DEV00410
&PREFIX.DT   DS    F                       DEV TAB ADDRESS              DEV00420
         AIF   ('&ADDINFO' EQ 'NO').NOMORE                              DEV00430
         DS    0D                      ALIGN ON D-WORD                  DEV00440
&PREFIX.CCW  DS  CL8                                                    DEV00450
&PREFIX.NOP  DS  CL8                                                    DEV00460
&PREFIX.WAIT DS  CL8                                                    DEV00470
&PREFIX.SYMD DS  CL4                                                    DEV00480
&PREFIX.ZERO DS  F                                                      DEV00490
&PREFIX.FLD  DS  F                                                      DEV00500
&PREFIX.WA   DS  CL80                   CARD IMAGE                      DEV00510
.NOMORE  ANOP                                                           DEV00520
         MEND                                                           DEV00530


FILE: DIOSCT   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01071000
         DIOSCT  &CSECT                                                 01072000
         AIF   (N'&CSECT NE 0).SEQ2                                     01073000
DIODSECT DSECT                                                          01074000
.SEQ2    ANOP                                                           01075000
IOOLD    DC    1D'0'          IO-OLD-PSW (FROM INTERRUPT ROUTINE)       01076000
DIOCSW   DC    1D'0'          CSW (FROM INTERRUPT ROUTINE)              01077000
*                                                                       01078000
*              WAIT CALLING SEQUENCE                                    01079000
*                                                                       01080000
         DS    0F                                                       01081000
PWAIT    DC    CL8'WAIT'                                                01082000
         DC    C'DSK-'        FILLED IN TO CORRECT SYMBOLIC DISK NO.    01083000
         DC    F'0'                                                     01084000
         DC    F'0'                                                     01085000
*                                                                       01086000
QQDSK1   DC    F'0'           1ST TWO BYTES ALWAYS = 0                  01087000
QQDSK2   EQU   QQDSK1+2       HALFWORD COPY OF 16TH TRACK DISK-ADDRESS  01088000
*                                                                       01089000
*              CCW CHAIN                                                01090000
*                                                                       01091000
CCW1     CCW   X'07',SEEKADR,CC,6       = SEEK                          01092000
CCW2     CCW   X'31',SEEKADR+2,CC,5     = SEARCH                        01093000
CCW3     CCW   X'08',*-8,0,1            = TIC BACK TO SEARCH            01094000
RWCCW    CCW   X'00',*-*,CC+SILI,*-*    = READ OR WRITE DATA            01095000
CCWNOP   CCW   X'03',0,SILI,1           = NO-OP FOR CE & DE TOGETHER    01096000
*                                                                       01097000
SEEKADR  DC    XL7'00'        SEEK/SEARCH INFO (1ST 3 BYTES ARE 0)      01098000
*                                                                       01099000
IOCOMM   DC    X'00'          SET TO READ (06) OR WRITE (05)            01100000
*                                                                       01101000
SENCCW   CCW   X'04',SENSB,SILI,6    = SENSE COMMAND (USED IF ERROR)    01102000
*                                                                       01103000
CC       EQU   X'40'          COMMAND-CHAIN                             01104000
SILI     EQU   X'20'          ...                                       01105000
*                                                                       01106000
*              I/O INFO                                                 01107000
*                                                                       01108000
LASTCYL  DC    F'0'           BECOMES 'LAST CYLINDER-NUMBER USED'       01109000
LASTHED  DC    F'0'           BECOMES 'LAST HEAD-NUMBER USED'           01110000
*                                                                       01111000
DEVTYP   DC    X'00'          UNIT-TYPE = 01 (2311), 08 (2314)          01112000
*                                                                       01113000
DIOFLAG  DC    X'00'          RDTK/WRTK FLAG:                           01114000
*                                                                       01115000
DIAGNG   EQU   X'80'          DIAGNOSE I/O INOPERATIVE                  01116000
*                                                                       01117000
TOOBIG   EQU   X'04'          BYTE-COUNT > 800, TOO BIG FOR DIAGNOSE IO 01118000
WRTKF    EQU   X'02'          WRITING FIRST CHAIN LINK                  01119000
QQTRK    EQU   X'01'          HANDLING FIRST CHAIN LINK                 01120000
*                                                                       01121000
DIAGNUM  EQU   24   **** NUMBER ASSIGNED BY 'CP' FOR DIAGNOSE I/O ****  01122000
*                                                                       01123000
SENSB    DC    XL6'00'        SENSE-INFORMATION                         01124000
*                                                                       01125000


FILE: DIOSCT   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 002


*        MISCELLANEOUS STORAGE...                                       01126000
*                                                                       01127000
DOUBLE   DC    1D'0'          (FOR 'CVD' USE)                           01128000
*                                                                       01129000
*        KEEP THE FOLLOWING THREE IN ORDER...                           01130000
XRSAVE   DS    15F            REGISTERS 0-14 SAVED HERE FOR RDTK-WRTK   01131000
         DC    AL3(0)         FIRST 3 BYTES OF R15 ERROR-CODE           01132000
ERRCODE  DC    AL1(*-*)       ERROR-CODE (IN R15 AT EXIT)               01133000
*                                                                       01134000
*        KEEP THE FOLLOWING TWO IN ORDER..                              01135000
FREER0   DC    F'0'           NO. DBL-WORDS OF FREE STORAGE (IF ANY)    01136000
DIOFREE  DC    F'0'           ADD. OF FREE STORAGE FOR BUFFER OR CCW'S  01137000
*                                                                       01138000
R1SAVE   DC    F'0'            ACTIVE-DISK-TABLE POINTER SAVED HERE     01139000
R2SAVE   DC    A(0)               A(DEVICE TABLE EXTENTION)             01140000
SAVECC   DC    F'0'               SIO CONDITION CODE                    01141000
DKIONORM DC    XL2'0C00'          NORMAL COMPLETION STATUS ("CE+DE")    01142000
DKIODE   DC    XL2'0400'          DEVICE END                            01143000
*              DEBUG CCW'S                                              01144000
DKSFP    CCW   X'1F',DKFPKEY,X'60',1   SET FILE MASK                    01145000
DKTIC    CCW   X'08',*,0,1   TIC TO SYSTEM                              01146000
DKFPKEY  DC    B'10000000'   WRITE KEY DATA                             01147000
DKNORM   EQU   C'N'               SUCCESSFUL DASD I/O                   01148000
DKERROR  EQU   C'Y'               UNSUCCESSFUL DASD I/O                 01149000
*                                                                       01150000
         MEND                                                           01151000


FILE: DJCB     ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01743000
         DJCB                                                           01744000
DJCB     DSECT                    JOB CONTROL LIMITS DSECT              01745000
         DS    0D                                                       01746000
JBUFF    DS    F                  ADDRESS OF SYSIN BUFFER               01747000
JREC     DS    F                  ADDRESS OF NEXT SYSIN LOGICAL         01748000
JNREC    DS    H                  SYSIN BLOCKING FACTOR                 01749000
JRECNO   DS    H                  NO OF THE NEXT LOGICAL REC IN BLOCK   01750000
JBUFSIZ  DS    F                  SIZE OF SYSIN BUFFER                  01751000
JWBUFF   DS    F             ADDRESS OF SYSOUT BUFFER                   01752000
JWREC    DS    F              ADDRESS OF NEXT SYSOUT LOGICAL REC        01753000
JNWREC   DS    H             BLOCKING FACTR OF SYSOUT                   01754000
JWRECN   DS    H             NO OF NEXT RECORD IN SYSOUT BUFFER         01755000
JWBUFSIZ DS    F                  SIZE OF SYSOUT BUFFER                 01756000
TIMINIT  DS    F                   INITIAL VALUE OF TIMER               01757000
JWLIM    DS    H                  NUMBER OF LINES PERMITTED             01758000
TIMLIM   DS    H                  NUMBER OF TIMER INTERRUPTS PERMITTED  01759000
JPFT07   DS    H                  NUMBER OF PUNCH CARDS PERMITTED       01760000
JSCLIM   DS    H                  NUMBER OF SC4020 FRAMES PERMITTED     01761000
JWTOT    DS    H                  NUMBER LINES WRITTEN THIS JOB         01762000
TIMTOT   DS    H                  NUMBER TIMER INTERRUPTS THIS JOB      01763000
JSCTOT   DS    H                  NUMBER OF SC4020 FRAMES THIS JOB      01764000
         MEND                                                           01765000


FILE: DTAPE    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01478000
         DTAPE                                                          01479000
DTAPE    DSECT                                                          01480000
         DS    0D                                                       01481000
TPLST    DS    8C                                                       01482000
TPOPTN   DS    8C                                                       01483000
TPUNIT   DS    4C                                                       01484000
TPMSK    DS    1X                                                       01485000
TPBUFA   DS    3X                                                       01486000
TPBUFL   DS    1F                                                       01487000
TPBYTR   DS    1F                                                       01488000
*                                                                       01489000
TPSAVE   DS    5F                                                       01490000
*                                                                       01491000
TPUTBL   DS    16C                                                      01492000
*                                                                       01493000
TPTBL    DS    8C                                                       01494000
*                                                                       01495000
TPMTBL   DS    4X                                                       01496000
*                                                                       01497000
TPIOSW   DS    4X                                                       01498000
         MEND                                                           01499000


FILE: EIOPL    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          ERP06920
         EIOPL                                                          ERP06930
EIOPL    DSECT                                                          ERP06940
EIOBEG        DS    0F                                                  ERP06950
EIOTYPE  DC    CL4' ' SIO OR CIO ERROR                                  ERP06960
EIODEVN  DS    CL4                     DEV NAME                         ERP06970
EIODEV1  DS    CL4                     ADDITIONAL DEV NAMES             ERP06980
EIODEV2  DS    CL4                     FOR ASYNCHRONOUS DEV             ERP06990
EIODEV3  DS    CL4                                                      ERP07000
EIOCSW   DS    CL8                     ERROR CSW                        ERP07010
EIOCAW   DS    CL4                     ADDRESS OF ERR OP                ERP07020
EIOSEEK  DS    CL7                     SEEK FIELD                       ERP07030
EIODVTYP DS    XL1                     DEVICE TYPE CODE                 ERP07040
EIODEVA  DS    CL2                     DEV ADDRESS IN HEX               ERP07050
EIOCC    DS    CL1                     STATUS OF ERROR RECOVERY         ERP07060
EIOSIOCC DS    CL1                     SIO OR DIAG CONDITION CODE       ERP07070
EIOUSER  DS    CL2                     VALUES:                          ERP07080
*                                      C'S ' = SYSTEM I/O ROUTINE       ERP07090
*                                      C'U ' = USER I/O                 ERP07100
*                                      C'ER' = IOERR                    ERP07110
EIODIAG  DS    CL2                     DIAG RETURN CD FROM CP           ERP07120
EIODTEXT DS    F                       DEV TAB EXT ADDRESS              ERP07130
EIOMESAD DS    F                       MESSAGE ADDRESS                  ERP07140
EIOMESLN DS    F                       MESSAGE LENGTH                   ERP07150
EIOREGS  DS    16F                     CALLER'S SAVE AREA               ERP07160
         MEND                                                           ERP07170


FILE: ERASE    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01396000
&LABEL   ERASE &FCB                                                     01397000
&LABEL   LA    1,&FCB                                                   01398000
         MVC   0(8,1),=CL8'ERASE'                                       01399000
         SVC   X'CA'                                                    01400000
         DC    AL4(*+4)                                                 01401000
         MEND                                                           01402000


FILE: ERPERRQ  ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          ERP07940
         ERPERRQ                                                        ERP07950
ERPERRQ  DSECT                                                          ERP07960
         DS    0F                                                       ERP07970
ERPDEVN  DS    CL4                     DEV NAME                         ERP07980
ERPCSW   DS    CL8                     ERROR CSW                        ERP07990
ERPCAW   DS    CL4                     ADDRESS OF ERROR OP              ERP08000
ERPSEEK  DS    XL7                     SEEK FIELD                       ERP08010
ERPDVTYP DS    XL1                     DEVICE TYPE CODE                 ERP08020
ERPDEVA  DS    CL2                     DEV ADDRESS IN HEX               ERP08030
ERPEC    DS    CL1                     INTERMEDIATE STATUS OF ERP       ERP08040
*                                      X'00' = SUCCESSFUL RETRY         ERP08050
*                                      X'08' = INTERNAL DEV NOT ATT     ERP08060
*                                      X'0E' = INTERNAL 'INTERVEN REQ'  ERP08070
*                                      X'EF' = NO RETRY POSSIBLE        ERP08080
*                                      X'FF' = RETRY FAILED             ERP08090
         DS    CL1                     FILLER                           ERP08100
ERPSS    DS    CL2                     STATUS OF SENSE                  ERP08110
ERPSEN   DS    CL6                     SENSE INFO                       ERP08120
         DS    0H                                                       ERP08130
ERPC1    DS    CL2                     COUNTER 1                        ERP08140
ERPC2    DS    CL2                     COUNTER 2                        ERP08150
ERPTC    DS    CL2                     TOTAL COUNTER                    ERP08160
ERPCSWFD DS    XL8                     ENTRY SEARCH CSW                 ERP08170
ERPSENFD DS    XL6                     ENTRY SEARCH SENSE               ERP08180
ERPWKFLD DS    CL8                     WORK AREA                        ERP08190
*                                      USES                             ERP08200
*                                      HOME ADDRESS INFO: CCHH          ERP08210
ERPERRSV DS    F                       SAVE AREA FOR ERROR ENTRY        ERP08220
ERPSTEND DS    0C                      END OF ERP ST                    ERP08230
         MEND                                                           ERP08240


FILE: ERPTRWT  ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          ERP00010
         ERPTRWT                                                        ERP00020
*        DATED:                   6 JUNE 1971                           ERP00030
*** PTF 'ERPTRWT AM3636CA' HAS BEEN APPLIED ***                         ERP00040
ERPTRWT  DSECT                                                          ERP00050
ERPBEGFD DS   0D                                                        ERP00060
ERPTYPE  DS    XL1                     SIO OR DIAG OR CIO ERROR         ERP00070
ERPSIOTY DS    XL1                     TYPE OF START OR DIAG ERROR      ERP00080
ERPTRACE DS    XL1                     ROUTINE TRACE BYTE               ERP00090
ERPSI    DS    XL1                     LOAD TRACE + SPECIAL INFO        ERP00100
*                                      2 WORK AREAS FOR                 ERP00110
*                                      ALL INTERNAL I/O OPERATIONS      ERP00120
*                                      FOLLOW:                          ERP00130
ERPADSEN DS    F                  ADDRESS OF SENSE                      ERP00140
ERPSENIN DS    CL6                     SEN INPUT AR                     ERP00150
         DS    CL2                     FILLER                           ERP00160
ERPFPOP  DS    CL8                     SSM CCW                          ERP00170
ERPSENOP DS    CL8                     CCW FOR SENSE                    ERP00180
ERPNOP   DS    CL8                     NOP CCW                          ERP00190
ERPFPOQ  DS    CL8                     INTERNAL IO AREA                 ERP00200
ERPSENOQ DS    CL8                                                      ERP00210
ERPNOPQ  DS    CL8                                                      ERP00220
ERPFLIST DS    0F                 PLIST FOR LOADING                     ERP00230
ERPFOP   DS    CL8                RT CALLED                             ERP00240
ERPFNAME DS    CL8                FILENAME                              ERP00250
ERPFTYPE DS    CL8                FILETYPE                              ERP00260
ERPFMODE DS    CL2                FILE MODE                             ERP00270
ERPFITEM DS    H                  FILE ITEM                             ERP00280
ERPFBUFF DS    F                  I/O AREA                              ERP00290
ERPFCT   DS    F                  DATA COUNT                            ERP00300
ERPFFORM DS    CL2                FILE FORMAT: FIXED/VARIABLE           ERP00310
ERPFBKCT DS    H                  RECORDS PER BLOCK                     ERP00320
ERPFRD   DS    F                  READ DATA COUNT                       ERP00330
ERPADDWT DS    CL4                ADDRESS OF WAIT                       ERP00340
ERPWT    DS    CL8                     WAIT PARM LIST                   ERP00350
ERPWTDEV DS    CL4                     DEV NAME                         ERP00360
ERPWTDTE DS    F                       FENSE1 (ADDRESS OF DEV EXT)      ERP00370
ERPWTID  DS    F                       INT DEV                          ERP00380
ERPDEV1  DS    F                       ADDITIONAL DEV NAMES             ERP00390
ERPDEV2  DS    F                       FOR ASYNCHRONOUS DEV             ERP00400
ERPDEV3  DS    F                       OR SIO BUSY CONDITION            ERP00410
ERPDEV4  DS    F                                                        ERP00420
ERPDEV5  DS    F                                                        ERP00430
ERPFF2   DS    F                       FENSE2 (ADDRESS OF DEV EXT)      ERP00440
ERPWTID2 DS    F                       INT DEV(2)                       ERP00450
ERPLDHDT DS    F                       TABLE RT'S SAVE AREA             ERP00460
ERPLDKT  DS    F                  TABLE RT'S CORE                       ERP00470
ERPLDHDR DS    F                       RECOVERY RT'S SAVE AREA          ERP00480
ERPLDKR  DS    F                  RECOVERY RT'S CORE                    ERP00490
ERPRTSV  DS    F                       STD INT RT ADDRESS               ERP00500
ERPRTSV1 DS    F                       CON INT RT ADDRESS               ERP00510
ERPRTS2  DS    F                       SYS INT RT ADDRESS               ERP00520
ERPRTSV3 DS    F                       RETURN ADDRESS SAVE              ERP00530
ERPRTSV4 DS    F                       REC RT'S MESSAGE AREA            ERP00540
ERPRTSW3 DS    F                  BASE REG SAVE                         ERP00550


FILE: ERPTRWT  ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 002


ERPRTSX3 DS    F                  NESTED RETURN ADDRESS SAVE            ERP00560
ERPRTSY3 DS    F                  NESTED BASE REG SAVE                  ERP00570
ERPRTSX4 DS    F                       N N RET ADDRESS                  ERP00580
ERPRTSY4 DS    F                       N N BASE REG SAVE                ERP00590
ERPRTSL1 DS    F                       N,N RET ADDRESS                  ERP00600
ERPRTSL2 DS    F                       N,N,N RET ADDRESS                ERP00610
ERPRTSL3 DS    F                       N,N,N,N RET ADDRESS              ERP00620
ERPFILL1 DS    F                       RESERVED                         ERP00630
*                                      IOERRREC + IOERRMES-             ERP00640
*                                      WORK AREA                        ERP00650
         DS    0D                      D-WORD ALIGN                     ERP00660
MESTABWK DS    CL24                    AREA FOR CONVERTING HEX TO       ERP00670
*                                      PRINTABLE CHARS AND BINARY       ERP00680
*                                      TO DECIMAL                       ERP00690
*                                      THREE F-AREAS USED FOR BXLE-     ERP00700
*                                      SEARCH LOOP                      ERP00710
LOOPCON1 DS    F                                                        ERP00720
LOOPCON2 DS    F                                                        ERP00730
LOOPCON3 DS    F                                                        ERP00740
HDDVNM   DS    XL1                     HOLD AREA FOR                    ERP00750
*                                      DEVICE NAME POINTER              ERP00760
RCSUP    DS    XL1                     SUPERIOR RET CODE                ERP00770
ERPDSEN1 DS    XL6                SENSE WORK AREA 1                     ERP00780
ERPDSEN2 DS    XL6                SENSE WORK AREA 2                     ERP00790
ERPEQC   DS    F                       ERROR QUEUE COUNT                ERP00800
         MEND                                                           ERP00810


FILE: EXISCT   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          00743000
         EXISCT &OPTION                                                 00744000
         AIF   (N'&OPTION NE 0).SEQ2                                    00745000
EXISECT  DSECT                                                          00746000
.SEQ2    ANOP                                                           00747000
*                                                                       00748000
*  STORAGE FOR TIMER INTERRUPT ...                                      00749000
*                                                                       00750000
EXSAVE   DS    16F _________ SAVED REGISTERS                            00751000
TYPLIST  DC    CL8'TYPE' ___ P-LIST TO TYPE BLIP-CHAR'S                 00752000
TIMCCW   DC    A(TIMCHAR)                                               00753000
         DC    C'B',AL3(1)                                              00754000
TIMCHAR  DC    X'FF',XL7'00' BLIP-CHARACTER(S)                          00755000
SCAW     DC    XL12'00' ____ SAVED CSW-CAW                              00756000
TIMINIT  DC    A(2000000/13) VALUE TO SET TIMER = 2 SECONDS             00757000
*                                                                       00758000
*  STORAGE FOR EXTERNAL (OTHER THAN TIMER) INTERRUPT ...                00759000
*                                                                       00760000
EXSAVE1  DS    16F _________ SAVED REGISTERS                            00761000
EXTPSW   DC    X'81000000' _ FILLED-IN PSW ...                          00762000
         DC    A(*-*)                                                   00763000
SAVEXT   DS    F      ______ TRANSFER-ADDRESS FOR EXT. INT.             00764000
         DC    V(IDEBUG) __  ADDRESS IN DEBUG FOR EXT. INT.
*                                                                       00765000
*  STORAGE FOR EXTERNAL INTERRUPT SET UP BY "TRAP" ...                  00766000
*                                                                       00767000
EXTRET   DS    D ___________ SAVED EXT-OLD-PSW                          00768000
JR0      DC    F'13' _______ 13 DBL-WORDS FOR FPRS & USER-SAVE-AREA     00769000
JR1      DC    A(*-*) ______ ADDRESS OF FREE STORAGE                    00770000
*                                                                       00771000
         MEND                                                           00772000


FILE: FCB      ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01328000
&LABEL   FCB   &NAME,&AREA                                              01329000
&LABEL   DS    0D                                                       01330000
         DC    CL8'        '                                            01331000
         DC    CL8'&NAME(1) '                                           01332000
         DC    CL8'&NAME(2) '                                           01333000
         DC    CL2'P5'                                                  01334000
         DC    H'0'                                                     01335000
         DC    A(&AREA)                                                 01336000
         DC    F'80'                                                    01337000
         DC    CL2'F'                                                   01338000
         DC    H'1'                                                     01339000
         DC    F'0'                                                     01340000
S&LABEL DC    CL8'STATE'                                                01341000
         DC    CL8'&NAME(1) '                                           01342000
         DC    CL8'&NAME(2) '                                           01343000
         DC    CL2'P5'                                                  01344000
         DC    H'0'                                                     01345000
         DC    F'0'                                                     01346000
         MEND                                                           01347000


FILE: FINIS    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01298000
&LABEL   FINIS &FCB                                                     01299000
&LABEL   LA    1,&FCB                                                   01300000
         MVC   0(8,1),=CL8'FINIS'                                       01301000
         SVC   X'CA'                                                    01302000
         DC    AL4(*+4)                                                 01303000
         MEND                                                           01304000


FILE: FREESCT  ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01893000
         FREESCT  &OPTION                                               01894000
         AIF   (N'&OPTION NE 0).CSECT                                   01895000
FREDSECT DSECT                                                          01896000
.CSECT   ANOP                                                           01897000
*                                                                       01898000
*   SYSTEM "FREE/FRET" STORAGE MANAGEMENT WORK AREA                     01899000
*                                                                       01900000
FREELIST DC    D'0' .             A(START OF FREE LIST CHAIN)           01901000
FREENUM  DC    H'0' .             N'BLOCKS IN FREE LIST                 01902000
         DC    X'00' .            ---NOT USED---                        01903000
FSW      DC    X'00' .            SWITCH BITS                           01904000
*   REGISTER AREAE                                                      01905000
JSAVE20  DC    16F'0' .           FREE: R2-R0.  FRET: R2-R1             01906000
JSAVE14  EQU   JSAVE20+48 .       R14: A(RETURN)                        01907000
JSAVE15  EQU   JSAVE20+52 .       A(FREE,FRET)                          01908000
JSAVE0   EQU   JSAVE20+56 .       R0: N'DBLE WORDS OF STORAGE           01909000
JSAVE1   EQU   JSAVE20+60 .       R1: A(FRET STORAGE)                   01910000
*   FREE/FRET VECTORS                                                   01911000
FREELOWR DC    F'262136' .        A(LOWEST POINT ACQUIRED)              01912000
FREEUPPR DC    F'0' .             A(HIGHEST POINT ACQUIRED)             01913000
AFREE    DC    A(FREE) .          V(FREE)                               01914000
AFRET    DC    A(FRET) .          V(FRET)                               01915000
         MEND                                                           01916000


FILE: FREEST   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01562000
         FREEST                                                         01563000
*********************************************************************** 01564000
*                                                                       01565000
*              STORAGE ADDRESSED VIA REGISTER 13                        01566000
*                                                                       01567000
*********************************************************************** 01568000
         SPACE 2                                                        01569000
FREEST   DSECT                                                          01570000
         SPACE 2                                                        01571000
GPRSAV   DS    3F             REGISTERS 9-12                            01572000
LOCSAV   DS    F                                                        01573000
RETT     DS    F                                                        01574000
LOCCT    DS    F              (LOCCNT) NEXT LOAD ADDRESS                01575000
BRAD     DS    F              (STADDR) EXECUTION ADDRESS                01576000
TBLREF   DS    F              (LDRTBL) TOP OF LOADER TABLE              01577000
CONS     DS    X              SWITCHES                                  01578000
SWS      DS    X              SWITCHES                                  01579000
TBLCT    DS    H                                                        01580000
MVC1     DS    3H             MVC WD(1),0(3)                            01581000
MVC2     DS    3H             MVC 0(1,3),WD                             01582000
WD       DS    F                                                        01583000
PLIST    DS    F              PARAMETER LIST POINTER (UPDATED)          01584000
RETREG   DS    F              RETURN ADDRESS                            01585000
SPEC     DS    200F           10 CARD INPUT BUFFER                      01586000
ESIDTB   DS    256H           256 ESD ENTRIES/OBJECT DECK               01587000
APSV     DS    16F                                                      01588000
TEMPST   DS    F                                                        01589000
TMPLOC   DS    2F                                                       01590000
RDBUF    DS    2F                                                       01591000
FNAME    DS    2F             FILE NAME                                 01592000
FTYPE    DS    2F             FILE TYPE                                 01593000
FMODE    DS    H              FILE MODE                                 01594000
RITEM    DS    H              NUMBER OF ITEMS                           01595000
RADD     DS    F              BUFFER ADDRESS                            01596000
RLENG    DS    F              BUFFER SIZE                               01597000
RFIX     DS    H              FIXED/VARIABLE FLAG BYTE                  01598000
RNUM     DS    H              NUMBER OF ITEMS                           01599000
NUMBYTE  DS    F              NUMBER OF BYTES ACTUALLY READ             01600000
CRDPTR   DS    F                                                        01601000
FINIS    DS    7F                                                       01602000
FLAGS    DS    F              LIBRARY SWITCHES, ETC.                    01603000
TYPLIN   DS    2F             TYPLIN PARAMETER LIST                     01604000
TYPEAD   DS    2F             TYPLIN BUFFER ADDRESS                     01605000
DSKLIN   DS    7F                                                       01606000
DSKAD    DS    13X                                                      01607000
HEXCON   DS    14X                                                      01608000
PACK     EQU   HEXCON                                                   01609000
UNPACK   EQU   HEXCON+5                                                 01610000
OUTPUT   DS    X                                                        01611000
OUTBUF   DS    100X                                                     01612000
PRCNT    DS    H              ADDRESS OF NEXT PRV "LOAD ADDRESS"        01613000
SAV2     DS    2F                                                       01614000
XEQPTR   DS    F                                                        01615000
ENTNAME  DS    CL8            'ENTRY' CONTROL CARD NAME                 01616000


FILE: FREEST   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 002


ENTADR   DS    F              'ENTRY' POINT LOADER TABLE ENTRY          01617000
MEMBOUND DS    F                                                        01618000
PLISTSAV DS    64D                                                      01619000
TXTLIBSV DS    64X                                                      01620000
ENDFREE  DS    0D                                                       01621000
         SPACE 2                                                        01622000
NEED     EQU   (ENDFREE-FREEST)/8                                       01623000
         MEND                                                           01624000


FILE: FSENTR   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01872000
&LABEL   FSENTR &RGSV                                                   01873000
         USING *,R15                                                    01874000
&LABEL   L     R15,AFVS -- A(FVS) INTO R15                              01875000
         USING FVSECT,R15                                               01876000
         STM   R0,R14,&RGSV -- SAVE R0 THRU 14                          01877000
         DROP  R15                                                      01878000
         LR    R13,R15 -- REFERENCE 'FVS' INFO                          01879000
         USING FVSECT,R13                                               01880000
         BALR  R12,0 -- OUR OWN ADDRESSABILITY                          01881000
         USING *,R12                                                    01882000
         MEND                                                           01883000


FILE: FST      ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01516000
&LABEL   FST   &NAME,&TYPE,&MASK                                        01517000
&LABEL   DS    0D            ALIGNMNENT                                 01518000
         DC    CL8'&NAME'    FILE NAME                                  01519000
         DC    CL8'&TYPE'    FILE TYPE                                  01520000
         AIF   (N'&MASK EQ 0).SEQ4                                      01521000
         DC    X'&MASK'      MASK                                       01522000
         AGO   .SEQ3                                                    01523000
.SEQ4    DC    X'00'          IGNORED                                   01524000
.SEQ3    AIF   ('&TYPE' NE '*').SEQ1                                    01525000
         DC    VL3(&NAME)     LOCATION                                  01526000
         AGO   .SEQ2                                                    01527000
.SEQ1    DC    AL3(0)         IGNORED                                   01528000
.SEQ2    DC    2H'1'          WRITE/READ POINTERS                       01529000
         DC    CL2'SY'       MODE --- SYSTEM                            01530000
         DC    H'0'          NO. OF ITEMS                               01531000
         DC    H'0'          ADDRESS OF 1ST CHAIN LINK                  01532000
         DC    CL1'F'        CHARACTERISTIC                             01533000
         DC    X'00'         FLAGS                                      01534000
         DC    F'80'         ITEM LENGTH                                01535000
         DC    H'0'          NO. OF 1/4 TRACKS                          01536000
         DC    H'0'          NOT USED                                   01537000
         MEND                                                           01538000


FILE: FSTB     ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          00857000
         FSTB                                                           00858000
*                                                                       00859000
* FILE STATUS TABLE (FILE DIRECTORY) BLOCK                              00860000
*                                                                       00861000
FSTSECT  DSECT                                                          00862000
FSTN     DS    1D             FILE NAME - 0                             00863000
FSTT     DS    1D             FILE TYPE - 8                             00864000
FSTD     DS    1F             DATE/TIME LAST WRITTEN - 16               00865000
FSTWP    DS    1H             WRITE POINTER (ITEM NO.) - 20             00866000
FSTRP    DS    1H             READ POINTER (ITEM NO.) - 22              00867000
FSTM     DS    1H             FILE MODE - 24                            00868000
FSTIC    DS    1H             ITEM COUNT - 26                           00869000
FSTFCL   DS    1H             FIRST CHAIN LINK - 28                     00870000
FSTFV    DS    1C             FIXED(F)/VARIABLE(V) FLAG - 30            00871000
FSTFB    DS    1C             FLAG BYTE (IF USED) - 31                  00872000
FSTIL    DS    1F             (MAXIMUM) ITEM LENGTH - 32                00873000
FSTDBC   DS    1H             800-BYTE DATA BLOCK COUNT - 36            00874000
FSTYR    DS    1H             YEAR - 38                                 00875000
FSTL     EQU   *-FSTSECT                                                00876000
*                                                                       00877000
*        "FSTFB" FLAG-BYTE DEFINITIONS                                  00878000
*                                                                       00879000
*        (APPLICABLE ONLY TO "STATEFST" COPY OF FST-ENTRY               00880000
*        AFTER SUCCESSFUL "STATE" OR "STATEW" CALL.)                    00881000
*                                                                       00882000
FSTFRO   EQU   X'00'          READ-ONLY DISK                            00883000
FSTFROX  EQU   X'40'          READ-ONLY EXTENSION OF READ-ONLY DISK     00884000
FSTFRW   EQU   X'80'          READ-WRITE DISK                           00885000
FSTFRWX  EQU   X'C0'          READ-ONLY EXTENSION OF READ-WRITE DISK    00886000
*                                                                       00887000
FSTFACT  EQU   X'07'          FILE "ACTIVE" - ONE OF THE FOLLOWING:     00888000
FSTFAR   EQU   X'04'          FILE ACTIVE FOR READING                   00889000
FSTFAW   EQU   X'02'          FILE ACTIVE FOR WRITING                   00890000
FSTFAP   EQU   X'01'          FILE ACTIVE FROM A "POINT"                00891000
*                                                                       00892000
*                                                                       00893000
*        FST HYPER-BLOCK PARAMETERS                                     00894000
*                                                                       00895000
FSTFWDP  EQU   800     FORWARD POINTER (TO NEXT HYPERBLOCK IN CORE)     00896000
FSTBKWD  EQU   804    BACKWARD POINTER (TO PREVIOUS HYPERBLOCK IN CORE) 00897000
*                                                                       00898000
         MEND                                                           00899000


FILE: FVS      ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          00900000
         FVS   &CSECT                                                   00901000
         AIF   (N'&CSECT NE 0).SEQ2                                     00902000
FVSECT   DSECT                                                          00903000
.SEQ2    ANOP                                                           00904000
DISK$SEG DS    15F    (1) FOR FSTLKP, FSTLKW, ACTLKP, TRKLKP, QQTRK     00905000
REGSAV3  DS    15F    (2) FOR RDBUF, WRBUF, FINIS, STATE, POINT         00906000
RWFSTRG  DS    18F    (3) REMAINING STORAGE FOR RDBUF, WRBUF, FINIS     00907000
*                                                                       00908000
ADTFVS   DC    2F'0'          ADTLKP                                    00909000
*                                                                       00910000
*        SAVE-AREA FOR LOWEST-LEVEL ROUTINES:                           00911000
*        E.G. READMFD, RELUFD, UPDISK, TYPSRCH, ADTLKW                  00912000
REGSAV0  DS    15F     -- (1) SAVED R0-R15                              00913000
         DC    AL3(00) -- (2) FIRST 3 BYTES OF RETURN-CODE              00914000
ERRCOD0  DC    AL1(*-*) -- (3) ERROR-CODE GOES HERE                     00915000
*                                                                       00916000
TRKLSAVE EQU   REGSAV0        FOR TRKLKP/X ONLY WHEN CALLED BY QQTRK/X  00917000
*                                                                       00918000
*        SAVE-AREA FOR NEXT-TO-LOWEST LEVEL ROUTINES:                   00919000
*        E.G. READFST, ERASE, ALTER, INTSVC-LOADMOD                     00920000
REGSAV1  DS    15F     -- (1)                                           00921000
         DC    AL3(00) -- (2)                                           00922000
ERRCOD1  DC    AL1(*-*) -- (3)                                          00923000
*                                                                       00924000
AACTLKP  DC    V(ACTLKP)                                                00925000
AACTNXT  DC    V(ACTNXT)                                                00926000
AACTFREE DC    V(ACTFREE)                                               00927000
AACTFRET DC    V(ACTFRET)                                               00928000
AADTLKP  DC    V(ADTLKP)                                                00929000
AADTNXT  DC    V(ADTNXT)                                                00930000
AFSTLKP  DC    V(FSTLKP)                                                00931000
AFSTLKW  DC    V(FSTLKW)                                                00932000
ARDTK    DC    V(RDTK)                                                  00933000
AWRTK    DC    V(WRTK)                                                  00934000
ATRKLKP  DC    V(TRKLKP)                                                00935000
ATRKLKPX DC    V(TRKLKPX)                                               00936000
AQQTRK   DC    V(QQTRK)                                                 00937000
AQQTRKX  DC    V(QQTRKX)                                                00938000
AADTLKW  DC    V(ADTLKW)                                                00939000
AERASE   DC    V(ERASE)                                                 00940000
ATYPSRCH DC    V(TYPSRCH)                                               00941000
AUPDISK  DC    V(UPDISK)                                                00942000
AKILLEX  DC    V(KILLEX)                                                00943000
ATFINIS  DC    V(TFINIS)                                                00944000
ARDBUF   DC    V(RDBUF)                                                 00945000
AWRBUF   DC    V(WRBUF)                                                 00946000
AFINIS   DC    V(FINIS)                                                 00947000
ASTATE   DC    V(STATE)                                                 00948000
ASTATEW  DC    V(STATEW)                                                00949000
APOINT   DC    V(POINT)                                                 00950000
F65535   DC    F'65535'       = X'0000FFFF'                             00951000
*                                                                       00952000
F4       DC    F'4'                                                     00953000
H4       EQU   F4+2                                                     00954000


FILE: FVS      ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 002


         SPACE 2                                                        00955000
AFREE    DC    V(FREE) -- (1)                                           00956000
F100     DC    F'100' -- (2)                                            00957000
*                                                                       00958000
AFRET    DC    V(FRET)        (INTO R15)                                00959000
JSR0     DC    F'0'           R0 AND ...                                00960000
JSR1     DC    F'0'           R1 SAVED HERE FOR FRET CALLS.             00961000
*                                                                       00962000
*        PARAMETER-LIST TO READ/WRITE MFD...                            00963000
RWMFD    DC    A(*-*) -- CORE-ADDRESS                                   00964000
F800     DC    F'800' -- 800 BYTES                                      00965000
         DC    A(H4)                                                    00966000
FVSDSKA  DC    A(*-*) -- ADD. OF ACTIVE-DISK-TABLE                      00967000
*                                                                       00968000
DSKLST   DS    0F             ALL-PURPOSE RDTK/WRTK P-LIST...           00969000
DSKLOC   DC    A(*-*)         CORE LOC. OF ITEM                         00970000
RWCNT    DC    A(*-*)         BYTE-COUNT (USUALLY 800)                  00971000
DSKADR   DC    A(*-*)         DISK ADDRESS OF ITEM                      00972000
ADTADD   DC    A(*-*)         ADDRESS OF CORRECT ACTIVE-DISK-TABLE      00973000
*                                                                       00974000
FINISLST DC    CL8'FINIS'     P-LIST TO CLOSE ALL FILES                 00975000
         DC    CL8'*'                                                   00976000
         DC    CL8'*'                                                   00977000
         DC    CL2'*'                                                   00978000
*                                                                       00979000
         DS    0H             HALFWORD CONSTANTS ...                    00980000
FFF      DC    X'FFFF'        MEANS NO SIGNIFICANT DATA PAST 215TH BYTE 00981000
FFE      DC    X'FFFE'       1968-ERA MFD STILL SUPPORTED ON INPUT ONLY 00982000
FFD      DC    X'FFFD'        NEWEST SIGNAL FOR FULL 2314 HANDLING      00983000
*                                                                       00984000
*        'SIGNAL' = SCRATCH HALFWORD USED BY READMFD OR ERASE...        00985000
SIGNAL   DC    H'0'           = 0000, X'FFFF', X'FFFE', OR X'FFFD'      00986000
SWTCH    EQU   SIGNAL+1       00, FF, FE, OR FD                         00987000
*                                                                       00988000
UFDBUSY  DC    X'00'          NONZERO MEANS 'UFD IS BUSY BEING UPDATED' 00989000
KXFLAG   DC    X'00'          NONZERO MEANS 'KX' DESIRED ASAP.          00990000
EXTFLAG  DC    X'00'          NONZERO MEANS EXTERNAL INTERRUPT WANTED.  00991000
FLGSAVE  DC    X'00'          FOR SCRATCH USE (E.G. BY RELUFD)          00992000
*                                                                       00993000
*        FLAG BITS FOR 'UFDBUSY' FLAG...                                00994000
WRBIT    EQU   X'80'          WRBUF                                     00995000
UPBIT    EQU   X'40'          UPDISK - READMFD                          00996000
FNBIT    EQU   X'20'          FINIS                                     00997000
ERBIT    EQU   X'10'          ERASE - ALTER - READFST                   00998000
DIOBIT   EQU   X'08'          RDTK/WRTK                                 00999000
*                                                                       01000000
FVSFLAG  DC    X'00'          (FOR GENERAL USE - AS NEEDED)             01001000
*                                                                       01002000
*        MISCELLANEOUS STORAGE USED BY ERASE (OR ALTER) ....            01003000
ERSFLAG  DC    X'00'          FLAG FOR USE BY ERASE OR ALTER            01004000
*                                                                       01005000
FVSERAS0 DC    F'0'           (1) - R0 TO/FROM FSTLKW (FOR ERASE)       01006000
FVSERAS1 DC    F'0'           (2) - R1 TO ACTLKP OR FSTLKW (FOR ERASE)  01007000
FVSERAS2 DC    F'0'           (3) ADDRESS OF FREE STORAGE USED BY ERASE 01008000
         SPACE 1                                                        01009000


FILE: FVS      ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 003


*                                                                       01010000
*   FILE STATUS TABLE (FST) COPY FROM "STATE"                           01011000
*                                                                       01012000
STATEFST DS    0D                 FULL FST OF 'STATED' FILE             01013000
FVSFSTN  DC    D'0'               FILENAME -0                           01014000
FVSFSTT  DC    D'0'               FILETYPE -8                           01015000
FVSFSTDT DC    2H'0'              DATE/TIME LAST WRITTEN -16,18         01016000
FVSFSTWP DC    H'0'               WRITE POINTER (ITEM ID) -20           01017000
FVSFSTRP DC    H'0'               READ POINTER (ITEM ID) -22            01018000
FVSFSTM  DC    H'0'               FILEMODE -24                          01019000
FVSFSTIC DC    H'0'               N'ITEMS IN FILE -26                   01020000
FVSFSTCL DC    H'0'               DA(FIRST CHAIN LINK) -28              01021000
FVSFSTFV DC    C' '               FIXED(F) / VARIABLE(V) INDICATOR -30  01022000
FVSFSTFB DC    X'00'              FLAG BYTE -31                         01023000
FVSFSTIL DC    F'0'               L'ITEMS -32                           01024000
FVSFSTDB DC    H'0'               N'DATA BLOCKS -36                     01025000
FVSFSTYR DC    2C' '              YEAR LAST WRITTEN -38                 01026000
*                                                                       01027000
FVSFSTAD DC    A(0)               A(ADT FOR THIS FILE)                  01028000
STATER0  EQU   FVSFSTAD                                                 01029000
FVSFSTAC DC    A(0)               A(REAL FST ENTRY FOR THIS FILE)       01030000
STATER1  EQU   FVSFSTAC                                                 01031000
*                                                                       01032000
         MEND                                                           01033000


FILE: GETCC    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01417000
&LABEL   GETCC &SYSCB,&ERROR=                                           01418000
&LABEL   LA    1,&SYSCB                                                 01419000
         L     15,=V(SYSCTL)                                            01420000
         MVC   8(8,1),=CL8'GETCC'                                       01421000
         BALR  14,15                                                    01422000
         AIF   (N'&ERROR EQ 0).SEQ2                                     01423000
         DC    AL4(&ERROR)                                              01424000
         AGO   .SEQ3                                                    01425000
.SEQ2    DC    AL4(*+4)                                                 01426000
.SEQ3    SPACE 1                                                        01427000
         MEND                                                           01428000


FILE: GOTO     ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          ERP03440
         GOTO  &WHERE                                                   ERP03450
         B     &WHERE                                                   ERP03460
         MEND                                                           ERP03470


FILE: IF       ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          ERP03260
         IF    &COND=,&GORT=,&GOREG=                                    ERP03270
         AIF   (T'&GORT EQ 'O').REG                                     ERP03280
         BC    &COND,&GORT                                              ERP03290
         MEXIT                                                          ERP03300
.REG     ANOP                                                           ERP03310
         BCR   &COND,&GOREG                                             ERP03320
         MEND                                                           ERP03330


FILE: IFC      ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          ERP03390
         IFC   &FLD=,&COMP=,&CONDC=,&GOCRT=,&GOCREG=                    ERP03400
         CLI   &FLD,&COMP                                               ERP03410
         IF    COND=&CONDC,GORT=&GOCRT,GOREG=&GOCREG                    ERP03420
         MEND                                                           ERP03430


FILE: IFT      ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          ERP03340
         IFT   &FLD=,&COMP=,&CONDT=,&GOTRT=,&GOTREG=                    ERP03350
         TM    &FLD,&COMP                                               ERP03360
         IF    COND=&CONDT,GORT=&GOTRT,GOREG=&GOTREG                    ERP03370
         MEND                                                           ERP03380


FILE: IO       ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          00100010
         IO    &OPTION                                                  00100020
         AIF   (N'&OPTION NE 0).SEQ2                                    00100030
OPSECT   DSECT                                                          00100040
.SEQ2    ANOP                                                           00100050
*                                                                       00100060
*   COMMANDER-IN-CHIEF OF ALL I/O OPERATION LISTS                       00100070
*                                                                       00100080
PLIST    DS    0D                                                       00100090
CMSOP    DS    CL8                 I/O OPERATION COMMAND WORD           00100100
FILENAME DS    CL8                 FILE NAME                            00100110
FILETYPE DS    CL8                 FILE TYPE                            00100120
FILEMODE DS    CL2                 FILE MODE                            00100130
FILEITEM DS    H                   ITEM IDENTIFICATION NUMBER           00100140
FILEBUFF DS    F                   INPUT-OUTPUT BUFFER                  00100150
FILEBYTE DS    F                   DATA COUNT                           00100160
FILEFORM DS    CL2                 FILE FORMAT: FIXED/VARIABLE RECORDS  00100170
FILECOUT DS    H                  RECORDS PER BLOCK                     00100180
FILEREAD DS    F                   READ DATA COUNT                      00100190
POINTERS EQU   FILEITEM                                                 00100200
AFST     EQU   FILEBUFF                                                 00100210
*                                                                       00100220
IOAREA   EQU   FILEBUFF       BUFFER AREA LOCATION                      00100230
IOLENGTH EQU   FILEBYTE       BUFFER LENGTH                             00100240
*                                                                       00100250
*   IMMEDIATE REGISTER SAVE ARE                                         00100260
*                                                                       00100270
*                                                                       00100280
SAVER14  DC    F'0'           TEMP R14 SAVE                             00100290
SAVER15  DC    F'0'           TEMP R15 SAVE                             00100300
SAVER0   DC    F'0'           TEMP R0 SAVE                              00100310
SAVER1   DC    F'0'           TEMP R1 SAVE                              00100320
*                                                                       00100330
FCBIO    DC    A(0)               A (LAST FCB USED DURING AN I/O OPER)  00100340
CMSNAME  DC    CL8'FILE    '      "DEFAULT FILENAME"
         DC    F'0'               ---NOT USED---                        00100360
*                                                                       00100370
*   CONSOLE PARAMETER LISTS                                             00100380
*                                                                       00100390
         DS    0F                                                       00100400
*   READ CONSOLE                                                        00100410
CONREAD  DC    CL8'WAITRD'         TERMINAL READ                        00100420
CONRDBUF DC    A(INPBUF)           INPUT BUFFER                         00100430
CONRDCOD DC    C'U'                TRANSLATE CODE                       00100440
         DC    X'00'                                                    00100450
CONRDCNT DC    AL2(*-*)            DATA BYTE COUNT                      00100460
NUMCH    DC    F'0'               N'BYTES SCANNED                       00100470
INPBUF   DC    132C' '            INPUT BUFFER                          00100480
*   CONSOLE WAIT LIST                                                   00100490
WAITLIST DS    0F                                                       00100500
         DC    CL8'CONWAIT'                                             00100510
*   WRITE CONSOLE                                                       00100520
CONWRITE DS    0F                                                       00100530
         DC    CL8'TYPLIN'                                              00100540
CONWRBUF DC    A(*-*)              LOCATION OF MESSAGE TEXT             00100550


FILE: IO       ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 002


CONWRCOD DC    C'B'                COLOR CODE                           00100560
         DC    X'00'                                                    00100570
CONWRCNT DC    AL2(*-*)            LENGTH OF MESSAGE TEXT               00100580
*                                                                       00100590
*   READER PARAMETER LIST                                               00100600
*                                                                       00100610
         DS    0F                                                       00100620
READLST  DC    CL8'CARDRD'                                              00100630
RDBUFF   DC    A(0)                BUFFER ADDRESS                       00100640
RDCCW    DC    H'0'               CCW BYTE COUNT                     TR 00100650
RDCOUNT  DC    H'0'               BYTES ACTUALLY READ                TR 00100660
*                                                                       00100670
*   CARD PUNCH PARAMETER LIST                                           00100680
*                                                                       00100690
PUNCHLST DS    0F                                                       00100700
         DC    CL8'CARDPH'                                              00100710
PUNBUFF  DC    A(0)               PUNCH BUFFER ADDRESS                  00100720
PUNCOUNT DC    A(0)               PUNCH CCW COUT                        00100730
*                                                                       00100740
*   PRINTER PARAMETER LIST                                              00100750
*                                                                       00100760
PRINTLST DS    0F                                                       00100770
         DC    CL8'PRINTR'                                              00100780
PRBUF    DC    A(0)                PRINTER BUFFER ADDRESS               00100790
PRCNT    DC    A(0)                PRINT DATA COUNT                     00100800
*                                                                       00100810
*   TAPE PARAMETER LIST                                                 00100820
*                                                                       00100830
TAPELIST DS    0F                                                       00100840
         DC    CL8'TAPEIO'                                              00100850
TAPEOPER DC    CL8' '              TAPE OPERATION COMMAND               00100860
TAPEDEV  DC    CL4'TAP2'           TAPE SYMBOLIC DEVICE                 00100870
TAPEMASK DC    X'93'               SET MODE                             00100880
TAPEBUFF DC    AL3(0)              BUFFER LOCATION                      00100890
TAPESIZE DC    F'0'                                                     00100900
TAPECOUT DC    F'0'               TAPE COUNTER                          00100910
*                                                                       00100920
*   CLOSE OUT DEVICE DEPENDENT DATA SET ON UNIT RECORD EQUIPMENT        00100930
*                                                                       00100940
CLOSIO   DS    0F                                                       00100950
         DC    CL8'CLOSIO'        OPERATION                             00100960
CLOSIODV DC    CL8' '             DEVICE TYPE                           00100970
         DC    4X'FF'                                                   00100980
*                                                                       00100990
*   CMS BATCH MONITOR INPUT-OUTPUT PARAMETER LIST                       00101000
*                                                                       00101010
RDSYS    DS    0D                                                       00101020
         DC    CL8'SYSCTL'                                              00101030
         DC    CL8'READ '                                               00101040
         DC    A(0)                                                     00101050
         DC    F'0'                                                     00101060
         SPACE                                                          00101070
*                                                                       00101080
WRSYS    DS    0D                                                       00101090
         DC    CL8'SYSCTL'                                              00101100


FILE: IO       ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 003


         DC    CL8'WRITE '                                              00101110
         DC    A(0)                                                     00101120
         DC    F'0'                                                     00101130
*                                                                       00101140
*                                                                       00101150
*   STORAGE FOR EXEC BOOTSTRAP:                                         00101160
EXLEVEL  DC    F'0'           EXEC "LEVEL"                              00101170
EXF1     DC    F'1'           (FOLLOWS EXLEVEL)                         00101180
EXNUM    DC    F'0'           NUMBER DBL-WORDS FREE STORAGE             00101190
EXADD    DC    F'0'           ADDRESS OF "EXECTOR" CORE-IMAGE           00101200
*                                                                       00101210
*                                                                       00101220
*   STORAGE FOR OVERNUC BOOTSTRAP:                                      00101230
OVERNUM  DC    F'0'           NUMBER DBL-WORDS FREE STORAGE             00101240
OVERADD  DC    F'0'           ADDRESS OF "OVERRIDE" CORE-IMAGE          00101250
*                                                                       00101260
         MEND                                                           00101270


FILE: IOGENE   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          IOG00010
         IOGENE &TYPE=(XL1,XL2,XL1,CL1,CL1,AL2,XL1,PL2,XL1,XL1,XL1,    -IOG00020
               XL1,FL1,XL1,XL1,XL1),&VALUE=                             IOG00030
         LCLA  &N,&LPCT                                                 IOG00040
         LCLC  &BRACK1,&DC,&BRACK2                                      IOG00050
*        DATED:                   6 JUNE 1971                           IOG00060
*** PTF 'IOGENE AM3636CA' HAS BEEN APPLIED ***                          IOG00070
&LPCT    SETA  N'&TYPE+&LPCT                                            IOG00080
&N       SETA  1+&N                                                     IOG00090
&DC      SETC  'DC'                                                     IOG00100
.LP      ANOP                                                           IOG00110
         AIF   ('&TYPE(&N)'(1,1) EQ 'A').PAR                            IOG00120
&BRACK1  SETC  ''''                                                     IOG00130
&BRACK2  SETC  ''''                                                     IOG00140
.DCST    ANOP                                                           IOG00150
         &DC   &TYPE(&N)&BRACK1&VALUE(&N)&BRACK2                        IOG00160
         AIF   (&N EQ &LPCT).DONE                                       IOG00170
&N       SETA  &N+1                                                     IOG00180
         AGO   .LP                                                      IOG00190
.PAR     ANOP                                                           IOG00200
&BRACK1  SETC  '('                                                      IOG00210
&BRACK2  SETC  ')'                                                      IOG00220
         AGO   .DCST                                                    IOG00230
.DONE    ANOP                                                           IOG00240
         DC    XL1'00'                                                  IOG00250
         DC    9XL2'0000'              ERROR CT1-9                      IOG00260
         MEND                                                           IOG00270


FILE: IOGENRT  ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          ERP08310
         IOGENRT &RTTYPE                                                ERP08320
&RTTYPE  START   0                                                      ERP08330
*                                 EXTRN'S                               ERP08340
         EXTRN SYSREF                                                   ERP08350
*        EXTRN IOERRSUP,FREE,FRET    REFERENCED BY SYSREF               ERP08360
*                                 ENTRY'S                               ERP08370
*                                      NONE                             ERP08380
*                                      REGISTER USAGE                   ERP08390
*                                      DEFINED IN IOERR                 ERP08400
*                                      AND SUBDEFINED IN DKRT-          ERP08410
*                                      PROCEDURES                       ERP08420
         EJECT                                                          ERP08430
*                                      DSECT'S                          ERP08440
         CMSREG                                                         ERP08450
         EJECT                                                          ERP08460
DIODSCT  DSECT                                                          ERP08470
         DIOSCT NODS                                                    ERP08480
         EJECT                                                          ERP08490
         ADT                                                            ERP08500
         EJECT                                                          ERP08510
         MESDKRCD                                                       ERP08520
         EJECT                                                          ERP08530
         DEVTABEX PREFIX=GRN1                                           ERP08540
         EJECT                                                          ERP08550
         EIOPL                                                          ERP08560
         EJECT                                                          ERP08570
         ERPTRWT                                                        ERP08580
         EJECT                                                          ERP08590
         ERPERRQ                                                        ERP08600
         EJECT                                                          ERP08610
         MESIN1                                                         ERP08620
         EJECT                                                          ERP08630
         CMSYSREF                                                       ERP08640
         EJECT                                                          ERP08650
&RTTYPE   CSECT                                                         ERP08660
*                                 USING STATEMENTS                      ERP08670
*                                      DEFINED IN TABLE OR MACRO        ERP08680
*                                 GLOBAL EQU'S                          ERP08690
*SYMBOL  EQU   VALUE              MEANING                               ERP08700
* CAW      EQU   X'48'              CAW LOCATION                        ERP08710
DVNOTATT EQU   X'3F'                     DEVICE NOT OPER=NOT ATT        ERP08720
ERRINERR EQU   C'E'                    REQ FOR IOERR FROM IOERR         ERP08730
         EJECT                                                          ERP08740
         IOGENTAB  TABTYPE=&RTTYPE                                      ERP08750
         MEND                                                           ERP08760


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          IOG00010
         IOGENTAB  &TABTYPE=                                            IOG00020
         LCLC  &T1052,&T1403,&T2540P,&T2540R                            IOG00030
         LCLC  &O1403                                                   IOG00040
         LCLC  &T2400,&T2404,&T2402,&T2403                              IOG00050
*        DATED:                   6 JUNE 1971                           IOG00060
*** PTF 'IOGENTAB AM3636CA' HAS BEEN APPLIED ***                        IOG00070
.* SOURCE FOR TABLES (IONUTAB + IOFRTAB) AND MACRO (IOMACTAB)           IOG00080
&T1052   SETC  '1052'                                                   IOG00090
&T1403   SETC  '1403'                                                   IOG00100
&T2540P  SETC  '2540P'                                                  IOG00110
&T2540R  SETC  '2540R'                                                  IOG00120
&O1403   SETC  '1403'                                                   IOG00130
&T2400   SETC  '2400'                                                   IOG00140
&T2404   SETC  '2404'                                                   IOG00150
&T2402   SETC  '2404'                                                   IOG00160
&T2403   SETC  '2403'                                                   IOG00170
         AIF   ('&TABTYPE'(3,3) NE 'MAC').NODSCT                        IOG00180
IOMACTAB DSECT                                                          IOG00190
.NODSCT  ANOP                                                           IOG00200
         AIF   ('&TABTYPE' EQ 'IONUTAB').RETYPE                         IOG00210
         AGO   .CONT1                                                   IOG00220
.RETYPE  ANOP                                                           IOG00230
&T1052   SETC  '2311'                                                   IOG00240
&T1403   SETC  '2311'                                                   IOG00250
&T2540P  SETC  '2311'                                                   IOG00260
&T2540R  SETC  '2311'                                                   IOG00270
&O1403   SETC  '2311'                                                   IOG00280
&T2400   SETC  '2311'                                                   IOG00290
&T2404   SETC  '2311'                                                   IOG00300
&T2402   SETC  '2311'                                                   IOG00310
&T2403   SETC  '2311'                                                   IOG00320
.CONT1   ANOP                                                           IOG00330
         DS    0D                                                       IOG00340
ERPTABLE EQU   *                                                        IOG00350
*                                                                       IOG00360
*                                 TABLES FOR ERROR RECOVERY             IOG00370
*                                 THE FOUR TYPES OF TABLES ARE:         IOG00380
*                                 DEVICE TYPE TABLE                     IOG00390
*                                 DEVICE RECOVERY TABLE:                IOG00400
*                                 1.ERROR ENTRIES                       IOG00410
*                                 2.ERROR RECOVERY PROCEDURES           IOG00420
*                                 DEVICE OPERATION CODE TABLE           IOG00430
*                                 DEVICE MESSAGE TABLE                  IOG00440
         EJECT                                                          IOG00450
*                                 DEVICE TYPE TABLE                     IOG00460
*                                 CHARACTERISTICS                       IOG00470
ADDTYPDL DC    CL4'****'           START OF TABLE DELIMITER             IOG00480
ADDTYPNO DC    AL2((TYPTB2-TYPTB1)/(TYPDT2-TYPDT1))   NO.               IOG00490
*                                                   OF ENTRIES          IOG00500
ADDTYPDT DC    AL2(TYPDT2-TYPDT1) SIZE OF EACH ENTRY                    IOG00510
ADDTYPTB DC    AL2(TYPTB2-TYPTB1) SIZE OF TABLE                         IOG00520
ADDTYPSN DC    AL2(0)             UNUSED                                IOG00530
*                                                                       IOG00540
*                                 ENTRIES                               IOG00550


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 002


*                                 DEVICE TYPE CODE    XL1               IOG00560
*                                 RECOVERY TABLE LOCATION    AL3        IOG00570
*                                 DEVICE NAME CODE           XL1        IOG00580
*                                 OPERATION TABLE LOCATION   AL3        IOG00590
*                                                                       IOG00600
TYPTABST EQU   *                  START OF TYPE TABLE                   IOG00610
TYPTB1   EQU   *                  CALCULATION (CAL) LOCATION            IOG00620
TYPDT1   EQU   *                  CAL LOC                               IOG00630
TYP1052  DC    XL1'00'                                                  IOG00640
         DC    AL3(TAB&T1052-ERPTABLE)                                  IOG00650
         DC    FL1'01'                                                  IOG00660
         DC    AL3(OP1052-ERPTABLE)                                     IOG00670
TYPDT2   EQU   *                  CAL LOC                               IOG00680
TYP1403  DC    XL1'30'                                                  IOG00690
         DC    AL3(TAB&T1403-ERPTABLE)                                  IOG00700
         DC    XL1'02'                                                  IOG00710
         DC    AL3(OP&O1403-ERPTABLE)                                   IOG00720
TYP2540P DC    XL1'34'                                                  IOG00730
         DC    AL3(TAB&T2540P-ERPTABLE)                                 IOG00740
         DC    XL1'03'                                                  IOG00750
         DC    AL3(OP2540P-ERPTABLE)                                    IOG00760
TYP2540R DC    XL1'3C'                                                  IOG00770
         DC    AL3(TAB&T2540R-ERPTABLE)                                 IOG00780
         DC    XL1'04'                                                  IOG00790
         DC    AL3(OP2540R-ERPTABLE)                                    IOG00800
TYP2311  DC    XL1'01'                                                  IOG00810
         DC    AL3(TAB2311-ERPTABLE)                                    IOG00820
         DC    XL1'05'                                                  IOG00830
         DC    AL3(OP2311-ERPTABLE)                                     IOG00840
TYP2314  DC    XL1'08'                                                  IOG00850
         DC    AL3(TAB2314-ERPTABLE)                                    IOG00860
         DC    XL1'06'                                                  IOG00870
         DC    AL3(OP2314-ERPTABLE)                                     IOG00880
TYP2400  DC    XL1'C0'                                                  IOG00890
         DC    AL3(TAB&T2400-ERPTABLE)                                  IOG00900
         DC    XL1'07'                                                  IOG00910
         DC    AL3(OP2400-ERPTABLE)                                     IOG00920
TYP2404  DC    XL1'C0'                                                  IOG00930
         DC    AL3(TAB&T2404-ERPTABLE)                                  IOG00940
         DC    XL1'08'                                                  IOG00950
         DC    AL3(OP2404-ERPTABLE)                                     IOG00960
TYP2402  DC    XL1'C0'                                                  IOG00970
         DC    AL3(TAB&T2402-ERPTABLE)                                  IOG00980
         DC    XL1'09'                                                  IOG00990
         DC    AL3(OP2402-ERPTABLE)                                     IOG01000
TYP2403  DC    XL1'C0'                                                  IOG01010
         DC    AL3(TAB&T2403-ERPTABLE)                                  IOG01020
         DC    XL1'0A'                                                  IOG01030
         DC    AL3(OP2403-ERPTABLE)                                     IOG01040
TYPTABEN EQU   *                                                        IOG01050
TYPTIMER DC    XL1'2C'                                                  IOG01060
         DC    AL3(TABTIMER-ERPTABLE)                                   IOG01070
         DC    XL1'0B'                                                  IOG01080
         DC    AL3(OPTIMER-ERPTABLE)                                    IOG01090
* TYPNONE      NON-EXISTENT DEVICE TYPE                                 IOG01100


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 003


*              PROCESSED IN IOERRREC                                    IOG01110
TYPTB2   EQU   *                                                        IOG01120
         EJECT                                                          IOG01130
*                                      EQUS ARE INCLUDED FOR FUTURE     IOG01140
*                                      IMPLEMENTATIONS AND FOR          IOG01150
*                                      INSURING CLEAN ASSEMBLY          IOG01160
*                                      OF TABLES OR MACRO               IOG01170
*                                                                       IOG01180
*                                                                       IOG01190
*SYMBOL  EQU   VALUE                   MEANING                          IOG01200
TAB1052  EQU   *                       CONSOLE RECOVERY TABLE           IOG01210
OP1052   EQU   *                       CONSOLE OPERATION CODE TAB       IOG01220
TAB2540P EQU   *                       PUNCH RECOVERY TABLE             IOG01230
OP2540P EQU    *                       PUNCH OPERATION CODE TAB         IOG01240
TAB2540R EQU   *                       READER RE1OVERY TABLE            IOG01250
OP2540R  EQU   *                       READER OPERATION CODE TABLE      IOG01260
OP2311   EQU   *                       2311 OPERATION CODE TABLE        IOG01270
OP2314   EQU   *                       2314 OPERATION CODE TABLE        IOG01280
TAB2400  EQU   *                       2400 TAPE RECOVERY TABLE         IOG01290
OP2400   EQU   *                       2400 OPERATION CODE TABLE        IOG01300
TAB2404  EQU   TAB2400                                                  IOG01310
OP2404   EQU   OP2400                                                   IOG01320
TAB2402  EQU   TAB2400                                                  IOG01330
OP2402   EQU   OP2400                                                   IOG01340
TAB2403  EQU   TAB2400                                                  IOG01350
OP2403   EQU   OP2400                                                   IOG01360
TABTIMER EQU   *                       TIMER RECOVERY TABLE             IOG01370
*                                      IS NONEXISTENT                   IOG01380
OPTIMER  EQU   *                       TIMER OPERATION CODE TABLE       IOG01390
*                                      IS NONEXISTENT                   IOG01400
*                                                                       IOG01410
*                                                                       IOG01420
*                                      TWO MESSAGE TABLES ARE           IOG01430
*                                      INCLUDED HERE IN ORDER TO        IOG01440
*                                      USE SAME DSECT FOR BOTH DSK-RES  IOG01450
*                                      AND IN-CORE OF REC TABLES        IOG01460
*                                                                       IOG01470
*                                                                       IOG01480
         DS    0F                                                       IOG01490
*                                      MESSAGE TYPE TABLE               IOG01500
*                                      CHARACTERISTICS                  IOG01510
* ADDINFOL DC    CL4'****'                                              IOG01520
* ADDINFON DC    AL2((INFOT2-INFOT1)/(INFOD2-INFOD1)) NO OF ENTRIES     IOG01530
* ADDINFOD DC    AL2(INFOD2-INFOD1) SIZE OF AN ENTRY                    IOG01540
* ADDINFOT DC    AL2(INFOT2-INFOT1) SIZE OF TABLE                       IOG01550
* ADDINFOS DC    AL2(0) NO SENSE INFO                                   IOG01560
*                                      ENTRIES                          IOG01570
MESINFO  EQU   *                                                        IOG01580
INFOT1   EQU   *                       CAL LOC                          IOG01590
INFOD1   EQU   *                                                        IOG01600
*        DC    X'01'                                                    IOG01610
*        DC    CL25'ERROR RECOVERY FAILED'                              IOG01620
* INFOD2   DC    X'02'                                                  IOG01630
*        DC    CL25'I/O OPERATION FAILED'                               IOG01640
*        DC    X'03'                                                    IOG01650


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 004


*        DC    CL25'INTERVENTION REQUIRED'                              IOG01660
*        DC    X'04'                                                    IOG01670
*        DC    CL25'RETYPE MESSAGE'                                     IOG01680
*        DC    X'05'                                                    IOG01690
*        DC    CL25'DEVICE NOT LOGGED IN'                               IOG01700
*        DC    X'06'                                                    IOG01710
*        DC    CL25'NON-EXISTENT DEVICE'                                IOG01720
*        DC    X'07'                                                    IOG01730
*        DC    CL25'NON-EXISTENT I/O ERROR'                             IOG01740
MESINFOE EQU   *                                                        IOG01750
*        DC    XL1'08'                                                  IOG01760
*        DC    CL25'DASD FILE PROT ERROR'                               IOG01770
INFOT2   EQU   *                                                        IOG01780
*                                                                       IOG01790
*                                                                       IOG01800
*                                      DETAIL MESSAGE TYPE TABLE        IOG01810
*                                      CHARACTERISTICS                  IOG01820
         DS    0F                                                       IOG01830
ADDDNFOL DC    CL4'****'                                                IOG01840
ADDDNFON DC    AL2((DNFOT2-DNFOT1)/(DNFOD2-DNFOD1))                     IOG01850
ADDDNFOD DC    AL2(DNFOD2-DNFOD1)                                       IOG01860
ADDDNFOT DC    AL2(DNFOT2-DNFOT1)                                       IOG01870
ADDDNFOS DC    AL2(0)                                                   IOG01880
MESDNFO  EQU   *                                                        IOG01890
DNFOT1   EQU   *                       CAL LOC                          IOG01900
DNFOD1   EQU   *                                                        IOG01910
         DC    XL1'01'                                                  IOG01920
         DC    CL15'CHAN CNT CHECK'                                     IOG01930
DNFOD2   EQU   *                                                        IOG01940
         DC    XL1'02'                                                  IOG01950
         DC    CL15'INTF CNT CHECK'                                     IOG01960
         DC    XL1'03'                                                  IOG01970
         DC    CL15'CHAN DATA CHECK'                                    IOG01980
         DC    XL1'04'                                                  IOG01990
         DC    CL15'SHLD NOT OCCUR'                                     IOG02000
         DC    XL1'05'                                                  IOG02010
         DC    CL15'UNIT CHECK'                                         IOG02020
         DC    XL1'06'                                                  IOG02030
         DC    CL15'EQUIPMENT CHECK'                                    IOG02040
         DC    XL1'07'                                                  IOG02050
         DC    CL15'INTERV REQ'                                         IOG02060
         DC    XL1'08'                                                  IOG02070
         DC    CL15'BUSOUT CHECK'                                       IOG02080
         DC    XL1'09'                                                  IOG02090
         DC    CL15'DATA CHECK'                                         IOG02100
         DC    XL1'0A'                                                  IOG02110
         DC    CL15'COMMAND REJECT'                                     IOG02120
         DC    XL1'0B'                                                  IOG02130
         DC    CL15'UNCOMM OPER SEQ'                                    IOG02140
         DC    XL1'0C'                                                  IOG02150
         DC    CL15'PROGRAM CHECK'                                      IOG02160
         DC    XL1'0D'                                                  IOG02170
         DC    CL15'PROTECTION CHK'                                     IOG02180
         DC    XL1'0E'                                                  IOG02190
         DC    CL15'UNIT EXCEPTION'                                     IOG02200


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 005


         DC    XL1'0F'                                                  IOG02210
         DC    CL15'INCORR LENGTH'                                      IOG02220
         DC    XL1'10'                                                  IOG02230
         DC    CL15'ADDRESS MARKER'                                     IOG02240
         DC    XL1'11'                                                  IOG02250
         DC    CL15'OVERRUN'                                            IOG02260
         DC    XL1'12'                                                  IOG02270
         DC    CL15'SEEK CHECK'                                         IOG02280
         DC    XL1'13'                                                  IOG02290
         DC    CL15'DATA CHK COUNT'                                     IOG02300
         DC    XL1'14'                                                  IOG02310
         DC    CL15'FILE PROTECTION'                                    IOG02320
         DC    XL1'15'                                                  IOG02330
         DC    CL15'UNDETR ERROR'                                       IOG02340
         DC    XL1'16'                                                  IOG02350
         DC    CL15'ERROR IN REC'                                       IOG02360
         DC    XL1'17'                                                  IOG02370
         DC    CL15'NRF-MADDMK'                                         IOG02380
         DC    XL1'18'                                                  IOG02390
         DC    CL15'INVALID SEEK'                                       IOG02400
         DC    XL1'19'                                                  IOG02410
         DC    CL15'UNKOWN DEVICE'                                      IOG02420
         DC    XL1'1A'                                                  IOG02430
         DC    CL15'DEVICE NOT OPER'                                    IOG02440
         DC    XL1'1B'                                                  IOG02450
         DC    CL15'NO RECORD FOUND'                                    IOG02460
         DC    XL1'1C'                                                  IOG02470
         DC    CL15'TRACK COND CHECK'                                   IOG02480
         DC    XL1'1D'                                                  IOG02490
         DC    CL15'TRACK OVERRUN'                                      IOG02500
         DC    XL1'1E'                                                  IOG02510
         DC    CL15'CYLINDER END'                                       IOG02520
         DC    XL1'1F'                                                  IOG02530
         DC    CL15'CHAINNING CHECK'                                    IOG02540
         DC    XL1'20'                                                  IOG02550
         DC    CL15'DEV NOT LOGGED '                                    IOG02560
         DC    XL1'21'                                                  IOG02570
         DC    CL15'CONSOLE RETYPE'                                     IOG02580
         DC    XL1'22'                                                  IOG02590
         DC    CL15'DSK DIAG FAILED'                                    IOG02600
         DC    XL1'23'                                                  IOG02610
         DC    CL15'UNKNOWN DEVICE'                                     IOG02620
MESDNFOE EQU   *                                                        IOG02630
         DC    XL1'24'                                                  IOG02640
         DC    CL15'STOR PARITY ERR'                                    IOG02650
DNFOT2   EQU   *                                                        IOG02660
         DS    0F                                                       IOG02670
*                                 DEVICE NOT OPERATIONAL                IOG02680
*                                 RETURN CODE TABLES                    IOG02690
ADDDKRCL DC    CL4'****'          TAB SEP                               IOG02700
ADDDKRCN DC    AL2((DKRCT2-DKRCT1)/(DKRCD2-DKRCD1)) NO OF ENTRIES       IOG02710
ADDDKRCD DC    AL2(DKRCD2-DKRCD1) SIZE OF AN ENTRY                      IOG02720
ADDDKRCT DC    AL2(DKRCT2-DKRCT1) SIZE OF TABLE                         IOG02730
ADDDKRCS DC    AL2(0)             SPECIAL INFO                          IOG02740
MESDKRC  EQU   *                                                        IOG02750


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 006


DKRCT1   EQU   *                  CAL LOC                               IOG02760
DKRCD1   EQU   *                                                        IOG02770
         DC    X'00'              1052                                  IOG02780
         DC    X'FF'                                                    IOG02790
DKRCD2   EQU   *                                                        IOG02800
         DC    XL1'30'            1403                                  IOG02810
         DC    X'04'                                                    IOG02820
         DC    XL1'34'            2540P                                 IOG02830
         DC    X'04'                                                    IOG02840
         DC    XL1'3C'            2540R                                 IOG02850
         DC    X'04'                                                    IOG02860
         DC    XL1'01'            2311                                  IOG02870
         DC    XL1'03'            TREATED AS SIO ERROR                  IOG02880
         DC    XL1'08'            2314                                  IOG02890
         DC    XL1'03'            TREATED AS SIO ERROR                  IOG02900
         DC    XL1'C0'            ALL TAPES                             IOG02910
         DC    X'05'                                                    IOG02920
MESDKRCE EQU   *                                                        IOG02930
         DC    XL1'2C'            TIMER                                 IOG02940
         DC    X'FF'                                                    IOG02950
DKRCT2   EQU   *                  CAL LOC                               IOG02960
*                                                                       IOG02970
*                                                                       IOG02980
*                                      USING STATEMENTS FOR             IOG02990
*                                      CLEAN TABLES OR                  IOG03000
*                                      MACRO ASSEMBLY                   IOG03010
         USING GRN1EXTD,R6                                              IOG03020
*        USING MESDKRCD,R13            INIT IN DKRT0 RT                 IOG03030
         USING EIOPL,R4                                                 IOG03040
         USING ERPTRWT,R5                                               IOG03050
         USING ERPERRQ,R7                                               IOG03060
         USING MESIN1,R12                                               IOG03070
*        USING DIODSCT,R8              INIT IN DKRTPRG1 RT              IOG03080
*        USING DIODSCT,R8              INIT IN DK-I/O SUBROUTINES       IOG03090
*        USING ADTSECT,R1              INIT IN DKRTPRG1 RT              IOG03100
         EJECT                                                          IOG03110
*                                 DEVICE  RECOVERY TABLES               IOG03120
*                                 CHARACTERISTICS:                      IOG03130
*                                 TABLE DEPENDENT     00                IOG03140
*                                 ENTRIES:                              IOG03150
*                                 PRIORITY            XL1               IOG03160
*                                 STATUS              XL2               IOG03170
*                                 OPTIONS             XL1               IOG03180
* NOTE: DIAGNOSE ERROR RECOVERY OPTIONS ARE INCLUDED,BUT ARE            IOG03190
*       IGNORED IN VERSION 3.1                                          IOG03200
*                                                     X'80'=DIAG REC    IOG03210
*                                                     X'40'=DIAG PROC   IOG03220
*                                                     X'20'=MESSAGE     IOG03230
*                                                     X'10'=MES CHK     IOG03240
*                                 MESSAGE CODE        CL1               IOG03250
*                                                     R=RESULT          IOG03260
*                                                     A=ACTION          IOG03270
*                                                     I=INFORMATION     IOG03280
*                                 RECOVERY ACTION     GRP HEADING       IOG03290
*                                 1.TYPE              CL1               IOG03300


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 007


*                                                     C=COMPLEX         IOG03310
*                                                     S=SIMPLE          IOG03320
*                                                     I=INTERVENTION    IOG03330
*                                                     N=NONE            IOG03340
*                                 2.RECOVERY-         AL2               IOG03350
*                                   PROCEDURE ADDRESS                   IOG03360
*                                 3.OPERATOR MESSAGE  XL1               IOG03370
*                                 MESSAGE NUMBER      PL2               IOG03380
*                                 MESSAGE CODE PT     XL1               IOG03390
*                                 OPERATION TYPE PT   XL1               IOG03400
*                                 ERROR POINTER       XL1               IOG03410
*                                 USER RETURN CODE    XL1               IOG03420
*                                 CCW ADD= CSW + FLD  FL1               IOG03430
*                                 DISPL OF SENSE      XL1               IOG03440
*                                 SENSE DATA          XL1               IOG03450
*                                 DET MES PTR         XL1               IOG03460
*                                 STATISTICS          9XL2              IOG03470
         EJECT                                                          IOG03480
         DS    0D                                                       IOG03490
*                                      DASD DEVICE RECOVERY TABLE       IOG03500
TAB2311  EQU   *                                                        IOG03510
TAB2314  EQU   *                                                        IOG03520
*                                      CHARACTERISTICS                  IOG03530
ADDT18L DC     CL4'****'                                                IOG03540
ADDT18N DC     AL2((T18T2-T18T1)/(T18D2-T18D1))                         IOG03550
ADDT18D DC     AL2(T18D2-T18D1) SIZE OF AN ENTRY                        IOG03560
ADDT18T  DC    AL2(T18T2-T18T1) SIZE OF TABLE                           IOG03570
ADDT18S  DC    AL2(6)                  NO. OF SENSE BYTES               IOG03580
*                                      ENTRIES                          IOG03590
T18T1    EQU   *                       CAL LOC                          IOG03600
T18D1    EQU   *                       CAL LOC                          IOG03610
*                                      CHANNEL CONTROL CHECK            IOG03620
         IOGENE VALUE=(01,0004,20,R,N,DKRT1-ERPTABLE,D5,999,02,06,02,  -IOG03630
               02,+8,00,00,01)                                          IOG03640
T18D2    EQU   *                                                        IOG03650
*                                      INTERFACE CONTROL CHECK          IOG03660
         IOGENE VALUE=(02,0002,20,R,N,DKRT1-ERPTABLE,D5,998,02,06,02,  -IOG03670
               02,-8,00,00,02)                                          IOG03680
*                                      CHANNEL DATA CHECK               IOG03690
         IOGENE VALUE=(03,0008,20,R,S,DKRT3-ERPTABLE,01,997,02,06,02,  -IOG03700
               02,-8,00,00,03)                                          IOG03710
*                                      UNIT CHECK                       IOG03720
*                                                                       IOG03730
*                                      EQUIPMENT CHECK                  IOG03740
         IOGENE VALUE=(04,0200,20,R,S,DKRT3-ERPTABLE,02,799,02,06,02,  -IOG03750
               02,-8,00,10,06)                                          IOG03760
*                                      NO RECORD FOUND                  IOG03770
         IOGENE VALUE=(05,0200,20,R,C,DKRT7-ERPTABLE,D5,794,02,06,02,  -IOG03780
               02,-8,01,08,17)                                          IOG03790
*                                      SEEK CHECK                       IOG03800
         IOGENE VALUE=(06,0200,A0,R,C,DKRT10-ERPTABLE,D5,793,02,06,02, -IOG03810
               02,-8,00,01,12)                                          IOG03820
*                                      INTERVENTION REQUIRED            IOG03830
ERPINTVE EQU   *                       INTERVENTION LOCATION            IOG03840
         IOGENE VALUE=(07,0200,E0,A,I,DKRT1-ERPTABLE,D5,797,03,06,01,  -IOG03850


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 008


               AF,-8,00,40,07)                                          IOG03860
*                                      BUS OUT CHECK                    IOG03870
         IOGENE VALUE=(08,0200,20,R,S,DKRT3-ERPTABLE,04,796,02,06,02,  -IOG03880
               02,-8,00,20,08)                                          IOG03890
*                                      DATA CHECK                       IOG03900
         IOGENE VALUE=(09,0200,20,R,C,DKRT11-ERPTABLE,D5,795,02,06,02, _IOG03910
               02,-8,00,08,09)                                          IOG03920
*                                      OVERRUN                          IOG03930
         IOGENE VALUE=(0A,0200,20,R,C,DKRT4-ERPTABLE,D5,792,02,06,02,  -IOG03940
               02,-8,00,04,11)                                          IOG03950
*                                      MISSING ADDRESS MARKER           IOG03960
         IOGENE VALUE=(0B,0200,20,R,C,DKRT4-ERPTABLE,D5,791,02,06,02,  -IOG03970
               02,-8,01,02,17)                                          IOG03980
*                                      COMMAND REJECT                   IOG03990
         IOGENE VALUE=(0C,0200,20,R,N,DKRT1CMR-ERPTABLE,D5,699,02,06,  -IOG04000
               01,03,-8,00,80,0A)                                       IOG04010
*                                      TRACK CONDITION CHECK            IOG04020
         IOGENE VALUE=(0D,0200,20,R,S,DKRT6-ERPTABLE,D5,790,02,06,02,  -IOG04030
               02,-8,00,02,1C)                                          IOG04040
*                                      TRACK OVERRUN                    IOG04050
         IOGENE VALUE=(0E,0200,20,R,N,DKRT1-ERPTABLE,D5,789,02,06,02,  -IOG04060
               02,-8,01,40,1D)                                          IOG04070
*                                      END OF CYLINDER                  IOG04080
         IOGENE VALUE=(0F,0200,20,R,N,DKRT1-ERPTABLE,D5,788,02,06,02,  -IOG04090
               02,-8,01,20,1E)                                          IOG04100
*                                      FILE PROTECT                     IOG04110
         IOGENE VALUE=(10,0200,E0,R,N,DKRT1FP-ERPTABLE,D5,787,08,      -IOG04120
               06,02,03,-8,01,04,14)                                    IOG04130
*                                      CHAINNING CHECK                  IOG04140
         IOGENE VALUE=(11,0001,20,R,C,DKRT10-ERPTABLE,D5,996,02,06,02, -IOG04150
               02,-8,00,00,1F)                                          IOG04160
*                                      PROGRAM CHECK                    IOG04170
         IOGENE VALUE=(12,0020,F0,R,N,DKRT1PRG-ERPTABLE,D5,698,02,06,  -IOG04180
               01,03,-8,00,00,0C)                                       IOG04190
*                                      PROTECTION CHECK                 IOG04200
         IOGENE VALUE=(13,0010,20,R,N,DKRT1-ERPTABLE,D5,697,02,06,01,  -IOG04210
               03,-8,00,00,0D)                                          IOG04220
*                                      UNIT EXCEPTION                   IOG04230
         IOGENE VALUE=(14,0100,20,R,N,DKRT1-ERPTABLE,D5,786,02,06,02,  -IOG04240
               02,-8,00,00,0E)                                          IOG04250
*                                      INCORRECT LENGTH                 IOG04260
         IOGENE VALUE=(15,0040,20,R,N,DKRT1-ERPTABLE,D5,696,02,06,01,  -IOG04270
               02,-8,00,00,0F)                                          IOG04280
*                                      PCI INTERRUPT                    IOG04290
*                                      PROCESSED IN INTER HANDLING RT   IOG04300
*                                      PRESENTLY NOT IMPLEMENTED        IOG04310
*                                                                       IOG04320
*                                                                       IOG04330
*                                      INTERNAL ERRORS                  IOG04340
*                                      AND                              IOG04350
*                                      MESSAGE TYPE ERRORS              IOG04360
*                                      AND                              IOG04370
*                                      DIAGNOSE INST FOR DASD I/O       IOG04380
*                                                                       IOG04390
*                                      REC RT LOAD FAILED:ERR IN IOERR  IOG04400


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 009


         IOGENE VALUE=(16,0200,20,I,N,DKRT0-ERPTABLE,D5,497,01,06,03,  -IOG04410
               9F,0,00,1F,16)                                           IOG04420
*                                      TABLE RT LOAD FAILED             IOG04430
         IOGENE VALUE=(17,0200,20,I,N,DKRT0-ERPTABLE,D5,498,01,06,03,  -IOG04440
               9F,0,00,2F,16)                                           IOG04450
*                                      DEVICE NOT OPERATIONAL           IOG04460
         IOGENE VALUE=(18,0200,E0,I,N,DKRT0-ERPTABLE,D5,499,06,06,01,  -IOG04470
               03,+0,00,3F,1A)                                          IOG04480
*                                      SENSE OPERATION FAILED           IOG04490
         IOGENE VALUE=(19,0200,20,I,N,DKRT0-ERPTABLE,D5,496,01,06,04,  -IOG04500
               9F,-8,00,4F,16)                                          IOG04510
*                                      DEVICE NOT LOGGED IN (DASD)      IOG04520
         IOGENE VALUE=(1A,0200,E0,I,N,DKRT0-ERPTABLE,D5,494,05,06,01,  -IOG04530
               01,+0,00,5F,20)                                          IOG04540
*                                      CONSOLE RETYPE                   IOG04550
         IOGENE VALUE=(1B,0200,20,I,N,DKRT0-ERPTABLE,D5,493,04,01,02,  -IOG04560
               FF,-8,00,7F,21)            VALID CONSOLE EQUIP CK        IOG04570
*                                      DIAGNOSE TYPE I/O                IOG04580
         IOGENE VALUE=(1C,0200,E0,I,N,DKRT1DIA-ERPTABLE,D5,492,02,07,  -IOG04590
               05,02,+0,00,6F,22)                                       IOG04600
*                                      NO MATCH ENTRY FOR               IOG04610
*                                      NON-EXISTENT ERROR               IOG04620
ERPERRE  EQU   *                       INVALID IO ERR LOC               IOG04630
         IOGENE VALUE=(1D,0200,20,R,N,DKRT0-ERPTABLE,D5,490,07,        -IOG04640
               06,06,DF,+0,00,DF,15)                                    IOG04650
TAB2311E EQU   *                                                        IOG04660
TAB2314E EQU   *                                                        IOG04670
*                                      NO MATCH ENTRY FOR               IOG04680
*                                      NON-EXISTENT DEVICE TYPE         IOG04690
ERPTYPL  EQU   *                       INVALID DEV TYPE LOC             IOG04700
         IOGENE VALUE=(1E,0200,20,R,N,DKRT0-ERPTABLE,D5,491,06,06,     -IOG04710
               06,8F,+0,00,8F,23)                                       IOG04720
T18T2    EQU   *                       CAL LOC                          IOG04730
         EJECT                                                          IOG04740
*                                      DASD RECOVERY PROCEDURES         IOG04750
*                                      EXTRN(S)                         IOG04760
*                                      ENTRY(S)                         IOG04770
*                                      REGISTER USAGE                   IOG04780
* SYMBOL EQU   REG                     MEANING                          IOG04790
* R1     EQU   1                       ADT DSECT REG                    IOG04800
* R2     EQU   2                       BASE REG                         IOG04810
* R8     EQU   8                       I/O REG + DIOSECT DSECT          IOG04820
* R14    EQU   14                      WORK REG1 + BALR1                IOG04830
* R15    EQU   15                      WORK REG2 + BALR2                IOG04840
* ALL OTHER REGS AS DEFINED IN IOERR                                    IOG04850
         EJECT                                                          IOG04860
*                                                                       IOG04870
         DS    0H                      INSURE INST ALIGN                IOG04880
DKRT0    EQU   *                       RETRY 0 TIMES                    IOG04890
*                                      FOR INTERNAL IOERR ERRORS        IOG04900
         USING *,R15                                                    IOG04910
         ST    R14,ERPRTSX3            SAVE RETURN                      IOG04920
         ST    R2,ERPRTSY3             SAVE BASE                        IOG04930
         L     R2,DKRTBASE             INIT BASE                        IOG04940
         DROP  R15                                                      IOG04950


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 010


         USING ERPTABLE,R2                                              IOG04960
*                                                                       IOG04970
         MVI   EIOCC,X'EF'             NO RETRY POSSIBLE                IOG04980
         MVI   ERPEC,X'EF'                                              IOG04990
*                                                                       IOG05000
*                                      CHECK FOR DEV                    IOG05010
*                                      NOT OPER = NOT ATTACHED          IOG05020
         CLI   MESISEN2,DVNOTATT                                        IOG05030
         BE    DKRT01                  YES,GO INSERT NOT OPER RET CD    IOG05040
*                                                                       IOG05050
DKRT00   L     R2,ERPRTSY3             RESTORE REGS                     IOG05060
         L     R14,ERPRTSX3                                             IOG05070
         BR    R14                     RETURN                           IOG05080
*                                                                       IOG05090
DKRT01   EQU   *                                                        IOG05100
         LOCATE PARMS=(MESDKRC,ADDDKRCD,MESDKRCE),                     -IOG05110
               DISP=(MESDKRC,MESDKRC),COMP=ERPDVTYP                     IOG05120
         USING MESDKRCD,R13                                             IOG05130
         MVC   RCSUP,MESDKRCV          MOVE NOT-OPER RET CD             IOG05140
         CLI   EIOUSER,ERRINERR    REQ BY IOERR?                        IOG05150
         BNE   DKRT00         NO,ENTER EXIT                             IOG05160
         MVI   RCSUP,X'08'    DEVICE NOT OPER                           IOG05170
         B     DKRT00                  ENTER EXIST                      IOG05180
*                                                                       IOG05190
*                                                                       IOG05200
*                                                                       IOG05210
DKRT1    EQU   *                       RETRY 0 TIMES                    IOG05220
*                                      LEGITIMATE DASD ERROR            IOG05230
         USING *,R15                                                    IOG05240
         ST    R14,ERPRTSX3            SAVE REGS                        IOG05250
         ST    R2,ERPRTSY3                                              IOG05260
         L     R2,DKRTBASE             INIT BASE                        IOG05270
         DROP  R15                                                      IOG05280
         USING ERPTABLE,R2                                              IOG05290
*                                                                       IOG05300
         MVI   EIOCC,X'EF'             NO RETRY POSSIBLE                IOG05310
         MVI   ERPEC,X'EF'                                              IOG05320
*                                                                       IOG05330
         L     R2,ERPRTSY3             RESTORE REGS                     IOG05340
         L     R14,ERPRTSX3                                             IOG05350
         BR    R14                     RETURN                           IOG05360
*                                                                       IOG05370
*                                                                       IOG05380
DKRT1PRG EQU   *                       PROG CHECK RT                    IOG05390
         USING *,R15                                                    IOG05400
         ST    R14,ERPRTSX3            SAVE REGS                        IOG05410
         ST    R2,ERPRTSY3                                              IOG05420
         L     R2,DKRTBASE             INIT BASE                        IOG05430
         DROP  R15                                                      IOG05440
         USING ERPTABLE,R2                                              IOG05450
*                                                                       IOG05460
         L     R8,ADDSYSRF             LOCATE DIOSECT                   IOG05470
         L     R8,DDIOSECT(R8)                                          IOG05480
         USING DIODSCT,R8                                               IOG05490
         CLI   IOCOMM,DKWRITE          WRITE OPER                       IOG05500


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 011


         BE    DKRT1PG1                YES,CHECK WRT ON READ-0NLY       IOG05510
         CLI   IOCOMM,DKWRITE2         INVALID WRITE OPERATION          IOG05520
         BE    DKRT1FQ                 YES,INDICATE FILE-PROT ERR       IOG05530
         CLI   IOCOMM,DKWRITE3         INVALID WRITE OPERATION          IOG05540
         BNE   DKRT1PR2                NO,PERM ERROR                    IOG05550
         B     DKRT1FQ                 YES,INDICATE FILE-PROT ERROR     IOG05560
DKRT1PG1 EQU   *                                                        IOG05570
         MVI   RCSUP,X'06'           WRT ON READ-ONLY                   IOG05580
         L     R1,R1SAVE                                                IOG05590
         USING ADTSECT,R1                                               IOG05600
*                                      CHECK ADT RD/WRT FLAG            IOG05610
         TM    ADTFLG1,ADTFRW          FLAG SET                         IOG05620
         BZ    DKRT1PR2                NO                               IOG05630
         NI    ADTFLG1,255-ADTFRW      YES,CLEAR+SET TO RD-ONLY         IOG05640
         OI    ADTFLG1,ADTFRO                                           IOG05650
         DROP  R1,R8                                                    IOG05660
DKRT1PR2 EQU   *                                                        IOG05670
         MVI   EIOCC,X'EF'             NO RETRY POSSIBLE                IOG05680
         MVI   ERPEC,X'EF'                                              IOG05690
         L     R2,ERPRTSY3             RESTORE REGS                     IOG05700
         L     R14,ERPRTSX3                                             IOG05710
         BR    R14                     RETURN                           IOG05720
*                                                                       IOG05730
*                                                                       IOG05740
DKRT1FP  EQU   *                       FILE PROTECT ERROR               IOG05750
         USING *,R15                                                    IOG05760
         ST    R14,ERPRTSX3            SAVE REGS                        IOG05770
         ST    R2,ERPRTSY3                                              IOG05780
         L     R2,DKRTBASE             INIT BASE                        IOG05790
         DROP  R15                                                      IOG05800
         USING ERPTABLE,R2                                              IOG05810
*                                                                       IOG05820
*                                                                       IOG05830
DKRT1FQ  EQU   *                                                        IOG05840
         MVI   ERPEC,X'EF'             NO RETRY POSSIBLE                IOG05850
         MVI   EIOCC,X'EF'                                              IOG05860
         LA    R1,DUMPLIST             DUMP CORE VIA CP FUNC            IOG05870
         SVC   X'CA'                                                    IOG05880
         DC    AL4(DKRT1FQ1)           ENSURE ERROR PROCESSING-         IOG05890
*                                      ERROR EX. BARE MACHINE           IOG05900
*                                                MACHINE SIZE?          IOG05910
DKRT1FQ1 EQU   *                       ERROR + NORMAL CONTINUE          IOG05920
         MEASURE DKCTWORK,MESICTFP,DKONE FP CT = FP+1                   IOG05930
*                                                                       IOG05940
*                                                                       IOG05950
* NOTE:PRESENTLY CMS TREATS A FILE PROTECT ERROR AS A                   IOG05960
*      REQUEST FOR A CORE DUMP.                                         IOG05970
*      NORMAL PROCESSING THEN CONTINUES.                                IOG05980
*                                                                       IOG05990
*                                                                       IOG06000
         L     R2,ERPRTSY3             RESTORE REGS                     IOG06010
         L     R14,ERPRTSX3                                             IOG06020
         BR    R14                     RETURN                           IOG06030
*                                                                       IOG06040
*                                                                       IOG06050


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 012


*                                                                       IOG06060
DKRT1DIA EQU   *                       DIAGNOSE CALL ERROR              IOG06070
DKRT1CMR EQU   *                       COMMAND REJECT ERROR             IOG06080
         USING *,R15                                                    IOG06090
         ST    R14,ERPRTSX3            SAVE REGS                        IOG06100
         ST    R2,ERPRTSY3                                              IOG06110
         L     R2,DKRTBASE             INIT BASE                        IOG06120
         DROP  R15                                                      IOG06130
         USING ERPTABLE,R2                                              IOG06140
*                                                                       IOG06150
         CLI   EIODIAG+1,DIANOR1W      INVALID READ/WRITE OPER          IOG06160
         BE    DKRT1FQ                 YES,INDICATE FILE-PROT ERROR     IOG06170
         TM    ERPSEN+1,DKFILPRT       FILE-PROT ERROR                  IOG06180
         BO    DKRT1FQ                 YES,INDICATE FILE-PROT ERROR     IOG06190
*                                                                       IOG06200
         MVI   EIOCC,X'EF'             NO RETRY POSSIBLE                IOG06210
         MVI   ERPEC,X'EF'                                              IOG06220
*                                                                       IOG06230
         L     R2,ERPRTSY3             RESTORE REGS                     IOG06240
         L     R14,ERPRTSX3                                             IOG06250
         BR    R14                     RETURN                           IOG06260
*                                                                       IOG06270
*                                                                       IOG06280
DKRT3    EQU   *                       RETRY 1 TIME                     IOG06290
         USING *,R15                                                    IOG06300
         ST    R14,ERPRTSX3            SAVE REGS                        IOG06310
         ST    R2,ERPRTSY3                                              IOG06320
         L     R2,DKRTBASE             INIT BASE                        IOG06330
         DROP  R15                                                      IOG06340
         USING ERPTABLE,R2                                              IOG06350
*                                                                       IOG06360
         CLI   EIOUSER,ERRINERR        REQ FROM IOERR?                  IOG06370
         BE    DKRT300                 YES,BYPASS ERROR REC             IOG06380
         SR    R14,R14                 CLEAR R14                        IOG06390
         STH   R14,ERPC1               CLEAR COUNTER 1                  IOG06400
DKRT30   EQU   *                                                        IOG06410
         RTCALLER ENTRY=DKUSERIO                                        IOG06420
         CLI   GRN1ERBY,C'N'           RETRY SUCCESSFUL                 IOG06430
         BE    DKRT31                  YES                              IOG06440
         CLI   GRN1ERBY,IMEDSTOP       FATAL ERROR?                     IOG06450
         BE    DKRT32                  YES,BYPASS POSSIBLE 2 MES        IOG06460
         LH    R14,ERPC1                                                IOG06470
         AH    R14,HWONE               CT = CT + 1                      IOG06480
         STH   R14,ERPC1               SAVE CT                          IOG06490
         CH    R14,HWTEN               OVER TEN RETRIES                 IOG06500
         BL    DKRT30                  NO,CONT REC                      IOG06510
DKRT300  EQU   *                                                        IOG06520
         MVI   ERPEC,X'FF'             NO,INDICATE PERM ERR             IOG06530
         MVI   EIOCC,X'FF'                                              IOG06540
         B     DKRT32                                                   IOG06550
DKRT31   EQU   *                                                        IOG06560
         MVI   ERPEC,X'00'             INDICATE SUC RETRY               IOG06570
         MVI   EIOCC,X'00'                                              IOG06580
DKRT32   EQU   *                                                        IOG06590
         L     R2,ERPRTSY3             RESTORE REGS                     IOG06600


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 013


         L     R14,ERPRTSX3                                             IOG06610
         BR    R14                     RETURN                           IOG06620
*                                                                       IOG06630
*                                                                       IOG06640
DKRT4    EQU   *                       RETRY 10 TIMES                   IOG06650
         USING *,R15                                                    IOG06660
         ST    R14,ERPRTSX3            SAVE REGS                        IOG06670
         ST    R2,ERPRTSY3                                              IOG06680
         L     R2,DKRTBASE             INIT BASE                        IOG06690
         DROP  R15                                                      IOG06700
         USING ERPTABLE,R2                                              IOG06710
*                                                                       IOG06720
         CLI   EIOUSER,ERRINERR        REQ FROM IOERR?                  IOG06730
         BE    DKRT415                 YES,BYPASS REC                   IOG06740
DKRT40   EQU   *                                                        IOG06750
         SR    R14,R14                                                  IOG06760
         STH   R14,ERPC1               CLEAR COUNTER                    IOG06770
DKRT41   EQU   *                                                        IOG06780
         RTCALLER ENTRY=DKUSERIO  RETRY REQUESTED OPER                  IOG06790
         CLI   GRN1ERBY,C'N'           RETRY SUCCESSFUL                 IOG06800
         BE    DKRT42                  YES                              IOG06810
         CLI   GRN1ERBY,IMEDSTOP       FATAL ERR IN RETRY FOR IOERR     IOG06820
         BE    DKRT43                  YES,BYPASS POSSIBLE 2 MES        IOG06830
         LH    R14,ERPC1               NO,INCREMENT COUNTER             IOG06840
         AH    R14,HWONE                                                IOG06850
         STH   R14,ERPC1                                                IOG06860
         CH    R14,HWTEN                                                IOG06870
         BL    DKRT41                  RETRY IF LESS THAN 10            IOG06880
DKRT415  EQU   *                                                        IOG06890
         MVI   ERPEC,X'FF'             PERMANENT ERR                    IOG06900
         MVI   EIOCC,X'FF'                                              IOG06910
         B     DKRT43                                                   IOG06920
DKRT42   EQU   *                                                        IOG06930
         MVI   ERPEC,X'00'             INDICATE SUCC RETRY              IOG06940
         MVI   EIOCC,X'00'                                              IOG06950
DKRT43   EQU   *                                                        IOG06960
         L     R2,ERPRTSY3             RESTORE REGS                     IOG06970
         L     R14,ERPRTSX3                                             IOG06980
         BR    R14                     RETURN                           IOG06990
*                                                                       IOG07000
*                                                                       IOG07010
DKRT6    EQU   *                       TRACK CONDITION CHECK            IOG07020
*                                      CMS DOESN'T SUPPORT ALTERNATE    IOG07030
*                                      TRACKS;THUS ITS ASSOCIATED ERROR IOG07040
*                                      RECOVERY PROCEDURE IS ALSO       IOG07050
*                                      NOT SUPPORTED                    IOG07060
*                                                                       IOG07070
         USING *,R15                                                    IOG07080
         ST    R14,ERPRTSX3            SAVE REGS                        IOG07090
         ST    R2,ERPRTSY3                                              IOG07100
         L     R2,DKRTBASE                                              IOG07110
         DROP  R15                                                      IOG07120
         USING ERPTABLE,R2                                              IOG07130
*                                                                       IOG07140
         MVI   ERPEC,X'EF'                                              IOG07150


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 014


         MVI   EIOCC,X'EF'             NO RETRY POSSIBLE                IOG07160
*                                                                       IOG07170
         L     R2,ERPRTSY3             RESTORE REGS                     IOG07180
         L     R14,ERPRTSX3                                             IOG07190
         BR    R14                     RETURN                           IOG07200
*                                                                       IOG07210
*                                                                       IOG07220
DKRT7    EQU   *                       NO RECORD FOUND                  IOG07230
         USING *,R15                                                    IOG07240
         ST    R14,ERPRTSX3            SAVE REGS                        IOG07250
         ST    R2,ERPRTSY3                                              IOG07260
         L     R2,DKRTBASE             INIT BASE                        IOG07270
         DROP  R15                                                      IOG07280
         USING ERPTABLE,R2                                              IOG07290
*                                                                       IOG07300
         CLI    EIOUSER,ERRINERR       REQ FROM IOERR?                  IOG07310
         BE    DKRT71                  YES,BYPASS ERR REC               IOG07320
         TM    ERPSEN+1,ADMARKER       MISSING ADDRESS MARKER           IOG07330
         BO    DKRT7RT8                MARKER PRESENT                   IOG07340
*                                      MARKER NOT PRESENT               IOG07350
DKRT7RT9 EQU   *                                                        IOG07360
         RTCALLER ENTRY=DKRDHOME  RD HOME ADDRESS                       IOG07370
         CLI   GRN1ERBY,C'Y'           NOT SUCC RD HOME ADDRESS         IOG07380
         BE    DKRT71                  YES,PERM ERROR                   IOG07390
         CLI   GRN1ERBY,IMEDSTOP       FATAL ERR IN RETRY FOR IOERR     IOG07400
         BE    DKRT73                  YES,BYPASS POSS 2 MES            IOG07410
         CLC   ERPWKFLD(4),ERPSEEK+2 CORRECT(CCHH) CYL+TRACK            IOG07420
         BE    DKRT71                  YES,PERM ERROR                   IOG07430
DKRT7RT5 EQU   *                                                        IOG07440
         RTCALLER ENTRY=DKRECAL  RECALIBRATE                            IOG07450
         CLI   GRN1ERBY,C'Y'           NOT SUCC RECAL                   IOG07460
         BE    DKRT71                  YES,PERM ERROR                   IOG07470
         CLI   GRN1ERBY,IMEDSTOP       FATAL ERR IN RETRY FOR IOERR     IOG07480
         BE    DKRT73                  BYPASS POSS 2 MES                IOG07490
         RTCALLER ENTRY=DKSEEK  SEEK ORIGINAL ADDRESS                   IOG07500
         CLI   GRN1ERBY,C'Y'           NOT SUCC SEEK OPERATION          IOG07510
         BE    DKRT71                  YES,PERM ERROR                   IOG07520
         CLI   GRN1ERBY,IMEDSTOP     FATAL ERR IN RETRY FOR IOERR       IOG07530
         BE    DKRT73                  BYPASS POSS 2 MES                IOG07540
         B     DKRT40                  RETRY REQUESTED OPERATION TEN    IOG07550
*                                      TIMES BY DKRT4                   IOG07560
*                                                                       IOG07570
DKRT7RT8 EQU   *                                                        IOG07580
         SR    R14,R14                 CLEAR RETRY COUNTER              IOG07590
         STH   R14,ERPC1                                                IOG07600
DKRT70   EQU   *                                                        IOG07610
         RTCALLER ENTRY=DKRECAL  RECALIBRATE                            IOG07620
         CLI   GRN1ERBY,C'Y'           NOT SUCC RECAL OPERATION         IOG07630
         BE    DKRT71                  YES,PERM ERROR                   IOG07640
         CLI   GRN1ERBY,IMEDSTOP       FATAL ERR IN RETRY FOR IOERR     IOG07650
         BE    DKRT73                  YES,BYPASS POSS 2 MES            IOG07660
         RTCALLER ENTRY=DKSEEK  SEEK ORIGINAL ADDRESS                   IOG07670
         CLI   GRN1ERBY,C'Y'           NOT SUCC SEEK OPERATION          IOG07680
         BE    DKRT71                  YES,PERM ERROR                   IOG07690
         CLI   GRN1ERBY,IMEDSTOP       FATAL ERR IN RETRY FOR IOERR     IOG07700


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 015


         BE    DKRT73                  YES,BYPASS POSS 2 MES            IOG07710
         RTCALLER ENTRY=DKUSERIO  RETRY REQUESTED I/O                   IOG07720
         CLI   GRN1ERBY,C'N'           SUCCESSFUL RETRY                 IOG07730
         BE    DKRT72                  YES                              IOG07740
         CLI   GRN1ERBY,IMEDSTOP       FATAL ERR IN RETRY FOR IOERR     IOG07750
         BE    DKRT73                  YES,BYPASS POSS 2 MES            IOG07760
         LH    R14,ERPC1               NO,INCREMENT COUNTER             IOG07770
         AH    R14,HWONE                                                IOG07780
         STH   R14,ERPC1                                                IOG07790
         CH    R14,HWTEN                                                IOG07800
         BL    DKRT70                  RETRY IF LESS THAN TEN ATTEMPTS  IOG07810
DKRT71   EQU   *                       PERMANENT ERROR                  IOG07820
         MVI   ERPEC,X'FF'             INDICATE PERM ERROR              IOG07830
         MVI   EIOCC,X'FF'                                              IOG07840
         B     DKRT73                                                   IOG07850
DKRT72   EQU   *                       SUCCESSFUL I/O RECOVERY          IOG07860
         MVI   ERPEC,X'00'             INDICATE SUCC RECOVERY           IOG07870
         MVI   EIOCC,X'00'                                              IOG07880
DKRT73   EQU   *                                                        IOG07890
         L     R2,ERPRTSY3             RESTORE REGS                     IOG07900
         L     R14,ERPRTSX3                                             IOG07910
         BR    R14                     RETURN                           IOG07920
*                                                                       IOG07930
*                                                                       IOG07940
DKRT10   EQU   *                       SEEK CHECK                       IOG07950
         USING *,R15                                                    IOG07960
         ST    R14,ERPRTSX3            SAVE REGS                        IOG07970
         ST    R2,ERPRTSY3                                              IOG07980
         L     R2,DKRTBASE             INIT BASE                        IOG07990
         DROP  R15                                                      IOG08000
         USING ERPTABLE,R2                                              IOG08010
*                                                                       IOG08020
         TM    ERPSEN,COMMDREJ         COMMAND REJECT?                  IOG08030
         BO    DKRT101                 YES,PERMANT ERROR                IOG08040
         CLI   EIOUSER,ERRINERR        REQ FROM IOERR                   IOG08050
         BE    DKRT102                 YES,BYPASS REC                   IOG08060
         B     DKRT7RT5                NO,PERFORM REC-RT5 IN REC-RT7    IOG08070
* NOTE: NEXT INSTRUCTIONS WILL BE USED WHEN CP-DIAGNOSE RETRY IS        IOG08080
*       IMPLEMENTED.PRESENTLY,CMS PERFORMS DIAGNOSE ERROR RECOVERY.     IOG08090
*        TM    MESICASE,DIAREC         DIAG RECOVERY                    IOG08100
*        BZ    DKRT7RT5                NO,PERFORM REC-RT5 IN REC-RT7    IOG08110
*        TM    MESICASE,DIAPROC        DIAG PROCEDURE ALLOWED           IOG08120
*        BO    DKRT7RT5                YES,PERFORM RECOVERY PROCEDURE 5 IOG08130
*                                      IN PROCEDURE 7                   IOG08140
*        B     DKRT102                 PERM ERR,BUT BYPASS SUP RET CD   IOG08150
DKRT101  EQU   *                                                        IOG08160
         MVI   RCSUP,X'05'             SENSE BYTE X'81' ERROR RETURN    IOG08170
DKRT102  EQU   *                                                        IOG08180
         MVI   EIOCC,X'FF'             INDICATE PERMANENT ERROR         IOG08190
         MVI   ERPEC,X'FF'                                              IOG08200
*                                                                       IOG08210
         L     R2,ERPRTSY3             RESTORE REGS                     IOG08220
         L     R14,ERPRTSX3                                             IOG08230
         BR    R14                     RETURN                           IOG08240
*                                                                       IOG08250


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 016


*                                                                       IOG08260
*                                                                       IOG08270
DKRT11   EQU   *                       DATA CHECK                       IOG08280
         USING *,R15                                                    IOG08290
         ST    R14,ERPRTSX3            SAVE REGS                        IOG08300
         ST    R2,ERPRTSY3                                              IOG08310
         L     R2,DKRTBASE             INIT BASE                        IOG08320
         DROP  R15                                                      IOG08330
         USING ERPTABLE,R2                                              IOG08340
*                                                                       IOG08350
         CLI   EIOUSER,ERRINERR        REQ BY IOERR                     IOG08360
         BE    DKRT1145                 YES,BYPASS ERR REC              IOG08370
         SR    R14,R14                 CLEAR COUNTER                    IOG08380
         STH   R14,ERPC1                                                IOG08390
DKRT112  EQU   *                                                        IOG08400
         CLC   ERPC1,HW256             256TH RETRY                      IOG08410
         BH    DKRT114                 YES,PERMANENT ERROR              IOG08420
         RTCALLER ENTRY=DKUSERIO  RETRY REQUSETED IO                    IOG08430
         CLI   GRN1ERBY,C'N'           SUCCESSFUL IO                    IOG08440
         BE    DKRT115                 YES                              IOG08450
         CLI   GRN1ERBY,IMEDSTOP       FATAL ERR IN RETRY FOR IOERR     IOG08460
         BE    DKRT116                 YES,BYPASS POSS 2 MES            IOG08470
         LH    R14,ERPC1               NO                               IOG08480
         AH    R14,HWONE               INCREMENT COUNTER                IOG08490
         STH   R14,ERPC1                                                IOG08500
         TM    ERPC1+1,X'0F'           16TH RETRY?                      IOG08510
         BZ    DKRT113                 YES,RECALIBRATE+SEEK             IOG08520
         B     DKRT112                 NO,RETRY AGAIN                   IOG08530
DKRT113  EQU   *                                                        IOG08540
         RTCALLER ENTRY=DKRECAL             RECALIBRATE                 IOG08550
         CLI   GRN1ERBY,C'N'           SUCC RECAL OPERATION             IOG08560
         BNE   DKRT114                 NO,PERM ERROR                    IOG08570
         RTCALLER ENTRY=DKSEEK              SEEK ORIGINAL CYL           IOG08580
         CLI   GRN1ERBY,C'N'           SUCC SEEK  OPERATION             IOG08590
         BE    DKRT112                 YES,CONTINUE REC PROCEDURE       IOG08600
DKRT114  EQU   *                                                        IOG08610
         CLI   GRN1ERBY,IMEDSTOP     FATAL ERR IN RETRTY FOR IOERR      IOG08620
         BE    DKRT116                 YES,BYPASS POSS 2 MES            IOG08630
DKRT1145 EQU   *                                                        IOG08640
         MVI   ERPEC,X'FF'             INDICATE PERMANENT ERROR         IOG08650
         MVI   EIOCC,X'FF'                                              IOG08660
         B     DKRT116                                                  IOG08670
DKRT115  EQU   *                                                        IOG08680
         MVI   EIOCC,X'00'             INDICATE SUCCESSFUL RETRY        IOG08690
         MVI   ERPEC,X'00'                                              IOG08700
DKRT116  EQU   *                                                        IOG08710
         L     R2,ERPRTSY3             RESTORE REGS                     IOG08720
         L     R14,ERPRTSX3                                             IOG08730
         BR    R14                     RETURN                           IOG08740
*                                                                       IOG08750
*                                      DASD SUBROUTINES                 IOG08760
*                                                                       IOG08770
DKUSERIO EQU   *                       PERFORM USER REQUESTED IO        IOG08780
          ST      R14,ERPRTSL1          SAVE RET ADDRESS                IOG08790
DKUSER0  EQU   *                                                        IOG08800


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 017


         MVC   ERPADSEN,ERPCAW         MOVE USES'S CAW TO WORK AREA     IOG08810
         XC    CSW(8),CSW                 CLEAR CSW                     IOG08820
         STARTIO DEVADD=ERPDEVA,IOREG=R8,PROGADD=ERPCAW RETRY           IOG08830
         BC    8,DKUSER3               SUCESSFUL START?                 IOG08840
         RTCALLER ENTRY=DKSIOER                                         IOG08850
DKUSER1  EQU   *                                                        IOG08860
         CLI   GRN1ERBY,ERRRETRY       REC RETRY OK?                    IOG08870
         BE    DKUSER0                 YES,RETRY                        IOG08880
DKUSER2  EQU   *                                                        IOG08890
         L     R14,ERPRTSL1            RESTORE RET LOC                  IOG08900
         BR    R14                     RET TO CALLER                    IOG08910
DKUSER3  EQU   *                                                        IOG08920
         MVC   ERPWTDEV,ERPDEVN        INIT WAIT LIST                   IOG08930
         RTCALLER PARM=ERPWT,ENTRY=ERPADDWT WAIT FOR IO COMPLETION      IOG08940
         CLI   GRN1ERBY,X'D5'          ANY IO ERRORS?                   IOG08950
         BC    8,DKUSER2               NO,RETURN TO CALLER WITH         IOG08960
*                                      RET CODE SET BY ERPINTD          IOG08970
         RTCALLER ENTRY=DKCIOER    YES,AT  LEAST 1 ERROR                IOG08980
         B     DKUSER1                 ENTER EXIT                       IOG08990
*                                                                       IOG09000
*                                                                       IOG09010
*                                                                       IOG09020
DKRDHOME EQU   *                       READ HOME ADDRESS                IOG09030
         ST    R14,ERPRTSL1            SAVE RET ADDRESS                 IOG09040
         SR    R14,R14                 CLEAR R14                        IOG09050
         STH   R14,ERPC2               CLEAR COUNTER 2                  IOG09060
DKRD0    EQU   *                                                        IOG09070
         MVC   ERPFPOP(8),DKFPOPER     MOVE SSM OPER                    IOG09080
         MVC   ERPSENOP(16),DKRDHA     INIT HOME OPERATION              IOG09090
         LA    R14,ERPSENIN                                             IOG09100
         ST    R14,ERPSENOP                                             IOG09110
         LA    R14,ERPFPOP                                              IOG09120
         ST    R14,ERPADSEN                                             IOG09130
         MVC   ERPSENOP(1),DKRDHA      REINIT OP CODE                   IOG09140
DKRD00   EQU   *                                                        IOG09150
         CLC   ERPC2,HWTEN             OVER TEN RETRIES?                IOG09160
         BNH   DKRD000                 NO                               IOG09170
         MVI   GRN1ERBY,C'Y'           YES,INDICATE ERROR               IOG09180
         B     DKRD11                  ENTER RET LOGIC                  IOG09190
DKRD000  EQU   *                                                        IOG09200
         XC    CSW(8),CSW                                               IOG09210
         STARTIO DEVADD=ERPDEVA,IOREG=R8,PROGADD=ERPADSEN               IOG09220
         BC    8,DKRD3                 SUCCESSFUL START?                IOG09230
         RTCALLER ENTRY=DKSIOER                                         IOG09240
DKRD1    EQU   *                                                        IOG09250
         CLI   GRN1ERBY,ERRRETRY       REC RETRY OK?                    IOG09260
         BE    DKRD00                  YES,RETRY                        IOG09270
         LH    R14,ERPC2                                                IOG09280
         AH    R14,HWONE               CT = CT + 1                      IOG09290
         STH   R14,ERPC2               SAVE CT                          IOG09300
         B     DKRD00                  CHECK CT'S VALUE                 IOG09310
DKRD11   EQU   *                       ERROR ENTRY PT                   IOG09320
         MEASURE DKCTWORK,MESICTHF,DKONE RD-HM=RD-HM+1                  IOG09330
DKRD2    EQU   *                                                        IOG09340
         L     R14,ERPRTSL1            RESTORE RET LOC                  IOG09350


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 018


         BR    R14                     RETURN TO CALLER                 IOG09360
DKRD3    EQU   *                                                        IOG09370
         MVC   ERPWTDEV,ERPDEVN        INIT WAIT LIST                   IOG09380
         RTCALLER PARM=ERPWT,ENTRY=ERPADDWT  WAIT FOR IO COMPLETION     IOG09390
         MVC   ERPWKFLD(4),ERPSENIN+1  MOVE HOME ADDRESS TO             IOG09400
*                                      ERROR QUEUE,EXCLUDING FLAG       IOG09410
         CLI   GRN1ERBY,X'D5'          ANY IO ERRORS                    IOG09420
         BC    8,DKRD2                 NO,RETURN TO CALLER WITH         IOG09430
*                                      RET CODE SET BY ERPINTD          IOG09440
         RTCALLER ENTRY=DKCIOER  YES,AT LEAST 1 ERROR                   IOG09450
         B     DKRD1                   ENTER CHECK ROUTINE              IOG09460
*                                                                       IOG09470
*                                                                       IOG09480
DKRECAL  EQU   *                       RECALIBRATE                      IOG09490
         ST    R14,ERPRTSL1            SAVE RET LOC                     IOG09500
         SR    R14,R14                 CLEAR R14                        IOG09510
         STH   R14,ERPC2               CLEAR COUNTER 2                  IOG09520
DKRE0    EQU   *                                                        IOG09530
         MVC   ERPFPOP(8),DKFPOPER     INIT SSM OPER                    IOG09540
         MVC   ERPSENOP(16),DKRECL     INIT RECAL OPER IN FREE STOR     IOG09550
*        LA    R14,ERPSENIN                                             IOG09560
*        ST    R14,ERPSENOP                                             IOG09570
         LA    R14,ERPFPOP                                              IOG09580
         ST    R14,ERPADSEN                                             IOG09590
*        MVC   ERPSENOP(1),DKRECL      REINIT RECAL OP                  IOG09600
DKRE00   EQU   *                                                        IOG09610
         CLC   ERPC2,HWTEN             OVER TEN RETRIES                 IOG09620
         BNH   DKRE000                 NO                               IOG09630
         MVI   GRN1ERBY,C'Y'           YES,INDICATE ERROR               IOG09640
         B     DKRE11                  ENTER RET LOGIC                  IOG09650
DKRE000  EQU   *                                                        IOG09660
         XC    CSW(8),CSW                 CLEAR CSW                     IOG09670
         STARTIO DEVADD=ERPDEVA,IOREG=R8,PROGADD=ERPADSEN               IOG09680
         BC    8,DKRE3                 SUCESSFUL START?                 IOG09690
         RTCALLER ENTRY=DKSIOER                                         IOG09700
DKRE1    EQU   *                                                        IOG09710
         CLI   GRN1ERBY,ERRRETRY       REC RETRY OK?                    IOG09720
         BE    DKRE00                  YES,RETRY                        IOG09730
         LH    R14,ERPC2                                                IOG09740
         AH    R14,HWONE               CT = CT + 1                      IOG09750
         STH   R14,ERPC2               SAVE CT                          IOG09760
         B     DKRE00                  CHECK CT'S VQLUE                 IOG09770
DKRE11   EQU   *                       ERROR ENTRY POINT                IOG09780
         MEASURE DKCTWORK,MESICTRF,DKONE RF=RF+1                        IOG09790
DKRE2    EQU   *                                                        IOG09800
         L     R14,ERPRTSL1            RESTORE RET LOC                  IOG09810
         BR    R14                     RETURN TO CALLER                 IOG09820
DKRE3    EQU   *                                                        IOG09830
         MVC   ERPWTDEV,ERPDEVN        INIT WAIT LIST                   IOG09840
         RTCALLER PARM=ERPWT,ENTRY=ERPADDWT WAIT FOR IO COMPLETION      IOG09850
         CLI   GRN1ERBY,X'D5'          ANY IOERRORS                     IOG09860
         BC    8,DKRE2                 NO, RETURN TO CALLER WITH        IOG09870
*                                      RET CODE SET BY ERPINTD          IOG09880
         RTCALLER ENTRY=DKCIOER   YES,AT LEAST 1 ERROR                  IOG09890
         B     DKRE1                   ENTER CHECK ROUTINE              IOG09900


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 019


*                                                                       IOG09910
*                                                                       IOG09920
DKSEEK   EQU   *                       SEEK OPERATION                   IOG09930
*                                      TEMP FIX                         IOG09940
*                                                                       IOG09950
         MVI   GRN1ERBY,C'N'           DUMMY-                           IOG09960
         BR    R14                     OPERATION                        IOG09970
*                                                                       IOG09980
*                                                                       IOG09990
         ST    R14,ERPRTSL1            SAVE RET LOC                     IOG10000
         MVC   ERPFPOP(8),DKFPOPER     INIT SSM OPER                    IOG10010
         MVC   ERPSENOP(16),DKSK       INIT SEEK IN FREE STOR           IOG10020
         LA    R14,ERPSEEK                                              IOG10030
         ST    R14,ERPSENOP                                             IOG10040
         LA    R14,ERPFPOP                                              IOG10050
         ST    R14,ERPADSEN                                             IOG10060
         MVC   ERPSENOP(1),DKSK        REINIT  SEEK OPER CODE           IOG10070
DKSK0    EQU   *                       RETRY POINT                      IOG10080
         XC    CSW(8),CSW                 CLEAR CSW                     IOG10090
         STARTIO DEVADD=ERPDEVA,IOREG=R8,PROGADD=ERPADSEN               IOG10100
         BC    8,DKSEEK3               SUCCESSFUL START?                IOG10110
         RTCALLER ENTRY=DKSIOER                                         IOG10120
DKSK1    EQU   *                                                        IOG10130
         CLI   GRN1ERBY,ERRRETRY       REC RETRY OK?                    IOG10140
         BE    DKSK0                   YES,RETRY                        IOG10150
         MEASURE DKCTWORK,MESICTSF,DKONE SEEK FAIL=SF+1                 IOG10160
DKSK2    EQU   *                                                        IOG10170
         L     R14,ERPRTSL1            RESTORE RET LOC                  IOG10180
         BR    R14                     RETURN TO CALLER                 IOG10190
DKSEEK3  EQU   *                                                        IOG10200
         MVC   ERPWTDEV,ERPDEVN        INIT WAIT LIST                   IOG10210
         RTCALLER PARM=ERPWT,ENTRY=ERPADDWT WAIT FOR IO COMPLETION      IOG10220
         CLI   GRN1ERBY,X'D5'          ANY IO ERRORS                    IOG10230
         BC    8,DKSK2                 NO, RETURN TO CALLER WITH        IOG10240
*                                      RETURN CODE SET BY ERPINTD       IOG10250
         RTCALLER ENTRY=DKCIOER   YES,AT LEAST 1 ERROR                  IOG10260
         B     DKSK1                   ENTER CHECK ROUTINE              IOG10270
*                                                                       IOG10280
*                                                                       IOG10290
*                                                                       IOG10300
*                                                                       IOG10310
* NOTE:ERROR IN IOERR INITIATES ANOTHER CALL TO IOERR.                  IOG10320
*                                      REG USAGE AND USING              IOG10330
         DROP  R4                                                       IOG10340
         USING EIOPL,R1                INIT NEW EIOPL DSCET REG         IOG10350
*        USING GRN1EXTD,R6             INIT IN ENTRY                    IOG10360
         USING  DIODSCT,R8                                              IOG10370
*                                                                       IOG10380
DKSIOER  EQU   *                                                        IOG10390
         ST    R14,ERPRTSL2             SAVE RET LOC                    IOG10400
         L     R8,ADDSYSRF                                              IOG10410
         L     R8,DDIOSECT(R8)         LOCATE DIOSECT                   IOG10420
*        BALR  R14,R0                  R14 CONTAINS CC FROM ENTRY       IOG10430
         ST    R14,SAVECC              SIO CC TO DIOSECT                IOG10440
*        L     R6,R2SAVE               INIT BY IOERRSUP                 IOG10450


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 020


         BAL   R14,BUILDPAR            CREATE PARM LIST                 IOG10460
         MVC   EIOTYPE,DKESIO          TYPE TO LIST                     IOG10470
         MVC   EIOSIOCC,SAVECC         CC TO LIST                       IOG10480
         MVC   EIOCSW,CSW              CSW TO LIST                      IOG10490
*                                      (FOR SYSTEM INTEGRITY)           IOG10500
         MVI   GRN1ERBY,DKERROR                                         IOG10510
         MVC   GRN1CSW,CSW             FLDS TO DEV EXT TAB              IOG10520
         MVC   DIOCSW(8),CSW           CSW TO DIOSECT                   IOG10530
         MVC   GRN1PSW,DKEPSW1                                          IOG10540
         MEASURE DKCTWORK,MESICTBF,DKONE                                IOG10550
*                                                                       IOG10560
         B     PROCENT                                                  IOG10570
*                                                                       IOG10580
*                                                                       IOG10590
DKCIOER  EQU   *                                                        IOG10600
         ST    R14,ERPRTSL2            SAVE RET LOC                     IOG10610
         L     R8,ADDSYSRF                                              IOG10620
         L     R8,DDIOSECT(R8)         LOCATE DIOSECT                   IOG10630
         BAL   R14,BUILDPAR            CREATE PARM LIST                 IOG10640
         MVC   EIOTYPE,DKECIO          TYPE TO LIST                     IOG10650
         MVC   EIOCSW,GRN1CSW                                           IOG10660
         MEASURE DKCTWORK,MESICTEF,DKONE                                IOG10670
*                                                                       IOG10680
PROCENT  EQU   *                                                        IOG10690
         L     R15,ADDSYSRF                                             IOG10700
         L     R15,DIOERRSP(R15)       LOCATE IOERRSUP                  IOG10710
         BALR  R14,R15                 ENTER IOERRSUP                   IOG10720
*                                                                       IOG10730
ESIORET  EQU   *                                                        IOG10740
ECIORET  EQU   *                                                        IOG10750
         CLI   EIOCC,URETRY            DEVICE AVAILABLE                 IOG10760
         BE    ESIOREP                 YES,ALLOW RETRY                  IOG10770
         CLI   EIOCC,NOERRMES          DEVICE NOT AVAILABLE             IOG10780
         BE    EFATAL2                 YES                              IOG10790
         CLI   EIOCC,ENOTATT           DEVICE NOT ATTACHED              IOG10800
         BE    EFATAL1                 YES                              IOG10810
ENOKERR  EQU   *                                                        IOG10820
*                                      ALL OTHERS ERRORS                IOG10830
         MVI   GRN1ERBY,C'Y'           IO ERR IN IOERR                  IOG10840
EGENRET  EQU   *                                                        IOG10850
         BAL   R14,STORREL             RELEASE FREE STORAGE             IOG10860
         L     R14,ERPRTSL2            RESTORE RET LOC                  IOG10870
         BR    R14                     RETURN TO CALLER                 IOG10880
ESIOREP  EQU   *                                                        IOG10890
         MVI   GRN1ERBY,ERRRETRY       RETRY ALLOWED                    IOG10900
         B     EGENRET                 ENTER EXIT                       IOG10910
EFATAL1  EQU   *                                                        IOG10920
         MVI   EIOCC-EIOBEG(R4),X'03'  SET EXPECTED RET CODE            IOG10930
         MVI   RCSUP,X'03'                                              IOG10940
         MVI   ERPEC,X'00'             DUMMY SUCCESS STATUS TO -        IOG10950
*                                      PREVENT DOUBLE MESSAGE           IOG10960
         B     EFATAL12                COMBINED RET LOGIC               IOG10970
EFATAL2  EQU   *                                                        IOG10980
         MVC   EIOCC-EIOBEG(1,R4),MESIRET      SET RET CODE             IOG10990
         MVC   RCSUP,MESIRET                                            IOG11000


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 021


         MVI   ERPEC,X'00'             DUMMY SUCCESS STATUS TO-         IOG11010
*                                      PREVENT DOUBLE MESSAGE           IOG11020
EFATAL12 EQU   *                                                        IOG11030
         MVI   GRN1ERBY,IMEDSTOP       SET STATUS TO FATAL ERR          IOG11040
         B     EGENRET                 ENTER EXIT                       IOG11050
*                                                                       IOG11060
BUILDPAR EQU   *                                                        IOG11070
         ST    R14,ERPRTSL3            SAVE RET  LOC                    IOG11080
         L     R0,EIOPLLEN                                              IOG11090
         L     R15,ADDSYSRF            SETUP CORE-REQ FOR LIST          IOG11100
         L     R15,DFREE(R15)                                           IOG11110
         BALR  R14,R15                 ENTER FREE                       IOG11120
         MVI   EIOBEG,X'00'            CLEAR LIST AREA                  IOG11130
         MVC   EIOBEG+1(EIOREGS+1-EIOBEG),EIOBEG                        IOG11140
*                                      USE FLDS IN ORIGINAL LIST-       IOG11150
*                                      ADDRESSED BY R4                  IOG11160
         MVC   EIODEVA,EIODEVA-EIOBEG(R4)                               IOG11170
         MVC   EIOCAW,ERPADSEN         DEBUG STRING HAS BEEN-           IOG11180
*                                      LINKED                           IOG11190
         MVC   EIOUSER,EDKSYS          USER = C'ER'                     IOG11200
         ST    R6,EIODTEXT             DEV EXT TO LIST                  IOG11210
         MVC   EIOSEEK,ERPSEEK                                          IOG11220
         MVC   EIODEVN,EIODEVN-EIOBEG(R4)                               IOG11230
         MVC   EIODVTYP,EIODVTYP-EIOBEG(R4)                             IOG11240
         L     R14,ERPRTSL3            RESTORE RET LOC                  IOG11250
         BR    R14                     RETURN TO CALLER                 IOG11260
*                                                                       IOG11270
STORREL  EQU   *                                                        IOG11280
         ST    R14,ERPRTSL3            SAVE RET LOC                     IOG11290
*                                      CHECK FOR NO MESSAGE-            IOG11300
*                                      PRINTED DUE TO CONSOLE ERROR     IOG11310
         SR    R15,R15                 CLEAR R15                        IOG11320
         C     R15,EIOMESLN            NO MESSAGE PRESENT?              IOG11330
         BE    STORREL2                YES,BYPASS MES-CORE RELEASE      IOG11340
*                                      AN UNTYPED MESSAGE IS PRESENT    IOG11350
         ST    R1,DOUBLE               SAVE REG1                        IOG11360
         L     R0,EIOMESLN             CAL MES KORE                     IOG11370
         SR    R1,R1                                                    IOG11380
         LA    R1,32                                                    IOG11390
         AR    R0,R1                   MES CORE                         IOG11400
         S     R1,FP64                 REG1=-32                         IOG11410
         A     R1,EIOMESAD             CAL OF MES LOC                   IOG11420
         L      R15,ADDSYSRF                                            IOG11430
         L     R15,DFRET(R15)                                           IOG11440
         BALR  R14,R15                 ENTER FRET                       IOG11450
         L     R1,DOUBLE               RESTORE R1                       IOG11460
STORREL2 EQU    *                                                       IOG11470
*                                      RELEASE EIOPL                    IOG11480
         L     R0,EIOPLLEN             R0 = D-WORDS TO RET TO FREE      IOG11490
*                                      R1 = AREA TO RETURN              IOG11500
         L     R15,ADDSYSRF                                             IOG11510
         L     R15,DFRET(R15)                                           IOG11520
         BALR  R14,R15                 ENTER FRET                       IOG11530
         L     R14,ERPRTSL3            RESTORE RET LOC                  IOG11540
         BR    R14                     RETURN TO CALLER                 IOG11550


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 022


*                                      REG CLEANUP                      IOG11560
         DROP  R1                                                       IOG11570
         USING EIOPL,R4                RESTORE EIOPL DSECT TO R4        IOG11580
         EJECT                                                          IOG11590
*                                      DKRT'S CONSTANTS,EQUS,ADDRESSES  IOG11600
*                                      CONSTANTS                        IOG11610
HWONE    DC    H'1'                    INCREMENT BY 1                   IOG11620
DKCTWORK DC    H'0'                    STAT WORK AREA                   IOG11630
DKONE    DC    H'1'                    STAT INCRE BY ONE                IOG11640
HWTEN    DC    H'30'                   TEN CONSTANT                     IOG11650
HW16     DC    H'16'                   16 CONSTANT                      IOG11660
HW256    DC    H'256'                  256                              IOG11670
FP64     DC    F'64'                   DECREMENT 64                     IOG11680
EIOPLLEN DC    F'16'                   D-WORDS FOR IOERR CALLING LIST   IOG11690
*                                                                       IOG11700
         DS    0D                                                       IOG11710
DUMPLIST DC    CL8'CPFUNCTN'                                            IOG11720
         DC    CL8'NOMSG'                                               IOG11730
         DC    C'DUMP 00-FFFFFF G0-15 PSW'                              IOG11740
*                                      TREAT AS MAX MACHINE-SIZE        IOG11750
         DC    X'FF'                   FENSE.                           IOG11760
*                                                                       IOG11770
DKESIO   DC    CL4'SIOE'                                                IOG11780
EDKSYS   DC     CL2'ER'    IOERR RQE STATUS                             IOG11790
DKECIO   DC    CL4'CIOE'                                                IOG11800
DKEPSW1  DC    XL1'00'                                                  IOG11810
         DC    CL4'SIOE'                                                IOG11820
         DC    AL3(ERPTABLE)                                            IOG11830
*                                                                       IOG11840
DKRDHA   CCW   X'1A',DKRDHA,X'60',5    CCW TO READ HOME ADDRESS         IOG11850
         CCW   X'03',0,X'20',1         NOP CCW                          IOG11860
DKRECL   CCW   X'13',0,X'60',1         CCW TO RECALIBRATE               IOG11870
         CCW   X'03',0,X'20',1         NOP CCW                          IOG11880
DKSK     CCW   X'07',DKSK,X'60',6      CCW TO SEEK ORIGINAL CYL+TRACK   IOG11890
         CCW   X'03',0,X'20',1         NOP CCW                          IOG11900
DKFPOPER CCW   X'1F',DKFPKY,X'60',1   CCW TO SSM                        IOG11910
DKSEN    CCW   X'04',DKSEN,X'60',6     CCW TO SENSE                     IOG11920
         CCW   X'03',0,X'20',1         NOP CCW                          IOG11930
*                                                                       IOG11940
DKFPKY   DC    B'10000000'             NO WRITE CT-KEY-DATA(NOR+SPEC)   IOG11950
*                                                                       IOG11960
*                                      EQU'S                            IOG11970
*SYMBOL  EQU   VALUE                   MEANING                          IOG11980
ADMARKER EQU   X'02'                   MISSING ADDRESS MARKER CONDITION IOG11990
COMMDREJ EQU   X'80'                   COMMAND REJECT CONDITION         IOG12000
DKWRITE  EQU   X'05'                    WRITE OPERATION CONDITION       IOG12010
URETRY   EQU   X'AF'                   DEVICE AVAILABLE                 IOG12020
NOERRMES EQU   X'BF'                   DEVICE NOT AVAILABLE             IOG12030
ENOTATT  EQU   X'08'                   DEVICE NOT ATTACHED-             IOG12040
*                                      IOERR INTERNAL CODE              IOG12050
IMEDSTOP EQU   C'F'                    FATAL ERROR(DEVICE NOT-          IOG12060
*                                      AVAILABLE) IN RETRY FOR IOERR    IOG12070
ERRRETRY EQU   C'R'                    ERROR RETRY ALLOWED              IOG12080
DKWRITE2 EQU   X'1D'                   INVALID WRT CT-KEY-DATA OPER     IOG12090
DKWRITE3 EQU   X'01'                   INVALID SPEC WRT CT-KEY-DATA     IOG12100


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 023


DIAREC   EQU   X'80'                   DIAG REC OK                      IOG12110
DIAPROC  EQU   X'40'                   DIAG PROC OK                     IOG12120
DKFILPRT EQU   X'04'                   FILE-PROT ERROR                  IOG12130
DIANOR1W EQU   X'05'                   CCW NEITHER RD(06) OR WRT(05)    IOG12140
*                                                                       IOG12150
*                                      ADDRESSES                        IOG12160
* ADDIOERS DC    A(IOERRSUP)             ENTRY PT OF IOERR              IOG12170
* ADDFREE3 DC    A(FREE)                 FREE RT                        IOG12180
* ADDFRET3 DC    A(FRET)                 FRET RT                        IOG12190
DKRTBASE DC    A(ERPTABLE)                BASE VALUE FOR REC PROCEDURES IOG12200
ADDSYSRF DC    A(SYSREF)               SYSREF ADDRESS FOR               IOG12210
*                                      LOCATING DIOSECT                 IOG12220
*                                           AND                         IOG12230
*                                      ALL SYSTEM ROUTINE ADDRESSES     IOG12240
         EJECT                                                          IOG12250
         AIF   ('&TABTYPE' EQ 'IOFRTAB').CONT2                          IOG12260
         AIF   ('&TABTYPE' EQ 'IOMACTAB').CONT3                         IOG12270
.*       MNOTE 1,'IONUTAB EXPANSION'                                    IOG12280
         AGO   .BYPASS                                                  IOG12290
.CONT2   ANOP                                                           IOG12300
.*       MNOTE 1,'IOFRTAB EXPANSION'                                    IOG12310
         AGO   .CONT4                                                   IOG12320
.CONT3   ANOP                                                           IOG12330
.*       MNOTE 1,'IOMACTAB EXPANSION'                                   IOG12340
.CONT4   ANOP                                                           IOG12350
*                                 1403 PRINTER RECOVERY TABLE           IOG12360
         DS    0D                 ALIGN TAB                             IOG12370
TAB1403  EQU   *                                                        IOG12380
*                                 CHARACTERISTICS                       IOG12390
ADDT48L DC    CL4'****'           TAB SEPERATOR                         IOG12400
ADDT48N  DC    AL2((T48TB2-T48TB1)/(T48DT2-T48DT1))   NO OF ENTRIES     IOG12410
ADDT48D  DC    AL2(T48DT2-T48DT1) SIZE OF AN ENTRY                      IOG12420
ADDT48T  DC    AL2(T48TB2-T48TB1) SIZE OF TABLE                         IOG12430
ADDT48S  DC    AL2(L'T48SN)       NO OF SENSE INFO                      IOG12440
*                                 ENTRIES                               IOG12450
T48TB1   EQU   *                  CALCULATION (CAL) LOCATION (LOC)      IOG12460
T48DT1   EQU   *                  CAL LOC                               IOG12470
         DC    XL1'01'                                                  IOG12480
         DC    XL2'0004'          CHANNEL CONTROL CHECK                 IOG12490
         DC    XL1'20'                                                  IOG12500
         DC    CL1'R'                                                   IOG12510
         DC    CL1'N'                                                   IOG12520
         DC    AL2(PRNNRT2-ERPTABLE) DO NOT RETRY                       IOG12530
         DC    XL1'D5'                                                  IOG12540
         DC    PL2'999'                                                 IOG12550
         DC    XL1'02'                                                  IOG12560
         DC    XL1'06'                                                  IOG12570
         DC    XL1'02'                                                  IOG12580
         DC    XL1'05'                                                  IOG12590
         DC    FL1'+8'                                                  IOG12600
         DC    XL1'00'                                                  IOG12610
T48SN    DC    XL1'00'                                                  IOG12620
         DC    XL1'01'                                                  IOG12630
         DC    XL1'00'                                                  IOG12640
         DC    9XL2'0000'                                               IOG12650


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 024


T48DT2   EQU   *                                                        IOG12660
         DC    XL1'02'                                                  IOG12670
         DC    XL2'0002'               INTERFACE CONTROL CHECK          IOG12680
         DC    XL1'20'                                                  IOG12690
         DC    CL1'R'                                                   IOG12700
         DC    CL1'N'                                                   IOG12710
         DC    AL2(PRNNRT2-ERPTABLE) DO NOT RETRY                       IOG12720
         DC    XL1'D5'                                                  IOG12730
         DC    PL2'998'                                                 IOG12740
         DC    XL1'02'                                                  IOG12750
         DC    XL1'06'                                                  IOG12760
         DC    XL1'02'                                                  IOG12770
         DC    XL1'05'                                                  IOG12780
         DC    FL1'-8'                                                  IOG12790
         DC    XL1'00'                                                  IOG12800
         DC    XL1'00'                                                  IOG12810
         DC    XL1'02'                                                  IOG12820
         DC    XL1'00'                                                  IOG12830
         DC    9XL2'0000'                                               IOG12840
         DC    XL1'03'                                                  IOG12850
         DC    XL2'0008'          CHANNEL DATA CHECK                    IOG12860
         DC    XL1'20'                                                  IOG12870
         DC    CL1'R'                                                   IOG12880
         DC    CL1'N'                                                   IOG12890
         DC    AL2(PRNNRT2-ERPTABLE)                                    IOG12900
         DC    XL1'01'                                                  IOG12910
         DC    PL2'997'                                                 IOG12920
         DC    XL1'02'                                                  IOG12930
         DC    XL1'06'                                                  IOG12940
         DC    XL1'02'                                                  IOG12950
         DC    XL1'05'                                                  IOG12960
         DC    FL1'-8'                                                  IOG12970
         DC    XL1'00'                                                  IOG12980
         DC    XL1'00'                                                  IOG12990
         DC    XL1'03'                                                  IOG13000
          DC   XL1'00'                                                  IOG13010
         DC    9XL2'0000'                                               IOG13020
         DC    XL1'04'                                                  IOG13030
         DC    XL2'8000'          SHOULD NOT OCCUR                      IOG13040
         DC    XL1'20'                                                  IOG13050
         DC    CL1'R'                                                   IOG13060
         DC    CL1'N'                                                   IOG13070
         DC    AL2(PRNNRT2-ERPTABLE)                                    IOG13080
         DC    XL1'D5'                                                  IOG13090
         DC    PL2'899'                                                 IOG13100
         DC    XL1'02'                                                  IOG13110
         DC    XL1'06'                                                  IOG13120
         DC    XL1'02'                                                  IOG13130
         DC    XL1'05'                                                  IOG13140
         DC    FL1'-8'                                                  IOG13150
         DC    XL1'00'                                                  IOG13160
         DC    XL1'00'                                                  IOG13170
         DC    XL1'04'                                                  IOG13180
         DC    XL1'00'                                                  IOG13190
         DC    9XL2'0000'                                               IOG13200


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 025


         DC    XL1'04'                                                  IOG13210
         DC    XL2'4000'          SHOULD NOT OCCUR                      IOG13220
         DC    XL1'20'                                                  IOG13230
         DC    CL1'R'                                                   IOG13240
         DC    CL1'N'                                                   IOG13250
         DC    AL2(PRNNRT2-ERPTABLE)                                    IOG13260
         DC    XL1'D5'                                                  IOG13270
         DC    PL2'899'                                                 IOG13280
         DC    XL1'02'                                                  IOG13290
         DC    XL1'06'                                                  IOG13300
         DC    XL1'02'                                                  IOG13310
         DC    XL1'05'                                                  IOG13320
         DC    FL1'-8'                                                  IOG13330
         DC    XL1'00'                                                  IOG13340
         DC    XL1'00'                                                  IOG13350
         DC    XL1'04'                                                  IOG13360
         DC    XL1'00'                                                  IOG13370
         DC    9XL2'0000'                                               IOG13380
         DC    XL1'04'                                                  IOG13390
         DC    XL2'2000'          SHOULD NOT OCCUR                      IOG13400
         DC    XL1'20'                                                  IOG13410
         DC    CL1'R'                                                   IOG13420
         DC    CL1'N'                                                   IOG13430
         DC    AL2(PRNNRT2-ERPTABLE)                                    IOG13440
         DC    XL1'D5'                                                  IOG13450
         DC    PL2'899'                                                 IOG13460
         DC    XL1'02'                                                  IOG13470
         DC    XL1'06'                                                  IOG13480
         DC    XL1'02'                                                  IOG13490
         DC    XL1'05'                                                  IOG13500
         DC    FL1'-8'                                                  IOG13510
         DC    XL1'00'                                                  IOG13520
         DC    XL1'00'                                                  IOG13530
         DC    XL1'04'                                                  IOG13540
         DC    XL1'00'                                                  IOG13550
         DC    9XL2'0000'                                               IOG13560
         DC    XL1'04'                                                  IOG13570
         DC    XL2'0001'          SHOULD NOT OCCUR                      IOG13580
         DC    XL1'20'                                                  IOG13590
         DC    CL1'R'                                                   IOG13600
         DC    CL1'N'                                                   IOG13610
         DC    AL2(PRNNRT2-ERPTABLE)                                    IOG13620
         DC    XL1'D5'                                                  IOG13630
         DC    PL2'899'                                                 IOG13640
         DC    XL1'02'                                                  IOG13650
         DC    XL1'06'                                                  IOG13660
         DC    XL1'02'                                                  IOG13670
         DC    XL1'05'                                                  IOG13680
         DC    FL1'-8'                                                  IOG13690
         DC    XL1'00'                                                  IOG13700
         DC    XL1'00'                                                  IOG13710
         DC    XL1'04'                                                  IOG13720
         DC    XL1'00'                                                  IOG13730
         DC    9XL2'0000'                                               IOG13740
*                                      X'05' = ASSUME UNIVERSAL CHAR    IOG13750


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 026


         DC    XL1'06'                                                  IOG13760
         DC    XL2'0200'      SHOULD NOT OCCUR - UNUSUAL COMMAND SEQ    IOG13770
         DC    XL1'20'                                                  IOG13780
         DC    CL1'R'                                                   IOG13790
         DC    CL1'N'                                                   IOG13800
         DC    AL2(PRNNRT2-ERPTABLE)                                    IOG13810
         DC    XL1'D5'                                                  IOG13820
         DC    PL2'899'                                                 IOG13830
         DC    XL1'02'                                                  IOG13840
         DC    XL1'06'                                                  IOG13850
         DC    XL1'02'                                                  IOG13860
         DC    XL1'01'                                                  IOG13870
         DC    FL1'-8'                                                  IOG13880
         DC    XL1'00'                                                  IOG13890
         DC    XL1'02'                                                  IOG13900
         DC    XL1'04'                                                  IOG13910
         DC    XL1'00'                                                  IOG13920
         DC    9XL2'0000'                                               IOG13930
         DC    XL1'07'                                                  IOG13940
         DC    XL2'0200'      EQUIPMENT CHECK                           IOG13950
         DC    XL1'20'                                                  IOG13960
         DC    CL1'R'                                                   IOG13970
         DC    CL1'N'                                                   IOG13980
         DC    AL2(PRNNRT2-ERPTABLE)                                    IOG13990
         DC    XL1'02'                                                  IOG14000
         DC    PL2'799'                                                 IOG14010
         DC    XL1'02'                                                  IOG14020
         DC    XL1'06'                                                  IOG14030
         DC    XL1'02'                                                  IOG14040
         DC    XL1'01'                                                  IOG14050
         DC    FL1'-8'                                                  IOG14060
         DC    XL1'00'                                                  IOG14070
         DC    XL1'10'                                                  IOG14080
         DC    XL1'06'                                                  IOG14090
         DC    XL1'00'                                                  IOG14100
         DC    9XL2'0000'                                               IOG14110
         DC    XL1'08'                                                  IOG14120
         DC    XL2'0200'      UNIT CHECK                                IOG14130
         DC    XL1'20'        CODE GEN. STOR. PARITY ERROR              IOG14140
         DC    CL1'R'                                                   IOG14150
         DC    CL1'N'                                                   IOG14160
         DC    AL2(PRNNRT2-ERPTABLE)                                    IOG14170
         DC    XL1'03'                                                  IOG14180
         DC    PL2'798'                                                 IOG14190
         DC    XL1'02'                                                  IOG14200
         DC    XL1'06'                                                  IOG14210
         DC    XL1'02'                                                  IOG14220
         DC    XL1'01'                                                  IOG14230
         DC    FL1'-8'                                                  IOG14240
         DC    XL1'00'                                                  IOG14250
         DC    XL1'04'                                                  IOG14260
         DC    XL1'24'                                                  IOG14270
         DC    XL1'00'                                                  IOG14280
         DC    9XL2'0000'                                               IOG14290
         DC    XL1'09'                                                  IOG14300


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 027


         DC    XL2'0200'      UNIT CHECK                                IOG14310
         DC    XL1'20'        INTERVENTION REQUIRED                     IOG14320
         DC    CL1'A'                                                   IOG14330
         DC    CL1'I'                                                   IOG14340
         DC    AL2(PRNNRT2-ERPTABLE)                                    IOG14350
         DC    XL1'D5'                                                  IOG14360
         DC    PL2'797'                                                 IOG14370
         DC    XL1'03'                                                  IOG14380
         DC    XL1'06'                                                  IOG14390
         DC    XL1'01'                                                  IOG14400
         DC    XL1'AF'                                                  IOG14410
         DC    FL1'-8'                                                  IOG14420
         DC    XL1'00'                                                  IOG14430
         DC    XL1'40'                                                  IOG14440
         DC    XL1'07'                                                  IOG14450
         DC    XL1'00'                                                  IOG14460
         DC    9XL2'0000'                                               IOG14470
         DC    XL1'0A'                                                  IOG14480
         DC    XL2'0200'      UNIT CHECK                                IOG14490
         DC    XL1'20'        BUS OUT CHECK                             IOG14500
*                                      BOTH COMMAND AND DATA            IOG14510
         DC    CL1'R'                                                   IOG14520
         DC    CL1'S'                                                   IOG14530
         DC    AL2(PRNNRT1-ERPTABLE)                                    IOG14540
         DC    X'04'                                                    IOG14550
         DC    PL2'796'                                                 IOG14560
         DC    XL1'02'                                                  IOG14570
         DC    XL1'06'                                                  IOG14580
         DC    XL1'02'                                                  IOG14590
         DC    XL1'01'                                                  IOG14600
         DC    FL1'-8'                                                  IOG14610
         DC    XL1'00'                                                  IOG14620
         DC    XL1'20'                                                  IOG14630
         DC    XL1'08'                                                  IOG14640
         DC    XL1'00'                                                  IOG14650
         DC    9XL2'0000'                                               IOG14660
*              CHANNEL 9 STATE = CHECKED FOR IN SENSE RT                IOG14670
         DC    X'0D'                                                    IOG14680
         DC    XL2'0200'      UNIT CHECK                                IOG14690
         DC    XL1'20'        COMMAND REJECT                            IOG14700
         DC    CL1'R'                                                   IOG14710
         DC    CL1'N'                                                   IOG14720
         DC    AL2(PRNNRT2-ERPTABLE)                                    IOG14730
         DC    XL1'D5'                                                  IOG14740
         DC    PL2'699'                                                 IOG14750
         DC    XL1'02'                                                  IOG14760
         DC    XL1'06'                                                  IOG14770
         DC    XL1'05'                                                  IOG14780
         DC    XL1'05'                                                  IOG14790
         DC    FL1'-8'                                                  IOG14800
         DC    XL1'00'                                                  IOG14810
         DC    XL1'80'                                                  IOG14820
         DC    XL1'0A'                                                  IOG14830
         DC    XL1'00'                                                  IOG14840
         DC    9XL2'0000'                                               IOG14850


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 028


         DC    XL1'0E'                                                  IOG14860
         DC    XL2'0200'      UNIT CHECK                                IOG14870
         DC    XL1'20'        DATA CHECK                                IOG14880
         DC    CL1'R'                                                   IOG14890
         DC    CL1'N'                                                   IOG14900
         DC    AL2(PRNNRT2-ERPTABLE)                                    IOG14910
         DC    X'D5'                                                    IOG14920
         DC    PL2'795'                                                 IOG14930
         DC    XL1'02'                                                  IOG14940
         DC    XL1'06'                                                  IOG14950
         DC    XL1'02'                                                  IOG14960
         DC    XL1'05'                                                  IOG14970
         DC    FL1'-8'                                                  IOG14980
         DC    XL1'00'                                                  IOG14990
         DC    XL1'08'                                                  IOG15000
         DC    XL1'09'                                                  IOG15010
         DC     XL1'00'                                                 IOG15020
         DC    9XL2'0000'                                               IOG15030
*                                      PROGRAM CHECK                    IOG15040
         IOGENE VALUE=(0F,0020,20,R,N,PRNNRT2-ERPTABLE,D5,698,02,06,   -IOG15050
               01,05,-8,00,00,0C)                                       IOG15060
*                                      PROTECTION CHECK                 IOG15070
         IOGENE VALUE=(10,0010,20,R,N,PRNNRT2-ERPTABLE,D5,697,02,06,   -IOG15080
               01,05,-8,00,00,0D)                                       IOG15090
*                                      UNIT EXCEPTION                   IOG15100
*                                      CHECKED FOR IN                   IOG15110
*                                      PRINTER'S INTERRUPT              IOG15120
*                                      PROCESSING RT (INTPRNN)          IOG15130
*                                      INCORRECT LENGTH                 IOG15140
         IOGENE VALUE=(11,0040,20,R,N,PRNNRT2-ERPTABLE,D5,696,02,06,   -IOG15150
               01,05,-8,00,00,0F)                                       IOG15160
*                                      NO MATCH ENTRY FOR               IOG15170
*                                      NON-EXISTENT DEVICE TYPE         IOG15180
         IOGENE VALUE=(12,0200,20,R,N,PRNNRT2-ERPTABLE,D5,491,06,06,   -IOG15190
               06,8F,+0,00,8F,23)                                       IOG15200
TAB1403E EQU   *                                                        IOG15210
*                                      NO MATCH ENTRY FOR               IOG15220
*                                      NON-EXISTENT IO ERROR            IOG15230
         IOGENE VALUE=(13,0200,20,R,N,PRNNRT2-ERPTABLE,D5,490,07,06,   -IOG15240
               06,DF,+0,00,DF,15)                                       IOG15250
*                                      PCI INTERRUPT                    IOG15260
*                                      CHECKED FOR IN                   IOG15270
*                                      PRINTER'S INTERRUPT              IOG15280
*                                      PROCESSING RT (INTPRNN)          IOG15290
*                                      PRESENTLY NOT IMPLEMENTED        IOG15300
T48TB2   EQU   *                       CAL LOC                          IOG15310
         EJECT                                                          IOG15320
*                                 PRNN RECOVERY PROCEDURES              IOG15330
*                                                                       IOG15340
         DS    0H                 INSURE INST ALIGN                     IOG15350
PRNNRT1  EQU   *                       RETRY = 1 TIME                   IOG15360
         USING *,R15                                                    IOG15370
         ST    R14,ERPRTSX3       SAVE RETURN                           IOG15380
         ST    R2,ERPRTSY3        SAVE BASE REG                         IOG15390
         DROP  R15                                                      IOG15400


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 029


         USING PRNNRT1,R2                                               IOG15410
         LR    R2,R15             INIT BASE REG                         IOG15420
*                                                                       IOG15430
         CLI   EIOTYPE,C'C'            COMPLETION ERR                   IOG15440
         BE    PRNN1ERR                YES,THEN DATA BUS OUT CHECK      IOG15450
*                                      RETRY FOR COMMAND BUS OUT CHECK  IOG15460
PRNNRU1  EQU   *                                                        IOG15470
         XC    CSW(8),CSW                 CLEAR CSW                     IOG15480
******                                                                  IOG15490
         LH    R8,ERPDEVA                                               IOG15500
PRNNRU2  EQU   *                                                        IOG15510
         TIO   0(R8)                                                    IOG15520
         BC    12,PRNNRU3              AVAIL+CSW-STORED                 IOG15530
         BC    1,PRNN1ERR              NOT OPERATIONAL                  IOG15540
         B     PRNNRU2                 CLEAR BUSY                       IOG15550
******                                                                  IOG15560
PRNNRU3 EQU   *                                                         IOG15570
         STARTIO DEVADD=ERPDEVA,IOREG=R8,PROGADD=ERPCAW  RETRY          IOG15580
         BC    8,PRNN1OK          GOOD START IO                         IOG15590
*                                     NO IMMEDIATE CSW-STORED BECAUSE-  IOG15600
*                                      NOP-CCW ADDED                    IOG15610
         RTCALLER ENTRY=PRNSIOER  ANALYZE ERR                           IOG15620
         CLI   GRN1ERBY,C'N'           PERM ERROR?                      IOG15630
         BE    PRNNRU1                 NO,RETRY                         IOG15640
*                                                                       IOG15650
PRNN1ERR MVI   ERPEC,X'FF'        IOERR FAILED = PERMANENT ERROR        IOG15660
         MVI   EIOCC,X'FF'                                              IOG15670
PRNN1RET EQU   *                                                        IOG15680
         L     R2,ERPRTSY3        RESTORE CALLER'S BASE REG             IOG15690
         L     R14,ERPRTSX3       AND RETURN ADDRESS                    IOG15700
         BR    R14                                                      IOG15710
*                                                                       IOG15720
PRNN1OK  EQU   *                                                        IOG15730
         MVC   ERPWTDEV,ERPDEVN   BUILD WAIT LIST                       IOG15740
         L     R15,ERPADDWT                                             IOG15750
         LA    R1,ERPWT                                                 IOG15760
         BALR  R14,R15                                                  IOG15770
*                                 UPON RETURN FROM WAIT                 IOG15780
*                                 CHECK FOR ERROR                       IOG15790
         CLI   GRN1ERBY,C'N'      AN I/O ERROR ON RETURN?               IOG15800
         BE    PRNN2OK            NO I/O ERROR ON RETRY                 IOG15810
         RTCALLER ENTRY=PRNCIOER                                        IOG15820
         CLI   GRN1ERBY,C'Y'           PERM ERROR                       IOG15830
         BE    PRNN1ERR                                                 IOG15840
PRNN2OK  EQU   *                                                        IOG15850
         MVI   ERPEC,X'00'        ENSURE NON-ERROR RETURN CODE          IOG15860
         MVI   EIOCC,X'00'        RETRY OK                              IOG15870
         B     PRNN1RET                                                 IOG15880
*                                                                       IOG15890
*                                                                       IOG15900
PRNNRT2  EQU   *                  NO-RETRY ROUTINE                      IOG15910
         USING *,R15                                                    IOG15920
         ST    R14,ERPRTSX3                                             IOG15930
         ST    R2,ERPRTSY3                                              IOG15940
         DROP  R15                                                      IOG15950


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 030


         USING PRNNRT2,R2                                               IOG15960
         LR    R2,R15                                                   IOG15970
*                                                                       IOG15980
         MVI   ERPEC,X'EF'        NO RETRY POSSIBLE                     IOG15990
         MVI   EIOCC,X'EF'                                              IOG16000
PRNN2RET EQU   *                                                        IOG16010
         L     R2,ERPRTSY3                                              IOG16020
         L     R14,ERPRTSX3                                             IOG16030
         BR    R14                     RETURN                           IOG16040
*                                                                       IOG16050
*                                                                       IOG16060
PRNSIOER EQU   *                                                        IOG16070
PRNCIOER EQU   *                                                        IOG16080
         ST    R14,ERPRTSL1            SAVE RET LOC                     IOG16090
         TM    CSW+4,X'01'             UNIT EXCEPTION                   IOG16100
         BO    PRNNSEN1                YES                              IOG16110
         TM    CSW+4,X'02'             UNIT CHECK                       IOG16120
         BO    PRNNSEN2                YES                              IOG16130
         B     PRNNSEN3                NONE,PERM ERROR                  IOG16140
PRNNSEN2 EQU   *                                                        IOG16150
*                                      INIT SENSE OPERATION             IOG16160
         MVC   ERPSENOQ(16),PTSEN      MOVE SENSE OPER                  IOG16170
         LA    R14,ERPSENIN            INIT INPUT AREA ADDRESS          IOG16180
         ST    R14,ERPSENOQ            ST INPUT AREA IN CCW             IOG16190
         LA    R14,ERPSENOQ            INIT ADDRESS OF FIRST CCW        IOG16200
         ST    R14,ERPADSEN            CCW ADDRESS TO HD AREA           IOG16210
         MVC   ERPSENOQ(1),PTSEN       REINIT SENSE OP CODE             IOG16220
         XC    CSW,CSW                 CLEAR CSW                        IOG16230
         STARTIO DEVADD=ERPDEVA,IOREG=R8,PROGADD=ERPADSEN               IOG16240
         BC    8,PRNNSEO2              SUCESSFUL START?                 IOG16250
         B     PRNNSEN3                NO                               IOG16260
PRNNSEO2 EQU   *                       YES                              IOG16270
         MVC   ERPWTDEV,ERPDEVN                                         IOG16280
         L     R15,ERPADDWT                                             IOG16290
         LA    R1,ERPWT                                                 IOG16300
         BALR  R14,R15                 WAIT FOR SENSE COMPLETION        IOG16310
         CLI   GRN1ERBY,C'Y'                                            IOG16320
         BE    PRNNSEN3                                                 IOG16330
         TM    ERPSENIN,X'40'          INTERVEN REQ?                    IOG16340
         BZ    PRNNSEP2                NO                               IOG16350
         MVI   GRN1ERBY,C'U'           YES.                             IOG16360
         MVI   ERPEC,X'0E'             INDICATE INTERVEN REQ            IOG16370
         MVI   EIOCC,X'0E'                                              IOG16380
         L     R2,ERPRTSY3             RESTORE REGS                     IOG16390
         L     R14,ERPRTSX3                                             IOG16400
         BR    R14                     RETURN                           IOG16410
PRNNSEP2 EQU   *                                                        IOG16420
         TM    ERPSEN,X'01'            CHANNEL 9 SENSED                 IOG16430
         BZ    PRNNSEN3                NO,PERM ERROR                    IOG16440
PRNNSEN1 EQU   *                                                        IOG16450
         MVI   GRN1ERBY,C'N'           INDICATE NO ERROR                IOG16460
PRNNRET  EQU   *                                                        IOG16470
         L     R14,ERPRTSL1            RESTORE RET LOC                  IOG16480
         BR    R14                     RET TO CALLER                    IOG16490
PRNNSEN3 EQU   *                                                        IOG16500


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 031


         MVI   GRN1ERBY,C'Y'           INDICATE PERM ERROR              IOG16510
         B     PRNNRET                 ENTER EXIT                       IOG16520
*                                      PRNN'S CONSTANTS,EQUS,ADDRESSES  IOG16530
*                                      CONSTANTS                        IOG16540
PTSEN    CCW   X'04',PTSEN,X'60',1                                      IOG16550
         CCW   X'03',0,X'20',1                                          IOG16560
*                                      EQUS                             IOG16570
*                                      NONE                             IOG16580
*                                      ADDRESSES                        IOG16590
*                                      NONE                             IOG16600
         EJECT                                                          IOG16610
*                                 DEVICE OPERATION CODE TABLE           IOG16620
*                             CHARACTERISTICS:                          IOG16630
*                             TABLE DEPENDENT         00                IOG16640
*                             ENTRIES:                                  IOG16650
*                             OPERATION CODE          XL1               IOG16660
*                             OPERATIONAL POINTER     XL1               IOG16670
         EJECT                                                          IOG16680
*                             1403 PRINTER OPERATION CODE               IOG16690
*                             CHARACTERISTICS                           IOG16700
         DS    0F                                                       IOG16710
ADDOP48L DC    CL4'****'                                                IOG16720
ADDOP48N DC    AL2((OP48T2-OP48T1)/(OP48D2-OP48D1))  NO OF ENTRIES      IOG16730
ADDOP48D DC    AL2(OP48D2-OP48D1)  SIZE OF AN ENTRY                     IOG16740
ADDOP48T DC    AL2(OP48T2-OP48T1)  SIZE OF TABLE                        IOG16750
ASSOP48S DC    AL2(0)          NO SENSE INFO                            IOG16760
*                             ENTRIES                                   IOG16770
OP48D1   EQU   *                                                        IOG16780
OP48T1     EQU   *                                                      IOG16790
OP1403   EQU   *                                                        IOG16800
         DC    XL1'01'                                                  IOG16810
         DC    XL1'02'                                                  IOG16820
OP48D2   DC    X'09'                                                    IOG16830
         DC    X'02'                                                    IOG16840
         DC    X'11'                                                    IOG16850
         DC    X'02'                                                    IOG16860
         DC    X'19'                                                    IOG16870
         DC    X'02'                                                    IOG16880
         DC    X'04'                                                    IOG16890
         DC    X'04'                                                    IOG16900
         DC    X'0B'                                                    IOG16910
         DC    X'05'                                                    IOG16920
         DC    X'13'                                                    IOG16930
         DC    X'05'                                                    IOG16940
         DC    X'1B'                                                    IOG16950
         DC    X'05'                                                    IOG16960
         DC    CL1'T'                                                   IOG16970
         DC    CL1'M'                                                   IOG16980
         DC    X'81'                                                    IOG16990
         DC    X'02'                                                    IOG17000
OP1403E  EQU   *                                                        IOG17010
         DC    X'83'                                                    IOG17020
         DC    X'05'                                                    IOG17030
OP48T2   EQU   *                                                        IOG17040
         EJECT                                                          IOG17050


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 032


*                             DEVICE MESSAGE TABLE                      IOG17060
*                             CHARACTERISTICS:                          IOG17070
*                             TABLE DEPENDENT         00                IOG17080
*                             ENTRIES:                                  IOG17090
*                             MESSAGE IDENTIFER       XL1               IOG17100
*                             MESSAGE                 CLNN              IOG17110
*                             MESSAGE NUMBER          PL2               IOG17120
*                             MESSAGE TYPE            CL1               IOG17130
         EJECT                                                          IOG17140
*                             OPERATOR MESSAGE TABLE                    IOG17150
*                                 NO DSECT(NOT USED)                    IOG17160
*                             CHARACTERISTICS                           IOG17170
         DS    0F                                                       IOG17180
ADDOPERL DC    CL4'****'                                                IOG17190
ADDOPERN DC    AL2((OPERT2-OPERT1)/(OPERD2-OPERD1))    NO OF ENTRIES    IOG17200
ADDOPERD DC    AL2(OPERD2-OPERD1)  SIZE OF AN ENTRY                     IOG17210
ADDOPERT DC    AL2(OPERT2-OPERT1)  SIZE OF TABLE                        IOG17220
ADDOPERS DC    AL2(0)         NO SENSE INFO                             IOG17230
*                             ENTRIES                                   IOG17240
MESOPER  EQU   *                                                        IOG17250
OPERT1   EQU   *              CAL LOC                                   IOG17260
OPERD1   EQU   *              CAL LOC                                   IOG17270
         DC    X'01'                                                    IOG17280
         DC    CL25'HARDWARE ERROR-CHANNEL'                             IOG17290
         DC    PL2'599'                                                 IOG17300
OPERD2   DC    X'02'                                                    IOG17310
         DC    CL25'HARDWARE ERROR-DEVICE'                              IOG17320
         DC    PL2'598'                                                 IOG17330
         DC    X'03'                                                    IOG17340
         DC    CL25'HARDWARE ERROR-DEVICE'                              IOG17350
         DC    PL2'597'                                                 IOG17360
MESOPERE DC    X'04'                                                    IOG17370
         DC    CL25'HARDWARE ERROR-INTERFACE'                           IOG17380
         DC    PL2'596'                                                 IOG17390
OPERT2   EQU   *                                                        IOG17400
         EJECT                                                          IOG17410
*                             OPERATION TYPE MESSAGE TABLE              IOG17420
*                             CHARACTERISTICS                           IOG17430
         DS    0F                                                       IOG17440
ADDOPL   DC    CL4'****'                                                IOG17450
ADDOPN   DC     AL2((OPT2-OPT1)/(OPD2-OPD1))     NO OF ENTRIES          IOG17460
ADDOPD   DC    AL2(OPD2-OPD1) SIZE OF AN ENTRY                          IOG17470
ADDOPT   DC    AL2(OPT2-OPT1) SIZE OF TABLE                             IOG17480
ADDOPS   DC    AL2(0)         NO SENSE INFO                             IOG17490
*                             ENTRIES                                   IOG17500
MESOP    EQU   *                                                        IOG17510
OPT1     EQU   *              CAL LOC                                   IOG17520
OPD1     EQU   *              CAL LOC                                   IOG17530
         DC    X'01'                                                    IOG17540
         DC    CL15'READ'                                               IOG17550
OPD2     DC    X'02'                                                    IOG17560
         DC    CL15'WRITE'                                              IOG17570
         DC    X'03'                                                    IOG17580
         DC    CL15'SETTING STATUS'                                     IOG17590
         DC    X'04'                                                    IOG17600


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 033


         DC    CL15'ACQUIRING STATUS'                                   IOG17610
         DC    X'05'                                                    IOG17620
         DC    CL15'POSITIONNING'                                       IOG17630
         DC    X'06'                                                    IOG17640
         DC    CL15'UNIDENTIFIED'                                       IOG17650
MESOPEND EQU   *                                                        IOG17660
         DC    XL1'07'                                                  IOG17670
         DC    CL15'DASD DIAG I/O'                                      IOG17680
OPT2     EQU   *              CAL LOC                                   IOG17690
         EJECT                                                          IOG17700
*                             ERROR TYPE TABLE                          IOG17710
*                             CHARACTERISTICS                           IOG17720
         DS    0F                                                       IOG17730
ADDSTL   DC    CL4'****'                                                IOG17740
ADDSTN   DC    AL2((STT2-STT1)/(STD2-STD1))                             IOG17750
ADDSTD   DC    AL2(STD2-STD1)                                           IOG17760
ADDSTT   DC    AL2(STT2-STT1)                                           IOG17770
ADDSTS   DC    AL2(0)         NO SENSE INFO                             IOG17780
*                             ENTRIES                                   IOG17790
MESST    EQU   *                                                        IOG17800
STT1     EQU   *                                                        IOG17810
STD1     EQU   *                                                        IOG17820
         DC    X'01'                                                    IOG17830
         DC    CL20'USER ERROR'                                         IOG17840
STD2     DC    X'02'                                                    IOG17850
         DC    CL20'HARDWARE ERROR'                                     IOG17860
         DC    X'03'                                                    IOG17870
         DC    CL20'ERP LOAD FAILED'                                    IOG17880
         DC    X'04'                                                    IOG17890
         DC    CL20'ERP SENSE FAILED'                                   IOG17900
         DC    X'05'                                                    IOG17910
         DC    CL20'SYSTEM ERROR'                                       IOG17920
MESSTE   EQU   *                                                        IOG17930
         DC    X'06'                                                    IOG17940
         DC    CL20'ERP LOOKUP FAILED'                                  IOG17950
STT2     EQU   *              CAL LOC                                   IOG17960
         EJECT                                                          IOG17970
*                             DEVICE TYPE TRANSLATION TABLE             IOG17980
*                             CHARACTERISTICS                           IOG17990
         DS    0F                                                       IOG18000
ADDDVNML DC    CL4'****'                                                IOG18010
ADDDVNMN DC    AL2((DVNMT2-DVNMT1)/(DVNMD2-DVNMD1))                     IOG18020
ADDDVNMD DC    AL2(DVNMD2-DVNMD1)                                       IOG18030
ADDDVNMT DC    AL2(DVNMT2-DVNMT1)                                       IOG18040
ADDDVNMS DC    AL2(0)                                                   IOG18050
*                             ENTRIES                                   IOG18060
MESDVNM  EQU   *                                                        IOG18070
DVNMT1   EQU   *              CAL LOC                                   IOG18080
DVNMD1   EQU   *              CAL LOC                                   IOG18090
         DC    X'01'                                                    IOG18100
         DC    CL8'CONSOLE'                                             IOG18110
DVNMD2   DC    X'02'                                                    IOG18120
         DC    CL8'PRINTER'                                             IOG18130
         DC    X'03'                                                    IOG18140
         DC    CL8'PUNCH'                                               IOG18150


FILE: IOGENTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 034


         DC    X'04'                                                    IOG18160
         DC    CL8'READER'                                              IOG18170
         DC    X'05'                                                    IOG18180
         DC    CL8'DSK-2311'                                            IOG18190
         DC    X'06'                                                    IOG18200
         DC    CL8'DSK-2314'                                            IOG18210
         DC    X'07'                                                    IOG18220
         DC    CL8'TAPE-00'                                             IOG18230
         DC    X'08'                                                    IOG18240
         DC    CL8'TAPE-04'                                             IOG18250
         DC    X'09'                                                    IOG18260
         DC    CL8'TAPE-03'                                             IOG18270
MESDVNME EQU   *                                                        IOG18280
         DC    X'0A'                                                    IOG18290
         DC    CL8'TIMER'                                               IOG18300
DVNMT2   EQU   *              CAL LOC                                   IOG18310
         EJECT                                                          IOG18320
.BYPASS  ANOP                                                           IOG18330
*                                      DROP ALL REGS USED BY MACRO      IOG18340
*                                      TO ENSURE PROPER ASSEMBLY        IOG18350
*                                      OF CSECT                         IOG18360
         DROP  R2,R4,R5,R6,R7,R8,R12,R13                                IOG18370
         MEND                                                           IOG18380


FILE: MESOPD   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          ERP04970
         MESOPD                                                         ERP04980
MESOPD   DSECT                                                          ERP04990
MESOPI   DS    XL1                                                      ERP05000
MESOPV   DS    CL15               OPERATION TYPE EX: READ               ERP05010
         MEND                                                           ERP05020


FILE: MESOUTD  ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          MES00010
         MESOUTD                                                        MES00020
*        DATED:                   6 JUNE 1971                           MES00030
*** PTF 'MESOUTD AM3636CA' HAS BEEN APPLIED ***                         MES00040
MESOUTD   DSECT                                                         MES00050
* MESIO1    DS    0D                 I/O PROG1 FOR ERROR MESSAGE        MES00060
* MESWT1   DS    CL8                                                    MES00070
* MESWT2   DS    CL8                                                    MES00080
* MESWT3   DS    CL8                                                    MES00090
* MESWT4   DS    CL8                                                    MES00100
MESIO2   DS    0D                      I/O PROG2 FOR ERROR MESSAGE      MES00110
MESLIST  DS    CL8                     TYPLIN FUNC                      MES00120
         DS    AL1                     TERMINAL NO.                     MES00130
MESLISTA DS    AL3                     ADDRESS OF OUTPUT                MES00140
         DS    CL1                     NOT USED                         MES00150
         DS    AL3                     MESSAGE LENGTH                   MES00160
MESLISTE EQU   *                                                        MES00170
         DS    2CL8                    FILLER TO EQUAL PROG1 LENGTH     MES00180
*                                                                       MES00190
MES1     DS    0CL80               MESSAGE 1                            MES00200
MES1ERP   DS    CL5                IOERR: PROGRAM ISSUING MESSAGE       MES00210
FILL1    DS    CL1                                                      MES00220
MES1TYPE DS    CL1                I OR R OR A                           MES00230
FILL2    DS    CL1                                                      MES00240
MES1NUM  DS    CL4                NUMBER OF ERROR MESSAGE               MES00250
FILL2A    DS    CL1                                                     MES00260
MES1INFO EQU   *              R OR A OR I GEN INFO                      MES00270
MES1DNFO DS    CL15               R OR A OR I DET INFO                  MES00280
FILL3    DS    CL1                                                      MES00290
MES1ADDH DS    CL8                     ADDRESS:                         MES00300
FILL6    DS    CL1                                                      MES00310
MES1ADD  DS    CL3                     HEX ADDRESS: EXAMPLE 192         MES00320
FILL7    DS    CL1                                                      MES00330
MES1DEVH  DS    CL7                DEVICE:                              MES00340
FILL4    DS    CL1                                                      MES00350
MES1DEV  DS    CL8                DEVICE TYPE: EXAMPLE: PRINTER         MES00360
FILL5    DS    CL1                                                      MES00370
MES1NAMH DS    CL5                NAME:                                 MES00380
FILL8    DS    CL1                                                      MES00390
MES1NAME DS    CL4                DEVICE NAME EXAMPLE PRN1              MES00400
FILL8A   DS    CL11                                                     MES00410
*                                                                       MES00420
         ORG   MES1+80                                                  MES00430
MES2     DS    0CL80               MESSAGE 2                            MES00440
MES2OP   DS    CL15               TYPE OF OPERATION--LISTED BELOW       MES00450
*                                 READ                                  MES00460
*                                 WRITE                                 MES00470
*                                 SETTING STATUS                        MES00480
*                                 SEEKING STATUS                        MES00490
*                                 POSITIONING                           MES00500
*                                 NOT IDENTIFIED                        MES00510
FILL9    DS    CL1                                                      MES00520
MES2OPH  DS    CL9            OPERATION                                 MES00530
FILL10  DS    CL1                                                       MES00540
MES2LOCH DS    CL9            LOCATION:                                 MES00550


FILE: MESOUTD  ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 002


FILL11   DS    CL1                                                      MES00560
MES2LOC  DS    CL8            ADDRESS OF I/O OPERATION                  MES00570
FILL12   DS    CL1                                                      MES00580
MES2STH  DS    CL7            STATUS:                                   MES00590
FILL12A  DS    CL1                                                      MES00600
MES2ERR  DS    CL20           TYPE OF ERROR--LISTED BELOW               MES00610
*                             USER ERROR                                MES00620
*                             HARDWARE ERROR                            MES00630
*                             IOERR LOAD FAILURE                        MES00640
*                             IOERR SENSE FAILURE                       MES00650
*                             SYSTEM ERROR                              MES00660
*                             I/O ERROR IN IOERR                        MES00670
FILL13   DS    CL1                                                      MES00680
*                                                                       MES00690
         ORG   MES2+80                                                  MES00700
MES3     DS    0CL80          MESSAGE3                                  MES00710
MES3DSHD DS    CL23           REQUESTED DISK POSITION                   MES00720
FILL14   DS    CL1                                                      MES00730
MES3CYLH DS    CL9            CYLINDER:                                 MES00740
FILL15   DS    CL1                                                      MES00750
MES3CYL  DS    CL3            CYLINDER NUMBER: EXAMPLE 005              MES00760
FILL16   DS    CL1                                                      MES00770
MES3TKH  DS    CL6            TRACK                                     MES00780
FILL17   DS    CL1                                                      MES00790
MES3TK   DS    CL2            TRACK NO. EXAMPLE 02                      MES00800
FILL18   DS    CL1                                                      MES00810
MES3RECH DS    CL7            RECORD:                                   MES00820
FILL19   DS    CL1                                                      MES00830
MES3REC  DS    CL2            RECORD NO: EXAMPLE 05                     MES00840
FILL20   DS    CL1                                                      MES00850
MES3OR   DS    CL2            OR                                        MES00860
FILL21  DS    CL1                                                       MES00870
MES3IDH  DS    CL3            ID:                                       MES00880
FILL21A   DS    CL1                                                     MES00890
MES3ID   DS    CL14           SEEK ARGUMENT & RECORD NO.                MES00900
*                                                                       MES00910
         ORG   MES3+80                                                  MES00920
MES4     DS    0CL80          MESSAGE 4                                 MES00930
MES4CCWH DS    CL4            CCW:                                      MES00940
FILL22   DS    CL1                                                      MES00950
MES4CCW  DS    CL16           CCW OF ERROR OPERATION                    MES00960
FILL23   DS    CL1                                                      MES00970
MES4CAWH DS    CL4            CAW:                                      MES00980
FILL24   DS    CL1                                                      MES00990
MES4CAW  DS    CL8            ADDRESS OF FIRST CCW IN I/O PROGRAM       MES01000
FILL25   DS    CL1                                                      MES01010
MES4CSWH DS    CL4            CSW:                                      MES01020
FILL26   DS    CL1                                                      MES01030
MES4CSW  DS    CL16           CSW OF ERROR OPERATION                    MES01040
FILL27   DS    CL1                                                      MES01050
MES4SENH  DS    CL6                SENSE:                               MES01060
FILL28   DS    CL1                                                      MES01070
MES4SEN  DS    CL12               SENSE INFO                            MES01080
FILL29   DS    CL1                                                      MES01090
         ORG   MES4+80                                                  MES01100


FILE: MESOUTD  ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 003


MESEND   EQU   *                                                        MES01110
         MEND                                                           MES01120


FILE: MESTBVAL ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          ERP05350
         MESTBVAL                                                       ERP05360
MESTBVAL DSECT                                                          ERP05370
*                                 TABLE CHARACTERISTICS                 ERP05380
MESSEP   DS    CL4                REC TABLE SEPARATORS                  ERP05390
MESENTNO DS    AL2                NO OF ENTRIES                         ERP05400
MESENTSZ DS    AL2                SIZE OF EACH ENTRY                    ERP05410
MESTABSZ DS    AL2                SIZE OF TABLE                         ERP05420
MESSNSZ  DS    AL2                NO. OF SENSE BYTES                    ERP05430
MESRCBEG EQU   *                  REC TABLE START                       ERP05440
         MEND                                                           ERP05450


FILE: NUCON    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          00286000
         NUCON &OPTION                                                  00287000
         AIF   (N'&OPTION NE 0).SEQ2                                    00288000
NUCONSCT DSECT                                                          00289000
.SEQ2    ANOP                                                           00290000
*                                                                       00291000
*   CMS SYSTEM CONTROL WORDS                                            00292000
*                                                                       00293000
LSTSVC   DC    A(0) _____________ V(SAVE AREA LAST SVC)                 00294000
         SPACE                                                          00295000
CORESIZ  DC    F'262144' ________ CORE SIZE                             00296000
         SPACE                                                          00297000
USFL     DC    A(*-*) ___________ V(START USER STORAGE)                 00298000
         SPACE                                                          00299000
SWITCH   DS    0F _______________ LANGUAGE PROCESSOR INDICATORS         00300000
*                                                                       00301000
SWFORT   DC    X'00' ____________ FORTRAN INDICATORS ...                00302000
FORTCOMP EQU   X'80' ____________ COMPILER IN PROGREES                  00303000
FORTEXEC EQU   X'08' ____________ EXECUTING FORTRAN OBJECT CODE         00304000
*                                                                       00305000
SWASM    DC    X'00' ____________ ASSEMBLY INDICATORS ...               00306000
ASMCOMP  EQU   X'80' ____________ ASSEMBLER IN PROGRESS                 00307000
BLDLSWT  EQU   X'10' ____________ BYPASS MACLIB DICTIONARY SEARCH       00308000
DYLD     EQU   X'08' ____________ DYNAMIC LOADING IN PROGRESS           00309000
DYLIBO   EQU   X'04' ____________ OMIT DYNAMIC LIBRARY SCAN             00310000
DYLIBNOW EQU   X'02' ____________ DYNAMIC SCAN LIBRARIES                00311000
MNSD     EQU   X'01' ____________ MAIN SEGMENT DEFINITION FOUND         00312000
*                                                                       00313000
SWPLI    DC    X'00' ____________ PL/I INDICATORS ...                   00314000
PLICOMP  EQU   X'80' ____________ COMPILER IN PROGRESS                  00315000
PLIEXEC  EQU   X'08' ____________ EXECUTING PL/I OBJECT CODE            00316000
*                                                                       00317000
SWCOB    DC    X'00' ____________ COBOL INDICATORS                      00318000
         SPACE                                                          00319000
STADDR   DC    F'0' _____________ ADDRESS TO START USER EXECUTION       00320000
LDRTBL   DC    X'02' ____________ NO. PAGES RESERVED FOR LOADER TABLES  00321000
         DC    AL3(X'40000') ____ V (HIGHEST LOADER TABLE ENTRY)        00322000
TBLNG    DC    H'0' _____________ H'?'                                  00323000
TBENT    DC    H'0' _____________ N'ENTRIES IN LOADER TABLE             00324000
         SPACE                                                          00325000
CMSAREA  DC    18F'00' __________ TEMPORARY CMS SAVE AREA               00326000
         SPACE                                                          00327000
LNKLST   DS    0F _______________ OS LINKAGE CHAINS                     00328000
CHNPTR   DC    F'0' _____________ V(LAST LINKAGE BLOCK)                 00329000
LSTBEG   DC    A(0) _____________ A(ENTRY POINT OF LAST MODULE)         00330000
         SPACE                                                          00331000
DEVEXT   DC    F'0' _____________ DEVICE TABLE EXTENSION                00332000
         SPACE                                                          00333000
GFLST    DS    0F _______________ USER STORAGE POINTERS                 00334000
FSTFRE   DC    F'0' _____________ A(START OF USER FREE STORAGE)         00335000
LENFRE   DC    F'0' _____________ L'ORIGINAL USER STORAGE AREA          00336000
FRELST   DC    F'0' _____________ A(FIRST BLOCK OF FREE STORAGE)        00337000
         SPACE                                                          00338000
OSAREA   DC    18F'00' __________ STANDARD OS-TYPE AREA                 00339000
         SPACE                                                          00340000


FILE: NUCON    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 002


LSTADR   DC    A(X'3F000') ______ (LAST PAGE IN CORE)
         SPACE                                                          00342000
LOCCNT   DC    A(0) _____________ LOCATION COUNTER                      00343000
LDADDR   DC    A(0) _____________ V(RETURN FROM LOAD/EXECUTE)           00344000
PSW      DC    D'0' _____________ USER'S "STARTING" PSW                 00345000
         SPACE                                                          00346000
LOWEXT   DC    A(X'3E000') ______ V(PRESENT LIMIT OF "EXTEND")          00347000
HIMAIN   DC    A(*-*) ___________ V(PRESENT BOUNDARY OF "STORAGE")      00348000
         SPACE                                                          00349000
IPLDEV   DC    H'0' ____________ CMS "IPL-DEVICE"  (0000 IF "IPL CMS")  00350000
SYSDEV   DC    H'0' ____________ "SYSTEM-DISK DEVICE NUMBER FOR SYSGEN" 00351000
         SPACE                                                          00352000
MISCFLAG DS    0F _______________ SOME SPARE PARTS                      00353000
*                                                                       00354000
SYSFLAG  DC    X'00' ____________ CMS SYSTEM-FLAG:                      00355000
BARECPU  EQU   X'80' ____________ BARE S/360-MODEL 65                   00356000
BATCHMON EQU   X'40' ____________ CMS BATCH MONITOR EXECUTING           00357000
RDYMSG   EQU   X'20' ____________ CONCISE "READY" MESSAGE               00358000
KLOVR    EQU   X'10' ____________ "KO" COMMAND INVOKED                  00359000
INEXEC   EQU   X'08' ____________ "EXEC" NOW RUNNING                    00360000
NOIMPEX  EQU   X'04' ____________ OMIT "IMPLIED EXEC" (FROM TERMINAL)   00361000
RELPG    EQU   X'02' ____________ "RELEASE" UNNEEDED PAGES.             00362000
MRELPG   EQU   X'01' ____________ PERMIT "RELEASE PAGE" LOGIC
*                                                                       00363000
SYSFLAG1 DC    X'00' ____________ 2ND CMS SYSTEM-FLAG:                  00364000
ERPSTAT  EQU   X'80' ____________ "ERROR RECOVERY PROCEDURE" ACTIVE     00365000
NOUPD    EQU   X'40' ____________ "NO UPDATE OF DIRECTORY"              00366000
NODATIM  EQU   X'10' ____________ "NO CHANGE OF DATE & TIME"            00368000
*                                                                       00369000
IOTYPE   DC    X'DD' ____________ OS ACCESS METHOD IDENT                00370000
*   C=CLOSE(20)                                                         00371000
*   E=NOTE                                                              00372000
*   G=GET                                                               00373000
*   I=POINT  J=OPENJ(22)                                                00374000
*   K=CHECK                                                             00375000
*   L="CONDITIONAL STORAGE REQUESTS"(4,5,10)                            00376000
*   M=WTOR(35)  N=WTO(35)                                               00377000
*   O=OPEN(19)  P=PUT                                                   00378000
*   R=READ                                                              00379000
*   T=TCLOSE(23)                                                        00380000
*                                                                       00381000
EXECSWT  DC    X'00' ____________ BATCH MONITOR INDICATORS              00382000
$EOS     EQU   X'40' ____________ END-OF-INPUT STREAM                   00383000
$BAT     EQU   X'20' ____________ BATCH MONITOR IN PROGRESS             00384000
$JTERM   EQU   X'10' ____________ JOB BEING TERMINATED                  00385000
$LOAD    EQU   X'08' ____________ LOADING EXECUTABLE CODE               00386000
$NONX    EQU   X'04' ____________ ERRORS. SUPPRESS EXECUTION            00387000
$DUMP    EQU   X'02' ____________ DUMPING...                            00388000
$INEX    EQU   X'01' ____________ IN EXECUTION                          00389000
         SPACE                                                          00390000
VIRTIME  DC    F'0' _____________ ELAPSED VIRTUAL TIME (TU)             00391000
CPUTIME  DC    F'0' _____________ ELAPSED REAL CPU TIME (TU)            00392000
         MEND                                                           00393000


FILE: PRGSCT   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          PRG00010
         PRGSCT   &OPTION                                               PRG00020
         AIF   (N'&OPTION  NE  0).CSECT                                 PRG00030
PRGSCT   DSECT                                                          PRG00040
.CSECT   ANOP                                                           PRG00050
*                                                                       PRG00060
*   STOREAGE FOR PROGRAM INTERRUPT ROUTINE ("INTPROG")                  PRG00070
*                                                                       PRG00080
DEBPSW   DC    F'0',V(BDEBUG) .   POINT TO DEBUG                        PRG00090
*                                                                       PRG00100
PIE      DS    0D .               ***PROGRAM INTERRUPT ELEMENT***       PRG00110
PICADDR  DC    F'0' .             PICA ADDRESS FROM RECENT "SPIE"       PRG00120
OPSW     DC    2F'0' .            OLD PSW AFTER PROGRAM INT.            PRG00130
         DC    5F'0' .            REGS: R14,R15,R0,R1,R2                PRG00140
*                                 ***END PROGRAM INTERRUPT ELEMENT***   PRG00150
TEMPOLD  DC    8X'00' .           WORK AREA                             PRG00160
TEMPNEW  DC    8X'00'                                                   PRG00170
R13AREA  DC    F'0' .             SAVED R13                             PRG00180
PSAVE    DC    16F'0' .           REGS SAVED AT INT TIME                PRG00190
*                                                                       PRG00200
         MEND                                                           PRG00210


FILE: RDBUF    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01305000
&LABEL   RDBUF &FCB,&AREA=,&ERROR=                                      01306000
&LABEL   LA    1,&FCB                                                   01307000
         MVC   0(8,1),=CL8'RDBUF'                                       01308000
         AIF   (N'&AREA EQ 0).SEQ1                                      01309000
         MVC   28(4,1),=A(&AREA)                                        01310000
.SEQ1    SVC   X'CA'                                                    01311000
         AIF   (N'&ERROR EQ 0).SEQ2                                     01312000
         DC    AL4(&ERROR)                                              01313000
         AGO   .SEQ3                                                    01314000
.SEQ2    DC    AL4(*+4)                                                 01315000
.SEQ3    MEND                                                           01316000


FILE: RDSYS    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01449000
&LABEL   RDSYS &SYSCB,&ERROR=                                           01450000
&LABEL   LA    1,&SYSCB                                                 01451000
         MVC   8(8,1),=CL8'READ'                                        01452000
         L     15,=V(SYSCTL)                                            01453000
         BALR  14,15                                                    01454000
         AIF   (N'&ERROR EQ 0).SEQ2                                     01455000
         DC    AL4(&ERROR)                                              01456000
         AGO   .SEQ3                                                    01457000
.SEQ2    DC    AL4(*+4)                                                 01458000
.SEQ3    SPACE 1                                                        01459000
         MEND                                                           01460000


FILE: RDTYP    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01362000
&LABEL   RDTYP &AREA,&LNG                                               01363000
&LABEL   LA    1,R&SYSNDX                                               01364000
         SVC   X'CA'                                                    01365000
         DC    AL4(*+4)                                                 01366000
         B     S&SYSNDX                                                 01367000
R&SYSNDX DS    0D                                                       01368000
         DC    CL8'WAITRD'                                              01369000
         DC    AL1(1)                                                   01370000
         DC    AL3(&AREA)                                               01371000
         DC    C'U'                                                     01372000
&LNG     DC    AL3(0)                                                   01373000
S&SYSNDX DS    0H                                                       01374000
         MEND                                                           01375000


FILE: REGS     ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01682000
         REGS                                                           01683000
         LCLA  &SETA1                                                   01684000
&SETA1   SETA  0                                                        01685000
.SEQ1    ANOP                                                           01686000
R&SETA1  EQU   &SETA1                                                   01687000
&SETA1   SETA  &SETA1+1                                                 01688000
         AIF   (&SETA1 LE 15).SEQ1                                      01689000
         MEND                                                           01690000


FILE: RET      ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          ERP03480
         RET   &REG                                                     ERP03490
         BCR   ALL,&REG                                                 ERP03500
         MEND                                                           ERP03510


FILE: RETL     ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01510000
&LABEL   RETL                                                           01511000
&LABEL   L     13,SAVEAREA                                              01512000
         LM    14,12,12(13)                                             01513000
         BR    14                                                       01514000
         MEND                                                           01515000


FILE: RTCALLER ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          ERP03520
         RTCALLER &PARM=,&ENTRY=*,&RETREG=R14,&GOREG=R15                ERP03530
         LCLC  &SVC                                                     ERP03540
         LCLC  &ADD                                                     ERP03550
&SVC     SETC  'SVC'                                                    ERP03560
&ADD     SETC  'ADD'                                                    ERP03570
         AIF   (T'&PARM EQ 'O').NTEST                                   ERP03580
         LA    1,&PARM                                                  ERP03590
.NTEST   ANOP                                                           ERP03600
         AIF   ('&ENTRY' EQ '&SVC').ISSUESV                             ERP03610
         AIF   ('&ENTRY'(1,3) EQ '&ADD').LDINST                         ERP03620
         AIF   (K'&ENTRY LT 6).LDAINST                                  ERP03630
         AIF   ('&ENTRY'(4,3) EQ '&ADD').LDINST                         ERP03640
.LDAINST ANOP                                                           ERP03650
         LA    &GOREG,&ENTRY                                            ERP03660
         AGO   .COMBR                                                   ERP03670
.LDINST  ANOP                                                           ERP03680
         L     &GOREG,&ENTRY                                            ERP03690
.COMBR   ANOP                                                           ERP03700
         BALR  &RETREG,&GOREG                                           ERP03710
         MEXIT                                                          ERP03720
.ISSUESV ANOP                                                           ERP03730
         SVC   X'CA'                                                    ERP03740
         MEND                                                           ERP03750


FILE: SAVEL    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01500000
         SAVEL                                                          01501000
         USING *,15                                                     01502000
         STM   14,12,12(13)                                             01503000
         ST    13,SAVEAREA                                              01504000
         LA    13,SAVEAREA                                              01505000
         BAL   12,*+76                                                  01506000
SAVEAREA DC    18F'0'                                                   01507000
         DROP  15                                                       01508000
         MEND                                                           01509000


FILE: SCAN     ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01691000
&LABEL   SCAN  &CCBUF,&ERROR=                                           01692000
&LABEL   L     15,ASCAN                                                 01693000
         LA    1,&CCBUF-4                                               01694000
         BALR  14,15                                                    01695000
         AIF   (N'&ERROR EQ 0).SEQ2                                     01696000
         LTR   15,15                                                    01697000
         BNZ   &ERROR                                                   01698000
.SEQ2    SPACE 1                                                        01699000
         MEND                                                           01700000


FILE: SETUP    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01388000
&LABEL   SETUP &FCB                                                     01389000
&LABEL   L     1,S&FCB+28                                               01390000
         MVC   &FCB+8(16),S&FCB+8                                       01391000
         MVC   &FCB+24(2),24(1)                                         01392000
         MVC   &FCB+32(4),32(1)                                         01393000
         MVC   &FCB+36(2),30(1)                                         01394000
         MEND                                                           01395000


FILE: STARTIO  ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          ERP08250
         STARTIO &DEVADD=,&IOREG=,&PROGADD=                             ERP08260
         LH    &IOREG,&DEVADD LOAD DEV ADDRESS                          ERP08270
         MVC   CAW(4),&PROGADD   ADDRESS OF IO PROGRAM TO CAW           ERP08280
         SIO   0(&IOREG)      SIO INST                                  ERP08290
         MEND                                                           ERP08300


FILE: STATE    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01348000
&LABEL   STATE &SFCB,&ERR                                               01349000
&LABEL   LA    1,&SFCB                                                  01350000
         SVC   X'CA'                                                    01351000
         AIF   (N'&ERR EQ 0).DEF                                        01352000
         DC    AL4(&ERR)                                                01353000
         AGO   .END                                                     01354000
.DEF     DC    AL4(*+4)                                                 01355000
.END     MEND                                                           01356000


FILE: STATUS   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          ERP03760
&NAME    STATUS  &FLD=,&CHNG=                                           ERP03770
         LCLC  &OF                                                      ERP03780
&OF      SETC  'OF'                                                     ERP03790
         AIF   (K'&CHNG LT 8).TURNACT                                   ERP03800
         AIF   ('&CHNG'(7,2) EQ '&OF').TURNOFF                          ERP03810
.TURNACT ANOP                                                           ERP03820
&NAME    OI    &FLD,&CHNG                                               ERP03830
         MEXIT                                                          ERP03840
.TURNOFF ANOP                                                           ERP03850
&NAME    XI    &FLD,&CHNG                                               ERP03860
         MEND                                                           ERP03870


FILE: SVCQ     ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          00738000
&LABEL   SVCQ                                                           00739000
&LABEL   L     15,=V(INTSVC)                                            00740000
         BALR  15,15                                                    00741000
         MEND                                                           00742000


FILE: SVCSCT   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          00683000
         SVCSCT  &CSECT                                                 00684000
         AIF   (N'&CSECT NE 0).SEQ2                                     00685000
SVCDSECT DSECT                                                          00686000
.SEQ2    ANOP                                                           00687000
*                                                                       00688000
USVCTBL  DS    0F __________ A 'HANDLE' FOR THE FOLLOWING ...           00689000
*                                                                       00690000
*   KEEP NEXT FOUR IN ORDER ....                                        00691000
JNUMB    DC    F'0' ________ NO. OF DBL-WORDS IN SVC-NUMBER TABLE       00692000
JFIRST   DC    A(*-*) ______ ADDRESS OF FIRST ITEM (IF ANY) IN TABLE    00693000
JF4      DC    F'4' ________ (FOR BXLE)                                 00694000
JLAST    DC    A(*-*) ______ ADDRESS OF LAST ITEM IN TABLE              00695000
*                                                                       00696000
*        FOR SVC-NUMBER (OTHER THAN 202) HANDLING ...                   00697000
*        (NOTE - 'CLILOOP+1' IS FILLED-IN TO SPECIFIED SVC-NUMBER)      00698000
*                                                                       00699000
CLILOOP  CLI   0(5),*-* ____ DOES SVC-NUMBER (FILLED-IN) MATCH TABLE?   00700000
         BCR   8,8 _________ 'BE' IF YES, EXIT VIA R8.                  00701000
         BXLE  5,6,0(15) ___ CONTINUE SEARCH AT "CLILOOP"               00702000
         BR    14 __________ EXIT VIA R14 IF NOT FOUND.                 00703000
*                                                                       00704000
INDEX    DC    F'0' ________ INDEXES "STACK"  (=0, 4, 8, 12, ETC.)      00705000
*                                                                       00706000
*   KEEP NEXT FOUR IN ORDER ....                                        00707000
NRMOVR   DC    1F'0' _______ NORMAL-OVERRIDE (NOT WANTED IF = 0)        00708000
ERROVR   DC    1F'0' _______ ERROR-OVERRIDE  (NOT WANTED IF = 0)        00709000
JSAVOV   DC    1F'0' _______ 'NRMOVR' SAVED HERE                        00710000
         DC    1F'0' _______ 'ERROVR' SAVED HERE                        00711000
*                                                                       00712000
NRMSAV   DC    28D'0' ______ NORMAL STANDARD INFORMATION                00713000
*                                                                       00714000
STACK    DC    A(BLK1) _____ "STACK" OF SVC-LEVELS  (1ST LEVEL)         00715000
         DC    A(BLK1+28*08)        (2ND LEVEL)                         00716000
         DC    A(BLK1+28*16)       (3RD LEVEL)                          00717000
         DC    A(BLK1+28*24)       (4TH LEVEL)                          00718000
         DC    16F'0' ____ UP TO 16 ADDITIONAL LEVELS FROM FREE STORAGE 00719000
*                                                                       00720000
*   KEEP NEXT FIVE IN ORDER .....                                       00721000
         DS    0F                                                       00722000
MODLIST  DC    CL8'LOADMOD '        ROUTINE NAME                        00723000
DUMCOM   DC    CL8'        '   MODULE 'FILENAME' FILLED IN HERE         00724000
SSMON    DC    X'FF' _______ 'FENCE' AND ALLOWS ALL INTERRUPTS.         00725000
ZERO3    DC    AL3(0) ______ THREE-BYTE ZERO                            00726000
*                                                                       00727000
TRANSRT  DC    CL8'        '  HOLDS FILENAME OF TRANSIENT ROUTINE       00728000
TRANMSK  DC    AL1(X'FF')                                               00729000
ADTRANS  DC    VL3(TRANSAR)                                             00730000
TEMP02   DC    D'0' ________ (FOR 'CVD' & OTHER SCRATCH-USE)            00731000
ADOVRSUB DC    F'0' ________ ADDRESS OF OVERRIDE SUBROUTINE             00732000
ADOVRRID DC    F'0' ________ ADDRESS OF MAIN OVERRIDE PROGRAM           00733000
*                                                                       00734000
BLK1     DC    112D'0' _____ FIRST FOUR 28-DBL-WORD SAVE-AREAS          00735000
*                                                                       00736000
         MEND                                                           00737000


FILE: SYSCB    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01461000
&LABEL   SYSCB &COM,&AREA                                               01462000
&LABEL   DS    0D                                                       01463000
         DC    CL8'SYSCTL'                                              01464000
         DC    CL8'&COM '                                               01465000
         AIF   (N'&AREA EQ 0).SEQ2                                      01466000
         DC    A(&AREA)                                                 01467000
         AGO   .SEQ3                                                    01468000
.SEQ2    DC    A(0)                                                     01469000
.SEQ3    DC    F'0'                                                     01470000
         SPACE 1                                                        01471000
         MEND                                                           01472000


FILE: SYSDVTAB ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          ERP05150
         SYSDVTAB                                                       ERP05160
SYSDVTAB DSECT                                                          ERP05170
*                                 SYSTEM DEVICE TABLE                   ERP05180
SYSDVST  EQU   *                                                        ERP05190
DEVADDD  DS    CL2                DEVICE ADDRESS                        ERP05200
DEVFLGD  DS    CL1                IO FLAG                               ERP05210
DEVUNITD DS    CL1                UNIT TYPE                             ERP05220
DEVNAMED DS    CL4                DEVICE NAME                           ERP05230
DVINTRTD DS    CL4                DEVICE INTERRUPT RT                   ERP05240
         MEND                                                           ERP05250


FILE: SYSOUT   ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01797000
&LABEL   SYSOUT  &MESS                                                  01798000
&LABEL   DS    0H                                                       01799000
         CNOP  0,4                                                      01800000
         L     15,=V(IBCOM#)                                            01801000
         BAL   14,4(15)                                                 01802000
         DC    F'6'                                                     01803000
         DC    A(M&SYSNDX)                                              01804000
         BAL   14,16(15)                                                01805000
         BAL   14,60(15)                                                01806000
         DC    XL2'0229'                                                01807000
         B     O&SYSNDX                                                 01808000
M&SYSNDX DC    X'02'                                                    01809000
         DC    X'1A'                                                    01810000
         DC    AL1(L'N&SYSNDX)                                          01811000
N&SYSNDX DC C&MESS                                                      01812000
         DC    X'22'                                                    01813000
O&SYSNDX DS    0H                                                       01814000
         MEND                                                           01815000


FILE: TSTPA    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01701000
&LABEL   TSTPA &PNO,&VALUE                                              01702000
         LCLA  &SETA                                                    01703000
&SETA    SETA  &PNO*8                                                   01704000
         AIF   ('&VALUE' EQ 'FF').SEQ2                                  01705000
&LABEL   CLC   &SETA.(8,1),=CL8&VALUE                                   01706000
         AGO   .SEQ3                                                    01707000
.SEQ2    ANOP                                                           01708000
&LABEL   CLC   &SETA.(8,1),=8X'FF'                                      01709000
.SEQ3    SPACE 1                                                        01710000
         MEND                                                           01711000


FILE: TYPE     ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01625000
&LABEL   TYPE  &MSG,&LNG                                                01626000
         GBLC  &NUM                                                     01627000
         LCLA  &SETA1,&SETA2,&SETA3                                     01628000
&NUM     SETC  '&SYSNDX'                                                01629000
&LABEL   DS    0H                                                       01630000
         AIF   ('&MSG'(1,1) NE     '''').SEQ1                           01631000
&SETA1   SETA  2                                                        01632000
&SETA2   SETA  K'&MSG-2                                                 01633000
.BACK    AIF   (&SETA1 GT &SETA2).END                                   01634000
         AIF   ('&MSG'(&SETA1,2) EQ '''''').EQ                          01635000
&SETA1   SETA  &SETA1+1                                                 01636000
         AGO   .BACK                                                    01637000
.EQ      ANOP                                                           01638000
&SETA3   SETA  &SETA3+1                                                 01639000
&SETA1   SETA  &SETA1+2                                                 01640000
         AGO   .BACK                                                    01641000
.END     ANOP                                                           01642000
&SETA1   SETA  K'&MSG-2-&SETA3                                          01643000
         GEN                                                            01644000
         DC    AL3(*+7)                                                 01645000
         DC    C'B'                                                     01646000
         DC    AL3(&SETA1)                                              01647000
         DC    CL&SETA1.&MSG                                            01648000
         AGO   .SEQ2                                                    01649000
.SEQ1    AIF   ('&MSG'(1,1) NE '(').SEQ1A                               01650000
         ST    &MSG(1),T&SYSNDX+8                                       01651000
         MVI   T&SYSNDX+8,X'01'                                         01652000
.SEQ1A   AIF   ('&LNG'(1,1) NE '(').SEQ3                                01653000
         STH   &LNG(1),T&SYSNDX+14                                      01654000
.SEQ7    ANOP                                                           01655000
         GEN                                                            01656000
         AIF   ('&MSG'(1,1) NE '(').SEQ2A                               01657000
         DC    AL3(0)                                                   01658000
         AGO   .SEQ3A                                                   01659000
.SEQ2A   ANOP                                                           01660000
         DC    AL3(&MSG)                                                01661000
.SEQ3A   ANOP                                                           01662000
         DC    C'B'                                                     01663000
         DC    AL3(0)                                                   01664000
         AGO   .SEQ2                                                    01665000
.SEQ3    AIF   (T'&LNG EQ 'U' OR T'&LNG EQ 'N').SEQ4                    01666000
         AIF   (T'&LNG EQ 'H').SEQ6                                     01667000
         MVC   T&SYSNDX+14(2),&LNG+2                                    01668000
         AGO   .SEQ7                                                    01669000
.SEQ6    ANOP                                                           01670000
         MVC   T&SYSNDX+14(2),&LNG                                      01671000
         AGO   .SEQ7                                                    01672000
.SEQ4    ANOP                                                           01673000
         GEN                                                            01674000
         DC    AL3(&MSG)                                                01675000
         DC    C'B'                                                     01676000
         DC    AL3(&LNG)                                                01677000
.SEQ2    ANOP                                                           01678000
U&SYSNDX DS    0H                                                       01679000


FILE: TYPE     ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 002


         SPACE 1                                                        01680000
         MEND                                                           01681000


FILE: TYPIN    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01403000
&LABEL   TYPIN &AREA,&LNG,&ED=U                                         01404000
&LABEL   LA    1,T&SYSNDX         ADDRESS OF P-LIST                     01405000
         SVC   202                ISSUE CMS SVC                         01406000
         DC    AL4(*+4)           IGNORE RARE ERROR.                    01407000
         B     U&SYSNDX           BRANCH AROUND P-LIST                  01408000
T&SYSNDX DS    0D                 DOUBLE WORD ALIGNMENT                 01409000
         DC    CL8'WAITRD'        ROUTINE TO READ A LINE                01410000
         DC    AL1(1)             CONSOLE ADDRESS                       01411000
         DC    AL3(&AREA)         130-CHARACTER BUFFER                  01412000
         DC    CL1'&ED'           S,T,U,V, OR X.                        01413000
&LNG     DC    AL3(0)             NUMBER OF CHARACTERS READ.            01414000
U&SYSNDX EQU   *                                                        01415000
         MEND                                                           01416000


FILE: WRBUF    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01376000
&LABEL   WRBUF &FCB,&AREA=,&ERROR=                                      01377000
&LABEL   LA    1,&FCB                                                   01378000
         MVC   0(8,1),=CL8'WRBUF'                                       01379000
         AIF   (N'&AREA EQ 0).SEQ1                                      01380000
         MVC   28(4,1),=A(&AREA)                                        01381000
.SEQ1    SVC   X'CA'                                                    01382000
         AIF   (N'&ERROR EQ 0).SEQ2                                     01383000
         DC    AL4(&ERROR)                                              01384000
         AGO   .SEQ3                                                    01385000
.SEQ2    DC    AL4(*+4)                                                 01386000
.SEQ3    MEND                                                           01387000


FILE: WRSYS    ASP360   W1                   CONVERSATIONAL MONITOR SYSTEM                         PAGE 001


         MACRO                                                          01429000
&LABEL   WRSYS &CCCB,&L,&ERROR=                                         01430000
&LABEL   LA    1,&CCCB                                                  01431000
         MVC   8(8,1),=CL8'WRITE'                                       01432000
         AIF   (N'&L EQ 0).SEQ1                                         01433000
         AIF   ('&L'(1,1) EQ '(').SEQ5                                  01434000
         MVC   20(4,1),=F'&L'                                           01435000
         AGO   .SEQA1                                                   01436000
.SEQ5    ST    &L(1),20(1)                                              01437000
         AGO   .SEQA1                                                   01438000
.SEQ1    MVC   20(4,1),=F'80'                                           01439000
.SEQA1   ANOP                                                           01440000
         L     15,=V(SYSCTL)                                            01441000
         BALR  14,15                                                    01442000
         AIF   (N'&ERROR EQ 0).SEQ2                                     01443000
         DC    AL4(&ERROR)                                              01444000
         AGO   .SEQ3                                                    01445000
.SEQ2    DC    AL4(*+4)                                                 01446000
.SEQ3    SPACE 1                                                        01447000
         MEND                                                           01448000