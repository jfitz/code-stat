; [ This translation created 10-Feb-83 by Version 4.3 ]

        .RADIX  8               ; To be safe

CSEG    SEGMENT PUBLIC 'CODESG' 
        ASSUME  CS:CSEG

INCLUDE OEM.H

        TITLE   DSKCOM - - COMMON ROUTINES FOR DISK BASICS

ALPCPM=0
MELCO=0
MODEL3=0



DSEG    SEGMENT PUBLIC 'DATASG'         ; Data Segment
        ASSUME DS:DSEG
        EXTRN   CURLIN:WORD,DSCTMP:WORD,PTRFIL:WORD,FRETOP:WORD
        EXTRN   SAVSTK:WORD,TXTTAB:WORD,VALTYP:WORD,VARTAB:WORD
        EXTRN   BUF:WORD
        EXTRN   DIRTMP:WORD
DSEG    ENDS                    ;End of data segment externals

                                ; Code Segment ( terminated by END at bottom of file )

        EXTRN   CLSFIL:NEAR,CONINT:NEAR,DERBFN:NEAR,DERRPE:NEAR,ERROR:NEAR
        EXTRN   FILOU3:NEAR,CHRGTR:NEAR
        EXTRN   INDSKC:NEAR,PRGFIL:NEAR,SCRTCH:NEAR,SNERR:NEAR
        EXTRN   STRLT3:NEAR,DERFOV:NEAR,DERBFM:NEAR,DOCNVF:NEAR,FCERR:NEAR
        EXTRN   FINBCK:NEAR,FINPRT:NEAR,FRCSTR:NEAR,FRESTR:NEAR
        EXTRN   FRMEVL:NEAR,GETBYT:NEAR,GETSPA:NEAR,GTBYTC:NEAR,GTMPRT:NEAR
        EXTRN   INCHRI:NEAR
        EXTRN   LINKER:NEAR,LIST:NEAR,MAIN:NEAR,NEWSTT:NEAR,OMERR:NEAR
        EXTRN   PTRGET:NEAR,PUTNEW:NEAR,READY:NEAR,RUNC:NEAR
        EXTRN   VMOVFM:NEAR,VMOVMF:NEAR,STRINI:NEAR

        EXTRN   FRMEQL:NEAR
        EXTRN   ENDCON:NEAR,GONE:NEAR
        EXTRN   DOASIG:NEAR,ERRFDR:NEAR,FIN:NEAR
        EXTRN   SCCPTR:NEAR

        EXTRN   GETYPR:NEAR,SYNCHR:NEAR,DCOMPR:NEAR
        PUBLIC  FIELD,PRGFLI
        PUBLIC  FILIND
        PUBLIC  MKI$,MKS$,MKD$,CVI,CVS,CVD
        PUBLIC  DLINE,PRGFL2,LRUN,LOAD,PRGFIN,MERGE,SAVE
        PUBLIC  OKGETM
        PUBLIC  RSET,LSET

DSEG    SEGMENT PUBLIC 'DATASG'
        EXTRN   PROFLG:WORD
DSEG    ENDS

        INCLUDE GIO86U
CPM86=0
        INCLUDE MSDOSU
        NMLOFS=F_BREM

        .RADIX  10
COMMENT % REVISION HISTORY
4/23/78 PGA     ALLOW # ON CLOSE

8/6/79  PGA     IF ^C ON MBASIC FOO, DONT RETURN TO SYSTEM. SEE 'NOTINI'
6/27/80 PGA     FIX INPUT#1,D# SO IT USES FINDBL INSTEAD OF FIN
                                AND THUS AVOIDS LOSING SIGNIFICANCE.

%

        SUBTTL FILINP AND FILGET -- SCAN A FILE NUMBER AND SETUP PTRFIL

        EXTRN   FILINP:NEAR,FILSCN:NEAR,FILSET:NEAR


        SUBTTL FILSCN, FILFRM, AND FILIDX

        SUBTTL  Conversion Routines
MKI$:   MOV     AL,LOW 2        ;VALUE TYPE FOR INTEGER AND NUMBER
                                ;OF CHARACTERS RESULT WILL NEED
        DB      271O    ; SKIP  ;SKIP NEXT TWO BYTES WITH "LXI  B,"
MKS$:   MOV     AL,LOW 4        ;VALUE TYPE OF SINGLE PRECISION
        DB      271O    ; SKIP  ;SKIP NEXT TWO BYTES
MKD$:   MOV     AL,LOW 8        ;VALUE TYPE OF DOUBLE-PRECISION
        PUSH    AX              ;SAVE THE NUMBER OF BYTES OF
                                ;STRING SPACE WE NEED
        CALL    DOCNVF          ;CONVERT FAC TO PROPER TYPE
        POP     AX              ;GET THE NUMBER OF BYTES NEEDED
        CALL    STRINI          ;GET A PLACE FOR THE STRING DATA
DSEG    SEGMENT PUBLIC 'DATASG'
        EXTRN   DSCPTR:WORD
DSEG    ENDS
        MOV     BX,DSCPTR       ;POINT TO THE PLACE TO STORE THE DATA
        CALL    VMOVMF          ;MOVE THE FAC VALUE INTO THE STRING CREATION
        JMP     FINBCK

CVI:    MOV     AL,LOW 1        ;SET [A] TO BE VALTYP-1
        DB      271O    ; SKIP  ;SKIP THE NEXT TWO BYTES WITH "LXI B,"
CVS:    MOV     AL,LOW 3        ;ALSO SET [A] TO NUMBER OF CHARACTERS REQUIRED -1
        DB      271O    ; SKIP  ;SKIP THE NEXT TWO BYTES
CVD:    MOV     AL,LOW 7        ;DOUBLE PRECISION VALUE TYPE -1
        PUSH    AX              ;SAVE THE VALTYP
        CALL    FRESTR          ;MAKE SURE THE ARGUMENT IS A STRING
                                ;AND GET A POINTER TO THE DESCRIPTOR
        POP     AX              ;GET BACK NUMBER OF CHARACTERS REQUIRED-1
        CMP     AL,BYTE PTR [BX]        ;MAKE SURE THE STRING IS LONGER THAN THAT
        JNAE    SHORT ??L000
        JMP     FCERR           ;IF NOT, "ILLEGAL FUNCTION CALL"
??L000:
        INC     AL              ;[A]=TRUE VALUE TYPE
        MOV     BYTE PTR VALTYP,AL      ;SETUP VALUE TYPE FOR MOVE
        INC     BX
        MOV     BX,[BX]         ;[H,L]=POINTER AT STRING DATA
                                ;AND FOR IDENTIFICATION
        JMP     VMOVFM


        SUBTTL  Read Items From A Sequential File

FILIND:
        CALL    GETYPR          ;SEE IF INPUT IS STRING OR NUMBER
        MOV     CX,OFFSET DOASIG        ;RETURN ADDRESS TO SETUP [FAC]
        CON1=0+(44*256)+32
        MOV     DX,OFFSET CON1  ;SETUP TERMINATORS SPACE AND COMMA
        JNZ     SHORT INPDOR    ;IF NUMERIC, GO READ THE FILE
        MOV     DL,DH           ;MAKE BOTH TERMINATORS COMMA
        JMP     SHORT INPDOR    ;GO READ THE FILE
;
; Entry for line input and read code for item fetching from
; sequential input files
;

DLINE:
        CALL    FILINP          ;GET FILE NUMBER SET UP
        CALL    PTRGET          ;READ STRING TO STORE INTO
        CALL    FRCSTR          ;MAKE SURE IT WAS A STRING
        MOV     CX,OFFSET FINPRT        ;RESET TO CONSOLE WHEN DONE READING
        PUSH    CX              ;SAVE ON STACK
        PUSH    DX              ;SAVE POINTER AT VARIABLE
        EXTRN   LETCON:NEAR
        MOV     CX,OFFSET LETCON        ;GOOD RETURN ADDRESS FOR ASSIGNMENT
        XOR     AL,AL           ;SET A=0 FOR STRING VALUE TYPE
        MOV     DH,AL           ;ZERO OUT BOTH TERMINATORS
        MOV     DL,AL
INPDOR: PUSH    AX              ;SAVE VALUE TYPE
        PUSH    CX              ;SAVE RETURN ADDRESS
        PUSH    BX              ;SAVE POINTER AT DATA COMING IN
                                ;A DUMMY POINTER AT BUFMIN
NOTNWT:
EXTRN   INCHR:NEAR
        CALL    INCHR           ;Read a character from file PTRFIL
        CMP     AL,LOW " "      ;SKIP LEADING SPACES
        JNZ     SHORT NOTSPC    ;EXCEPT FOR LINE INPUT
        INC     DH              ;CHECK FOR LINEINPUT
        DEC     DH
        JNZ     SHORT NOTNWT    ;SKIP ANY NUMBER
NOTSPC: CMP     AL,LOW 34       ;QUOTED STRING COMING IN?
        JNZ     SHORT NOTQTE
        MOV     AL,DL           ;MUST BE INPUT OF A STRING
        CMP     AL,LOW 44D      ;WHICH HAS [E]=44 (",")
        MOV     AL,LOW 42O      ;QUOTE BACK INTO [A]
        JNZ     SHORT NOTQTE
        MOV     DH,AL           ;TERMINATORS ARE QUOTES ONLY
        MOV     DL,AL
        CALL    INDSKC          ;READ PAST QUOTATION
        JB      SHORT QUITSI    ;IF EOF, ALL DONE
NOTQTE:
        MOV     BX,OFFSET BUF   ;BUFFER FOR DATA
        MOV     CH,LOW 255      ;MAXIMUM NUMBER OF CHARACTERS (255)
LOPCRS: MOV     CL,AL           ;SAVE CHARACTER IN [C]
        MOV     AL,DH           ;CHECK FOR QUOTED STRING
        CMP     AL,LOW 42O
        MOV     AL,CL           ;RESTORE CHARACTER
        JZ      SHORT NOTQTL    ;DON'T IGNORE CR OR STOP ON LF
        CMP     AL,LOW 13       ;CR?
        PUSH    BX              ;SAVE DEST PTR. ON STACK
        JZ      SHORT ICASLF    ;EAT LINE FEED IF ONE
        POP     BX              ;RESTORE DEST. PTR.
        CMP     AL,LOW 10       ;LF?
        JNZ     SHORT NOTQTL    ;NO, TEST OTHER TERMINATORS
LPISLF:                         ;** 5/14/82 BUG FIX(MULTIPLE LF FOR UNQUOTED STRING)
        MOV     CL,AL           ;SAVE CURRENT CHAR
        MOV     AL,DL           ;GET TERMINATOR 2
        CMP     AL,LOW 54O      ;CHECK FOR COMMA (UNQUOTED STRING)
        MOV     AL,CL           ;RESTORE CHARACTER
        JZ      SHORT ??L001
        CALL    STRCHR          ;IF NOT, STORE LF (?)
??L001:
        CALL    INDSKC          ;GET NEXT CHAR
        JB      SHORT QUITSI    ;IF EOF, ALL DONE.
        CMP     AL,LOW 10       ;** 5/14/82 BUG FIX(MULTIPLE LF FOR UNQUOTED STRING)
        JZ      SHORT LPISLF    ;** 5/14/82 BUG FIX(MULTIPLE LF FOR UNQUOTED STRING)
        CMP     AL,LOW 13       ;IS IT A CR?
        JNZ     SHORT NOTQTL    ;IF NOT SEE IF STORE NORMALLY
        MOV     AL,DL           ;GET TERMINATOR
        CMP     AL,LOW " "      ;IS IT NUMERIC INPUT?
        JZ      SHORT LPCRGT    ;IF SO, IGNORE CR, DONT PUT IN BUFFER
        CMP     AL,LOW 44       ;IS IT NON-QUOTED STRING (TERM=,)
        MOV     AL,LOW 13       ;GET BACK CR.
        JZ      SHORT LPCRGT    ;IF SO, IGNORE CR.
NOTQTL: OR      AL,AL           ;IS CHAR ZERO
        JZ      SHORT LPCRGT    ;ALWAYS IGNORE, AS IT IS TERMINATOR
                                ;FOR STRLIT (SEE QUIT2B)
        CMP     AL,DH           ;TERMINATOR ONE?
        JZ      SHORT QUITSI    ;STOP THEN
        CMP     AL,DL           ;TERMINATOR TWO?
        JZ      SHORT QUITSI
        CALL    STRCHR          ;SAVE THE CHAR
LPCRGT: CALL    INDSKC          ;READ ANOTHER CHARACTER
        JAE     SHORT LOPCRS    ;IF NOT, CHECK AS TERMINATOR
QUITSI: PUSH    BX              ;SAVE PLACE TO STUFF ZERO
        CMP     AL,LOW 34       ;STOPPED ON QUOTE?
        JZ      SHORT MORSPC    ;DON'T SKIP SPACES THEN
                                ;BUT DO SKIP FOLLOWING COMMA OR
                                ;CRLF THOUGH
        CMP     AL,LOW " "      ;STOPPED ON SPACE?
        JNZ     SHORT NOSKCR    ;NO, DON'T SKIP SPACES
                                ;OR ANY FOLLOWING COMMAS OR CRLFS EITHER
MORSPC: CALL    INDSKC          ;READ SPACES
        JB      SHORT NOSKCR    ;EOF, ALL DONE.
        CMP     AL,LOW " "
        JZ      SHORT MORSPC
        CMP     AL,LOW 44       ;COMMA?
        JNZ     SHORT ??L002
        JMP     NOSKCR          ;OK, SKIP IT
??L002:
        CMP     AL,LOW 13       ;CARRIAGE RETURN?
        JNZ     SHORT BAKUPT    ;BACK UP PAST THIS CHARACTER
ICASLF:
        CALL    INDSKC          ;READ ANOTHER
        JB      SHORT NOSKCR    ;EOF, ALL DONE.
        CMP     AL,LOW 10       ;LINE FEED?
        JZ      SHORT NOSKCR    ;OK, SKIP IT TOO
BAKUPT:
        EXTRN   BAKCHR:NEAR
        CALL    BAKCHR          ;backup file PTRFIL
NOSKCR: POP     BX              ;GET BACK PLACE TO STORE TERMINATOR
QUIT2B: MOV     BYTE PTR [BX],LOW 0     ;STORE THE TERMINATOR
        MOV     BX,OFFSET BUF-1 ;ITEM IS NOW STORED AT THIS POINT +1
        MOV     AL,DL           ;WAS IT A NUMERIC INPUT?
        SUB     AL,LOW " "      ;IF SO, [E]=" "
        JZ      SHORT NUMIMK    ;USE FIN TO SCAN IT
        EXTRN   STRLT2:NEAR
        MOV     CH,DH           ;SET [B]=44 IF SCANNING UNQUOTED STRING
        MOV     DH,LOW 0
        CALL    STRLT2
        POP     BX              ;GET BACK [H,L]
        RET                     ;DO ASSIGNMENT

NUMIMK:
        CALL    GETYPR          ;GET TYPE OF NUMERIC VARIABLE BEING READ
        LAHF                    ; PUSH PSW
        XCHG    AL,AH
        PUSH    AX
        XCHG    AL,AH           ;SAVE IT
        CALL    CHRGTR          ;READ FIRST CHARACTER
        POP     AX              ; POP PSW
        XCHG    AL,AH
        SAHF                    ;RESTORE TYPE OF VARIABLE
        LAHF                    ; PUSH PSW
        XCHG    AL,AH
        PUSH    AX
        XCHG    AL,AH           ;SAVE BACK
        JNB     SHORT ??L003
        CALL    FIN             ;SINGLE PRECISION INPUT
??L003:
        POP     AX              ; POP PSW
        XCHG    AL,AH
        SAHF                    ;GET BACK TYPE OF VAR
        EXTRN   FINDBL:NEAR
        JNAE    SHORT ??L004
        CALL    FINDBL
??L004:                         ;DOUBLE PRECISION INPUT
        POP     BX              ;GET [H,L]
        RET                     ;DO THE ASSIGNMENT

STRCHR: OR      AL,AL           ;TRYING TO STORE NULL BYTE
        JNZ     SHORT $+3
        RET                     ;RETURN, DONT STORE IT
        MOV     BYTE PTR [BX],AL        ;STORE THE CHARACTER
        INC     BX
        DEC     CH              ;128 YET?
        JZ      SHORT $+3
        RET                     ;MORE SPACE IN BUFFER, RETURN
        POP     CX              ;GET RID OF SUPERFLUOUS STACK ENTRY
        JMP     SHORT QUIT2B    ;SPECIAL QUIT

        SUBTTL  LOAD and RUN routines

PRGFLI: MOV     DH,LOW OFFSET MD_SQI    ;SEQUENTIAL INPUT MODE
PRGFL2: XOR     AL,AL           ;INTERNAL FILE NUMBER IS ALWAYS ZERO
        JMP     PRGFIL          ;SCAN FILE NAME AND DISK NUMMER
                                ;AND DO THE RIGHT THING USING MD.KIL
                                ;AS A FLAG

LRUN:
LRUN2:  MOV     AL,LOW 1
        JMP     LRUN3           ;1 INDICATES, CLOSE FILES, RUN
LOAD:   XOR     AL,AL           ;FLAG ZERO FOR "LOAD"
LRUN3:                          ;A=1 INDICATES CLOSE FILES, RUN
                                ;A=0 INDICATES CLOSE FILES, NO RUN
        LAHF                    ; PUSH PSW
        XCHG    AL,AH
        PUSH    AX
        XCHG    AL,AH           ;SAVE "RUN"/"LOAD" FLAG
        CALL    PRGFLI          ;FIND THAT FILE AND SETUP FOR
                                ;USING INDSKC SUBROUTINE
        DEC     BX              ;SEE IF NO RUN OPTION
        CALL    CHRGTR
        JZ      SHORT NOTRNL    ;NO, JUST LOAD
        CALL    SYNCHR
        DB      OFFSET 44       ;GOTTA HAVE A COMMA
        CALL    SYNCHR
        DB      OFFSET "R"      ;ONLY OPTION IS RUN
        JZ      SHORT ??L005
        JMP     SNERR           ;AND THAT BETTER BE THE END
??L005:
        POP     AX              ; POP PSW
        XCHG    AL,AH
        SAHF                    ;GET RID OF "RUN"/"LOAD" FLAG
; RUN fn closes all files, loads program, and executes program
; RUN fn,R loads program, and executes program
; LOAD fn closes all files, loads program
; LOAD fn,R loads program, and executes program
; MERGE fn merges an ASCII program
; CHAIN fn loads/merges a program, and executes program (leaving files opened)
;
        PUBLIC  CHNENT
DSEG    SEGMENT PUBLIC 'DATASG'
        EXTRN   RUNFLG:WORD
DSEG    ENDS
CHNENT:
        MOV     AL,LOW 201O     ;A=LEAVE FILES OPEN, RUN
        JMP     SHORT NOTRN1
NOTRNL: POP     AX              ; POP PSW
        XCHG    AL,AH
        SAHF                    ;restore [A]=1 for RUN, 0 for LOAD
NOTRN1:
                                ;Variable  ",R"    CHAIN   LOAD   RUN
        MOV     BYTE PTR RUNFLG,AL      ;RUNFLG=  ^O201    ^O201      0     1
        OR      AL,LOW 1        ;
        MOV     BYTE PTR NLONLY,AL      ;NLONLY=  ^O201    ^O201      1     1
                                ;If NLONLY and 200 <> 0, don't close any files
                                ;If NLONLY and 001 <> 0, don't close file 0
        CALL    SCRTCH          ;Clear variables, close files
; BELOW IS FIX (TO LABEL NOTINI) SO THAT IF ^C DURING MBASIC FOO, WONT EXIT TO SYSTEM
        MOV     BX,CURLIN       ;GET LINE NUMBER
        LAHF
        INC     BX              ;SEE IF IN INITIALIZATION
        SAHF
        MOV     AL,BH
        AND     AL,BL
        INC     AL
        JNZ     SHORT NOTINI    ;NO
        MOV     CURLIN,BX       ;SAVE DIRECT LINE NUMBER
NOTINI:
        CALL    INDSKC          ;READ THE FIRST CHARACTER
        JAE     SHORT ??L006
        JMP     MAIN            ;ALL DONE IF NOTHING IN FILE
??L006:
        CMP     AL,LOW 254      ;IS THIS A PROTECTED FILE?
        JNZ     SHORT NTPROL    ;NO
        MOV     BYTE PTR PROFLG,AL      ;SET PROTECTED FILE
        JMP     SHORT BINLOD    ;DO BINARY LOAD
NTPROL:
        INC     AL              ;IS IT A BINARY FILE?
        JZ      SHORT ??L007
        JMP     MAINGO          ;NO, SINCE PTRFIL IS NON-ZERO
??L007:
                                ;INCHR WILL USE INDSKC INSTEAD OF POLLING
                                ;THE TERMINAL
                                ;WHEN EOF IS HIT PTRFIL WILL BE RESTORED
                                ;AND LSTFRE WILL BE USED AS A FLAG
                                ;TO INDICATE WHETHER TO RUN THE
                                ;LOADED PROGRAM
;
; TIME FOR A BINARY LOAD.
; AFTER THE LOAD, THE FILE IS LINKED TOGETHER
; LSTFRE IS USED AS A FLAG WHETHER TO RUN OR NOT
;
BINLOD: MOV     BX,TXTTAB       ;GET PLACE TO START STORING INTO

EXTRN   FSTLOD:NEAR
        CALL    FSTLOD          ;USE FAST LOAD CODE IN DCPM.MAC
        EXTRN   PROLOD:NEAR
        MOV     VARTAB,BX       ;SAVE END TEMP FOR DECODING
        MOV     AL,BYTE PTR PROFLG      ;IS THIS A PROTECTED FILE?
        OR      AL,AL           ;SET CC'S
        JZ      SHORT ??L008
        CALL    PROLOD
??L008:                         ;TRANSLATE TO GOOD STUFF
        CALL    LINKER          ;FIX THE LINKS
        INC     BX              ;WHEN LINKER RETURNS, [H,L]
        INC     BX              ;POINTS TO DOUBLE ZERO
        MOV     VARTAB,BX       ;UPDATE [VARTAB]
        CALL    RUNC            ;SETUP ARYTAB, STREND
DSEG    SEGMENT PUBLIC 'DATASG'
        EXTRN   CHNFLG:WORD
DSEG    ENDS
        EXTRN   CHNRET:NEAR
        MOV     AL,BYTE PTR CHNFLG      ;CHAIN IN PROGRESS
        OR      AL,AL           ;TEST
        JZ      SHORT ??L009
        JMP     CHNRET          ;YES, GO BACK TO CHAIN CODE
??L009:
DSEG    SEGMENT PUBLIC 'DATASG'
        EXTRN   NLONLY:WORD
DSEG    ENDS
        XOR     AL,AL           ;
        MOV     BYTE PTR NLONLY,AL      ;allow all files to be closed
        CALL    FINPRT          ;reset PTRFIL to 0 and close file 0
DSEG    SEGMENT PUBLIC 'DATASG'
        EXTRN   RUNFLG:WORD
DSEG    ENDS
        MOV     AL,BYTE PTR RUNFLG      ;RUN OR NOT?
        OR      AL,AL
        JNZ     SHORT ??L010
        JMP     READY
??L010:
        JMP     NEWSTT

PRGFIN: CALL    FINPRT          ;ZERO PTRFIL
        CALL    CLSFIL          ;CLOSE FILE ZERO
        JMP     GTMPRT          ;REFETCH TEXT POINTER
PUBLIC  OUTLOD
OUTLOD: CALL    SCRTCH
        JMP     OMERR

;MERGE filespec  Statement
; (OKGETM is called by CHAIN MERGE ... in FIVEO)
;
MERGE:  POP     CX              ;ELIMINATE NEWSTT RETURN
        CALL    PRGFLI          ;READ THE NAME AND DISK
        DEC     BX              ;MUST END THERE
        CALL    CHRGTR
        JZ      SHORT OKGETM    ;READ THE FILE
        CALL    PRGFIN          ;CLOSE OUT TIME
        JMP     SNERR           ;AND "SYNTAX ERROR"
OKGETM: XOR     AL,AL           ;NO RUN OPTION WITH "MERGE"
DSEG    SEGMENT PUBLIC 'DATASG'
        EXTRN   RUNFLG:WORD
DSEG    ENDS
        MOV     BYTE PTR RUNFLG,AL      ;SET UP THE RUN FLAG
        INC     AL
        MOV     BYTE PTR NLONLY,AL      ;NLONLY=1, don't close file 0
PUBLIC  OKGET2
OKGET2:
        CALL    INDSKC          ;READ FROM [PTRFIL] FILE
        JAE     SHORT ??L011
        JMP     MAIN            ;GO BACK IF EOF
??L011:
        INC     AL              ;IS IT A BINARY FILE??
        JNZ     SHORT ??L012
        JMP     DERBFM          ;BINARY IS WRONG FILE MODE
??L012:
        INC     AL              ;OR PROTECTED BINARY FILE??
        JNZ     SHORT ??L013
        JMP     DERBFM          ;ALSO GIVE BAD FILE MODE
??L013:
        DEC     AL              ;adjust [A] for 1st INR A above
                                ;so we can put it back (backup input file)
MAINGO:
        DEC     AL              ;[A]=character which was just read from file
        CALL    BAKCHR          ;backup file PTRFIL
EXTRN   SCDASC:NEAR
        CALL    SCDASC          ;set File Code to ASCII overridding user setting)
        JMP     MAIN


        SUBTTL DISPATCH FOR DIRECT STATEMENT
;
; MAKE SURE WE'RE NOT READING A FILE IN
;

        SUBTTL  SAVE COMMAND -- ASCII OR BINARY

SAVE:
        PUSH    BX              ;save text pointer for rescan of filename
        CALL    FRMEVL          ;skip filename
        PUSH    BX
        CALL    FRESTR          ;release string temporary
        POP     BX
        DEC     BX
        CALL    CHRGTR
        JNZ     SHORT ??L014
        JMP     ENDOFS          ;branch if end-of-statement
??L014:
        CALL    SYNCHR
        DB      OFFSET 54O      ;eat comma
        CMP     AL,LOW "A"
        JNZ     SHORT ??L015
        JMP     GOODSV          ;branch if got "A"
??L015:
        CMP     AL,LOW "P"
        JZ      SHORT ??L016
        JMP     SNERR           ;branch if not "A" or "P"
??L016:
GOODSV: PUSH    BX              ;save text pointer
        PUSH    AX              ;save "A" or "P"
        CALL    CHRGTR          ;check for end-of-statement
        JZ      SHORT ??L017
        JMP     SNERR           ;error if not end-of-statement
??L017:
        POP     AX
        POP     BX              ;leave text pointer pointing at "A" or "P"
ENDOFS: POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;save End-Of-Statement text pointer
                                ;HL = text pointer to filename
        PUSH    AX              ;save "A", "P" or null option
        MOV     DH,LOW OFFSET MD_SQO    ;ELIMINATE EARLIER VERSION OF FILE
                                ;AND CREATE EMPTY FILE
        CALL    PRGFL2          ;parse file name and open it
        POP     AX              ;[A]="A", "P" or null option
        POP     BX              ;restore End-Of-Statement text pointer
        EXTRN   PROSAV:NEAR
        CMP     AL,LOW "P"      ;PROTECTED SAVE?
        JNZ     SHORT ??L018
        JMP     PROSAV          ;DO IT
??L018:
        CMP     AL,LOW "A"      ;Ascii save?
        JZ      SHORT ??L019
        JMP     BINSAV          ;if not, must be Binary Save
??L019:
        CALL    CHRGTR          ;skip "A"
        CALL    SCDASC          ;set File Code to ASCII (overridding user setting)
        JMP     LIST            ;USE THE LIST CODE TO DO THE OUTPUT
                                ;CONTROL-CS ARE NOT ALLOWED
                                ;AND AT THE END PTRFIL IS ZEROED
        EXTRN   BINSAV:NEAR


        SUBTTL  DRIVER CODE FOR CLOSE


        SUBTTL "FIELD" STATEMENT FOR SETTING UP I/O STRINGS

DSEG    SEGMENT PUBLIC 'DATASG'
        EXTRN   TEMPA:WORD,TEMPB:WORD
DSEG    ENDS
FIELD:
        CALL    FILSCN          ;GET DATA BLOCK POINTER IN [B,C]
        EXTRN   DERFNO:NEAR
        JNZ     SHORT ??L020
        JMP     DERFNO          ;error if File Not Opened
??L020:
        SUB     AL,LOW OFFSET MD_RND    ;MAKE SURE ITS A RANDOM FILE
        JZ      SHORT ??L021
        JMP     DERBFM          ;IF NOT, "BAD FILE MODE"
??L021:
        XCHG    BX,DX           ;SAVE TEXT POINTER
        MOV     BX,OFFSET FD_SIZ        ;POINT TO RECORD SIZE
        ADD     BX,CX
        MOV     BX,[BX]         ;GET IT
        MOV     TEMPA,BX        ;STORE MAX ALLOWED
        MOV     BX,0            ;ZERO MAX # OF CHARS
        MOV     TEMPB,BX
        MOV     AL,BH           ;MAKE [A]=0
        XCHG    BX,DX           ;GET BACK TEXT POINTER
        MOV     DX,OFFSET FD_DAT        ;POINT TO 5.0 FIELD BUFFER
LOPFLD: XCHG    BX,DX           ;SAVE TEXT POINTER IN [D,E]
        ADD     BX,CX           ;ADD ON DATA POINTER SO [H,L] NOW POINTS
                                ;AT THE START OF THE DATA
        MOV     CH,AL           ;SETUP COUNT OF CHARACTERS PAST BY
                                ;IN DATA AREA, SO TOTAL IS NEVER GREATER THAN 128
        XCHG    BX,DX           ;TEXT POINTER BACK INTO [H,L]
                                ;[D,E]=POINTER INTO DATA AREA
        MOV     AL,BYTE PTR [BX]        ;MORE "AS"S TO SCAN?
        CMP     AL,LOW 44       ;COMMA STARTS THE CLAUSE
        JZ      SHORT $+3
        RET                     ;BACK TO NEWSTT IF NOT
        PUSH    DX              ;SAVE THE POINTER INTO THE DATA BLOCK
        PUSH    CX              ;SAVE [B]=NUMBER OF CHARACTERS ALLOCATED
        CALL    GTBYTC          ;READ NUMBER INTO [A] FROM TEXT
        PUSH    AX              ;SAVE THIS NUMBER
        CALL    SYNCHR
        DB      OFFSET "A"      ;SCAN THE "AS"
        CALL    SYNCHR
        DB      OFFSET "S"
        CALL    PTRGET          ;GET A POINTER AT THE STRING DESCRIPTOR
        CALL    FRCSTR          ;INTO [D,E]
        POP     AX              ;GET THE NUMBER OF CHARACTERS
        POP     CX              ;GET THE NUMBER ALREADY USED
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;SAVE THE TEXT POINTER AND
                                ;[H,L]=POINTER INTO DATA BLOCK
        MOV     CL,AL           ;SAVE # OF CHARACTERS IN [C]
        PUSH    DX              ;SAVE [D,E]
        PUSH    BX              ;SAVE [H,L]
        MOV     BX,TEMPB        ;GET TOTAL SO FAR
        MOV     CH,LOW 0        ;ACCUMULATE COUNT
        ADD     BX,CX           ;Add in current field size
        MOV     TEMPB,BX        ;SAVE TOTAL AGAIN
        XCHG    BX,DX           ;TOTAL TO [D,E]
        MOV     BX,TEMPA        ;GET MAX ALLOWED
        CMP     BX,DX           ;IN RANGE?
        JAE     SHORT ??L022
        JMP     DERFOV          ;NO, GIVE ERROR
??L022:
        POP     BX              ;RESTORE [H,L]
        POP     DX              ;RESTORE [D,E]
        XCHG    BX,DX           ;[H,L] POINT AT STRING DESCRIPTOR
        MOV     BYTE PTR [BX],CL        ;STORE THE LENGTH
        INC     BX
        MOV     [BX],DX         ;STORE THE POINTER INTO THE DATA BLOCK
        POP     BX              ;GET BACK THE TEXT POINTER
        JMP     SHORT LOPFLD    ;CONTINUE SCANNING "AS" CLAUSES IF MORE

        SUBTTL  Random Non-I/O -- LSET/RSET/FIELD

;LSET/RSET stringvar = stringexp
;
; If stringvar points to an I/O buffer, use the string size to
;justify string. If stringvar is a literal, make new var with length
;of literal. If stringvar points to string space, use it. If the
;length of the variable is zero, return the null string. If a copy
;must be created, and stringexp is a temporary, use this space over
;unless length stringvar greater than stringexp.

RSET:   OR      AL,LOW 0        ;clear carry
        JMP     SHORT LSETA
LSET:   STC                     ;Set carry if lset
LSETA:
        PUSHF                   ;Save LSET/RSET flag
        CALL    PTRGET          ;Get pointer to stringvar
        CALL    FRCSTR          ;Must be a string variable
        PUSH    DX              ;Save pointer to descriptor
        CALL    FRMEQL          ;EAT "=" AND EVALUATE STRINGEXP
        POP     CX              ; [B,C] = ptr to descr.
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;Text ptr on bottom of stack
        PUSH    BX              ;LSET/RSET flag next
        PUSH    CX              ;Put descr. ptr back on
        CALL    FRESTR          ;Error if not string, free temp.
        MOV     CH,BYTE PTR [BX]        ;Get length of stringexp
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ; [H,L] = descr. of var,save othr
        MOV     AL,BYTE PTR [BX]        ;Get length of stringvar
        MOV     CL,AL           ;Save in [C]
        PUSH    CX              ;Save lengths of both
        PUSH    BX              ;Save descriptor pointer
        PUSHF                   ;PSW zero if was temp.
        INC     BX
        MOV     DX,[BX]         ;get point to stringvar text
        OR      AL,AL           ;stringvar null?
        JZ      SHORT RETCUR    ;Yes, don't change
        MOV     BX,TXTTAB
        CMP     BX,DX           ;Stringvar in disk buffer?
        JAE     SHORT OLDSTR    ;Yes, use it
        MOV     BX,VARTAB
        CMP     BX,DX           ;stringvar in program(literal)?
        JB      SHORT OLDSTR    ;No, in string space so use it
;Need to make new string for result since stringvar points to a literal.
;If stringexp was a temporary, it has been freed. If the length of
;stringexp is greater than or equal to the length of stringvar, GETSPA
;can be called and no garbage collection can occur so temp. can be reused.
;If stringvar is greater, must get a temp. to point to stringexp if it
;was a temp. , then call GETSPA which in this case can garbage collect.
        MOV     DL,CL
        MOV     DH,LOW 0        ;# BYTES TO ALLOCATE FOR RESULT
DSEG    SEGMENT PUBLIC 'DATASG'
        EXTRN   STREND:WORD
DSEG    ENDS
        MOV     BX,STREND
        ADD     BX,DX
        XCHG    BX,DX
        MOV     BX,FRETOP
        CMP     BX,DX           ;will GETSPA garbage collect?
        JB      SHORT MAKDSC    ;Yes, better have stringexp temp.
        POPF                    ;get rid of temp indicator
MADESC: MOV     AL,CL           ;Get length of stringvar
        CALL    GETSPA          ;Get space for result
        POP     BX              ;Get stringvar descr.
        POP     CX              ;Get lengths off stack
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;Get what we wanted, stringexp descr.
        PUSH    DX
        PUSH    CX
        CALL    FRESTR          ;Free temp if any
        POP     CX
        POP     DX
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI
        PUSH    CX              ;Restore stack to previous state
        PUSH    BX
        INC     BX
        PUSHF
        MOV     [BX],DX         ;set pointer to stringvar copy
OLDSTR: POPF
        POP     BX              ;Get stringvar descr.
        INC     BX
        MOV     DX,[BX]         ;Get pointer to text area
        POP     CX              ;Get lengths off stack
        POP     BX              ;Get pointer to stringexp descr.
        INC     BX              ;point to address part
        MOV     BX,[BX]         ;Get ptr to stringexp text
        MOV     AL,CL           ;Get length of field(stringvar)
        CMP     AL,CH
        JAE     SHORT FILDOK    ;Jump if field large enough for result
        MOV     CH,AL           ;Save # of bytes to copy
FILDOK: SUB     AL,CH
        MOV     CL,AL           ;[C] = # blanks to fill
        POPF                    ;Get LSET/RSET flag
        JNAE    SHORT ??L023
        CALL    BLKFIL          ;Fill leading if RSET
??L023:
        INC     CH              ;In case zero
COPLOP: DEC     CH              ;Decr. # to copy
        JZ      SHORT LRSTDN    ;Done when all copied
        MOV     AL,BYTE PTR [BX]        ;Get byte from stringexp
        MOV     DI,DX
        STOSB                   ;Copy to stringvar
        INC     BX
        INC     DX
        JMP     SHORT COPLOP

RETCUR: POP     CX
        POP     CX
        POP     CX
        POP     CX
        POP     CX              ;Get garb off stack
LRSTDN: JNB     SHORT ??L024
        CALL    BLKFIL          ;Fill trailing if LSET
??L024:
        POP     BX              ;Restore text pointer
        RET     

BLKFIL: MOV     AL,LOW " "      ;Fill with spaces
        INC     CL              ;In case zero
BLKFL1: DEC     CL              ;Decr. # to fill
        JNZ     SHORT $+3
        RET                     ;Return when done
        MOV     DI,DX
        STOSB                   ;Store space
        INC     DX
        JMP     SHORT BLKFL1

; If stringexp was a temporary, create a new temporary to point to
; stringexp since old one was freed. This must be done since GETSPA
; will be called and garbage collection might occur. If stringexp is
; not a temporary, return.

MAKDSC: POPF                    ;Get temp flag
        POP     BX
        POP     CX
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;Dig down to stringexp descr.
        XCHG    BX,DX           ;FRETMS wants [D,E]
        JNZ     SHORT MAKDS1    ;Not a temp, don't reallocate
        PUSH    CX
        MOV     AL,CH
        CALL    STRINI          ;Make a temp point to stringexp
        CALL    PUTNEW          ;Get a temp to point to it
        POP     CX
MAKDS1: POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI
        PUSH    CX
        PUSH    BX              ;Restore stack to previous state
        JMP     MADESC

        SUBTTL  Program I/O -- Fixed Length INPUT

; Format:
;               stringvar = INPUT$(#bytes[,[#] file#])
;If no file # is given, characters will be read from the user's
; terminal. No echoing will be done and no editing will be allowed
; (i.e. rubout,@,_,^U are just input as characters).

        PUBLIC  FIXINP
FIXINP: CALL    CHRGTR
        CALL    SYNCHR
        DB      OFFSET "$"      ;STRING FUNCTION
        CALL    SYNCHR
        DB      OFFSET "("
        PUSH    BX              ;Preserve PTRFIL across INPUT$ so
        MOV     BX,PTRFIL       ;cases like PRINT #2,INPUT$(3,#1)
        MOV     DX,0            ;will work properly.
        MOV     PTRFIL,DX       ;(Clear PTRFIL in case no file number
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;is specified.)
        CALL    GETBYT          ;Get # of bytes to read
        PUSH    DX              ;Save # of bytes to read
        MOV     AL,BYTE PTR [BX]
        CMP     AL,LOW 54O      ;Read from disk file?
        JNZ     SHORT REDTTY    ;No, from user's terminal
        CALL    CHRGTR
        CALL    FILSCN          ;Set up file #
        EXTRN   DERFNO:NEAR
        JNZ     SHORT ??L025
        JMP     DERFNO          ;error if File Not Open
??L025:
        CMP     AL,LOW OFFSET MD_SQO    ;SEQUENTIAL OUTPUT FILE?
        JNZ     SHORT ??L026
        JMP     DERBFM
??L026:                         ;THEN BAD FILE MODE
        CALL    FILSET          ;SET UP PTRFIL
        XOR     AL,AL           ;SET ZERO FOR FLAG
REDTTY: LAHF                    ; PUSH PSW
        XCHG    AL,AH
        PUSH    AX
        XCHG    AL,AH           ;NON ZERO SET IF TERMINAL I/O
        CALL    SYNCHR
        DB      OFFSET ")"      ;Must have paren
        POP     AX              ; POP PSW
        XCHG    AL,AH
        SAHF                    ;Get flag off stack
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;Save text ptr, [L]=# to read
        LAHF                    ; PUSH PSW
        XCHG    AL,AH
        PUSH    AX
        XCHG    AL,AH           ;Save flag
        MOV     AL,BL
        OR      AL,AL           ;Read no characters?
        JNZ     SHORT ??L027
        JMP     FCERR           ;Yes, error
??L027:
        PUSH    BX              ;Save #
        CALL    STRINI          ;Get space for string
        XCHG    BX,DX
        POP     CX              ;[C] = # to read
FIXLOP: POP     AX              ; POP PSW
        XCHG    AL,AH
        SAHF
        LAHF                    ; PUSH PSW
        XCHG    AL,AH
        PUSH    AX
        XCHG    AL,AH           ;NON-ZERO set if should read from TTY
        JZ      SHORT DSKCHR    ;Read from disk file
        CALL    INCHRI          ;Read a char from terminal
CHARCW:
;Note : will check flag on interrupt ^c
PUTCHR: MOV     BYTE PTR [BX],AL        ;Put char into string
        INC     BX
        DEC     CL              ;Read enough yet?
        JNZ     SHORT FIXLOP    ;No, read more
DSEG    SEGMENT PUBLIC 'DATASG'
        EXTRN   SAVKEY:WORD     ;Second half of two-byte key
DSEG    ENDS
        XOR     AL,AL
        MOV     BYTE PTR SAVKEY,AL      ;Clear saved second byte
        POP     AX              ; POP PSW
        XCHG    AL,AH
        SAHF                    ;Get flag off stack
        POP     CX              ;B:=text pointer.
        POP     BX              ;Restore PTRFIL.
        MOV     PTRFIL,BX
        PUSH    CX              ;Restack the text pointer.
        JMP     PUTNEW          ;Return string as result

DSKCHR: CALL    INDSKC          ;Get char from file
        JAE     SHORT ??L028
        JMP     DERRPE          ;If carry, read past EOF
??L028:
        JMP     SHORT PUTCHR    ;Put char in string
CSEG    ENDS
        END
                                                        
                                                          