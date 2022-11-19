; [ This translation created 10-Feb-83 by Version 4.3 ]

        .RADIX  8               ; To be safe

CSEG    SEGMENT PUBLIC 'CODESG' 
        ASSUME  CS:CSEG

INCLUDE OEM.H

        TITLE   BISTRS  BASIC Interpreter String  routines/WHG/PGA etc.

        .RADIX  10

TSHIBA=0
PC8A=0
ZENITH=0
ALPCPM=0
CAN8=0
TRSHHC=0
NECPPC=0

DSEG    SEGMENT PUBLIC 'DATASG'         ; Data Segment
        ASSUME DS:DSEG
        EXTRN   ARYTAB:WORD,DSCTMP:WORD,FRETOP:WORD,MEMSIZ:WORD
        EXTRN   STREND:WORD,TEMPPT:WORD,TEMPST:WORD,VALTYP:WORD,VARTAB:WORD
        EXTRN   TEMP8:WORD,ARYTA2:WORD
        EXTRN   TEMP9:WORD,PRMPRV:WORD
        EXTRN   FACLO:WORD
DSEG    ENDS                    ; End of data segment externals

                                ; Code Segment ( terminated by END at bottom of file )

        EXTRN   MOVRM:NEAR
        EXTRN   BLTUC:NEAR,CONINT:NEAR,CRFIN:NEAR,ERRLS:NEAR,ERROR:NEAR
        EXTRN   ERRSO:NEAR,ERRST:NEAR
        EXTRN   EVAL:NEAR,FCERR:NEAR,FOUT:NEAR,FRMEQL:NEAR,FRMEVL:NEAR
        EXTRN   FRMPRN:NEAR
        EXTRN   GETBYT:NEAR,OUTDO:NEAR,POPHRT:NEAR,PTRGET:NEAR,SIGNS:NEAR
        EXTRN   SNERR:NEAR
        EXTRN   TSTOP:NEAR,SNGFLT:NEAR,GIVDBL:NEAR,FIN:NEAR,CHKSTR:NEAR

        EXTRN   IADAHL:NEAR
        EXTRN   FINDBL:NEAR,GETBCD:NEAR,VMOVE:NEAR
        EXTRN   GETYPR:NEAR
        EXTRN   CHRGTR:NEAR,DCOMPR:NEAR,SYNCHR:NEAR

        PUBLIC  STRPRT,STROUI,LEN,FRESTR,STRCMP,VAL,STRLTI
        PUBLIC  FRETMS,FRETMP,RIGHT$,LEFT$,GARBA2,STR$
        PUBLIC  FRE,STRLIT,STRCPY,CAT,STRLT3,MID$
        PUBLIC  STRINI,STRIN1,STRAD1,PUTDEI,INCSTR

        PUBLIC  LHSMID

;
; THE FOLLOWING ROUTINE COMPARES TWO STRINGS
; ONE WITH DESC IN [D,E] OTHER WITH DESC. IN [FACLO, FACLO+1]
; A=0 IF STRINGS EQUAL
; A=377 IF B,C,D,E .GT. FACLO
; A=1 IF B,C,D,E .LT. FACLO
;
STRCMP: CALL    FRESTR          ;FREE UP THE FAC STRING, AND GET THE
                                ;POINTER TO THE FAC DESCRIPTOR IN [H,L]
        MOV     AL,BYTE PTR [BX]        ;SAVE THE LENGTH OF THE FAC STRING IN [A]
        INC     BX
        MOV     CL,BYTE PTR [BX]        ;SAVE THE POINTER AT THE FAC STRING
                                ;DATA IN [B,C]
        INC     BX
        MOV     CH,BYTE PTR [BX]
        POP     DX              ;GET THE STACK STRING POINTER
        PUSH    CX              ;SAVE THE POINTER AT THE FAC STRING DATA
        PUSH    AX              ;SAVE THE FAC STRING LENGTH
        CALL    FRETMP          ;FREE UP THE STACK STRING AND RETURN
                                ;THE POINTER TO THE STACK STRING DESCRIPTOR
                                ;IN [H,L]
        POP     AX              ;GET BACK LENGTH OF STRING
        MOV     DH,AL           ;[D]=LENGTH OF FAC STRING
        MOV     DL,BYTE PTR [BX]        ;[E]=LENGTH OF STACK STRING
        INC     BX
        MOV     CL,BYTE PTR [BX]        ;[B,C]=POINTER AT STACK STRING
        INC     BX
        MOV     CH,BYTE PTR [BX]
        POP     BX              ;GET BACK 2ND CHARACTER POINTER
CSLOOP: MOV     AL,DL           ;BOTH STRINGS ENDED
        OR      AL,DH           ;TEST BY OR'ING THE LENGTHS TOGETHER
        JNZ     SHORT $+3
        RET                     ;IF SO, RETURN WITH A ZERO
        MOV     AL,DH           ;GET FACLO STRING LENGTH
        SUB     AL,LOW 1        ;SET CARRY AND MAKE [A]=255 IF [D]=0
        JNB     SHORT $+3
        RET                     ;RETURN IF THAT STRING ENDED
        XOR     AL,AL           ;MUST NOT HAVE BEEN ZERO, TEST CASE
        CMP     AL,DL           ;OF B,C,D,E STRING HAVING ENDED FIRST
        INC     AL              ;RETURN WITH A=1
        JNAE    SHORT $+3
        RET                     ;TEST THE CONDITION
;HERE WHEN NEITHER STRING ENDED
        DEC     DH              ;DECREMENT BOTH CHARACTER COUNTS
        DEC     DL
        MOV     SI,CX
        MOV     AL,[SI]         ;GET CHARACTER FROM B,C,D,E STRING
        INC     CX
        CMP     AL,BYTE PTR [BX]        ;COMPARE WITH FACLO STRING
        LAHF
        INC     BX              ;BUMP POINTERS (INX DOESNT CLOBBER CC'S)
        SAHF
        JZ      SHORT CSLOOP    ;IF BOTH THE SAME, MUST BE MORE TO STRINGS
        CMC                     ;HERE WHEN STRINGS DIFFER
        JMP     SIGNS           ;SET [A] ACCORDING TO CARRY
        SUBTTL  STRING FUNCTIONS
        EXTRN   FOUTO:NEAR,FOUTH:NEAR
        PUBLIC  STRO$,STRH$
; THE STRO$ FUNCTION TAKES A NUMBER AND GIVES
; A STRING WITH THE CHARACTERS THE NUMBER WOULD GIVE IF
; OUTPUT IN OCTAL
;
STRO$:  CALL    FOUTO           ;PUT OCTAL NUMBER IN FBUFFR
        JMP     SHORT STR$1     ;JUMP INTO STR$ CODE

; STRH$ SAME AS STRO$ EXCEPT USES HEX INSTEAD OF OCTAL
STRH$:  CALL    FOUTH           ;PUT HEX NUMBER IN FBUFFR
        JMP     SHORT STR$1     ;JUMP INTO STR$ CODE
;
; THE STR$ FUNCTION TAKES A NUMBER AND GIVES
; A STRING WITH THE CHARACTERS THE OUTPUT OF THE NUMBER
; WOULD HAVE GIVEN
;
STR$:
                                ;IS A NUMERIC
        CALL    FOUT            ;DO ITS OUTPUT
STR$1:  CALL    STRLIT          ;SCAN IT AND TURN IT INTO A STRING
        CALL    FREFAC          ;FREE UP THE TEMP
        MOV     CX,OFFSET FINBCK
        PUSH    CX              ;SET UP ANSWER IN NEW TEMP
;
; STRCPY CREATES A COPY OF THE STRING
; WHOSE DESCRIPTOR IS POINTED TO BY [H,L].
; ON RETURN [D,E] POINTS TO DSCTMP
; WHICH HAS THE STRING INFO (LENGTH,WHERE COPIED TO)
;
STRCPY: MOV     AL,BYTE PTR [BX]        ;GET LENGTH
        INC     BX              ;MOVE UP TO THE POINTER
        PUSH    BX              ;GET POINTER TO POINTER OF ARG
        CALL    GETSPA          ;GET THE SPACE
        POP     BX              ;FIND OUT WHERE STRING TO COPY
        MOV     CL,BYTE PTR [BX]
        INC     BX
        MOV     CH,BYTE PTR [BX]
        CALL    STRAD2          ;SETUP DSCTMP
        PUSH    BX              ;SAVE POINTER TO DSCTMP
        MOV     BL,AL           ;GET CHARACTER COUNT INTO [L]
        CALL    MOVSTR          ;MOVE THE CHARS IN
        POP     DX              ;RESTORE POINTER TO DSCTMP
        RET                     ;RETURN

STRIN1: MOV     AL,LOW 1        ;MAKE ONE CHAR STRING (CHR$, INKEY$)
STRINI: CALL    GETSPA          ;GET SOME STRING SPACE ([A] CHARS)
STRAD2: MOV     BX,OFFSET DSCTMP        ;GET DESC. TEMP
STRAD1: PUSH    BX              ;SAVE DESC. POINTER
        MOV     BYTE PTR [BX],AL        ;SAVE CHARACTER COUNT
PUTDEI: INC     BX              ;STORE [D,E]=POINTER TO FREE SPACE
        MOV     [BX],DX
        POP     BX              ;AND RESTORE [H,L] AS THE DESCRIPTOR POINTER
        RET     
;
; STRLT2 TAKES THE STRING LITERAL WHOSE FIRST CHARACTER
; IS POINTED BY [H,L]+1 AND BUILDS A DESCRIPTOR FOR IT.
; THE DESCRIPTOR IS INITIALLY BUILT IN DSCTMP, BUT PUTNEW
; TRANSFERS IT INTO A TEMPORARY AND LEAVES A POINTER
; AT THE TEMPORARY IN FACLO. THE CHARACTERS OTHER THAN
; ZERO THAT TERMINATE THE STRING SHOULD BE SET UP IN [B]
; AND [D]. IT THE TERMINATOR IS A QUOTE, THE QUOTE IS SKIPPED
; OVER. LEADING QUOTES SHOULD BE SKIPPED BEFORE CALL. ON RETURN
; THE CHARACTER AFTER THE STRING LITERAL IS POINTED TO
; BY [H,L] AND IS IN [A], BUT THE CONDITION CODES ARE
; NOT SET UP.
;
        PUBLIC  STRLT2
STRLIT: DEC     BX
STRLTI: MOV     CH,LOW 34       ;ASSUME STR ENDS ON QUOTE
STRLT3: MOV     DH,CH
STRLT2: PUSH    BX              ;SAVE POINTER TO START OF LITERAL
        MOV     CL,LOW 255      ;INITIALIZE CHARACTER COUNT
STRGET: INC     BX
        MOV     AL,BYTE PTR [BX]        ;GET CHAR
        INC     CL              ;BUMP CHARACTER COUNT
        OR      AL,AL           ;IF 0, (END OF LINE) DONE
        JZ      SHORT STRFIN    ;TEST
        CMP     AL,DH
        JZ      SHORT STRFIN
        CMP     AL,CH           ;CLOSING QUOTE
        JNZ     SHORT STRGET    ;NO, GO BACK FOR MORE
STRFIN: CMP     AL,LOW 34       ;IF QUOTE TERMINATES THE STRING
        JNZ     SHORT ??L000
        CALL    CHRGTR          ;SKIP OVER THE QUOTE
??L000:
        PUSH    BX              ;SAVE POINTER AT END OF STRING
        MOV     AL,CH           ;WERE WE SCANNING AN UNQUOTED STRING?
        CMP     AL,LOW 44
        JNZ     SHORT NTTRLS    ;IF NOT, DON'T SUPPRESS TRAILING SPACES
        INC     CL              ;FIX [C] WHICH IS THE CHARACTER COUNT
LPTRLS: DEC     CL              ;DECREMENT UNTIL WE FIND A NON-SPACE CHARACTER
        JZ      SHORT NTTRLS    ;DON'T GO PAST START (ALL SPACES)
        DEC     BX              ;LOOK AT PREVIOUS CHARACTER
        MOV     AL,BYTE PTR [BX]
        CMP     AL,LOW " "
        JZ      SHORT LPTRLS    ;IF SO CONTINUE LOOKING
NTTRLS: POP     BX
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI
        INC     BX
        XCHG    BX,DX           ;GET POINTER TO TEMP
        MOV     AL,CL           ;GET CHARACTER COUNT IN A
        CALL    STRAD2          ;SAVE STR INFO
;
; SOME STRING FUNCTION IS RETURNING A RESULT IN DSCTMP
; WE WANT TO SETUP A TEMP DESCRIPTOR WITH DCSTMP IN IT
; PUT A POINTER TO THE DESCRIPTOR IN FACLO AND FLAG THE 
; RESULT AS TYPE STRING
;
        PUBLIC  PUTNEW
PUTNEW: MOV     DX,OFFSET DSCTMP        ;[D,E] POINT AT RESULT DESCRIPTOR
        PUBLIC  PUTTMP
        DB      260O    ; SKIP  ;SKIP THE NEXT BYTE ("MVI AL,")
PUTTMP: PUSH    DX              ;SAVE A POINTER TO THE START OF THE STRING
        MOV     BX,TEMPPT       ;[H,L]=POINTER TO FIRST FREE TEMP
        MOV     FACLO,BX        ;POINTER AT WHERE RESULT DESCRIPTOR WILL BE
        MOV     AL,LOW 3
        MOV     BYTE PTR VALTYP,AL      ;FLAG THIS AS A STRING
        CALL    VMOVE           ;AND MOVE THE VALUE INTO A TEMPORARY
        MOV     DX,OFFSET DSCTMP+3      ;IF THE CALL IS TO PUTTMP, [D,E]
                                ;WILL NOT EQUAL DSCTMP +3
        CMP     BX,DX           ;DSCTMP IS JUST BEYOND THE TEMPS
                                ;AND IF TEMPPT POINTS AT IT THERE
                                ;ARE NO FREE TEMPS
        MOV     TEMPPT,BX       ;SAVE NEW TEMPORARY POINTER
        POP     BX              ;GET THE TEXT POINTER
        MOV     AL,BYTE PTR [BX]        ;GET CURRENT CHARACTER INTO [A]
        JZ      SHORT $+3
        RET
        MOV     DX,OFFSET ERRST ;"STRING TEMPORARY" ERROR
        JMP     ERROR           ;GO TELL HIM
;
; PRINT THE STRING POINTED TO BY [H,L] WHICH ENDS WITH A ZERO
; IF THE STRING IS BELOW DSCTMP IT WILL BE COPIED INTO STRING SPACE
;
STROUI: INC     BX              ;POINT AT NEXT CHARACTER
        PUBLIC  DSOUT
DSOUT:                          ;PRINT FROM THE DATA SEGMENT
        CALL    STRLIT          ;GET A STRING LITERAL
;
; PRINT THE STRING WHOSE DESCRIPTOR IS POINTED TO BY FACLO.
;
STRPRT: CALL    FREFAC          ;RETURN TEMP POINTER BY FACLO
        CALL    GETBCD          ;[D]=LENGTH [B,C]=POINTER AT DATA
        INC     DH              ;INCREMENT AND DECREMENT EARLY
                                ;TO CHECK FOR NULL STRING
STRPR2: DEC     DH              ;DECREMENT THE LENGTH
        JNZ     SHORT $+3
        RET                     ;ALL DONE
        MOV     SI,CX
        MOV     AL,[SI]         ;GET CHARACTER TO PRINT
        CALL    OUTDO
        CMP     AL,LOW 13
        JNZ     SHORT ??L001
        CALL    CRFIN
??L001:
        INC     CX              ;POINT TO THE NEXT CHARACTER
        JMP     SHORT STRPR2    ;AND PRINT IT...
        PAGE    
        SUBTTL  STRING GARBAGE COLLECTION - GETSPA, GARBAG
;
; GETSPA - GET SPACE FOR CHARACTER STRING
; MAY FORCE GARBAGE COLLECTION.
;
; # OF CHARS (BYTES) IN [A]
; RETURNS WITH POINTER IN [D,E] OTHERWISE IF CANT GET SPACE
; BLOWS OFF TO "OUT OF STRING SPACE" TYPE ERROR.
;
        PUBLIC  GETSPA
GETSPA: OR      AL,AL           ;MUST BE NON ZERO. SIGNAL NO GARBAG YET
        JMP     SHORT TRYGI3
TRYGI2: POP     AX
        SAHF                    ;IN CASE COLLECTED WHAT WAS LENGTH?
TRYGI3:
        LAHF
        PUSH    AX              ;SAVE IT BACK
        MOV     BX,STREND
        XCHG    BX,DX           ;IN [D,E]
        MOV     BX,FRETOP       ;GET TOP OF FREE SPACE IN [H,L]
        NOT     AL              ;-# OF CHARS
        MOV     CL,AL           ;IN [B,C]
        MOV     CH,LOW 255
        ADD     BX,CX           ;SUBTRACT FROM TOP OF FREE
        INC     BX
        CMP     BX,DX           ;COMPARE THE TWO
        JB      SHORT GARBAG    ;NOT ENOUGH ROOM FOR STRING, OFFAL TIME
        MOV     FRETOP,BX       ;SAVE NEW BOTTOM OF MEMORY
        INC     BX              ;MOVE BACK TO POINT TO STRING
        XCHG    BX,DX           ;RETURN WITH POINTER IN [D,E]
        POP     AX
        SAHF
        RET     
        PUBLIC  PPSWRT
PPSWRT: POP     AX              ; POP PSW
        XCHG    AL,AH
        SAHF                    ;GET CHARACTER COUNT
        RET                     ;RETURN FROM GETSPA

GARBAG:
        POP     AX
        SAHF                    ;HAVE WE COLLECTED BEFORE?
        MOV     DX,OFFSET ERRSO ;GET READY FOR OUT OF STRING SPACE ERROR
        JNZ     SHORT ??L002
        JMP     ERROR           ;GO TELL USER HE LOST
??L002:
        CMP     AL,AL           ;SET ZERO FLAG TO SAY WEVE GARBAGED
        LAHF
        PUSH    AX              ;SAVE FLAG BACK ON STACK
        MOV     CX,OFFSET TRYGI2        ;PLACE FOR GARBAG TO RETURN TO.
        PUSH    CX              ;SAVE ON STACK
GARBA2:
        MOV     BX,MEMSIZ       ;START FROM TOP DOWN
FNDVAR: MOV     FRETOP,BX       ;LIKE SO
        MOV     BX,0            ;GET DOUBLE ZERO
        PUSH    BX              ;SAY DIDNT SEE VARS THIS PASS
        MOV     BX,STREND       ;FORCE DVARS TO IGNORE STRINGS
                                ;IN THE PROGRAM TEXT (LITERALS, DATA)
        PUSH    BX              ;FORCE FIND HIGH ADDRESS
        MOV     BX,OFFSET TEMPST        ;GET START OF STRING TEMPS
TVAR:   MOV     DX,TEMPPT       ;SEE IF DONE
        CMP     BX,DX           ;TEST
        MOV     CX,OFFSET TVAR  ;FORCE JUMP TO TVAR
        JZ      SHORT ??L003
        JMP     DVAR2           ;DO TEMP VAR GARBAGE COLLECT
??L003:

        MOV     BX,OFFSET PRMPRV        ;SETUP ITERATION FOR PARAMETER BLOCKS
        MOV     TEMP9,BX
        MOV     BX,ARYTAB       ;GET STOPPING POINT IN [H,L]
        MOV     ARYTA2,BX       ;STORE IN STOP LOCATION
        MOV     BX,VARTAB       ;GET STARTING POINT IN [H,L]
SVAR:   MOV     DX,ARYTA2       ;GET STOPPING LOCATION
        CMP     BX,DX           ;SEE IF AT END OF SIMPS
        JZ      SHORT ARYVAR
        MOV     AL,BYTE PTR [BX]        ;GET VALTYP
        INC     BX              ;BUMP POINTER TWICE
        INC     BX              ;
        INC     BX              ;POINT AT THE VALUE
        PUSH    AX              ;SAVE VALTYP
        CALL    IADAHL          ;AND SKIP OVER EXTRA CHARACTERS AND COUNT
        POP     AX
        CMP     AL,LOW 3        ;SEE IF ITS A STRING
        JNZ     SHORT SKPVAR    ;IF NOT, JUST SKIP AROUND IT
        CALL    DVARS           ;COLLECT IT
        XOR     AL,AL           ;AND DON'T SKIP ANYTHING MORE
SKPVAR: MOV     DL,AL
        MOV     DH,LOW 0        ;[D,E]=AMOUNT TO SKIP
        ADD     BX,DX
        JMP     SHORT SVAR      ;GET NEXT ONE
ARYVAR: MOV     BX,TEMP9        ;GET LINK IN PARAMETER BLOCK CHAIN
        MOV     DX,[BX]         ;GO BACK ONE LEVEL
        OR      DX,DX           ;WAS THAT THE END?
        MOV     BX,ARYTAB       ;SETUP TO START ARRAYS
        JZ      SHORT ARYVA4    ;OTHERWISE GARBAGE COLLECT ARRAYS
        XCHG    BX,DX
        MOV     TEMP9,BX        ;SETUP NEXT LINK IN CHAIN FOR ITERATION
        INC     BX              ;SKIP CHAIN POINTER
        INC     BX
        MOV     DX,[BX]         ;PICK UP THE LENGTH
        INC     BX
        INC     BX
        XCHG    BX,DX           ;SET [D,E]= ACTUAL END ADDRESS BY
        ADD     BX,DX           ;ADDING BASE TO LENGTH
        MOV     ARYTA2,BX       ;SET UP STOP LOCATION
        XCHG    BX,DX
        JMP     SHORT SVAR

ARYVA2: POP     CX              ;GET RID OF STACK GARBAGE
ARYVA4: MOV     DX,STREND       ;GET END OF ARRAYS
        CMP     BX,DX           ;SEE IF DONE WITH ARRAYS
        JNZ     SHORT ??L004
        JMP     GRBPAS          ;YES, SEE IF DONE COLLECTING
??L004:
        MOV     AL,BYTE PTR [BX]        ;GET THE VALUE TYPE INTO [A]
        INC     BX
        PUSH    AX              ;SAVE THE VALTYP
        INC     BX              ;SKIP THE NAME CHARACTERS
        INC     BX
        CALL    IADAHL          ;SKIP THE EXTRA CHARACTERS
        MOV     CL,BYTE PTR [BX]        ;PICK UP THE LENGTH
        INC     BX
        MOV     CH,BYTE PTR [BX]
        INC     BX
        POP     AX              ;RESTORE THE VALTYP
        PUSH    BX              ;SAVE POINTER TO DIMS
        ADD     BX,CX           ;ADD TO CURRENT POINTER POSITION
        CMP     AL,LOW 3        ;SEE IF ITS A STRING
        JNZ     SHORT ARYVA2    ;IF NOT JUST SKIP IT
        MOV     TEMP8,BX        ;SAVE END OF ARRAY
        POP     BX              ;GET BACK CURRENT POSITION
        MOV     CL,BYTE PTR [BX]        ;PICK UP NUMBER OF DIMS
        MOV     CH,LOW 0        ;MAKE DOUBLE WITH HIGH ZERO
        ADD     BX,CX           ;GO PAST DIMS
        ADD     BX,CX           ;BY ADDING ON TWICE #DIMS (2 BYTE GUYS)
        INC     BX              ;ONE MORE TO ACCOUNT FOR #DIMS.
ARYSTR: XCHG    BX,DX           ;SAVE CURRENT POSIT IN [D,E]
        MOV     BX,TEMP8        ;GET END OF ARRAY
        XCHG    BX,DX           ;FIX [H,L] BACK TO CURRENT
        CMP     BX,DX           ;SEE IF AT END OF ARRAY
        JZ      SHORT ARYVA4    ;END OF ARRAY, TRY NEXT ARRAY
        MOV     CX,OFFSET ARYSTR        ;ADDR OF WHERE TO RETURN TO
DVAR2:  PUSH    CX              ;GOES ON STACK
DVAR:
DVARS:  XOR     AL,AL
        OR      AL,BYTE PTR [BX]        ;SEE IF ITS THE NULL STRING
        LAHF
        INC     BX
        SAHF
        MOV     DL,BYTE PTR [BX]
        LAHF
        INC     BX
        SAHF
        MOV     DH,BYTE PTR [BX]
        LAHF
        INC     BX              ;[D,E]=POINTER AT THE VALUE
        SAHF
        JNZ     SHORT $+3
        RET                     ;NULL STRING, RETURN
        MOV     CX,BX           ;MOVE [H,L] TO [B,C]
        MOV     BX,FRETOP       ;GET POINTER TO TOP OF STRING FREE SPACE
        CMP     BX,DX           ;IS THIS STRINGS POINTER .LT. FRETOP
        MOV     BX,CX           ;MOVE [B,C] BACK TO [H,L]
        JNB     SHORT $+3
        RET                     ;IF NOT, NO NEED TO MESS WITH IT FURTHUR
        POP     BX              ;GET RETURN ADDRESS OFF STACK
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;GET MAX SEEN SO FAR & SAVE RETURN ADDRESS
        CMP     BX,DX           ;LETS SEE
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;SAVE MAX SEEN & GET RETURN ADDRESS OFF STACK
        PUSH    BX              ;SAVE RETURN ADDRESS BACK
        MOV     BX,CX           ;MOVE [B,C] BACK TO [H,L]
        JNAE    SHORT $+3
        RET                     ;IF NOT, LETS LOOK AT NEXT VAR
        POP     CX              ;GET RETURN ADDR OFF STACK
        POP     AX              ;POP OFF MAX SEEN
        POP     AX              ;AND VARIABLE POINTER
        PUSH    BX              ;SAVE NEW VARIABLE POINTER
        PUSH    DX              ;AND NEW MAX POINTER
        PUSH    CX              ;SAVE RETURN ADDRESS BACK
        RET                     ;AND RETURN
;
; HERE WHEN MADE ONE COMPLETE PASS THRU STRING VARS
;
GRBPAS: POP     DX              ;POP OFF MAX POINTER
        POP     BX              ;AND GET VARIABLE POINTER
        OR      BX,BX           ;SEE IF ZERO POINTER
        JNZ     SHORT $+3
        RET                     ;IF END OF COLLECTION,
                                ;THEN MAYBE RETURN TO GETSPA
        DEC     BX              ;CURRENTLY JUST PAST THE DESCRIPTOR
        MOV     CH,BYTE PTR [BX]        ;[B]=HIGH BYTE OF DATA POINTER
        DEC     BX
        MOV     CL,BYTE PTR [BX]        ;[B,C]=POINTER AT STRING DATA
        PUSH    BX              ;SAVE THIS LOCATION SO THE POINTER
                                ;CAN BE UPDATED AFTER THE STRING IS
                                ;MOVED
        DEC     BX
        MOV     BL,BYTE PTR [BX]        ;[L]=STRING LENGTH
        MOV     BH,LOW 0        ;[H,L] GET CHARACTER COUNT
        ADD     BX,CX           ;[H,L]=POINTER BEYOND STRING
        MOV     DH,CH
        MOV     DL,CL           ;[D,E]=ORIGINAL POINTER
        DEC     BX              ;DON'T MOVE ONE BEYOND STRING
        MOV     CX,BX           ;GET TOP OF STRING IN [B,C]
        MOV     BX,FRETOP       ;GET TOP OF FREE SPACE
        CALL    BLTUC           ;MOVE STRING
        POP     BX              ;GET BACK POINTER TO DESC.
        MOV     BYTE PTR [BX],CL        ;SAVE FIXED ADDR
        INC     BX              ;MOVE POINTER
        MOV     BYTE PTR [BX],CH        ;HIGH PART
        MOV     BX,CX           ;[H,L]=NEW POINTER
        DEC     BX              ;FIX UP FRETOP
        JMP     FNDVAR          ;AND TRY TO FIND HIGH AGAIN

        PAGE    
        SUBTTL  STRING CONCATENATION
;
; THE FOLLOWING ROUTINE CONCATENATES TWO STRINGS
; THE FACLO CONTAINS THE FIRST ONE AT THIS POINT,
; [H,L] POINTS BEYOND THE + SIGN AFTER IT
;
CAT:    PUSH    CX              ;PUT OLD PRECEDENCE BACK ON
        PUSH    BX              ;SAVE TEXT POINTER
        MOV     BX,FACLO        ;GET POINTER TO STRING DESC.
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;SAVE ON STACK & GET TEXT POINTER BACK
        CALL    EVAL            ;EVALUATE REST OF FORMULA
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;SAVE TEXT POINTER, GET BACK DESC.
        CALL    CHKSTR
        MOV     AL,BYTE PTR [BX]
        PUSH    BX              ;SAVE DESC. POINTER.
        MOV     BX,FACLO        ;GET POINTER TO 2ND DESC.
        PUSH    BX              ;SAVE IT
        ADD     AL,BYTE PTR [BX]        ;ADD TWO LENGTHS TOGETHER
        MOV     DX,OFFSET ERRLS ;SEE IF RESULT .LT. 256
        JAE     SHORT ??L005
        JMP     ERROR           ;ERROR "LONG STRING"
??L005:
        CALL    STRINI          ;GET INITIAL STRING
        POP     DX              ;GET 2ND DESC.
        CALL    FRETMP
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;SAVE POINTER TO IT
        CALL    FRETM2          ;FREE UP 1ST TEMP
        PUSH    BX              ;SAVE DESC. POINTER (FIRST)
        INCSTR=2
        INCSTR=1
        MOV     BX,DSCTMP+INCSTR        ;GET POINTER TO FIRST
        XCHG    BX,DX           ;IN [D,E]
        CALL    MOVINS          ;MOVE IN THE FIRST STRING
        CALL    MOVINS          ;AND THE SECOND
        MOV     BX,OFFSET TSTOP ;CAT REENTERS FORMULA EVALUATION AT TSTOP
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI
        PUSH    BX              ;TEXT POINTER OFF FIRST
        JMP     PUTNEW          ;THEN RETURN ADDRESS OF TSTOP


MOVINS: POP     BX              ;GET RETURN ADDR
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;PUT BACK, BUT GET DESC.
        MOV     AL,BYTE PTR [BX]        ;[A]=STRING LENGTH
        INC     BX
        MOV     CL,BYTE PTR [BX]        ;[B,C]=POINTER AT STRING DATA
        INC     BX
        MOV     CH,BYTE PTR [BX]
        MOV     BL,AL           ;[L]=STRING LENGTH
MOVSTR: INC     BL
MOVLP:  DEC     BL              ;SET CC'S
        JNZ     SHORT $+3
        RET                     ;0, NO BYTE TO MOVE
        MOV     SI,CX
        MOV     AL,[SI]         ;GET CHAR
        MOV     DI,DX
        STOSB                   ;SAVE IT
        INC     CX              ;MOVE POINTERS
        INC     DX
        JMP     SHORT MOVLP     ;KEEP DOING IT
        PAGE    
        SUBTTL  FREE UP STRING TEMPORARY - FRESTR, FREFAC, FRETMP, FRETMS
;
; FRETMP IS PASSED A POINTER TO A STRING DESCRIPTOR IN [D,E]
; THIS VALUE IS RETURNED IN [H,L]. ALL THE OTHER REGISTERS ARE MODIFIED.
; A CHECK TO IS MADE TO SEE IF THE STRING DESCRIPTOR [D,E] POINTS
; TO IS THE LAST TEMPORARY DESCRIPTOR ALLOCATED BY PUTNEW.
; IF SO, THE TEMPORARY IS FREED UP BY THE UPDATING OF TEMPPT.
; IF A TEMPORARY IS FREED UP, A FURTHER CHECK IS MADE TO SEE IF THE
; STRING DATA THAT THAT STRING TEMPORARY POINTED TO IS THE
; THE LOWEST PART OF STRING SPACE IN USE.
; IF SO, FRETMP IS UPDATED TO REFLECT THE FACT THAT THAT SPACE IS NO
; LONGER IN USE.
;
        PUBLIC  FREFAC
FRESTR: CALL    CHKSTR          ;MAKE SURE ITS A STRING
FREFAC: MOV     BX,FACLO
        PUBLIC  FRETM2
FRETM2: XCHG    BX,DX           ;FREE UP THE TEMP IN THE FACLO
FRETMP: CALL    FRETMS          ;FREE UP THE TEMPORARY
        XCHG    BX,DX           ;PUT THE STRING POINTER INTO [H,L]
        JZ      SHORT $+3
        RET
        PUSH    DX              ;SAVE [D,E] TO RETURN IN [H,L]
        MOV     DH,CH           ;[D,E]=POINTER AT STRING
        MOV     DL,CL
        DEC     DX              ;SUBTRACT ONE
        MOV     CL,BYTE PTR [BX]        ;[C]=LENGTH OF THE STRING FREED UP
        MOV     BX,FRETOP       ;SEE IF ITS THE FIRST 
                                ;ONE IN STRING SPACE
        CMP     BX,DX
        JNZ     SHORT NOTLST    ;NO SO DON'T ADD
        XOR     AL,AL           ;MUST EXPLICITLY ZERO A
        MOV     CH,AL           ;MAKE [B]=0
        ADD     BX,CX           ;ADD
        MOV     FRETOP,BX       ;AND UPDATE FRETOP
NOTLST: POP     BX              ;GET POINTER AT CURRENT DESCRIPTOR
        RET     
FRETMS:
        MOV     BX,TEMPPT       ;GET TEMP POINTER
        DEC     BX              ;LOOK AT WHAT IS IN THE LAST TEMP
        MOV     CH,BYTE PTR [BX]        ;[B,C]=POINTER AT STRING
        DEC     BX              ;DECREMENT TEMPPT BY STRSIZ
        MOV     CL,BYTE PTR [BX]
        DEC     BX
        CMP     BX,DX           ;SEE IF [D,E] POINT AT THE LAST 
        JZ      SHORT $+3
        RET                     ;RETURN NOW IF NOW FREEING DONE
        MOV     TEMPPT,BX       ;UPDATE THE TEMP POINTER SINCE
                                ;ITS BEEN DECREMENTED BY 4
        RET     
        PAGE    
        SUBTTL STRING FUNCTIONS - LEN, ASC, CHR$
;
; THE FUNCTION LEN($) RETURNS THE LENGTH OF THE
; STRING PASSED AS AN ARGUMENT
;
LEN:    MOV     CX,OFFSET SNGFLT        ;CALL SNGFLT WHEN DONE
        PUSH    CX              ;LIKE SO
LEN1:   CALL    FRESTR          ;FREE UP TEMP POINTED TO BY FACLO
        XOR     AL,AL           ;FORCE NUMERIC FLAG
        MOV     DH,AL           ;SET HIGH OF [D,E] TO ZERO FOR VAL
        MOV     AL,BYTE PTR [BX]
        OR      AL,AL           ;SET CONDITION CODES ON LENGTH
        RET                     ;RETURN

; THE FOLLOWING IS THE ASC($) FUNCTION. IT RETURNS AN INTEGER
; WHICH IS THE DECIMAL ASCII EQUIVALENT
;
        PUBLIC  ASC,ASC2
ASC:
        MOV     CX,OFFSET SNGFLT        ;WHERE TO GO WHEN DONE
        PUSH    CX              ;SAVE RETURN ADDR ON STACK
ASC2:   CALL    LEN1            ;SET UP ORIGINAL STR
        JNZ     SHORT ??L006
        JMP     FCERR           ;NULL STR, BAD ARG.
??L006:
        INC     BX              ;BUMP POINTER
        MOV     DX,[BX]         ;[D,E]=POINTER AT STRING DATA
        MOV     SI,DX
        MOV     AL,[SI]         ;[A]=FIRST CHARACTER
        RET     
;
; CHR$(#) CREATES A STRING WHICH CONTAINS AS ITS ONLY
; CHARACTER THE ASCII EQUIVALENT OF THE INTEGER ARG (#)
; WHICH MUST BE .LE. 255.
;
        PUBLIC  CHR$
CHR$:   CALL    STRIN1          ;GET STRING IN DSCTMP
        CALL    CONINT          ;GET INTEGER IN RANGE
        INCSTR=2
        INCSTR=1
        PUBLIC  SETSTR,FINBCK
SETSTR: MOV     BX,DSCTMP+INCSTR        ;GET ADDR OF STR
        MOV     BYTE PTR [BX],DL        ;SAVE ASCII BYTE
FINBCK: POP     CX              ;RETURN TO HIGHER LEVEL &
                                ;SKIP THE CHKNUM CALL.
        JMP     PUTNEW          ;GO CALL PUTNEW

        PUBLIC  STRNG$
STRNG$: CALL    CHRGTR          ;GET NEXT CHAR FOLLOWING "STRING$"
        CALL    SYNCHR
        DB      OFFSET "("      ;MAKE SURE LEFT PAREN
        CALL    GETBYT          ;EVALUATE FIRST ARG (LENGTH)
        PUSH    DX              ;SAVE IT
        CALL    SYNCHR
        DB      OFFSET 54O      ;COMMA
        CALL    FRMEVL          ;GET FORMULA ARG 2
        CALL    SYNCHR
        DB      OFFSET ")"      ;EXPECT RIGHT PAREN
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;SAVE TEXT POINTER ON STACK, GET REP FACTOR
        PUSH    BX              ;SAVE BACK REP FACTOR
        CALL    GETYPR          ;GET TYPE OF ARG
        JZ      SHORT STRSTR    ;WAS A STRING
        CALL    CONINT          ;GET ASCII VALUE OF CHAR
        JMP     SHORT CALSPA    ;NOW CALL SPACE CODE
STRSTR: CALL    ASC2            ;GET VALUE OF CHAR IN [A]
CALSPA: POP     DX              ;GET REP FACTOR IN [E]
        CALL    SPACE2          ;INTO SPACE CODE, PUT DUMMY ENTRY
                                ;ON STACK POPPED OFF BY FINBCK

        PUBLIC  SPACE$
SPACE$: CALL    CONINT          ;GET NUMBER OF CHARS IN [E]
        MOV     AL,LOW 32       ;GET SPACE CHAR
SPACE2: PUSH    AX              ;SAVE CHAR
        MOV     AL,DL           ;GET NUMBER OF CHARS IN [A]
        CALL    STRINI          ;GET A STRING THAT LONG
        MOV     CH,AL           ;COUNT OF CHARS BACK IN [B]
        POP     AX              ;GET BACK CHAR TO PUT IN STRING
        INC     CH              ;TEST FOR NULL STRING
        DEC     CH
        JZ      SHORT FINBCK    ;YES, ALL DONE
        MOV     BX,DSCTMP+INCSTR        ;GET DESC. POINTER
SPLP$:  MOV     BYTE PTR [BX],AL        ;SAVE CHAR
        INC     BX              ;BUMP PTR
                                ;DECR COUNT
        DEC     CH
        JNZ     SHORT SPLP$     ;KEEP STORING CHAR
        JMP     SHORT FINBCK    ;PUT TEMP DESC WHEN DONE
        PAGE    
        SUBTTL STRING FUNCTIONS - LEFT$, RIGHT$, MID$
;
; THE FOLLOWING IS THE LEFT$($,#) FUNCTION.
; IT TAKES THE LEFTMOST # CHARS OF THE STR.
; IF # IS .GT. THAN THE LEN OF THE STR, IT RETURNS THE WHOLE STR.
;
LEFT$:  CALL    PREAM           ;TEST THE PARAMETERS
        XOR     AL,AL           ;LEFT NEVER CHANGES STRING POINTER
LEFT3:  POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;SAVE TEXT POINTER
        MOV     CL,AL           ;OFFSET NOW IN [C]
        DB      260O    ; SKIP  ;SKIP THE NEXT BYTE WITH "MVI A,"
;
; THIS IS PRINT USINGS ENTRY POINT INTO LEFT$
;
        PUBLIC  LEFTUS
LEFTUS: PUSH    BX              ;THIS IS A DUMMY PUSH TO OFFSET
                                ;THE EXTRA POP IN PUTNEW
LEFT2:  PUSH    BX              ;SAVE DESC. FOR  FRETMP
        MOV     AL,BYTE PTR [BX]        ;GET STRING LENGTH
        CMP     AL,CH           ;ENTIRE STRING WANTED?
        JB      SHORT ALLSTR    ;IF #CHARS ASKED FOR.GE.LENGTH,YES
        MOV     AL,CH           ;GET TRUNCATED LENGTH OF STRING
        DB      272O    ; SKIP  ;SKIP OVER MVI USING "LXI D,"
ALLSTR: MOV     CL,LOW 0        ;MAKE OFFSET ZERO
        PUSH    CX              ;SAVE OFFSET ON STACK
        CALL    GETSPA          ;GET SPACE FOR NEW STRING
        POP     CX              ;GET BACK OFFSET
        POP     BX              ;GET BACK DESC POINTER.
        PUSH    BX              ;BUT KEEP ON STACK
        INC     BX              ;MOVE TO STRING POINTER FIELD
        MOV     CH,BYTE PTR [BX]        ;GET POINTER LOW
        INC     BX              ;
        MOV     BH,BYTE PTR [BX]        ;POINTER HIGH
        MOV     BL,CH           ;GET LOW IN  L
        MOV     CH,LOW 0        ;GET READY TO ADD OFFSET TO POINTER
        ADD     BX,CX           ;ADD  IT
        MOV     CX,BX           ;GET OFFSET POINTER IN [B,C]
        CALL    STRAD2          ;SAVE INFO IN DSCTMP
        MOV     BL,AL           ;GET#  OF CHARS TO  MOVE IN L
        CALL    MOVSTR          ;MOVE THEM IN
        POP     DX              ;GET BACK DESC. POINTER
        CALL    FRETMP          ;FREE IT UP.
        JMP     PUTNEW          ;PUT TEMP IN TEMP LIST

RIGHT$: CALL    PREAM           ;CHECK ARG
        POP     DX              ;GET DESC. POINTER
        PUSH    DX              ;SAVE BACK FOR LEFT
        MOV     SI,DX
        MOV     AL,[SI]         ;GET PRESENT LEN OF STR
        SUB     AL,CH           ;SUBTRACT 2ND PARM
        JMP     SHORT LEFT3     ;CONTINUE WITH LEFT CODE
;
; MID ($,#) RETURNS STR WITH CHARS FROM # POSITION
; ONWARD. IF # IS GT LEN($) THEN RETURN NULL STRING.
; MID ($,#,#) RETURNS STR WITH CHARS FROM # POSITION
; FOR #2 CHARS. IF #2 GOES PAST END OF STRING, RETURN
; AS MUCH AS POSSIBLE.
;
MID$:   XCHG    BX,DX           ;PUT THE TEXT POINTER IN [H,L]
        MOV     AL,BYTE PTR [BX]        ;GET THE FIRST CHARACTER
        CALL    PREAM2          ;GET OFFSET OFF STACK AND MAKE
        INC     CH
        DEC     CH              ;SEE IF EQUAL TO ZERO
        JNZ     SHORT ??L007
        JMP     FCERR           ;IT MUST NOT BE 0
??L007:
        PUSH    CX              ;PUT OFFSET ON TO THE STACK
        CALL    MIDRST          ;DUPLICATE OF CODE CONDITIONED OUT
        POP     AX              ; POP PSW
        XCHG    AL,AH
        SAHF                    ;GET OFFSET BACK IN A
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;SAVE TEXT POINTER, GET DESC.
        MOV     CX,OFFSET LEFT2 ;WHERE TO RETURN TO.
        PUSH    CX              ;GOES ON STACK
        DEC     AL              ;SUB ONE FROM OFFSET
        CMP     AL,BYTE PTR [BX]        ;POINTER PAST END OF STR?
        MOV     CH,LOW 0        ;ASSUME NULL LENGTH STR
        JNAE    SHORT $+3
        RET                     ;YES, JUST USE NULL STR
        MOV     CL,AL           ;SAVE OFFSET OF CHARACTER POINTER
        MOV     AL,BYTE PTR [BX]        ;GET PRESENT LEN OF STR
        SUB     AL,CL           ;SUBTRACT INDEX (2ND ARG)
        CMP     AL,DL           ;IS IT TRUNCATION
        MOV     CH,AL           ;GET CALCED LENGTH IN B
        JNB     SHORT $+3
        RET                     ;IF NOT USE PARTIAL STR
        MOV     CH,DL           ;USE TRUNCATED LENGTH
        RET                     ;RETURN TO LEFT2
;
; THE VAL FUNCTION TAKES A STRING AND TURN IT INTO
; A NUMBER BY INTERPRETING THE ASCII DIGITS. ETC..
; EXCEPT FOR THE PROBLEM THAT A TERMINATOR MUST BE SUPPLIED
; BY REPLACING THE CHARACTER BEYOND THE STRING, VAL
; IS MERELY A CALL TO FLOATING INPUT (FIN).
;
VAL:    CALL    LEN1            ;DO SETUP, SET RESULT=REAL
        JNZ     SHORT ??L008
        JMP     SNGFLT
??L008:                         ;MAKE SURE TYPE SET UP OK IN EXTENDED
        MOV     DL,AL           ;GET LENGTH OF STR
        INC     BX              ;TO HANDLE THE FACT THE IF
        MOV     BX,[BX]         ;TWO STRINGS "1" AND "2" ARE STORED
                                ;NEXT TO EACH OTHER
        PUSH    BX              ;AND FIN IS CALLED POINTING TO
        ADD     BX,DX           ;THE FIRST TWELVE WILL BE RETURNED
        MOV     CH,BYTE PTR [BX]        ;THE IDEA IS TO STORE 0 IN THE
        MOV     BYTE PTR [BX],DH        ;STRING BEYOND THE ONE VAL
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;IS BEING CALLED ON
        PUSH    CX              ;THE FIRST CHARACTER OF THE NEXT STRING
        DEC     BX              ;***CALL CHRGET TO MAKE SURE
        CALL    CHRGTR          ;VAL(" -3")=-3
        CALL    FINDBL          ;IN EXTENDED, GET ALL THE PRECISION WE CAN
        POP     CX              ;GET THE MODIFIED CHARACTER OF THE NEXT
                                ;STRING INTO [B]
        POP     BX              ;GET THE POINTER TO THE MODIFIED CHARACTER
        MOV     BYTE PTR [BX],CH        ;RESTORE THE CHARACTER
                                ;IF STRING IS HIGHEST IN STRING SPACE
                                ;WE ARE MODIFYING [MEMSIZ] AND
                                ;THIS IS WHY [MEMSIZ] CAN'T BE USED TO STORE
                                ;STRING DATA BECAUSE WHAT IF THE
                                ;USER TOOK VAL OFF THAT HIGH STRING
        RET     
;USED BY RIGHT$ AND LEFT$ FOR PARAMETER CHECKING AND SETUP
PREAM:  XCHG    BX,DX           ;PUT THE TEXT POINTER IN [H,L]
        CALL    SYNCHR
        DB      OFFSET ")"      ;PARAM LIST SHOULD END
;USED BY MID$ FOR PARAMETER CHECKING AND SETUP
PREAM2: POP     CX              ;GET RETURN ADDR OFF STACK
        POP     DX              ;GET LENGTH OF ARG OFF STACK
        PUSH    CX              ;SAVE RETURN ADDR BACK ON
        MOV     CH,DL           ;SAVE INIT LENGTH
        RET     

        PAGE    
        SUBTTL STRING FUNCTIONS - INSTR

; THIS IS THE INSTR FUCNTION. IT TAKES ONE OF TWO
; FORMS: INSTR(I%,S1$,S2$) OR INSTR(S1$,S2$)
; IN THE FIRST FORM THE STRING S1$ IS SEARCHED FOR THE
; CHARACTER S2$ STARTING AT CHARACTER POSITION I%.
; THE SECOND FORM IS IDENTICAL, EXCEPT THAT THE SEARCH
; STARTS AT POSITION 1. INSTR RETURNS THE CHARACTER
; POSITION OF THE FIRST OCCURANCE OF S2$ IN S1$.
; IF S1$ IS NULL, 0 IS RETURNED. IF S2$ IS NULL, THEN
; I% IS RETURNED, UNLESS I% .GT. LEN(S1$) IN WHICH
; CASE 0 IS RETURNED.

        PUBLIC  INSTR
INSTR:  CALL    CHRGTR          ;EAT FIRST CHAR
        CALL    FRMPRN          ;EVALUATE FIRST ARG
        CALL    GETYPR          ;SET ZERO IF ARG A STRING.
        MOV     AL,LOW 1        ;IF SO, ASSUME, SEARCH STARTS AT FIRST CHAR
        PUSH    AX              ;SAVE OFFSET IN CASE STRING
        JZ      SHORT WUZSTR    ;WAS A STRING
        POP     AX              ;GET RID OF SAVED OFFSET
        CALL    CONINT          ;FORCE ARG1 (I%) TO BE INTEGER
        OR      AL,AL           ;DONT ALLOW ZERO OFFSET
        JNZ     SHORT ??L009
        JMP     FCERR           ;KILL HIM.
??L009:
        PUSH    AX              ;SAVE FOR LATER
        CALL    SYNCHR
        DB      OFFSET 44       ;EAT THE COMMA
        CALL    FRMEVL          ;EAT FIRST STRING ARG
        CALL    CHKSTR          ;BLOW UP IF NOT STRING
WUZSTR: CALL    SYNCHR
        DB      OFFSET 44       ;EAT COMMA AFTER ARG
        PUSH    BX              ;SAVE THE TEXT POINTER
        MOV     BX,FACLO        ;GET DESCRIPTOR POINTER
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;PUT ON STACK & GET BACK TEXT PNT.
        CALL    FRMEVL          ;GET LAST ARG
        CALL    SYNCHR
        DB      OFFSET ")"      ;EAT RIGHT PAREN
        PUSH    BX              ;SAVE TEXT POINTER
        CALL    FRESTR          ;FREE UP TEMP & CHECK STRING
        XCHG    BX,DX           ;SAVE 2ND DESC. POINTER IN [D,E]
        POP     CX              ;GET TEXT POINTER IN B
        POP     BX              ;DESC. POINTER FOR S1$
        POP     AX              ;OFFSET
        PUSH    CX              ;PUT TEXT POINTER ON BOTTOM
        MOV     CX,OFFSET POPHRT        ;PUT ADDRESS OF POP H, RET ON
        PUSH    CX              ;PUSH IT
        MOV     CX,OFFSET SNGFLT        ;NOW ADDRESS OF [A] RETURNER
        PUSH    CX              ;ONTO STACK
        PUSH    AX              ;SAVE OFFSET BACK
        PUSH    DX              ;SAVE DESC. OF S2
        CALL    FRETM2          ;FREE UP S1 DESC.
        POP     DX              ;RESTORE DESC. S2
        POP     AX              ;GET BACK OFFSET
        MOV     CH,AL           ;SAVE UNMODIFIED OFFSET
        DEC     AL              ;MAKE OFFSET OK
        MOV     CL,AL           ;SAVE IN C
        CMP     AL,BYTE PTR [BX]        ;IS IT BEYOND LENGTH OF S1?
        MOV     AL,LOW 0        ;IF SO, RETURN ZERO. (ERROR)
        JNAE    SHORT $+3
        RET
        MOV     SI,DX
        MOV     AL,[SI]         ;GET LENGTH OF S2$
        OR      AL,AL           ;NULL??
        MOV     AL,CH           ;GET OFFSET BACK
        JNZ     SHORT $+3
        RET                     ;ALL IF S2 NULL, RETURN OFFSET
        MOV     AL,BYTE PTR [BX]        ;GET LENGTH OF S1$
        INC     BX              ;BUMP POINTER
        MOV     CH,BYTE PTR [BX]        ;GET 1ST BYTE OF ADDRESS
        INC     BX              ;BUMP POINTER
        MOV     BH,BYTE PTR [BX]        ;GET 2ND BYTE
        MOV     BL,CH           ;GET 1ST BYTE SET UP
        MOV     CH,LOW 0        ;GET READY FOR DAD
        ADD     BX,CX           ;NOW INDEXING INTO STRING
        SUB     AL,CL           ;MAKE LENGTH OF STRING S1$ RIGHT
        MOV     CH,AL           ;SAVE LENGTH OF 1ST STRING IN [B]
        PUSH    CX              ;SAVE COUNTER, OFFSET
        PUSH    DX              ;PUT 2ND DESC (S2$) ON STACK
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;GET 2ND DESC. POINTER
        MOV     CL,BYTE PTR [BX]        ;SET UP LENGTH
        INC     BX              ;BUMP POINTER
        MOV     DX,[BX]         ;GET FIRST BYTE OF ADDRESS
        POP     BX              ;RESTORE POINTER FOR 1ST STRING

CHK1:   PUSH    BX              ;SAVE POSITION IN SEARCH STRING
        PUSH    DX              ;SAVE START OF SUBSTRING
        PUSH    CX              ;SAVE WHERE WE STARTED SEARCH
CHK:    MOV     SI,DX
        MOV     AL,[SI]         ;GET CHAR FROM SUBSTRING
        CMP     AL,BYTE PTR [BX]        ; = CHAR POINTER TO BY [H,L]
        JNZ     SHORT OHWELL    ;NO
        INC     DX              ;BUMP COMPARE POINTER
        DEC     CL              ;END OF SEARCH STRING?
        JZ      SHORT GOTSTR    ;WE FOUND IT!
        INC     BX              ;BUMP POINTER INTO STRING BEING SEARCHED
                                ;DECREMENT LENGTH OF SEARCH STRING
        DEC     CH
        JNZ     SHORT CHK       ;END OF STRING, YOU LOSE
RETZER: POP     DX              ;GET RID OF POINTERS
        POP     DX              ;GET RID OF GARB
        POP     CX              ;LIKE SO
RETZR1: POP     DX
        XOR     AL,AL           ;GO TO SNGFLT.
        RET                     ;RETURN

GOTSTR: POP     BX
        POP     DX              ;GET RID OF GARB
        POP     DX              ;GET RID OF EXCESS STACK
        POP     CX              ;GET COUNTER, OFFSET
        MOV     AL,CH           ;GET ORIGINAL SOURCE COUNTER
        SUB     AL,BH           ;SUBTRACT FINAL COUNTER
        ADD     AL,CL           ;ADD ORIGINAL OFFSET (N1%)
        INC     AL              ;MAKE OFFSET OF ZERO = POSIT 1
        RET                     ;DONE


OHWELL: POP     CX
        POP     DX              ;POINT TO START OF SUBSTRING
        POP     BX              ;GET BACK WHERE WE STARTED TO COMPARE
        INC     BX              ;AND POINT TO NEXT CHAR
                                ;DECR. # CHAR LEFT IN SOURCE STRING
        DEC     CH
        JNZ     SHORT CHK1      ;TRY SEARCHING SOME MORE
        JMP     SHORT RETZR1    ;END OF STRING, RETURN 0

        PAGE    
        SUBTTL STRING FUNCTIONS - LEFT HAND SIDE MID$
LHSMID: CALL    SYNCHR
        DB      OFFSET "("      ;MUST HAVE ( 
        CALL    PTRGET          ;GET A STRING VAR
        CALL    CHKSTR          ;MAKE SURE IT WAS A STRING
        PUSH    BX              ;SAVE TEXT POINTER
        PUSH    DX              ;SAVE DESC. POINTER
        XCHG    BX,DX           ;PUT DESC. POINTER IN [H,L]
        INC     BX              ;MOVE TO ADDRESS FIELD
        MOV     DX,[BX]         ;GET ADDRESS OF LHS IN [D,E]
        MOV     BX,STREND       ;SEE IF LHS STRING IS IN STRING SPACE
        CMP     BX,DX           ;BY COMPARING IT WITH STKTOP
        JB      SHORT NCPMID    ;IF ALREADY IN STRING SPACE
                                ;DONT COPY.
DSEG    SEGMENT PUBLIC 'DATASG'
        EXTRN   TXTTAB:WORD
DSEG    ENDS
        MOV     BX,TXTTAB
        CMP     BX,DX           ;Is this a fielded string?
        JAE     SHORT NCPMID    ;Yes, Don't copy!!
NCP1:   POP     BX              ;GET BACK DESC. POINTER
        PUSH    BX              ;SAVE BACK ON STACK
        CALL    STRCPY          ;COPY THE STRING LITERAL INTO STRING SPACE
        POP     BX              ;GET BACK DESC. POINTER
        PUSH    BX              ;BACK ON STACK AGAIN
        CALL    VMOVE           ;MOVE NEW DESC. INTO OLD SLOT.
NCPMID: POP     BX              ;GET DESC. POINTER
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;GET TEXT POINTER TO [H,L] DESC. TO STACK
        CALL    SYNCHR
        DB      OFFSET 54O      ;MUST HAVE COMMA
        CALL    GETBYT          ;GET ARG#2 (OFFSET INTO STRING)
        OR      AL,AL           ;MAKE SURE NOT ZERO
        JNZ     SHORT ??L010
        JMP     FCERR           ;BLOW HIM UP IF ZERO
??L010:
        PUSH    AX              ;SAVE ARG#2 ON STACK
        MOV     AL,BYTE PTR [BX]        ;RESTORE CURRENT CHAR
        CALL    MIDRST          ;USE MID$ CODE TO EVALUATE POSIBLE THIRD ARG.
        PUSH    DX              ;SAVE THIRD ARG ([E]) ON STACK
                                ;MUST HAVE = SIGN
        CALL    FRMEQL          ;EVALUATE RHS OF THING.
        PUSH    BX              ;SAVE TEXT POINTER.
        CALL    FRESTR          ;FREE UP TEMP RHS IF ANY.
        XCHG    BX,DX           ;PUT RHS DESC. POINTER IN [D,E]
        POP     BX              ;TEXT POINTER TO [H,L]
        POP     CX              ;ARG #3 TO C.
        POP     AX              ;ARG #2 TO A.
        MOV     CH,AL           ;AND [B]
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;GET LHS DESC. POINTER TO [H,L]
                                ;TEXT POINTER TO STACK
        PUSH    BX              ;SAVE TEXT POINTER
        MOV     BX,OFFSET POPHRT        ;GET ADDR TO RETURN TO
        POP     SI              ;XTHL
        XCHG    SI,BX
        PUSH    SI              ;SAVE ON STACK & GET BACK TXT PTR.
        MOV     AL,CL           ;GET ARG #3
        OR      AL,AL           ;SET CC'S
        JNZ     SHORT $+3
        RET                     ;IF ZERO, DO NOTHING
        MOV     AL,BYTE PTR [BX]        ;GET LENGTH OF LHS
        SUB     AL,CH           ;SEE HOW MANY CHARS IN EMAINDER OF STRING
        JAE     SHORT ??L011
        JMP     FCERR           ;CANT ASSIGN PAST LEN(LHS)!
??L011:
        INC     AL              ;MAKE PROPER COUNT
        CMP     AL,CL           ;SEE IF # OF CHARS IS .GT. THIRD ARG
        JB      SHORT BIGLEN    ;IF SO, DONT TRUNCATE
        MOV     AL,CL           ;TRUNCATE BY USING 3RD ARG.
BIGLEN: MOV     CL,CH           ;GET OFFSET OF STRING IN [C]
        DEC     CL              ;MAKE PROPER OFFSET
        MOV     CH,LOW 0        ;SET UP [B,C] FOR LATER DAD B.
        PUSH    DX              ;SAVE [D,E]
        INC     BX              ;POINTER TO ADDRESS FIELD.
        MOV     DL,BYTE PTR [BX]        ;GET LOW BYTE IN [E]
        INC     BX              ;BUMP POINTER
        MOV     BH,BYTE PTR [BX]        ;GET HIGH BYTE IN [H]
        MOV     BL,DL           ;NOW COPY LOW BYTE BACK TO [L]
        ADD     BX,CX           ;ADD OFFSET
        MOV     CH,AL           ;SET COUNT OF LHS IN [B]
        POP     DX              ;RESTORE [D,E]
        XCHG    BX,DX           ;MOVE RHS. DESC. POINTER TO [H,L]
        MOV     CL,BYTE PTR [BX]        ;GET LEN(RHS) IN [C]
        INC     BX              ;MOVE POINTER
        MOV     BX,[BX]         ;GET LOW BYTE OF ADDRESS IN [A]
        XCHG    BX,DX           ;ADDRESS OF RHS NOW IN [D,E]
        MOV     AL,CL           ;IS RHS NULL?
        OR      AL,AL           ;TEST
        JNZ     SHORT $+3
        RET                     ;THEN ALL DONE.
; NOW ALL SET UP FOR ASSIGNMENT.
; [H,L] = LHS POINTER
; [D,E] = RHS POINTER
; C = LEN(RHS)
; B = LEN(LHS)

MID$LP: MOV     SI,DX
        MOV     AL,[SI]         ;GET BYTE FROM RHS.
        MOV     BYTE PTR [BX],AL        ;STORE IN LHS
        INC     DX              ;BUMP RHS POINTER
        INC     BX              ;BUMP LHS POINTER.
        DEC     CL              ;BUMP DOWN COUNT OF RHS.
        JNZ     SHORT $+3
        RET                     ;IF ZERO, ALL DONE.
                                ;IF LHS ENDED, ALSO DONE.
        DEC     CH
        JNZ     SHORT MID$LP    ;IF NOT DONE, MORE COPYING.
        RET                     ;BACK TO NEWSTT

MIDRST: MOV     DL,LOW 255      ;IF TWO ARG GUY, TRUNCATE.
        CMP     AL,LOW ")"
        JZ      SHORT MID2      ;[E] SAYS USE ALL CHARS
                                ;IF ONE ARGUMENT THIS IS CORRECT
        CALL    SYNCHR
        DB      OFFSET 44       ;COMMA? MUST DELINEATE 3RD ARG.
        CALL    GETBYT          ;GET ARGUMENT  IN  [E]
MID2:   CALL    SYNCHR
        DB      OFFSET ")"      ;MUST BE FOLLOWED BY )
        RET                     ;ALL DONE.

        SUBTTL FRE  FUNCTION AND INTEGER TO FLOATING  ROUTINES
FRE:
        CALL    GETYPR
        JZ      SHORT ??L012
        JMP     CLCDIF
??L012:
        CALL    FREFAC          ;FREE UP ARGUMENT AND SETUP
                                ;TO GIVE FREE STRING SPACE
        CALL    GARBA2          ;DO GARBAGE COLLECTION
CLCDIF: MOV     DX,STREND
        MOV     BX,FRETOP       ;TOP OF FREE AREA
        JMP     GIVDBL

CSEG    ENDS
        END
                                                             