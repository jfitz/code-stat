;Start of modifications

FALSE EQU 0  ;Logical FALSE for conditionals   JFS
TRUE EQU NOT FALSE ;Logical TRUE for conditionals  JFS

Z100 EQU TRUE ;Set TRUE for Z-100 or    JFS
IBMPC EQU NOT Z100 ;FALSE for IBM-PC compatibles

IF Z100
;Z-100 MTR-100 ROM Definitions JFS

ESC EQU 1BH     ;ASCII escape character JFS
MTRSEG SEGMENT AT OFEO1H ;MTR-100 ROM Segment JFS
 ORG 19H ;0ffset to JMP instruction JFS
MTRSCRT LABEL FAR ;Smart Terminal Emulator JFS
MTRSEG ENDS 3End of Segment JES
 ENDIF

.xlist
INCLUDE PARMS.ASM
INCLUDE MACLIB.ASM
INCLUDE DEFASCII.ASM
INCLUDE DEFMS.ASM
INCLUDE DEFDEV.ASM
.list

* MD IO - Major I/O routine
;‘This is the I/O routine

MD_IO PROC NEAR
 PUSH DS
 PUSH DX

 PUSH ES    ; Save registers JFS
 PUSH BX
 MOV BX,OFFSET CS:MDRD ; Memory Disk Read Logo JFS
 TEST CS:BYTE PTR TFLAG,OFFH ; Memory Disk Read? JFS
 JZ MD_IO1 ; Jump if so JFS
 MOV BX,OFFSET CS:MDWR ; Memory Disk Write Logo JFS
MD_IO1: CALL MD_LOGO ; Go output string JFS
 POP BX     ; Restore registers JFS
 POP ES

;End of transfer

MD_1O9:
 PUSH BX ; Save register JFS
 MOV BX,OFFSET CS:MDOFF ; Turn off Memory Disk Logo JFS
 CALL MD_LOGO ; Go output string JFS

 POP BX     ; Restore register JFS
 POP DX
 POP DS
 JMP MD_SUCC
MD_IO ENDP

;Turn the Memory Disk Logo On or Off JFS

 IF Z100
MD_LOGO PROC NEAR

LOOP: MOV AL,CS:[BX] ; Get next character JFS
 CMP AL,'$' ; End of string? JFS
 JZ DONE ; Jump if so   JES
 PUSH BX ; Save register JFS
 CLD    ; Clear direction flag JFS
 CALL MTRSCRT ; Output character JFS
 POP BX     ; Restore register JFS
 INC BX     ; Point to next character JFS
 JMP SHORT LOOP ; Continue looping JFS

DONE: RET   ; Return to caller JFS
MD_LOGO ENDP
 ENDIF

 IF IBMPC
MD_LOGO PROC NEAR

 MOV BX,CS:[BX] ; Get characters into BX JFS
 MOV WORD PTR CS:MDPC,BX ; Save them for later JFS
 MOV BH,O ; BH=video page # JFS
 MOV AH,3 ; Get cursor position JFS
 INT 10H ; DH=row, DL=col JFS
 MOV CS:CURSOR,DX ; Save cursor position JFS

 MOV BH,O ; BH=video page # JFS
 MOV DX,256*24+78 ; DH=row, DL=col JFS
 MOV AH,2 ; Set cursor position JFS
 INT 10H

 MOV AL,CS:MDPC ; lst character to write JFS
 MOV BX,70H ; BL=attr (reverse video) JFS
 CMP AL,' ' ; Space character? JFS
 JNZ MD_L1 ; Jump if not JES
 MOV BX,07H ; BL=attr (normal video) JFS
MD_L1: MOV CX,1 ; CX=# of characters JFS
 MOV AH,9 ; Write char and attribute JFS
 INT 10H

 MOV BH,O ; BH=video page # JFS
 MOV DX,256*24+79 ; DH=row, DL=col JFS
 MOV AH,2 ; Set cursor position JFS
 INT 10H

 MOV AL,CS:MDPC+1 ; 2nd character to write JFS
 MOV BX,70H ; BL=attr (reverse video) JFS
 CMP. AL,' ' ; Space character? JFS
 JNZ MD_L2 ; Jump if not JFS
 MOV BX,O7H ; BL=attr (normal video) JFS
 MD _L2: MOV CX,1 ; CX=# of characters JFS
 MOV AH,9 ; Write char and attribute JFS
 INT 10H

 MOV BH,O ; BH=video page # JFS
 MOV DX,CS:CURSOR ; Restore cursor position JFS
 MOV AH,2 ; Set cursor position JFS
 INT 10H

 RET ; Return to caller JFS
MD_LOGO ENDP
 ENDIF

* DATA areas

MDISK_START DW O ; Start segment of MDISK
PTRSAV DD 0 ; Request packet address
SAVESS DW ? ; Old stack save
SAVESP DW ?
TFLAG DB 0 ; 0 = read, 1 = write
SECCNT DW ?

 DW 128 DUP (?)
MYSTACK LABEL NEAR
;Memory Disk Logo Strings JES

 IF Z100
MDRD DB ESC,'x5' ;cursor off JFS
 DB ESC,'j'     ;save cursor position JFS
 DB ESC,'x1'    ;enable line 25 JFS
 DB ESC,'Y8n'   ;cursor to row 25 col JFS
 DB ESC,'p'     ;enter reverse video JFS
 DB 'RD'        ;memory disk read logo JFS
 DB ESC,'q'     ;exit reverse video JFS
 DB ESC,'k'     ;restore cursor position JFS
 DB ESC,'y5'    ;cursor on JES
 DB '$'     ;end of string JFS

MDWR DB ESC,'x5' ;cursor off JFS
 DB ESC,'j'     ;save cursor position JFS
 DB ESC,'x1'    ;enable line 25 JFS
 DB ESC,'Y8n'   ;cursor to row 25 col 79 JES
 DB ESC,'p'     ;enter reverse video JFS
 DB 'WR'        ;memory disk write logo JFS
 DB ESC,'q'     ;exit reverse video JFS
 DB ESC,'k'     ;restore cursor position JFS
 DB ESC,'y5'    ;cursor on JFS
 DB '$'         ;end of string JFS

MDOFF DB ESC,'x5' ;cursor off JES
 DB ESC,'j'     ;save cursor position JFS
 DB ESC,'x1'    ;enable line 25 JFS
 DB ESC,'Y8n'   ;cursor to row 25 col 79 JES
 DB '  '        ;erase memory disk logo JFS
 DB ESC,'k'     ;restore cursor position JFS
 DB ESC,'y5'    ;cursor on JFS
 DB '$'         ;end of string JFS
 ENDIF

 IF IBMPC
CURSOR DW 0     ;saved cursor row and column JFS
MDPG DB   '     ;saved memory disk logo chars JFS
MDRD DB 'RD'    ;memory disk read logo string JFS
MDWR DB 'WR'    ;memory disk write logo string JFS
MDOFF DB '  '   ;erase memory disk logo string JFS
 ENDIF

End of resident code/data

MD_END LABEL NEAR

