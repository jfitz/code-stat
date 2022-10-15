* Program..: INV_DSP.PRG
* Notes....: Display groups of items
* calls item display and item update
PRIVATE A_aa, TopLine, R_eno
DO WHILE .T.
SET HEAD OFF
Order = ""
Topline = 0
SELECT INVENTORY
SET ORDER TO 1
IF Rcnum = 0
If Again
SET COLOR TO &HighVideo
@ 21,15 say Title
SET COLOR TO &stdvideo
Again = .F.
ENDIF
Y_n = 'D'
@ 23,15 SAY 'Item (D)escription, (C)ategory, and (L)ocation '
SET COLOR TO &highvideo
@ 23,21 SAY 'D'
@ 23,36 SAY 'C'
@ 23,52 SAY 'L'
SET COLOR TO &stdvideo
@ 22,15 SAY "Enter Order Of Display? (D/C/L) " GET Y_n PICTURE "!"
READ
IF UPPER(Y_n) = "L" .OR. UPPER(Y_n) = "C"
IF UPPER(Y_n) = "L"
SET ORDER TO 2
Which = 'L'
Order='Location Order'
A_aa=SPACE(3)
@ 23,50 SAY " "
@ 23,15 SAY "Enter Location Code...............: " GET A_aa PICTURE "!!!"
ENDI && UPPER(Y_n) = "L"
IF UPPER(Y_n) = "C"
SET ORDER TO 3
Which = 'C'
Order='Category Order'
A_aa=SPACE(3)
@ 23,50 SAY " "
@ 23,15 SAY "Enter Category Code...............: " GET A_aa PICTURE "!!!"
ENDI && UPPER(Y_n) = "C"
ELSE
A_aa=SPACE(20)
@ 23,15 SAY "Enter part of the ITEM description: " GET A_aa
ENDIF && UPPER(Y_n) = "L" .OR. UPPER(Y_n) = "C"
READ
STORE TRIM(A_aa) TO A_aa
IF ""=a_aa
GOTO TOP
ELSE
FIND &a_aa
IF EOF()
GOTO TOP
ENDI && EOF()
ENDI
ELSE
IF Which = 'C'
SET ORDER TO 3
Order='Category Order'
ENDIF && Which = 'C'
IF Which = 'L'
SET ORDER TO 2
Order='Location Order'
ENDIF && Which = 'L'
GOTO RECNO()
ENDI && Rcnum = 0
SET PROCEDURE TO INV_BOT
CLEAR
@ 0,0 TO 4,79 DOUBLE
SET COLOR TO &highvideo
@ 1,(40-len(Title)/2) SAY Title
SET COLOR TO &stdvideo
DO Botline
@ 2,1 TO 2,78 DOUBLE
IF Which = 'C'
@ 3,2 SAY 'Record No. Category Description'
@ 4,0 SAY ""
LIST NEXT 17 SPACE(7),TYPE+" "+TRIM(Descriptin)
ELSE
@ 3,2 SAY 'Record No. Location Description'
@ 4,0 SAY ""
LIST NEXT 17 SPACE(7),Location+" "+TRIM(Descriptin)
ENDIF && Which = 'C'
IF EOF()
SET COLOR TO &highvideo
@ ROW(),22 SAY " End of Items "
SET COLOR TO &stdvideo
ENDI && EOF()
S_crpos=ROW()
Y_n='F'
DO WHILE .T.
@ 23,47 GET Y_n PICTURE "!"
READ
IF READKEY() = 262 .OR. READKEY() = 6 && PAGE Up
Y_n = 'B'
ENDIF && READKEY() = 262 .OR. READKEY() = 6 && P
IF READKEY() = 263 .OR. READKEY() = 7 && PAGE Down
Y_n = 'F'
ENDIF && READKEY() = 263 .OR. READKEY() = 7 && P
IF READKEY() = 12 && Esc
Y_n = 'Q'
ENDIF && READKEY() = 12 && Esc
DO CASE
CASE UPPER(Y_n) = 'Q'
RETURN
CASE UPPER(Y_n) = 'G'
EXIT
CASE UPPER(Y_n) = 'S'
@ 23,0 CLEAR
Rcnum = 0
Which = " "
DO Inv_dsp
RETURN
CASE UPPER(Y_n)='B'
SKIP -(S_crpos+10)
IF BOF()
Y_n='F'
ENDI && BOF()
ENDCASE
@ 5,0 CLEAR
DO Botline
@ 4,0 SAY ""
IF Which = 'C'
LIST NEXT 17 SPACE(7),TYPE+" "+TRIM(Descriptin)
ELSE
LIST NEXT 17 SPACE(7),Location+" "+TRIM(Descriptin)
ENDIF && Which = 'C'
IF EOF()
SET COLOR TO &highvideo
@ ROW(),22 SAY " End of Items "
SET COLOR TO &stdvideo
Y_n='B'
ENDI && EOF()
S_crpos=ROW()
ENDD && WHILE .T.
IF UPPER(Y_n)='G'
@ 24,0 CLEAR
R_eno= 000
@ 24,08 SAY "What is the Item's RECORD number? " GET R_eno PICTURE "999"
READ
IF R_eno>RECCOUNT() .OR. R_eno=0
WAIT 'Invalid record number, press any key to continue'
@ 24,0 clear
LOOP
ENDIF
GOTO R_eno
EXIT
ELSE
RETURN
ENDIF && UPPER(Y_n)='G'
ENDD && WHILE .T.
RETURN

