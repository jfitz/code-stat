********************************************************************************
*
* System : LIBRARY
*
*
* System Description :
*
*
* Module : LIBRARY.PRG
*
*
* Module Description : Main Program
*
*
* Written by : James Canby Landerkin
*
*
* Copyright (c) 1988, 1989 by Questar Associates, Inc.
*
********************************************************************************
* database: LIBRARY
SET STATUS OFF
SET TALK OFF
SET ECHO OFF
SET BELL OFF
SET HEADING OFF
SET SAFETY OFF
SET DEVICE TO SCREEN
SET PROCEDURE TO LIBRARY.PRC
LOAD LIBRARY
LOAD DPMM
LOAD DPOUT
LOAD DPSORT
LOAD DPMMSORT
LOAD OPENING
SELECT F
CALL OPENING
DO WAT
SELECT A
IF .NOT. FILE("LIBRARY.NDX")
DO IND
ENDIF
USE LIBRARY INDEX LIBRARY
SET FILTER TO
DO PUB
STORE "A - Add new books or articles" TO MH_Menu1
STORE "U - Update, Edit, Scan, Find books or articles" TO MH_Menu2
STORE "R - Run Reports from the Library Catalog" TO MH_Menu3
STORE "I - Rebuild the Index to the Library Catalog" TO MH_Menu4
STORE "P - Remove Deleted records from the Library Catalog" TO MH_Menu5
STORE "S - Define Report/Sort/Select" TO MH_Menu6
STORE "Q - Quit to DOS" TO MH_Menu7
STORE "D - Exit to Dbase" TO MH_Menu8
STORE 8 TO MH_Options
STORE 1 TO MH_I
STORE "1" TO MH_Sub
STORE 0 TO MH_Col
STORE 52 TO MH_Col
CALL DPMM
CLEAR GETS
DO WHILE .T.
@ 5,14 SAY "A - Add new books or articles"
@ 6,14 SAY "U - Update, Edit, Scan, Find books or articles"
@ 7,14 SAY "R - Run Reports from the Library Catalog"
@ 8,14 SAY "I - Rebuild the Index to the Library Catalog"
@ 9,14 SAY "P - Remove Deleted records from the Library Catalog"
@ 10,14 SAY "S - Define Report/Sort/Select"
@ 11,14 SAY "Q - Quit to DOS"
@ 12,14 SAY "D - Exit to Dbase"
STORE " " TO MH_Function
@ 24,0 SAY SPACE(78)
@ 2,3 SAY DTOC(DATE())
@ 2,30 SAY "Library Catalog Main Menu"
@ 2,69 SAY Time()
STORE "1" TO MH_Sub
STORE 0 TO MH_Sel
STORE 1 TO MH_Mline
@5,14 GET MH_Menu1
CLEAR GETS
STORE " " TO MH_Function
DO WHILE MH_Function = " "
STORE 0 TO MH_Sel
DO WHILE MH_Sel=0
STORE INKEY() TO MH_Sel
ENDDO
IF MH_Sel = 24 .OR. MH_Sel=5 .OR. MH_Sel=32
@MH_MLine+4,14 SAY MH_Menu&MH_Sub
IF MH_Sel = 24 .OR. MH_Sel = 32
STORE MH_MLine+1 TO MH_MLine
ELSE
STORE MH_MLine-1 TO MH_MLine
ENDIF
IF MH_MLine>MH_Options
STORE 1 TO MH_MLine
ENDIF
IF MH_MLine<1
STORE MH_Options TO MH_MLine
ENDIF
DO St2 WITH MH_Mline, MH_Sub
@MH_MLine+4,14 GET MH_Menu&MH_Sub
CLEAR GETS
LOOP
ENDIF
IF At(UPPER(CHR(MH_Sel)),"AURIPSQD") # 0
@MH_MLine+4,14 SAY MH_Menu&MH_Sub
STORE At(UPPER(CHR(MH_Sel)),"AURIPSQD") TO MH_MLine
DO St2 WITH MH_Mline, MH_Sub
@MH_MLine+4,14 GET MH_Menu&MH_Sub
CLEAR GETS
STORE UPPER(CHR(MH_Sel)) TO MH_Function
ENDIF
IF MH_Sel = 13
DO St2 WITH MH_MLine, MH_Sub
STORE LEFT(MH_Menu&MH_Sub,1) TO MH_Function
ENDIF
ENDDO
DO CASE
CASE UPPER(MH_Function)="A"
DO ADD
CALL DPMM
LOOP
CASE UPPER(MH_Function)="U"
DO UPD
CALL DPMM
LOOP
CASE UPPER(MH_Function)="R"
DO RPT
CALL DPMM
LOOP
CASE UPPER(MH_Function)="I"
DO IND
LOOP
CASE UPPER(MH_Function)="P"
@24,0 SAY SPACE(78)
@24,0 SAY "Shall I delete all marked records?"
STORE "N" TO MH_Ans
@24,35 GET MH_Ans
READ
IF UPPER(MH_Ans) = "Y"
PACK
ENDIF
RELEASE MH_Ans
LOOP
CASE UPPER(MH_Function)="Q"
RELEASE MH_Function
DO REL
CLOSE DATABASES
QUIT
CASE UPPER(MH_Function)="S"
DO DPSORT
STORE "A - Add new books or articles" TO MH_Menu1
STORE "U - Update, Edit, Scan, Find books or articles" TO MH_Menu2
STORE "R - Run Reports from the Library Catalog" TO MH_Menu3
STORE "I - Rebuild the Index to the Library Catalog" TO MH_Menu4
STORE "P - Remove Deleted records from the Library Catalog" TO MH_Menu5
STORE "S - Define Report/Sort/Select" TO MH_Menu6
STORE "Q - Quit to DOS" TO MH_Menu7
STORE "D - Exit to Dbase" TO MH_Menu8
STORE 8 TO MH_Options
STORE 1 TO MH_I
STORE "1" TO MH_Sub
STORE 0 TO MH_Col
STORE 52 TO MH_Col
CALL DPMM
CLEAR GETS
SET PROCEDURE TO LIBRARY.PRC
USE LIBRARY INDEX LIBRARY
LOOP
CASE UPPER(MH_Function)="D"
RELEASE MH_Function
DO REL
CLOSE DATABASES
CLEAR
RETURN
ENDCASE
ENDDO
RETURN
LAYOUT 000
r B
<?Aú\---#----+----+----------------+--------------------+---------@-/---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---- B
<?Aú---------\----+----------------+--------------------+------@-/------------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---- B

