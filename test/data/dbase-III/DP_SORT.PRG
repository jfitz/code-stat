********************************************************************************
*
* System : DPSORT
*
*
* System Description : Sort/Select Criteria
*
*
* Module : DPSORT.PRG
*
*
* Module Description : Main Program
*
*
* Written by : A. Michael Hearn
*
*
* Copyright (c) 1987 by University Research, Inc
*
********************************************************************************
* database: DPSORT
SET STATUS OFF
SET TALK OFF
SET ECHO OFF
SET BELL OFF
SET HEADING OFF
SET SAFETY OFF
SET DEVICE TO SCREEN
SET PROCEDURE TO DPSORT.PRC
SELECT I
IF .NOT. FILE("DPSORT.NDX")
DO IND
ENDIF
USE DPSORT INDEX DPSORT
SET FILTER TO
DO PUB
STORE .T. TO MH_Loop
CALL DPMMSORT
CLEAR GETS
DO WHILE MH_Loop
STORE " " TO MH_Function
@ 24,1 SAY SPACE(78)
@ 2,3 SAY DTOC(DATE())
@ 2,69 SAY Time()
@ 23,47 SAY "Choice:"
@ 23,55 GET MH_Function
READ
DO CASE
CASE UPPER(MH_Function)="A"
DO ADD
CALL DPMMSORT
LOOP
CASE UPPER(MH_Function)="U"
DO UPD
CALL DPMMSORT
LOOP
CASE UPPER(MH_Function)="I"
DO IND
LOOP
CASE UPPER(MH_Function)="H"
DO HLP WITH 1
CALL DPMMSORT
LOOP
CASE UPPER(MH_Function)="P"
@24,1 SAY SPACE(78)
@24,1 SAY "Shall I delete all marked records?"
STORE "N" TO MH_Ans
@24,35 GET MH_Ans
READ
IF UPPER(MH_Ans) = "Y"
PACK
ENDIF
RELEASE MH_Ans
LOOP
CASE UPPER(MH_Function)="Q"
RELEASE MH_Loop,MH_Function
DO REL
CLOSE DATABASES
QUIT
CASE UPPER(MH_Function)="D"
RELEASE MH_Loop,MH_Function
DO REL
CLOSE DATABASES
RETURN
CASE UPPER(mh_function)="R"
STORE " " TO MH_Name
@24,1 SAY SPACE(78)
@24,1 SAY "Report Name:"
@24,14 GET MH_Name
READ
IF MH_Name <> " "
SELE A
CREATE REPORT &MH_Name
SELE I
ENDIF
CALL dpmmsort
LOOP
CASE UPPER(mh_function)="L"
STORE " " TO MH_Name
@24,1 SAY SPACE(78)
@24,1 SAY "Label Name:"
@24,14 GET MH_Name
READ
IF MH_Name <> " "
SELE A
CREATE LABEL &MH_Name
SELE I
ENDIF
CALL dpmmsort
LOOP
ENDCASE
ENDDO
RETURN
LAYOUT 000
r B
<?Aú\---#----+----+----------------+--------------------+---------@-/---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---- B
<?Aú---------\----+----------------+--------------------+------@-/------------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---- B

