* Program..: INV_cata.PRG
* Date.....: 01/26/90
* Notes....: Add for category
SET CONFIRM ON
SET PROCEDURE TO INV_LCDE
Title = "C a t e g o r y A d d i t i o n"
Function =" F1 = Category Codes F3 = Quit F4 = Add Record"
SELECT CATEGORY
T_ype = SPACE(LEN(TYPE))
Desc = SPACE(LEN(Descriptin))
* display screens
Ok = 0
Oldtyp = " "
Olddsc = " "
CLEAR
DO WHILE .T.
DO WHILE .T.
SELECT CATEGORY
DO Inv_scr3
DO CASE
CASE READKEY() = 12 && Esc KEY
Ok = 0
EXIT
CASE READKEY() = 292 .OR. READKEY() = 36 && F1
DO Listcat
OTHERWISE
GOTO TOP
SEEK UPPER(T_ype)
IF FOUND()
Ok = 0
@ 10,20 SAY "Category code is already in file -"+UPPER(T_ype)+"-"
@ 11,20 SAY "Description: "+Descriptin
READ
GOTO TOP
ELSE
IF T_ype = Oldtyp .AND. DESC = Olddsc .AND. READKEY() = 270
Ok = 1
EXIT
ENDIF && T_ype = Oldtyp .AND. DESC = Olddsc
@ 12,20 SAY 'Record Verified, Press Ctl-End to add. '
Oldtyp = T_ype
Olddsc = DESC
ENDI && FOUND()
ENDCASE

ENDD && WHILE .T.
IF Ok = 1
SELECT CATEGORY
APPEND BLANK
REPLACE TYPE WITH UPPER(T_ype)
REPLACE Descriptin WITH DESC
ELSE
@ 12,20 SAY 'No record added, press any key to continue'
READ
EXIT
ENDI && Ok = 1

Y_n = 'Y'
@ 15,20 SAY "Do you want to Add another? (Y/N) " GET Y_n PICTURE "!!!"
READ
IF UPPER(Y_n) = "N"
EXIT
ENDI && UPPER(Y_n) = "N"
T_ype = SPACE(LEN(TYPE))
Desc = SPACE(LEN(Descriptin))
ENDD && WHILE .T.
RETURN
