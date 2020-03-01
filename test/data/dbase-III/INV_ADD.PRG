* Program..: INV_add.PRG
* Notes....: Add Inventory Item
Title = " I t e m A d d i t i o n"
Function =" F1 = Location Codes F2 = Category Codes F3 = Quit F4 = Add Record"
CLEAR
Already = '0'
DO WHILE .T.
SELECT INVENTORY
STORE DATE() TO Date_aq, Date_en, Date_up
STORE "N" TO Warrnt
STORE SPACE(len(descriptin)) TO DESC
STORE SPACE(len(serial_num)) TO Serial
STORE SPACE(len(comment1)) TO Comm1
STORE SPACE(len(comment2)) TO Comm2
STORE SPACE(len(Comment3)) TO Comm3
STORE SPACE(len(type)) TO T_ype
STORE SPACE(len(location)) TO Loca
STORE 0.00 TO Cost, Replacea, Current, Amount
Catdesc = " "
Locdesc = " "
DO WHILE .T.
SELECT INVENTORY
DO Inv_scr
DO CASE
CASE READKEY() = 12 && Esc KEY
Got = 0
Got2 = 0
EXIT
CASE READKEY()=292.OR.READKEY()=36.OR.READKEY()=6.OR.READKEY()=262
SET PROCEDURE TO INV_LCDE
IF READKEY() = 292 .OR. READKEY() = 36 && F1
DO Listloc
ELSE
DO Listcat
ENDIF
Already = '0'
OTHERWISE && Edit DATA
SELECT LOCATION
GOTO TOP
SEEK UPPER(Loca)
IF FOUND()
Got = 1
Locdesc = Descriptin
@ 21,20 SAY " "
ELSE
Got = 0
SET COLOR TO &highvideo
@ 21,20 SAY "Location code is invalid -"+UPPER(Loca)+"-"
SET COLOR TO &stdvideo
ENDI && FOUND()

SELECT CATEGORY
GOTO TOP
SEEK UPPER(T_ype)
IF FOUND()
Catdesc = Descriptin
Got2 = 1
@ 22,20 SAY " "
ELSE
SET COLOR TO &highvideo
@ 22,20 SAY "Type code is invalid -"+UPPER(T_ype)+"-"
SET COLOR TO &stdvideo
Got2 = 0
ENDI && FOUND()

IF Got = 1 .AND. Got2 = 1 .AND. READKEY() = 270
EXIT
ENDI && Got = 1 .AND. Got2 = 1
ENDCASE
ENDD && WHILE .T.
IF Got = 1 .AND. Got2 = 1
@ 23,20 SAY 'Record being added, please wait'
SELECT INVENTORY
?? '.'
APPEND BLANK
?? '.'
REPLACE Type WITH T_ype,Location WITH Loca, Warranty WITH Warrnt,;
Dateaquird WITH Date_aq,Date_enter WITH Date_en,Lastupdate WITH Date_up,;
Descriptin WITH Desc,Serial_num with Serial,Cost_amt WITH Cost
REPLACE Replaceamt WITH Replacea,Currentval WITH current,Amountdue with Amount,;
Comment1 WITH Comm1,Comment2 WITH Comm2,Comment3 with Comm3
ELSE
@ 23,20 SAY 'No record added, press any key to continue'
READ
EXIT
ENDI && Got = 1 .AND. Got2 = 1
Y_n = 'N'
@ 23,20 SAY "Do you want to Add another? (Y/N) " GET Y_n PICTURE "!!!"
READ
IF UPPER(Y_n) = "N"
EXIT
ENDI && UPPER(Y_n) = "N"
ENDD && WHILE .T.
RETURN
