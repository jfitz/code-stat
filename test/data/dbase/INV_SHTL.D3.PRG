* Program..: INV_shtl.PRG
* Notes....: Print Worksheet
*
SET CONFIRM ON
@ 20,0 CLEAR
@ 24,30 SAY "Printing Worksheet"
SET DEVICE TO PRINT
SET PRINT ON
SET CONSOLE OFF
* files used
@ 1,1 SAY "Date "
@ 1,6 SAY DATE()
@ 1,25 SAY "Personal Inventory Worksheet"
@ 1,25 SAY "Personal Inventory Worksheet"
* 8 lines/inch 120 characters
? CHR(27)+CHR(48)+CHR(27)+CHR(103)
@ 3,01 SAY "Type Description Serial # Cost"
@ 3,73 SAY " Current Aquired Warranty Amt Due Loca"
L_r=5

? CHR(27)+CHR(65)+CHR(6)+CHR(27)+CHR(80)
DO WHILE .T.
? "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
@ L_r,0 SAY "³"
@ L_r,4 SAY "³"
@ L_r,30 SAY "³"
@ L_r,43 SAY "³"
@ L_r,48 SAY "³"
@ L_r,54 SAY "³"
@ L_r,62 SAY "³"
@ L_r,65 SAY "³"
@ L_r,75 SAY "³"
@ L_r,79 SAY "³"

L_r=L_r+2
IF L_r>75
? "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
EXIT

ENDIF && L_r>75
ENDDO && WHILE .T.
SELECT CATEGORY

EJECT
SET CONSOLE ON
SET PRINT OFF
SET DEVICE TO SCREEN
RETURN