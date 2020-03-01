* Program..: INV_util.PRG
* Notes....: Utilities Menu
DO WHILE .T.
@ 0,0 CLEAR
@ 2, 0 TO 20,79 DOUBLE
SET COLOR TO &HighVideo
@ 3,16 SAY [P E R S O N A L I N V E N T O R Y S Y S T E M]
@ 5,16 SAY [ U T I L I T Y M E N U]
SET COLOR TO &StdVideo
@ 4,1 TO 4,78 DOUBLE
@ 6,1 TO 6,78
* ---display detail lines
@ 8,28 SAY [1. Add Location Codes]
@ 9,28 SAY [2. Update Location Codes]
@ 10,28 SAY [3. Add Category Codes]
@ 11,28 SAY [4. Update Category Codes]
@ 12,28 SAY [5. Browse Location Codes]
@ 13,28 SAY [6. Browse Category Codes]
@ 14,28 SAY [7. Re-Organize Database]
@ 15,28 SAY [8. Temporary Exit To DOS]
@ 16,28 SAY [9. Exit to dBase III+]
@ 18, 28 SAY '0. Return to Main Menu'
STORE 0 TO Selectnum
@ 20,33 SAY " Select "
@ 20,42 GET Selectnum PICTURE "9" RANGE 0,9
READ
DO CASE
CASE Selectnum = 0
RETURN
CASE Selectnum = 1
DO Inv_loca
CASE Selectnum = 2
DO Inv_locu
CASE Selectnum = 3
DO Inv_cata
CASE Selectnum = 4
DO Inv_catu
CASE Selectnum = 5
SELECT LOCATION
BROWSE FREEZE Descriptin NOAPPEND
CASE Selectnum = 6
SELECT CATEGORY
BROWSE FREEZE Descriptin NOAPPEND
CASE Selectnum = 7
@ 2,0 TO 4,79 DOUBLE
@ 5,0 CLEAR
@ 5,0 SAY ' '
SET TALK ON
SELECT INVENTORY
? 'Packing Inventory Database to Remove Records Marked For Deletion'
PACK
? 'Packing Category'
SELECT CATEGORY
PACK
? 'Packing Location'
SELECT LOCATION
PACK
WAIT
SET TALK OFF
RETURN
CASE Selectnum = 8
!Cls^4Dos && 4Dos Command Shell
* !Command && DOS Command Shell
CASE Selectnum = 9
SET TALK ON
SET BELL ON
SET STATUS ON
SET ESCAPE ON
SET CONFIRM ON
CLOSE DATABASES
CANCEL
ENDCASE
ENDD T
RETURN
