* Program..: Fdate.PRG
* Author...: Chris White
* Date.....: January 25, 1985
* Note(s)..: This program takes a date variable passed to it
* with the PARAMETERS phrase and returns the date
* as a formatted character string.
*
PARAMETERS date,code,datestrg
DO CASE
CASE code = 1
* ---"Jan 25, 1984"
datestrg = SUBSTR(CMONTH(date),1,3) + ' ' +;
STR(DAY(date),2) + ', ' + STR(YEAR(date),4)
CASE code = 2
* ---"Jan 1984"
datestrg = SUBSTR(CMONTH(date),1,3) + ' ' +;
STR(YEAR(date),4)
CASE code = 3
* ---"Jan 25"
datestrg = SUBSTR(CMONTH(date),1,3) + ' ' +;
STR(DAY(date),2)
CASE code = 4
* ---"January 25, 1984"
datestrg = CMONTH(date) + ' ' +;
STR(DAY(date),2) + ', ' + STR(YEAR(date),4)
CASE code = 5
* ---"25-Jan-84"
datestrg = STR(DAY(date),2) + '-' +;
SUBSTR(CMONTH(date),1,3) +;
'-' + SUBSTR(STR(YEAR(date),4),3,2)
OTHERWISE
datestrg = 'Error'
ENDCASE
RETURN
* EOF: Fdate.PRG
