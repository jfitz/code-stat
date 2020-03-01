*:*********************************************************************
*:
*: Program: PROC.PRG
*:
*: System: Procedure and UDF Library
*: Author: CSF/MHF
*: Copyright (c) 1990, CSF/MHF
*: Last modified: 05/18/91 13:59
*:
*: Procs & Fncts: KEYTEST
*: : EMPTY()
*: : NEXTDATE()
*: : MONTHCHECK
*: : DAYCHECK
*: : PREVMONTH()
*: : SETPRINT
*: : PRINTERR
*: : SETCOLOR
*: : EXTRCLR()
*: : OPEN_SCREEN
*: : DO_WAIT()
*: : JAZCLEAR
*: : CENTER
*: : CENTER2()
*: : SURROUND()
*: : MESSAGE1()
*: : MESSAGE2()
*: : MESSAGE3()
*: : MESSAGE4()
*: : MONITOR
*: : SCRNHEAD()
*: : YESNO()
*: : YES
*: : NO
*: : ERRORMSG()
*: : DATETEXT()
*: : DATETXT2()
*: : ISUNIQUE()
*: : ALLTRIM()
*: : SHADOW
*: : FULLWIN
*: : DOSRUN()
*: : RCD_LIST
*: : FULDSP
*:
*: Documented 05/18/91 at 14:06 SNAP! version 4.02i
*:*********************************************************************
********************************************************************************
*-- Note that to use this program file, the program must have the lines
*-- in it stating SET PROCEDURE TO PROC (usually early in the prog.)
*-- DO SETCOLOR && procedure to set screen colors
*-- DO SETPRINT && procedure for printer setup
*-- When done, it is a good idea to close the procedure file, using the
*-- command SET PROCEDURE TO
*===============================================================================
* Quick Summary of Procedures/functions herein:
*===============================================================================
* Name Description
*---------- -----------------------------------------------------------------
*
* Keytest PROCEDURE: Used to show the ASCII value of a key as returned
* by DBASE IV, Press ESC key to exit
* USAGE: DO KEYTEST
*
* EMPTY FUNCTION: Used to check whether a variable contains anything:
*
* Field type Checks for
* ---------- -------------------
* C - Character Any character(s) other than blank(s)
* N - Numeric Any value other than zero
* F - Float Any value other than zero
* D - Date Any date other than a blank field: " / / "
* M - Memo Any contents (a single line with only blanks
* returns .F.; multiple lines always return .T.)
*
* Syntax:
* empty( / / )
*
* Returns: logical .T. or .F.
*
* Samples:
*
* SET PROCEDURE TO EMPTY |
* | use my_dbf
* Z = "Hello" | ? empty( my_dbf->field_1 )
* ? empty( Z ) | .T.
* .F. |
* | replace my_dbf->field_1 with "AnyThing"
* Z = {} | ? empty( my_dbf->field_1 )
* ? empty( Z ) | .F.
* .T. |
* |
************************************************************************
* NEXTDATE() FUNCTION
*
* Note: Handy for Billing Cycles, Appointments, Payables etc!
*
* Usage: NEXTDATE({date},string)
* where {date} is a date expression (starting date)
* and the string is one of the Interval Codes listed below
* or a number of days (in Character data type).
*
************************************************************************

* Valid Interval Codes include "D","W","BW","SM","M","BM","Q","SA","A", or a
* number of days from the start date.
*
* Can be used in an @ SAY/GET along with a cycle in a Client.dbf, or with a
* REPLACE command - I've found it especially handy for Billing programs
* where clients are on different billing cycles or payment plans!
*
* NOTE: This UDF handles Leap Year & 'pushes' non-existent dates to the end of
* the previous month (i.e. a calculation for 2/30/91 would return 2/28/91).
*
*
* DAYCHECK PROCEDURE To Correct Days Higher than the number of
* days in the month
* USAGE: Do DAYCHECK
*
* PREVMONTH FUNCTION to Get range for previous month
* using a base date (usually DATE())
* If today is 12/xx/90 then returns are 11/01/90 and 11/30/90
* EXAMPLE:
*
* monthstart = {}
* monthend = {}
* @ 8,1 say "Print Last Months Labor Report"
** Defaults to the previous month range for user,
** or user can enter new date range.
* dummyvar = prevmonth(monthstart,monthend,date())
* @ 11,1 say "Starting Date:" get monthstart
* @ 12,1 say "Ending Date :" get monthend
* read
** report form labor while workdate => monthstart;
** .and. workdate <= monthend;
** to file (pr1)
* return
*
*
* setprint PROCEDURE: Used to set the the appropriate default settings.
* (Can be modified easily for other printers ...)
* USAGE: do setprint
*
* printerr PROCEDURE: Used to display a printer error for STAND-ALONE
* systems. (The dBASE function PRINTSTATUS() doesn't work
* on a Network with Print Spoolers ...)
* USAGE: do setprint
* if .not. PRINTSTATUS()
* DO PRINTERR
* endif
* or
* DO WHILE .not. PRINTSTATUS()
* DO PRINTERR
* ENDDO
*
* setcolor PROCEDURE: Used to set the screen colors for a system. It
* checks to see if a color monitor is attached (ISCOLOR()),
* and sets system variables, that can be used in SET COLOR OF
* commands.
* NOTE - if program CLRSHOW has been executed, it will create
* a file: COLOR.MEM that is used here. It can be used to
* design screen colors for the user, but if the COLOR.MEM
* file does not exist, this routine will use defaults
* given below. More docs on colors and SET commands
* are in the procedure's comments.
* USAGE: do setcolor
*
* extrclr FUNCTION: Used to extract the first parameter of the MEMVARS
* created from SETCOLOR above. The SET COLOR OF commands can
* only use the first parameter. REQUIRES THAT SETCOLOR BE
* EXECUTED FIRST -- otherwise the memvars will not be accessible.
* USAGE: set color of highlight to &extrclr(cl_stand)
*
* open_screen PROCEDURE: Used to give a texture to the background of the screen
* USAGE: do open_screen
*
* do_wait FUNCTION: Used in place of the standard WAIT command, deals with
* centering the text at the message line (row 24).
* USAGE: lc_wait = do_wait("message")
*
* jazclear PROCEDURE: Used to clear the screen from the middle out --
* could be used with open_screen, to really fancify things!
* USAGE: do jazclear
*
* center PROCEDURE: Centers text on the screen with @says
* USAGE: DO CENTER with row,screenwidth,"color(s),"message"
*
* center2 FUNCTION: centers text similar to above, only two parameters.
* USAGE: @row,center2(ln_width,"TEXT") say "TEXT"
* or @row,center2(ln_width,memvar) say memvar
*
* surround FUNCTION: Displays a message surrounded by a box anywhere on
* the screen
* USAGE: Dummy = SURROUND(row,column,"colors","text")
*
* message1 FUNCTION: Displays a message, centered, pauses until user
* presses a key
* USAGE: Dummy = MESSAGE1(row,screenwidth,"colors","text")
*
* message2 FUNCTION: Displays a message in a window, pauses for user to
* press key
* USAGE: Dummy = MESSAGE2("Text")
*
* message3 FUNCTION: Displays a message in a window, pauses for user,
* will wrap a long message inside the window.
* USAGE: Dummy = MESSAGE3("text")
*
* message4 FUNCTION: Displays a 2-line message in a predefined window
* and pauses
* USAGE: Dummy = MESSAGE4("text1","text2")
*
* monitor PROCEDURE: Displays a status message to monitor a long-running
* operation that operates on multiple records . . .
* USAGE: DO MONITOR WITH "text"
* DO WHILE (or SCAN)
* stuff -- process records
* @4,x DISPLAY ltrim(str(recno())) && current record
* && in window MONITOR
* ENDDO (endscan)
* DEACTIVATE WINDOW MONITOR
* RELEASE WINDOW MONITOR
* RESTORE SCREEN FROM SC_MONITOR
* RELEASE SCREEN SC_MONITOR
*
* scrnhead FUNCTION: Displays a heading on the screen in a box 2
* spaces wider than the text, with a custom border (double
* line top, single the rest)
* USAGE: Dummy = SCRNHEAD("colors","Text")
*
* yesno FUNCTION: Asks a yes/no question in a dialog window/box
* USAGE: yesno(.t.,"Message1","Message2","Message3")
* can be used anywhere a function is used.
* first parameter is default state (.t. or .f.)
* "Message1" is the first line of message (max length = 36)
* to display in the window describing the action
* There is a blank line between message1 and message2,
* but not between 2 and 3. You can leave any of them blank
* by placing a set of quotes there, i.e.:
* yesno(.t.,"Message1","","")
*
* yes These two PROCEDUREs are a part of the yesno function above.
* no
*
* errormsg FUNCTION: Display an error message: 1= ** ERROR **, 2 and 3
* are user defined, 4= Press any key to continue ...
* colors should be VIVID, since it's an error message.
* USAGE: Dummy = ERRORMSG("message2","message3","Colors")
* message2 and 3 should be 36 characters or less ...
* see YESNO about blank message(s). Colors should include
* foreground/background,foreground/background (border ...)
*
* datetext FUNCTION: Display date in format Month, day year (eg July 1,1991)
* USAGE: DATETEXT(datefield)
*
* datetxt2 FUNCTION: Display date in format dayofweek, month day, year
* (eg Monday, July 1, 1991)
* USAGE: DATETXT2(datefield)
*
* isunique FUNCTION: Checks a keyfield to see if it is a unique entry
* USAGE: USE database ORDER tag ALIAS aliasname
* USE database ORDER tag ALIAS DupCheck AGAIN
* * second use of database is read only for ISUNIQUE
* @x,y SAY "prompt" GET lc_var PICTURE "picture";
* valid required ISUNIQUE(lc_var);
* message "Enter a UNIQUE code";
* error chr(7)+"Field must be unique!"
* where 'lc_var' is memory variable/field being checked against
* a specific 'field'. Make sure the correct index is set
* as this function uses the SEEK command.
* **** ALSO modify the field in the function below.
*
* alltrim FUNCTION: Used to remove trailing/leading spaces
* USAGE: field = alltrim(field)
*
* shadow PROCEDURE: Creates a shadow for a window (taken from the dBASE IV
* picklist commands)
* USAGE: SAVE SCREEN TO SC_name
* DEFINE WINDOW name FROM trow,tcol TO brow,bcol DOUBLE
* DO shadow WITH trow,tcol,brow,bcol
* ACTIVATE WINDOW name
* perform actions in window
* DEACTIVATE WINDOW name
* RESTORE SCREEN FROM SC_name
* RELEASE SCREEN SC_name
*
* fullwin PROCEDURE: Overlays the menus on screen with a full window,
* so that processing is done in the window, and one can return
* directly to the menus, without redrawing screen and such.
* USAGE: DO fullwin WITH "colors","winname","screenname"
* perform whatever actions are needed in the window
* DEACTIVATE WINDOW winname
* RELEASE WINDOW winname
* RESTORE SCREEN FROM screenname
* RELEASE SCREEN screenname
*
* dosrun FUNCTION: routine to run a DOS program, checks to see if a
* window is active -- if so, it avoids the inevitable
* "Press any key to continue" and the subsequent messing
* up of the screen display.
* USAGE: dummy = DOSRUN("doscommand /parm(s)...") or
* dummy = DOSRUN(memvar) && where memvar contains dos
* && command and parameters ...
*
*
* RCD_LIST PROCEDURE to simulate a popup file pick list which can
* can have as many fields displyed as desired
*
* set escape off && ESCAPE MUST be off
*
* Be sure the data base and index is active before calling rcd_list
* EXAMPLE:
* USE STUDENT INDEX STNAME
*
* This procedure will probably be called from a UDF as follows:
* @1,1 get ans VALID Check
* Then Check UDF calls the procedure rcd_list. When get return
* from rcd_list, can @ x,x say varable or use keyboard stuff
*********************************************************************
* Either title_1 or title_2 Must be as big as
* total length of fields to be displayed including
* spaces added as field separators (separator = ', 'in fld= below)
* EXAMPLE:
* title_1 = "Student Data Base" && 1st title line
* title_2 = "<------Last Name-------><---First Name-->" && 2nd title line
*
* FLD contains fields to display. The file MUST be indexed by these fields
* NOTE how the ' is used within the "
* EXAMPLE:
* fld="last+' '+first"
*
* scrnmax=22 && # of lines to use for window-2 lines of this used by box
* ok_to_srch= .T. && .T. allows alpha search. IF .F. search is numeric
*
* srow=0 && SETS THE TOP ROW OF THE DISPLAY
* scol=0 && SETS THE LEFT COLUMN OF THE DISPLAY
*
* do rcd_list && Execute the procedure
*
* Use PgDn, PgUp to display next or prior screen, CtlHome or CtlEnd to go
* to first or last screen. Home and End position the pointer to the
* top or bottom of the screen. If the database is indexed properly, you
* can enter keyboard keys to search for records.
* If you press ESC, the database is sent to EOF, If you use ENTER, the
* database is positioned to the record at the cursor of the list. When you
* exit the rcd_list test for these conditions and also have the record.
*****************************************************************
*===============================================================================
* END OF DESCRIPTIONS, On to the procedures and functions themselves!
*===============================================================================

*!*********************************************************************
*!
*! Procedure: KEYTEST
*!
*!*********************************************************************
PROCEDURE keytest
****************

* Used to show the ASCII value of a key as returned by DBASE IV
* Press ESC key to exit

SET TALK OFF
SET ESCAPE OFF
CLEAR
I = 0
DO WHILE .T.
I = INKEY()
IF I # 0
IF I = 27
EXIT
ENDIF
@ 5,25 SAY STR(I, 3)
I = 0
ENDIF
ENDDO
SET TALK ON
SET ESCAPE ON
RETURN
* End of KeyTest.prg


*!*********************************************************************
*!
*! Function: EMPTY()
*!
*!*********************************************************************
FUNCTION EMPTY

PARAMETERS f_variable && may be memory variable or database field name
PRIVATE f_set_talk, f_return

IF SET("TALK") = "ON"
SET TALK OFF
f_set_talk = "ON"
ELSE
f_set_talk = "OFF"
ENDIF


f_return = .F. && FALSE means: variable is NOT empty

DO CASE
CASE TYPE( "f_variable" ) = "C"
IF LEN( LTRIM(RTRIM( f_variable )) ) = 0
f_return = .T.
ENDIF

CASE TYPE( "f_variable" ) = "N" .OR. TYPE( "f_variable" ) = "F"
IF f_variable = 0
f_return = .T.
ENDIF

CASE TYPE( "f_variable" ) = "L"
f_return = .F. && Can't check logical fields

CASE TYPE( "f_variable" ) = "D"
IF f_variable = {}
f_return = .T.
ENDIF

CASE TYPE( "f_variable" ) = "M"
IF LEN( f_variable ) = 0
f_return = .T.
ENDIF

OTHERWISE && TYPE = "U"
f_return = .T.

ENDCASE

IF f_set_talk = "ON"
SET TALK ON
ENDIF

RETURN f_return


*!*********************************************************************
*!
*! Function: NEXTDATE()
*!
*! Calls: MONTHCHECK (procedure in PROC.PRG)
*! : DAYCHECK (procedure in PROC.PRG)
*!
*!*********************************************************************
FUNCTION nextdate && Figures the next date based on given Date & Interval
PARAMETERS ld_olddate, lc_intervl && Date to work from & Character
* String defining the interval

*--Clean up the interval
lc_intervl = UPPER(LTRIM(TRIM(lc_intervl)))

PRIVATE ALL
STORE 0 TO ln_newmo,ln_newdy,ln_newyr,ln_olddy
STORE {} TO ld_newdate

DO CASE && Valid Interval Codes Include:

CASE lc_intervl = "D" && "D" - Daily (Every Day)
ld_newdate = ld_olddate + 1

CASE lc_intervl = "W" && "W" - Weekly (Every Week)
ld_newdate = ld_olddate + 7

CASE lc_intervl = "BW" && "BW" - BiWeekly (Every Other Week)
ld_newdate = ld_olddate + 14

CASE lc_intervl = "SM" && "SM" - SemiMonthly (Twice Monthly on the same 'Days')
ln_olddy = DAY(ld_olddate)
IF ln_olddy <= 14
ld_newdate = ld_olddate + 14
ELSE
ln_newdy = ln_olddy - 14
ln_newmo = MONTH(ld_olddate)+1
DO monthcheck
ld_newdate = CTOD(LTRIM(STR(ln_newmo))+"/"+LTRIM(STR(ln_newdy))+"/"+LTRIM(STR(ln_newyr)))
ENDIF

CASE lc_intervl = "M" && "M" - Monthly (Same day of the month)
ln_newmo = MONTH(ld_olddate)+1
DO monthcheck
ln_newdy = DAY(ld_olddate)
DO daycheck
ld_newdate = CTOD(LTRIM(STR(ln_newmo))+"/"+LTRIM(STR(ln_newdy))+"/"+LTRIM(STR(ln_newyr)))

CASE lc_intervl = "BM" && "BM" - BiMonthly (Every other month)
ln_newmo = MONTH(ld_olddate)+2
DO monthcheck
ln_newdy = DAY(ld_olddate)
DO daycheck
ld_newdate = CTOD(LTRIM(STR(ln_newmo))+"/"+LTRIM(STR(ln_newdy))+"/"+LTRIM(STR(ln_newyr)))

CASE lc_intervl = "Q" && "Q" - Quarterly
ln_newmo = MONTH(ld_olddate)+3
DO monthcheck
ln_newdy = DAY(ld_olddate)
DO daycheck
ld_newdate = CTOD(LTRIM(STR(ln_newmo))+"/"+LTRIM(STR(ln_newdy))+"/"+LTRIM(STR(ln_newyr)))

CASE lc_intervl = "SA" && "SA" - SemiAnnually (Twice Yearly)
ln_newmo = MONTH(ld_olddate)+6
DO monthcheck
ln_newdy = DAY(ld_olddate)
DO daycheck
ld_newdate = CTOD(LTRIM(STR(ln_newmo))+"/"+LTRIM(STR(ln_newdy))+"/"+LTRIM(STR(ln_newyr)))

CASE lc_intervl = "A" && "A" - Annually (Same Day each year)
ln_newmo = MONTH(ld_olddate)
ln_newdy = DAY(ld_olddate)
ln_newyr = YEAR(ld_olddate)+1
ld_newdate = CTOD(LTRIM(STR(ln_newmo))+"/"+LTRIM(STR(ln_newdy))+"/"+LTRIM(STR(ln_newyr)))

OTHERWISE && Interval specified by a number of days.
ld_newdate = ld_olddate + VAL(lc_intervl)
ENDCASE


RETURN ld_newdate
*-- EOP: NextDate


*!*********************************************************************
*!
*! Procedure: MONTHCHECK
*!
*! Called by: NEXTDATE() (function in PROC.PRG)
*!
*!*********************************************************************
PROCEDURE monthcheck && Corrects Months Higher than 12 & Sets Correct Year
IF ln_newmo > 12
ln_newmo = ln_newmo - 12
ln_newyr = YEAR(ld_olddate)+1
ELSE
ln_newyr = YEAR(ld_olddate)
ENDIF
*-- EOP: MonthCheck


*!*********************************************************************
*!
*! Procedure: DAYCHECK
*!
*! Called by: NEXTDATE() (function in PROC.PRG)
*!
*!*********************************************************************
PROCEDURE daycheck &&Corrects Days Higher than the number of days in the month
*NEXT CALCULATES 1ST OF MONTH AND BEGINNING OF NEXT MONTH
ld_beg=CTOD(STR(ln_newmo,2,0)+"/"+"01/"+STR(ln_newyr,4,0))
ld_next=CTOD(STR(ln_newmo+1,2,0)+"/"+"01/"+STR(ln_newyr,4,0))
ln_lastday=ld_next-ld_beg && CALCULATE # OF DAYS IN MONTH
IF ln_newdy > ln_lastday
ln_newdy = ln_lastday
ENDIF
*-- EOP: DayCheck


*!*********************************************************************
*!
*! Function: PREVMONTH()
*!
*!*********************************************************************
FUNCTION prevmonth

PARAMETERS ms,me,bd

me = bd - DAY(bd)
ms = me - DAY(me) + 1
RETURN .T.



*!*********************************************************************
*!
*! Procedure: SETPRINT
*!
*!*********************************************************************
PROCEDURE setprint && used to set the printer to the proper mode
_PDRIVER = "HPLAS2I" && printer driver
_PPITCH = "PICA" && printer pitch (10 CPI)
_BOX = .T. && make sure we can print boxes/line draw
_PLOFFSET = 0 && page offset (left side) to 0
_LMARGIN = 0 && left margin (also set to 0)
_PLENGTH = 60 && page length 60, since HP has top/bottom margins
_PEJECT = "NONE" && don't send extra blank pages . . .
* _pquality = .t. && set print quality to high -- not available
&& for HP Laserjets ...

RETURN && -- end of procedure SETPRINT

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Procedure: PRINTERR
*!
*! Calls: SHADOW (procedure in PROC.PRG)
*!
*!*********************************************************************
PROCEDURE printerr && used to display specific printer error message
&& This one's mine Ken
IF ISCOLOR()
lc_color = "RG+/R,RG+/R,RG+/R"
ELSE
lc_color = "N/W,N/W,N/W"
ENDIF

define WINDOW printerr FROM 7,15 TO 16,57 DOUBLE COLOR &lc_color
SAVE SCREEN TO sc_perr && store current screen
DO shadow WITH 7,15,16,57 && shadow box!
ACTIVATE WINDOW printerr && here we go ..

@0,0 SAY CHR(7) + " *** PRINTER ERROR ***"
@2,0 SAY " The printer is not ready. Please check:"
@3,0 SAY " 1) that the printer is ON,"
@4,0 SAY " 2) that the printer is ONLINE, and"
@5,0 SAY " 3) that the printer has paper."
@6,0 SAY " "
WAIT " Press any key to continue . . ."

DEACTIVATE WINDOW printerr
RELEASE WINDOW printerr
RESTORE SCREEN FROM sc_perr
RELEASE SCREEN sc_perr

RETURN && from printerr

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Procedure: SETCOLOR
*!
*! Memory Files: COLOR.MEM
*!
*!*********************************************************************
PROCEDURE setcolor && used to set the screen color memory variables
** "Borrowed" from Phil Steele's PCSDEMO.PRG (public domain)
** with lots of extra comments by me ...
* note: colors can be combined in lots of interesting ways,
* here are the color codes:
* W = White W+ = Bright White
* N = Black N+ = Grey
* R = Red R+ = Pink
* G = Green G+ = Light Green
* B = Blue B+ = Light Blue
* RG = Brown RG+ = Yellow
* RB = Magenta RB+ = Bright Magenta
* GB = Cyan GB+ = Light Cyan
* NOTE that you can set the colors from CLRSHOW.PRG, which WILL
* store the correct memory variables (below) with the settings
* determined in that program into COLOR.MEM.
*--------------------------------------------------------
* Here are the SET COLOR OF options:
*
* SET COLOR OF NORMAL TO
* @/Say without COLOR keyword
* Non-selected BROWSE items
* Layout design surface
* Uncolored box borders with @..TO
* Scoreboard
* SET COLOR OF MESSAGES TO
* Message Line messages
* Error messages in error box (dbase)
* Navigation line messages
* Available, unselected menu/list choices
* SET COLOR OF TITLES TO
* List/Display headings
* Help box headings
* BROWSE field name headings
* Ruler line (in editor)
* SET COLOR OF BOX TO
* Menu borders
* Pick List/prompt box
* SET COLOR OF HIGHLIGHT TO
* Highlighted menu/list choices
* SET COLOR OF INFORMATION TO
* Clock/Help box/Status line
* SET COLOR OF FIELDS TO
* Selected fields in BROWSE
* Editable fields in @...Get
* to use the memory variables (which all begin with CL_) defined
* in this function, in the SET COLOR OF commands, you will need
* the function EXTRCLR below this procedure to extract the first
* paramter only. For example:
* SET COLOR OF BOX TO &extrclr(cl_warn)
* This will extract the first parameter of CL_Warn and use it
* to define the colors of boxes ...
*----------------
* These memvars can also be used in any command with a
* COLOR clause, such as:
* DEFINE WINDOW name FROM x,y TO x2,y2 DOUBLE COLOR &cl_warn.
*--------------------------------------------------------

IF FILE("COLOR.MEM")
RESTORE FROM COLOR ADDITIVE && if color.mem exists, restore from it
ELSE && otherwise, create it
PUBLIC cl_blank,cl_help,cl_data,cl_error,cl_entry,cl_stand,;
cl_menu,cl_warn
C = ISCOLOR() && remember -- foreground/background
cl_blank = "N/N,N/N,N" && black on black on black ...
cl_func = "N/W"
* if iscolor() = true, define color, otherwise black/white
cl_help = IIF(C,"N/G,G/N,N" , "W+/N,N/W,N") && help
cl_data = IIF(C,"RG+/GB,GB/RG+,N", "W+/N,N/W,N") && data entry fields
cl_error = IIF(C,"RG+/R,R/RG+,N" , "W/N,N/W,N/W") && error messages
cl_entry = IIF(C,"n/w,W/N,N" , "n/w,W/N,N") && data entry??
cl_stand = IIF(C,"w+/b,b/w,N" , "w+/n,n/w,N") && standard screen
cl_menu = IIF(C,"rg+/b,B/RG+,N" , "w+/n,N/W,N") && menus
cl_warn = IIF(C,"rg+/r,R/RG+,N" , "w/n,N/W,N") && warning messages
SAVE TO COLOR ALL LIKE cl_* && create COLOR.MEM
ENDIF

*-- change current color settings to these ...
SET COLOR TO &cl_stand && default

RETURN && from procedure setcolor

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: EXTRCLR()
*!
*!*********************************************************************
FUNCTION extrclr && extract the first color combination for use in
** SET COLOR OF routines -- SETCOLOR must have been
** executed first ...

PARAMETERS lc_memvar

RETURN SUBSTR(lc_memvar,1,(AT(",",lc_memvar)-1))

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Procedure: OPEN_SCREEN
*!
*!*********************************************************************
PROCEDURE open_screen

** Designed to give textured opening screen -- written by Rick Price
** stolen agregiously by Ken Mayer (with Rick's permission)

CLEAR
X=0
lc_backdrp = CHR(176) && chr(176) = "°"
DO WHILE X<3
@X,0 TO X+3,79 lc_backdrp && display this box
sx=X
X=X+6
@X,0 TO X+3,79 lc_backdrp
X=X+6
@X,0 TO X+3,79 lc_backdrp
X=X+6
@X,0 TO X+3,79 lc_backdrp
X = sx+1
ENDDO
@24,0 TO 24,79 lc_backdrp

RETURN && end of procedure open_screen

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: DO_WAIT()
*!
*!*********************************************************************
FUNCTION do_wait

** Another routine stolen from Rick Price to handle the need for
** a wait, but killing the ESCAPE key, amongst other things.

PARAMETERS lc_message

lc_waitcur = SET("CURSOR") && save status of cursor
SET CURSOR OFF

** The the passed parameter (message_to_display) is null, use a generic
** message.
lc_message = ;
IIF(""=lc_message," Press any key to continue . . . ",lc_message)
* deal with centering/truncating message
ln_meslen = LEN(lc_message)
lc_message = IIF(ln_meslen>80,LEFT(lc_message,80),lc_message)
ln_meslen = LEN(lc_message) && reset if message was longer than 80
@24,INT((80-ln_meslen)/2) SAY lc_message
lc_retstr=CHR(INKEY(0))
SET CURSOR &lc_waitcur

RETURN lc_retstr && end of function Do_Wait

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Procedure: JAZCLEAR
*!
*!*********************************************************************
PROCEDURE jazclear

** also stolen from Rick Price -- another fancy screen clear
** explode outward from the center -- pretty fancy stuff

ln_winr1 = 0 && row 1
ln_winr2 = 24 && row 2
ln_winc1 = 0 && column 1
ln_winc2 = 79 && column 2
ln_step = 1 && amount to increment by
* set starting point
mn_winc1 = INT((ln_winc2-ln_winc1)/2)+ln_winc1
mn_winc2 = mn_winc1+1
mn_winr1 = INT((ln_winr2-ln_winr1)/2)+ln_winr1
mn_winr2 = mn_winr1+1

** Adjust step offset values: ln_ColOff & ln_RowOff
** Vertical steps - mn_WinR1-ln_WinR1
ln_tmpadjr = INT((ln_winr2 - ln_winr1)/2)
ln_tmpadjc = INT((ln_winc2 - ln_winc1)/2)

ln_adjrow = ;
IIF(ln_tmpadjc > ln_tmpadjr, ln_tmpadjr/ln_tmpadjc,1) * ln_step

ln_adjcol = ;
IIF(ln_tmpadjr > ln_tmpadjc, ln_tmpadjc/ln_tmpadjr,1) * ln_step

ln_colleft = ln_winc1
ln_colrite = ln_winc2
ln_rowtop = ln_winr1
ln_rowbot = ln_winr2
ln_winc1 = mn_winc1
ln_winc2 = mn_winc2
ln_winr1 = mn_winr1
ln_winr2 = mn_winr2
DO WHILE (ln_winc1#ln_colleft .OR. ln_winc2#ln_colrite .OR. ;
ln_winr1 # ln_rowtop .OR. ln_winr2 # ln_rowbot)

* Adjust coordinates for the clear (moving out from the middle)
ln_winr1 = ;
ln_winr1-IIF(ln_rowtop ln_winr2 = ;
ln_winr2+IIF(ln_rowbot>ln_winr2+ln_adjrow,ln_adjrow,ln_rowbot-ln_winr2)
ln_winc1 = ;
ln_winc1-IIF(ln_colleft ln_winc2 = ;
ln_winc2+IIF(ln_colrite>ln_winc2+ln_adjcol,ln_adjcol,ln_colrite-ln_winc2)

* Perform the clear
@ln_winr1,ln_winc1 CLEAR TO ln_winr2,ln_winc2
@ln_winr1,ln_winc1 TO ln_winr2,ln_winc2
ENDDO
CLEAR
RETURN && from JazClear

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Procedure: CENTER
*!
*! Called by: MESSAGE1() (function in PROC.PRG)
*! : MESSAGE2() (function in PROC.PRG)
*! : MESSAGE4() (function in PROC.PRG)
*! : MONITOR (procedure in PROC.PRG)
*! : YESNO() (function in PROC.PRG)
*! : ERRORMSG() (function in PROC.PRG)
*!
*!*********************************************************************
PROCEDURE CENTER
** Used to center text on the screen with @Says, borrowed from
** Miriam Liskin's dBASE IV Programming Book

PARAMETERS ln_line,ln_width,lc_color,lc_text

lc_text = ALLTRIM(lc_text) && alltrim from below
ln_col = (ln_width - LEN(lc_text)) /2
@ln_line,ln_col SAY lc_text COLOR &lc_color.

RETURN && from procedure center

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: CENTER2()
*!
*!*********************************************************************
FUNCTION center2 && centers text from a function, making it more
** accessible than CENTER (above).
** got the idea from Jeff (aka: The Musician/STUDENT
** from the AT-Bulletin Board (ATBBS)).

PARAMETERS ln_width,lc_text

RETURN (ln_width - LEN(lc_text)) / 2 && end of function center2

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: SURROUND()
*!
*!*********************************************************************
FUNCTION surround

** from Miriam Liskin's Book
** Displays text surrounded by a box anywhere on the screen
** changed to a function: 4/19/91 KJM

PARAMETERS ln_line,ln_column,lc_color,lc_text

lc_text = " " + TRIM(lc_text) + " " && add spaces around text
@ln_line-1,ln_column-1 TO ln_line+1,ln_column+LEN(lc_text) DOUBLE;
COLOR &lc_color. && draw box
@ln_line,ln_column SAY lc_text COLOR &lc_color. && disp. text

RETURN "" && from procedure Surround

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: MESSAGE1()
*!
*! Calls: CENTER (procedure in PROC.PRG)
*!
*!*********************************************************************
FUNCTION message1

** from Miriam Liskin's Book
** Displays a centered message and pauses until user presses a key
** uses CENTER above
** changed to a function: 4/19/91 KJM

PARAMETERS ln_line,ln_width,lc_color,lc_text

@ln_line,0
DO CENTER WITH ln_line,ln_width,lc_color,lc_text
WAIT "" TO X
@ln_line,0

RETURN X && from procedure Message

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: MESSAGE2()
*!
*! Calls: SHADOW (procedure in PROC.PRG)
*! : CENTER (procedure in PROC.PRG)
*!
*!*********************************************************************
FUNCTION message2

** from Miriam Liskin's Book
** Displays a message in a window and pauses until user presses a key
** changed to a function: 4/19/91 KJM
** modified for shadow: 4/29/91 KJM

PARAMETERS lc_text

SAVE SCREEN TO sc_message
define WINDOW MESSAGE FROM 10,10 TO 14,70 DOUBLE COLOR &cl_warn.
DO shadow WITH 10,10,14,70
ACTIVATE WINDOW MESSAGE

DO CENTER WITH 1,60,"W+/B",lc_text
WAIT "" TO X

DEACTIVATE WINDOW MESSAGE
RELEASE WINDOW MESSAGE
RESTORE SCREEN FROM sc_message
RELEASE SCREEN sc_message

RETURN X && from Message2

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: MESSAGE3()
*!
*! Calls: SHADOW (procedure in PROC.PRG)
*!
*!*********************************************************************
FUNCTION message3

** From Miriam Liskin's Book
** displays message in a window and pauses until user presses a key
** changed to a function: 4/19/91 KJM
** modified to add shadow: 4/29/91 KJM

PARAMETERS lc_text

ln_lines = INT(LEN(lc_text) / 38) + 5 && set # of lines for window

SAVE SCREEN TO sc_message
define WINDOW MESSAGE FROM 8,20 TO 8+ln_lines,60 DOUBLE COLOR &cl_warn.
DO shadow WITH 8,20,8+ln_lines,60
ACTIVATE WINDOW MESSAGE

ln_lmargin = _LMARGIN
ln_rmargin = _RMARGIN
lc_alignment = _ALIGNMENT
ll_wrap = _WRAP

_LMARGIN = 1
_RMARGIN = 38
_ALIGNMENT = "CENTER"
_WRAP = .T.

?lc_text
?
WAIT " Press any key to continue . . ." TO X

_LMARGIN = ln_lmargin
_RMARGIN = ln_rmargin
_ALIGNMENT = lc_alignment
_WRAP = ll_wrap

DEACTIVATE WINDOW MESSAGE
RELEASE WINDOW MESSAGE
RESTORE SCREEN FROM sc_message
RELEASE SCREEN sc_message

RETURN X && from procedure Message3

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: MESSAGE4()
*!
*! Calls: SHADOW (procedure in PROC.PRG)
*! : CENTER (procedure in PROC.PRG)
*!
*!*********************************************************************
FUNCTION message4

** from Miriam Liskin's Book
** Display a message in a predefined window and pause
** changed to function: 4/19/91 KJM
** modified to add shadow: 4/29/91 KJM

PARAMETERS lc_text1,lc_text2

SAVE SCREEN TO sc_message
define WINDOW monitor FROM 10,10 TO 17,70 DOUBLE COLOR &cl_warn.
DO shadow WITH 10,10,17,70
ACTIVATE WINDOW monitor

ln_lmargin = _LMARGIN
ln_rmargin = _RMARGIN
ll_wrap = _WRAP

_LMARGIN = 1
_RMARGIN = 58
_WRAP = .T.

DO CENTER WITH 1,58,"",lc_text1
DO CENTER WITH 2,58,"",lc_text2
DO CENTER WITH 4,58,"","Press any key to continue . . ."
WAIT "" TO X

_LMARGIN = ln_lmargin
_RMARGIN = ln_rmargin
_WRAP = ll_wrap

DEACTIVATE WINDOW monitor
RELEASE WINDOW monitor
RESTORE SCREEN FROM sc_message
RELEASE SCREEN sc_message

RETURN X && from procedure MESSAGE4

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Procedure: MONITOR
*!
*! Calls: SHADOW (procedure in PROC.PRG)
*! : CENTER (procedure in PROC.PRG)
*!
*!*********************************************************************
PROCEDURE monitor

** taken from Miriam Liskin's Book
** display a status message to monitor a long-running operation
** user must specify in processing the record# to place in the
** box on line 4 . . . Must also deactivate window MONITOR and
** release it from MEMORY
** Modified to add shadow: 4/29/91 KJM

PARAMETERS lc_text

SAVE SCREEN TO sc_monitor
define WINDOW monitor FROM 10,10 TO 18,70 DOUBLE COLOR &cl_help.
DO shadow WITH 10,10,18,70
ACTIVATE WINDOW monitor

DO CENTER WITH 1,60,"",lc_text
DO CENTER WITH 2,60,"","Please do not interrupt"
@4,10 SAY "Working on record of " + LTRIM(STR(RECCOUNT(),5))

** remember to: DEACTIVATE WINDOW MONITOR
** RELEASE WINDOW MONITOR
** RESTORE SCREEN FROM SC_MONITOR
** RELEASE SCREEN SC_MONITOR

RETURN && from procedure MONITOR

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: SCRNHEAD()
*!
*!*********************************************************************
FUNCTION scrnhead

** taken from Miriam Liskin's Book
** Display a heading in a box 2 spaces wider than text with
** custom border (double line top, single line sides)

PARAMETERS lc_color,lc_text

lc_text = " "+TRIM(lc_text)+" " && ad spaces to left and right
lc_textstart = (80-LEN(TRIM(lc_text)))/2
@1,lc_textstart-1 TO 3,81-lc_textstart 205,196,179,179,213,184,192,217;
COLOR &lc_color. && display box
@2, lc_textstart SAY lc_text COLOR &lc_color. && display text

RETURN "" && from procedure scrnhead

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: YESNO()
*!
*! Calls: YES (procedure in PROC.PRG)
*! : NO (procedure in PROC.PRG)
*! : SHADOW (procedure in PROC.PRG)
*! : CENTER (procedure in PROC.PRG)
*!
*!*********************************************************************
FUNCTION yesno

** from Miriam Liskin's Book, changed to a function
** asks a Yes-No question in a dialog box!
** Modified to function: 4/19/91 KJM
** Modified to shadow window: 4/29/91 KJM

PARAMETER ll_answer,lc_mess1,lc_mess2,lc_mess3

SAVE SCREEN TO sc_yesno
define WINDOW yesno FROM 8,20 TO 16,60 DOUBLE COLOR &cl_help.

define MENU yesno
define PAD yes of yesno PROMPT "[Yes]" AT 5,10
define PAD no of yesno PROMPT "[No]" AT 5,25
ON SELECTION PAD yes of yesno DO yes
ON SELECTION PAD no of yesno DO no

DO shadow WITH 8,20,16,60
ACTIVATE WINDOW yesno
ln_lmargin = _LMARGIN && store system values
ln_rmargin = _RMARGIN
ll_wrap = _WRAP
_LMARGIN = 2 && set local values
_RMARGIN = 38
_WRAP = .T.

DO CENTER WITH 0,38,"",lc_mess1 && center the text
DO CENTER WITH 2,38,"",lc_mess2
DO CENTER WITH 3,38,"",lc_mess3
IF ll_answer
ACTIVATE MENU yesno PAD yes
ELSE
ACTIVATE MENU yesno PAD no
ENDIF

_LMARGIN = ln_lmargin && reset system values
_RMARGIN = ln_rmargin
_WRAP = ll_wrap

DEACTIVATE WINDOW yesno
RELEASE WINDOW yesno
RESTORE SCREEN FROM sc_yesno
RELEASE SCREEN sc_yesno
RELEASE MENU yesno

RETURN ll_answer && from procedure Yesno

*!*********************************************************************
*!
*! Procedure: YES
*!
*! Called by: YESNO() (function in PROC.PRG)
*!
*!*********************************************************************
PROCEDURE yes
ll_answer = .T.
DEACTIVATE MENU
RETURN

*!*********************************************************************
*!
*! Procedure: NO
*!
*! Called by: YESNO() (function in PROC.PRG)
*!
*!*********************************************************************
PROCEDURE no
ll_answer = .F.
DEACTIVATE MENU
RETURN

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: ERRORMSG()
*!
*! Calls: SHADOW (procedure in PROC.PRG)
*! : CENTER (procedure in PROC.PRG)
*!
*!*********************************************************************
FUNCTION errormsg && display a window describing an error
** this one's mine (Ken Mayer)

PARAMETERS lc_mess1,lc_mess2,lc_color

SAVE SCREEN TO sc_err
define WINDOW err FROM 8,20 TO 15,60 DOUBLE COLOR &lc_color.
DO shadow WITH 8,20,15,60
ACTIVATE WINDOW err

DO CENTER WITH 0,38,"","** ERROR **"
DO CENTER WITH 2,38,"",lc_mess1
DO CENTER WITH 3,38,"",lc_mess2
DO CENTER WITH 5,38,"","Press any key to continue ..."
lc_wait=INKEY(0)

DEACTIVATE WINDOW err
RELEASE WINDOW err
RESTORE SCREEN FROM sc_err
RELEASE SCREEN sc_err

RETURN lc_wait && from function ERRORMSG

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: DATETEXT()
*!
*!*********************************************************************
FUNCTION datetext

** stolen from Miriam Liskin's book
** displays date in text format (e.g., July 1, 1991)

PARAMETERS ld_date

RETURN CMONTH(ld_date)+" "+LTRIM(STR(DAY(ld_date),2))+", "+;
STR(YEAR(ld_date),4)

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: DATETXT2()
*!
*!*********************************************************************
FUNCTION datetxt2

** from Miriam Liskin's book
** displays date in text format (e.g., Monday, July 1, 1991)

PARAMETERS ld_date

RETURN CDOW(ld_date)+", "+CMONTH(ld_date)+" "+;
LTRIM(STR(DAY(ld_date),2))+", "+STR(YEAR(ld_date),4)

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: ISUNIQUE()
*!
*!*********************************************************************
FUNCTION isunique

** from Miriam Liskin, minor mods by Rick Price
** Used to determine if a keyfield is unique

PARAMETERS lc_var

ln_record = RECNO() && store current record number
ll_unique = .T. && init to true
lc_dbf = ALIAS() && store current alias, so we can return to it
SELECT dupcheck && second copy of database
GO TOP && ensure we're at the top ...

SEEK lc_var && make sure database is set to correct index here
LOCATE FOR keyfield = lc_var .AND. RECNO() <> ln_record REST
* ^^^^^^^^ *
* ========> MUST BE KEYFIELD IN DATABASE <========== *
IF FOUND()
ll_unique = .F.
ENDIF

SELECT (lc_dbf) && back to original copy of file

RETURN ll_unique && return value of that field

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: ALLTRIM()
*!
*!*********************************************************************
FUNCTION ALLTRIM && trim leading/trailing spaces

* Borrowed from Phil Steele's PCSDEMO.PRG (public domain)

PARAMETERS lc_string

RETURN LTRIM(RTRIM(lc_string)) && that's it.

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Procedure: SHADOW
*!
*! Called by: PRINTERR (procedure in PROC.PRG)
*! : MESSAGE2() (function in PROC.PRG)
*! : MESSAGE3() (function in PROC.PRG)
*! : MESSAGE4() (function in PROC.PRG)
*! : MONITOR (procedure in PROC.PRG)
*! : YESNO() (function in PROC.PRG)
*! : ERRORMSG() (function in PROC.PRG)
*!
*!*********************************************************************
PROCEDURE shadow && routine is taken from A-T routines generated by
** CCBooster to do a shadow on picklist ...

PARAMETERS ln_x1,ln_y1,ln_x2,ln_y2

ln_x0 = ln_x2+1
ln_y0 = ln_y2+2
ln_dx = 1
ln_dy = (ln_y2-ln_y1) / (ln_x2-ln_x1)
DO WHILE ln_x0 <> ln_x1 .OR. ln_y0 <> ln_y1+2
@ ln_x0,ln_y0 FILL TO ln_x2+1,ln_y2+2 COLOR N+/N
ln_x0 = IIF(ln_x0<>ln_x1,ln_x0 - ln_dx,ln_x0)
ln_y0 = IIF(ln_y0<>ln_y1+2,ln_y0 - ln_dy,ln_y0)
ln_y0 = IIF(ln_y0 ENDDO

RETURN && from procedure SHADOW

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Procedure: FULLWIN
*!
*!*********************************************************************
PROCEDURE fullwin && just a small routine to setup a window overlaying
** the main/calling screen ... completely (except
** for row 24, which is used for messages)

PARAMETERS lc_color,lc_winname,sc_screen

define WINDOW &lc_winname FROM 0,0 TO 23,79 NONE COLOR &lc_color.
SAVE SCREEN TO &sc_screen.
ACTIVATE WINDOW &lc_winname.

* see documentation at beginning of procedure file about releasing
* and deactivating things ...

RETURN && from procedure fullwin

*---------------------------------------------------------------------

*!*********************************************************************
*!
*! Function: DOSRUN()
*!
*!*********************************************************************
FUNCTION dosrun && function created by Micheal P. Dean, posted on the
** Ashton-Tate BBS, used to avoid problems with active
** windows and DOS commands executed from within progs.

PARAMETER lc_cmd

lc_window = WINDOW()
IF LEN(TRIM(lc_window)) > 0
DEACTIVATE WINDOW &lc_window
ENDIF
SET CONSOLE OFF
RUN &lc_cmd
SET CONSOLE ON
IF LEN(TRIM(lc_window)) > 0
ACTIVATE WINDOW &lc_window
ENDIF

RETURN "" && from function DOSRUN

*****************************
*!*********************************************************************
*!
*! Procedure: RCD_LIST
*!
*! Calls: FULDSP (procedure in PROC.PRG)
*!
*!*********************************************************************
PROCEDURE rcd_list
*****************************
************************************************************
*
* set escape off && ESCAPE MUST be off
*
* Be sure the data base and index is active befire calling
* USE STUDENT INDEX STNAME
*
* This procedure will probably be called from a UDF as follows:
* @1,1 get ans VALID Check
* Then this Check UDF calls the procedure rcd_list. When get return
* from rcd_list, can @ x,x say varable or use keyboard stuff
*********************************************************************
* Either title_1 or title_2 Must be as big as
* total length of fields to be displayed including
* spaces added as field separators (separator = ', 'in fld= below)
* EXAMPLE:
* title_1 = "Student Data Base" && 1st title line
* title_2 = "<------Last Name-------><---First Name-->" && 2nd title line
*
* FLD contains fields to display. The file MUST be indexed by these fields
* NOTE how the ' is used within the "
* EXAMPLE:
* fld="last+' '+first"
*
* scrnmax=22 && # of lines to use for window-2 lines of this used by box
* ok_to_srch= .T. && .T. allows alpha search. IF .F. search is numeric
*
* srow=0 && SETS THE TOP ROW OF THE DISPLAY
* scol=0 && SETS THE LEFT COLUMN OF THE DISPLAY
*
* do rcd_list && Execute the procedure
*
* Use PgDn, PgUp to display next or prior screen, CtlHome or CtlEnd to go
* to first or last screen. Home and End position the pointer to the
* top or bottom of the screen. If the database is indexed properly, you
* can enter keyboard keys to search for records.
* If you press ESC, the database is sent to EOF, If you use ENTER, the
* database is positioned to the record at the cursor of the list. When you
* exit the rcd_list test for these conditions and also have the record.
*****************************************************************

PRIVATE frec,skpr
SAVE SCREEN TO rcd_list
fldtp=''
GOTO BOTTOM
lrec = RECNO() && set lrec to last record number
GOTO TOP && set to first record before calling rcd_list
color_stat=SET("ATTRIBUTES")
SET CURSOR OFF
frec=RECNO() && ASSUMES DATABASE HAS BEEN SET TO 1ST RECORD
fldlen=MAX(LEN(title_1),LEN(title_2))
ecol=fldlen+1+scol && SETS THE RIGHT COLUMN OF THE DISPLAY
erow=scrnmax && SET THE BOTTOM ROW OF THE DISPLAY
SET COLOR TO W+/N
@ srow,scol CLEAR TO erow,ecol
@ srow,scol TO erow,ecol DOUBLE
IF erow @srow+1,ecol+1 FILL TO erow,ecol+2 COLOR W/N
@erow+1,scol+2 FILL TO erow+1,ecol+2 COLOR W/N
ENDIF
trow=srow+1
tcol=scol+1
bcol=ecol-1
b_row=erow-1
IF LEN(TRIM(title_1))>0 && IF THERE IS A TITLE_1
IF LEN(title_1) @ trow,tcol SAY title_1 PICTURE "@I "+REPLICATE("X",fldlen) COLOR GR+/R
ELSE
@ trow,tcol SAY title_1 COLOR GR+/R
ENDIF
trow=trow+1
ENDIF && IF THERE IS A TITLE_2
IF LEN(TRIM(title_2))>0
@ trow,tcol SAY title_2 COLOR GR+/N
trow=trow+1
ENDIF
wndht=b_row-trow+1 && SETS THE # ROWS OF DISPLAYED RECORDS
STORE trow TO lrow,scrnrow && INITIALIZE 1ST ROW TO DISPLAY RECORD
first_page=.T.
last_page=.F.
GOTO frec
DO fuldsp WITH 0
STORE '' TO bid,skey
DO WHILE .T.
CLEAR TYPEAHEAD
STORE 0 TO xkey
DO WHILE xkey = 0
xkey=INKEY()
ENDDO
IF ok_to_srch && ALLOWS ALPHA SEARCH ON DATABASE
skey=UPPER(CHR(xkey))
IF (skey>='A' .AND. skey<='Z') .OR. (skey>='0' .AND. skey<='9')
bid=bid+skey
orgrec=RECNO()
IF .NOT. SEEK(LTRIM(fldtp+bid))
bid=SUBS(bid,1,LEN(bid)-1)
GOTO orgrec
ELSE
DO fuldsp WITH 0
ENDIF
LOOP
ELSE
bid=""
ENDIF
ENDIF
DO CASE
CASE xkey=27 .OR. xkey=13 && ESCAPE or RETURN
IF xkey=27

SET FILTER TO
SET RELATION TO
GOTO BOTTOM
SKIP 1
ENDIF
EXIT
CASE xkey=5 && UP ARROW
@ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/N
scrnrow=IIF(scrnrow>trow,scrnrow-1,lrow)
@ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/BG
LOOP
CASE xkey=24 && DOWN ARROW
@ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/N
scrnrow=IIF(scrnrow @ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/BG
LOOP
CASE xkey=3 && PgDn
IF last_page
@ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/N
scrnrow=lrow
@ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/BG
ELSE
DO fuldsp WITH wndht
ENDIF
LOOP
CASE xkey=18 && PgUp
IF first_page
@ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/N
scrnrow=trow
@ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/BG
ELSE
DO fuldsp WITH -wndht
ENDIF
LOOP
CASE xkey=26 && HOME
IF scrnrow>trow
@ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/N
scrnrow=trow
@ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/BG
ENDIF
LOOP
CASE xkey=2 && END
IF scrnrow @ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/N
scrnrow=lrow
@ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/BG
ENDIF
LOOP
CASE xkey=29 && CTRL HOME
IF first_page
@ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/N
scrnrow=trow
@ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/BG
ELSE
GOTO frec
DO fuldsp WITH 0
ENDIF
LOOP
CASE xkey=23 && CTRL END
IF last_page
@ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/N
scrnrow=lrow
@ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/BG
ELSE
GOTO lrec
SKIP 1-wndht
DO fuldsp WITH 0
ENDIF
ENDCASE
ENDDO
skpr=scrnrow-trow
IF skpr<>0 .AND. xkey<>27 && GOTO SELECTED RECORD IF ESC NOT PRESSED
SKIP skpr
ENDIF
SET COLOR TO &color_stat
RESTORE SCREEN FROM rcd_list
RELEASE SCREEN rcd_list
SET CURSOR ON
RELEASE color_stat,srow,scol,erow,ecol,trow,tcol,b_row,wndht,lrow,scrnrow
RELEASE first_page,last_page,frec,bid,skey,xkey,orgrec,skpr
RETURN

*!*********************************************************************
*!
*! Procedure: FULDSP
*!
*! Called by: RCD_LIST (procedure in PROC.PRG)
*!
*!*********************************************************************
PROCEDURE fuldsp
PARAMETER skpr
@ trow,tcol CLEAR TO b_row,bcol
IF skpr<>0
SKIP skpr
ENDIF
trec=RECNO()
lrow=trow-1
first_page=trec=frec
last_page=.F.
SCAN REST WHILE lrow lrow=lrow+1
last_page=RECNO()=lrec && STOPS LISTING RECORDS WHEN LAST RECORD REACHED
@ lrow,tcol SAY &fld && DISPLAY RECORD AS DEFINED BY fld
ENDSCAN
GOTO trec
scrnrow=trow
@ scrnrow,tcol FILL TO scrnrow,bcol COLOR W+/BG
RETURN



*---------------------------------------------------------------------
* End of procedure File
*---------------------------------------------------------------------
*: EOF: PROC.PRG