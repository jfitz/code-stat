* EV_maint.PRG
module = [EV]
proc_keys = [CFP]

wtitle = [ ÄÄDateÄÄÄÂÄWhomÄÂÄÄÄÄÄÄÄEventÄÄÄÄÄÄÄÄÄÄ]
wfields = [DTOC(date)+' ³ '+whom+' ³ '+event]
mtitle = [Schedules]

DO whom
DO events
browmenu = [+Copy]
mprompt = ' / for menu, [F10] for calendar'

PRIVATE last_date
last_date = DATE()

SOFT(DATE()) && get close to today
SET KEY -9 TO cal_list && see below

DO browse WITH 6,2,20,44

SET KEY -9 TO

CLOSE DATA
RETURN


PROC ev_gets
PARAM mrow
IF adding()
M->date = M->last_date
ENDIF
@ mrow,left_col+4 GET M->date VALID ! EMPTY(M->date)
@ mrow,col()+3 GET M->whom PICT [@!]
@ mrow,col()+4 GET M->event
RETURN

PROC ev_F
@ 24,0
IF INDEXORD() = 1
IF yes_no([Switch to BY WHOM index?])
SET ORDER TO 2
ENDIF
ELSE
IF yes_no([Switch to DATE index?])
SET ORDER TO 1
ENDIF
ENDIF

@ 24,0
IF INDEXORD() = 1
M->date = CTOD([])
@ 24,0 SAY [Find ] GET M->date
READ
soft( DTOS(M->date) )
ELSE
M->date = CTOD([])
M->whom = SPAC(LEN(whom))
@ 24,0 SAY [Find ] GET M->whom PICT [@!]
@ 24,COL() SAY [ starting at ] GET M->date
READ
soft(M->whom+DTOS(M->date) )
ENDIF

mloop = .F.
RETURN

PROC ev_P && Print
oldrec = RECNO()
M->thru = LDOM(LDOM(DATE())+1)
BEGIN SEQUENCE
@ 24,0
@ 24,0 SAY [Print calendar for month ending ] GET M->thru
READ
ESCBREAK([leave open])

DO printit
ESCBREAK([leave open])
DO cal_rep1 WITH [] ,;
[] ,;
m->thru ,;
[date] ,;
[Company Calendar for]

END SEQUENCE
DO closeit
GOTO oldrec
mloop = .F.
RETURN


PROC ev_C && Copy
PRIVATE i,mfrom,mthru
BEGIN SEQUENCE
STORE events->date+1 TO mfrom, mthru
@ 24,0
@ 24,0 SAY [Copy to ] GET M->mfrom VALID ! EMPTY(mfrom)
@ 24,COL() SAY [ thru ] GET M->mthru VALID mthru >= mfrom
READ
ESCBREAK([leave open])

M->date = events->date
M->whom = events->whom
M->event = events->event
FOR i = M->mfrom TO M->mthru
IF add_rec(5)
REPLA date WITH i ,;
whom WITH M->whom ,;
event WITH M->event
UNLOCK
ENDIF
NEXT i

END SEQUENCE
mloop = .F.
RETURN



PROC cal_list && pops-up a calendar, and uses date as the group
* Returns to your browse program on ESC, Enter, A)dd, or E)dit.
* We might be able to infer the field name from indexkey(0).
PRIVATE _date, cal[7*6], colnames[7],tab[7],i,group,group_key,date_field
AFILL(cal,[])
colnames[1] = [Sun]
colnames[2] = [Mon]
colnames[3] = [Tue]
colnames[4] = [Wed]
colnames[5] = [Thu]
colnames[6] = [Fri]
colnames[7] = [Sat]

tab[1] = 50
FOR i = 2 TO 7
tab[i] = tab[i-1] + 4
NEXT i

*-----Parse the group_key from the INDEXKEY(0), up to the end of DTOS()
_index_exp = INDEXKEY(0)
IF ! [DTOS] $ _index_exp
BREAK
ENDIF
_date_start = AT([DTOS(], _index_exp) + 5
IF _date_start > 6 && part of the index is left of the date
_left_exp = SUBS(_index_exp, 1, _date_start - 7)
_big_group = &_left_exp
ELSE
_big_group = []
ENDIF
_date_len = AT([)], SUBS(_index_exp, _date_start)) - 1
_date_field = SUBS(_index_exp, _date_start, _date_len) && from the database
group_key = SUBS(_index_exp, 1, _date_start + _date_len)
_date = &_date_field && a field from the database

PRIVATE key, first_date, cal_screen

BEGIN SEQUENCE
*--------display the month box-----------------------------------
line24 = SAVESCR(24,0,24,79) && save line 24
cal_screen = BOX(12,tab[1]-2,7,29,(CMON(_date)+[ ]+STR(YEAR(_date),4,0)), .T.)
FOR i = 1 TO 7
@ 15,tab[i] SAY colnames[i]
NEXT i
@ 24,0
@ 24,0 SAY [Use arrows to move within the month, Page to other months.]


DO WHIL .T. && while they stay in calendar help
*----------display the current month name------------------------
@13,tab[1] SAY CENTER( CMON(_date)+[ ]+STR(YEAR(_date),4,0), 28)
@16,tab[1] CLEAR TO 21,tab[7]+2

FOR i = (M->_date - DAY(_date)+ 1 ) TO LDOM(M->_date)
@ 15+_WEEK(i),tab[DOW(i)] SAY DAY(i) PICT [99]
NEXT i
first_date = _date - DAY(_date) + 1

DO WHIL .T.
*------list just the current date in browse window----------------
group = M->_big_group + DTOS(M->_date)
TOP()
DO list_em

* say the current date in another color:
CO_PUSH()
CO_CHG(c_pop3,c_text)
@ 15+_WEEK(_date),tab[DOW(_date)] SAY DAY(_date) PICT [99]
CO_POP()

M->key = INKEY(0) && wait for a key
@ 15+_WEEK(_date),tab[DOW(_date)] SAY DAY(_date) PICT [99]
M->char = UPPER(CHR(M->key))

DO CASE
CASE M->key = 13 .OR. M->char = [E] .OR. M->key = 27
@ cur_row,left_col+1 CLEAR TO cur_row,right_col-1
@ cur_row,left_col+4 SAY &wfields
M->white = ! white
DO rev_line
BREAK

CASE M->key = 27 && escape
BREAK

CASE M->char = [A]
M->white = ! white
KEYBOARD [A]
BREAK

CASE M->key = 24 && down
_date = _date + 7

CASE M->key = 5 && up
_date = _date - 7

CASE M->key = 4 && right
_date = _date + 1

CASE M->key = 19 && left
_date = _date - 1

CASE M->key = 1 && home
_date = _date - DAY(_date) + 1

CASE M->key = 6 && end - go to the end in any direction
@ 24,77 SAY [END]
key = INKEY(0)
DO CASE
CASE M->key = 24 && down
_date = _date + 7 * (_WEEK(LDOM(_date)) - _WEEK(_date))

CASE M->key = 5 && up
_date = _date + 7 * (1- _WEEK(_date))

CASE M->key = 4 && right
_date = _date + 7 - DOW(_date)

CASE M->key = 19 && left
_date = _date - DOW(_date) + 1

CASE M->key = 6 .OR. M->key = 1 && end or home
_date = LDOM(_date)
ENDCASE
@ 24,77 SAY [ ]

CASE M->key = 3 && pagedown -- same day of next month
_date = ADDMON(_date,1)

CASE M->key = 18 && pageup -- same day of prior month
_date = ADDMON(_date,-1)
ENDCASE

IF _date > LDOM(M->first_date) .OR. _date < M->first_date
EXIT && have moved into another month
ENDIF
ENDDO
* want to display another month
ENDDO

END SEQUENCE
*CO_POP()
RESTSCR(cal_screen)
RESTSCR(line24)
last_date = m->_date
RETURN
*** eof ***

