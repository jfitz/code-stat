* cal_rep.prg
IF DAY(DATE()) > 25
M->from = ldom(ldom(DATE())+1)
ELSE
M->from = ldom(DATE())
ENDIF

DO BOX WITH 7,6,12,65,[Calendar Report],.F.
@12,10 SAY [Month ending ] GET M->from
@24,0
READ
escbreak()

DO printit
escbreak()

DO events

DO cal_rep1 WITH [] ,;
[] ,;
m->from ,;
[date] ,;
[Company Calendar for]

DO closeIt
CLOSE DATA
RETURN
*** EOF ***


PROC cal_rep1
PRIVATE group_key,group
PARAM group_key,group,mdate,date_field,rep_title1
mdate = LDOM(mdate)

PRIVATE tab[8], break_bars, mid_bars, top_bars, bot_bars, day_width
day_width = 25
top_bars = [Ú] + REPLIC(REPLIC([Ä],day_width-1)+[Â],6) + REPLIC([Ä],day_width-1)+[¿]
break_bars = [Ã] + REPLIC(REPLIC([Ä],day_width-1)+[Å],6) + REPLIC([Ä],day_width-1)+[´]
mid_bars = [³] + REPLIC(REPLIC([ ],day_width-1)+[³],7)
bot_bars = [À] + REPLIC(REPLIC([Ä],day_width-1)+[Á],6) + REPLIC([Ä],day_width-1)+[Ù]

* shading for weekends:
mid_bars = [³] + REPLIC([°], M->day_width-1) + [³] ;
+ REPLIC(REPLIC([ ],day_width-1)+[³],5) ;
+ REPLIC([°], M->day_width-1) + [³]

tab[1] = 0
FOR i = 2 TO 8
tab[i] = tab[i-1] + M->day_width
NEXT i

week_width = 132 && sideways at 12 CPI
*------------start the report----------------------------------------
?? sideways
rep_title1 = rep_title1 + [ ] + CMON(mdate)+STR(YEAR(mdate),5,0)
@ PROW()+0,1 SAY CENTER(M->rep_title1, week_width)
?? condense

*----------print top bar and day names-----------------------------
@ PROW()+3,tab[1] SAY top_bars
@ PROW()+1,tab[1] SAY mid_bars
FOR i = 1 TO 7
@ PROW(),tab[i] SAY CENTER(CDOW(CTOD([01/01/1989])+i-1),M->day_width)
@ PROW(),tab[i]+1 SAY REPLIC([°], M->day_width-1) && shading
NEXT i


first_day = mdate - DAY(mdate) + 1 && first day of month
this_date = first_day
PRIVATE day_nums[7], dates[7], line1[7], line2[7]

DO WHIL this_date <= mdate
AFILL(day_nums ,0)
print_day = DAY(this_date)
FOR i = MAX(1,DOW(this_date)) TO 7
day_nums[i] = IF(print_day <= DAY(Mdate),print_day,0)
print_day = print_day + 1
NEXT i
*------------------- print the whole week of day numbers-------------
@ PROW()+1,tab[1] SAY break_bars

@ PROW()+1,tab[1] SAY mid_bars
FOR i = 1 TO 7
@ PROW(),tab[i]+1 SAY day_nums[i] PICT [@Z 99]
NEXT i
@ PROW()+1,tab[1] SAY mid_bars && a blank line


*----------fill array of seven dates of the week---------------------
AFILL(dates ,[])
this_week = .T.
DO WHIL this_week .AND. this_date <= mdate
dates[DOW(this_date)] = this_date
this_date = this_date + 1
this_week = (DOW(this_date) > 1)
ENDDO

*--------print the data------------------------------------------------
more_lines = .T.
line_num = 0
DO WHIL more_lines && no restriction on line numbers
more_lines = .F.
line_num = line_num + 1
AFILL(line1,[])
AFILL(line2,[])
FOR i = 1 TO 7
IF ! EMPTY(dates[i])
SEEK M->group + DTOS(dates[i])
SKIP (line_num - 1)
IF &date_field = dates[i] .AND. ! OFF()
more_lines = .T.
line1[i] = TRIM(whom)+ [ ] + event
ENDIF
ENDIF
NEXT i
@ PROW()+1,tab[1] SAY mid_bars
FOR i = 1 TO 7
@ PROW(),tab[i]+1 SAY line1[i]
NEXT i
ENDDO

* blank lines up to 4 per week:
FOR i = line_num TO 4
@ PROW()+1,tab[1] SAY mid_bars
NEXT i

ENDDO

*---------print bottom bars at end of month
@ PROW()+1,tab[1] SAY bot_bars
RETURN



