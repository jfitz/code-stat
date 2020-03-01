set function 4 to dtoc(date())
set function 8 to 'BLANK;'
set function 10 to 'modi comm '
* set escape off
SELECT 1
set talk off
PUBLIC 
M_A,M_B,M_C,M_D,M_E,M_F,M_G,M_H,M_I,M_J,M_K,M_L,M_M,M_N,M_O,M_P,M_Q,M_R,M_S,M_T,M_U,M_V,M_W,M_X,M_Y,M_Z,DEF_COLOR
public form,f4
public last_file
PUBLIC M_FILE,NT,ACT,TALLY,eb,END,KF,KL
xA=''
yA=0
kl='NONE'
kf='NONE'
kk='NONE'
se=''
TEM=0
NT=''
ACT=2
do &fl_na
DO DQ010 
GO BOTTOM
count all to END
do while yA<10
clear
set color to 1/15
@1,0 say ' DataQuick File Manager Menu'
@1,50 say space(22)
@1,72 say date()
set color to &def_color
text


[1] Modify - Edit/Change/View One or More Records
[2] List - List records by specific parameters
[3] Append - Add new records
[4] Browse - Scan through all records and fields
[5] Print - Print lists or labels
[6] Delete - Remove Files from Databases
[7] Quit - Exit this Database
[8] Utilities - Redefine Database, Screen, Reports, Etc.
[?] Help
endtext
@22,(len(trim(dn_na))+18)/2 say 'FILE BEING EDITED:'
@22,col() say dn_na
if bof() .and. eof()
set color to 131/0
@7,10 say '[3] Append - Add new records'
set color to &def_color
@20,10 say 'Press Any Key to Begin Adding Records...'
wait ' '
xa='3'
else
wait '' to xA
endif
if xa='0'
clear
text
YOU HAVE JUST HALTED THE ALUMNI DATABASE PROGRAM

To return to the program...Press [F5]

endtext
cancel
endif
if xa='?'
do DQ100
endif
yA=val(xA)
clear
IF YA=1
DO DQ110
ENDIF
if ya=2
clear
@0,22 say 'PARAMETERS AND FIELD NAMES'
DO &FL_NA
do DQ120
endif
if yA=3
do DQ130 
endif
if ya=4
browse
endif
if yA=5
do DQ150 
endif
if ya=6
do DQ160 
endif
if yA=7
wait 'Press any key to Exit this Database or [A] to abort ' to
en
do case
case upper(en)='A'
endif
otherwise
ya=10
endcase
endif
if ya=8
do DQ180 
endif
enddo

