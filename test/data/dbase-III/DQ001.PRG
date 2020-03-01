public fl_na,dn_na
set safety off
set talk off
set function 5 to 'modi comm '
set function 6 to 'master;'
ok=""
clear
fl=space(8)
dn=space(20)
dor=1
do while dor>0
@2,5 say 'Enter The Name of This Database (Up to 8 Characters NO 
Extension)' 
@3,33 get fl
read
fl=trim(fl)
fltest='"'+fl+'.dbf"'
fltes2='"'+fl+'.prg"'
if file(&fltest) .or. file(&fltes2) .or. len(fl)<2
? ' File Name Already Exists or Is Illegal...Enter Another'
fl=space(8)
dor=1
endif
if at(' ',fl)>0
fl=space(8)
dor=1
else
dor=0
endif
enddo
@5,15 say 'The Name of This File will be '
fl_na=upper(substr(fl,1,8))
@5,col()+1 say fl_na
@5,col() say '.dbf'
@7,5 say 'Enter The Descriptive Name for this Database (Up to 20 
Characters)' 
@8,29 get dn
read
dn_na=substr(dn,1,20)
@10,14 say 'The Descriptive Name will be '
@10,col() say dn_na
wait ' Is this okay??? ' to ok
do case
case upper(ok)='Y'
zz=1
otherwise
zz=0
endcase
if zz=0
return to master
endif
CLEAR
SELECT 1
USE DQ1002
ZAP
rec=1
eyn=.y.
do while eyn
clear
@1,0 say 'Enter Another Field???'
@1,col()+1 get eyn
read
if eyn
clear
append blank
dor=1
do while dor>0
@0,0 SAY 'DataQuik Database Creator'
@3,0 say 'Working on Field Number'
@3,col() say rec
@5,0 say 'Define the Input Prompt That will Appear During Data
Entry [0] to end'
@9,0 say 'Data Type: [C]haracter [N]umeric [L]ogical [D]ate'
@6,0 get input_prom
te=' '
@9,71 get te
read
replace next 1 type with upper(te)
@9,71 say type
do case
case type='C' 
dor=0
cl=254
@11,0 say 'How Long Will This Field Be:'
@11,col()+1 get length range 1,cl
read
replace next 1 decimal with 0
case type='N' 
dor=0
cl=15
@11,0 say 'How Long Will This Field Be: How Many 
Decimal Places:'
@11,30 get length range 2,cl
read
cl=length-1
@11,65 get decimal range 0,cl
read
case type='L'
dor=0
replace next 1 length with 1,decimal with 0
case type='D' 
dor=0
replace next 1 length with 8,decimal with 0
otherwise
dor=1
endcase
enddo
@13,0 say 'Will this field be used for INDEXING???'
@13,col() get index
read
replace next 1 field with chr(rec+64)
endif
REC=REC+1
enddo
select 4
use DQ1004 
zap
select 1
go top
do while .not. eof()
select 1
a_t=field
b_t=length
c_t=upper(type)
d_t=decimal
select 4
if len(trim(a_t))>0
append blank
replace next 1 field_name with a_t,field_type with c_t,field_len 
with b_t,field_dec with d_t
endif
select 1
skip
enddo
public ud,lr,ll,x
select 2
use DQ1003 
ZAP
select 1
use DQ1002
go bottom
rec=recno()
rcd=1
set function 7 to 'scedit;'
clear
ent=''
csr=''
x='xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxx'
set color to 0/15
@22,0 clear
@0,40 SAY '****RESERVED****'
@22,23 say 'This Space Reserved for Instructions'
prompt='Use Numeric keys to position [T] to Enter Text [R] to 
Record [E] to end'
@24,2 say prompt
set color to 15/0
ud=10
lr=39
go 1
csr=trim(input_prom)+substr(x,1,length)
@ud,lr say csr
do while upper(ent)<>'E' 
@23,20 say ''
wait '' to ent
@ud,lr say space(len(csr))
do case
case ent='8' .and. ud>0
ud=ud-1
case ent='2' .and. ud<21
ud=ud+1
case ent='4' .and. lr>0
lr=lr-1
case ent='6' .and. lr<79
lr=lr+1
case ent='7' 
ud=0
lr=0 
case ent='9'
ud=0 
lr=79
case ent='1'
ud=21
lr=0 
case ent='3'
ud=21 
lr=79
case ent='5'
ud=10
lr=40
case upper(ent)='M'
browse
case upper(ent)='R'
? chr(7)
f1=rcd
f2=input_prom
f3=field
select 2
append blank
replace next 1 prom with f2,l_r with lr,u_d with 
ud,file with f1,field_n with f3
go bottom
r1=recno()
go top
r=1
do while r select 2
go r
udt=u_d
lrt=l_r
@udt,lrt say prom
le=len(trim(prom))
fle=file
select 1
go fle
@udt,lrt+le say substr(x,1,length)
r=r+1
enddo
@0,40 say '****RESERVED****'
select 1
ud=21
lr=0
case upper(ent)='T'
set color to 0/15
@23,0 clear
@23,0 say '--------------------------------- Enter Text Line 
-----------------------------'
accept '' to tl 
if len(tl)>1
append blank
replace next 1 input_prom with tl,length with 0
rcd=recno()
rec=rec+1
csr=tl
endif
@23,2 say prompt
set color to 15/0
case ent='+' .and. rcd rcd=rcd+1
go rcd
if field<>' '
ll=length
else
ll=0
endif
csr=trim(input_prom)+substr(x,1,ll)
case ent='-' .and. rcd>1
rcd=rcd-1
go rcd
if field<>' '
ll=length
else
ll=0
endif
csr=trim(input_prom)+substr(x,1,ll)
endcase
@ud,lr say csr 
enddo
PUBLIC AA,AB,FF,st2,LENN,LEND,LENT
li=''
do while len(li)<17
li=li+chr(205)
enddo
set color to 0/15
@10,30 say chr(201)+li+chr(187)
@11,30 say chr(186)+'Creating Database'+chr(186)
@12,30 say chr(186)+' Please Wait '+chr(186)
@13,30 say chr(200)+li+chr(188)
select 3
use DQ1001 
zap
select 2
GO BOTTOM
RN=RECNO()
go top
REC=RECNO()
do while REC SELECT 2
GO REC
FLN1=FILE
SELECT 1
GO FLN1
LENN=LENGTH
LEND=DECIMAL
LENT=TYPE
SELECT 2
L1=L_R
U1=U_D
NU='999999999999999999999'
IF UPPER(LENT)='N'
DO CASE
CASE LEND>0
PIC=' PICTURE "'+SUBSTR(NU,1,LENN-(LEND+1))+'.'+SUBSTR(NU,1,LEND)+'"'
OTHERWISE
PIC=' PICTURE "'+SUBSTR(NU,1,LENN)+'"'
ENDcase
ELSE
PIC=''
ENDIF
if substr(prom,1,1)='+'
AA='@'+str(u1,2)+','+str(l1,2)+' SAY '+substr(trim(prom),2,len(trim(prom)))
else
AA='@'+str(U1,2)+','+str(L1,2)+' SAY '+"'"+TRIM(PROM)+"'"
endif
ab='@'+str(U1,2)+','+str(L1+len(trim(prom)),2)+' get m_'+field_n+PIC
FF=LEN(TRIM(FIELD_N))
select 3
append blank
replace next 1 format with aa
IF FF>0
append blank
replace next 1 format with AB
ENDIF
REC=REC+1
ENDDO
SELECT 3
fl1=fl_na+'.fmt'
fl2=fl_na
FL3=FL_NA+'.PRG'
COPY TO &fl1 ALL FIELDS FORMAT DELIMITED WITH BLANK
zap
SET TALK OFF
append blank
replace next 1 format with 'CLEAR'
append blank
replace next 1 format with '@0,22 say "PARAMETERS AND FIELD 
NAMES"'
SELECT 1
GO BOTTOM
X1=RECNO()+1
GO TOP
X2=1
gg=2
@12,30 say chr(186)+'Creating Programs'+chr(186)
@13,30 say chr(186)+' Please Wait '+chr(186)
@14,30 say chr(200)+li+chr(188)
DO WHILE X2 X3=1
ST1=''
DO WHILE X3<4 .AND. X2 GO X2
IF LEN(TRIM(FIELD))>0
ST='['+TRIM(FIELD)+'] '+SUBST(INPUT_PROM,1,20)+' '
last_f=field
ST1=ST1+ST
X3=X3+1
ENDIF
X2=X2+1
ENDDO
gg=gg+1
st2='@'+str(gg,2)+',1 say "'+st1+'"'
select 3
append blank
replace next 1 format with st2
SELECT 1
ENDDO
SELECT 3
APPEND BLANK
REPLACE NEXT 1 FORMAT WITH '@18,1 SAY "[1] CLEAR ENTRIES 
[2] ABORT [0] ACCEPT AND CONTINUE"'
APPEND BLANK
REPLACE FORMAT WITH 'LAST_FILE="'+trim(last_f)+'"'
public xvar,xas,st5,adx
select 3
copy to &fl3 all fields format DELIMITED WITH BLANK
select 1
go bottom
rcd=recno()+1
rc=1
ndx=''
do while rc go rc
if index
do case
case upper(type)='C'
ndx=ndx+trim(field)+'+'
endcase
endif
rc=rc+1
enddo
adx=subst(ndx,1,len(ndx)-1)
select 5
use dqfiles
append blank

replace next 1 file_na with fl_na,desc_na with dn_na,ndx_key with adx
CLOSE DATABASES
CREATE &FL2 FROM DQ1004.dbf
use &fl2
if len(adx)>0
index on &adx to &fl2 
else
index on recno() to &fl2
endif
set color to 15/0
xy=1
ent='Y'
do while xy<4
clear
@0,0 say 'REPORT CREATE MENU'
@2,0 say ''
wait 'Would You Like To Design Report Forms for This Database 
(Y/N) ' to ent
file_n=space(8)
desc_n=space(20)
if upper(ent)='Y'
@5,20 say 'This Report File will Be Titled: '
if len(fl_na)=8
zz=7
else
zz=len(fl_na)
endif
file_n=substr(fl_na,1,zz)+str(xy,1)
@5,col() say file_n+'.rpt' 
@6,20 say 'Descriptive Name '
@6,col() get Desc_n
read
use &fl_na
modify report &file_n
fr_n='for_f'+str(xy,1)
dfr='df'+str(xy,1)
xy=xy+1
use dqfiles
go bottom
replace next 1 &fr_n with file_n,&dfr with desc_n
else
xy=4 
endif
enddo
xy=1
ent='Y'
do while xy<4
clear
@0,0 say 'LABEL CREATE MENU'
@2,0 say ''
wait 'Would You Like To Design Label Forms for This Database (Y/N) ' to ent
file_n=space(8)
desc_n=space(20)
if upper(ent)='Y'
@5,20 say 'This Label File will Be Titled: '
if len(fl_na)=8
zz=7
else
zz=len(fl_na)
endif
file_n=substr(fl_na,1,zz)+str(xy,1)
@5,col() say file_n+'.lbl' 
@6,20 say 'Descriptive Name '
@6,col() get Desc_n
read
use &fl_na
modify label &file_n
fr_n='lbl_f'+str(xy,1)
dfr='dl'+str(xy,1)
xy=xy+1
use dqfiles
go bottom
replace next 1 &fr_n with file_n,&dfr with desc_n
else
xy=4 
endif
enddo

