SET FORMAT TO
CLEAR
DO DQ010 
act=0
f1=fl_na+'.fmt'
@0,60 say 'Record#'
@0,70 get m_file
do &f1
@22,0 SAY '[CRSR]-Move around [PG U/D]-Accept entry [F8]-Empty fields 
[<>] Not Equal to'
READ
NT=''
CH2=65
@22,0 clear
@22,0 say 'Setting Up Search...'
if m_file>0 .and. m_file act=1
ch2=90
else 
chk=CH2
do while chk if at(chr(chk),trim(indx))>0
chc='M_'+chr(chk)
che=chr(chk)
do case
case type('&che')='C' 
IF ASC(trim(&chc))>0 
nt=nt+' .and. '+che+'="'+TRIM(&chc)+'"'
act=3
chK=90
endIF 
case type('&che')='N' 
IF &chc<>0 
nt=nt+' .and. '+che+'='+str(&chc)
act=3
chK=90
endIF 
endcase
endif
chk=chk+1
enddo
SET TALK OFF
c1=' .and. '
chk=65 
do while chk3
@row(),col() say '.'
chc='M_'+chr(chk)
cht=chr(chk)
if type('&cht')='C'
do case
case substr(&chc,1,2)='<>'
c2='<>"'
&chc=substr(&chc,3,len(&chc))
otherwise
c2='="'
endcase
endif
do case
case type('&cht')='C' 
if asc(trim(&chc))>0 
nt=nt+c1+chr(chk)+c2+trim(&chc)+'"'
act=2
endif
case type('&cht')='N' 
if &chc<>0 
nt=nt+c1+chr(chk)+'='+str(&chc)
act=2
endif
case type('&cht')='L' 
do case
case upper(&chc)='T' .or. upper(&chc)='Y'
nt=nt+c1+chr(chk)
act=2
case upper(&chc)='F' .or. upper(&chc)='N'
nt=nt+c1+' .not. '+chr(chk)
act=2
endcase
case type('&cht')='D' 
if dtoc(&chc)<>' / / '
nt=nt+c1+chr(chk)+'=ctod("'+dtoc(&chc)+'")'
act=2
endif
endcase
chk=chk+1
enddo
endif
SNT=SUBSTR(NT,8,LEN(NT))
NT=SNT
RETURN
