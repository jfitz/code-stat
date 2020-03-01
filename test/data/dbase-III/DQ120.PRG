set color to 11/1
@20,0 clear
@20,0 say '----------The Screen or Printer List will 
appear as follows----------Width' 
XX='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX 
XXXXXXXXXXX'
io='Record# '
KK=''
@22,1 say 'Items to be Listed 
'
al=UPPER(kf)
KF=''
KL=''
@22,20 say al
DO WHILE KK<>'0'
if kk='2'
return
endif
set color to 1/15
@21,0 say substr(io,1,160)
set color to &def_color 
ia=len(io)
@20,75 say substr(str(ia),8,3)
@22,20 CLEAR
@22,20 SAY KL
WAIT ' Which Items ' to kE
KK=UPPER(KE)
ok=asc(kk)
if asc(kE)=0 .or. ok>asc(last_file)
kl=al+','
kk='X'
endif
if ok>64 .and. ok do case
case TYPE('&ke')='L'
io=io+'.?. '
case TYPE('&ke')='N'
vc='99999999999999999999999'
io=io+substr(vc,len(str(&ke)))
case TYPE('&ke')='D'
io=io+dtoc(date())
case kk=' '
ii='________________________'
spa=0
accept 'How Many Spaces ' to spa
kk='space(&spa)'
@row()-1,0 clear
io=io+substr(ii,1,&spa)
otherwise
io=io+substr(xx,1,len(&kk))+' '
endcase
endif
kl=KL+kk+','
if kk='1'
kl=''
kk=''
io='Record# '
ia=0
@16,0 clear
enddo
endif
if kK='?'
do DQ100 
endif
ENDDO
set color to &def_color
KF=SUBSTR(KL,1,LEN(KL)-3)
KL=''
KJ=''
KS=''
KI=''
@20,0 CLEAR
DO DQ200 
KL=NT
EMP=1
DO WHILE EMP>0
emp=at('BLANK',upper(kl))
DO CASE
CASE EMP>0
KZ=SUBSTR(KL,1,EMP-1)+SPACE(1)+SUBSTR(KL,EMP+5,LEN(KL))
KL=KZ
ENDCASE
ENDDO
if LEN(kf)>0 .and. kf<>'NO' .and. LEN(kl)>0 .and. kl<>'NO'
@20,0 clear
wait ' PRESS [P] To send list to Printer [S] To screen ' to en
do case
case upper(en)='P'
clear
@5,15 say 'Set printer width to at least'
@5,45 say ia
wait
list all fields &kf for &kl to print
otherwise
clear
display all fields &kf for &kl
endcase
wait
endif

