5 ' Escape from Warehouse, Copyright 1989 by Thierry Klein
10 CLS:TIM=VAL(RIGHTS(TIMES,2)): RANDOMIZE TIM:DEFINT A-Z
15 DIM E(2,40),BC(25),BL(25)
17 DIM CAISSE(23),BANDIT(23),PERSON(23),PORTE(23),BLANK(23)
19 ' Create the graphic figures
20 LINE(3,2)-(12,4),4,BF:LINE(2,1)-(13,5),6,B:GET(0,0)-(15,6),CAISSE
21 CLS:FOR I=1 TO 11:READ X,Y,C:PSET(X,Y),C
22 NEXT I:LINE(5,2)-(9,2),5
23 GET(0,0)-(15,6),BANDIT
24 DATA 7,0,4,7,1,4,7,3,5,3,3,7,3,4,7,10,3,4,7,4,1,6,5,1,8,5,1,5,6,6,9,6,6
25 FOR I=1 TO 4:READ X,Y,C:PSET(X,Y),C:NEXT:LINE(5,2)-(9,2),2
26 GET(0,0)-(15,6),PERSON
27 DATA 7,3,2,3,3,0,4,3,4,3,4,0
28 CLS:LINE(5,0)-(0,5),5:LINE(6,0)-(1,5),5:GET(0,0)-(15,6),PORTE
29 CLS:GET(0,0)-(15,6),BLANK
34 ' Signon message
35 LOCATE 5,30:PRINT" Escape from Warehouse"
40 LOCATE 13,18:PRINT'Do you want to read the instructions (Y/N) ?";
50 I$=INPUTS(1):CLS:IF I$="Y" OR I$="y" THEN GOSUB 3000
90 PV=100:LOCATE 13,21:INPUT"Number of gangsters (1 to 25)";NB$
100 XSB=0:NB=VAL(NBS):IF NB<1 OR NB>25 THEN BEEP:GOTO 90
110 CLS:PRINT"One moment please..."
230 ' Creation of the warehouse
250 FOR L=1 TO 30:FOR C=1 TO 38:E(L,C)=INT(RND*2):NEXT C,L
290 FOR C=0 TO 39:E(0,C)=4:EG1,C)=4:NEXT C
320 FOR L=0 TO 31:E(L,0)=4:E(L,39)=4:NEXT L
360 ' Positioning the door
380 E(16,0)=5:E(16,1)=0:E(15,1)=0:E(17,1)=0
400 ' Positioning of the gangsters
420 FOR I=0 TO NB-i
430 L=INT(RND*30)+1:C=INT(RND*30)+1
450 IF E(L,C)0 THEN 430
460 E(L,C)=2:BC(I)=C:BL(I)=L
470 NEXT I
490 ' Positioning the character
510 LP=INT(RND*3)+16:CP=INT(RND*4)+35:E(LP,CP)=3
550 ' Display of the warehouse
570 CLS:LINE(5,2)-(634,222),1,B:LINE(10,4)-(629,220),1,B:PAINT(6,3),1
570 FOR L=0 TO 31:FOR C=0 TO 39
590 Y=L*7:X=C*16:1=E(L,C)
620 IF I=1 THEN PUT(X,Y),CAISSE:GOTO 710
630 IF I=2 THEN PUT(X,Y),BANDIT:GOTO 710
640 IF I=3 THEN PUT(X,Y),PERSON: GOTO 710
650 IF I=5 THEN PUT(X+5,Y),PORTE,PSET
710 NEXT CL:LOCATE 1,4,0:PRINT"100"
740 ' Waiting loop for the depress of a key
760 IF XSB THEN PUT(XSB, YSB), BANDIT,PSET:XSB=0
765 I$=INKEY$:IF I$="" THEN 1000
766 WHILE INKEY$"":WEND
770 C=ASC(I$):IF C=27 THEN CLS:LOCATE 1,1,1:END
830 ' Moving of the character
845 ON C-27 GOTO 850,860,870,880
847 GOTO 1000
850 DY=0:DX=1:GOTO 885
860 DY=0:DX=-1:GOTO 885
870 DY=-1:DX=0:GOTO 885
880 DY=1:DX=0
885 LN=LP+DY:CN=CP+DX:ET1=E(LN,CN)
890 ON ET1+1 GOTO 970,920,900,900,900,980
900 BEEP:GOTO 1000
920 IL=DY:IC=DX
930 ETI1=E(LN+IL,CN+IC):IF ET1=0 THEN GOTO 2000
940 IF ET12 THEN 950
941 FOR I=0 TO NB-1:IF BL(DL)LN+IL OR BC(I)CN+IC THEN 945
942 BL(I)=-1:E(LN+IL,CN+IC)=0:I=NB
945 NEXT I:GOTO 2000
950 IF ET1=4 OR ET1=5 THEN 900
960 IL=IL+DY:IC=IC+DX:GOTO 930
970 PUT(CN*16,LN*7),PERSON,PSET:PUT(CP*16,LP*7),BLANK,PSET
972 E(LP,CP)=0:LP=LN:CP=CN:E(LP,CP)=3:GOTO 760
980 PUT(0,112),PERSON,PSET:LINE(16,112)-(31,118),0,BF:BEEP
985 LOCATE 13,33:COLOR 0,7:PRINT"YOU SUCCEEDED !"
987 COLOR 7,0:GOTO 2500
990 ' Moving of the bandits (gangsters)
1000 TIM1=VAL(RIGHTS(TIME$,2)):IF TIM1=TIM THEN 760
1010 TIM=TIM1
1020 FOR I=0 TO NB-1:IF BL(I)=-1 THEN 1470
1030 LB=BL(I):CB=BC(I):SL=SGN(LP-LB):SC=SGN(CP-CB)
1035 LN=LB+SL:CN=CB+SC
1050 IF E(LB+SL,CB+SC)=0 THEN 1150
1060 IF E(LB+SL,CB)=0 THEN SC=0:GOTO 1150
1070 IF E(LB,CB+SC)=0 THEN SL=0:GOTO 1150
1080 FOR SL=-1 TO 1:FOR SC=-1TO1
1090 IF E(LB+SL,CB+SC)=0 THEN 1150
1100 NEXT SC,SL
1140 SL=0:SC=0:GOTO 1190
1150 BL(I)=LB+SL:BC(I)=CB+SC:PUT(BC(1)*16,BL(1)*7), BANDIT,PSET
1155 PUT(CB*16,LB*7),BLANK,PSET:E(LB,CB)=0:E(LB+SL,CB+SC)=2
1170 ' Shooting
1190 Y=BL(I):X=BC(I)
1200 DX=CP-X:DY=LP-Y:DIST=SQR(DX*DX+DY*DY)
1202 IF DIST>9 THEN 1470
1205 ADX=ABS(DX):ADY=ABS(DY)
1210 IF ADX>=1.5*ADY THEN X=X+SGN(DX):GOTO 1260
1230 IF ADY>=1.5*ADX THEN Y=Y+SGN(DY):GOTO 1260
1250 X=X+SGN(DX):Y=Y+SGN(DY)
1260 DX=CP-X:DY=LP-Y:IF DX=0 AND DY=0 THEN 1430
1270 IFE(Y,X)>0 THEN 1470
1290 GOTO 1205
1430 YSB=BL(I)*7:XSB=BC(I)*16
1435 PUT(XSB,YSB),BANDIT,PRESET:BEEP
1440 PV=PV-10+DIST:IF PV<0 THEN PV=0
1450 LOCATE 1,4:PRINT USING"###";PV
1460 IF PV>0 THEN I=NB:GOTO 1470
1462 X=CP*16:Y=LP*7:PUT(X,Y),BLANK,PSET
1463 LINE(X+7,Y)-(X+8,Y+6),7,B
1467 LINE(X+2,Y+2)-(X+13,Y+2),7
1468 LOCATE 13,34:PRINT"YOU ARE DEAD !";:GOTO 2500
1470 NEXT I:GOTO 760
1980 ' Moving of a file of cases
2000 PUT((CN+IC)*16,(LN+IL)*7),CAISSE,PSET
2010 PUT((CP+DX)*16,(LP+DY)*7),PERSON,PSET
2020 PUT(CP*16,LP*7), BLANK,PSET:E(LN+IL,CN+IC)=1
2030 E(LP,CP)=0:LP=LN:CP=CN:E(LP,CP)=3:GOTO 760
2480 ' End or beginning again
2500 IF XSB THEN PUT(XSB,YSB),BANDIT,PSET:XSB=0
2505 LOCATE 25,22,1:PRINT "Do you want to play again (Y/N) ?";
2510 I$=INPUTS(1)
2520 IF I$="y" OR I$="Y" THEN PRINT "Yes.";:GOTO 90
2530 IF I$="n" OR I$="N" THEN PRINT "No.";:LOCATE 1,1,1:END
2540 GOTO 2510
2990 ' Display the instructions
3000 PRINT" You are in a warehouse. Gangsters'' are between you and the"
3011 PRINT"way out. If the gangsters happen to be face to face with you, they"
3020 PRINT"will shoot (you have 100 Life points). The closer they get, the"
3021 PRINT"more life points you lose. To survive, you must make your way out"
3022 PRINT"the door'' at the middle of the screen on your left."
3023 PRINT:PUT(304,0),BANDIT:PUT(80,36),PORTE
3030 PRINT" You direct your character '' by means of the arrow-keys. You"
3031 PRINT"can push the cases '' which are in the warehouse. The number of"
3040 PRINT"cases you are pushing is not important, provided that there is a"
3041 PRINT"free place at the other end. If the file of cases bumps against"
3042 PRINT"the wall or door, the cases will not move! If a gangster is behind"
3050 PRINT"the cases you are pushing, he is crushed!"
3051 PRINT:PUT(232,54),PERSON:PUT(160,63),CAISSE
3052 PRINT"Your character can move only horizontally or vertically. But the"
3053 PRINT"gangsters can move diagonally as well. They automatically move "
3060 PRINT"even if you do not move (after 3 seconds)."
3070 PRINT:PRINT"You can choose the number of gangsters you will have to"
3071 PRINT"encounter: from 5 TO 15! You can stop the game at any time by"
3080 PRINT"hitting ESC. Your life points are displayed at the upper left"
3081 PRINT"corner of the screen. When a gangster shoots, the bell rings, and"
3090 PRINT"he is highlighted!"
3091 PRINT:PRINT'It only remains for me to wish you good luck!";
3100 PRINT" Hit any key to play...";:I$=INPUT$(1):RETURN
