10 REMARK AIRINPUT.BAS\tAlan Bose\t08-Jan-82
20 CLEAR1500:WIDTH255:ON ERROR GOTO3860
30 BL$=CHR$(7):E$=CHR$(27):ER$=E$+"E":P$=E$+"p":Q$=E$+"q":G$=E$+"F":NG$=E$+"G":Y$=E$+"Y":L$=E$+"l":J$=E$+"j":K$=E$+"k":J1$=E$+"J":U=1.790493109375
40 DEF FNC$(C1,C2)=Y$+CHR$(C1+31)+CHR$(C2+31)
50 DEF FNS6(X)=INT(X*10+0.5)/10
60 DEF FNS7(X)=ATN(X/SQR(1-X*X))*U
70 DEF FNS8(X)=SIN(ABS(A/2)/U)*COS(X/U)/SIN(Q2/2)
80 PRINT ER$;"Standby one..."
90 OPEN"R",1,"SY1:AIRPORTS.RND"
100 OPEN"R",2,"SY1:AIRINDEX.RND"
110 MD=(LOC(1)*5)-1
115 IF MD=-1 THEN MD=0
120 OL=MD+10
130 DIM ID$(OL),RN$(2),RN(2)
140 FOR J=0 TO MD
150 REC=(JMOD51)+1:SS=J \ 51
160 IF LOF(2)<>REC THEN GET #2,REC
170 FIELD #2,SS*5 AS DU$,5 AS ID$
180 ID$(J)=ID$
190 IF ASC(ID$)=0 THEN ID$(J)="     "
200 NEXT J:CLOSE#2
210 IM=MD
220 REMARKdata box
230 PRINT ER$;G$;FNC$(2,6);"f";
240 FOR J=7 TO 74:PRINT "a";:NEXT J:PRINT "c"
250 PRINT FNC$(4,6);"e";
260 FOR J=7 TO 74:PRINT "a";:NEXT J:PRINT "d"
270 PRINT FNC$(2,12);"s";FNC$(2,15);"s";FNC$(2,23);"s";FNC$(2,44);"s";FNC$(2,52);"s";FNC$(2,61);"s";FNC$(2,69);"s"
280 PRINT FNC$(4,12);"u";FNC$(4,15);"u";FNC$(4,23);"u";FNC$(4,44);"u";FNC$(4,52);"u";FNC$(4,61);"u";FNC$(4,69);"u"
290 PRINT FNC$(3,6);"`";FNC$(3,12);"`";FNC$(3,15);"`";FNC$(3,23);"`";FNC$(3,44);"`";FNC$(3,52);"`";FNC$(3,61);"`";FNC$(3,69);"`";FNC$(3,75);"`"
300 PRINT NG$
310 PRINT FNC$(1,7);"Ident Fac Freq"
320 PRINT FNC$(1,32);"Name"
330 PRINT FNC$(1,47);"Lat"
340 PRINT FNC$(1,55);"Long"
350 PRINT FNC$(1,64);"Var"
360 PRINT FNC$(1,70);"Elev"
370 PRINT FNC$(5,9);"1";FNC$(5,13);"2";FNC$(5,19);"3";FNC$(5,33);"4";FNC$(5,47);"5";FNC$(5,56);"6";FNC$(5,65);"7";FNC$(5,72);"8"
380 REMARKmenu
390 PRINT FNC$(7,1);J1$
400 PRINT FNC$(8,7);"PRESS  `I' to input new data"
410 PRINT FNC$(9,14);"`U' to update existing data"
420 PRINT FNC$(10,14);"`E' to exit"
430 X$=INPUT$(1):GOSUB3820:MD$=X$:PRINT FNC$(8,1);J1$:IF MD$="I" THEN 490
440 IF MD$="U" THEN 500
450 IF MD$<>"E" THEN PRINT BL$:GOTO400
460 PRINT ER$;"Returning to menu.  Sure? (Y or N)  <N>  ";:X$=INPUT$(1):PRINT X$:IF X$=CHR$(13) THEN X$="N"
470 GOSUB3820:IF X$="Y" THEN CLOSE:GOSUB3340:LOAD"MENU",R
480 IF X$="N" THEN 220 ELSE PRINT BL$:GOTO460
490 C8=0:GOTO600
500 REMARKrevise
510 PRINT FNC$(8,7);L$;"Enter Identifier   <MENU>   ";J$;STRING$(5,95);K$;
520 LINE INPUT X$:PRINT J1$
530 IF X$="" THEN 380
540 IF LEN(X$)>5 THEN PRINT BL$:GOTO510
550 GOSUB3820
560 AP$=X$+SPACE$(5-LEN(X$)):NL$=AP$
570 GOSUB1730
580 IF FD=0 THEN PRINT BL$;"Can't find ";AP$:GOTO510
590 RO=3:GOSUB2090
600 IF MD$="I" AND C8=8 THEN PUT#1,REC:ID$(PI)=ID$:EN=1:GOTO380
610 IF MD$="I" THEN C8=C8+1:GOTO660
620 PRINT FNC$(7,1);J1$:PRINT FNC$(8,7);"Press number for revision  <EXIT>  ";
630 C$=INPUT$(1)
640 IF C$=CHR$(13) THEN PUT#1,REC:ID$(PI)=ID$:GOTO380
650 C8=VAL(C$)
660 PRINT FNC$(7,1);J1$;
670 ON C8 GOTO700,890,990,1070,1160,1360,1550,1660
680 PRINT BL$:GOTO620
690 REMARK
700 REMARKid
710 PRINT "Enter airport/facility code: ";J$;STRING$(5,95)
720 IF MD$="U" THEN PRINT:PRINT"Enter 'D' to erase listing"
730 PRINT K$;:LINE INPUT X$
740 IF MD$="I" AND X$="" THEN 380
750 IF X$="" THEN 1720
760 GOSUB3820
770 AP$=X$+SPACE$(5-LEN(X$)):NL$=AP$
780 IF (MD$="I" AND X$="D") OR LEN(X$)>5 THEN PRINT BL$:GOTO660
790 IF MD$="I" THEN GOSUB1920
800 IF X$<>"D" THEN LSET ID$=NL$:EN=1:GOTO1720
810 GOSUB3890:KY=KY-1:FOR J=1 TO KY:IF LI$(J)=ID$ THEN LI$(J)="":EE=1
820 IF R1$(J)=ID$ AND R1(J)=PI THEN R1$(J)="":EE=1
830 IF R2$(J)=ID$ AND R2(J)=PI THEN R2$(J)="":EE=1
840 IF R1$(J)="" AND R2$(J)="" THEN LI$(J)=""
850 IF LI$(J)="" THEN DD=DD+1
860 NEXT J:IF EE=1 THEN GOSUB4000 ELSE GOSUB4090
870 GOSUB3220:GOSUB2090
880 GOTO380
890 REMARKfacility
900 PRINT L$;"Enter facility code:  ";J$;STRING$(2,95)
910 PRINT:PRINT"A = Airport":PRINT"V = VOR/VORTAC":PRINT"N = NDB/LOM":PRINT"I = Intersection":PRINT"R = Reporting point":PRINT"C = Checkpoint":PRINT"W = Waypoint":PRINT"L = Landmark"
920 PRINT K$;:LINE INPUT X$
930 IF LEN(X$)>2 THEN PRINT BL$:GOTO660
940 IF MD$="I" AND X$="" THEN 380
950 IF X$="" THEN 980
960 GOSUB3820
970 LSET FAC$=X$
980 GOTO1720
990 REMARKfreq
1000 IF MD$="I" AND INSTR(FAC$,"V")=0 AND INSTR(FAC$,"N")=0 THEN 1060
1010 PRINT"Enter navaid frequency  ";J$;STRING$(7,95);K$;
1020 LINE INPUT X$
1030 IF MD$="I" AND X$="" THEN 380
1040 IF X$="" THEN 1060
1050 LSET FR$=MKI$(VAL(X$))
1060 GOTO1720
1070 REMARKname
1080 PRINT L$;"Enter facility name  ";J$;STRING$(20,95);K$;
1090 LINE INPUT X$
1100 IF MD$="I" AND X$="" THEN 380
1110 IF LEN(X$)>20 THEN PRINT BL$;"20 characters maximum";FNC$(7,1);:GOTO 1080
1120 IF INSTR(X$,",")=0 THEN PRINT BL$;"Forgot state preceded by comma";FNC$(7,1);:GOTO1080
1130 IF X$="" THEN 1150
1140 LSET NM$=X$
1150 GOTO1720
1160 REMARKlat
1170 IF MD$="I" AND INSTR(FAC$,"I")>0 AND INSTR(FAC$,"V")=0 AND INSTR(FAC$,"N")=0 THEN GOSUB2290:GOTO1350
1180 PRINT"Enter degrees latitude";FNC$(7,30);J$;STRING$(2,95);"   deg"
1190 PRINT:PRINT"Enter `R' for RNAV calculation of lat. & long. from known fix"
1200 PRINT K$;:LINE INPUT X$:X=VAL(X$)
1210 IF MD$="I" AND X$="" THEN 380
1220 IF X$="R" OR X$="r" THEN TR=REC:TS=SS:I$=ID$:PUT#1,REC:GOSUB2290:GOTO1350
1230 IF X$="" THEN 1350
1240 IF X>90 OR X<=0 THEN PRINT BL$:GOTO1200
1250 LSET D1$=LOF(X)
1260 PRINT J1$;"Enter minutes latitude  <0>  ";J$;STRING$(4,95);" min";K$;
1270 LINE INPUT X$:X=VAL(X$)
1280 IF X$="" THEN X=0:PRINT K$;"0"
1290 IF X>=60 OR X<0 THEN PRINT BL$:GOTO1260
1300 PRINT J1$;"Enter seconds latitude  <0>  ";J$;STRING$(4,95);" sec";K$;
1310 LINE INPUT X$:Y=VAL(X$):IF X$="" THEN Y=0:PRINT K$;"0"
1320 IF Y>60 OR Y<0 THEN PRINT BL$:GOTO1300
1330 X=X+(Y/60)
1340 LSET M1$=MKI$(X)
1350 GOTO1720
1360 REMARKenter long
1370 PRINT"Enter degrees longitude";FNC$(7,31);J$;STRING$(3,95);"  deg"
1380 PRINT:PRINT"Enter `R' for RNAV calculation of lat. & long. from known fix"
1390 PRINT K$;:LINE INPUT X$:X=VAL(X$)
1400 IF MD$="I" AND X$="" THEN 380
1410 IF X$="" THEN 1540
1420 IF X$="R" OR X$="r" THEN TR=REC:TS=SS:I$=ID$:PUT#1,REC:GOSUB2290:GOTO1540
1430 IF X>180 OR X<=0 THEN PRINT BL$:GOTO1370
1440 LSET D$=LOF(X)
1450 PRINT J1$;"Enter minutes longitude  <0>  ";J$;STRING$(4,95);" min";K$;
1460 LINE INPUT X$:X=VAL(X$)
1470 IF X$="" THEN X=0:PRINT K$;"0"
1480 IF X>=60 OR X<0 THEN PRINT BL$;:GOTO1450
1490 PRINT"Enter seconds longitude  <0>  ";J$;STRING$(4,95);" sec";K$;
1500 LINE INPUT X$:Y=VAL(X$):IF X$="" THEN Y=0:PRINT K$;"0"
1510 IF Y>60 OR Y<0 THEN PRINT BL$:GOTO1490
1520 X=X+(Y/60)
1530 LSET M$=MKI$(X)
1540 GOTO1720
1550 REMARKvar
1560 PRINT"Enter magnetic variation  <0>  ";J$;STRING$(4,95);" deg";K$;
1570 LINE INPUT X$:X=VAL(X$)
1580 IF MD$="I" AND X$="" THEN X=0
1590 IF X$="" THEN 1650
1600 LSET V$=MKI$(X)
1610 IF X=0 THEN LSET V1$=" ":GOTO1650
1620 PRINT"East or West variation?  ";J1$;
1630 X$=INPUT$(1):PRINT X$:GOSUB3820:IF X$<>"E" AND X$<>"W" THEN PRINT BL$;:GOTO1620
1640 PRINT FNC$(7,1);J1$:LSET V1$=X$
1650 GOTO1720
1660 REMARKelev
1670 PRINT"Enter elevation of facility  ";J$;STRING$(5,95);K$;
1680 LINE INPUT X$:X=VAL(X$)
1690 IF MD$="I" AND X$="" THEN PRINT BL$:GOTO660
1700 IF X$="" THEN 1720
1710 LSET EL$=LOF(X)
1720 RO=3:GOSUB2090:GOTO600
1730 REMARKsearch-match
1740 RO=3
1750 FD=0
1760 FOR J=0 TO IM
1770 IF ID$(J)<>AP$ THEN 1820
1780 IF FD=1 THEN RO=7:GET#1,REC:PRINT FNC$(7,1);J1$:GOSUB2090:RO=8:FD=2
1790 PI=J
1800 IF FD>1 THEN REC=(JMOD5)+1:SS=J \ 5:GET#1,REC:GOSUB2090:FD=FD+1:RO=RO+1
1810 IF FD=0 THEN FD=1:REC=(JMOD5)+1:SS=J \ 5:GET#1,REC
1820 NEXT J
1830 IF FD=0 THEN 1910
1840 IF FD=1 THEN 1910
1850 PRINT FNC$(RO+1,1);"Enter number of your choice  <";PI;">  ";J$;STRING$(3,95);K$;:LINE INPUT X$
1860 IF X$="" THEN 1910
1870 PI=VAL(X$)
1880 REC=(PIMOD5)+1:SS=PI \ 5
1890 GET #1,REC
1900 PRINT FNC$(6,1);J1$
1910 RETURN
1920 REMARKsearch-blank
1930 FD=0:FH=0
1940 FOR J=0 TO IM
1950 IF ID$(J)="     " THEN FH=1:TI=J:J=IM+1
1960 IF ID$(J)=AP$ THEN FD=FD+1:RO=8+FD:REC=(JMOD5)+1:SS=J \ 5:GET#1,REC:PI=J:GOSUB2090
1970 NEXT J
1980 IF FH=0 THEN IM=IM+1:TI=IM
1990 IF IM>OL THEN PRINT ER$;"Standby one...then re-enter":CLOSE:GOSUB3340:GOTO10
2000 RO=3:IF FD=0 THEN 2050
2010 PRINT FNC$(10+FD,7);"Found...continue with additional entry?  (Y or N)  <Y>";
2020 X$=INPUT$(1):GOSUB3820
2030 IF X$="N" THEN 380
2040 IF X$<>"Y" AND X$<>CHR$(13) THEN PRINT BL$:GOTO2010
2050 PI=TI
2060 REC=(PIMOD5)+1:SS=PI \ 5:GET#1,REC
2070 GOSUB2090:GOSUB3220
2080 RETURN
2090 REMARKdecode & display
2100 FIELD #1,SS*50 AS DU$,5 AS ID$,2 AS FAC$,4 AS FR$,20 AS NM$,2 AS D1$,4 AS M1$,2 AS D$,4 AS M$,4 AS V$,1 AS V1$,2 AS EL$
2110 F5=CVI(FR$):D6=CVS(D1$):M6=CVI(M1$):D5=CVS(D$):M5=CVI(M$):V5=CVI(V$):E5=CVS(EL$)
2120 PI$=STR$(PI):PI$=PI$+SPACE$(4-LEN(PI$)):PRINT FNC$(RO,1);PI$;
2130 PRINT FNC$(RO,7);ID$;FNC$(RO,13);FAC$;
2140 PRINT FNC$(RO,16);"       ";FNC$(RO,16);
2150 IF F5=0 THEN 2200
2160 IF F5>136 THEN PRINT USING"#####";F5;:GOTO2200
2170 IF F5*10MOD1=F5*10/1 THEN PRINT USING"####.#";F5;:GOTO2200
2180 IF F5*100MOD1=F5*100/1 THEN PRINT USING"####.##";F5;:GOTO2200
2190 PRINT USING"###.###";F5;
2200 PRINT FNC$(RO,24);NM$;
2210 PRINT FNC$(RO,45);USING"##";D6
2220 PRINT FNC$(RO,48);USING"##.#";M6;
2230 PRINT FNC$(RO,53);USING"###";D5;
2240 PRINT FNC$(RO,57);USING"##.#";M5;
2250 PRINT FNC$(RO,62);USING"###.#";V5;
2260 PRINT FNC$(RO,68);V1$;FNC$(RO,70);USING"#####";E5
2270 IF INSTR(FAC$,"V")=0 AND INSTR(FAC$,"N")=0 THEN NV=0 ELSE NV=1
2280 RETURN
2290 REMARKRNAV lat & long
2300 PRINT FNC$(6,1);J1$
2310 PRINT"This routine will find the latitude & longitude of ";I$:PRINT"by taking fixes on 1 or 2 navaids already on file.":PRINT
2320 PRINT"The navaids you specify should be the ones you'll use in the air":PRINT"to determine your position.":PRINT
2330 PRINT"Postion can be determined two ways:":PRINT
2340 PRINT TAB(5);"1  -  Distance & bearing FROM one navaid":PRINT
2350 PRINT TAB(5);"2  -  Bearings FROM two navaids":PRINT
2360 PRINT J$;J1$;TAB(5);"Enter selection  <RETURN>  ";:X$=INPUT$(1):PRINT X$
2370 REMARK2 bearings
2380 IF X$=CHR$(13) THEN C8=C8-1:GOTO3210
2390 IF X$="2" THEN RN=1:GOTO2410
2400 IF X$="1" THEN RN=0 ELSE PRINT BL$;K$;:GOTO2360
2410 PRINT FNC$(7,1);J1$;
2420 PRINT FNC$(7,1);L$;"Enter identifier of known fix on file  ";J$;STRING$(5,95);K$;
2430 LINE INPUT X$:PRINT J1$
2440 IF X$="" THEN C8=C8-1:GOTO3210
2450 IF LEN(X$)>5 THEN PRINT BL$:GOTO2420
2460 GOSUB3820
2470 AP$=X$+SPACE$(5-LEN(X$))
2480 PUT#1,REC:TI=PI:RO=9:GOSUB 1750
2490 IF FD=0 THEN PRINT BL$;"Can't find ";AP$:GOTO2420
2500 PRINT FNC$(7,1);J1$:RO=9:GOSUB 2090:RN$(RN)=ID$:RN(RN)=PI:PI=TI
2510 IF NV=1 THEN 2550
2520 PRINT BL$;"Not listed as navaid. Use? (Y or N) <N> ";:X$=INPUT$(1):PRINT X$:IF X$=CHR$(13) THEN X$="N"
2530 GOSUB3820:IF X$="N" THEN 2420
2540 IF X$<>"Y" THEN PRINT BL$:GOTO2520
2550 X4=D6+(M6/60):X6=D5+(M5/60)
2560 X6=-X6 REMARKwest hemis
2570 K9=0:L9=0
2580 IF RN<>0 THEN PRINT FNC$(RO+2,1);"Bearing FROM ";ID$;" to ";NL$:GOTO2630
2590 REMARKdist & 1 bearing
2600 PRINT FNC$(RO+2,1);"Distance & bearing FROM ";ID$;" to ";NL$
2610 PRINT FNC$(RO+4,5);"Enter distance in nautical miles  ";J$;STRING$(3,95);K$;
2620 LINE INPUT D$:D=VAL(D$):IF D=0 THEN PRINT BL$:GOTO2610
2630 PRINT FNC$(RO+5,5);"Enter bearing  ";J$;STRING$(3,95);K$;
2640 LINE INPUT H$:H=VAL(H$):IF H<0 OR H>360 THEN PRINT BL$:GOTO2630
2650 IF H$="" THEN C8=C8-1:GOTO2290
2660 PRINT FNC$(RO+6,5);"Is bearing True or Magnetic?  <T>  ";J1$;
2670 X$=INPUT$(1):PRINT X$:GOSUB3820:IF X$="T" OR X$=CHR$(13) THEN 2710
2680 IF X$<>"M" THEN PRINT BL$:GOTO2660
2690 V=V5:IF V1$="E" THEN V=-V
2700 H=H-V
2710 IF RN<>0 THEN P2(RN)=X4:P1(RN)=-X6:RA(RN)=H
2720 IF RN=1 THEN RN=2:GOTO2410
2730 IF RN=2 THEN GOSUB3480:GOTO2760
2740 C=D:C1=H
2750 REMARKsolve lat & long
2760 IF C1>270 THEN 2800
2770 IF C1>180 THEN 2810
2780 IF C1>90 THEN 2820
2790 IF C1<=90 THEN 2830
2800 A=360-C1:GOSUB2840:GOTO2880
2810 A=C1-180:GOSUB2840:GOTO2890
2820 A=180-C1:GOSUB2840:GOTO2900
2830 A=C1:GOSUB2840:GOTO2910
2840 B=A/U
2850 B1=C*COS(B)
2860 B2=C*SIN(B)
2870 RETURN
2880 K=B1:L=-B2:GOTO2920
2890 K=-B1:L=-B2:GOTO2920
2900 K=-B1:L=B2:GOTO2920
2910 K=B1:L=B2
2920 K9=K:L9=L
2930 X8=X4+(K9/60)
2940 X9=(X4+X8)/(2*U)
2950 X8=ABS(X8)
2960 Y=INT(X8)
2970 Y1=X8-Y
2980 Y2=Y1*60
2990 Y3=(L9/COS(X9))/60
3000 Y4=X6+Y3
3010 Y4=ABS(Y4)
3020 Y5=INT(Y4)
3030 Y6=Y4-Y5
3040 Y7=Y6*60
3050 REC=(PIMOD5)+1:SS=PI \ 5:GET#1,REC
3060 RO=3:GOSUB2090
3070 LSET D1$=LOF(Y)
3080 LSET M1$=MKI$(Y2)
3090 LSET D$=LOF(Y5)
3100 LSET M$=MKI$(Y7)
3110 GOSUB2090
3120 C8=C8+1
3130 IF INSTR(FAC$,"V")>0 THEN 3210
3140 GOSUB3890
3150 LI$(KY)=NL$
3160 IF RN=0 THEN R1$(KY)=RN$(0):R1(KY)=RN(0):R2$(KY)="":R2(KY)=0:GOTO3180
3170 R1$(KY)=RN$(1):R1(KY)=RN(1):R2$(KY)=RN$(2):R2(KY)=RN(2)
3180 RP=0:FOR J=1 TO KY-1:IF LI$(J)=LI$(KY) AND R1$(J)=R1$(KY) AND R1(J)=R1(KY) AND R2$(J)=R2$(KY) AND R2(J)=R2(KY) THEN RP=1
3190 NEXT J
3200 IF RP=0 THEN GOSUB4000 ELSE GOSUB4090
3210 RETURN
3220 REMARKclear
3230 EN=1:LSET ID$="     "
3240 LSET FAC$="  "
3250 LSET FR$=MKI$(0)
3260 LSET NM$=SPACE$(20)
3270 LSET D1$=LOF(0):LSET M1$=MKI$(0)
3280 LSET D$=LOF(0):LSET M$=MKI$(0)
3290 LSET V$=MKI$(0)
3300 LSET V1$=" "
3310 LSET EL$=LOF(0)
3320 PUT#1,REC:ID$(PIND)="     "
3330 RETURN
3340 REMARKwrite index
3350 IF EN=0 THEN 3460
3360 PRINT ER$;"Standby one..."
3370 OPEN"R",2,"SY1:AIRINDEX.RND"
3380 REC=1:FOR J=0 TO IM
3390 RC=(JMOD51)+1:SS=J \ 51
3400 IF REC<>RC THEN REC=RC:PUT#2,LOF(2)+1:FIELD#2,255 AS CL$:LSET CL$=" "
3410 FIELD #2,SS*5 AS DU$,5 AS ID$
3420 LSET ID$=ID$(J)
3430 NEXT J
3440 IF RC<>LOF(2) THEN PUT#2,RC
3450 CLOSE#2
3460 RETURN
3470 REMARK2 bearings
3480 IF RA(1)>RA(2) AND RA(2)<RA(1)-180 THEN AB=(360-RA(1))+RA(2) ELSE AB=ABS(RA(1)-RA(2))
3490 IF AB>180 THEN AB=AB-180
3500 IF AB<15 OR AB>165 THEN PRINT BL$;FNC$(7,1);J1$;FNC$(9,1);"You're too close to the line that runs between the navaids":PRINT"to compute your position accurately.":GOTO2420
3510 GOSUB 3610
3520 IF RA(1)>T AND T<RA(1)-180 THEN AA=(360-RA(1))+T ELSE AA=ABS(T-RA(1))
3530 IF AA>180 THEN AA=AA-180
3540 IF T>180 THEN T1=T-180 ELSE T1=T+180
3550 IF RA(2)>T1 AND T1<RA(2)-180 THEN AC=(360-RA(2))+T1 ELSE AC=ABS(T1-RA(2))
3560 IF AC>180 THEN AC=AC-180
3570 SC=SIN(AC/U)*SIN(Q2)/SIN(AB/U)
3580 SC=ATN(SC/SQR(-SC*SC+1)):C=SC*U*60
3590 C1=RA(1):X4=P2(1):X6=-P1(1):H=RA(1)
3600 RETURN
3610 REMARKdistance
3620 A=P1(1)-P1(2)
3630 B1=P2(1)-P2(2)
3640 P=COS(P2(1)/U)*COS(P2(2)/U)
3650 Q=P*COS(ABS(A)/U)+COS(ABS(B1)/U)-P:IF Q<=0 THEN PRINT BL$:GOTO3810
3660 Q2=ATN(SQR(1-Q*Q)/Q):Q=Q2*U*60
3670 C=FNS6(Q):IF C>900 AND ABS(A)>30 THEN PRINT BL$:GOTO3800
3680 IF C=0 THEN T=0:GOTO 3790
3690 REMARK true course
3700 S=FNS8((P2(1)+P2(2))/2):IF S>=1 THEN S=90-S:GOTO3720
3710 S=FNS7(S)
3720 IF A>0 AND B1=0 THEN T=90:GOTO3780
3730 IF A<0 AND B1=0 THEN T=270:GOTO3780
3740 IF A>0 AND B1<0 THEN T=S:GOTO3780
3750 IF A>=0 AND B1>0 THEN T=180-S:GOTO3780
3760 IF A<0 AND B1>0 THEN T=180+S:GOTO3780
3770 T=360-S
3780 T=FNS6(T)
3790 RETURN
3800 PRINT BL$;"Distance excessive...":GOTO2330
3810 PRINT BL$;"Distance excesssive.":PRINT"Possible course errors due to rhumb line.":GOTO2330
3820 REMARKmap lc
3830 FOR L=1 TO LEN(X$):U$=MID$(X$,L,1)
3840 IF ASC(U$)>96 AND ASC(U$)<123 THEN MID$(X$,L,1)=CHR$(ASC(U$)-32)
3850 NEXT L:RETURN
3860 REMARKerror
3870 IF ERR=53 AND ERL=3900 THEN KY=1:RESUME3990
3875 IF ERL=3420 AND ERR=9 THEN RESUME NEXT
3880 ON ERROR GOTO 0
3890 REMARKread RNAV
3900 OPEN"I",2,"SY1:RNAVLIST.DAT"
3910 INPUT#2,KY
3920 KY=KY+1:DIM LI$(KY),R1$(KY),R1(KY),R2$(KY),R2(KY)
3930 FOR J=1 TO KY-1:LINE INPUT#2,LI$(J)
3940 LINE INPUT#2,R1$(J)
3950 INPUT#2,R1(J)
3960 LINE INPUT#2,R2$(J)
3970 INPUT#2,R2(J)
3980 NEXT J:CLOSE#2
3990 RETURN
4000 REMARKwrite RNAV
4010 OPEN"O",2,"SY1:RNAVLIST.DAT"
4020 PRINT#2,KY-DD
4030 FOR J=1 TO KY:IF LI$(J)="" THEN 4080 ELSE PRINT#2,LI$(J)
4040 PRINT#2,R1$(J)
4050 PRINT#2,R1(J)
4060 PRINT#2,R2$(J)
4070 PRINT#2,R2(J)
4080 NEXT J:CLOSE#2
4090 DD=0:ERASE LI$,R1$,R1,R2$,R2
4100 RETURN
