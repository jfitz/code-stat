10 REMARK\t\tBACKGAMM.BAS
20 REMARK\t\tFOR HEATH H-8 OR H-89 WITH H-19
30 REMARK\t\tREQUIRES MICROSOFT
40 REMARK\t\tBy:     R. Wild \n\t\t\tOSO11 Nepil Ave. \n\t\t\tWheaton, IL  60187\t\t\t
50 REMARK\t\tIdea by Cursor Magazine
60 CLEAR1000:PK=12
70 :
80 DIMB(27),D(4),DA$(3),DI$(6,6),US(4),P(24),PR(6),M$(5)
90 DIMT(6)
100 BK$="                                      "
110 M$(1)="HIT YOUR BLOT AT ":M$(2)="COVERED MY BLOT AT ":M$(3)="MOVED TO "
120 M$(5)="MOVED BLOT TO SAFETY AT ":M$(4)="CAN'T MAKE MOVE"
130 M$(0)="BEARING OFF FROM ":FR$=" FROM "
140 A$="":DA$(1)="q":DA$(2)=" ":DA$(3)="*":ER$=A$+""
150 AA$=CHR$(27)+"Y"+CHR$(31+6)+CHR$(33):AL$="XWVUTSRQPONMLKJIHGFEDCBA"
160 A$=CHR$(27)+"Y"+CHR$(31+10)+CHR$(33)
170 FORI=1TO12:READP(I):NEXT
180 DATA.31,.33,.39,.42,.42,.47,.17,.17,.14,.08,.06,.08
190 FORI=0TO6:READPR(I):NEXT
200 DATA0,.03,.11,.25,.44,.69,1
210 GOSUB3170
220 CM=24:F=6:S=0:HM=24:HB=0:CB=0:XC=0:XH=0
230 PRINTLC$;ERAS$;CN$"Difficulty (1 easy, 10 hard)? ";:LINEINPUT IN$
240 PRINTCO$
250 TT=(VAL(IN$)/10):PRINTGRA$
260 IF TT>1ORTT<=0THEN230
270 PRINTERAS$:A=TT:IFRND(1)<0.5THENF=1
280 GOSUB2020:GOSUB2160
290 GOSUB3350:IFD(1)=D(2)THEN290
300 NP=1: PRINTAA$"         MINE     YOURS":FORI=1TO500:NEXT
310 GOSUB3390:FORI=1TO500:NEXT:GOSUB2220
320 PRINTA$;:IFD(2)>D(1)THENNP=-1:PRINT"YOU";
330 IFD(2)<D(1)THENNP=1:PRINT"I";
340 PRINT" GO FIRST!"
350 FORL=1TO1000:NEXT:PRINTAA$;BK$:TU=2:GOTO400
360 D(1)=0:D(2)=0:GOSUB2250
370 IFNP=-1THENGOSUB3100
380 TU=2:GOSUB3350:D(3)=D(1)+D(2)
390 IFD(1)=D(2)THEND(3)=D(1):D(4)=D(1):TU=4
400 GOSUB2250:GOSUB2220
410 MV=0:Y=0:FORI=1TOTU:Y=Y+D(I):US(I)=0:NEXT:IFNP=1THEN1220
420 XH=XH+Y:IN$=""
430 I=1:GOTO450
440 IN$="":FORJ=1TO1000:NEXTJ
450 IFLEN(IN$)>2THENIN$=MID$(IN$,2):GOTO470
460 GOSUB2220:PRINTA$;CN$;"   MOVE";I;"? ";:GOSUB3070:PRINT:PRINT:IFIN$=""THEN1000
470 IFLEN(IN$)>1THEN520
480 IFIN$>="A"ANDIN$<="F"THENIN$="="+IN$
490 IFIN$>="S"ANDIN$<="X"THENIN$=IN$+"-"
500 IFLEN(IN$)>1THEN520
510 PRINT"BAD MOVE.":GOTO440
520 IFLEN(IN$)>3THEN510
530 IFMID$(IN$,2,1)=","THENIN$=LEFT$(IN$,1)+MID$(IN$,3):GOTO530
540 F$=LEFT$(IN$,1)
550 IFF$>="A"ANDF$<="X"THENFM=89-ASC(F$):GOTO570
560 FM=25:IFF$<>"="ANDF$<>"-"THEN510
570 T$=MID$(IN$,2,1)
580 IFT$="="ORT$="-"THENTM=0:GOTO610
590 IFT$<"A"ORT$>"X"THEN510
600 TM=89-ASC(T$):N=0:M=FM-TM:K=1:L=0
610 IFFM=25ANDB(25)=0THENPRINT"YOU HAVE NO MEN ON THE BAR.":GOTO440
620 IFFM<TMTHENPRINT"WRONG DIRECTION.":GOTO440
630 IFB(25)<0ANDFM<>25THENPRINT"YOU HAVE MEN ON THE BAR.":GOTO440
640 IFB(FM)>=0THENPRINT"YOU HAVE NO MEN ON ";F$;".":GOTO440
650 IFTM=0THEN870
660 L=0:FORJ=1TOTU:IFUS(J)THENK=K+1:GOTO690
670 L=L+1:N=N+D(J):IFN=MTHEN700
680 IFD(J)=MTHENK=J:L=1:GOTO700
690 NEXTJ:PRINT"YOU CAN'T MOVE";M;".":GOTO440
700 IFB(TM)>1THENPRINT"POINT ";T$;" IS BLOCKED.":GOTO440
710 IFL=1THENGOSUB2880:US(K)=1:GOTO1200
720 IF B(25)<-1 THEN PRINT"YOU HAVE MEN ON THE BAR.":GOTO 440
730 IFTU=4THEN790
740 IFB(FM-D(1))<=1THENM=1:GOTO770
750 IFB(FM-D(2))<=1THENM=2:GOTO770
760 PRINT"THE PATH IS BLOCKED.":GOTO440
770 N=TM:TM=FM-D(M):GOSUB2880:FM=TM:TM=N::GOSUB2880:I=I+1
780 GOTO1200
790 FORN=1TOL
800 IFB(FM-D(1)*N)>1THEN760
810 NEXTN
820 FORN=KTOJ
830 TM=FM-D(N):US(N)=1:GOSUB2880
840 I=I+1:FM=TM:NEXTN
850 I=I-1:GOTO1200
860 NEXTK
870 FORJ=7TO25:IFB(J)<0THENPRINT"YOU CAN'T CHEAT!!":GOTO440
880 NEXTJ
890 TM=26:FORJ=1TOTU:IFUS(J)=0ANDFM=D(J)THENGOSUB2880:US(J)=1:GOTO1200
900 NEXTJ
910 IFFM=6THEN950
920 FORJ=6TOFM+1STEP-1
930 IFB(J)<0THENPRINT"YOU CAN'T BEAR OFF FROM ";F$;".":GOTO440
940 NEXTJ
950 K=0:D(K)=0:FORJ=1TOTU:IFUS(J)=0ANDD(J)>FMANDD(J)>D(K)THENK=J
960 NEXTJ
970 IFK=0THENPRINT"YOU CAN'T MOVE";FM;"=.":GOTO440
980 PRINT"ASSUMING USE OF";D(K);".":US(K)=1
990 GOSUB2880:GOTO1200
1000 IFB(25)>=0THEN1050
1010 FORJ=1TO6
1020 IFJ=D(TU)ANDUS(TU)=0ANDB(25-J)<2THEN1190
1030 IFJ=D(TU-1)ANDUS(TU-1)=0ANDB(25-J)<2THEN1190
1040 NEXTJ:GOTO1210
1050 FORJ=24TO2STEP-1
1060 IFB(J)>=0THEN1090
1070 IFJ-D(TU-1)>0THENIFUS(TU-1)=0ANDB(J-D(TU-1))<2THEN1190
1080 IFJ-D(TU)>0THENIFUS(TU)=0ANDB(J-D(TU))<2THEN1190
1090 NEXTJ
1100 FORJ=24TO7STEP-1:IFB(J)<0THEN1210
1110 NEXTJ
1120 IFUS(TU)=0ANDB(D(TU))<0THEN1190
1130 IFUS(TU-1)=0ANDB(D(TU-1))<0THEN1190
1140 IFUS(TU)THEND=D(TU-1):GOTO1160
1150 D=D(TU):IFUS(TU-1)=0ANDD<D(TU-1)THEND=D(TU-1)
1160 IFD=6THEN1190
1170 FORJ=6TOD+1:IFB(J)<0THEN1190
1180 NEXTJ:GOTO1210
1190 PRINT"YOU CAN TOO MOVE!":GOTO440
1200 I=I+1:IFI<=TUTHEN450
1210 NP=1:GOTO2660
1220 MO=0:B9=0
1230 XC=XC+Y
1240 K=0
1250 K=K+1:IFB(0)=0ORK>TUTHEN1310
1260 IFB(D(K))<>-1THEN1250
1270 US(K)=1:MO=MO+1
1280 FM=0:TM=D(K):GOSUB2880
1290 PRINT"OFF BAR AND HIT BLOT AT ";MID$(AL$,TM,1)
1300 GOTO1250
1310 K=0
1320 K=K+1:IFB(0)=0ORK>TUTHEN1380
1330 IFB(D(K))<0ORUS(K)>0THEN1320
1340 US(K)=1:MO=MO+1
1350 FM=0:TM=D(K):GOSUB2880
1360 PRINT"OFF BAR TO ";MID$(AL$,TM,1)
1370 GOTO1320
1380 IFB(0)=0ANDMO<TUTHEN1410
1390 IFMO<TUTHENFORI=1TO500:NEXT:PRINTA$;"CAN'T GET OFF BAR":FORI=1TO500:NEXT
1400 GOTO1990
1410 IFCM>6THEN1600
1420 FORJ=1TOTU:L=25-D(J):IFB(L)<1THEN1440
1430 FM=L:TM=27:GOSUB2880:PRINTM$(0);MID$(AL$,L,1):MO=MO+1:GOTO1560
1440 K=6
1450 Y=25-K:IFB(L-K)<1ORB(Y)<-1THEN1490
1460 IFB(Y)=-1THENB(Y)=0
1470 FM=L-K:TM=Y:GOSUB2880
1480 PRINTM$(3);MID$(AL$,Y,1);FR$;MID$(AL$,FM,1):GOTO1560
1490 IFK>1THENK=K-1:GOTO1450
1500 K=1
1510 IFL+K>24THEN1550
1520 IFB(L+K)<1THEN1550
1530 MO=MO+1:FM=L+K:TM=27:GOSUB2880
1540 PRINTM$(0);MID$(AL$,FM,1);" WITH ROLL OF";D(J):GOTO1560
1550 IFK<6THENK=K+1:GOTO1510
1560 IFB(27)=15THEN2660
1570 IFTU=MOTHEN1990
1580 NEXTJ
1590 GOTO1990
1600 IFTU=MOTHEN1990
1610 IFTU-MO<2THEN1770
1620 IFHB=0THEN1770
1630 J=1
1640 IFB(J)<>-1THEN1760
1650 J1=J-D(1):J2=J-D(2):IFJ1<1ORJ2<1THEN1760
1660 IFD(1)=D(2)ANDB(J1)=1THEN1760
1670 IFB(J1)<1ORB(J2)<1THEN1760
1680 IFTU=4THEN1710
1690 IFJ1>18ANDB(J1)=2THEN1760
1700 IFJ2>18ANDB(J2)=2THEN1760
1710 MV=MV-1:TM=J:FM=J1:GOSUB2880:FM=J2:GOSUB2880
1720 PRINTM$(1);MID$(AL$,J,1);FR$;
1730 PRINTMID$(AL$,J1,1);" AND ";MID$(AL$,J2,1):MO=MO+2:HB=HB-1
1740 IFTU=4THENUS(3)=1:US(4)=1
1750 GOTO1600
1760 IFJ<24THENJ=J+1:GOTO1640
1770 TRY=0:Y=1
1780 IFUS(Y)=1THEN1970
1790 GOSUB2490
1800 IFY<>1ORMO<>0ORTU=4ORBM=4THEN1860
1810 S1=BS:F1=FM:M1=BM:Y=2:GOSUB2490
1820 S2=BS:F2=FM:M2=BM:Y=3:G=1:GOSUB2490
1830 G=0:IFBS>S1+S2THENY=TM:TM=FM+D(Y):GOTO1860
1840 IFS2>S1THENFM=F2:Y=2:TM=F2+D(Y):BM=M2:GOTO1860
1850 FM=F1:Y=1:TM=F1+D(Y):BM=M1
1860 IFBM<>4THEN1920
1870 IFF<>1THENF=1:GOTO1790
1880 IFTRY<TU+4THEN1970
1890 FORJ=1TO18:IFB(J)>0ORB9THENPRINTM$(4);:GOTO1990
1900 NEXTJ:B9=1:GOTO1420
1910 GOTO1970
1920 IFBM=1THENHB=HB-1
1930 MO=MO+1:US(Y)=1:IFHM<24-CMTHENBM=3:A=0
1940 GOSUB2880
1950 PRINTM$(BM);MID$(AL$,TM,1);FR$;MID$(AL$,FM,1)
1960 IFMO=TUTHEN1990
1970 Y=Y+1:IFY>TUTHENY=1
1980 GOTO1780
1990 NP=-1
2000 GOTO2660
2010 PRINTM$(MN);MID$(AL$,TM,1);
2020 PRINTERAS$;:H=8:L$=" 2 ":M$="   2 ":R$=" 2"
2030 ESC$=CHR$(27):GR$=ESC$+"F":LC$=ESC$+"G":RV$=ESC$+"p":RVO$=ESC$+"q"
2040 DA$(1)=GR$+"^"+LC$
2050 ERAS$=ESC$+"E":PRINTERAS$
2060 PRINTTAB(39);ESC$;"p";"  A B C D E F  =  G H I J K L  "RVO$
2070 FORI=1TO8
2080 PRINTTAB(39);GR$"i `   `   `   iii `   `   `   i":NEXT
2090 FORI=1TO2
2100 PRINTTAB(39);GR$"i             iii             i"LC$:NEXT
2110 FORI=1TO8
2120 PRINTTAB(39);GR$; "i   `   `   ` iii   `   `   ` i"LC$:NEXT
2130 PRINTTAB(39);ESC$"p  X W V U T S     R Q P O N M  "RVO$
2140 GOTO3450
2150 PRINTRIGHT$(" "+STR$(13-I),2);"&";TAB(21);"&";MID$(STR$(I+12),2):RETURN
2160 FORI=0TO25:B(I)=0:NEXTI:B(26)=-15:B(27)=15
2170 B(1)=2:B(6)=-5:B(8)=-3:B(12)=5
2180 B(13)=-5:B(17)=3:B(19)=5:B(24)=-2
2190 FORI=1TO24:N=B(I):TM=I:FM=27+(N<0)
2200 B(I)=0:IFN<>0THENFORJ=1TOABS(N):GOSUB2880:NEXTJ
2210 NEXTI
2220 PRINTA$;
2230 FORL=1TO3:PRINTBK$:NEXTL
2240 RETURN
2250 PRINTESC$"Y";CHR$(31+6);CHR$(33+9);"  ";:PRINTTEMP$;MID$("YOUR MY ",3+NP+NP,4);:PRINT" ROLL "
2260 PRINTRO$
2270 GOSUB3390
2280 RETURN
2290 PP=0:J6=24:IFK<13THENJ6=K+12
2300 FORI=KTOJ6:IFB(I)<0THENPP=PP+P(I-K)
2310 NEXTI:IFK>14THENPP=PP+P(25-K)*2
2320 RETURN
2330 MS=3:CS=0
2340 Q4=8*PR(BH)*(1+PR(BH))
2350 Q5=8*PR(BC)*(1+PR(BC))
2360 K=P5:GOSUB2290:CZ=PP*(P5+Q4)*A
2370 K=P5+RL:GOSUB2290:PZ=PP
2380 CW=PZ*(P5+RL+Q4)*A
2390 IFB(P5)=2THENCS=CS-CZ
2400 IFB(P5)=1ANDB(K)>0THENCS=CS+CZ:MS=5
2410 IFB(K)=-1THENCS=CS+25-K+Q5:MS=1:IFK>18THENCS=CS-4
2420 IFB(K)<>1THEN2450
2430 CS=CS+CW:IFB(P5)>2THENMS=2
2440 GOTO2480
2450 SM=0:J=20:NN=K+R1:IFNN<20THENJ=NN
2460 FORI=KTOJ:IFB(I)=-1THENSM=SM+P(I-K)*(25-I)
2470 NEXTI:CS=CS-CW+(1-PZ)*SM
2480 RETURN
2490 BS=-999:BM=4:FM=0:TM=0:TRY=TRY+1
2500 FORL=24TO1STEP-1:IFB(L)<1THEN2650
2510 RL=D(Y):R=L+RL:IFR>24THEN2650
2520 IFR<FANDB(R)<>-1THEN2650
2530 IFB(R)<-1THEN2650
2540 IFG<>1THEN2580
2550 Q=0:L1=L+D(1):L2=L+D(2):IFB(L1)>-2THENQ=1
2560 IFB(L2)>-2THENQ=2
2570 IFQ=0THEN2650
2580 P5=L:GOSUB2330
2590 IFG<>1THEN2630
2600 MS=3:IFB(L1)=-1THENQ=1:CS=CS+25-L1:MS=1
2610 IFB(L2)=-1THENQ=2:CS=CS+25-L2:MS=1
2620 K=Q
2630 IFCS<BSTHEN2650
2640 BS=CS:FM=L:TM=K:BM=MS
2650 NEXTL:GOSUB2220:RETURN
2660 IFB(26)=-15THENX$="YOU":X=CP:GOTO2830
2670 IFB(27)=15THENX$="I":X=HP:GOTO2830
2680 HM=0:CM=0:JM=0:HP=0:CP=0:HB=0:CB=0:BC=0:BH=0
2690 FORI=0TO25
2700 IFB(I)<0THENHP=HP-B(I)*I:HM=I
2710 IFB(I)>0THENCP=CP+B(I)*(25-I):JM=25-I
2720 IFJM>CMTHENCM=JM
2730 IFB(I)=-1THENHB=HB+1
2740 IFB(I)=+1THENCB=CB+1
2750 IFI<7ANDB(I)<-1THENBH=BH+1
2760 IFI>18ANDB(I)>1THENBC=BC+1
2770 NEXT
2780 XX=CP:IFHP<XXTHENXX=HP
2790 Y=(CP-HP)/XX:IFY>0.2THENS=1:F=6:A=0.1
2800 IFY<-0.1THENS=0:F=1:A=TT
2810 IFS=1ANDHM<6THENF=HM
2820 GOTO360
2830 PRINTER$A$;X$;" WON BY";X;"POINTS."
2840 PRINTCN$
2850 PRINT"TOTAL POINTS ROLLED";XH;DA$(1);XC;DA$(3):PRINT:INPUT"WANT TO PLAY AGAIN";X$
2860 IFLEFT$(X$,1)="Y"THEN220
2870 PRINTCN$:END
2880 PT=FM:GOSUB2980:P=SGN(B(FM)):GOSUB2980:B(FM)=B(FM)-P
2890 IFFM=25ORFM=0THENPRINTGR$"i";LC$:GOTO2910
2900 IFFM<26THENPRINTGR$;MID$(" `",2+((1ANDPT)=1ORABS(B(PT))>8),1);LC$
2910 IFB(TM)<>-PTHEN2940
2920 PT=-25*(P>0):B(PT)=B(PT)-P:GOSUB2980:B(TM)=0
2930 IFP=-1THENPRINTDA$(3)ELSEPRINTDA$(1)
2940 B(TM)=B(TM)+P:PT=TM:IFTM<26THENGOSUB2980:PRINTDA$(2+P)
2950 IFMV<=-1THENMV=0
2960 PRINTA$:MV=MV+1
2970 RETURN
2980 IFPT>25THENRETURN
2990 IFPT=0ORPT=25THEN3040
3000 VT=ABS(B(PT)):IFPT<13THENVT=19-VT
3010 TB=ABS(1.5625-PT)*2+1:IFTB>12THENTB=TB+4
3020 TB=30-TB
3030 GOTO3060
3040 VT=-B(PT):IFVT<0THENVT=19+VT
3050 TB=15
3060 PRINTESC$"Y";CHR$(31+2+VT);CHR$(31+40+TB);:RETURN
3070 LINEINPUTIN$
3080 PRINTA$;CO$;
3090 RETURN
3100 SI=INP(&O351):LN=SIAND&O376:OUT&O351,LN
3110 GOSUB2220
3120 PRINTA$;"  ";"Press any key to roll."
3130 REM
3140 GOSUB3350
3150 IFINP(&O350)=13THEN3130
3160 OUT&O351,SI:RETURN
3170 REM DICE ROUTINE
3180 E$=CHR$(155):H$=E$+"H"
3190 ER$=E$+"E":PRINTER$;
3200 RO$=E$+"q":PRINTRO$
3210 EG$=E$+"G":PRINTEG$
3220 RV$=E$+"p"
3230 TEMP$=RV$
3240 COF$=E$+"x"+"5":PRINTCOF$
3250 CN$=E$+"y"+"5"
3260 PRINTER$
3270 FORI=1TO6:FORJ=1TO6:READDI$(I,J):NEXTJ,I
3280 DATA"   ","  ^","^  ","^ ^","^ ^","^^^"
3290 DATA" ^ ","   "," ^ ","   "," ^ ","   "
3300 DATA"   ","^  ","  ^","^ ^","^ ^","^^^"
3310 DATA"   ","^  ","  ^","^ ^","^ ^","^ ^"
3320 DATA" ^ ","   "," ^ ","   "," ^ ","^ ^"
3330 DATA"   ","  ^","^  ","^ ^","^ ^","^ ^"
3340 RETURN
3350 FOR I=1TO2
3360 FX=INT(RND(1)*6+1):T(I)=3*(RND(1)>0.5)+3:CT(FX)=CT(FX)+1:D(I)=FX
3370 NEXTI
3380 RETURN
3390 PRINTH$;GR$;:FORI=1TO3:PRINT
3400 FORJ=1TO2
3410 IFDI$(I+T(J),D(J))=""THENDI$(I+T(J),D(J))="   "
3420 PRINTTAB(2*J+7);"} "+DI$(I+T(J),D(J))+" |  ";:NEXTJ,I
3430 PRINTCHR$(27);"G";:PRINT
3440 RETURN
3450 PRINTH$;E$+"F";:PRINTTAB(9);"   {{{{{    {{{{{    "
3460 FORI=1TO3:PRINTTAB(9);"}     |  }     |"
3470 NEXTI
3480 PRINTTAB(10)"zzzzz    zzzzz"
3490 PRINTLC$
3500 RETURN
