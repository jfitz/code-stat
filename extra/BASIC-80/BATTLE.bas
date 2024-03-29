10 REM \tBATTLE.BAS \tBATTLESHIP GAME FOR THE H8/H89
20 REM \tAUTHOR FRANK R. NEWCOMER 05/20/78
30 REM \tGRAPHICS ADDED FOR H19 TERMINAL 8-12-79
40 REM \tBY: Bill Phillips \n\t    6 Monterey Cir. \n\t    Ormond Beach, FL 32074
50 CLEAR 5000:REM NEEDS 48K TO RUN LIKE THIS
60 DEFINT A-Z
70 DIM A(9,9),B$(9,9),C$(9),N$(9),T(50,4)
80 DATA " ","A","B","C","D","E","F","G","H","I"
90 DATA " ","1","2","3","4","5","6","7","8","9"
100 E$=CHR$(27):REM ESCAPE
110 E1$=E$+"j":REM SAVE CURSOR POSITION
120 E2$=E$+"k":REM RETURN CURSOR TO OLD POSITION
130 E3$=E$+"I":REM REVERSE INDEX
140 E4$=E$+"p":REM ENTER REVERSE VIDEO MODE
150 E5$=E$+"q":REM EXIT REVERSE VIDEO MODE
160 E6$=E$+"F":REM ENTER GRAPHICS MODE
170 E7$=E$+"G":REM EXIT GRAPHICS MODE
180 E8$=E$+"E":REM CLEAR SCREEN
190 E9$=E$+"Y":REM SEND CURSOR TO FOLLOWING COORDINATES
200 E0$=E$+"l":REM ERASE ENTIRE LINE
210 F2$=E$+"J":REM ERASE FROM CURSOR TO END OF PAGE
220 F3$=E9$+"3 "+F2$:REM GO TO MESSAGE LINE & ERASE TO END OF PAGE
230 C1$="  `  ":D1$="aabaa":REM GRID
240 C2$=" {{{ ":D2$="}{{{|":REM BOX
250 C3$=" "+E4$+"pzp"+E5$+"{":D3$=CHR$(95)+E4$+"    "+E5$:REM Ship
260 C4$=C3$:D4$=CHR$(95)+E4$+" ":D5$="  "+E5$:REM SHIP WITH NUMBER
270 P1$=E1$+E9$:P2$=E7$+E2$
280 P3$=E6$+C1$+E9$:P4$=D1$+P2$
290 P5$=E6$+C2$+E9$:P6$=D2$+P2$
300 P7$=E6$+C3$+E9$:P8$=D3$+P2$
310 REM 
320 DEF FNA$(U,V)=P1$+CHR$(31+U*2)+CHR$(48+V*5)+P3$+CHR$(32+U*2)+CHR$(48+V*5)+P4$
330 DEF FNA1$(U)=P1$+CHR$(32+U*2)+"2"+C$(U)+E2$
340 DEF FNA2$(V)=P1$+CHR$(32)+CHR$(50+V*5)+N$(V)+E2$
350 DEF FNB1$(U,V)=P1$+CHR$(31+U*2)+CHR$(48+V*5)+P5$+CHR$(32+U*2)+CHR$(48+V*5)+P6$
360 DEF FNC1$(U,V)=P1$+CHR$(31+U*2)+CHR$(48+V*5)+P7$+CHR$(32+U*2)+CHR$(48+V*5)+P8$
370 DEF FND1$(U,V)=P1$+CHR$(31+U*2)+CHR$(48+V*5)+E6$+C4$
380 DEF FND2$(U,V)=E9$+CHR$(32+U*2)+CHR$(48+V*5)
390 DEF FND3$(W)=CHR$(95)+E4$+" "+N$(W)+"  "+E5$
400 DEF FND4$(U,V,W)=FND1$(U,V)+FND2$(U,V)+FND3$(W)
410 REM 
420 REM ***** OUTPUT INSTRUCTIONS
430 PRINT E8$;"THE GAME OF BATTLESHIP IS PLAYED ON A 9X9 GRID."
440 PRINT "OBJECT OF THE GAME IS TO LOCATE AND SINK THE"
450 PRINT "SHIPS HIDDEN ON THE GRID."
460 PRINT 
470 PRINT "THE GRID WILL BE IDENTIFIED WITH LETTERS ALONG THE"
480 PRINT "LEFT SIDE AND NUMBERS ACROSS THE TOP. WHEN YOU"
490 PRINT "ARE REQUESTED TO ENTER YOUR GUESS - YOU SHOULD ENTER"
500 PRINT "A LETTER AND NUMBER COMBINATION, SUCH AS A1 INDICATING"
510 PRINT "FIRST ROW - FIRST COLUMN. OR PERHAPS B6 INDICATING"
520 PRINT "SECOND ROW - SIXTH COLUMN."
530 PRINT 
540 PRINT "THERE WILL BE FROM 4 TO 8 SHIPS HIDDEN ON THE GRID."
550 PRINT "EACH SHIP WILL BE FROM 1 TO 6 UNITS LONG."
560 PRINT 
570 PRINT "THE STANDARD VERSION OF THE GAME REQUIRES THAT YOU HIT"
580 PRINT "ALL PARTS OF A SHIP TO SINK IT (A SHIP SIX UNITS LONG"
590 PRINT "WOULD REQUIRE SIX HITS TO SINK IT). YOU MAY HOWEVER,"
600 PRINT "PLAY A MODIFIED VERSION, IN WHICH A SINGLE HIT"
610 PRINT "ANYWHERE ON A SHIP WILL SINK IT."
620 PRINT 
630 INPUT "DO YOU WISH TO PLAY THE MODIFIED VERSION (YES OR NO) ";F1$
640 IF F1$="" THEN 630
650 IF F1$="YES" GOTO 700
660 IF F1$="NO" GOTO 700
670 PRINT E8$;"I NEED A YES OR NO ANSWER TO THE QUESTION."
680 PRINT "PLEASE READ THE INSTRUCTIONS CAREFULLY."
690 GOTO 560
700 FOR I=0 TO 9
710 READ C$(I)
720 NEXT I
730 FOR I=0 TO 9
740 READ N$(I)
750 NEXT I
760 REM ***** CALC NUMBER OF SHIPS AND SET-UP GAME
770 I9=INT(8*RND(1))+1
780 IF I9<4 THEN I9=4
790 PRINT E8$;"THERE WILL BE";I9;"SHIPS IN THIS GAME."
800 PRINT "I WILL NOW HIDE THE SHIPS AND SET-UP THE GAME."
810 PRINT "THIS WILL TAKE ME A LITTLE WHILE, SO PLEASE -"
820 PRINT "BE PATIENT."
830 PRINT :PRINT :PRINT :PRINT :PRINT :PRINT 
840 PRINT "YOU MAY STOP THE GAME AT ANY TIME BY ENTERING - I QUIT."
850 REM ----- ZERO THE ARRAYS
860 FOR I=0 TO 9
870 FOR J=0 TO 9
880 A(I,J)=0
890 B$(I,J)=FNA$(I,J)
900 NEXT J
910 B$(I,0)=FNA1$(I)
920 NEXT I
930 FOR J=0 TO 9
940 B$(0,J)=FNA2$(J)
950 NEXT J
960 M5=3
970 R=0
980 FOR I=1 TO 50
990 FOR J=1 TO 4
1000 T(I,J)=0
1010 NEXT J
1020 NEXT I
1030 REM ----- HIDE THE SHIPS
1040 REM X,Y COORD., Z DIRECTION, AND L LENGTH
1050 G=0
1060 H=0
1070 S9=0
1080 T1=0
1090 FOR I=1 TO I9
1100 L=INT(6*RND(1))+1
1110 IF L<3 THEN L=3
1120 Z=INT(4*RND(1))+1
1130 X=INT(9*RND(1))+1
1140 IF X>7 GOTO 1130
1150 Y=INT(9*RND(1))+1
1160 IF Y>7 GOTO 1150
1170 IF A(X,Y)<>0 GOTO 1130
1180 A(X,Y)=I
1190 T1=T1+1
1200 T(T1,1)=I
1210 T(T1,2)=X
1220 T(T1,3)=Y
1230 FOR K=2 TO L
1240 ON Z GOTO 1250,1270,1290,1320
1250 X=X+1
1260 GOTO 1340
1270 Y=Y+1
1280 GOTO 1340
1290 X=X+1
1300 Y=Y+1
1310 GOTO 1340
1320 X=X+1
1330 Y=Y-1
1340 IF X>9 GOTO 1450
1350 IF Y>9 GOTO 1450
1360 IF X=0 GOTO 1450
1370 IF Y=0 GOTO 1450
1380 IF A(X,Y)<>0 GOTO 1450
1390 A(X,Y)=I
1400 T1=T1+1
1410 T(T1,1)=I
1420 T(T1,2)=X
1430 T(T1,3)=Y
1440 NEXT K
1450 NEXT I
1460 REM ***** PRINT OUT GRID
1470 PRINT E8$;E$;"x5"
1480 FOR I=0 TO 9
1490 FOR J=0 TO 9
1500 PRINT B$(I,J):PRINT E3$;
1510 NEXT J,I
1520 PRINT E$;"y5";
1530 REM ***** GET GUESS
1540 PRINT E9$;"/ "
1550 PRINT "SHOTS ";G
1560 PRINT "HITS  ";H
1570 PRINT F3$;
1580 LINE INPUT "YOUR GUESS? ";G$
1590 IF G$="I QUIT" THEN 2900
1600 IF G$="DISPLAY" GOTO 1470
1610 X$=LEFT$(G$,1)
1620 FOR X=1 TO 9
1630 IF X$=C$(X) GOTO 1680
1640 NEXT X
1650 PRINT "INPUT INCORRECT - TRY AGAIN"
1660 GOSUB 3310
1670 GOTO 1540
1680 Y$=RIGHT$(G$,1)
1690 FOR Y=1 TO 9
1700 IF Y$=N$(Y) GOTO 1740
1710 NEXT Y
1720 GOTO 1650
1730 REM ***** ANALYSIS INPUT
1740 IF B$(X,Y)=FNA$(X,Y) THEN 1780
1750 PRINT "YOU ALREADY TRIED THAT SPOT, DUMMY! NOW TRY AGAIN."
1760 GOSUB 3310
1770 GOTO 1540
1780 G=G+1
1790 B$(X,Y)=FNB1$(X,Y)
1800 IF A(X,Y)<>0 GOTO 2220
1810 R=R+1
1820 IF R<5 THEN 1850
1830 PRINT "YOU HAVE NOW MISSED";R;"TIMES IN A ROW."
1840 GOTO 2120
1850 M=INT(18*RND(1))+1
1860 IF M=M5 GOTO 1850
1870 M5=M
1880 IF M>9 GOTO 3050
1890 ON M GOTO 1900,1920,1940,1960,1990,2010,2030,2060,2080
1900 PRINT "SPLASH, YOU MISSED."
1910 GOTO 2090
1920 PRINT "GUESS WHAT? YOU MISSED."
1930 GOTO 2090
1940 PRINT "ANOTHER SHOT INTO THE DRINK. YOU MISSED."
1950 GOTO 2090
1960 PRINT "GREAT SHOT - WE'LL HAVE FISH FOR LUNCH. NOW LET'S SEE"
1970 PRINT "IF YOU CAN HIT A SHIP."
1980 GOTO 2090
1990 PRINT "IT'S GOOD YOU DON'T HAVE TO BUY YOUR SHELLS - YOU MISSED."
2000 GOTO 2090
2010 PRINT "THE NAVY WANTS YOU TO TAKE A REFRESHER COURSE - YOU MISSED."
2020 GOTO 2090
2030 PRINT "IF YOU DID THIS FOR A LIVING YOU WOULD BE IN BIG TROUBLE."
2040 PRINT "YOU MISSED."
2050 GOTO 2090
2060 PRINT "YOU JUST SANK TWO LOVERS IN A CANOE - SORRY NO POINTS FOR THAT."
2070 GOTO 2090
2080 PRINT "HOORAY! YIPPEE! YOU MISSED."
2090 REM 
2100 GOSUB 3310
2110 PRINT B$(X,Y):PRINT E3$;:GOTO 1540
2120 IF R=10 GOTO 2160
2130 IF R=15 GOTO 2180
2140 IF R=20 GOTO 2200
2150 GOTO 2090
2160 PRINT "DON'T YOU FEEL SILLY?"
2170 GOTO 2090
2180 PRINT "REMEMBER, IF YOU GIVE UP - ENTER I QUIT."
2190 GOTO 2090
2200 PRINT "SURPRISE! YOU HAVE JUST BEEN NAMED 'WORST SHOT OF THE YEAR'."
2210 GOTO 2090
2220 FOR I=1 TO 3:PRINT CHR$(7);:GOSUB 3320:NEXT I:REM RING BELL ON HIT
2230 PRINT "DIRECT HIT!":GOSUB 3310
2240 B$(X,Y)=FNC1$(X,Y)
2250 H=H+1
2260 R=0
2270 REM ***** PROCESS TABLE
2280 FOR I=1 TO T1
2290 IF T(I,2)<>X GOTO 2320
2300 IF T(I,3)<>Y GOTO 2320
2310 GOTO 2350
2320 NEXT I
2330 PRINT "LOGIC PROBLEM - CALL A PROGRAMMER"
2340 STOP 
2350 T(I,4)=1
2360 REM ***** CHECK FOR SHIP SUNK
2370 S=T(I,1)
2380 IF F1$="YES" GOTO 2470
2390 FOR I=1 TO T1
2400 IF T(I,1)=S GOTO 2430
2410 NEXT I
2420 GOTO 2470
2430 IF T(I,4)=0 GOTO 2450
2440 GOTO 2410
2450 IF F2$="YES" GOTO 1540
2460 PRINT B$(X,Y):PRINT E3$;:GOTO 1540
2470 PRINT E3$;F2$;
2480 PRINT "SHIP NUMBER";S;"HAS BEEN SUNK."
2490 S9=S9+1
2500 IF I9-S9=0 GOTO 2560
2510 IF I9-S9=1 GOTO 2540
2520 PRINT I9-S9;"SHIPS TO GO!":GOSUB 3310
2530 GOTO 2580
2540 PRINT "ONLY ONE MORE SHIP TO FIND. HAVING FUN?":GOSUB 3310
2550 GOTO 2580
2560 PRINT 
2570 REM ***** OUTPUT NUMBER TO GRID
2580 FOR I=1 TO T1
2590 IF T(I,1)<>S GOTO 2630
2600 X=T(I,2)
2610 Y=T(I,3)
2620 B$(X,Y)=FND4$(X,Y,S)
2630 PRINT B$(X,Y):PRINT E3$;
2640 NEXT I
2650 REM ***** CHECK FOR ALL SHIPS SUNK
2660 IF I9-S9<>0 GOTO 2450
2670 REM ***** END OF GAME
2680 PRINT F3$
2690 PRINT "TERRIFIC! YOU HAVE SUNK ALL THE SHIPS."
2700 PRINT "OUT OF";G;"SHOTS - YOU HAD";H;"HITS,";
2710 P=INT((H/G)*100)
2720 PRINT " SO YOU HIT";P;"% OF YOUR SHOTS."
2730 IF P>10 GOTO 2760
2740 PRINT "YOU NEED MORE PRACTICE, RIGHT?"
2750 GOTO 2830
2760 IF P>20 GOTO 2790
2770 PRINT "YOU PLAYED A VERY GOOD GAME."
2780 GOTO 2830
2790 IF P>30 GOTO 2820
2800 PRINT "YOU ARE AN EXCELLENT PLAYER."
2810 GOTO 2830
2820 PRINT "WITH A SCORE LIKE THAT YOU ARE READY TO PLAY STAR WARS!"
2830 PRINT
2840 LINE INPUT "DO YOU WISH TO PLAY AGAIN? (YES OR NO) ";A$
2850 IF A$="YES" GOTO 770
2860 IF A$="NO" GOTO 3020
2870 PRINT "I DON'T UNDERSTAND THAT ANSWER."
2880 GOTO 2840
2890 REM ***** PRINT OUT SHIP LOCATIONS
2900 PRINT E8$;E$;"x5";"PLEASE WAIT"
2910 FOR I=1 TO T1
2920 X=T(I,2)
2930 Y=T(I,3)
2940 S=T(I,1)
2950 B$(X,Y)=FND4$(X,Y,S)
2960 NEXT I
2970 FOR I=0 TO 9
2980 FOR J=0 TO 9
2990 PRINT B$(I,J):PRINT E3$;
3000 NEXT J,I
3010 PRINT F3$
3020 PRINT "THANKS FOR PLAYING - COME AGAIN."
3030 PRINT E$;"y5";E7$
3040 END 
3050 M=M-9
3060 ON M GOTO 3070,3090,3120,3140,3160,3180,3200,3250,3290
3070 PRINT "IT SURE IS FUN WHEN YOU HIT A SHIP - BUT YOU MISSED."
3080 GOTO 2090
3090 PRINT "GO DIRECTLY TO JAIL. DO NOT PASS GO."
3100 PRINT "OOPS, SORRY! WRONG GAME --- YOU MISSED."
3110 GOTO 2090
3120 PRINT "BOO-HOO! YOU SANK MY RUBBER DUCKY - NO POINTS FOR YOU."
3130 GOTO 2090
3140 PRINT "THAT WAS THE BEST SHOT SO FAR - BUT - YOU MISSED."
3150 GOTO 2090
3160 PRINT "NOW YOU DID IT!    YOU MISSED."
3170 GOTO 2090
3180 PRINT "HEY! YOU MISSED. REMEMBER THESE SHELLS ARE NOT TAX-DEDUCTIBLE."
3190 GOTO 2090
3200 PRINT "BETTER FIND A LOT OF SMALL TIN CANS -"
3210 PRINT "YOU JUST HIT CHARLIE THE TUNA! (SORRY, CHARLIE)"
3220 GOSUB 3310:GOSUB 3310:PRINT F3$;
3230 PRINT "BY THE WAY - YOU MISSED."
3240 GOTO 2090
3250 PRINT "DIRECT MISS!"
3260 GOSUB 3310
3270 PRINT "GOTCHA - YOU MISSED."
3280 GOTO 2090
3290 PRINT "I'M SORRY TO TELL YOU THIS - BUT - YOU MISSED."
3300 GOTO 2090
3310 FOR K=1 TO 2500:NEXT:RETURN
3320 FOR K=1 TO 200:NEXT:RETURN
