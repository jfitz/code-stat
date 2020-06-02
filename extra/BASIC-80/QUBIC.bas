10 :REM \t\tQUBIC.BAS
20 :REM \t\tOriginally submitted to HUG by: Sam Cox, Ft. Collins, CO 80526
30 :REM \t\tAdapted for MBASIC and H19 Video Terminal, misc.\n\t\tbells & whistles added
40 :REM \t\tBy: Bill Phillips \n\t\t    6 Monterey Cir. \n\t\t    Ormond Beach, FL 32074
50 :REM January 5, 1980
60 :REM 
70 CLEAR 1000
80 DEFINT A-Z
90 DIM LO$(4,4,4)
100 :REM 
110 :REM Set up cursor addresses for playing board
120 FOR I=1 TO 4:FOR J=1 TO 4:FOR K=1 TO 4
130 LO$(I,J,K)=CHR$(33+2*J)+CHR$(34+(K-1)*4+(I-1)*20)
140 NEXT K,J,I
150 :REM 
160 WIDTH 255:PRINT CHR$(27);"v";
170 :REM 
180 :REM H19 control characters
190 ES$=CHR$(27):DC$=ES$+"x5":EC$=ES$+"y5":SC$=ES$+"j":RC$=ES$+"k":EP$=ES$+"J"
200 EG$=ES$+"F":DG$=ES$+"G":RV$=ES$+"p":PV$=ES$+"q":AC$=ES$+"Y":EL$=ES$+"l"
210 :REM 
220 PRINT ES$;"E";
230 INPUT "DO YOU WANT INSTRUCTIONS? <NO> ";A$:IF A$="" THEN 430
240 IF ASC(A$)<>89 AND ASC(A$)<>121 THEN 430
250 :REM 
260 PRINT 
270 PRINT "QUBIC IS A COMPUTER IMPLEMENTATION OF 3-DIMENSIONAL TIC-TAC-TOE."
280 PRINT
290 PRINT "IT IS PLAYED ON A 4 BY 4 BY 4 PLAYING CUBE.  THE OBJECT IS TO PLACE"
300 PRINT "FOUR MARKERS IN A ROW IN ANY DIRECTION."
310 PRINT:PRINT "THIS VERSION OF QUBIC IS WRITTEN TO ALLOW:"
320 PRINT "           1. COMPUTER VS. COMPUTER"
330 PRINT "           2. COMPUTER VS. HUMAN"
340 PRINT "           3. HUMAN VS. HUMAN"
350 PRINT:PRINT "THE PROGRAM IS VERY DEFENSIVE, ALSO PARANOID,";
360 PRINT " AND IS VERY DIFFICULT TO BEAT."
370 PRINT
380 PRINT "A MOVE IS ENTERED AS A THREE DIGIT NUMBER INDICATING THE LEVEL, ROW"
390 PRINT "AND COLUMN. (EACH DIGIT BETWEEN ONE AND FOUR)"
400 PRINT:PRINT "THE BEST OF LUCK (YOU'LL NEED IT)."
410 PRINT:LINE INPUT "(Press RETURN when ready to start the game)";A$
420 :REM 
430 PRINT:PRINT "Please Wait"
440 :
450 :REM READ IN DATA ARRAYS
460 DIM P(76,4):FOR I=1 TO 76:FOR J=1 TO 4:READ P(I,J):NEXT J,I::REM -- PATHS
470 DIM I(64,7):FOR I=1 TO 64:FOR J=1 TO 7:READ I(I,J):NEXT J,I::REM -- INDEX
480 :REM 
490 DIM M(64)::REM -- MOVES
500 DIM V(76)::REM -- VALUPATH
510 DIM N(76)::REM -- NUPATH
520 F1=1::REM -- VARIABLES FLAG
530 :REM 
540 B$(0)=RV$+"  "+PV$:B$(1)="ww":B$(2)="  "
550 C$(0)="WHITE":C$(2)="BLACK"
560 :REM 
570 PRINT ES$;"E";:GOSUB 1870::REM \t\t\t-- START GAME HERE
580 PRINT AC$;", ";
590 :REM ZERO ARRAYS
600 ERASE M:DIM M(64):ERASE V:DIM V(76):ERASE N:DIM N(76)
610 C1=0: W=0
620 :
630 Z$="":PRINT "COMPUTER VS COMPUTER? (Y/N) ";
640 :  GOSUB 1720
650 :  IF Z1=1 THEN F=1: F2=1: GOTO 850
660 :
670 PRINT AC$;", ";EL$;"HUMAN VS HUMAN? (Y/N) ";
680 :  GOSUB 1720
690 :  F2=0: IF Z1=0 THEN 720
700 F=1:F2=2:INPUT "FIRST PLAYER'S NAME";N$(2)
710 INPUT "SECOND PLAYER'S NAME";N$(0):GOTO 850
720 PRINT AC$;", ";EL$;"COMPUTER GOES FIRST? (Y/N) ";
730 :  GOSUB 1720
740 INPUT "YOUR NAME";N$(2)
750 :  F=1: IF Z1=1 THEN F=-1: GOTO 850
760 :
770 :REM UPDATE BOARD\t\t\t\t-- PLAY LOOP ENTRY
780 PRINT DC$;EG$;SC$;AC$;LO$(L+1,R+1,C);
790 FOR I=1 TO 6
800 PRINT B$((I\2)*2);CHR$(8);CHR$(8);:FOR J=1 TO 150:NEXT J
810 NEXT I
820 PRINT B$(M(M)+1);RC$;EC$;DG$;
830 :REM 
840 IF W<>0 OR C1=64 THEN 920
850 C1=C1+1
860 ON F+2+F2 GOSUB 1110,1110,1540,1110,1540
870 M(M)=F
880 GOSUB 1780
890 F=-F
900 GOTO 780
910 :
920 PRINT AC$;"- "::REM \t\t\t\t-- END GAME HERE
930 :REM 
940 FOR I=1 TO 3:FOR J=1 TO 150:NEXT J:PRINT CHR$(7);:NEXT I::REM RING BELL
950 IF W=0 THEN PRINT EP$;"CAT'S GAME! ";:GOTO 1060
960 IF F2=1 THEN PRINT C$(SGN(W)+1);" WINS!";:GOTO 990
970 IF F2=0 THEN N$(0)="COMPUTER"
980 PRINT N$(SGN(W)+1);" (";C$(SGN(W)+1);") WINS!";
990 PRINT TAB(27);"LVL   ROW   COL";
1000 :  FOR J=1TO 4
1010 :  M=P(ABS(W),J)
1020 :  L=(M-1)MOD16
1030 :  R=(M-16*L-1)MOD4
1040 :  PRINT :PRINT TAB(27);L+1;TAB(33);R+1;TAB(39);M-16*L-4*R;
1050 :  NEXT J
1060 PRINT:PRINT:PRINT "WANT TO PLAY AGAIN? (Y/N) ";
1070 :  GOSUB 1720
1080 :  IF Z1=1 THEN 570
1090 END
1100 :
1110 PRINT AC$;". ";EL$;::REM \t\t\t-- COMPUTER'S MOVE
1120 IF F2=0 THEN PRINT EP$;
1130 PRINT "COMPUTER (";C$(F+1);") THINKING...";
1140 IF C1=1 THEN 1370::REM \t\t\t-- FIRST MOVE?
1150 :
1160 FOR I=1TO 64::REM \t\t\t\t-- A WINNING MOVE?
1170 IF V(I)=3*F THEN 1420
1180 NEXT I
1190 :
1200 FOR I=1TO 64::REM \t\t\t\t-- BLOCK OPPONENT'S WIN?
1210 IF V(I)=-3*F THEN 1420
1220 NEXT I
1230 :
1240 M=0:V2=0::REM \t\t\t\t\t-- NO, EVALUATE PATHS
1250 FOR I=1TO 64
1260 IF M(I)<>0 THEN 1340
1270 IF I>15 THEN PRINT ".";
1280 V1=0
1290 :  FOR J=1TO 7
1300 :  P=I(I,J):IF P=0 THEN 1320
1310 :  IF ABS(V(P))=N(P) THEN V1=V1+V(P)*V(P)
1320 :  NEXT J
1330 IF V1>V2 THEN V2=V1: M=I
1340 NEXT I
1350 :
1360 IF M<>0 THEN 1470::REM \t\t\t-- FOUND GOOD MOVE?
1370 :  M1=64*RND(1)+1::REM \t\t\t-- PICK ONE AT RANDOM
1380 :  FOR M=M1TO 64
1390 :  IF M(M)=0 THEN 1470
1400 :  NEXT M: M1=1: GOTO 1380
1410 :
1420 FOR J=1TO 4::REM \t\t\t\t-- A MUST MOVE!
1430 PRINT ".";
1440 IF M(P(I,J))=0 THEN M=P(I,J)
1450 NEXT J
1460 :
1470 L=(M-1)MOD16:M1=M-16*L::REM \t\t\t-- DISPLAY MOVE
1480 R=(M1-1)MOD4
1490 C=M1-4*R
1500 PRINT CHR$(7);EL$:PRINT "COMPUTER (";C$(F+1);") MOVES TO"
1510 PRINT "LEVEL";L+1;CHR$(8);", ROW";R+1;CHR$(8);", COLUMN";M1-4*R
1520 RETURN 
1530 :
1540 PRINT AC$;"1 ";EP$;::REM \t\t\t-- HUMAN'S MOVE
1550 PRINT "YOUR TURN, ";N$(F+1);" (";C$(F+1);")"
1560 PRINT "LEVEL ?";:GOSUB 1640
1570 IF EF=-1 THEN 1670
1580 L=VAL(M$)-1:PRINT ", ROW ?";:GOSUB 1640
1590 IF EF=-1 THEN 1670
1600 R=VAL(M$)-1:PRINT ", COLUMN ?";:GOSUB 1640
1610 IF EF=-1 THEN 1670
1620 C=VAL(M$):GOTO 1680
1630 :REM GET INPUT CHARACTER & TEST IT
1640 M$=INPUT$(1):IF ASC(M$)<49 OR ASC(M$)>52 THEN EF=-1:RETURN
1650 PRINT CHR$(8);M$;:RETURN
1660 :REM BAD INPUT - PRINT ERROR MESSAGE
1670 PRINT:PRINT CHR$(7);"ILLEGAL MOVE";AC$;"2 ";EL$;:EF=0:GOTO 1560
1680 M=L*16+R*4+C
1690 IF M(M)<>0 THEN 1670
1700 RETURN 
1710 :
1720 Z$=INPUT$(1)::REM \t\t\t\t-- YES OR NO?
1730 IF ASC(Z$)=78 OR ASC(Z$)=110 THEN PRINT "No":Z1=0:RETURN
1740 IF ASC(Z$)=89 OR ASC(Z$)=121 THEN PRINT "Yes":Z1=1:RETURN
1750 :  PRINT CHR$(7);: GOTO 1720
1760 :
1770 :
1780 FOR I=1TO 7::REM \t\t\t\t-- UPDATE VALUPATH
1790 P=I(M,I): IF P=0 THEN 1830
1800 V(P)=V(P)+F
1810 N(P)=N(P)+1
1820 IF ABS(V(P))=4 THEN W=SGN(V(P))*P
1830 NEXT I
1840 RETURN 
1850 :REM 
1860 :REM PRINT THE BOARD
1870 FOR I=1 TO 4:PRINT "      LEVEL";I;"     ";:NEXT:PRINT:PRINT:
1880 PRINT DC$;EG$;
1890 FOR I=1 TO 9:FOR J=1 TO 4:FOR K=1 TO 18
1900 PRINT "i";:NEXT K:PRINT "  ";
1910 NEXT J:NEXT I
1920 FOR I=1 TO 4:FOR J=1 TO 4:FOR K=1 TO 4
1930 PRINT SC$;AC$;LO$(I,J,K);"ww";RC$;:NEXT K,J,I
1940 PRINT DG$;EC$;:RETURN
1950 :REM 
1960 DATA 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25
1970 DATA 26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48
1980 DATA 49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,1,5,9,13,2,6,10,14
1990 DATA 3,7,11,15,4,8,12,16,17,21,25,29,18,22,26,30,19,23,27,31,20,24,28,32
2000 DATA 33,37,41,45,34,38,42,46,35,39,43,47,36,40,44,48,49,53,57,61,50,54
2010 DATA 58,62,51,55,59,63,52,56,60,64,1,6,11,16,4,7,10,13,17,22,27,32,20,23
2020 DATA 26,29,33,38,43,48,36,39,42,45,49,54,59,64,52,55,58,61,1,17,33,49,2
2030 DATA 18,34,50,3,19,35,51,4,20,36,52,5,21,37,53,6,22,38,54,7,23,39,55,8
2040 DATA 24,40,56,9,25,41,57,10,26,42,58,11,27,43,59,12,28,44,60,13,29,45,61
2050 DATA 14,30,46,62,15,31,47,63,16,32,48,64,1,21,41,61,2,22,42,62,3,23,43,63
2060 DATA 4,24,44,64,13,25,37,49,14,26,38,50,15,27,39,51,16,28,40,52,1,18,35
2070 DATA 52,5,22,39,56,9,26,43,60,13,30,47,64,4,19,34,49,8,23,38,53,12,27,42
2080 DATA 57,16,31,46,61,1,22,43,64,4,23,42,61,13,26,39,52,16,27,38,49,1,17,33
2090 DATA 41,57,65,73,1,18,42,58,0,0,0,1,19,43,59,0,0,0,1,20,34,44,60,69,74,2
2100 DATA 17,45,66,0,0,0,2,18,33,46,0,0,0,2,19,34,47,0,0,0,2,20,48,70,0,0,0
2110 DATA 3,17,49,67,0,0,0,3,18,34,50,0,0,0,3,19,33,51,0,0,0,3,20,52,71,0,0,0
2120 DATA 4,17,34,53,61,68,75,4,18,54,62,0,0,0,4,19,55,63,0,0,0,4,20,33,56,64
2130 DATA 72,76,5,21,35,41,0,0,0,5,22,42,65,0,0,0,5,23,43,69,0,0,0,5,24,36,44
2140 DATA 0,0,0,6,21,45,57,0,0,0,6,22,35,46,58,66,73,6,23,36,47,59,70,74,6,24
2150 DATA 48,60,0,0,0,7,21,49,61,0,0,0,7,22,36,50,62,67,75,7,23,35,51,63,71,76
2160 DATA 7,24,52,64,0,0,0,8,21,36,53,0,0,0,8,22,54,68,0,0,0,8,23,55,72,0,0,0
2170 DATA 8,24,35,56,0,0,0,9,25,37,41,0,0,0,9,26,42,69,0,0,0,9,27,43,65,0,0,0
2180 DATA 9,28,38,44,0,0,0,10,25,45,61,0,0,0,10,26,37,46,62,70,76,10,27,38,47
2190 DATA 63,66,75,10,28,48,64,0,0,0,11,25,49,57,0,0,0,11,26,38,50,58,71,74,11
2200 DATA 27,37,51,59,67,73,11,28,52,60,0,0,0,12,25,38,53,0,0,0,12,26,54,72,0
2210 DATA 0,0,12,27,55,68,0,0,0,12,28,37,56,0,0,0,13,29,39,41,61,69,76,13,30
2220 DATA 42,62,0,0,0,13,31,43,63,0,0,0,13,32,40,44,64,65,75,14,29,45,70,0,0,0
2230 DATA 14,30,39,46,0,0,0,14,31,40,47,0,0,0,14,32,48,66,0,0,0,15,29,49,71,0
2240 DATA 0,0,15,30,40,50,0,0,0,15,31,39,51,0,0,0,15,32,52,67,0,0,0,16,29,40
2250 DATA 53,57,72,74,16,30,54,58,0,0,0,16,31,55,59,0,0,0,16,32,39,56,60,68,73