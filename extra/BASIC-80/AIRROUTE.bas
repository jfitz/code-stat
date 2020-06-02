10 :REM  AIRROUTE.BAS\tNAVPROGseven Route Generation Program     08-Jan-82
20 :REM 
30 :REM  Developed by\tAlan Bose   (AOPA 642188)
40 :REM \t\t\tVice President, Taildragger Flyers
50 :REM \t\t\tRoss Field, Benton Harbor, MI
60 :REM 
70 CLEAR 2000:WIDTH 255:ON ERROR GOTO 1450
80 BL$=CHR$(7):E$=CHR$(27):ER$=E$+"E":PG$=E$+"p":QG$=E$+"q":G$=E$+"F": \n   NG$=E$+"G":Y$=E$+"Y":L$=E$+"l":J$=E$+"j":K$=E$+"k":J1$=E$+"J"
90 DEF FN C$(C1,C2)=Y$+CHR$(C1+31)+CHR$(C2+31)
100 PRINT FNC$(25,1);ER$;E$+"H";ER$;"Standby one..."
110 :REM 
120 OPEN "R",1,"SY1:AIRPORTS.RND"
130 OPEN "R",2,"SY1:AIRINDEX.RND"
140 MD=(LOC(1)*5)-1
150 DIM ID$(MD)
160 FOR J=0 TO MD
170 REC=(JMOD51)+1:SS=J \ 51
180 IF [0xFF][AF](2)<>REC THEN GET #2,REC
190 OPTION #2,SS*5 AS DU$,5 AS ID$
200 ID$(J)=ID$
210 NEXT J:CLOSE#2
220 IM=MD
230 :REM 
240 PRINT ER$;FN C$(1,25);"NAVPROGseven Route Preparation"
250 PRINT FN C$(13,19);"Enter number of checkpoints (10 max.)  <MENU>  ";J$; \n    STRING$(2,95);K$;:LINE INPUT N$:N=VAL(N$)
260 IF N$="" THEN CLOSE:LOAD"MENU",R
270 IF N<2 OR N>10 THEN PRINT BL$;"2 to 10 checkpoints only":GOTO 250
280 :REM 
290 :REM data box
300 PRINT FN C$(2,1);J1$
310 PRINT G$;FN C$(3,6);"f";
320 FOR J=7 TO 74:PRINT "a";:NEXT J:PRINT "c"
330 PRINT FN C$(4+N,6);"e";
340 FOR J=7 TO 74:PRINT "a";:NEXT J:PRINT "d"
350 PRINT FN C$(3,12);"s";FN C$(3,15);"s";FN C$(3,23);"s";FN C$(3,44);"s"; \n    FN C$(3,52);"s";FN C$(3,61);"s";FN C$(3,69);"s"
360 PRINT FN C$(4+N,12);"u";FN C$(4+N,15);"u";FN C$(4+N,23);"u"; \n    FN C$(4+N,44);"u";FN C$(4+N,52);"u";FN C$(4+N,61);"u";FN C$(4+N,69);"u"
370 FOR I=1 TO N
380 PRINT FN C$(3+I,6);"`";FN C$(3+I,12);"`";FN C$(3+I,15);"`"; \n    FN C$(3+I,23);"`";FN C$(3+I,44);"`";FN C$(3+I,52);"`";FN C$(3+I,61);"`"; \n    FN C$(3+I,69);"`";FN C$(3+I,75);"`"
390 NEXT I
400 PRINT NG$
410 PRINT FN C$(2,7);"Ident Fac Freq"
420 PRINT FN C$(2,32);"Name"
430 PRINT FN C$(2,47);"Lat"
440 PRINT FN C$(2,55);"Long"
450 PRINT FN C$(2,64);"Var"
460 PRINT FN C$(2,70);"Elev"
470 :REM 
480 FOR I=1 TO N
490 PRINT FN C$(N+6,1);
500 PRINT "Enter checkpoint";I;
510 PRINT "  <EXIT>  ";J$;STRING$(5,95);K$;:LINE INPUT X$
520 IF X$="" THEN 240
530 IF LEN(X$)>5 THEN PRINT BL$;"5 characters maximum":GOTO 490
540 IF LEN(X$)<2 THEN PRINT BL$;"2 characters minimium":GOTO490
550 GOSUB1410:P$=X$+SPACE$(5-LEN(X$))
560 :REM 
570 :REM search index for match & get
580 RO=I+4
590 FD=0
600 FOR J=0 TO IM
610 IF ID$(J)<>P$ THEN 660
620 IF FD=1 THEN RO=N+8:GET #1,REC:PRINT FNC$(N+8,1);J1$:GOSUB780:RO=N+9:FD=2
630 PI=J
640 IF FD>1 THEN REC=(JMOD5)+1:SS=J \ 5:GET#1,REC:GOSUB 780:FD=FD+1:RO=RO+1
650 IF FD=0 THEN FD=1:REC=(JMOD5)+1:SS=J \ 5:GET#1,REC
660 NEXT J
670 IF FD=0 THEN PRINT BL$;"Can't find ";P$: \n     PRINT"Return to menu and input data?":GOTO 490
680 IF FD=1 THEN 740
690 PRINT FN C$(RO+1,1);"Enter number of your choice  <";PI;">  ";J$; \n     STRING$(3,95);K$;:LINE INPUT X$
700 IF X$="" THEN 740
710 PI=VAL(X$)
720 REC=(PIMOD5)+1:SS=PI \ 5
730 GET #1,REC
740 PRINT FN C$(N+6,1);J1$
750 RO=I+3:FD=0:GOSUB 780:NEXT I
760 GOTO 1020
770 :REM 
780 :REM decode & display
790 REF(I)=PI
800 OPTION #1,SS*50 AS DU$,5 AS ID$,2 AS FAC$,4 AS FR$,20 AS NM$,2 AS D1$, \n     4 AS M1$,2 AS D$,4 AS M$,4 AS V$,1 AS V1$,2 AS EL$
810 F5=CVI(FR$):D6=[0xFF][AA](D1$):M6=CVI(M1$):D5=[0xFF][AA](D$):M5=CVI(M$):V5=CVI(V$): \n     E5=[0xFF][AA](EL$)
820 I$(I)=ID$:FC$(I)=FAC$:FR(I)=F5:P$(I)=NM$
830 M1=M6/60:P2(I)=D6+M1
840 M=M5/60:P1(I)=D5+M
850 V(I)=V5:V$(I)=V1$:EL(I)=E5
860 PRINT FN C$(RO,1);:IF FD=0 THEN PRINT I; :ELSE PRINT PI;
870 PRINT FN C$(RO,7);ID$;FN C$(RO,13);FAC$;
880 PRINT FN C$(RO,16);
890 IF F5=0 THEN PRINT "       ";:GOTO 940
900 IF F5>136 THEN PRINT USING"####";F5;:GOTO 940
910 IF F5*10MOD1=F5*10/1 THEN PRINT USING"####.#";F5;:GOTO 940
920 IF F5*100MOD1=F5*100/1 THEN PRINT USING"####.##";F5;:GOTO 940
930 PRINT USING"###.###";F5;
940 PRINT FN C$(RO,24);NM$;
950 PRINT FN C$(RO,45);USING"##";D6
960 PRINT FN C$(RO,48);USING"##.#";M6;
970 PRINT FN C$(RO,53);USING"###";D5;
980 PRINT FN C$(RO,57);USING"##.#";M5;
990 PRINT FN C$(RO,62);USING"###.#";V5;
1000 PRINT FN C$(RO,68);V1$;FN C$(RO,70);USING"#####";E5
1010 RETURN
1020 :REM 
1030 PRINT FN C$(N+6,1);J1$;;"Route of flight correct? (Y or N)  <Y>  ";: \n     X$=INPUT$(1):PRINT X$:IF X$=CHR$(13) THEN X$="Y"
1040 GOSUB1410:IF X$<>"N" AND X$<>"Y" THEN PRINT BL$:GOTO 1030
1050 IF X$="N" THEN 250
1060 CLOSE
1070 PRINT FNC$(N+6,1);J1$; \n     "Save route of flight for future use?  (Y or N)  <Y>  ";
1080 X$=INPUT$(1):PRINT X$:IF X$=CHR$(13) THEN X$="Y"
1090 GOSUB1410:IF X$<>"N" AND X$<>"Y" THEN PRINT BL$:GOTO1070
1100 IF X$="N" THEN 1120
1110 GOSUB 1170
1120 F$="SY1:FLIGHT.SEQ"
1130 GOSUB 1350
1140 PRINT FNC$(N+6,1);J1$;"Standby one...":LOAD"NAVPROG7",R
1150 END
1160 :REM 
1170 OPEN"I",1,"SY1:ROUTINGS.DAT"
1180 PRINT FNC$(N+6,1);J1$;"Standby one..."
1190 RF$=LEFT$(I$(1),3)+"."+LEFT$(I$(N),3)
1200 INPUT #1,RN
1210 DIM RT$(RN+1)
1220 FOR J=1 TO RN:LINE INPUT #1,RT$(J)
1230 IF RT$(J)=RF$ THEN DR=1
1240 NEXT J:CLOSE
1250 RT$(RN+1)=RF$
1260 IF ASC(RF$)<65 OR ASC(RF$)>90 THEN RF$="X"+RF$
1270 F$="SY1:"+RF$:GOSUB1350
1280 IF DR=1 THEN 1330
1290 OPEN"O",1,"SY1:ROUTINGS.DAT"
1300 PRINT #1,RN+1
1310 FOR J=1 TO RN+1:PRINT #1,RT$(J)
1320 NEXT J:CLOSE
1330 RETURN
1340 :REM 
1350 OPEN"O",1,F$
1360 FOR J=1 TO N
1370 PRINT #1,I$(J)
1380 PRINT #1,REF(J)
1390 NEXT J:CLOSE
1400 RETURN
1410 :REM map lc
1420 FOR L=1 TO LEN(X$):U$=MID$(X$,L,1)
1430 IF ASC(U$)>96 AND ASC(U$)<123 THEN MID$(X$,L,1)=CHR$(ASC(U$)-32)
1440 NEXT L:RETURN
1450 :REM error trap
1460 IF ERR=53 AND ERL=1170 THEN RESUME 1250
1470 ON ERROR GOTO 0