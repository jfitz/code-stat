1000 CLS:KEY OFF:CLEAR:SCREEN 0:WIDTH 80
1010 DIM A$(100),FACT$(100),HYPO$(20),Q(50),RULE$(250),RULE(50),S$(200),S(200)
1020 RESTORE
1030 A9=100 : F9=100 : H9=250 : RULE9=250 : S8=200 : S9=200 : S=0 : S2=0
1040 CLS:PRINT "Welcome to the Sea Creature Identification Program."
1050     REM -------------------------------------------------------------------------
1060     REM ---- Load rules and initialize all "RULE" variables
1070     REM -------------------------------------------------------------------------
1080     READ C$
1090 RULE7=0
1100 RULE8=0
1110 IF RULE8<RULE9 THEN 1170
1120 GOTO 6230
1140     REM -------------------------------------------------------------------------
1150     REM ---- Read next rule into RULE$
1160     REM -------------------------------------------------------------------------
1170 RULE8=RULE8+1
1180 READ RULE$(RULE8)
1190 IF RULE$(RULE8)<>"IF" THEN 1220
1200 RULE7=RULE7+1
1210 RULE(RULE7)=RULE8-1
1220 IF RULE$(RULE8)<>"STOP" THEN 1110
1250 REM ********************  RULE DATA ********************
1270 DATA "CREATURE"
1280 DATA "RULE 1","IF","HAS A BACKBONE"
1290 DATA "BREATHES THROUGH A BLOWHOLE"
1300 DATA "HAS A HORIZONTAL TAIL FIN","THEN","IS A CETACEAN"
1310 DATA "RULE 2","IF","IS A CETACEAN","IS GREATER THAN 25 FEET LONG","THEN"
1320 DATA "IS A WHALE"
1330 DATA "RULE 3","IF","IS A CETACEAN","IS APPROXIMATELY 6 FEET LONG"
1340 DATA "HAS A SHORT, BLUNT NOSE","LIVES NEAR THE COAST","THEN"
1350 DATA "IS A PORPOISE"
1360 DATA "RULE 4","IF","IS A CETACEAN","IS APPROXIMATELY 6 FEET LONG"
1370 DATA "HAS LONG BEAK-LIKE NOSE","LIVES AT SEA","THEN","IS A DOLPHIN"
1380 DATA "RULE 5","IF","HAS A BACKBONE"
1390 DATA "BREATHES THROUGH GILLS","HAS A VERTICAL TAIL FIN","THEN","IS A FISH"
1400 DATA "RULE 6","IF","IS A FISH","HAS TRIANGULAR VERTICAL FIN"
1410 DATA "HAS MOUTH UNDERSIDE OF HEAD","HAS SEVERAL ROWS OF TEETH"
1420 DATA "THEN","IS A SHARK"
1430 DATA "RULE 7","IF","IS A FISH","LIVES PRIMARILY IN NORTHERN PACIFIC OCEAN"
1440 DATA "SPAWNS IN FRESHWATER RIVERS","THEN","IS A SALMON"
1450 DATA "RULE 8","IF","IS A FISH","LIVES ON THE BOTTOM OF THE SEA"
1460 DATA "HAS TWO EYES ON ONE SIDE OF ITS HEAD","THEN","IS A FLOUNDER"
1470 DATA "RULE 9","IF","HAS NO BACKBONE","USES A SHELL FOR SKELETON"
1480 DATA "HAS EYES ON STALKS","BREATHES THROUGH GILLS"
1490 DATA "HAS FIVE PAIRS OF LEGS","THEN","IS A CRUSTACEAN"
1500 DATA "RULE 10","IF","IS A CRUSTACEAN","HAS TWO CLAWS"
1510 DATA "HAS A SINGLE HARD-JOINTED SHELL","THEN","IS A CRAB"
1520 DATA "RULE 11","IF","IS A CRUSTACEAN","HAS NO CLAWS","HAS A SOFT SHELL"
1530 DATA "THEN","IS A SHRIMP"
1540 DATA "RULE 12","IF","HAS TWO SHELLS","HAS A WEDGE-SHAPED FOOT","THEN"
1550 DATA "IS A BIVALVE MOLLUSK"
1560 DATA "RULE 13","IF","IS A BIVALVE MOLLUSK","ROAMS THE SEA"
1570 DATA "HAS EYES RIMMING ITS SHELL","THEN","IS A SCALLOP"
1580 DATA "RULE 14","IF","IS A BIVALVE MOLLUSK","LIVES IN BEDS ON THE BOTTOM"
1590 DATA "HAS AN IRREGULAR OVAL-SHAPED SHELL","THEN","IS AN OYSTER"
1600 DATA "RULE 15","IF","IS A BIVALVE MOLLUSK","LIVES IN BEDS ON THE BOTTOM"
1610 DATA "HAS A SIPHON FOR EATING","HAS A CIRCULAR-SHAPED SHELL"
1620 DATA "THEN","IS A CLAM"
1630 DATA "STOP"
2000 IF RULE7>0 THEN 2050
2010 GOTO 6230
2020     REM -------------------------------------------------------------------------
2030     REM ---- Load hypotheses and initialize all H variables
2040     REM -------------------------------------------------------------------------
2050 H8=0
2060 IF H8<H9 THEN 2110
2070 GOTO 6230
2080     REM -------------------------------------------------------------------------
2090     REM ---- Read next hypothesis
2100     REM -------------------------------------------------------------------------
2110 H8=H8+1
2120 READ HYPO$(H8)
2130 IF HYPO$(H8)<>"STOP" THEN 2110
2140 H8=H8-1
2150 REM ******************** HYPOTHESIS DATA ********************
2160 DATA "IS A WHALE","IS A PORPOISE","IS A DOLPHIN","IS A SHARK"
2170 DATA "IS A FLOUNDER","IS A SALMON","IS A CRAB","IS A SHRIMP"
2180 DATA "IS A SCALLOP","IS A CLAM","IS AN OYSTER","STOP"
2190 REM ******************** ******************** ********************
2200 IF H8>0 THEN 2230
2210 PRINT "You have given me no hypotheses to use!"
2220 STOP
2230 PRINT "The knowledge base contains";RULE7;" rules which will be used to"
2240 PRINT "deduce on the following";H8;" hypotheses:"
2250 FOR H1=1 TO H8
2260     PRINT "          ";C$;" ";HYPO$(H1)
2270 NEXT H1
2280 PRINT
2290 PRINT "Answer the following questions with T(true), F(false), or W(why)."
2300 PRINT
2310 REM ********** ********** ********** ********** ********** *********
2320 REM THE MAIN SECTION OF THE PROGRAM
2330 REM ********** ********** ********** ********** ********** *********
2340 A1=0
2350 F1=0
2360 FOR H1=1 TO H8
2370     REM ----
2380     F1$=HYPO$(H1)
2390     Y=1
2400     GOTO 2930
2410     REM -------------------------------------------------------------------------
2420     REM CALL SUBROUTINE READ STRING VARIABLE
2430     REM -------------------------------------------------------------------------
2440     GOSUB 2770
2450     IF X$<>"" THEN 2530
2460 NEXT H1
2470     REM -------------------------------------------------------------------------
2480     REM FAILURE -- CANNOT CONFIRM ANY HYPOTHESIS
2490     REM -------------------------------------------------------------------------
2500 PRINT "No hypothesis can be confimed."
2510 GOTO 2560
2520     REM -------------------------------------------------------------------------
2530     REM SUCCESS -- HYPOTHESIS HAS BEEN CONFIRMED
2540     REM -------------------------------------------------------------------------
2550 PRINT "Final conclusion is ";C$;" ";HYPO$(H1);"."
2560 PRINT
2570 PRINT "Rerun of Quit (R or Q):";
2580 INPUT A$
2590 IF A$="R" OR A$="r" THEN 1020
2600 STOP
2610     REM -------------------------------------------------------------------------
2620     REM SUBROUTINE STORE STRING VARIABLE
2630     REM -------------------------------------------------------------------------
2640 IF S1<S9 THEN 2660
2650 GOTO 6230
2660 S1=S1+1
2670 S$(S1)=X$
2680 RETURN
2690     REM -------------------------------------------------------------------------
2700     REM SUBROUTINE STORE VARIABLE
2710     REM -------------------------------------------------------------------------
2720 IF S2<S8 THEN 2740
2730 GOTO 6230
2740 S2=S2+1
2750 S(S2)=X
2760 RETURN
2770     REM -------------------------------------------------------------------------
2780     REM SUBROUTINE READ STRING VARIABLE
2790     REM -------------------------------------------------------------------------
2800 IF S1>0 THEN 2820
2810 GOTO 6230
2820 X$=S$(S1)
2830 S1=S1-1
2840 RETURN
2850     REM -------------------------------------------------------------------------
2860     REM SUBROUTINE READ VARIABLE
2870     REM -------------------------------------------------------------------------
2880 IF S2>0 THEN 2900
2890 GOTO 6230
2900 X=S(S2)
2910 S2=S2-1
2920 RETURN
2930     REM -------------------------------------------------------------------------
2940     REM SUBROUTINE TO VERIFY THAT FACT IS TRUE AND
2950     REM CHECK TO SEE IF FACT IS ALREADY KNOWN
2960     REM -------------------------------------------------------------------------
2970 F2$=F1$
2980 GOSUB 3810
2990 IF RULE2$<>"" THEN 3680
3000     REM -------------------------------------------------------------------------
3010     REM FACT NOT KNOWN -- TRY TO DEDUCE IT
3020     REM -------------------------------------------------------------------------
3030 GOSUB 4100
3040     REM -------------------------------------------------------------------------
3050     REM ANSWER IS IN Q -- ARE THERE ANY RULES?
3060     REM -------------------------------------------------------------------------
3070 Q8=Q(0)
3080 IF Q8>0 THEN 3160
3090     REM -------------------------------------------------------------------------
3100     REM NO RULES -- ASK THE USER
3110     REM -------------------------------------------------------------------------
3120 GOSUB 4390
3130 IF A1$="" THEN 3730
3140 GOTO 2680
3150     REM -------------------------------------------------------------------------
3160     REM CHAIN BACKWARDS THROUGH RULES
3170     REM -------------------------------------------------------------------------
3180 Q1=1
3190     REM -------------------------------------------------------------------------
3200     REM STORE NEEDED LOCAL VARIABLES
3210     REM -------------------------------------------------------------------------
3220 X$=F1$
3230 GOSUB 2610
3240 X=Y
3250 GOSUB 2690
3260 FOR Q2=1 TO Q8
3270     X=Q(Q2)
3280     GOSUB 2690
3290 NEXT Q2
3300 X=Q8
3310 GOSUB 2690
3320 X=Q1
3330 GOSUB 2690
3340     REM -------------------------------------------------------------------------
3350     REM SET UP ARGUMENTS AND TRY RULES
3360     REM -------------------------------------------------------------------------
3370 RULE1=Q(Q1)
3380 Y = 2
3390 GOTO 5050
3400     REM -------------------------------------------------------------------------
3410     REM SAVE ANSWER AND LOCAL VARIABLES
3420     REM -------------------------------------------------------------------------
3430 GOSUB 2770
3440 X1$=X$
3450 GOSUB 2850
3460 Q1=X
3470 GOSUB 2850
3480 Q8=X
3490 FOR Q2=Q8 TO 1 STEP -1
3500     GOSUB 2850
3510     Q(Q2)=X
3520 NEXT Q2
3530 GOSUB 2850
3540 Y=X
3550 GOSUB 2770
2560 F1$=X$
3570 IF F1$="TRUE" THEN 3680
3580     REM -------------------------------------------------------------------------
3590     REM RULE1 DID NOT WORK TRY ANOTHER
3600     REM -------------------------------------------------------------------------
3610 Q1=Q1+1
3620 IF Q1<=Q8 THEN 3200
3630     REM -------------------------------------------------------------------------
3640     REM NONE OF THE RULES WORKED
3650     REM -------------------------------------------------------------------------
3660 GOTO 3730
3670     REM -------------------------------------------------------------------------
3680     REM SUCCESS
3690     REM -------------------------------------------------------------------------
3700 X$="TRUE"
3710 GOTO 3770
3720     REM -------------------------------------------------------------------------
3730     REM FAILURE
3740     REM -------------------------------------------------------------------------
3750 X$=""
3760     REM -------------------------------------------------------------------------
3770     REM STORE ANSWER AND RETURN
3780     REM -------------------------------------------------------------------------
3790 GOSUB 2610
3800 ON Y GOTO 2420,3410,5200,5710
3810     REM -------------------------------------------------------------------------
3820     REM SUBROUTINE CHECKS TO SEE IF F2$ HAS BEEN RECORDED
3830     REM -------------------------------------------------------------------------
3840 RULE2$=""
3850 IF F1=0 THEN 3910
3860 FOR I=1 TO F1
3870     IF F2$=FACT$(I) THEN 3900
3880 NEXT I
3890 GOTO 3910
3900 RULE2$=F2$
3910 RETURN
3920     REM -------------------------------------------------------------------------
3930     REM SUBROUTINE REMEMBER F2$ -- RECORD FACT
3940     REM -------------------------------------------------------------------------
3950 RULE1$=""
3960     REM -------------------------------------------------------------------------
3970     REM CALL TO SEE IF ALREADY STORED
3980     REM -------------------------------------------------------------------------
3990 GOSUB 3810
4000 IF RULE2$<>"" THEN 4090
4010     REM -------------------------------------------------------------------------
4020     REM ADD NEW FACT
4030     REM -------------------------------------------------------------------------
4040 IF F1<F9 THEN 4060
4050 GOTO 6230
4060 F1=F1+1
4070 FACT$(F1)=F2$
4080 RULE1$=F2$
4090 RETURN
4100     REM -------------------------------------------------------------------------
4110     REM SUBROUTINE FINDS ALL THE RULES THAT HAVE FACT F1$
4120     REM AS A CONSEQUENT
4130     REM -------------------------------------------------------------------------
4140 Q(0)=0
4150     REM -------------------------------------------------------------------------
4160     REM LOOP THROUGH THE RULES
4170     REM -------------------------------------------------------------------------
4180 FOR RULE3=1 TO RULE2
4190     REM -------------------------------------------------------------------------
4200     REM FIND THE CONSEQUENTS OF RULE3
4210     REM -------------------------------------------------------------------------
4220 RULE4=RULE(RULE3)+2
4230 F2$=RULE$(RULE4)
4240 IF F2$="THEN" THEN 4280
4250 RULE4=RULE4+1
4260 GOTO 4230
4270     REM -------------------------------------------------------------------------
4280     REM LOOP THROUGH THE CONSEQUENTS
4290     REM -------------------------------------------------------------------------
4300 RULE4=RULE4+1
4310 F2$=RULE$(RULE4)
4320 IF F2$="STOP" THEN 4370
4330 IF RULE$(RULE4+1)="IF" THEN 4370
4340 IF F2$<>F1$ THEN 4300
4350 Q(0)=Q(0)+1
4360 Q(Q(0))=RULE3
4370 NEXT RULE3
4380 RETURN
4390     REM -------------------------------------------------------------------------
4400     REM SUBROUTINE ASKS ABOUT THE F1$ AND EXPLAINS ITS
4410     REM REASONS
4420     REM -------------------------------------------------------------------------
4430 A$=""
4440     REM -------------------------------------------------------------------------
4450     REM HAVE WE ASKED USER ABOUT F1$ BEFORE?
4460     REM -------------------------------------------------------------------------
4470 IF A1=0 THEN 4520
4480 FOR A2=1 TO A1
4490 IF F1$=A$(A2) THEN 5040
4500 NEXT A2
4510     REM -------------------------------------------------------------------------
4520     REM WE HAVEN’T ASKED BEFORE -- IF ROOM, ASK
4530     REM -------------------------------------------------------------------------
4540 IF A1<A9 THEN 4560
4550 GOTO 6230
4560 A1=A1+1
4570 A$(A1)=F1$
4580 PRINT "True or False?:";C$;" ";F1$;" ";
4590 INPUT A$
4600 IF A$="T" OR A$="t" THEN 4980
4610 IF A$="F" OR A$="f" THEN 5040
4620 IF A$="W" OR A$="w" THEN 4680
4630 GOTO 4580
4640     REM -------------------------------------------------------------------------
4650     REM THE USER WANTS TO KNOW WHY I ASKED THE
4660     REM QUESTION -- ARE WE WORKING ON A RULE?
4670     REM -------------------------------------------------------------------------
4680 IF F1$<>HYPO$(H1) THEN 4760
4690     REM -------------------------------------------------------------------------
4700     REM NO, F1$ IS A TOP-LEVEL HYPOTHESIS
4710     REM -------------------------------------------------------------------------
4720 PRINT "One of the possibilities is ";C$;" ";F1$
4730 PRINT "Unfortunately, I have no way to deduce this concept except to ask you."
4740 GOTO 4580
4750     REM -------------------------------------------------------------------------
4760     REM YES, F1$ IS A SUBGOAL -- PRINT RULE 1
4770     REM -------------------------------------------------------------------------
4780 PRINT "I am trying to use ";RULE$(RULE(RULE1))
4790 RULE3=RULE(RULE1)+2
4800 IF RULE3=RULE2 THEN 4850
4810 PRINT "I already know that:"
4820 PRINT C$;" ";RULE$(RULE3)
4830 RULE3=RULE3+1
4840 IF RULE3<RULE2 THEN 4820
4850 PRINT "If:"
4860 PRINT C$;" ";RULE$(RULE3)
4870 RULE3=RULE3+1
4880 IF RULE$(RULE3)<>"THEN" THEN 4860
4890 PRINT "Then:"
4900 RULE3=RULE3+1
4910 PRINT C$;" ";RULE$(RULE3)
4920 RULE3=RULE3+1
4930 IF RULE$(RULE3)="STOP" THEN 4950
4940 IF RULE$(RULE3+1)<>"IF" THEN 4910
4950 PRINT
4960 GOTO 4580
4970     REM -------------------------------------------------------------------------
4980     REM THE USER SAYS THAT F1$ IS TRUE
4990     REM CALL REMEMBER TO RECORD IT
5000     REM -------------------------------------------------------------------------
5010 F2$=F1$
5020 GOSUB 3920
5030 A1$="TRUE"
5040 RETURN
5050     REM -------------------------------------------------------------------------
5060     REM SUBROUTINE TRY RULE -- TRIES TO APPLY RULE1
5070     REM STORE NEEDED LOCAL VARIABLES
5080     REM -------------------------------------------------------------------------
5090 REM ----
5100 X=RULE1
5110 GOSUB 2690
5120 X=Y
5130 GOSUB 2690
5140     REM -------------------------------------------------------------------------
5150     REM SET UP ARGUMENTS AND GOTO TEST IF
5160     REM -------------------------------------------------------------------------
5170 Y=3
5180 GOTO 5480
5190     REM -------------------------------------------------------------------------
5200     REM READ THE RESULTS OF TEST IF AND
5210     REM RESTORE VARIABLES
5220     REM -------------------------------------------------------------------------
5230 GOSUB 2770
5240 GOSUB 2850
5250 Y=X
5260 GOSUB 2850
5270 RULE1=X
5280 IF X$="" THEN 5400
5290     REM -------------------------------------------------------------------------
5300     REM CALL USE THEN
5310     REM -------------------------------------------------------------------------
5320 GOSUB 5930
5330 IF U1$="" THEN 5400
5340     REM -------------------------------------------------------------------------
5350     REM SUCCESS
5360     REM -------------------------------------------------------------------------
5370 X$="TRUE"
5380 GOTO 5440
5390     REM -------------------------------------------------------------------------
5400     REM FAILURE
5410     REM -------------------------------------------------------------------------
5420 X$=""
5430     REM -------------------------------------------------------------------------
5440     REM STORE THE ANSWER AND RETURN
5450     REM -------------------------------------------------------------------------
5460 GOSUB 2610
5470 ON Y GOTO 2420, 3410, 5060, 5710
5480     REM -------------------------------------------------------------------------
5490     REM SUBROUTINE TEST IF -- CHECKS ANTECEDENTS
5500     REM TO SEE OF RULE1 IS APPLICABLE
5510     REM -------------------------------------------------------------------------
5520 RULE2=RULE(RULE1)+2
5530 F2$=RULE$(RULE2)
5540 IF F2$="THEN" THEN 5850
5550     REM -------------------------------------------------------------------------
5560     REM STORE NEEDED LOCAL VARIABLES
5570     REM -------------------------------------------------------------------------
5580 X=Y
5590 GOSUB 2690
5600 X=RULE1
5610 GOSUB 2690
5620 X=RULE2
5630 GOSUB 2690
5640     REM -------------------------------------------------------------------------
5650     REM SET UP ARGUMENTS AND GOTO VERIFY
5660     REM -------------------------------------------------------------------------
5670 Y=4
5680 F1$=F2$
5690 GOTO 2930
5700     REM -------------------------------------------------------------------------
5710     REM READ ANSWER FROM VERIFY AND RESTORE
5720     REM LOCAL VARIABLES
5730     REM -------------------------------------------------------------------------
5740 GOSUB 2770
5750 2850
5760 RULE2=X
5770 GOSUB 2850
5780 RULE1=X
5790 GOSUB 2850
5800 Y=X
5810 IF X$="" THEN 5890
5820 RULE2=RULE2+1
5830 GOTO 5530
5840     REM -------------------------------------------------------------------------
5850     REM SUCCESS
5860     REM -------------------------------------------------------------------------
5870 X$="TRUE"
5880     REM -------------------------------------------------------------------------
5890     REM STORE ANSWER AND RETURN
5900     REM -------------------------------------------------------------------------
5910 GOSUB 2610
5920 ON Y GOTO 2420, 3410, 5200, 5710
5930     REM -------------------------------------------------------------------------
5940     REM SUBROUTINE USE THEN -- APPLY RULE1 AND
5950     REM PRINTS NEW DEDUCTIONS
5960     REM -------------------------------------------------------------------------
5970 U1$=""
5980     REM -------------------------------------------------------------------------
5990     REM FIND THE CONSEQUENTS
6000     REM -------------------------------------------------------------------------
6010 RULE2=RULE(RULE1)+2
6020 F2$=RULE$(RULE2)
6030 IF F2$="THEN" THEN 6070
6040 RULE2=RULE2+1
6050 GOTO 6020
6060     REM -------------------------------------------------------------------------
6070     REM LOOP THROUGH CONSEQUENTS
6080     REM -------------------------------------------------------------------------
6090 RULE2=RULE2+1
6100 F2$=RULE$(RULE2)
6110 IF F2$="STOP" THEN 6210
6120 IF RULE$(RULE2+1)="IF" THEN 6210
6130     REM -------------------------------------------------------------------------
6140     REM CALL REMEMBER TO ASSERT THE CONSEQUENTS
6150     REM -------------------------------------------------------------------------
6160 GOSUB 3920
6170 IF RULE1$="" THEN 6090
6180 PRINT RULE$(RULE(RULE1));" deduces that the ";C$;" ";F2$
6190 U1$="TRUE"
6200 GOTO 6090
6210 RETURN
6220 STOP
6230 PRINT "An ERROR has occurred. Please check program for errors."
6240 END
