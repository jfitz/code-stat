100 DEF FNF(X) = SIN(X)
110 READ A,B,S
120 READ C,D,N
130 LET H = (D-C)/N
140 IF N <= 50 THEN 180
150 PRINT "ONLY 50 SUB-DIVISIONS ALLOWED ON Y-AXIS"
160 STOP
170 DEF FNR(X) = INT(X+0.5)
180 PRINT "Y-AXIS:  FROM " C; "TO " D; "IN STEPS OF " H
190 PRINT
200 PRINT "'";
210 FOR I = 1 TO N-1
220 PRINT "-";
230 NEXT I
240 PRINT "'"
250 FOR X = A TO B STEP S
270 LET Y = FNF(X)
280 LET Y1 = FNR((Y-C)/H)
290 FOR I = 0 TO N
292 IF I = Y1 THEN 302
300 PRINT " ";
301 GOTO 310
302 PRINT "*";
310 NEXT I
315 PRINT "   ";
320 PRINT X
330 NEXT X
340 DATA 0,6.401,0.2
350 DATA -1,1,40
360 END

