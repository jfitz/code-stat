100 PRINT CHR$(27);CHR$(69)
120 PRINT "DISK DIRECTORY"
190 OPEN "I",1,"DIRECT.L"
200 IF EOF(1) GOTO 240
210 INPUT #1,A$
220 PRINT A$
230 GOTO 200
240 CLOSE #1
250 INPUT "PRESS RETURN TO RESUME";Z$:LOAD"REPO",R