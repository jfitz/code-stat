REM Number filter
Total$=""
DP%=FALSE
PRINT "Type your number or Enter to finish"
REPEAT
  Key%=GET
  REM Check for numeric characters
  IF CHR$(Key%)>="0" AND CHR$(Key%)<="9" THEN
    Total$=Total$+CHR$(Key%)
  PRINT TAB(0,1);Total$;
  ENDIF
  REM Check for decimal point
  IF CHR$(Key%)="." AND NOT DP% THEN
    Total$=Total$+CHR$(Key%)
    DP%=TRUE
    PRINT TAB(0,1);Total$;
  ENDIF
  REM Action backspace only if there's
  REM something to delete
  IF Key%=8 AND LEN(Total$)> 0 THEN
    REM If last character is a point,
    REM reset flag the DP flag
    IF RIGHT$(Total$,1)="." DP%=FALSE
    Total$=LEFT$(Total$)
    REM Print string with trailing space
    REM to erase deleted character
    PRINT TAB(0,1);Total$;" ";
    PRINT TAB(0,1);Total$;
  ENDIF
  UNTIL Key%=13
PRINT "You entered: ";VAL(Total$)
END
