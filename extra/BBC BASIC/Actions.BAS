REM *****************************************
REM PROC_DrawMainScreen - prints title & help
DEF PROC_DrawMainScreen
REM Set background colour
COLOUR 128+7
CLS
COLOUR 0
REM Print title
PRINT TAB(15,0);"Character Generator"
PRINT TAB(15,1);STRING$(19,"=")
REM Print help instructions
PRINT TAB(1,3);"Instructions:"
PRINT TAB(1,4);"Use arrow keys to move cursor."
PRINT TAB(1,5);"Space to toggles selected cell."
PRINT TAB(1,6);"C clears grid, F fills it"
PRINT TAB(1,7);"X or ESC to exits."
ENDPROC
REM *****************************************
REM FN_GetUserAction - returns an action code
DEF FN_GetUserAction
LOCAL Key%,Code%
REPEAT
  REM Wait for keypress
  Key%=GET
  REM Translate key press to action code
  CASE Key% OF
    WHEN 139: Code%=1
    WHEN 137: Code%=2
    WHEN 138: Code%=3
    WHEN 136: Code%=4
    WHEN 32: Code%=5
    WHEN 67 OR 99: Code%=6
    WHEN 70 OR 102: Code%=7
    WHEN 88: Code%=999
    WHEN 120: Code%=999
    OTHERWISE Code%=0
  ENDCASE
UNTIL Code%<>0
=Code%
REM *****************************************
REM PROC_ProcessAction - actions a valid code
DEF PROC_ProcessAction(Code%)
LOCAL Row%,Col%
CASE Code% OF
  WHEN 1: PROC_MoveCursor(1)
  WHEN 2: PROC_MoveCursor(2)
  WHEN 3: PROC_MoveCursor(3)
  WHEN 4: PROC_MoveCursor(4)
  WHEN 5:
    IF Grid%(Cursor.Col%,Cursor.Row%)=1 THEN
      Grid%(Cursor.Col%,Cursor.Row%)=0
    ELSE
      Grid%(Cursor.Col%,Cursor.Row%)=1
    ENDIF
    PROC_DrawCharacter
  WHEN 6:
    FOR Row%=1 TO 8
      FOR Col%=1 TO 8
        Grid%(Row%,Col%)=0
      NEXT Col%
    NEXT Row%
    PROC_DrawCharacter
  WHEN 7:
    FOR Row%=1 TO 8
      FOR Col%=1 TO 8
        Grid%(Row%,Col%)=1
      NEXT Col%
    NEXT Row%
    PROC_DrawCharacter
  WHEN 999:
    REM Set exit flag
    Exit=TRUE
ENDCASE
PROC_DrawCursor
ENDPROC