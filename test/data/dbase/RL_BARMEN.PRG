* Function..: BARMENU
* Author....: Richard Low
* Syntax....: BARMENU( row, options [,columns [,start [,altkeys [,exitkeys ;
* [,prompts [,prompt_row [,colors ] ] ] ] )
* Returns...: Number of array element option picked, or 0 if escape pressed.
* Parameters: row - Numeric row for bar menu to appear
* options - Array of bar menu option choices
* columns - Optional array of column numbers for each option
* start - Optional starting array element number
* altkeys - Optional list of alternate selection keys
* exitkeys - Optional list of keys to cause a 0 return value exit
* Pass a null string skip (default = escape)
* Pass .F. to disable 0 return value exit altogether
* prompts - Optional array of menu option messages
* promptrow - Optional row number on which these messages appear
* colors - Optional array of colors to use in menu
* Notes.....: Optional parameters are not required, but if you wish to skip
* an optional parameter, you must pass a dummy value. The best
* dummy value to use is a null string '' (set up a memvar named
* dummy where dummy = '').

FUNCTION BARMENU

PARAMETERS p_row, p_options, p_cols, p_choice, p_altkeys, p_exitkeys,;
p_prompts, p_prmtrow, p_colors

PRIVATE f_autocol, f_nextcol, f_prompton, f_incolor, f_canexit,;
f_junk, f_x, f_lkey, f_display, f_menubar, f_barline, f_selected

*-- if the public variable exists and is logical
*-- skip parameter checking

IF .NOT. TYPE('rlib_fast') = 'L'

*-- make sure options array specified
IF TYPE('p_options') != 'A'
RETURN 0
ENDIF

*-- if first parameter is numeric
IF TYPE('p_row') = 'N'
*-- make sure it is in range, if not, default to row
p_row = IF( p_row < 0 .OR. p_row > 24, 1, p_row )
ELSE
*-- else default to row number one
p_row = 1
ENDIF

*-- if p_choice specified make sure it is in range, else default to option 1
p_choice = IF( TYPE('p_choice') = 'N', MIN(MAX(p_choice,1),LEN(p_options)), 1 )

ENDIF

*-- if colums array not specified, build columns automatically
IF TYPE('p_cols') = 'A'
f_autocol = .F.
ELSE
f_autocol = .T.
*-- set up array to hold columns
DECLARE p_cols[ LEN(p_options) ]
ENDIF

*-- messages displayed only if parm is of type array
f_prompton = ( TYPE('p_prompts') = 'A' )

*-- messages displayed on line 24 unles otherwise specified
p_prmtrow = IF( TYPE('p_prmtrow') = 'N', p_prmtrow, 24 )

*-- save incoming color
STORE SETCOLOR() TO f_incolor

*-- use if it is an array AND it has at least 5 elements
IF IF( TYPE('p_colors') = 'A', IF(LEN(p_colors) >= 5, .T., .F.) , .F. )
f_display = p_colors[1] && C - display color
f_menubar = p_colors[2] && C - menu bar color
f_barline = p_colors[4] && C - menu bar line background color
f_selected = p_colors[5] && C - selected option color
ELSE
STORE SETCOLOR() TO f_display, f_barline
f_menubar = GETPARM(2,f_incolor)
f_selected = BRIGHT()
ENDIF

*-- clear the bar menu line and set the background color
SETCOLOR(f_barline)
@ p_row,0

*-- display options and build a list of first letter pick keys
f_junk = ''
SETCOLOR(f_display)

IF f_autocol
*-- if auto building columns, start at column 0
f_nextcol = 0
FOR f_x = 1 TO LEN(p_options)
p_cols[f_x] = f_nextcol && assign column number
@ p_row,p_cols[f_x] SAY p_options[f_x] && say option
f_nextcol = COL() + 2 && next column
f_junk = f_junk + SUBSTR( LTRIM(p_options[f_x]),1,1 )
NEXT f_x
ELSE
FOR f_x = 1 TO LEN(p_options)
@ p_row,p_cols[f_x] SAY p_options[f_x]
f_junk = f_junk + SUBSTR( LTRIM(p_options[f_x]),1,1 )
NEXT f_x
ENDIF

*-- now add any alternate pick keys passed as parameters to the list, if any
p_altkeys = IF( TYPE('p_altkeys') = 'C', f_junk + p_altkeys, f_junk )

*-- if a Logical was passed in place of exit keys, disable exit feature
f_canexit = IF( TYPE('p_exitkeys') = 'L', p_exitkeys, .T. )

*-- see if any exit keys were passed (and not empty), else default to Escape
p_exitkeys = IF( TYPE('p_exitkeys') = 'C', p_exitkeys, CHR(27) )
p_exitkeys = IF( .NOT. EMPTY(p_exitkeys), p_exitkeys, CHR(27) )

DO WHILE .T.

*-- display current selection
SETCOLOR(f_menubar)
@ p_row,p_cols[p_choice] SAY p_options[p_choice]

*-- if message prompts are on, clear row and display in previous color
IF f_prompton
SETCOLOR(f_incolor)
@ p_prmtrow,0
@ p_prmtrow,0 SAY p_prompts[p_choice]
ENDIF

*-- reset display color
SETCOLOR(f_display)

*-- wait for a key
f_lkey = INKEY(0)

DO CASE

CASE f_lkey = 4 .OR. f_lkey = 32
*-- Right Arrow or Space Bar
@ p_row,p_cols[p_choice] SAY p_options[p_choice]
p_choice = IF( p_choice = LEN(p_options), 1, p_choice + 1 )

CASE f_lkey = 19 .OR. f_lkey = 8
*-- Left Arrow or Back Space
@ p_row,p_cols[p_choice] SAY p_options[p_choice]
p_choice = IF( p_choice = 1, LEN(p_options), p_choice - 1 )

CASE f_lkey = 1
*-- Home Key
@ p_row,p_cols[p_choice] SAY p_options[p_choice]
p_choice = 1

CASE f_lkey = 6
*-- End key
@ p_row,p_cols[p_choice] SAY p_options[p_choice]
p_choice = LEN(p_options)

CASE f_lkey = 13
*-- Enter key
EXIT

CASE UPPER(CHR(f_lkey)) $ p_altkeys
@ p_row,p_cols[p_choice] SAY p_options[p_choice]
f_x = 1
p_choice = 0
DO WHILE p_choice = 0
p_choice = AT(UPPER(CHR(f_lkey)),SUBSTR(p_altkeys,f_x,LEN(p_options)))
f_x = f_x + LEN(p_options)
ENDDO
EXIT

CASE f_canexit
IF UPPER(CHR(f_lkey)) $ p_exitkeys
*-- Escape request
p_choice = 0
EXIT
ENDIF

ENDCASE
ENDDO

*-- display selected option in selected color
IF p_choice > 0 .AND. p_choice <= LEN(p_options)
SETCOLOR(f_selected)
@ p_row,p_cols[p_choice] SAY p_options[p_choice]
ENDIF

*-- restore original color
SETCOLOR(f_incolor)

*-- if messages are on, clear the message line
IF f_prompton
@ p_prmtrow,0
ENDIF

RETURN (p_choice)
