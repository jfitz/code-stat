* Function: MULTIMENU
* Author..: Richard Low
* Syntax..: MULTIMENU( top, left, bottom, right, options [, columns ;
* [, messages [, message_row [, colors ] ] ] ] )
* Returns.: choice = - number of array element option picked, or
* 0 (zero) if escape was pressed
* Notes...: If a parameter is not used, must pass a dummy parameter.
* Where...: top = - top row number of window
* left = - top left corner of menu box
* bottom =
* right =
* options = - array of choices
* columns = - Optional number of columns
* messages = - Optional array of choice messages
* mess_row = - Optional row # to center messages
* colors = - Optional ARRAY of color settings

FUNCTION MULTIMENU
PARAMETERS p_top, p_left, p_bottom, p_right, p_opts, p_cols,;
p_mess, p_messrow, p_colors

*-- all parameter variables identified with 'p_'
*-- all local (function) variables identified with 'f_'

PRIVATE f_mess_on, f_widest, f_incolor, f_selected, f_menubar, f_space,;
f_filler, f_choice, f_firstopt, f_lastopt, f_lastrow, f_lastcol,;
f_row, f_col, f_x

*-- verify that all required parameters are the correct type
IF TYPE('p_top') + TYPE('p_left') + TYPE('p_bottom') +;
TYPE('p_right') + TYPE('p_opts') != 'NNNNA'
RETURN 0
ENDIF

*-- verify the window coordinates are within bounds and in the correct order
IF .NOT. ( p_top >= 0 .AND. p_top < 25 .AND.;
p_left >= 0 .AND. p_left < 80 .AND.;
p_bottom > p_top .AND. p_bottom < 25 .AND.;
p_right > p_left .AND. p_right < 80 )
RETURN 0
ENDIF

*-- verify there is at least 1 element in the options array
IF LEN(p_opts) = 0
RETURN 0
ENDIF

*-- messages displayed only if parmameter is an array
f_mess_on = ( TYPE('p_mess') = 'A' )

*-- messages displayed on line 24 unles otherwise specified
p_messrow = IF( TYPE('p_messrow') = 'N', p_messrow, 24 )


*-- get the widest option from the array
f_widest = 1
FOR f_x = 1 TO LEN(p_opts)
f_widest = MAX( f_widest, LEN(p_opts[f_x]) )
NEXT f_x


*-- if # columns not specified, or skipped with wrong data type
IF TYPE('p_cols') != 'N'
p_cols = 0
ENDIF

*-- from above or if zero passed
IF p_cols = 0
*-- use as many columns as can fit with widest option in window
p_cols = INT( (p_right - p_left + 1) / (f_widest + 1) ) + 1
ENDIF


*-- make sure the number of columns specified will fit on screen
*-- allowing a minimum of 1 space between each option
DO WHILE ( ( f_widest + 1 ) * p_cols ) > ( p_right - p_left + 1 )
*-- if not, trim down the number of columns (sorry!)
p_cols = p_cols - 1
ENDDO


*-- if the widest option was too wide to fit in the window, bomb out
IF p_cols < 1
RETURN 0
ENDIF


*-- set up array to hold column numbers
DECLARE f_column[p_cols]

*-- default minimum amount of space between column options is 1 space
f_filler = 1

*-- if number of columns is more than 1, (why else would this UDF be used)
*-- calculate column positions based on widest option, # columns, and window
IF p_cols > 1
*-- amount of space to use for filler between columns
f_space = (p_right - p_left + 1) - (f_widest * p_cols)
*-- divvy white space up between the columns
f_filler = f_space / (p_cols - 1)
*-- make sure remainders dont screw it all up, trim down filler if needed
DO WHILE (((f_widest + f_filler) * (p_cols - 1)) + f_widest) > (p_right-p_left+1)
f_filler = f_filler - 1
ENDDO
*-- make sure it results to positive
f_filler = MAX( f_filler, 1 )
ENDIF


*-- now fill column array with column numbers, starting at left position
f_column[1] = p_left
FOR f_x = 2 TO p_cols
f_column[f_x] = f_column[f_x-1] + f_widest + f_filler
NEXT f_x

*-- now convert filler number to spaces
f_filler = IF( f_filler > 1, SPACE(f_filler), ' ' )


*****************************************************************************
** now we are in business, having checked for most all that can go wrong **
*****************************************************************************


*-- save incoming color
STORE SETCOLOR() TO f_incolor

*-- use if it is an array AND it has at least 5 elements
IF IF( TYPE('p_colors') = 'A', IF(LEN(p_colors) >= 5, .T., .F.) , .F. )
f_display = p_colors[1] && display color
f_menubar = p_colors[2] && menu bar color
f_selected = p_colors[5] && selected option color
ELSE
f_display = SETCOLOR()
f_selected = BRIGHT()
f_menubar = GETPARM(2,f_incolor)
ENDIF


*-- first time in, start at first array element
f_firstopt = 1

*-- store the last column used
f_lastcol = p_cols && maximum last column is actual last column


*-- now display the options in the window
DO f_say_opts


DO WHILE .T.
SETCOLOR(f_menubar)
f_choice = f_element(f_row,f_col)
@ f_row,f_column[f_col] SAY p_opts[f_choice]
SETCOLOR(f_display)

IF f_mess_on
@ p_messrow,0
@ p_messrow,(80-LEN(p_mess[f_choice]))/2 SAY p_mess[f_choice]
ENDIF
lkey = INKEY(0)

*-- put current selection back in normal video
@ f_row,f_column[f_col] SAY p_opts[f_choice]

DO CASE

CASE lkey = 13
*-- Enter key
EXIT

CASE lkey = 27
*-- Escape key
f_choice = 0
EXIT

CASE lkey = 24 .OR. lkey = 32
*-- Down Arrow or Space Bar

DO CASE
*-- first try same column, one row down
CASE f_element(f_row+1,f_col) <= f_lastopt
f_row = f_row + 1

*-- next try top of next column to right
CASE f_element(p_top,f_col+1) <= f_lastopt
f_row = p_top
f_col = f_col + 1

*-- else must be at bottom right corner, so go to beginning
OTHERWISE
f_row = p_top
f_col = 1

ENDCASE


CASE lkey = 5
*-- Up Arrow

DO CASE
*-- first try going up one row in the current column
CASE f_element(f_row-1,f_col) <= f_lastopt
f_row = f_row - 1

*-- next try going to the bottom (last row used) of column to left
CASE f_element(f_lastrow,f_col-1) <= f_lastopt
f_row = f_lastrow
f_col = f_col - 1

*-- after that, try one row up from last row used
CASE f_element(f_lastrow-1,f_col-1) <= f_lastopt
f_row = f_lastrow - 1
f_col = f_col - 1

*-- then must be on first option, so try to go to end
CASE f_element(f_lastrow,f_lastcol) <= f_lastopt
f_row = f_lastrow
f_col = f_lastcol

*-- if that didn't work, row dind't fill to end so go up 1
OTHERWISE
f_row = f_lastrow - 1
f_col = f_lastcol

ENDCASE


CASE lkey = 4 .OR. lkey = 32
*-- Right Arrow or Space Bar

DO CASE
*-- first try same row, one column over
CASE f_element(f_row,f_col+1) <= f_lastopt
f_col = f_col + 1

*-- next try first column, one row down
CASE f_element(f_row+1,1) <= f_lastopt
f_row = f_row + 1
f_col = 1

*-- otherwise, go to beginning (may want to disable this)
OTHERWISE
f_row = p_top
f_col = 1

ENDCASE


CASE lkey = 19 .OR. lkey = 8
*-- Left Arrow or Back Space

DO CASE
*-- first try same row, one column to the left
CASE f_element(f_row,f_col-1) <= f_lastopt
f_col = f_col - 1

*-- next try last column, one row up
CASE f_element(f_row-1,f_lastcol) <= f_lastopt
f_row = f_row - 1
f_col = f_lastcol

*-- then must be on first option, so try to go to end
CASE f_element(f_lastrow,f_lastcol) <= f_lastopt
f_row = f_lastrow
f_col = f_lastcol

*-- if that didn't work, row didn't fill to end so go up 1
OTHERWISE
f_row = f_lastrow - 1
f_col = f_lastcol

ENDCASE

CASE lkey = 3
*-- Page Down key
IF f_lastopt < LEN(p_opts) && see if any more elements exist
f_firstopt = f_lastopt + 1 && position one beyond last
DO f_say_opts && re-display new options set
ENDIF


CASE lkey = 18
*-- Page Up key
IF f_firstopt > 1 && see if not at top
*-- if on a second page, then the previous page must
*-- have been filled, so subtract options per page
f_firstopt = f_firstopt - ( (p_bottom - p_top + 1) * p_cols )
DO f_say_opts && re-display new options set
ENDIF


CASE lkey = 1
*-- Home Key
f_row = p_top
f_col = 1


CASE lkey = 6
*-- End key

*-- try to go to the end
IF f_element(f_lastrow,f_lastcol) <= f_lastopt
f_row = f_lastrow
f_col = f_lastcol
ELSE
*-- if that didn't work, row didn't fill to end so go up 1
f_row = f_lastrow - 1
f_col = f_lastcol
ENDIF

ENDCASE
ENDDO

IF f_choice > 0 .AND. f_choice <= LEN(p_opts)
SETCOLOR(f_selected)
@ f_row,f_column[f_col] SAY p_opts[f_choice]
ENDIF

*-- if messages are on, clear the message line
IF f_mess_on
@ p_messrow,0
ENDIF

*-- restore original color, redraw box
SETCOLOR(f_incolor)
RETURN (f_choice)




*****************************************************************************
* Procedure: F_SAY_OPTS
* Notes....: Sub-routine to display the optins in the window
* Assumes..: The memvar is the array element number
* to use in starting the display.
*****************************************************************************
PROCEDURE f_say_opts

*-- set up LAST values
f_lastopt = LEN(p_opts) && default last array element
f_lastrow = p_bottom && maximun last row is actual last row used

*-- starting display controls
STORE p_top TO f_row, f_lastrow
STORE 1 TO f_col, f_lastcol

SETCOLOR(f_display) && use display color
SCROLL(p_top, p_left, p_bottom, p_right, 0) && clear window for display

FOR f_x = f_firstopt TO LEN(p_opts) && display starting at first
IF f_col > p_cols && when we get to last column
f_col = 1 && loop around
f_row = f_row + 1 && and down one row
ENDIF
IF f_row > p_bottom && if row is below the bottom
f_lastopt = f_x - 1 && tag last array element used
EXIT && and stop listing elements
ENDIF
@ f_row,f_column[f_col] SAY p_opts[f_x] && display this option
f_lastrow = f_row && tag the last row used
f_lastcol = MAX( f_col, f_lastcol ) && tag farthest column used
f_col = f_col + 1 && next column
NEXT f_x

*-- start at row,column number 1
f_col = 1
f_row = p_top

RETURN






*****************************************************************************
* Function: F_ELEMENT
* Syntax..: F_ELEMENT( f_row, f_col )
* Notes...: Function to return the array element number corresponding
* to the row,col coordinates specified.
* Assumes.: The memvar = the element number of the first
* option displayed in the window. This is used as the offset
* to determine the element number based on the current Page.
*
* Parms...: row_num = The actual row number
* col_num = The column array element number
*
* the array element number will be calculated from the formula:
* element = ( (relative_row_number - 1) * number_of_columns) +;
* column_num + ( f_firstopt - 1 )
* where: relative_row_number = real_row_number - top_of_window + 1
*****************************************************************************
FUNCTION f_element
PARAMETERS p_rownum, p_colnum

*-- test if an invalid row,col position given
IF p_rownum < p_top .OR. p_rownum > f_lastrow .OR. p_colnum < 1 .OR. p_colnum > f_lastcol
*-- return invalid element number to cause test to fail
RETURN f_lastopt + 1
ENDIF

RETURN INT(((p_rownum - p_top) * f_lastcol) + p_colnum + f_firstopt - 1)
