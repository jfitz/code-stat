*****************************************************************************
* Copyright 1989, Financial Dynamics, Inc. *
* *
* (703) 671 - 3003 *
*****************************************************************************

* BROWSE.PRG

FUNC browse
PARA top_row,left_col,bot_row,right_col,new_screen
* vars defined in calling prg:
* module
* mtitle
* wtitle
* wfields

PRIV mscreen, ; && screen to save
firstrow, ; && starting row
mquit , ; && Logical Quit/ not Quit
keypress,; && ASCII of key pressed
mloop ,; && Logical Loop/ not Loop
mchoice ,; && Character menu choice ... Browse mode
fchoice ,; && Character menu choice ... Full screen mode
lastrow ,; && last row displayed in browse
cur_row ,; && current row of cursor
fieldname,; && Used to hold field names in initializing memvars
browse,; && Used to know which mode we're in
last_col,; && last column in wfields expansion - used by changed_key()
white,; && status of prompt
mret,; && screen below browse window
meminit,; && variable to be macroed that will contain name of init memvars routine
mreplace,; && variable to be macroed that will contain name of replace fields routine
full_screen,; && variable that tells whether there is a full screen mode to this maint
i,; && counter
fromread && tells if just came from read

* Define and open conditional statements would go here

* IF TYPE(M->module+[_def()])=[UI]
* PRIV brow_menu,scr_menu,module,wtitle,wfields,mtitle,;
* full_screen,mreplace,meminit,proc_keys,key_field,;
* group,group_key,mscreen,xchoice,scrmenu,browmenu,;
* allow_browse
* DO &module._def
* ENDIF
* IF TYPE(M->module+[_open()])=[UI]
* DO &module._open
* ENDIF

browse = .T.
mloop = .F.
fchoice = []
mprompt = IF(TYPE([M->mprompt]) =[U], '[F1] for Help, / for Menu', M->mprompt)
scr_prompt = IF(TYPE([M->scr_prompt]) =[U], 'Fill in fields, [PgDn] when finished', M->scr_prompt)
allow_browse = IF(TYPE([M->allow_browse]) =[U], .T., M->allow_browse)
scrmenu = IF(TYPE([M->scrmenu]) =[U], [], M->scrmenu)
browmenu = IF(TYPE([M->browmenu]) =[U], [], M->browmenu)
default_color = IF(TYPE([M->default_color]) =[U],.T., M->default_color)
add_keys = IF(TYPE([M->add_keys]) =[U], [AQ], M->add_keys)
init_chr = IF(TYPE([M->init_chr]) =[U], [E], M->init_chr)
key_field = IF(TYPE([M->key_field]) =[U], INDEXKEY(0), M->key_field)
new_screen = IF(TYPE([M->new_screen]) =[U], .T., M->new_screen) && do backdrop first?
draw_box = IF(TYPE([M->draw_box] ) =[U], .T., M->draw_box)
mreplace = IF(TYPE(M->module+[_repl()])=[UI],M->module,[std]) + [_repl]
meminit = IF(TYPE(M->module+[_init()])=[UI],M->module,[std]) + [_init]
proc_keys = IF(TYPE([M->proc_keys]) =[U], [], M->proc_keys)
stayinfull = IF(TYPE([M->stayinfull]) =[U], .F.,M->stayinfull)


* see if any GROUP is specified:
IF TYPE([M->group_key]) = [U]
group_key = []
ENDIF
IF TYPE([M->group]) = [U]
group = IF(EMPTY(M->Group_key), [], &group_key)
ENDIF


*-------------------------- chg_colors is not used yet---------------------
chg_colors = IF(TYPE([M->chg_colors])=[U], .T., M->chg_colors)

mchoice = []

IF TYPE([M->full_screen])=[U] && not specified
full_screen = TYPE(M->module+[_frame()])=[UI]
ENDIF

* Declare memory variables here, so INIT and REPLACE can access them:
FOR i = 1 TO FCOUNT()
fieldname = FIELD(i)
PRIV &fieldname
&fieldname = []
NEXT

CURS_OFF()
SET MESSAGE TO
mquit = .F.

CO_PUSH() && Saves color before BROWSE

*----------------------------------------------------------------------
IF ! allow_browse
DO std_V
mret = []
ELSE
DO set_browse && Below in BROWSE.PRG
mret = SAVESCR(top_row,MAX(left_col-1,0),MIN(bot_row+1,24),right_col)

* ÄÄÄÄÄÄÄ Screen ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
IF new_screen && draw a new backdrop for this screen
DO backdrop
IF top_row > 4 && put up the title and date if we have enough room
DO topscreen
ENDIF
ENDIF

IF TYPE([M->brow_menu]) = [U] && use default menu
PRIV brow_menu[14]
AFILL(brow_menu,[])
brow_menu[1] = [Edit]
brow_menu[2] = [Add]
brow_menu[3] = [Delete]
brow_menu[4] = [Find]
brow_menu[5] = [Print]
IF full_screen
brow_menu[6] = [View]
brow_menu[7] = [Quit]
ELSE
brow_menu[6] = [Quit]
ENDIF
ENDIF
DO menufix WITH brow_menu,M->browmenu

IF M->draw_box && draw box for browsing
CO_CHG(0,c_frame)
DRAWSHADOW(top_row,left_col,bot_row,right_col)

IF M->curr_grp > 2 .AND. curr_grp # c_fullscr
* they've already changed colors, so use current group:
CO_CHG(M->curr_grp, c_frame)
ELSE
CO_CHG(c_browse, c_frame) && Change to browse color group
ENDIF

@ top_row,left_col,bot_row,right_col BOX stdbox
CO_CHG(curr_grp,c_sayget)
SCROLL(top_row+1,left_col+1,bot_row-1,right_col-1,0) && Clears box

IF ! EMPTY(mtitle)
* ÄÄÄÄtitle on top line:ÄÄÄÄÄÄÄÄÄÄ
mtitle = [ ]+mtitle+[ ]
xcol = left_col + (((right_col-left_col) - LEN(mtitle)) / 2)
CO_CHG(curr_grp,c_title)
@ top_row,xcol+1 SAY mtitle
ENDIF
ENDIF
mscreen = SAVESCR(0,0,24,79) && this is the browse screen

* ÄÄÄÄÄÄ Ready to go ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
firstrow = top_row+3
white = .F.

DO WHIL ! mquit
CO_CHG(curr_grp,c_text)
IF M->draw_box && SCREENGRAB(top_row+2,left_col+3,top_row+2,right_col-2) # wtitle
@ top_row+2,left_col+3 SAY wtitle && to handle index switching.
ENDIF
@ 24,0
@ 24,0 SAY mprompt
CO_CHG(curr_grp,c_sayget)

white = .F.
DO list_em

mloop = .T.
DO WHIL mloop
fromread = .F.
IF ! [Q] $ fchoice+mchoice
keypress = GET_KEY()
ELSE
mquit = .T.
EXIT
ENDIF
IF OFF() .AND. ! mchoice $ M->add_keys .AND. ! ESC()
DO kbhit WITH [No data in file. Any key continues...]
CURS_OFF()
LOOP
ENDIF
IF mchoice $ proc_keys && Asking for a procedure
CURS_ON()
IF mchoice $ [AE] .AND. M->white
DO rev_line
ENDIF

IF TYPE([&module._&mchoice()]) = [UI] && it HAS BEEN Linked.
DO &module._&mchoice
ENDIF
ELSE
DO std_keys
ENDIF
CURS_OFF()
IF browse .AND. (! stayinfull .OR. ! fromread)
CO_CHG(curr_grp,c_text)
@ 24,0
@ 24,0 SAY mprompt
ELSEIF browse .AND. (stayinfull .AND. fromread) .AND. full_screen
CO_POP()
DO std_v
ENDIF
ENDDO
ENDDO
ENDIF
CO_POP()
CURS_ON()
RETU mret

PROC list_em
PRIV mrec,list_one
SCROLL(top_row+3,left_col+1,bot_row-1,right_col-1,0) && clears box

list_one = .F.
LOCA FOR ! DELE() WHIL ! EOF() && get to next non-deleted record
IF OFF()
TOP()
ENDIF
mrec = RECNO()
@ firstrow-1,left_col+4 SAY []
DO WHIL ! OFF() .AND. ROW() < bot_row-1
@ ROW()+1,left_col+4 SAY &wfields
list_one = .T.
SKIP
ENDDO
cur_row = firstrow
lastrow = ROW() && bottom row used
GOTO mrec

* just in case wfields does something special with the current record:
IF list_one
@ cur_row,left_col+4 SAY &wfields
ENDIF
RETURN


PROC full_scr && Full Screen Standard Maintenance

PRIVATE full_frame,; && frame screen
i,; && loop counter
wchoice && menu to variable

* mchoice && Character choice May carry V from previous


* ÄÄÄÄÄÄÄ Menu ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
IF TYPE([M->scr_menu]) = [U] && Use the default Full Screen Menu.
PRIV scr_menu[14]
AFILL(scr_menu,[])
scr_menu[1] = [Edit]
scr_menu[2] = [Add]
scr_menu[3] = [Delete]
scr_menu[4] = [Find]
scr_menu[5] = [Print]
scr_menu[6] = [Next]
scr_menu[7] = [Back]
IF allow_browse
scr_menu[8] = [View]
scr_menu[9] = [Quit]
ELSE
scr_menu[8] = [Quit]
ENDIF
ENDIF
DO menufix WITH scr_menu,M->scrmenu

* ÄÄÄÄÄÄÄ Screen ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
CO_PUSH()
CO_CHG(IF(default_color,c_fullscr,curr_grp),c_frame)
IF new_screen && draw a new backdrop for this screen
DO backdrop
ENDIF
DO &module._frame
full_frame = SAVESCR(0,0,24,79)

* ÄÄÄÄÄÄ Ready to go ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
CURS_ON()
*fchoice = []
mquit = .F.
DO WHIL .T.
IF OFF()
KEYBOARD [V]
ENDIF
fchoice = []
CO_CHG(curr_grp,c_sayget)
DO &module._gets WITH ALIAS() && display current record
CLEAR GETS

CO_CHG(curr_grp,c_text)
@ 24,0
CO_PUSH()
CO_CHG(c_menus,c_sayget)
PRIV i,from_ud
from_ud = .F.
SET KEY 5 TO seedback && Up arrow
SET KEY 24 TO seednext && Down arrow
@24,0
FOR i = 1 TO LEN(scr_menu)
IF ! EMPTY(scr_menu[i])
@ 24,COL()+2 PROMPT scr_menu[i]
ENDIF
NEXT i
MENU TO wchoice
SET KEY 5 TO
SET KEY 24 TO
CO_POP()
IF from_ud
fchoice = CHR(INKEY())
from_ud = .F.
ELSE
IF wchoice = 0 && Escape ...
fchoice = [Q]
ELSE
fchoice = SUBS(scr_menu[wchoice],1,1)
ENDIF
ENDIF
IF fchoice $ proc_keys
IF TYPE([&module._&fchoice()]) = [UI] && it HAS BEEN Linked.
DO &module._&fchoice
ENDIF
RESTSCR(full_frame)
LOOP
ENDIF

IF fchoice $ [AEDPFNB]
DO std_&fchoice && executes the standard procedure
LOOP
ENDIF
fromread = .F.

DO CASE && standard cases that do not call procedures
CASE fchoice = [Q]
mquit = .T.
CO_POP()
RETURN

CASE fchoice = [V] .AND. allow_browse && Change View!
CO_POP()
RETURN

ENDCASE
ENDDO
RETURN
*** Eof: Full_scr.prg


PROC set_browse && called by WINDOW, BROWSE, PICKS
* ÄÄÄÄÄ Default corners ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
last_col = LEN(&wfields)
top_row = IF(TYPE([M->top_row])=[U], 6, M->top_row)
bot_row = IF(TYPE([M->bot_row])=[U], MIN(M->top_row+16,23), M->bot_row)
mwide = MIN(MAX(LEN(wtitle),last_col)+6,79)
left_col = IF(TYPE([M->left_col])=[U], INT((80 - M->mwide)/2), M->left_col ) && left col
last_col = M->left_col + 3 + M->last_col
right_col = IF(TYPE([M->right_col])=[U], M->left_col+M->mwide, M->right_col ) && right col
right_col = MAX(M->right_col,M->left_col + M->mwide) && right col
RETURN

PROC menufix
PARA menuarray,mstr
PRIV option,i,adding
IF ! EMPTY(M->mstr)
DO WHILE [+] $ M->mstr .OR. [-] $ M->mstr
mstr = SUBS(mstr,LAZ(AT([+],mstr),AT([-],mstr)))
adding = IF(SUBS(mstr,1,1)=[+],.T.,.F.)
mstr = SUBS(mstr,2)
option = SUBS(mstr,1,IF(MAX(AT([+],mstr),AT([-],mstr))>0,LAZ(AT([+],mstr),AT([-],mstr))-1,LEN(M->mstr)))
IF adding
FOR i = 1 TO LEN(menuarray)
IF EMPTY(menuarray[i])
EXIT
ENDIF
NEXT
AINS(menuarray,i-1)
menuarray[i-1] = M->option
ELSE
ADEL(menuarray,ASCAN(menuarray,M->option))
menuarray[14] = []
ENDIF
ENDDO
ENDIF
RETU

FUNC laz && lowest above zero of numeric. Used in menufix.
PARA val1,val2
PRIV mret
mret = MIN(val1,val2)
mret = IF(mret>0,mret,0)
mret = IF(mret>0,mret,val1)
mret = IF(mret>0,mret,val2)
RETU mret


FUNC get_key && Get next key pressed
PRIVATE keypress
CO_CHG(curr_grp,c_sayget)
IF ! M->white
DO rev_line
ENDIF
SET CONS OFF
WAIT []
SET CONS ON
keypress = LASTKEY()
mchoice = UPPER(CHR(keypress)) && Character

DO CASE
CASE keypress = 13 && Edit
mchoice = init_chr

CASE keypress = 7 && Delete
mchoice = [D]

CASE keypress = 47 && Slash key for menu.
CO_PUSH()
CO_CHG(c_menus,c_sayget)
@ 24,0
FOR i = 1 TO LEN(brow_menu)
IF ! EMPTY(brow_menu[i])
@ 24,COL()+2 PROMPT brow_menu[i]
ENDIF
NEXT i
wchoice = 1
MENU TO wchoice
CO_POP()
CO_PUSH()
CO_CHG(curr_grp,c_text)
@ 24,0
IF wchoice = 0 && Escape ...
mchoice = [ ] && just erases the menu bar.
@ 24,0 SAY mprompt
ELSE
mchoice = SUBS(brow_menu[wchoice],1,1)
ENDIF
CO_POP()
CLEAR TYPEAHEAD
ENDCASE
RETURN (keypress)


PROC rev_line && Reverses color on the current line
PRIV cur_line
cur_line = SAVESCREEN(cur_row,left_col+2,cur_row,right_col-1)
REVERSE(cur_line)
RESTSCREEN(cur_row,left_col+2,cur_row,right_col-1,cur_line)
@ cur_row,left_col+1 SAY []
white = (! M->white)
RETU

PROC seednext && handles up arrow like Back
KEYBOARD CHR(27)+CHR(78)
from_ud = .T.
RETU

PROC seedback && handles down arrow like next
KEYBOARD CHR(27)+CHR(66)
from_ud = .T.
RETU

