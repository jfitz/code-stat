*---------------------------------------------------
*-- Program...: MAIN.PRG
*---------------------------------------------------
*-- save the environment and reset it
cTalk = set("TALK")
set talk off
cStat = set("STATUS")
set stat off
cBell = set("BELL")
set bell off
cDel  = set("DELETED")
set deleted on

*-- define the form:
fMain = new form()              && create a new form
fMain.Left        = 1           && left at 1
fMain.Top         = 0           && top at 0
fMain.Text        = "Ken's Address Book"
fMain.MenuFile    = "MAIN.MNU"  && menu we created
fMain.MDI         = .f.    && not "Multiple Document
                           && Interface" form
fMain.OnOpen      = {;fMain.WindowState = 2} 
                           && When we open, maximize
                           && the form! Note the use
                           && of a codeblock ...
fMain.ReadModal()          && open form and suspend
                           && this program until
                           && it's closed.

*-- when the form closes, we will continue here:
set talk &cTalk.
set stat &cStat.
set bell &cBell.
set deleted &cDel.

*---------------------------------------------------
*-- End of Program: MAIN.PRG
*---------------------------------------------------