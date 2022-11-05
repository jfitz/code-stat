* MENU.PRG
DO prologue && standard prologue

SET EXCLUSIVE OFF

DO std_set && standard initialization from SETUP.DBF
DO netprint
mhead_2 = CENTER([Calendar],36)

DO backdrop
CO_CHG(c_menus)
DO topscreen
DO box WITH 7,18,6,40,[Main Menu],.F.
SAVE SCREEN TO menuscreen

DO WHIL .T.
scr_file = [F_]+ msta +[_1] && saves screens to mem files
SET MESSAGE TO 24
@ 24,0
@ 10,23 PROMPT [Events ] MESSAGE []
@ 11,23 PROMPT [Print Calendar ] MESSAGE []
@ 12,23 PROMPT [Setup ] MESSAGE [System setup screen]
MENU TO xchoice

BEGIN SEQUENCE
DO CASE
CASE xchoice = 0
DO leave

CASE xchoice = 1
DO ev_MAINT

CASE xchoice = 2
DO cal_rep
CASE xchoice = 3
DO std_define
ENDCASE
END SEQUENCE

CLOSE DATA
RESTORE SCREEN FROM menuscreen
ENDDO
** EOF ***


