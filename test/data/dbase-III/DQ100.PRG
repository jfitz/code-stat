ha=''
clear
do &fl_na
@0,0 say 'Help File:'
@13,0 clear
text
MODIFY - This command allows you to alter the contents of files that meet
specific parameters. You will see a Blank Entry frame where you will
enter the Parameters for the record to be Modified. DQ then searches
for and displays the record that matches these parameters for Editing.

LIST - To display specific items meeting specific parameters. 
Allows you do define exactly which items will be displayed or 
printed when the list is executed. Parameters for the list are 
defined in the same manner as in the MODIFY command.
endtext
wait ' PRESS SPACE BAR TO RETURN' To ha
if ha=' '
@12,0 CLEAR
return
endif
@12,0 clear
return

