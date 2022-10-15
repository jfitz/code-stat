* REPT3DM: This program prints or displays a salary increase report.
* Amount of increase is supplied by user at runtime.
erase
set format to screen
set print off
store 0 to percent
@ 14,3 say 'Percentage Increase? (5, 12, etc.)' get percent picture '999'
read
@ 16,3
* get title
accept 'Report Title? ' to headng
erase
if menu:let = 'y'
   set format to print
   set print on
   endif
if len(trim(headng)) < 80
   * center heading if < 80 characters
   @ 4,(80- len(trim(headng)))/2 say headng
else
   @ 4,0 say headng
endif
use employee
index on dept:num to dptndxdm
report form salincr plain
return














* chosen. Reports may be displayed or printed.
store 0 to menu:num
do while t
   erase
   @ 5,30 say 'REPORT MENU'
   @ 8,10 say '1) Average salary for each job title'
   @ 10,10 say '2) Employees earning more than midpoint of salary range'
   @ 12,10 say '3) Salary increase (by Department)'
   @ 14,10 say '4) Return to MASTER MENU'
   @ 17,5 say 'Please 