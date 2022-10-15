* REPORTDM: This program gives user 3 report options: REPT1DM, REPT2DM,
* and REPT3DM (q.v.). Control returns to DEMO only if menu option #4 is
* chosen. Reports may be displayed or printed.
store 0 to menu:num
do while t
   erase
   @ 5,30 say 'REPORT MENU'
   @ 8,10 say '1) Average salary for each job title'
   @ 10,10 say '2) Employees earning more than midpoint of salary range'
   @ 12,10 say '3) Salary increase (by Department)'
   @ 14,10 say '4) Return to MASTER MENU'
   @ 17,5 say 'Please choose one ' get menu:num picture '9'
   read
   if menu:num >0 .and. menu:num < 4
      store t to true2
      do while true2
         @ 20,15 say 'Do you want a printed copy of this report? (y,n)';
           get menu:let picture 'A'
         read
         if menu:let = 'y'
            set format to print
            set print on
            store f to true2
         else
            if menu:let = 'n'
               @ 22,20 say ;
                '(Press <CTRL> S to stop scrolling, any key to continue.)'
               set console off
               wait
               set console on
               store f to true2
            endif n
         endif y
      enddo true2
   endif 0,4
   do case
      case menu:num = 1
         do rept1dm
      case menu:num = 2
         do rept2dm
      case menu:num = 3
         do rept3dm
      case menu:num = 4
         return
   endcase
   if menu:num >= 1 .and. menu:num <= 3
      set print off
      set format to screen
      ?
      ? 'Press any key to continue'
      set console off
      wait
      set console on
   endif
enddo t


















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