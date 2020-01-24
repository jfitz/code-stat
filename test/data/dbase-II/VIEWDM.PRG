* VIEWDM: This program allows users to view part or all of the database
* chosen in UPDATEDM.
select secondary
use
select primary
count to reccount
copy structure extended to structx
select secondary
use structx
do while t
   erase
   store t to true2
   * input record #'s
   do while true2
      store 0 to startrec
      @ 15,5 say 'List from record #' get startrec picture '99999'
      read
      if startrec > 0 .and. startrec <= reccount
         select primary
         go startrec
         * main loop
         do while .not. eof
            erase
            store 1 to accum
            @ accum, 1 say 'Record #' + str(#,5)
            if *
               @ 1,30 say 'DELETED'
            endif *
            select secondary
            go top
            * print field names and contents
            do while .not. eof
               store accum + 1 to accum
               store field:name to field
               @ accum, 1 say field:name + ': '
               @ accum,len(field:name) + 3 say &field
               skip
            enddo .not. eof
            select primary
            @ 23,10 say "Press any key to continue ('q' to quit)"
            set console off
            wait to waitvar
            set console on
            if !(waitvar) = 'Q'
               go bottom
            endif !
            skip
         enddo .not. eof
         store f to true2
      endif > 0 .and. <= reccount
   enddo true2
   erase
   store t to true2
   do while true2
      @ 5,30 say 'VIEW MENU'
      @ 8,25 say '1) List records'
      @ 10,25 say '2) Return to UPDATE MENU'
      @ 12,25 say '3) Return to MASTER MENU'
      @ 15,25 say 'Please choose one ' get menu:num2 picture '9'
      read
      do case
         case menu:num2 = 1
            store f to true2
         case menu:num2 = 2
            erase
            return
         case menu:num2 = 3
            store 0 to menu:num2
            return
      endcase
   enddo true2
enddo t
