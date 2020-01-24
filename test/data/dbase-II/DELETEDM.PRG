* DELETEDM: This program deletes and recalls records from database chosen
* in UPDATEDM.
erase
select primary
count to reccount
do while t
   @ 5,30 say 'DELETE MENU'
   @ 8,25 say '1) Delete a record'
   @ 10,25 say '2) Recall a record'
   @ 12,25 say '3) Return to UPDATE MENU'
   @ 14,25 say '4) Return to MASTER MENU'
   @ 17,18 say 'Please choose one ' get menu:num2 picture '9'
   read
   do case
      case menu:num2 = 1
         store t to true2
         * input loop
         do while true2
            store 0 to recnum
            @ 21,28 say 'Delete record #' get recnum picture '99999'
            read
            if recnum > 0 .and. recnum <= reccount
               go recnum
               * display DELETE message
               set talk on
               delete
               set talk off
               store f to true2
            endif >0 .and. <= reccount
         enddo true2
      case menu:num2 = 2
         store t to true2
         do while true2
            store 0 to recnum
            @ 21,28 say 'Recall record #' get recnum picture '99999'
            read
            if recnum > 0 .and. recnum <= reccount
               go recnum
               * display RECALL message
               set talk on
               recall
               set talk off
               store f to true2
            endif > 0 .and. <= reccount
         enddo true2
      case menu:num2 = 3
         erase
         return
      case menu:num2 = 4
         store 0 to menu:num2
         return
   endcase
enddo t


























LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL0212      0.00T                                                                                                                                                                                                                                                                                                Hudsell, Hembert                                                                                                                                go recnum
               set talk  enddo true2
      case menu:num2 = 3
         erase
         return
      case menu:num2 = 4
         store 0 to menu:num2
         return
   endcase
enddo true


























LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL