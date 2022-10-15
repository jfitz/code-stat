* UPDATEDM: This program directs processing of updates. The user selects a
* database, which is loaded into the primary area. Then a second menu offers
* a number of update options.
do while t
   erase
   @ 2,30 say 'UPDATE MENU'
   @ 5,25 say '1) Update Employee Database'
   @ 7,25 say '2) Update Job Desc. Database'
   @ 9,25 say '3) Return to MASTER MENU'
   @ 11,18 say 'Please choose one ' get menu:num picture '9'
   read
   do case
      case menu:num = 3
         return
      * This block erases unchosen menu options from the display and loads
      * the appropriate database into the primary area.
      case menu:num = 1 .or. menu:num = 2
         @ 9,18 say spaces
         @ 11,18 say spaces
         select primary
         if menu:num = 1
            @ 5,25 say '* '
            @ 7,25 say spaces
            ?
            use employee
         else
            @ 5,25 say spaces
            @ 7,25 say '* '
            ?
            use jobdet
         endif 1
         store t to update:on
         * Update loop. Store f to update:on if another database is to be
         * loaded.(Returns to first menu.)
         do while update:on
            store 0 to menu:num2
            @ 10,25 say '1) View Database'
            @ 12,25 say '2) Add Records'
            @ 14,25 say '3) Change Records'
            @ 16,25 say '4) Delete/Recall Records'
            @ 18,25 say '5) Update Another Database'
            @ 20,25 say '6) Return to MASTER MENU'
            @ 23,18 say 'Please choose one ' get menu:num2 picture '9'
            read
            do case
               case menu:num2 = 1
                  do viewdm
               case menu:num2 = 2
                  do addm
               case menu:num2 = 3
                  do changedm
               case menu:num2 = 4
                  do deletedm
               case menu:num2 = 5
                  store f to update:on
               case menu:num2 = 6
                  return
            endcase
            * Menu:num2 = 0 when user chooses to return to MASTER MENU from
            * VIEWDM, ADDM, CHANGEDM, or DELETEDM.
            if menu:num2 = 0
               return
            endif 0
         enddo update:on
   endcase
enddo t

            @ 20,25 say '6) Return to MASTER MENU'
            @ 23,18 say 'Please choose one ' get menu:num2 picture '9'
            read
            if menu:num2 = 1
               do viewdm
            else
               if menu:num2 = 2
                