* ADDM: This program adds records to the database chosen in UPDATEDM.
select secondary
use
select primary
copy structure extended to structx
select secondary
use structx
do while t
   set colon off
   erase
   * line counter
   store 1 to accum
   select primary
   append blank
   @ accum,1 say 'Record #' + str(#,5)
   select seco
   go top
   * display field names
   do while .not. eof
      store accum + 1 to accum
      @ accum, 1 say field:name + ': '
      skip
   enddo .not. eof
   go top
   store 1 to accum
   * loop until all fields have been entered
   do while .not. eof
      store field:name to field
      store accum + 1 to accum
      do case
         * get character field
         case field:type = 'C'
            store t to true2
            do while true2
               store spaces to charin
               @ accum,len(field:name) + 3 get charin ;
                   picture 'XXXXXXXXXXXXXXXXXXXX'
               read
               if len(trim(charin)) <= field:len
                  store f to true2
                  select primary
                  replace &field with trim(charin)
                  select secondary
               endif <= field:len
            enddo true2
         * get logical field
         case field:type = 'L'
            store t to INLOG
            @ accum,len(field:name) + 3 get INLOG picture 'A'
            read
            select primary
            replace &field with INLOG
            select secondary
         * get numerical field
         otherwise
            store t to true2
            do while true2
               store 0 to numin
               @ accum,len(field:name) + 3 get numin picture '99999.99'
               read
               if (NUMIN < 100000)
                  select primary
                  replace &field with numin
                  select secondary
                  store f to true2
               endif
            enddo true2
      endcase
      skip
   enddo .not. eof
   set colon on
   store t to true2
   do while true2
      erase
      @ 5,30 say 'ADD MENU'
      @ 8,25 say '1) Add another record'
      @ 10,25 say '2) Return to UPDATE MENU'
      @ 12,25 say '3) Return to MASTER MENU'
      @ 15,18 say 'Please choose one ' get menu:num2 picture '9'
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
















say '2) Return to UPDATE MENU'
      @ 12,25 say '3) Return to MASTER MENU'
      @ 15,18 say 'Please choose one ' get menu:num2 picture '9'
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
