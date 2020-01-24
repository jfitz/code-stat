* CHANGEDM: This program edits records, one field at a time.
select secondary
use
select primary
* count # records (for input checking)
count to reccount
copy structure extended to structx
select secondary
use structx
do while true
   erase
   store t to true2
   * input record #'s
   do while true2
      store 0 to lorec, hirec
      @ 10,0 say 'Edit record #' get lorec picture '99999'
      @ 10,24 say 'thru record #' get hirec picture '99999'
      @ 11,37 say '<RETURN> if only one'
      @ 12,35 say 'record is to be changed'
      read
      if hirec = 0
         store lorec to hirec
      endif
      if hirec - lorec >= 0 .and. .not.(hirec > reccount) .and. lorec > 0
         store f to true2
      endif
   enddo true2
   set colon off
   * execute once/record
   do while lorec <= hirec
      erase
      select primary
      go lorec
      @ 1,2 say 'Record #' + str(#,5)
      if *
         @ 1,30 say 'DELETED'
      endif
      select secondary
      go top
      store 0 to fieldno
      * display field names and contents
      do while .not. eof
         store field:name to fieldname
         store fieldno + 1 to fieldno
         @ fieldno + 1,1 say str(fieldno,2) + ') ' + field:name + ': '
         @ fieldno + 1,len(field:name) + 7 say &fieldname
         skip
      enddo .not. eof
      @ fieldno + 2,1 say str(fieldno + 1,2) + ') ';
          + '**Proceed to next record'
      store t to true3
      * execute until changes to current record are complete
      do while true3
         store t to true2
         * input field number
         do while true2
            store 0 to menu:num2
            @ fieldno + 4,2 say 'Please choose one (<RETURN> to quit) ' ;
                get menu:num2 picture '99'
            read
            * if <RETURN>, quit
            if menu:num2 = 0
               store hirec + 1 to lorec
               store f to true2, true3
            else
               * skip to next record
               if menu:num2 = fieldno + 1
                  store lorec + 1 to lorec
                  store f to true2, true3
               else
                  if menu:num2 > 0 .and. menu:num2 <= fieldno
                     store f to true2
                  endif > 0
               endif = fieldno + 1
            endif = 0
         enddo true2
         * change block
         if menu:num2 > 0 .and. menu:num2 <= fieldno
            go menu:num2
            store field:name to fieldname
            do case
               * change character field
               case field:type = 'C'
                  store t to true2
                  do while true2
                     store spaces to charin
                     @ menu:num2 + 1,len(field:name) + field:len + 10  ;
                        say ':: ' get charin picture 'XXXXXXXXXXXXXXXXXXXX'
                     read
                     if len(trim(charin)) <= field:len
                        store f to true2
                        select primary
                        replace &fieldname with trim(charin)
                        select secondary
                        @ menu:num2 + 1,len(field:name) + 7 say charin
                     endif <= field:len
                  enddo true2
               * change logical field
               case field:type = 'L'
                  store t to login
                  @ menu:num2 + 1,len(field:name) + field:len + 10 ;
                      say ':: ' get login picture 'A'
                  read
                  select primary
                  replace &fieldname with login
                  select secondary
                  @ menu:num2 + 1,len(field:name) + 7 say login
               * change numerical field
               otherwise
                  store t to true2
                  do while true2
                     store 0 to numin
                     @ menu:num2 + 1,len(field:name) + field:len + 10 ;
                         say ':: ' get numin picture '99999.99'
                     read
                     if (NUMIN < 100000)
                        select primary
                        replace &fieldname with numin
                        select secondary
                        @ menu:num2 + 1,len(field:name) + 7 say &fieldname
                        store f to true2
                     endif
                  enddo true2
            endcase
            * erase display beyond new field (in case of input beyond
            * allowable length)
            @ menu:num2 + 1,len(field:name) + field:len + 10 say spaces
         endif = fieldno
      enddo true3
   enddo lorec = hirec
   store t to true2
   do while true2
      set colon on
      erase
      @ 5,30 say 'CHANGE MENU'
      @ 8,25 say '1) Edit records'
      @ 10,25 say '2) Return to UPDATE MENU'
      @ 12,25 say '3) Return to MASTER MENU'
      @ 15,18 say 'Please choose one ' get menu:num2 picture '9'
      read
      erase
      do case
         case menu:num2 = 1
            store f to true2
         case menu:num2 = 2
            return
         case menu:num2 = 3
            store 0 to menu:num2
            return
      endcase
   enddo true2
enddo t



















o,2) + ') ' + field:name + ': '
         @ fieldno + 1,len(field:name) + 7 say &fieldname
         skip
      enddo .not. eof
      @ fieldno + 2,1 say str(fieldno + 1,2) + ') ';
          + '**Proceed to next record'
      store t to true3
      * execute until changes to current record are complet