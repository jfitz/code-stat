* DEMO: This is the system's master program. It calls command files UPDATEDM
* and REPORTDM, and it exits to either the dBASE prompt or CP/M (depending
* on user input).

* display colons at boundaries of 'get' fields
set colon on
set screen on
* do not display dBASE messages
set talk off
* program cannot be halted with <ESC>
set escape off
set bell off
* saves current memory variables
save to tempdm
set print off
set format to screen
* create and initialize memory variables
store '                               ' to spaces
store ' ' to menu:let
store 0 to menu:num
do while t
   erase
   @ 5,30 say 'MASTER MENU'
   @ 8,26 say '1) Update Database'
   @ 10,26 say '2) Report'
   @ 12,26 say '3) Return to DBASE prompt'
   @ 14,26 say '4) Return to Operating System level'
   @ 18,18 say 'Please choose one ' get menu:num  picture '9'
   read
   do case
      case menu:num = 1
         do updatedm
      case menu:num = 2
         do reportdm
      case menu:num = 3
         select primary
         use
         set talk on
         set bell on
         restore from tempdm
         return
      case menu:num = 4
         quit
   endcase
enddo t






















面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面面�