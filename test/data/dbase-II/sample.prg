
* ************************************************************************
* * The following is a sample program which has been written in several  *
* * languages to provide an example in interactive programming for Magee *
* * students. This program has been written in: ICL PC dBASE II          *
* * Programmer: Ted Leath                                                *
* * Date: 22/11/85                                                       *
* ************************************************************************
*                              Main Program
SET TALK OFF
USE BOOK.DAT
STORE 0 TO choice
STORE " No. Author          Title                          Publisher" TO head
DO WHILE choice <> 6
ERASE
@ 3,1 SAY "========================================================="
@ 4,26 SAY "Sample Program Main Menu"
@ 5,1 SAY "========================================================="
@ 8,26 SAY "1) Display Book File"
@ 10,26 SAY "2) Display Record"
@ 12,26 SAY "3) Input Record"
@ 14,26 SAY "4) Delete Record"
@ 16,26 SAY "5) Print Book File"
@ 18,26 SAY "6) Exit"STORE 0 TO choice
DO WHILE ((choice < 1) .OR. (choice > 6))
   @ 20,0
   @ 20,14 SAY "Please select one of the above options ";
      GET choice PICTURE '9'
   READ
ENDDO
DO CASE
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*  Display_file
   CASE choice = 1
      ERASE
      GO TOP
      @ 1,1 SAY head
      STORE 4 TO lncount
* While the screen is not full and not EOF display records
      DO WHILE .NOT. EOF
         IF lncount >= 20
            DO hold_screen
            ERASE
            @ 1,1 SAY head
            STORE 4 TO lncount
         ENDIF
         @ lncount,2 SAY booknumber USING '999'
         @ lncount,6 SAY author USING 'XXXXXXXXXXXXXXX'
         @ lncount,22 SAY title USING 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
         @ lncount,53 SAY publisher USING 'XXXXXXXXXXXXXXX'
         STORE lncount + 1 TO lncount
         SKIP
      ENDDO
      DO hold_screen

* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*  Display_record
   CASE choice = 2
      DO validate_booknumber
      GO TOP
      ERASE
      DISPLAY head OFF
      STORE 0 TO found
* Check to see if a record exists with the chosen booknumber
      DO WHILE .NOT. EOF
         IF booknum = booknumber
            DISPLAY booknumber,author,title,publisher OFF
            STORE 1 TO found
         ENDIF
         SKIP
      ENDDO
      IF found = 0
         @ 21,0
         @ 21,1 SAY "No record with this key"
      ENDIF
      DO hold_screen
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*  Input_record
   CASE choice = 3
      DO validate_booknumber
      GO TOP
      STORE 0 TO found
* Check to see if record exists with booknumber to avoid duplication
      DO WHILE .NOT. EOF
         IF booknum = booknumber
            STORE 1 TO found
         ENDIF
         SKIP
      ENDDO
      IF found = 0
         APPEND BLANK
         ERASE
         @ 11,1 SAY "Please Enter Author (15 Characters Maximum) ";
            GET author
         @ 12,1 SAY "Please Enter Title (30 Characters Maximum) ";
            GET title
         @ 13,1 SAY "Please Enter Publisher (15 Characters maximum) ";
            GET publisher
         READ
         REPLACE booknumber WITH booknum
         REPLACE author WITH author
         REPLACE title WITH title
         REPLACE publisher WITH publisher
* BOOK.DAT only needs to be sorted when a new record is added
         SORT ON booknumber TO TEMP
         USE TEMP
         COPY TO BOOK.DAT
         USE BOOK.DAT
      ELSE
         @ 21,0
         @ 21,1 SAY "Record already exists"
      ENDIF
      DO hold_screen
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*  Delete_record
   CASE choice = 4
      DO validate_booknumber
      GO TOP
      STORE 0 TO found
* Check to see if record exists
      DO WHILE .NOT. EOF
         IF booknum = booknumber
            STORE 1 TO found
            DELETE
            PACK
         ENDIF
         SKIP
      ENDDO
      IF found = 1
         @ 21,0
         @ 21,1 SAY "Record deleted"
      ELSE
         @ 21,0
         @ 21,1 SAY "No record with this key"
      ENDIF
      DO hold_screen
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*  Print_file
   CASE choice = 5
      GO TOP
      ERASE
      @ 1,1 SAY "File now printing..."
      @ 2,1 SAY "Please make sure printer is on-line"
      SET FORMAT TO PRINT
      @ 1,1 SAY head
      STORE 4 TO lncount
      DO WHILE .NOT. EOF
         IF lncount >= 50
            EJECT
            @ 1,1 SAY head
            STORE 4 TO lncount
         ENDIF
         @ lncount,2 SAY booknumber USING '999'
         @ lncount,6 SAY author USING 'XXXXXXXXXXXXXXX'
         @ lncount,22 SAY title USING 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
         @ lncount,53 SAY publisher USING 'XXXXXXXXXXXXXXX'
         STORE lncount + 1 TO lncount
         SKIP
      ENDDO
   EJECT
   SET FORMAT TO SCREEN
   DO hold_screen
END
CASE
ENDDO
RETURN

* Here is a listing of the program validate_booknumber:

STORE 0 TO BOOKNUM
ERASE
DO WHILE ((BOOKNUM < 101) .OR. (BOOKNUM > 899))
   @ 12,0
   @ 12,1 SAY "Please Enter Booknumber (101 to 899) " GET BOOKNUM;
      PICTURE '999'
   READ
ENDDO
RETURN

* Here is a listing of the program hold_screen:

STORE " " TO holdchar
@ 22,0
@ 22,1 SAY "Press RETURN to continue..." 
GET holdchar PICTURE 'X'
READ
RETURN
