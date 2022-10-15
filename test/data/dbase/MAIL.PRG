NOTE Example dBASE Command file program
*
*  - this is a sample form letter generator and mailing label 
*  - dBASE II system.  The system allows the user to add new 
*  - entries to a mailing list, print the mailing list on gummed
*  - labels or print a form letter addressed to all the people 
*  - in the mailing list.  No provision is made to modify the 
*  - mailing list.  The user can use the dBASE commands EDIT and/or
*  - DELETE to modify the database file MAIL.DBF to change the 
*  - mailing list.
*
*  - turn off display of commands to screen
SET TALK OFF
*  - assign database file to command stream.
USE MAIL
*  - enter current date to print on form letter
ACCEPT 'Enter date as mm/dd/yy' TO DATE
*  - select system option
DO WHILE T
   ?
   ?
   ?
   ? '          Form Letter Writing System'
   ?
   ?
   ? '          0 - EXIT'
   ? '          1 - Enter new entries'
   ? '          2 - Make Labels'
   ? '          3 - Write Letters'
   ?
   ? ' enter desired action'
   *  - wait for user response
   WAIT TO ACTION
   *  - test for exit 
   IF ACTION='0'
      SET TALK on
      CANCEL
   ENDIF
   *  - test for new entries
   IF ACTION='1' 
      *  - add new names to MAIL.DBF
      DO MAILENTR
   ENDIF
   *  - test to make labels
   IF ACTION='2'
      *  - print mail labels
      DO MAILLAB
   ENDIF
   *   - test to write form letters
   IF ACTION='3'
      *  - print form letter to names in MAIL.DBF
      DO MAILLTTR
   ENDIF
ENDDO
RETURN
     