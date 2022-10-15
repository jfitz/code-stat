NOTE - THIS COMMAND FILE WRITES THE FORM LETTER
*
*  - position pointer at top of MAIL.DBF
GO TOP
*  - sets up to store everything that appears on the screen into
*  - a file called PRINT.TXT which the user can later list using
*  - his work processor.
SET ALTERNATE TO PRINT
SET ALTERNATE ON
* PUT PRINT FORMATTER SETUP DIRECTIVES HERE, SUCH AS PAGE NUMBERING,
* PRINTER PITCH, PAGE OFFSET, LINE HEIGHT, AND FOOTNOTES 
* EXAMPLE:
* ? '.OFFSET = 8'
* ? '.CHARACTER WIDTH = 12/INCH'
* etc.
*  - print letter
DO WHILE .NOT.EOF
? '                                                      Your Company'
? '                                                      8080 Micro St.'
? '                                                      Silicon, CA 93002'
? '                                                      (213) 555-1234'
? '                                                      '+DATE
*  - print name and address of client
? $(NAME,13,18)-(' '+$(NAME,1,12))
IF OF#'  '
 ? OF
ENDIF
? ADDR
IF VAL(ZIP)=0
  ? CITY-(', '+STATE)
ELSE
  ? CITY-(', '+STATE)-(' '+ZIP)
ENDIF
?
?
*  - print salutation
IF TITLE=1
  ? 'Dear Mr. '+$(NAME,1,12)
ENDIF
IF TITLE=2
  ? 'Dear Mrs. '+$(NAME,1,12)
ENDIF
IF TITLE=3
  ? 'Dear Ms. '+$(NAME,1,12)
ENDIF
IF TITLE=4
  ? 'Dear Dr. '+$(NAME,1,12)
ENDIF
?
*  - print text of letter
DO MAILINFO
*
* PUT A WORD PROCESSOR COMMAND TO SKIP TO THE NEXT PAGE HERE
* EXAMPLE
* ? '.SKIP'
*  - this is a wordstar page skip
? '.pa'
SKIP 
ENDDO
SET ALTERNATE OFF
RETURN
