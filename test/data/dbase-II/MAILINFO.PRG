NOTE - THIS IS THE TEXT FOR THE USER FORM LETTER
*
*  - the text presented here describes how to generate the 
*  - letter.  The user will put in own text.
? 'This  is  the body of a form letter.  It was first written with  a  word'
? 'processor and then modified to become a command file.  The modifications'
? 'are:  placement  of  question  marks at the beginning of each  line  and' 
? 'enclosing  the text in single quotes or square brackets.  In  this  way,' 
? 'each  line of text is written to the ALTERNATE file by means of the  SET' 
? 'ALTERNATE commands in MAILLTTR.CMD (which calls this command file).'
?
? 'The text of the letter can be altered based on the database data fields.'
? 'For instance,  if the content of the letter should change for  different' 
? 'states,  then  this condition can be checked and the appropriate  action' 
? 'can be taken.'
?
?
? '                                              Yours truly'
?
?
?
?
? '                                              Mike R. O. Chip'
?
? 'Example:'
?
IF STATE='CA'
  ? 'California residents add 6% sales tax'
ENDIF
RETURN
