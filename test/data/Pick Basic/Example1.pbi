001 * EX.001
002 * BASIC TERMINAL I/O AND CONDITIONAL EXPRESSIONS
003 * mm/dd/yy: date last modified
004 * JES: author's initials
005 *
006     PROMPT ":"
007  PRINT
008 *
009 * GET NAME
010 *
011  PRINT "PLEASE ENTER YOUR NAME " :
012  INPUT NAME
013  PRINT
014 *
015 * CHECK TO SEE IF THEY WANT TO STOP
016 *
017  IF NAME = "" OR NAME = "QUIT" THEN STOP
018  PRINT "HELLO THERE " : NAME
019  PRINT
020 *
021 * NOW GET STRING OR NUMBER
022 *
023  PRINT "PLEASE ENTER A NUMBER OR A STRING OF LETTERS " :
024  INPUT RESPONSE
025  IF RESPONSE = "" OR RESPONSE = "QUIT" THEN STOP
026  PRINT
027 *
028 * CHECK TO SEE IF STRING IS NUMERIC
029 *
030  IF NUM(RESPONSE) THEN
031     PRINT "ABS VALUE OF " : RESPONSE : " IS " : ABS(RESPONSE)
032     PRINT "THE SQUARE IS " : RESPONSE * RESPONSE
033     PRINT "THE SQUARE ROOT IS " : SQRT(RESPONSE)
034     STOP
035  END
036 *
037 * CHECK TO SEE IF STRING IS LETTERS ONLY
038 *
039  IF ALPHA(RESPONSE) THEN
040     PRINT "THE LENGTH OF " : RESPONSE : " IS " : LEN(RESPONSE)
041     STOP
042  END
043 *
044 * STRING MUST CONTAIN NON-ALPHA AND NON-NUMERIC CHARACTERS
045 *
046  PRINT "YOUR RESPONSE WAS NOT CONSIDERED NUMERIC OR ALPHABETIC"
047  END
048