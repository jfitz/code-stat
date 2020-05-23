000010 @OPTIONS MAIN
000020*----------------------------------------------------------------------
000030* Continuation lines can be used for coding over several lines.
000040* 
000050* The method for specifying a continued line depends on the Source 
000055* Reference Format specified.  This example assumes that VAR (Variable) 
000060* format has been specified.  
000070*----------------------------------------------------------------------
000080 IDENTIFICATION   DIVISION.
000090 PROGRAM-ID       SAMPLE.
000100 DATA             DIVISION.
000110 WORKING-STORAGE  SECTION.
000120*----------------------------------------------------------------------
000130* For continuation within the definitions, the line to be continued  
000140* (line 170) is coded without the closing quote mark. 
000150* The continued line (line 180) is continued from the position of the 
000155* leading quote mark, and specified with a "-"in the indicator area.
000160*----------------------------------------------------------------------
000170  01 COL-LINE     PIC X(60) VALUE "----+----1----+----2
000180-                                 "----+----3----+----4
000190-                                 "----+----5----+----6".
000200*----------------------------------------------------------------------
000210  01 STR-DATA     PIC X(60).
000220 PROCEDURE        DIVISION.
000230*----------------------------------------------------------------------
000240* Reserved words and user-defined words can also be continued.
000250*----------------------------------------------------------------------
000260     DISPLAY "Enter the character string 
000270-            " to be displayed with the column line. >>" WITH NO ADV
000275-            ANCING.
000280     ACCEPT STR-D
000290-                ATA FROM CONSOLE.
000300*----------------------------------------------------------------------
000310     DISPLAY " ".
000320     DISPLAY COL-LINE.
000330     DISPLAY STR-DATA.
000340 END PROGRAM SAMPLE.
