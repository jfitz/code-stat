NOTE - MAIL SYSTEM NAME AND ADDRESS ENTRY PROGRAM
*
*  - move pointer to bottom of MAIL.DBF
GO BOTTOM
*  - send output of @ commands to MAILFMT.FMT
SET FORMAT TO MAILFMT
*  - read name and address data 
DO WHILE T
  *  - initialize address variables to blank
  STORE ' ' TO MTTL
  STORE '  ' TO MSTATE
  STORE '      ' TO MZIP
  STORE '            ' TO MSNAM,MCITY
  STORE '                    ' TO MGNAME
  STORE '                              ' TO MOF,MADDR
  *  - read format file
  READ
  *  - check to see if ready to exit this command file
  IF MTTL=' '
    RETURN
  ENDIF
  *  - combine first and last name and store 
  STORE MSNAM+MGNAME TO MNAME
  *  - add blank card to MAIL.DBF
  APPEND BLANK
  *  - store name and address data to MAIL.DBF
  *  - substitute input variables for database file variables
  REPLACE TITLE WITH &MTTL,NAME WITH MNAME,OF WITH MOF,ADDR WITH MADDR
  REPLACE CITY WITH MCITY,STATE WITH !(MSTATE),ZIP WITH MZIP
ENDDO
RETURN
