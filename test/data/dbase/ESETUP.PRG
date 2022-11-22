* 03/03/83  ESETUP.CMD
*   Copyright (c) 1983      ASHTON-TATE
*   Programmed by joe stegman  for dBASE II(tm) release 2.4
*
*  employee sample programs database setup program
*   calls  EMAIN.cmd
set talk off
? '  Initializing the employee database and memory files.'
?
? 'THIS WILL DESTROY ALL EMPLOYEE RECORDS!!!'
ACCEPT 'IS THIS WHAT YOU WANT TO DO (Y/N) ?' TO ANS
IF !(ans) ='Y'
  use emp
  delete all
  pack
  index on !(trim(first)) +' '+ !(trim(last)) to e'byname
  index on emp:nmbr to e'bynmbr
  store 0 to Memp:nmbr
  save to employee
  rele all
  ?
  ?
  ? 'Loading employee main menu program.'
  do emain
endif
return
�����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������