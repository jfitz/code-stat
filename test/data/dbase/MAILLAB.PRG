NOTE - THIS COMMAD FILE PRINTS MAILING LABELS ON THE PRINTER
*
REMARK WHEN LABELS ARE IN PLACE, HIT CARRIAGE RETURN
WAIT
*  - echo printed information to screen
SET PRINT ON
*  - position to top of MAIL.DBF
GO TOP
*  - print labels
DO WHILE .NOT.EOF
   *  - print name
   DISP OFF $(NAME,13,17)-(' '+$(NAME,1,12))
   *  - checking to see if office address
   IF OF#'  '
     *  - print office name   
     DISP OFF OF
     *  - print address
     DISP OFF ADDR
     *  - print city, state, zip
     DISP OFF $(CITY,1,20)-(', '+$(STATE,1,2))-(' '+$(ZIP,1,5))
   ELSE
     *  - print home address
     DISP OFF ADDR
     *  - print city, state, zip
     DISP OFF $(CITY,1,20)-(', '+$(STATE,1,2))-(' '+$(ZIP,1,5))
     ?
   ENDIF
   ?
  SKIP
ENDDO
*  - turn off echo of labels
SET PRINT OFF
REMARK ALL DONE
RETURN
