       PROCEDURE DIVISION.
           MAIN-LINE-ROUTINE. 

       PERFORM 1A-HOUSEKEEPING.
           PERFORM 1B-PROCESS-INPUT
               UNTIL END-OF-FILE.
           PERFORM 1C-PRINT-TABLE.
           PERFORM 1D-WRAPUP.
           STOP RUN. 

      ********** FIRST-LEVEL PERFORMED ROUTINES 
       1A-HOUSEKEEPING.
           OPEN INPUT EMP-MASTER-CARD-IN
                OUTPUT PRINT-FILE.
           MOVE LOW-VALUES TO STAT-TABLE. 

       1B-PROCESS-INPUT.
           PERFORM 2B1-READ-INPUT.
           IF NOT END-OF-FILE
               PERFORM 2B2-SEARCH-AND-TALLY. 

       1C-PRINT-TABLE.
           PERFORM 2C2-PRINT-UPPER-HEADER.
           PERFORM 2C1-PRINT-TABLE-ROW VARYING ROW-INDEX FROM 1 BY 1
               UNTIL ROW-INDEX > 9.
           PERFORM 2CS-PRINT-LOWER-HEADER.
           PERFORM 2C1-PRINT-TABLE-ROW VARYING ROW-INDEX FROM 10 BY 1
               UNTIL ROW-INDEX > 18. 

       1D-WRAPUP.
           CLOSE EMP-MASTER-CARD-IN
                 PRINT-FILE.

      ******** SECOND-LEVEL PERFORMED ROUTINES 
       2B1-READ-INPUT.
           READ EMP-MASTER-CARD-IN
               AT END MOVE 'X' TO END-OF-FILE-FLAG. 

       2B2-SEARCH-AND-TALLY.
           SET AGE-INDEX ROW-INDEX TO 1.
           SEARCH AGE-TABLE VARYING ROW-INDEX
               WHEN AGE < AGE-TABLE (AGE-INDEX) NEXT SENTENCE.
           SET LOS-INDEX COL-INDEX TO 1.
           SEARCH LOS-TABLE VARYING COL-INDEX
               WHEN LOS < LOS-TABLE (LOS-INDEX) NEXT SENTENCE.
           IF FEMALE SET COL-INDEX UP BY 8.
           IF NON-MGT SET ROW-INDEX UP BY 9.
           ADD 1 TO STAT (ROW-INDEX, COL-INDEX). 

       2C1-PRINT-TABLE-ROW.
           PERFORM 3CIA-FILL-BUCKETS VARYING COL-INDEX FROM 1 BY 1
               UNTIL COL-INDEX > 16.
           SET ROW-IDENT-INDEX TO ROW-INDEX.
           MOVE ROW-IDENT (ROW-IDENT-INDEX) TO ROW-IDENT-OUT.
           WRITE PRINT-RECORD FROM TEMP-AREA AFTER ADVANCING 1 LINES. 

       2C2-PRINT-UPPER-HEADER.
           MOVE HEADER-1 TO PRINT-LINE.
           WRITE PRINT-RECORD AFTER ADVANCING NEW-PAGE.
           MOVE HEADER-2 TO PRINT-LINE.
           WRITE PRINT-RECORD AFTER ADVANCING 2 LINES.
           MOVE HEADER-3 TO PRINT-LINE.
           WRITE PRINT-RECORD AFTER ADVANCING 2 LINES.
           MOVE HEADER-4 TO PRINT-LINE.
           WRITE PRINT-RECORD AFTER ADVANCING 2 LINES.
           MOVE ' AGE' TO PRINT-LINE.
           WRITE PRINT-RECORD AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-RECORD AFTER ADVANCING 1 LINES. 

       2C3-PRINT-LOWER-HEADER.
           MOVE HEADER-5 TO PRINT-LINE.
           WRITE PRINT-RECORD AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-RECORD AFTER ADVANCING 1 LINES. 

      ********** THIRD LEVEL PERFORMED ROUTINE ********** 
       3C1A-FILL-BUCKETS.
           SET BUCKET-INDEX TO COL-INDEX.
           MOVE STAT (ROW-INDEX, COL-INDEX) TO BUCKET (BUCKET-INDEX). 
