        IDENTIFICATION DIVISION. 
        PROGRAM-ID. 
               EXAMPLE. 
        AUTHOR. 
               HASKINS. 
        INSTALLATION. 
               UF, INTRO TO COBOL. 
        DATE-WRITTEN. 
               JANUARY 1, 2000. 
        DATE-COMPILED. 
  
       *  EVERY program you write for this class must have information 
       *  for the CURRENT program here.  In industry, this area 
       *  is also used to document any changes made to the program. 
  
       *  Here is a sample: 
       * This program "dumps" (i.e., directly copies)  data from an 
       *  input file called  Transaction-File into an output file called 
       *  Output-File.   The program is part of the catalogue system 
       *  for Spee-Dee-Sports, a company that sells running supplies. 
  
        ENVIRONMENT DIVISION. 
  
        INPUT-OUTPUT SECTION. 
  
        FILE-CONTROL. 
               SELECT  TRANSACTION-FILE   ASSIGN TO   '/class/cgs3403/c3403bgq/extrans.data'. 
               SELECT OUTPUT-FILE                ASSIGN TO 
                                                           'example.data' 
                                                           ORGANIZATION IS LINE-SEQUENTIAL. 
  
        DATA DIVISION. 
        FILE SECTION. 
  
        FD  TRANSACTION-FILE 
              RECORD CONTAINS 60 CHARACTERS 
              DATA RECORD IS TRANSACTION-REC. 
  
  
        01  TRANSACTION-REC. 
             05 TR-ORDER-DATE. 
                    10 TR-OD-DD                      PIC XX. 
                    10  TR-OD-MM                    PIC XX. 
                    10  TR-OD-YY                     PIC X(4). 
             05  TR-CUST-NUMBER             PIC X(8). 
             05 TR-CAT-CODE. 
                    10  TR-CC-SUPPLIER         PIC XX. 
                    10  TR-CC-ITEM-NO          PIC  X(5). 
                    10  TR-CC-SIZE                   PIC X(5). 
                    10  TR-CC-COLOR             PIC X. 
             05  TR-UNIT-PRICE                   PIC S9(4)V99. 
             05  TR-QUANTITY                     PIC S9(5). 
             05  FILLER                                  PIC X(20). 
        FD  OUTPUT-FILE 
              RECORD CONTAINS 133 CHARACTERS 
              DATA RECORD IS OUTPUT-REC. 
  
        01  OUTPUT-REC                            PIC X(133). 
        WORKING-STORAGE SECTION. 
        01  END-OF-FILE                            PIC X        VALUE 'N'. 
        01  TRANSACTION-OUTPUT-FORMAT. 
             05  FILLER                                                 PIC X        VALUE SPACES. 
             05  TRANSACTION-REC-SLOT              PIC X(60). 
             05  FILLER                                                 PIC X(72)  VALUE SPACES. 
        PROCEDURE DIVISION. 
  
     *  In a structured program there will always be a Driving Paragraph; its 
     *  job is to trigger the execution of lower paragraphs.  In structured 
     *  programs, control is always held in the last analysis by this paragraph, 
     *  the point being that such programs work "top down". 
  
     *  In programs you do for class, comment EVERY paragraph of the Procedure 
     *  Division (in your own words, tell the reader what that paragraph does). 
     *  This is very important in a work setting because 80% of programming time 
     *  involves maintenance of existing programs so you're always having to get 
     *  familiar with programs you may not have seen before. 
  
      100-DRIVER. 
             PERFORM 200-SETUP. 
             PERFORM 300-MAINLOOP UNTIL END-OF-FILE = 'Y'. 
             PERFORM 400-WRAPUP. 
             STOP RUN. 
      *  Setup performs all preparatory operations necessary 
      *  before main processing.  Here, it opens both files and reads 
      *  the first input record. 
       200-SETUP. 
  
             OPEN INPUT       TRANSACTION-FILE 
                       OUTPUT    OUTPUT-FILE. 
  
             READ TRANSACTION-FILE 
                      AT END MOVE 'Y' TO END-OF-FILE 
             END-READ. 
      *  Mainloop processes the normal record.  Here 
      *  it writes a record from the input file and reads the next 
      *  and writes it, and so on until there are no records left. 
  
       300-MAINLOOP. 
             MOVE TRANSACTION-REC TO TOF-REC-SLOT. 
             MOVE TRANSACTION-OUTPUT-FORMAT TO OUTPUT-REC. 
             WRITE OUTPUT-REC. 
             READ TRANSACTION-FILE 
                        AT END MOVE 'Y' TO END-OF-FILE 
             END-READ. 
  
  
      *  Wrapup performs  left-over operations once Mainloop is finished. 
      *   Here it simply closes both files. 
  
        400-WRAPUP. 
  
               CLOSE TRANSACTION-FILE 
                            OUTPUT-FILE. 
  