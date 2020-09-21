*********************************************************************** 00001000
*                                                                     * 00002000
*    VSE KERMIT UPDATE CONVERTOR                                      * 00003000
*                                                                     * 00004000
*    Author: Jeff Huestis, Washington University Libraries            * 00005000
*                                                                     * 00006000
*    Purpose: Read sequential file of updates in GUPI format and      * 00007000
*             submit a job via POWER to update Kermit sources.        * 00008000
*                                                                     * 00009000
*    Method: Read update file from SYSIPT and create LIBR control     * 00010000
*        sequences for individual library members based on a table    * 00011000
*        of starting sequence numbers for the Kermit source.  Write   * 00012000
*        the output to SYSPCH along with LIBR directives for copying  * 00013000
*        the Kermit source library members to a work library.  The    * 00014000
*        updates are then applied to the work library.  Any errors    * 00015000
*        are noted on SYSLST for the conversion process.  Bad syntax  * 00016000
*        in the input file will cause the program to abort.           * 00017000
*                                                                     * 00018000
*    IKXTRN parameters are entered is the first lines in the input    * 00019000
*        file.  Parameter lines are recognized by a keyword or all    * 00020000
*        numeric string in columns 1 through 8, and "=" in column 9.  * 00021000
*                                                                     * 00022000
*    REQUIRED Parameters:                                             * 00023000
*  1. BASELIBR= specify a 'library.sublibrary' string indicating where* 00024000
*     unmodified KERMIT source is kept.                               * 00025000
*  2. WORKLIBR= specify a 'library.sublibrary' string for the library * 00026000
*     where modified, working copies of KERMIT source are to be kept. * 00027000
*  3. LOCALLST= specify the text you want for the * $$ LST card for   * 00028000
*     the job which IKXTRN will punch to the POWER internal reader.   * 00029000
*                                                                     * 00030000
*    OPTIONAL Parameters:                                             * 00031000
*  4. LOCALSLI= parameter is optional.  Use it to specify the name of * 00032000
*     a library member containing label information not in the        * 00033000
*     standard label area.  This member will be copied into the       * 00034000
*     generated job by a POWER SLI statement.  The generated SLI will * 00035000
*     use the "MEM=" format, so the member name qualifier should be   * 00036000
*     included.  Example:  LOCALSLI=KERMSLIS.P                        * 00037000
*  5. nnnnnnnn= Future releases of CICS Kermit (or local re-packaging)* 00038000
*     may alter the sequence of module names included in the table    * 00039000
*     starting at label STARTNUM.  For this reason, it is possible to * 00040000
*     replace this table with a series of lines giving the starting   * 00041000
*     line number for each module in columns 1 through 8 and the name * 00042000
*     of the module in the 12 columns from column 10 onwards.  If this* 00043000
*     option is used, the entire table must be replaced, and the      * 00044000
*     parameter cards must be in ascending order by line sequence #.  * 00045000
*                                                                     * 00046000
*    Update history:                                                  * 00047000
*    1.0 - 1991 January - Implementation complete, except for the     * 00048000
*                         resequence command "./ S".                  * 00049000
*    1.1 - 1991 January - Updated by Jeff Huestis and John Chandler.  * 00050000
*                         Libraries specified via PARM                * 00051000
*    1.2 - 1991 March   - Updated by Jeff Huestis.                    * 00052000
*                         Libraries and other parameters specified    * 00053000
*                         via SYSIPT.                                 * 00054000
*    1.3 - 1991 July    - Updated by John Chandler.                   * 00054300
*                         Input line number in error messages.        * 00054600
*    1.4 - 1992 May     - Updated by John Chandler. Ignore blank lines* 00054800
*                                                                     * 00055000
*********************************************************************** 00056000
*                                                                       00057000
*    REGISTER EQUATES                                                   00058000
R0       EQU   0                                                        00059000
R1       EQU   1                                                        00060000
R2       EQU   2                                                        00061000
R3       EQU   3                                                        00062000
R4       EQU   4                                                        00063000
R5       EQU   5                                                        00064000
R6       EQU   6                                                        00065000
R7       EQU   7                                                        00066000
R8       EQU   8                                                        00067000
R9       EQU   9                                                        00068000
R10      EQU   10                                                       00069000
R11      EQU   11                                                       00070000
R12      EQU   12                                                       00071000
R13      EQU   13                                                       00072000
R14      EQU   14                                                       00073000
R15      EQU   15                                                       00074000
*                                                                       00075000
IKXTRN   CSECT                                                          00076000
         BALR  12,0              STANDARD ENTRY LINKAGE                 00077000
         USING *,12                                                     00078000
         LA    13,REGSAVE                                               00079000
         B     A000              BRANCH AROUND PROGRAM ID & SAVE AREA   00080000
PGRMNAM  DC    CL8'IKXTRN'       PROGRAM NAME                           00081000
         DC    PL2'1,4'          VERSION/LEVEL                          00082000
REGSAVE  DS    9D                REGISTER SAVE AREA                     00083000
*                                                                       00084000
*********************************************************************** 00085000
*   PROCESS PARM DATA                                                   00086000
*********************************************************************** 00087000
A000     EQU   *                                                        00088000
         XC    NRECBL,NRECBL Initialize blank count                     00088200
         XC    NRECIN,NRECIN Initialize input count                     00088500
         OPEN  LIST,INPFILE,OUTFILE          OPEN FILES                 00089000
         MVC   MESSLINE(35),=C'FILES OPENED; PARAMETER CARDS READ:'     00090000
         PUT   LIST,MESSAGE                                             00091000
         L     R4,=A(STARTNUM)       POINT TO START OF MODULE TABLE     00092000
         USING STARTNUM,R4                                              00093000
         LA    R5,MODCOUNT       GET DEFAULT MODULE COUNT               00094000
         STH   R5,NUMMOD            AND STORE WHERE IT COUNTS           00095000
         LA    R5,STARTEND       GET POINTER TO LAST MODULE OF DEFAULT  00096000
         ST    R5,LSTMOD            AND STORE IT FOR POSSIBLE CHANGE    00097000
         XR    R5,R5             INITIALIZE MODULE NAME COUNTER         00098000
NEXTPARM EQU   *                                                        00099000
         LA    0,1                                                      00099200
         A     0,NRECIN                                                 00099400
         ST    0,NRECIN      Count input records                        00099600
         GET   INPFILE            GET ANOTHER INPUT CARD                00100000
         CLC   =C'./ ',INRECORD   ARE WE DONE WITH PARM CARDS?          00101000
         BE    JCLPROC               YES, GO PROCESS JCL TABLE          00102000
         MVC   MESSLINE(80),INRECORD NO, ECHO INPUT TO SYSLST           00103000
         PUT   LIST,MESSAGE                                             00104000
         CLC   BLANKS(72),INRECORD Blank card?                          00104100
         BNE   NEXTPAR1      No, assume it's a parm card                00104200
         LA    0,1           Yes, skip it entirely                      00104300
         A     0,NRECBL                                                 00104400
         ST    0,NRECBL      But count blank records                    00104500
         B     NEXTPARM      Go get another record                      00104600
NEXTPAR1 DS    0H                                                       00104700
         CLI   INRECORD+8,C'='    DO WE HAVE A VALID CARD?              00105000
         BNE   PARMEXIT           No, assume we're done with parms      00106000
         CLC   =C'BASELIBR',INRECORD      SEE IF THIS IS BASE LIBRARY   00107000
         BNE   NOTBASE                       NO, TRY SOMETHING ELSE     00108000
         MVC   CONNECT+18(16),INRECORD+9  YES, MOVE IT INTO JCL TABLE   00109000
         OI    LIBFLAG,X'01'              INDICATE IT WAS FOUND         00110000
         B     NEXTPARM                   GO READ ANOTHER CARD          00111000
NOTBASE  EQU   *                                                        00112000
         CLC   =C'WORKLIBR',INRECORD      SEE IF THIS IS WORK LIBRARY   00113000
         BNE   NOTWORK                       NO, TRY SOMETHING ELSE     00114000
         MVC   CONNECT+35(16),INRECORD+9  YES, MOVE IT INTO JCL TABLE   00115000
         MVC   ACCESS+17(16),INRECORD+9      IN TWO PLACES              00116000
         OI    LIBFLAG,X'02'              INDICATE IT WAS FOUND         00117000
         B     NEXTPARM                   GO READ ANOTHER CARD          00118000
NOTWORK  EQU   *                                                        00119000
         CLC   =C'LOCALSLI',INRECORD      IS THERE A POWER SLI CARD?    00120000
         BNE   NOTSLI                        NO, TRY SOMETHING ELSE     00121000
         MVC   SLI(13),=C'* $$ SLI MEM='  YES, CHANGE COMMENT TO SLI    00122000
         MVC   SLI+13(17),INRECORD+9         AND PUT IN MEMBER NAME     00123000
         B     NEXTPARM                   GO READ ANOTHER CARD          00124000
NOTSLI   EQU   *                                                        00125000
         CLC   =C'LOCALLST',INRECORD      IS THIS THE LOCAL LST CARD?   00126000
         BNE   NOTLST                        NO, GO LOOK FOR MODNAMES   00127000
         MVC   LSTCARD+9(JOBMX-9),INRECORD+9  YES, MOVE LST DATA        00128000
         OI    LIBFLAG,X'04'              INDICATE LST FOUND            00129000
         B     NEXTPARM            GO READ ANOTHER CARD                 00130000
NOTLST   EQU   *                                                        00131000
         LA    R3,INRECORD        OTHER POSSIBILITIES EXHAUSTED, THIS   00132000
         LR    R2,R3                 BETTER BE A MODULE CARD            00133000
         BAL   R14,DIGITVER          SO CHECK FOR START NUMBER          00134000
         SR    R2,R3                                                    00135000
         CH    R2,=H'8'           LENGTH 8 => PROPER NUMERIC STRING     00136000
         BNE   NOPARM                NOPE                               00137000
         MVC   STARTNUM,INRECORD        OKAY, MOVE START # TO TABLE     00138000
         MVC   MODNAME,INRECORD+9       MOVE NAME TO TABLE              00139000
         LA    R4,STTBLLEN(,R4)         POINT TO NEXT TABLE ENTRY       00140000
         ST    R4,LSTMOD                UPDATE LAST-MODULE POINTER      00141000
         LA    R5,1(,R5)                INCREMENT MODULE COUNTER        00142000
         B     NEXTPARM                                                 00143000
         DROP  R4                                                       00144000
*********************************************************************** 00145000
*   COPY JCL TABLE LINES TO SYSPCH                                      00146000
*********************************************************************** 00147000
PARMEXIT DS    0H                                                       00147200
         MVI   MESSLINE,C' ' Clear message buffer                       00147260
         MVC   MESSLINE+1(79),MESSLINE                                  00147320
         MVC   MESSLINE(49),=C'PRECEDING INVALID CARD TERMINATES PARM P$00147400
               ROCESSING'                                               00147600
         PUT   LIST,MESSAGE                                             00147800
JCLPROC  EQU   *                                                        00148000
         TM    LIBFLAG,X'07' DID WE GET THE REQUIRED PARAMETERS?        00149000
         BNO   NOPARM           NO, GO ABORT                            00150000
         MVC   OUTREC,BLANKS       Clear output buffer                  00151000
         LA    R3,JOBCARDS   Start of JCL table                         00152000
         LTR   R5,R5         DID WE HAVE ANY MODULE CARDS IN DECK?      00153000
         BZ    JCLLOOP          NO; LEAVE DEFAULT                       00154000
         STH   R5,NUMMOD        YES; ADJUST COUNT                       00155000
JCLLOOP  EQU   *                                                        00156000
         MVC   OUTREC(JOBMX),0(R3) Copy a line to output                00157000
         CLC   COPIES,OUTREC Reached list of COPY directives?           00158000
         BNE   JCLWRITE      No, just write the record                  00159000
*  Fill in names of modules in COPY directives                          00160000
         L     R2,=A(STARTNUM)   Start of module table                  00161000
         LH    R5,NUMMOD     Length of table (in entries)               00162000
JCLLP1   DS    0H                                                       00163000
         MVC   OUTREC+8(12),8(R2)    Copy module name                   00164000
         LA    R4,OUTREC+8   Start of name                              00165000
         LA    R4,1(,R4)                                                00166000
         CLI   0(R4),C' '    Look for end of name                       00167000
         BNE   *-8                                                      00168000
         MVC   1(20,R4),=CL20'REPLACE=YES'                              00169000
         PUT   OUTFILE                                                  00170000
         LA    R2,STTBLLEN(,R2)                                         00171000
         BCT   R5,JCLLP1                                                00172000
         B     JCLLP2        Finished COPY directives                   00173000
JCLWRITE DS    0H                                                       00174000
         PUT   OUTFILE               PUNCH IT OUT                       00175000
JCLLP2   DS    0H                                                       00176000
         LA    R3,JOBMX(,R3)       Point to next line                   00177000
         CLC   =C'EOD',0(R3)       CHECK FOR END OF TABLE               00178000
         BNE   JCLLOOP             Not yet, keep copying                00179000
         B     PROCESS             Go process first update card         00180000
*                                                                       00181000
*********************************************************************** 00182000
*   PROCESS UPDATE LINES                                                00183000
*********************************************************************** 00184000
SKIPBLNK LA    0,1                                                      00184200
         A     0,NRECBL                                                 00184400
         ST    0,NRECBL      Count blank records                        00184600
*          Main loop - read a control card                              00185000
CTLP     DS    0H                                                       00186000
         LA    0,1                                                      00186200
         A     0,NRECIN                                                 00186400
         ST    0,NRECIN      Count input records                        00186600
         GET   INPFILE            READ NEXT LINE                        00187000
         CLC   BLANKS(72),INRECORD Blank card?                          00187300
         BE    SKIPBLNK            Yes, skip it                         00187600
         CLC   =C'./ ',INRECORD    Control card?                        00188000
         BNE   BADDECK             No, too bad                          00189000
*                                                                       00190000
*          Process an editing card                                      00191000
PROCESS  EQU   *                                                        00192000
         LA    R2,INRECORD+3       Start after control indicator        00193000
         LA    R8,SYNTAX           Error handler for running off end    00194000
         BAL   R14,BLNKSKIP        Find command code                    00195000
         MVC   CMD,0(R2)           Save code                            00196000
         CLI   1(R2),C' '          Should be 1 char                     00197000
         BNE   SYNTAX              No, bad syntax                       00198000
         CLI   CMD,C'*'            Comment?                             00199000
         BE    CTLP                Yes, ignore it                       00200000
         LA    R2,2(,R2)           Skip over command code               00201000
         BAL   R8,ARGLOAD          Get a numeric argument               00202000
          B    SYNTAX              Missing, too bad                     00203000
         UNPK  FRSTLINE,ARGNO      Get zoned copy                       00204000
         MVC   LASTLINE,BLANKS     Blank out last line as default       00205000
         BAL   R8,ARGLOAD          Get 2nd numeric argument, if any     00206000
          B    PROCES2             Just one card                        00207000
         UNPK  LASTLINE,ARGNO      Get zoned copy of last line          00208000
         BAL   R8,ARGLOAD          Should be followed by '$' (or end)   00209000
          B    PROCES2             Ok                                   00210000
         B     SYNTAX              Oops, extra number there             00211000
PROCES2  DS    0H                                                       00212000
         CLI   CMD,C'D'                                                 00213000
         BE    NOSEQ               Done if delete                       00214000
         LA    R2,1(,R2)           Skip over '$'                        00215000
         BAL   R8,ARGLOAD          Get output line number, if any       00216000
          B    SYNTAX              Not given, too bad                   00217000
         MVC   LINENUM,ARGNO       FIRST NUMBER SHOULD BE START LINE #  00218000
         BAL   R8,ARGLOAD          Get increment value, if any          00219000
          B    SYNTAX              Not given, too bad                   00220000
         MVC   INCRMENT,ARGNO      Ok, use it                           00221000
NOSEQ    EQU   *                                                        00222000
         BAL   R8,MODSRCH          GO FIND OUT NAME OF MODULE           00223000
         MVC   OUTREC,BLANKS       Clear output buffer                  00224000
         CLI   FRSTFLAG,X'FF'      SEE IF THIS IS FIRST TIME THROUGH    00225000
         BE    FIRST               YES                                  00226000
         CLC   MODOLD,MODULE       Same module?                         00227000
         BNE   SWITCH              No, start new update                 00228000
         CLC   LASTOLD,FRSTLINE    Later in same module?                00229000
         BL    CONTINUE            Yes, keep same update                00230000
SWITCH   DS    0H                                                       00231000
         MVC   OUTREC(4),=C')END'  NO, TERMINATE PREVIOUS UPDATE        00232000
         PUT   OUTFILE                                                  00233000
FIRST    EQU   *                                                        00234000
         XC    FRSTFLAG,FRSTFLAG   CLEAR LOOP TAG FOR FUTURE TESTS      00235000
         MVC   OUTREC(10),=C'   UPDATE '  SET UP VSE LIBR DIRECTIVE     00236000
         MVC   OUTREC+10(12),MODULE       SUPPLY MODULE NAME            00237000
         MVC   OUTREC+22(14),=C',SE=FS,C=73:80'  LINE SEQUENCING INFO   00238000
         PUT   OUTFILE             WRITE OUTPUT RECORD                  00239000
         MVC   MODOLD,MODULE       Save for comparison next time        00240000
CONTINUE DS    0H                                                       00241000
         MVC   LASTOLD,FRSTLINE    Ditto                                00242000
         MVC   OUTREC,BLANKS       Clear output buffer                  00243000
         MVC   OUTREC+5(8),FRSTLINE   PUT IN LOCATION WHERE IT STARTS   00244000
*    NOW SEE WHAT TYPE OF UPDATE IT IS                                  00245000
ICHECK   EQU   *                                                        00246000
         CLI   CMD,C'I'            Insert?                              00247000
         BNE   DCHECK              NO, GO CHECK FOR DELETION            00248000
         MVC   OUTREC(4),=C')ADD'  YES, SET UP VSE LIBR FORM            00249000
         B     PUTCMND             GO WRITE IT OUT                      00250000
DCHECK   EQU   *                                                        00251000
*          Now must be D or R, so copy end of range                     00252000
         MVI   OUTREC+13,C','      Delimiter and                        00253000
         MVC   OUTREC+14(8),LASTLINE  Location of end of range          00254000
         CLI   CMD,C'D'            Delete?                              00255000
         BNE   RCHECK              NO, GO CHECK FOR REPLACEMENT         00256000
         MVC   OUTREC(4),=C')DEL'  YES, SET UP VSE LIBR FORM            00257000
         B     PUTCMND             GO WRITE IT OUT                      00258000
RCHECK   EQU   *                                                        00259000
         CLI   CMD,C'R'            Replace?                             00260000
         BNE   SYNTAX              NO, ERROR                            00261000
         MVC   OUTREC(4),=C')REP'  YES, SET UP VSE LIBR FORM            00262000
PUTCMND  EQU   *                                                        00263000
         PUT   OUTFILE             WRITE UPDATE DIRECTIVE OUT           00264000
*    NOW GET THE UPDATE LINES THEMSELVES                                00265000
LINELOOP EQU   *                                                        00266000
         LA    0,1                                                      00266200
         A     0,NRECIN                                                 00266400
         ST    0,NRECIN      Count input records                        00266600
         GET   INPFILE             GET ANOTHER INPUT LINE               00267000
         CLC   =C'./ ',INRECORD    Control card?                        00268000
         BE    PROCESS             YES, PREVIOUS UPDATES ARE FINISHED   00269000
         CLC   BLANKS(72),INRECORD Blank card?                          00269100
         BNE   LINELOO1      No, must be new data                       00269200
         LA    0,1           Yes, skip it entirely                      00269300
         A     0,NRECBL                                                 00269400
         ST    0,NRECBL      But count blank records                    00269500
         B     LINELOOP      Go get another record                      00269600
LINELOO1 DS    0H                                                       00269700
         UNPK  ZONEHOLD,LINENUM    NO, UNPACK CURRENT LINE NUMBER       00270000
         OI    ZONEHOLD+7,X'F0'    GET RID OF ZONED DECIMAL SIGN        00271000
         MVC   OUTREC,INRECORD     COPY RECORD                          00272000
         MVC   OUTREC+72(8),ZONEHOLD  AND OVERLAY LINE SEQUENCE NUMBER  00273000
         PUT   OUTFILE             WRITE OUTPUT RECORD                  00274000
         MVC   LASTOLD,ZONEHOLD    Latest line put out                  00275000
         AP    LINENUM,INCRMENT    INCREMENT LINE NUMBER FOR NEXT PASS  00276000
         B     LINELOOP            GO GET ANOTHER LINE                  00277000
*    END OF INPUT FILE REACHED                                          00278000
A100     EQU   *                                                        00279000
         MVC   OUTREC,BLANKS       Clear output buffer                  00280000
         MVC   OUTREC(4),=C')END'  TERMINATE PREVIOUS UPDATE            00281000
         PUT   OUTFILE                AND WRITE IT OUT                  00282000
         MVC   OUTREC,BLANKS       Clear output buffer                  00283000
*    WRITE TERMINATION SEQUENCE TO GENERATED JOB                        00284000
         MVC   OUTREC(2),=C'/*'                                         00285000
         PUT   OUTFILE                                                  00286000
         MVC   OUTREC(2),=C'/&&'                                        00287000
         PUT   OUTFILE                                                  00288000
         MVC   OUTREC(8),=C'* $$ EOJ'                                   00289000
         PUT   OUTFILE                                                  00290000
EOJ      EQU   *                                                        00291000
         BAL   R8,PRNTSKIP   Print number of skipped records, if any    00291500
         CLOSE INPFILE,OUTFILE,LIST                                     00292000
         EOJ                                                            00293000
*                                                                       00294000
*          Find name of module to be updated                            00295000
MODSRCH  EQU   *                                                        00296000
         LH    R7,NUMMOD           Number of entries in table           00297000
         L     R2,LSTMOD           End of table                         00298000
MODLOOP  EQU   *                                                        00299000
         SH    R2,=Y(STTBLLEN)     Back up one entry                    00300000
         CLC   0(8,R2),FRSTLINE     COMPARE START LINE TO CURRENT ENTRY 00301000
         BNH   FOUND               ENTRY L.E. => THIS IS THE ONE        00302000
         BCT   R7,MODLOOP          DECREMENT INDEX                      00303000
NOTFOUND EQU   *                                                        00304000
         XC    MODULE,MODULE       ZERO VALUE INDICATES ERROR           00305000
         MVC   MESSLINE,BLANKS Clear message buffer                     00305500
         MVC   MESSLINE(21),=C'MODULE NAME NOT FOUND'                   00306000
         PUT   LIST,MESSAGE                                             00307000
         MVC   MESSLINE(80),INRECORD                                    00308000
         PUT   LIST,MESSAGE                                             00309000
         BR    R8                  RETURN TO CALLER                     00310000
FOUND    EQU   *                                                        00311000
         MVC   MODULE,8(R2)        Copy module name from table          00312000
         BR    R8                 RETURN TO CALLER                      00313000
*          Table of starting line numbers and module names              00314000
*                                                                       00315000
*          Find next numeric field in card, skip if ok                  00316000
*          Clobbers R3, R14.  Returns via R8.  Advances R2              00317000
ARGLOAD  DS    0H                                                       00318000
         BAL   R14,BLNKSKIP                                             00319000
         LR    R3,R2                                                    00320000
         CLI   0(R3),C'0'          Numeric?                             00321000
         BLR   R8                  No, return without skipping          00322000
         BAL   R14,DIGITVER                                             00323000
         BCTR  R2,0                Last char                            00324000
         SR    R2,R3               Count - 1                            00325000
         EX    R2,PCKA                                                  00326000
         OI    ARGNO+4,15          Fix sign, just in case               00327000
         LA    R2,1(R2,R3)         RESTORE POINTER TO END + 1           00328000
         B     4(,R8)              Return and skip                      00329000
PCKA     PACK  ARGNO,0(,R3)        Get packed decimal                   00330000
*          Scan R2 until it reaches a non-blank character               00331000
BLNKSKIP EQU   *                                                        00332000
         CLI   0(R2),C' '                                               00333000
         BNER  R14                                                      00334000
         LA    R2,1(,R2)                                                00335000
         C     R2,=A(INRECORD+50)  Off the end?                         00336000
         BNLR  R8                  Yes, go to error handler             00337000
         B     BLNKSKIP                                                 00338000
*          Scan R2 until it reaches a non-numeric character             00339000
DIGITVER EQU   *                                                        00340000
         CLI   0(R2),C'0'                                               00341000
         BLR   R14                                                      00342000
         CLI   0(R2),C'9'                                               00343000
         BHR   R14                                                      00344000
         LA    R2,1(,R2)                                                00345000
         C     R2,=A(INRECORD+50)  Off the end?                         00346000
         BNLR  R8                  Yes, go to error handler             00347000
         B     DIGITVER                                                 00348000
PRNTSKIP ICM   0,15,NRECBL   Test count of blank records                00348100
         BZR   R8                                                       00348200
         MVC   MESSLINE,BLANKS Clear message buffer                     00348300
         MVC   MESSLINE(29),=C'BLANK INPUT RECORDS IGNORED: '           00348400
         CVD   0,TEMPDW                                                 00348500
         OI    TEMPDW+7,15                                              00348600
         UNPK  MESSLINE+29(5),TEMPDW                                    00348700
         PUT   LIST,MESSAGE                                             00348800
         BR    R8                                                       00348900
BADDECK  EQU   *                                                        00349000
         MVC   MESSLINE,BLANKS Clear message buffer                     00349500
         MVC   MESSLINE(34),=C'OUT-OF-SEQUENCE STATEMENT AT LINE '      00350000
         L     0,NRECIN      Input record count                         00350100
         CVD   0,TEMPDW                                                 00350200
         OI    TEMPDW+7,15                                              00350300
         UNPK  MESSLINE+34(5),TEMPDW                                    00350400
         PUT   LIST,MESSAGE                                             00351000
         MVC   MESSLINE(80),INRECORD                                    00352000
         PUT   LIST,MESSAGE                                             00353000
         B     EOJ                                                      00354000
NOPARM   EQU   *                                                        00355000
         MVC   MESSLINE,BLANKS                                          00355500
         MVC   MESSLINE(26),=C'MISSING OR INVALID PARM(S)'              00356000
         PUT   LIST,MESSAGE                                             00357000
         B     EOJ                                                      00358000
SYNTAX   DS    0H                                                       00359000
         MVC   MESSLINE,BLANKS                                          00359500
         MVC   MESSLINE(33),=C'INVALID UPDATE STATEMENT AT LINE '       00360000
         L     0,NRECIN      Input record count                         00360100
         CVD   0,TEMPDW                                                 00360200
         OI    TEMPDW+7,15                                              00360300
         UNPK  MESSLINE+33(5),TEMPDW                                    00360400
         MVI   MESSLINE+38,C':'                                         00360500
         PUT   LIST,MESSAGE                                             00361000
         MVC   MESSLINE(80),INRECORD                                    00362000
         PUT   LIST,MESSAGE                                             00363000
         B     CTLP                                                     00364000
INPERROR EQU   *                                                        00365000
         MVC   MESSLINE(19),=C'ERROR ON INPUT FILE'                     00366000
         PUT   LIST,MESSAGE                                             00367000
         BAL   R8,PRNTSKIP   Print number of skipped records, if any    00367500
ABORT    SVC   50                                                       00368000
*                                                                       00369000
TEMPDW   DS    D             Work area                                  00369300
NRECBL   DS    F             Counter for input blank records            00369400
NRECIN   DS    F             Input record counter                       00369600
LSTMOD   DS    F                                                        00370000
NUMMOD   DS    H                                                        00371000
LIBFLAG  DC    X'00'                                                    00372000
MODULE   DS    CL12         NAME OF MODULE TO WHICH UPDATE LINES APPLY  00373000
ZONEHOLD DS    CL8          PLACE TO HOLD ZONED DECIMAL NUMBERS         00374000
MODOLD   DS    CL12          Saved MODULE from previous control group   00375000
LASTOLD  DS    CL8           Saved output line number from ditto        00376000
ARGNO    DS    PL5           Number read from control card              00377000
CMD      DS    C             Control command code                       00378000
LINENUM  DS    PL5          STARTING LINE # FROM INPUT RECORD           00379000
INCRMENT DS    PL5          INCREMENT VALUE FROM INPUT RECORD           00380000
FRSTLINE DS    CL8           Line where changes start                   00381000
LASTLINE DS    CL8           Last line to delete or replace             00382000
INRECORD DS    CL81                                                     00383000
OUTCARD  DS    0CL81                                                    00384000
         DC    C'W'                                                     00385000
OUTREC   DS    CL80         OUTPUT WORK AREA                            00386000
FRSTFLAG DC    X'FF'                                                    00387000
MESSAGE  DC    C' '                                                     00388000
MESSLINE DC    CL132' '                                                 00389000
BLANKS   DC    132C' '                                                  00390000
         LTORG                                                          00391000
JOBMX    EQU   60                  Maximum length string below          00392000
JOBCARDS EQU   *                                                        00393000
LSTCARD  DC    CL(JOBMX)'* $$ LST '                                     00394000
         DC    CL(JOBMX)'// JOB APPLY KERMIT UPDATES'                   00395000
SLI      DC    CL(JOBMX)'*'                                             00396000
         DC    CL(JOBMX)'// EXEC LIBR'                                  00397000
CONNECT  DC    CL(JOBMX)'   CONNECT SUBLIB=                :'           00398000
COPIES   DC    CL(JOBMX)'   COPY'      This card is to be duplicated    00399000
         DC    CL(JOBMX)'/*'                                            00400000
         DC    CL(JOBMX)'// EXEC LIBR'                                  00401000
ACCESS   DC    CL(JOBMX)'   ACCESS SUBLIB='                             00402000
         DC    C'EOD'                                                   00403000
*    INPUT AND OUTPUT FILES                                             00404000
INPFILE  DTFDI DEVADDR=SYSIPT,IOAREA1=INRECORD,RECSIZE=81,EOFADDR=A100,X00405000
               ERROPT=INPERROR                                          00406000
OUTFILE  DTFDI DEVADDR=SYSPCH,IOAREA1=OUTCARD,RECSIZE=81                00407000
LIST     DTFPR DEVADDR=SYSLST,CTLCHR=ASA,BLKSIZE=133,WORKA=YES,        X00408000
               IOAREA1=AREA                                             00409000
AREA     DS    CL133                                                    00410000
IKXTMODS CSECT                                                          00411000
STARTNUM DC    CL8'00001000'                                            00412000
MODNAME  DC    CL12'IK0DOC.A'                                           00413000
STTBLLEN EQU   *-STARTNUM          Length of one entry                  00414000
         DC    CL8'00300000',CL12'IK0MAC.A'                             00415000
         DC    CL8'00800000',CL12'IKXMAC.A'                             00416000
         DC    CL8'01400000',CL12'IK0DEF.A'                             00417000
         DC    CL8'01500000',CL12'IK0MAI.A'                             00418000
         DC    CL8'01800000',CL12'IK0COM.A'                             00419000
         DC    CL8'03000000',CL12'IK0CMD.A'                             00420000
         DC    CL8'05000000',CL12'IKXUTL.A'                             00421000
         DC    CL8'07000000',CL12'IK0PRO.A'                             00422000
STARTEND EQU   *                                                        00423000
MODCOUNT EQU   (*-STARTNUM)/STTBLLEN                                    00424000
         DS    100CL(STTBLLEN)                                          00425000
         END                                                            00426000
