**************************** TERMINAL.PRG **********************************
*
* This version runs under: FoxPro and DbaseIV
* -------------------
*
* This is a sample program which demonstrates a number of the COMETMP library
* commands used to emulate a simple terminal program.
*
* Command keys while in TERMINAL:
* F2 - Clears the screen
* F3 - Send a file or group of files(if Ymodem specified for protocol)
* F4 - Receive a file or files(if Ymodem)
* ESC - Suspends TERMINAL program or CANCEL an active file transfer
*******************************************************************************
*
SET ESCAPE OFF
SET TALK OFF
SET BELL OFF
SET STATUS OFF
SET SCOREBOARD OFF
SET SAFETY OFF
PUBLIC Event, LF, Msg, ChkCmd, Thresh, Fox, FoxPro, LastMsg, TranHow
PUBLIC ComPort, ComAddr, ComIRQn, ComBaud, ComPrty, ComDBts, ComFlow, ComPhon
TranHow = ' '

DEFINE WINDOW TERMINAL FROM 0,0 TO 23,79 none
ACTIVATE WINDOW TERMINAL
CLEAR

* Let's find out if COMETMP has been LOADed by CALLing without 1st LOADing
* and trapping any "File was not LOADed" (error 91) errors.
DoLoad = .F.
ON ERROR DoLoad = .T.
CALL COMETMP
IF DoLoad
IF FoxPro
Nul = MEMORY() && Prevents FPro from LOADing into EMS frame
ENDIF
LOAD COMETMP && Loads COMETMP.BIN communications library
ENDIF

ON ERROR

Vers = 'VERS' + SPACE(15)
CALL COMETMP WITH Vers && Get version #
Vers = SUBSTR(Vers, 6) && Strip off "VERS " leaving only version info

* Display sign-on message
@ 5, 13 TO 13,65 DOUBLE
@ 7,15 SAY 'TERMINAL - A Terminal Emulation Program Using ...'
@ 9,28 SAY '*** ' + Vers + ' ***'
@ 11,15 SAY 'The B A C K G R O U N D Communication Library'
Msg = 'COPYRIGHT(c) 1989 by CompuSolve, Rockaway, NJ (201)983-9429'
NULL = ShowOn24(Msg)

* Wait loop 
NULL = INKEY(5)

CLEAR

* Get default settings from TERMINAL.MEM file, if present
IF FILE('TERMINAL.MEM')
RESTORE FROM TERMINAL ADDITIVE
ELSE
ComPort = '1'
ComAddr = 'x03F8'
ComIRQn = '4'
ComBaud = '2400 '
ComPrty = 'E'
ComDBts = '7'
ComStop = '1'
ComFlow = 'N'
*
ComPhon = SPACE(20)
ENDIF

@ 6,8 TO 15, 72
@ 7,10 SAY 'COM Port # (1-5) ?' GET ComPort PICTURE '9' VALID ComPort $ '12345' ERROR 'VALID CHOICES: 1 - 5'
@ 8,10 SAY "I/O Address (x#### = heX) ?" GET ComAddr VALID .NOT. '?' $ ComAddr ERROR 'NEED TO SPECIFY PORT ADDRESS IN heX OR DECIMAL'
@ 9,10 SAY "IRQ # (2-7) ?" GET ComIRQn PICTURE '9' VALID ComIRQn $ '234567' ERROR 'VALID CHOICES: 2 - 7'
@10,10 SAY "BAUD Rate (300-38400) ?" GET ComBaud PICTURE 'X9999' VALID VAL(ComBaud) >= 300 .AND. VAL(ComBaud) <=38400 ERROR 'VALID CHOICES: 300 - 38400'
@11,10 SAY "Parity (None, Odd or Even) ?" GET ComPrty PICTURE '!' VALID ComPrty $ 'NEO' ERROR 'VALID CHOICES: None, Even or Odd (N, E or O)'
@12,10 SAY "# Data Bits (7 or 8) ?" GET ComDBts PICTURE '9' VALID ComDBts $ '78' ERROR 'VALID CHOICES: 7 or 8'
@13,10 SAY "Flow Control (Xon/xoff, Rts/cts or None) ?" GET ComFlow PICTURE "!" VALID ComFlow $ 'XRN' ERROR 'VALID CHOICES: Xon/Xoff, Rts/cts or None (X, R or N)'
@14,10 SAY "# Stop Bits (1 or 2) ?" GET ComStop VALID ComStop $ '12' ERROR 'VALID CHOICES: 1 or 2'
READ

RKey = READKEY()
IF MOD(RKey,256) = 12 && ESCape 
SUSPEND
ENDIF

Msg = 'Enter a telephone # to dial (ENTER = local mode) ?'
NULL = ShowOn24(Msg)
@0,0
* Init variables
ChkCmd = ''

* Function keys used to invoke local commands
F1 = 28
F2 = -1 && Clear Screen
F3 = -2 && Send file
F4 = -3 && Receive file
F5 = -4

SET FUNC 'F2' TO ''
SET FUNC 'F3' TO ''
SET FUNC 'F4' TO ''
SET FUNC 'F5' TO ''

* INKEY() values
Up = 5
Dn = 24
Rgt = 4
Lft = 19
BkSpc = 127

* Build OPEN command for COMET
Open = "OPEN COM" + ComPort + "," + ComAddr + "," + ComIRQn + ":" ;
+ ComBaud + "," + ComPrty + "," + ComDBts + ",1," + ComFlow

ClsPort = 'CLOSE #' + ComPort && In case port is being redefined ...
CALL COMETMP WITH ClsPort

CALL COMETMP WITH Open && Now OPEN it for use, that was easy!


*** Must set FLAVOR if using dBASEIV ***
IF .NOT. FoxPro && Must be dBASEIV
Flavor = 'FLAVOR D4'
CALL COMETMP WITH Flavor
ENDIF

* Now we'll dial a phone#
* Request # to dial 1st
PhoneNo = SPACE(20)
@16,10 SAY "Phone # to Dial (ENTER = direct/local) ?" GET ComPhon
READ

*Save settings
SAVE TO TERMINAL ALL LIKE Com????


IF LEN(TRIM(ComPhon)) > 0

* The ATTD is output to instruct HAYES compatible modems to dial a #
Dial = "OUTPUT #" + ComPort + ",ATTD" + TRIM(ComPhon) + CHR(13) && Build OUTPUT command
CALL COMETMP WITH Dial && Have modem dial #

* Now, wait till we sense Data Carrier Detect(DCD) from our COM port.
Msg = "CHECKING FOR MODEM'S DATA CARRIER DETECT (DCD) ..."
NULL = ShowOn24(Msg)
Elapsed = 0 && Simple timer for our DO .. WHILE loop
LastTime = TIME() && Also used for timing purposes
MdmStat = "MSTAT #" + ComPort + "," + SPACE(25) && Build MSTAT command
DO WHILE Elapsed <= 45 .AND. (.NOT. "+DCD" $ MdmStat)
CALL COMETMP WITH MdmStat && Get COM port's modem status

IF LastTime <> TIME() && Test if we need to updated timer count
Elapsed = Elapsed+1 && Another second has gone by ..
LastTime = TIME()
ACTIVATE SCREEN
@ 24, 66 SAY STR(45-Elapsed,2,0) COLOR W/N && Display #secs till abort
ACTIVATE WINDOW TERMINAL
ENDIF

IF INKEY() = 27
EXIT
ENDIF

ENDDO

* Check if we timed out
IF Elapsed > 45
??CHR(7)
Null = ShowOn24("Sorry, can't establish phone connection. Aborting ...")
SUSPEND
ENDIF

ENDIF && If phone # was entered



* Now that we have a call established we have 2 things to do:
* 1) Check COMETMP's receive buffer and display any incoming characters
* 2) Detect any keystrokes and determine if local command or data to output

* #2 is simple, use an ONKEY approach
ON KEY DO GotAkey with .f.

SET ESCAPE ON
ON ESCAPE DO GotAKey WITH .t. && 27 = INKEY() value of ESC key


CLEAR

* Display status message on line 24
Msg = "F2 - Clear | F3 - Send | F4 - Recv | TERM"
LastMsg = Msg
NULL =ShowOn24(Msg)

***************************************************************************
* This is main loop for testing for and displaying any incoming data
DO WHILE .T.
Inp = "INPUT #" + ComPort + "," + SPACE(100) && Build INPUT command
CALL COMETMP WITH Inp && Read COMET's COM port data buffer

AmtRetd = VAL(SUBSTR(Inp,10,5)) && Determine how many chars were returned, if any
COMactive = IIF(AmtRetd > 0, .T., .F.)

IF AmtRetd > 0
ComData = SUBSTR(Inp, 15, AmtRetd) && Get just the COM data from 
?? ComData
ENDIF
ENDDO

***************************************************************************

***************************** GotAKey *************************************
* Anytime a key gets pressed, we jump here
*
PROCEDURE GotAKey
PARAMETERS EscKey

ON KEY && Disable ON KEY & ON ESCAPE
ON ESCAPE

IF EscKey
Key = 27
ELSE
Key = INKEY()
ENDIF

DO CASE && Decide whether key is data to output or local command
CASE Key > 0 .AND. Key <> 27 && data to output ?
IF .NOT. 'ACTIVE' $ ChkCmd .OR. TranHow = 'A' && Output if: no xfers active OR ASEND/ARECV active
Output = "OUTPUT #" + ComPort + "," + CHR(Key) && Build OUTPUT command
CALL COMETMP WITH Output && Output char to COM port
ELSE
CLEAR
?? CHR(7)
@ 4,0 TO 12,76 DOUBLE
@ 6,2 SAY "Sorry but we're busy " + event + "ing a file now!"
@ 7,2 SAY "But, that fact that I can display this alert box "
@ 8,2 say "proves COMET is running in the background."
@ 9,2 say "Hit the 'D' key and I'll do a !DIR command in DOS."
@10,2 say "Hit any key ..."
* Wait loop using INKEY(n) if FoxBase+ otherwise Do .. While
Ky = INKEY(5)
IF ky = ASC('D') .OR. ky = ASC('d')
!DIR
ENDIF
ENDIF

CASE Key = 27 && ESC hit ?
IF 'ACTIVE' $ ChkCmd && File transfer active ?
FlshPort = 'FLUSH #' + ComPort
CALL COMETMP WITH FlshPort && If so, user wants to cancel it
ELSE
CALL COMETMP WITH 'ONTIME '
DEACTIVATE WINDOW TERMINAL
ACTIVATE SCREEN
SUSPEND && If no active file transfer, then quit
ENDIF
OTHERWISE && If INKEY() < 0, then a function key was hit
DO Local
ENDCASE

ON KEY DO GotAKey WITH .F. && Enable ON KEY again
ON ESCAPE DO GotAKey WITH .T.
RETURN


****************************** Local ***************************************
* Support for function keys (ie. local commands like send and receive)
PROCEDURE Local

DO CASE
CASE Key = F2 && Clear screen ?
CLEAR
CASE Key = F3 && Send file ?
DO TranFile WITH 'SEND'
CASE Key = F4 && Receive file ?
DO TranFile WITH 'RECV'
CASE Key = F5 && ONTIME command requesting STATUS update ?
DO Status
ENDCASE

RETURN

************************ TranFile *******************************************
PROCEDURE TranFile
PARAMETERS Action
IF 'ACTIVE' $ ChkCmd && We're good, but not that good that we can have two transfers simultaneously!
Msg = 'Request denied ! There is a file transfer ACTIVE'
NULL = ShowOn24(Msg)
NULL = INKEY(3)
Msg = LastMsg
NULL = ShowOn24(Msg)
RETURN
ENDIF

ExitFlg = .F.
DEFINE WINDOW INPUT FROM 15,5 TO 20,75
ACTIVATE WINDOW INPUT

* Prompt for transfer protocol desired (Ascii, Xmodem, Xmodem-1K or Ymodem)
* We don't use a VALID clause since DBASE doesn't support
TranHow = ' '
Null = ShowOn24("CHOOSE FILE PROTOCOL: A=Ascii, X=Xmodem, X1=Xmodem(1K) or Y=Ymodem")
DO WHILE .NOT. (ExitFlg .OR. LTRIM(RTRIM(TranHow)) $ 'AX1Y')
@ 1, 0 SAY 'Protocol(A,X,X1 or Y) ?' GET TranHow PICTURE '@! A9'
READ && Get protocol
ExitFlg = IIF(READKEY()=12 .OR. READKEY()=268, .T., ExitFlg)
?? IIF(.NOT. LTRIM(RTRIM(TranHow)) $ 'AX1Y', CHR(7), '') && Beep if invalid
ENDDO

TranHow = LTRIM(RTRIM(TranHow))

* Prompt for filename except for YRECV since filename gets transmitted w/data
TranFil = SPACE(40)
IF .NOT. ExitFlg .AND. (TranHow <> 'Y' .OR. Action = 'SEND')
Null = ShowOn24("ENTER FILENAME TO " + IIF(Action = 'RECV', 'RECEIVE', 'SEND'))
@ 1, 29 SAY 'Filename ?' GET TranFil PICTURE '@S25'
READ
ExitFlg = IIF(READKEY()=12 .OR. READKEY()=268, .T., ExitFlg)
ENDIF

* Prompt for timeout in seconds if ARECV, default is 60 secs
TimeOut = 60
IF .NOT. ExitFlg .AND. TranHow = 'A' .AND. Action = 'RECV'
Null = ShowOn24("ENTER RECEIVER IDLE TIME IN SECONDS BEFORE AUTO-CLOSING OF FILE")
@ 2, 20 SAY 'ARECV timeout in seconds ?' GET TimeOut PICTURE "999"
READ
ExitFlg = IIF(READKEY()=12 .OR. READKEY()=268, .T., ExitFlg)
ENDIF

RELEASE WINDOW INPUT

IF ExitFlg && Look for ESC key
Null = ShowOn24(Msg)
RETURN
ENDIF

*Now build COMETMP SEND or RECV command
TranCmd = TranHow + Action + ' #' + ComPort + ',' + TRIM(TranFil)
IF 'ARECV' $ TranCmd .AND. TimeOut <> 60 && Test if we need ARECV timeout option
TranCmd = TranCmd + ',' + STR(TimeOut,3,0)
ENDIF

* If X/YModem, port must be set to 8 data bits/No parity
IF TranHow # 'A' && ASCII file xfer?
DBits7 = AT(',7,', Open) && Currently OPENed for 7 data bits ?
IF DBits7 > 0
OpnN8 = STUFF(Open,DBits7-1,3,"N,8") && Create modified version of original Open
CALL COMETMP WITH OpnN8
ENDIF
ENDIF

* Issue command to COMETMP
CALL COMETMP WITH TranCmd && Startup background file transfer

*Check that file transfer was able to start
ChkCmd = 'FCHK #' + ComPort + ',' + SPACE(80)
CALL COMETMP WITH ChkCmd
IF .NOT. 'ACTIVE' $ ChkCmd && Should be active if command started!
LBracAt = AT('[',ChkCmd) && Find start of FCHK failure description, if any
IF LBracAt > 0 && If [ present, we have a failure description
RBracAt = AT(']', ChkCmd) && Find ] which is end of description
Reason = SUBSTR(ChkCmd, LBracAt+1, RBracAt-LBracAt-1)
ELSE
Reason = 'GENERAL ERROR'
ENDIF
?? CHR(7) && If wasn't successful at starting SEND, alert operator
Msg = LEFT(Msg,37) + Action + ' Command Failed - ' + Reason
NULL = ShowOn24(Msg)
NULL = INKEY(3)
Msg = LastMsg
NULL = ShowOn24(Msg)
CALL COMETMP WITH Open && Restore original COM port OPEN params
RETURN
ENDIF

Event = TranHow + Action && This will be used by Status procedure
Thresh = 0
DO Status

*File Send or Recv in progress, now use ONTIME command to update status every 3 secs
*STATUS procedure will now execute every 5 seconds
OnTime = 'ONTIME 5,0,63' && #secs=5, ASCII cd=0 , Aux Byte=63 (F5 key)
CALL COMETMP WITH OnTime

RETURN && All done, returns back to Local proc


*************************** Status ************************************
* F10 key or COMETMP's ONTIME command brings us here
* Updates bottom line on screen with file transfer status
*
PROCEDURE Status
PRIVATE CurR, CurC

CurR = ROW() && Save TERMINAL window's cursor loc
CurC = COL()

ChkCmd = 'FCHK #' + ComPort + ',' + SPACE(80)
CALL COMETMP WITH ChkCmd && Get current file transfer status

* Now extract the status info we want; FCHK's status, size and filename
FCHKstat = SUBSTR(ChkCmd,25,8) && Status - ACTIVE, COMPLETE or FAILED
FCHKsize = SUBSTR(ChkCmd,34,7) && Size in bytes - #######
FCHKfile = SUBSTR(ChkCmd,42) && Filename - path\filename (variable length)

* Adjust filename if necessary
SpcAt = AT(' ',FCHKfile) && Look for end of path\filename
FCHKfile = IIF(SpcAt > 0, SUBSTR(FCHKfile,1,SpcAt-1), FCHKfile)
FCHKfile = IIF(LEN(FCHKfile) > 12, RIGHT(FCHKfile,12), FCHKfile)

* Append failure description to FCHKstat - if FAILED
IF 'FAILED' $ FCHKstat
LBracAt = AT('[',ChkCmd) && Find start of FCHK failure description, if any
RBracAt = AT(']', ChkCmd) && Find ] which is end of description
Reason = SUBSTR(ChkCmd, LBracAt+1, RBracAt-LBracAt-1)
FCHKstat = FCHKstat + Reason
FCHKfile = "" && Need the room to display failure description
ENDIF



* Display extracted status
Msg = LEFT(Msg,37) + Event + ' | ' + FCHKstat + ' | ' + FCHKsize + ' | ' + FCHKfile
NULL = ShowOn24(Msg)

IF .NOT. 'ACTIVE' $ ChkCmd && COMPLETEd or FAILED ?
Thresh = Thresh + 1
IF Thresh > 1 && Don't want to redisplay old stat msg till 1 cycle
Ontime = 'ONTIME'
CALL COMETMP WITH Ontime && If so, turn off timer event trapping
Msg = LastMsg
NULL = ShowOn24(Msg)
ELSE

?? CHR(7) && Call attention to COMPLETE or FAILED status
IF TranHow # 'A'
CALL COMETMP WITH Open && Restore original COM port OPEN params
ENDIF

ENDIF
ENDIF

@ CurR, CurC SAY ''

RETURN

FUNCTION ShowOn24
PARAMETERS MsgToOut
PRIVATE WindFunc, NowWind

WindFunc = IIF(FoxPro, 'WOUTPUT()', 'WINDOW()')
NowWind = &WindFunc

ACTIVATE SCREEN
Strt = INT( (80 - LEN(MsgToOut))/2 )
@ 24,0 SAY SPACE(80) COLOR N/W
@ 24, Strt SAY MsgToOut COLOR N/W
ACTIVATE WINDOW &NowWind

RETURN ''