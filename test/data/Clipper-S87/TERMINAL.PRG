*********************** TERMINAL.PRG ****************************************
*
* This version runs under: CLIPPER Summer 87 & 5.0
* -------------------------
*
* This is a sample program which demonstrates a number of the COMETMP library
* commands used to emulate a simple terminal program.
*
* Command keys while in TERMINAL:
* F2 - Clears the screen
* F3 - Send a file or group of files(if Ymodem specified for protocol)
* F4 - Receive a file or files(if Ymodem)
* ESC - Exits TERMINAL program or CANCEL an active file transfer
*******************************************************************************
*
SET BELL OFF
SET STATUS ON
SET SCOREBOARD OFF
SET SAFETY OFF
PUBLIC Event, LF, Msg, ChkCmd, Thresh, Fox, FoxPro, LastMsg, TranHow
PUBLIC ComPort, ComAddr, ComIRQn, ComBaud, ComPrty, ComDBts, ComFlow, ComPhon
PUBLIC NKey
TranHow = ' '

CLEAR

Vers = 'VERS' + SPACE(15)
CALL COMETMP WITH Vers && Get version #
Vers = SUBSTR(Vers, 6) && Strip off "VERS " leaving only version info

* Display sign-on message
@ 5, 13 TO 13,65 DOUBLE
@ 7,15 SAY 'TERMINAL - A Terminal Emulation Program Using ...'
@ 9,28 SAY '*** ' + Vers + ' ***'
@ 11,15 SAY 'The B A C K G R O U N D Communication Library'
Msg = 'COPYRIGHT(c) 1989-91 by CompuSolve, Rockaway, NJ (201)983-9429'
DO ShowOn24 WITH Msg

INKEY(5)
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

DO ShowOn24 WITH "ENTER DESIRED COM PORT SETTINGS ..."
@ 6,8 TO 15, 72
@ 7,10 SAY 'COM Port # (1-5) ?' GET ComPort PICTURE '9'
@ 8,10 SAY "I/O Address (x#### = heX) ?" GET ComAddr
@ 9,10 SAY "IRQ # (2-7) ?" GET ComIRQn PICTURE '9'
@10,10 SAY "BAUD Rate (300-38400) ?" GET ComBaud PICTURE 'X9999'
@11,10 SAY "Parity (None, Odd or Even) ?" GET ComPrty PICTURE '!'
@12,10 SAY "# Data Bits (7 or 8) ?" GET ComDBts PICTURE '9'
@13,10 SAY "Flow Control (Xon/xoff, Rts/cts or None) ?" GET ComFlow PICTURE "!"
@14,10 SAY "# Stop Bits (1 or 2) ?" GET ComStop
READ

RKey = READKEY()
IF MOD(RKey,256) = 12 && ESCape 
QUIT
ENDIF

Msg = 'Enter a telephone # to dial (ENTER = local mode) ?'
DO ShowOn24 WITH Msg
@0,0
* Init variables
ChkCmd = ''

* Function keys used to invoke local commands
F1 = 28
F2 = -1 && Clear Screen
F3 = -2 && Send file
F4 = -3 && Receive file
F5 = -4

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

* Now we'll dial a phone#
* Request # to dial 1st
PhoneNo = SPACE(20)
@16,10 SAY "Phone # to Dial (ENTER = direct/local) ?" GET ComPhon
READ

*Save settings
SAVE TO TERMINAL ALL LIKE Com????


IF LEN(TRIM(ComPhon)) > 0

* Issue Hayes modem setup commands
StUp1 = "OUTPUT #" + ComPort + ",ATQ0V1&C1&D2&W0" + CHR(13)
CALL COMETMP WITH StUp1
INKEY(1)
StUp2 = "OUTPUT #" + ComPort + ",ATZ" + CHR(13)
CALL COMETMP WITH StUp2
INKEY(1)

* The ATTD is output to instruct HAYES compatible modems to dial a #
Dial = "OUTPUT #" + ComPort + ",ATTD" + TRIM(ComPhon) + CHR(13) && Build OUTPUT command
CALL COMETMP WITH Dial && Have modem dial #

* Now, wait till we sense Data Carrier Detect(DCD) from our COM port.
Msg = "CHECKING FOR MODEM'S DATA CARRIER DETECT (DCD) ..."
DO ShowOn24 WITH Msg
Elapsed = 0 && Simple timer for our DO .. WHILE loop
LastTime = TIME() && Also used for timing purposes
MdmStat = "MSTAT #" + ComPort + "," + SPACE(25) && Build MSTAT command
DO WHILE Elapsed <= 45 .AND. (.NOT. "+DCD" $ MdmStat)
CALL COMETMP WITH MdmStat && Get COM port's modem status

IF LastTime <> TIME() && Test if we need to updated timer count
Elapsed = Elapsed+1 && Another second has gone by ..
LastTime = TIME()
@ 24, 66 SAY STR(45-Elapsed,2,0) && Display #secs till abort
ENDIF

IF INKEY() = 27
EXIT
ENDIF

ENDDO

* Check if we timed out
IF Elapsed > 45
??CHR(7)
DO ShowOn24 WITH "Sorry, can't establish phone connection. Aborting ..."
QUIT
ENDIF

ENDIF && If phone # was entered



* Now that we have a call established we have 2 things to do:
* 1) Check COMETMP's receive buffer and display any incoming characters
* 2) Detect any keystrokes and determine if local command or data to output

* #2 is simple, use an ONKEY approach


CLEAR

* Display status message on line 24
Msg = "F2 - Clear | F3 - Send | F4 - Recv | TERM"
LastMsg = Msg
DO ShowOn24 WITH Msg

OFLOW = ' '


***************************************************************************
* This is main loop for testing for and displaying any incoming data
* and checking for keypress

DO WHILE .T.
OurKey = INKEY() && Look for a key press
IF OurKey <> 0
DO GotAKey WITH OurKey
ENDIF

NoColsLft = 79 - COL()
Inp = "INPUT #" + ComPort + ",?????" + SPACE(NoColsLft) + CHR(10) && Build INPUT command
CALL COMETMP WITH Inp && Read COMET's COM port data buffer

AmtRetd = VAL(SUBSTR(Inp,10,5)) && Determine how many chars were returned, if any
COMactive = IIF(AmtRetd > 0, .T., .F.)

IF AmtRetd > 0
ComData = SUBSTR(Inp, 15, AmtRetd) && Get just the COM data from 
?? ComData
IF ROW() > 23
SCROLL(0,0,23,79,1)
@23, 0
ENDIF
ENDIF

ENDDO

***************************************************************************

***************************** GotAKey *************************************
* Anytime a key gets pressed, we jump here
*
PROCEDURE GotAKey
PARAMETERS Key


DO CASE && Decide whether key is data to output or local command
CASE Key > 0 .AND. Key <> 27 && data to output ?
IF .NOT. 'ACTIVE' $ ChkCmd .OR. TranHow = 'A' && Output if: no xfers active OR ASEND/ARECV active
Output = "OUTPUT #" + ComPort + "," + CHR(Key) && Build OUTPUT command
CALL COMETMP WITH Output && Output char to COM port
ELSE
CLEAR
?? CHR(7)
@ 4,0 TO 12,79 DOUBLE
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
QUIT && If no active file transfer, then quit
ENDIF
OTHERWISE && If INKEY() < 0, then a function key was hit
DO Local
ENDCASE


RETURN


****************************** Local ***************************************
* Support for function keys (ie. local commands like send and receive)
PROCEDURE Local

DO CASE
CASE Key = F2 && Clear screen ?
CLEAR
DO ShowOn24 WITH Msg
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
DO ShowOn24 WITH Msg
INKEY(3)
Msg = LastMsg
DO ShowOn24 WITH Msg
RETURN
ENDIF

ExitFlg = .F.
SAVE SCREEN
SET COLOR TO N/W
@ 6,5 CLEAR TO 12,75
@ 6,5 TO 12,75
SET COLOR TO N/W, W/N


* Prompt for transfer protocol desired (Ascii, Xmodem, Xmodem-1K or Ymodem)
* We don't use a VALID clause since DBASE doesn't support
TranHow = ' '
DO ShowOn24 WITH "CHOOSE FILE PROTOCOL: A=Ascii, X=Xmodem, X1=Xmodem(1K) or Y=Ymodem"
DO WHILE .NOT. (ExitFlg .OR. ALLTRIM(TranHow) $ 'AX1Y')
@ 8, 6 SAY 'Protocol(A,X,X1 or Y) ?' GET TranHow PICTURE '@! A9'
READ && Get protocol
ExitFlg = IIF(READKEY()=12 .OR. READKEY()=268, .T., ExitFlg)
?? IIF(.NOT. ALLTRIM(TranHow) $ 'AX1Y', CHR(7), '') && Beep if invalid
ENDDO

TranHow = ALLTRIM(TranHow)

* Prompt for filename except for YRECV since filename gets transmitted w/data
TranFil = SPACE(40)
IF .NOT. ExitFlg .AND. (TranHow <> 'Y' .OR. Action = 'SEND')
DO ShowOn24 WITH "ENTER FILENAME TO " + IIF(Action = 'RECV', 'RECEIVE', 'SEND')
@ 8, 35 SAY 'Filename ?' GET TranFil PICTURE '@S30'
READ
ExitFlg = IIF(READKEY()=12 .OR. READKEY()=268, .T., ExitFlg)
ENDIF

* Prompt for timeout in seconds if ARECV, default is 60 secs
TimeOut = 60
IF .NOT. ExitFlg .AND. TranHow = 'A' .AND. Action = 'RECV'
DO ShowOn24 WITH "ENTER RECEIVER IDLE TIME IN SECONDS BEFORE AUTO-CLOSING OF FILE"
@ 10, 26 SAY 'ARECV timeout in seconds ?' GET TimeOut PICTURE "999"
READ
ExitFlg = IIF(READKEY()=12 .OR. READKEY()=268, .T., ExitFlg)
ENDIF

SET COLOR TO W/N, N/W
RESTORE SCREEN


IF ExitFlg && Look for ESC key
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
ChkCmd = 'FCHK #' + TRIM(ComPort) + ',' + SPACE(80)
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
DO ShowOn24 WITH Msg
INKEY(3)
Msg = LastMsg
DO ShowOn24 WITH Msg
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

CurR = ROW() && Save cursor loc
CurC = COL()

ChkCmd = 'FCHK #' + TRIM(ComPort) + ',' + SPACE(80)
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


OFLOW = IIF(OFLOW = '*' .OR. 'DATA LOSS' $ ChkCmd, '*', ' ')

* Display extracted status
*Msg = LEFT(Msg,37) + Event + ' | ' + FCHKstat + ' | ' + FCHKsize + ' | ' + FCHKfile
Msg = LEFT(Msg,37) + Event + ' | ' + FCHKstat + ' | ' + FCHKsize + ' |' + OFLOW + FCHKfile
DO ShowOn24 WITH Msg

IF .NOT. 'ACTIVE' $ ChkCmd && COMPLETEd or FAILED ?
Thresh = Thresh + 1
IF Thresh > 1 && Don't want to redisplay old stat msg till 1 cycle
Ontime = 'ONTIME'
CALL COMETMP WITH Ontime && If so, turn off timer event trapping
Msg = LastMsg
DO ShowOn24 WITH Msg
ELSE

?? CHR(7) && Call attention to COMPLETE or FAILED status
IF TranHow # 'A'
CALL COMETMP WITH Open && Restore original COM port OPEN params
ENDIF

ENDIF
ENDIF

@ CurR, CurC SAY ''

RETURN

* Displays a message centered on last line in reverse video
PROCEDURE ShowOn24
PARAMETERS MsgToOut
PRIVATE RRow, RCol

RRow = ROW()
RCol = COL()

MsgLn = LEN(MsgToOut)
NoToPad = INT((80-MsgLn)/2)
Spcs = SPACE(NoToPad)
SET COLOR TO N/W
@ 24,0
@ 24,0 SAY Spcs + MsgToOut
SET COLOR TO W/N

@ RRow, RCol SAY ''

RETURN