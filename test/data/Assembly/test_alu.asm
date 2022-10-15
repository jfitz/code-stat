..  TEST ALU OPS
      GHI 0     .. SET UP R6
      PHI 6
      LDI DOIT  .. FOR INPUT OF OPCODE
      PLO 6
      SEX 0     .. (X=0 ALREADY)
      OUT 4,00  .. ANNOUNCE US READY
      SEX 6     .. NOW X=6
      BN4 *     .. WAIT FOR IT
      INP 4     .. OK, GET IT
      OUT 4     .. AND ECHO TO DISPLAY
      B4 *      .. WAIT FOR RELEASE
      LDI #60   .. NOW GET READY FOR
      PLO 6     .. FIRST OPERAND
      SEX 0     .. SAY SO
      OUT 4,01
      BN4 *
      SEX 6     .. TAKE IT IN AND ECHO
      INP 4     .. (TO 0060)
      OUT 4     .. (ALSO INCREMENT R6)
      B4 *
      SEX 0     .. DITTO SECOND OPERAND
      OUT 4,02
      SEX 6
LOOP: BN4 *     .. WAIT FOR IT
      INP 4     .. GET IT (NOTE: X=6)
      OUT 4     .. ECHO IT
      B4 *      .. WAIT FOR RELEASE
      DEC 6     .. BACK UP R6 TO 0060
      DEC 6
      LDA 6     .. GET 1ST OPERAND TO D
DOIT: NOP       .. DO OPERATION
      NOP       .. (SPARE)
      DEC 6     .. BACK TO 0060
      STR 6     .. OUTPUT RESULT
      OUT 4     .. (X=6 STILL)
      REQ       .. TURN OFF Q
0     LBNZ LOOP .. THEN IF ZERO,
      SEQ       .. TURN IT ON AGAIN
      BR LOOP   .. REPEAT IN ANY CASE
