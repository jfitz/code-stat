; sincosr.z80
; CORDIC for sin() and cos() for the Zilog Z80 and Assembler Z80DT
; reentrant version
; Author: Andre Adrian
; Version: 12Apr2011
; length is 333 bytes

; LOAD TEST VALUES
    LD      IX,1E80H        ; init second stack pointer
    LD      BC,12AFH        ; Angle = -1.57079632679 (-90°)
    EXX
    LD      BC,9B78H
    EXX
    CALL    SINCOS
                            ; expected sin = B'C'BC = 0C0000000H (-1.0)
                            ; expected cos = D'E'DE = 000000000H (0.0)
                            ; delivered sin = B'C'BC = 0BFFFFFFFH
                            ; delivered cos = D'E'DE = 000000031H
    HALT

;==================================================
; Constants
;
CN:        EQU        22
CM:        EQU        8

;==================================================
; constant storage (ROM)
;
ATR:
    DEFW     0F6A8H,03243H    ; atan(1)
    DEFW     06705H,01DACH    ; atan(1/2)
    DEFW     0BAFCH,00FADH    ; atan(1/4)
    DEFW     06EA6H,007F5H    ; atan(1/8)
    DEFW     0AB76H,003FEH    ; ...
    DEFW     0D55BH,001FFH
    DEFW     0FAAAH,000FFH
    DEFW     0FF55H,0007FH
    DEFW     0FFEAH,0003FH

;==================================================
; N BIT ARITHMETRIC SHIFT RIGHT ROUTINE 32BIT = 32BIT
; D'E'DE >>= A
; CHANGES A, AF' and FLAGS
;
SRDEDE:
    SUB     8           ; SET CARRY FLAG IF A < 8
    JR      C,SRDEBT    ; NO MORE BYTES TO SHIFT
    LD      E,D         ; SHIFT BITS 8..15 TO 7..0
    EXX
    EX      AF,AF'
    LD      A,E         ; SHIFT BITS 16..23 TO 8..15
    LD      E,D         ; SHIFT BITS 24..31 TO 16..23
    LD      D,0         ; ASSUME POSITIVE NUMBER
    BIT     7,E         ; SET ZERO FLAG IF BIT 7 == 0
    JR      Z,SRDEPL
    DEC     D           ; CHANGE + TO - SIGN
SRDEPL:
    EXX
    LD      D,A
    EX      AF,AF'
    JR      SRDEDE
SRDEBT:
    ADD     A,8         ; UNDO SUB 8, SET ZERO FLAG IF A == 0
SRDELP:
    JR      Z,SRDERT    ; NO MORE BITS TO SHIFT
    EXX
    SRA     D
    RR      E
    EXX
    RR      D
    RR      E
    DEC     A           ; SET ZERO FLAG IF A == 0
    JR      SRDELP
SRDERT:
    RET                 ; RESULT IS IN D'E'DE.

;==================================================
; SIN() COS() ROUTINE 2 * 32BIT = 32BIT
; B'C'BC, D'E'DE = f(B'C'BC)
; NEEDS REGISTERS A A' BC B'C' DE D'E' HL H'L' IX IY, CHANGES FLAGS
;
SINCOS:
    SUB    A            ; unsigned char af = 0;     // i
    LD     L,A          ; fixed hlhl = 0;           // y
    LD     H,A
    LD     DE,03B6AH    ; fixed dede = FIXED(InvK); // x
    EXX
    LD     L,A
    LD     H,A
    LD     DE,026DDH
    EXX
    LD     IY,ATR       ; fixed *iy = atr_fixed;
SNCSDO:                 ; do {
    EXX                 ; push(hlhl);       // y
    PUSH   HL
    EXX
    PUSH   HL
    EXX                 ; push(dede);       // y x
    PUSH   DE
    EXX
    PUSH   DE
    EXX                 ; push(bcbc);       // y x angle
    PUSH   BC
    EXX
    PUSH   BC
    PUSH   AF           ; push(af);         // y x angle i
    CALL   SRDEDE       ; dede >>= af;
    EXX
    BIT    7,B          ; if (bcbc >= 0) {
    EXX
    JR     NZ,SNCSL1
    ADD    HL,DE        ;     hlhl += dede;
    EXX
    ADC    HL,DE
    JR     SNCSE1
SNCSL1:                 ; } else {
    AND    A            ;     clear Carry
    SBC    HL,DE        ;     hlhl -= dede;
    EXX
    SBC    HL,DE
SNCSE1:                 ; }
    DEC    IX           ; rpush(hlhl);      // R: y+1
    LD     (IX+0),H
    DEC    IX
    LD     (IX+0),L
    EXX
    DEC    IX
    LD     (IX+0),H
    DEC    IX
    LD     (IX+0),L
    POP    AF           ; pop(af);          // y x angle (i)
    POP    BC           ; pop(bcbc);        // y x (angle)
    EXX
    POP    BC
    EXX
    POP    HL           ; pop(hlhl);        // y (x)
    EXX
    POP    HL
    EXX
    POP    DE           ; pop(dede);        // nil (y)
    EXX
    POP    DE
    PUSH   BC           ; push(bcbc);       // angle
    EXX
    PUSH   BC
    PUSH   AF           ; push(af);         // angle i
    CALL   SRDEDE       ; dede >>= af;
    EXX
    BIT    7,B          ; if (bcbc >= 0) {
    EXX
    JR     NZ,SNCSL2
    AND    A
    SBC    HL,DE        ;    hlhl -= dede;
    EXX
    SBC    HL,DE
    EXX
    JR     SNCSE2
SNCSL2:                 ; } else {
    ADD    HL,DE        ;    hlhl += dede;
    EXX
    ADC    HL,DE
    EXX
SNCSE2:                 ; }
    LD     E,L          ; dede = hlhl;
    LD     D,H
    EXX
    LD     E,L
    LD     D,H
    EXX
    LD     C,(IX+0)     ; rpop(bcbc);       // R: nil (y+1)
    INC    IX
    LD     B,(IX+0)
    INC    IX
    EXX
    LD     C,(IX+0)
    INC    IX
    LD     B,(IX+0)
    INC    IX
    EXX
    POP    AF           ; pop(af);          // angle (i)
    POP    HL           ; pop(hlhl);        // nil (angle)
    EXX
    POP    HL
    PUSH   BC           ; push(bcbc);       // y+1
    EXX
    PUSH   BC
    EXX                 ; push(dede);       // y+1 x+1
    PUSH   DE
    EXX
    PUSH   DE
    PUSH   AF           ; push(af);         // y+1 x+1 i
    SUB    CM+1         ; af -= M+1;
    JR     NC,SNCSL3    ; if (af < 0) {
    LD     E,(IY+0)     ;    dede = *iy++;
    INC    IY
    LD     D,(IY+0)
    INC    IY
    EXX
    LD     E,(IY+0)
    INC    IY
    LD     D,(IY+0)
    INC    IY
    EXX
    JR    SNCSE3
SNCSL3:                 ; } else {
    INC    A            ;    ++af;
    LD     DE,(ATR+32)  ;    dede = atr_fixed[M];
    EXX
    LD     DE,(ATR+34)
    EXX
    CALL   SRDEDE       ;    dede >>= af;
SNCSE3:                 ; }
    EXX                 ; if (hlhl >= 0) {
    BIT    7,H
    EXX
    JR     NZ,SNCSL4
    AND    A            ;    hlhl -= dede;
    SBC    HL,DE
    EXX
    SBC    HL,DE
    EXX
    JR    SNCSE4
SNCSL4:                 ; } else {
    ADD    HL,DE        ;    hlhl += dede;
    EXX
    ADC    HL,DE
    EXX
SNCSE4:                 ; }
    POP    AF           ; pop(af);      // y+1 x+1 (i)
    LD     C,L          ; bcbc = hlhl;
    LD     B,H
    EXX
    LD     C,L
    LD     B,H
    EXX
    POP    DE           ; pop(dede);    // y+1 (x+1)
    EXX
    POP    DE
    EXX
    POP    HL           ; pop(hlhl);    // nil (y+1)
    EXX
    POP    HL
    EXX
    INC    A            ; ++af;
    CP     CN           ; } while (af-CN < 0);
    JP     C,SNCSDO     ;
                        ; *cos = dede;
    LD     C,L          ; bcbc = hlhl;
    LD     B,H
    EXX
    LD     C,L
    LD     B,H
    EXX
    RET                 ; return bcbc;

    END