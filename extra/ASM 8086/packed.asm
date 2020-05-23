; bcd.asm
comment ^
        This is a small demonstration program to show how one might
        handle the addition of IBM 370-style packed decimal numbers
        (which are similar to BCD).  This particular program imposes an
        arbitrary simplifying limitation by only allowing the addition
        of numbers which are both the same sign, but it would be easy to
        extend by either converting all negative operands to ten's
        complement and adding or by implementing a subtraction routine
        (which would use SBB, DAS instead of ADC, DAA).

^
        .MODEL small             ; DOS - small model
        .STACK 200h              ; allocate a bit of stack

        .DATA
OPLEN   equ 4                   ; the size of operands & result (bytes)
first   db  00h, 01h, 23h, 4Ch  ; 1234 in packed decimal
second  db  00h, 00h, 00h, 9Ch  ;    9 in packed decimal
result  db  OPLEN dup (?)       ; allocate space for result

        .CODE
        .STARTUP
        mov     si,offset first + OPLEN - 1     ; point to first op
        mov     bx,offset second + OPLEN - 1    ; second op
        mov     di,ds                           ;
        mov     es,di                           ; load es
        mov     di,offset result + OPLEN - 1    ; point to result area
        mov     cx,OPLEN                        ; how big are they?
        call    AddPackedDecimal                ; add 'em up!
        .EXIT 0
;****************************************************************************
;                                                          AddPackedDecimal
;
; add two 370-style packed decimal numbers with identical signs
;
; Entry:
;       DS:SI ==> end of first operand
;       DS:BX ==> end of second operand
;       ES:DI ==> end of pre-allocated result space
;       CX = size of operands, result (in bytes)
;
; Exit:
;       if CY set, error occurred
;       otherwise, result of addition is in result space
;
; Trashed:
;       none
;
;****************************************************************************
AddPackedDecimal proc
        push    ax                      ; save used regs
        push    bx
        push    cx
        push    dx
        push    di
        push    si
        mov     al,[si]                 ;
        mov     ah,[bx]                 ;
        mov     dx,ax                   ;
        and     dx,00f0fh               ; use only low nybbles in DX
        and     ax,0f0f0h               ; save high nybbles in AX
        cmp     dl,dh                   ; are they identical?
        stc                             ; (assume they're not)
        jnz     Done                    ; if not, it's an error
        std                             ; set dir flag (decrement ptrs)
        clc                             ; clear carry flag
        push    di                      ; save original DI for later
AddEmUp:                                ;
        adc     al,ah                   ; add (with carry)
        daa                             ; decimal adjust packed BCD
        stosb                           ; save result
        lahf                            ; save carry flag
        dec     si                      ; adjust pointer for first op
        dec     bx                      ; adjust pointer for second op
        sahf                            ; restore flag
        mov     al,[si]                 ; fetch next digits of first op
        mov     ah,[bx]                 ; fetch next digits of second op
        loop    AddEmUp                 ; do 'em all (CX is counter)
        pop     di                      ; restore original DI
        jc      Done                    ; if carry, it's an overflow
        or      [di],dl                 ; place sign nybble (and clear CY)
Done:                                   ;
        pop     si                      ; restore used registers
        pop     di
        pop     dx
        pop     cx
        pop     bx
        pop     ax
        ret                             ;
AddPackedDecimal endp
        END