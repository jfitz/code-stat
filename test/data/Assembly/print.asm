INIT	MOV #600, R0	    ; set up source address
        MOV #prtbuf, R1	    ; set up destination address
        MOV #76, R2	        ; set up loop count
START	MOVB (R0)+, (R1)+	; move one character
                            ; and increment
                            ; both source and
                            ; destination addresses
        DEC R2          	; decrement count by one
        BNE START       	; loop back if
        HALT	            ; decremented counter is not
                            ; equal to zero
                            