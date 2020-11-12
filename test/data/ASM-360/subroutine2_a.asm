        subr
           stm  r14,r12,12(r13)        ! store multiple registers into caller's
                                       !    save area (r14,r15,r0,...,r12)
           balr r12,0                  ! set address in r12 for use as base reg
           using *,r12                 ! pseudo-op to tell assembler to use r12
           la   r11,savearea           ! load address of my save area
           st   r13,savearea+4         ! store address of caller's save area
           st   r11,8(r13)             ! store address of my save area in
                                       !    caller's save area

         *  ... body of subroutine ...

         *  ... r0 is used for return code, if present ...

           l    r13,savearea+4         ! load address of caller's save area
           lm   r14,r12,12(r13)        ! load multiple registers from caller's
                                       !    save area (r14,r15,r0,...,r12)
           br   r14                    ! return to caller

        savearea
           ds   18f    ! 18-word save area
                       !    byte  0: reserved
                       !    byte  4: address of caller's save area
                       !    byte  8: address of called subroutine's save area
                       !    byte 12: contents of r14
                       !    byte 16: contents of r15
                       !    byte 20: contents of r0
                       !    byte 24: contents of r1
                       !    ...
                       !    byte 68: contents of r12
