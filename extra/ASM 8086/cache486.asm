Disables the cache on a 486 and Pentium processor

; cache disable routine
;
    public DisableCache     

code segment    ; simple but effective for demonstration purposes
;*                                                          DisableCache()  *
;*                                                                          *
;*  This routine disables cache(s) on a 486 or Pentium processor            *
;*                                                                          *
;*  NOTE: due to the protection schemes incorporated into the 486 and       *
;*          Pentium processors, it will NOT work in virtual 8086 mode.      *
;*                                                                          *
;*  written on Thursday, 2 November 1995 by Ed Beroset                      *
;*    and released to the public domain by the author                       *

    .486P

CR0_CD equ 040000000h   ; Cache Disable bit of CR0
CR0_NW equ 020000000h   ; Not Write-through bit of CR0

DisableCache proc
    pushf                       ; save the flags
    push    eax                 ; save eax
    cli                         ; disable interrupts while we do this
    mov     eax,cr0             ; read CR0
    or      eax,CR0_CD          ; set CD but not NW bit of CR0
    mov     cr0,eax             ; cache is now disabled
    wbinvd                      ; flush and invalidate cache

    ; the cache is effectively disabled at this point, but memory
    ; consistency will be maintained.  To completely disable cache,
    ; the following two lines may used as well:

    or      eax,CR0_NW          ; now set the NW bit
    mov     cr0,eax             ; turn off the cache entirely
    pop     eax                 ; restore eax
    popf                        ; restore the flags
    ret                         ; return to caller
DisableCache endp
code ends
        end
