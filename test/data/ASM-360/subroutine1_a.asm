        *  ...
           l    r15=A(subr)            ! load address of subroutine in r15
           balr r14,r15                ! save return address in r14 and set
        *  ...                         !    pc to contents of r15