program test_swap_real
    implicit none

!   explicit interface to the swap_real subroutine
    interface
        subroutine swap_real(a1, a2)
            real, intent(inout) :: a1(:), a2(:)
        end subroutine swap_real
    end interface

!   Declare variables
    integer :: i
    real :: a(10), b(10)

!   Initialize a, b
    a = [(real(i), i = 1, 20, 2)]
    b = a + 1

!   Output before swap
    print '(/"before swap:")'
    print '("a = [", 10f6.1, "]")', a
    print '("b = [", 10f6.1, "]")', b

!   Call the swap_real subroutine
    call swap_real(a, b)

!   Output after swap
    print '(// "after swap:")'
    print '("a = [", 10f6.1, "]")', a
    print '("b = [", 10f6.1, "]")', b

end program test_swap_real
