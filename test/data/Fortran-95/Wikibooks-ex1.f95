subroutine swap_real(a1, a2)

   implicit none

!  Input/Output
   real, intent(inout) :: a1(:), a2(:)

!  Locals
   integer :: N

!  Swap, using the internal subroutine
   N = min(size(a1), size(a2))
   call swap_e(a1(:N), a2(:N))

 contains
   elemental subroutine swap_e(a1, a2)
      real, intent(inout) :: a1, a2
      real :: a
      a = a1
      a1 = a2
      a2 = a
   end subroutine swap_e
end subroutine swap_real
