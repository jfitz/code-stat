subroutine swap_real(a1, a2)

   implicit none

!  Input/Output
   real, intent(inout) :: a1(:), a2(:)

!  Locals
   integer :: i
   real :: a

!  Swap
   do i = 1, min(size(a1), size(a2))
      a = a1(i)
      a1(i) = a2(i)
      a2(i) = a
   enddo

end subroutine swap_real