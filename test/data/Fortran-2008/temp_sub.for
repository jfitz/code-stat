module convert_mod
   implicit none
contains
   subroutine cels_from_fahr(degrees_fahr,degrees_cels)
   real, intent(in)  :: degrees_fahr
   real, intent(out) :: degrees_cels
   degrees_cels = (degrees_fahr-32)/1.8
   end subroutine cels_from_fahr
end module convert_mod

program xtemperature
   use convert_mod, only: cels_from_fahr
   real    :: deg_f,deg_c
   integer :: i
   write (*,"(2a10)") "degrees_F","degrees_C"
   do i=12,100,20
      deg_f = real(i)
      call cels_from_fahr(deg_f,deg_c)
      write (*,"(2f10.1)") deg_f,deg_c
   end do
end program xtemperature
