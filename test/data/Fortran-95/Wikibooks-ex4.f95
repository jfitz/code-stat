module GlobalModule

!  Reference to a pair of procedures included in a previously compiled
!  module named PortabilityLibrary
   use PortabilityLibrary, only: GetLastError, &  ! Generic procedure
                                 Date             ! Specific procedure
!  Constants
   integer, parameter :: dp_k = kind (1.0d0)      ! Double precision kind
   real, parameter :: zero = (0.)
   real(dp_k), parameter :: pi = 3.141592653589793_dp_k

!  Variables
   integer :: n, m, retint
   logical :: status, retlog
   character(50) :: AppName

!  Arrays
   real, allocatable, dimension(:,:,:) :: a, b, c, d
   complex(dp_k), allocatable, dimension(:) :: z

!  Derived type definitions
   type ijk
      integer :: i
      integer :: j
      integer :: k
   end type ijk

   type matrix
     integer m, n
     real, allocatable :: a(:,:)  ! Fortran 2003 feature. For Fortran 95, use the pointer attribute instead
   end type matrix

!  All the variables and procedures from this module can be accessed
!  by other program units, except for AppName
   public
   private :: AppName

!  Generic procedure swap
   interface swap
      module procedure swap_integer, swap_real
   end interface swap

   interface GetLastError  ! This adds a new, additional procedure to the
                           ! generic procedure GetLastError
      module procedure GetLastError_GlobalModule
   end interface GetLastError

!  Operator overloading
   interface operator(+)
      module procedure add_ijk
   end interface

!  Prototype for external procedure
   interface
      function gauss_sparse(num_iter, tol, b, A, x, actual_iter) result(tol_max)
         real ::  tol_max
         integer, intent(in) :: num_iter
         real, intent(in) :: tol
         real, intent(in), dimension(:) :: b, A(:,:)
         real, intent(inout) :: x(:)
         integer, optional, intent(out) :: actual_iter
      end function gauss_sparse
   end interface

!  Procedures included in the module
   contains

!  Internal function
   function add_ijk(ijk_1, ijk_2)
     type(ijk) add_ijk, ijk_1, ijk_2
     intent(in) :: ijk_1, ijk_2
     add_ijk = ijk(ijk_1%i + ijk_2%i, ijk_1%j + ijk_2%j, ijk_1%k + ijk_2%k)
   end function add_ijk

!  Include external files
   include 'swap_integer.f90' ! Comments SHOULDN'T be added on include lines
   include 'swap_real.f90'
end module GlobalModule
