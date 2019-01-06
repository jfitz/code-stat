function gauss_sparse(num_iter, tol, b, A, x, actual_iter) result(tol_max)

!  This function solves a system of equations (Ax = b) by using the Gauss-Seidel Method

   implicit none

   real ::  tol_max

!  Input: its value cannot be modified from within the function
   integer, intent(in) :: num_iter
   real, intent(in) :: tol
   real, intent(in), dimension(:) :: b, A(:,:)

!  Input/Output: its input value is used within the function, and can be modified
   real, intent(inout) :: x(:)

!  Output: its value is modified from within the function, only if the argument is required
   integer, optional, intent(out) :: actual_iter

!  Locals
   integer :: i, n, iter
   real :: xk

!  Initialize values
   n = size(b)  ! Size of array, obtained using size intrinsic function
   tol_max = 2. * tol
   iter = 0

!  Compute solution until convergence
   convergence_loop: do while (tol_max >= tol .and. iter < num_iter); iter = iter + 1

      tol_max = -1.  ! Reset the tolerance value

!     Compute solution for the k-th iteration
      iteration_loop: do i = 1, n

!        Compute the current x-value
         xk = (b(i) - dot_product(A(i,:i-1),x(:i-1)) - dot_product(A(i,i+1:n),x(i+1:n))) / A(i, i)

!        Compute the error of the solution
!        dot_product(a,v)=a'b
         tol_max = max((abs(x(i) - xk)/(1. + abs(xk))) ** 2, abs(A(i, i) * (x(i) - xk)), tol_max)
         x(i) = xk
      enddo iteration_loop
   enddo convergence_loop

   if (present(actual_iter)) actual_iter = iter

end function gauss_sparse