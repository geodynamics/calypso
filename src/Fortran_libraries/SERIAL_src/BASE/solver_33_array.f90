!>@file   solver_33_array.f90
!!@brief  module solver_33_array
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui on July, 2006
!
!>@brief Solve linear equation for vector
!!
!!@verbatim
!!     definition of matrix
!!            / a(1,1)  a(1,2)  a(1,3)  \
!!       A =  | a(2,1)  a(2,2)  a(2,3)  |
!!            \ a(3,1)  a(3,2)  a(3,3)  /
!!
!!      subroutine solve_33_array(x, b, a)
!!
!!     x(3):   Solution
!!     b(3):   Right hand vector
!!     a(3,3): coeeficients of matrix
!!
!!    Equation:
!!      b(1) = a(1,1)*x(1) + a(1,2)*x(2) + a(1,3)*x(3)
!!      b(2) = a(2,1)*x(1) + a(2,2)*x(2) + a(2,3)*x(3)
!!      b(3) = a(3,1)*x(1) + a(3,2)*x(2) + a(3,3)*x(3)
!!
!!      subroutine solve_22_array(x, b, a)
!!    Equation:
!!      b(1) = a(1,1)*x(1) + a(1,2)*x(2)
!!      b(2) = a(2,1)*x(1) + a(2,2)*x(2)
!!@endverbatim
!
      module solver_33_array
!
      use m_precision
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine solve_33_array(x, b, a)
!
      real(kind = kreal), intent(in) :: b(3)
      real(kind = kreal), intent(in) :: a(3,3)
!
      real(kind = kreal), intent(inout) :: x(3) 
!
      real(kind = kreal) :: aj
!
!
       aj =  a(1,1)*a(2,2)*a(3,3) - a(3,1)*a(2,2)*a(1,3)                &
     &     + a(2,1)*a(3,2)*a(1,3) - a(2,1)*a(1,2)*a(3,3)                &
     &     + a(3,1)*a(1,2)*a(2,3) - a(1,1)*a(3,2)*a(2,3)
!
       if ( aj .eq. 0.0d0 ) then 
        aj = 1.0d99
       else
        aj = 1.0d0 / aj
       end if
!
       x(1) =  b(1)*a(2,2)*a(3,3) - b(3)*a(2,2)*a(1,3)                  &
     &       + b(2)*a(3,2)*a(1,3) - b(2)*a(1,2)*a(3,3)                  &
     &       + b(3)*a(1,2)*a(2,3) - b(1)*a(3,2)*a(2,3)
       x(2) =  a(1,1)*b(2)*a(3,3) - a(3,1)*b(2)*a(1,3)                  &
     &       + a(2,1)*b(3)*a(1,3) - a(2,1)*b(1)*a(3,3)                  &
     &       + a(3,1)*b(1)*a(2,3) - a(1,1)*b(3)*a(2,3)
       x(3) =  a(1,1)*a(2,2)*b(3) - a(3,1)*a(2,2)*b(1)                  &
     &       + a(2,1)*a(3,2)*b(1) - a(2,1)*a(1,2)*b(3)                  &
     &       + a(3,1)*a(1,2)*b(2) - a(1,1)*a(3,2)*b(2)
!
       x(1) = x(1) *aj
       x(2) = x(2) *aj
       x(3) = x(3) *aj
!
       end subroutine solve_33_array
!
! ----------------------------------------------------------------------
!
      subroutine solve_22_array(x, b, a)
!
      real(kind = kreal), intent(in) :: b(2)
      real(kind = kreal), intent(in) :: a(2,2)
!
      real(kind = kreal), intent(inout) :: x(2) 
!
      real(kind = kreal) :: aj
!
!
       aj =  a(1,1)*a(2,2) - a(2,1)*a(1,2)
!
       if ( aj .eq. 0.0d0 ) then 
        aj = 1.0d99
       else
        aj = 1.0d0 / aj
       end if
!
       x(1) =  ( a(2,2)*b(1) - a(1,2)*b(2) ) * aj
       x(2) =  (-a(2,1)*b(1) + a(1,1)*b(2) ) * aj
!
       end subroutine solve_22_array
!
! ----------------------------------------------------------------------
!
       end module solver_33_array
