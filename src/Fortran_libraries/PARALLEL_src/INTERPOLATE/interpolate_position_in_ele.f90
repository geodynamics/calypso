!>@file   interpolate_position_in_ele.f90
!!@brief  module interpolate_position_in_ele
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006
!!
!>@brief  Interpolate positions in each element, surface, and edge
!!
!!@verbatim
!!      subroutine interporate_one_position_linear(x_out, x_local, an)
!!        real (kind=kreal), intent(in) :: x_local(8,3)
!!        real (kind=kreal), intent(in) :: an(8)
!!        real (kind=kreal), intent(inout) :: x_out(3)
!!      subroutine interporate_one_position_quad(x_out, x_local, an)
!!        real (kind=kreal), intent(in) :: x_local(20,3)
!!        real (kind=kreal), intent(in) :: an(20)
!!        real (kind=kreal), intent(inout) :: x_out(3)
!!      subroutine interporate_one_position_lag(x_out, x_local, an)
!!        real (kind=kreal), intent(in) :: x_local(27,3)
!!        real (kind=kreal), intent(in) :: an(27)
!!        real (kind=kreal), intent(inout) :: x_out(3)
!!
!!      subroutine interporate_one_position_by_4(x_out, x_local, an)
!!        real (kind=kreal), intent(in) :: x_local(4,3)
!!        real (kind=kreal), intent(in) :: an(4)
!!        real (kind=kreal), intent(inout) :: x_out(3)
!!      subroutine interporate_one_position_by_9(x_out, x_local, an)
!!        real (kind=kreal), intent(in) :: x_local(9,3)
!!        real (kind=kreal), intent(in) :: an(9)
!!        real (kind=kreal), intent(inout) :: x_out(3)
!!
!!      subroutine interporate_one_position_by_2(x_out, x_local, an)
!!        real (kind=kreal), intent(in) :: x_local(2,3)
!!        real (kind=kreal), intent(in) :: an(2)
!!        real (kind=kreal), intent(inout) :: x_out(3)
!!      subroutine interporate_one_position_by_3(x_out, x_local, an)
!!        real (kind=kreal), intent(in) :: x_local(3,3)
!!        real (kind=kreal), intent(in) :: an(3)
!!        real (kind=kreal), intent(inout) :: x_out(3)
!!@endverbatim
      module interpolate_position_in_ele
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine interporate_one_position_linear(x_out, x_local, an)
!
      real (kind=kreal), intent(in) :: x_local(8,3)
      real (kind=kreal), intent(in) :: an(8)
!
      real (kind=kreal), intent(inout) :: x_out(3)
!
!
        x_out(1) = an( 1)  * x_local( 1,1) + an( 2)  * x_local( 2,1)    &
     &           + an( 3)  * x_local( 3,1) + an( 4)  * x_local( 4,1)    &
     &           + an( 5)  * x_local( 5,1) + an( 6)  * x_local( 6,1)    &
     &           + an( 7)  * x_local( 7,1) + an( 8)  * x_local( 8,1)
!
        x_out(2) = an( 1)  * x_local( 1,2) + an( 2)  * x_local( 2,2)    &
     &           + an( 3)  * x_local( 3,2) + an( 4)  * x_local( 4,2)    &
     &           + an( 5)  * x_local( 5,2) + an( 6)  * x_local( 6,2)    &
     &           + an( 7)  * x_local( 7,2) + an( 8)  * x_local( 8,2)
!
        x_out(3) = an( 1)  * x_local( 1,3) + an( 2)  * x_local( 2,3)    &
     &           + an( 3)  * x_local( 3,3) + an( 4)  * x_local( 4,3)    &
     &           + an( 5)  * x_local( 5,3) + an( 6)  * x_local( 6,3)    &
     &           + an( 7)  * x_local( 7,3) + an( 8)  * x_local( 8,3)
!
!
      end subroutine interporate_one_position_linear
!
! ----------------------------------------------------------------------
!
      subroutine interporate_one_position_quad(x_out, x_local, an)
!
      real (kind=kreal), intent(in) :: x_local(20,3)
      real (kind=kreal), intent(in) :: an(20)
!
      real (kind=kreal), intent(inout) :: x_out(3)
!
!
        x_out(1) = an( 1)  * x_local( 1,1) + an( 2)  * x_local( 2,1)    &
     &           + an( 3)  * x_local( 3,1) + an( 4)  * x_local( 4,1)    &
     &           + an( 5)  * x_local( 5,1) + an( 6)  * x_local( 6,1)    &
     &           + an( 7)  * x_local( 7,1) + an( 8)  * x_local( 8,1)    &
     &           + an( 9)  * x_local( 9,1) + an(10)  * x_local(10,1)    &
     &           + an(11)  * x_local(11,1) + an(12)  * x_local(12,1)    &
     &           + an(13)  * x_local(13,1) + an(14)  * x_local(14,1)    &
     &           + an(15)  * x_local(15,1) + an(16)  * x_local(16,1)    &
     &           + an(17)  * x_local(17,1) + an(18)  * x_local(18,1)    &
     &           + an(19)  * x_local(19,1) + an(20)  * x_local(20,1)
!
        x_out(2) = an( 1)  * x_local( 1,2) + an( 2)  * x_local( 2,2)    &
     &           + an( 3)  * x_local( 3,2) + an( 4)  * x_local( 4,2)    &
     &           + an( 5)  * x_local( 5,2) + an( 6)  * x_local( 6,2)    &
     &           + an( 7)  * x_local( 7,2) + an( 8)  * x_local( 8,2)    &
     &           + an( 9)  * x_local( 9,2) + an(10)  * x_local(10,2)    &
     &           + an(11)  * x_local(11,2) + an(12)  * x_local(12,2)    &
     &           + an(13)  * x_local(13,2) + an(14)  * x_local(14,2)    &
     &           + an(15)  * x_local(15,2) + an(16)  * x_local(16,2)    &
     &           + an(17)  * x_local(17,2) + an(18)  * x_local(18,2)    &
     &           + an(19)  * x_local(19,2) + an(20)  * x_local(20,2)
!
        x_out(3) = an( 1)  * x_local( 1,3) + an( 2)  * x_local( 2,3)    &
     &           + an( 3)  * x_local( 3,3) + an( 4)  * x_local( 4,3)    &
     &           + an( 5)  * x_local( 5,3) + an( 6)  * x_local( 6,3)    &
     &           + an( 7)  * x_local( 7,3) + an( 8)  * x_local( 8,3)    &
     &           + an( 9)  * x_local( 9,3) + an(10)  * x_local(10,3)    &
     &           + an(11)  * x_local(11,3) + an(12)  * x_local(12,3)    &
     &           + an(13)  * x_local(13,3) + an(14)  * x_local(14,3)    &
     &           + an(15)  * x_local(15,3) + an(16)  * x_local(16,3)    &
     &           + an(17)  * x_local(17,3) + an(18)  * x_local(18,3)    &
     &           + an(19)  * x_local(19,3) + an(20)  * x_local(20,3)
!
!
      end subroutine interporate_one_position_quad
!
! ----------------------------------------------------------------------
!
      subroutine interporate_one_position_lag(x_out, x_local, an)
!
      real (kind=kreal), intent(in) :: x_local(27,3)
      real (kind=kreal), intent(in) :: an(27)
!
      real (kind=kreal), intent(inout) :: x_out(3)
!
!
        x_out(1) = an( 1)  * x_local( 1,1) + an( 2)  * x_local( 2,1)    &
     &           + an( 3)  * x_local( 3,1) + an( 4)  * x_local( 4,1)    &
     &           + an( 5)  * x_local( 5,1) + an( 6)  * x_local( 6,1)    &
     &           + an( 7)  * x_local( 7,1) + an( 8)  * x_local( 8,1)    &
     &           + an( 9)  * x_local( 9,1) + an(10)  * x_local(10,1)    &
     &           + an(11)  * x_local(11,1) + an(12)  * x_local(12,1)    &
     &           + an(13)  * x_local(13,1) + an(14)  * x_local(14,1)    &
     &           + an(15)  * x_local(15,1) + an(16)  * x_local(16,1)    &
     &           + an(17)  * x_local(17,1) + an(18)  * x_local(18,1)    &
     &           + an(19)  * x_local(19,1) + an(20)  * x_local(20,1)    &
     &           + an(21)  * x_local(21,1) + an(22)  * x_local(12,1)    &
     &           + an(23)  * x_local(23,1) + an(24)  * x_local(14,1)    &
     &           + an(25)  * x_local(25,1) + an(26)  * x_local(16,1)    &
     &           + an(27)  * x_local(27,1)
!
        x_out(2) = an( 1)  * x_local( 1,2) + an( 2)  * x_local( 2,2)    &
     &           + an( 3)  * x_local( 3,2) + an( 4)  * x_local( 4,2)    &
     &           + an( 5)  * x_local( 5,2) + an( 6)  * x_local( 6,2)    &
     &           + an( 7)  * x_local( 7,2) + an( 8)  * x_local( 8,2)    &
     &           + an( 9)  * x_local( 9,2) + an(10)  * x_local(10,2)    &
     &           + an(11)  * x_local(11,2) + an(12)  * x_local(12,2)    &
     &           + an(13)  * x_local(13,2) + an(14)  * x_local(14,2)    &
     &           + an(15)  * x_local(15,2) + an(16)  * x_local(16,2)    &
     &           + an(17)  * x_local(17,2) + an(18)  * x_local(18,2)    &
     &           + an(19)  * x_local(19,2) + an(20)  * x_local(20,2)    &
     &           + an(21)  * x_local(21,2) + an(22)  * x_local(12,2)    &
     &           + an(23)  * x_local(23,2) + an(24)  * x_local(14,2)    &
     &           + an(25)  * x_local(25,2) + an(26)  * x_local(16,2)    &
     &           + an(27)  * x_local(27,2)
!
        x_out(3) = an( 1)  * x_local( 1,3) + an( 2)  * x_local( 2,3)    &
     &           + an( 3)  * x_local( 3,3) + an( 4)  * x_local( 4,3)    &
     &           + an( 5)  * x_local( 5,3) + an( 6)  * x_local( 6,3)    &
     &           + an( 7)  * x_local( 7,3) + an( 8)  * x_local( 8,3)    &
     &           + an( 9)  * x_local( 9,3) + an(10)  * x_local(10,3)    &
     &           + an(11)  * x_local(11,3) + an(12)  * x_local(12,3)    &
     &           + an(13)  * x_local(13,3) + an(14)  * x_local(14,3)    &
     &           + an(15)  * x_local(15,3) + an(16)  * x_local(16,3)    &
     &           + an(17)  * x_local(17,3) + an(18)  * x_local(18,3)    &
     &           + an(19)  * x_local(19,3) + an(20)  * x_local(20,3)    &
     &           + an(21)  * x_local(21,3) + an(22)  * x_local(12,3)    &
     &           + an(23)  * x_local(23,3) + an(24)  * x_local(14,3)    &
     &           + an(25)  * x_local(25,3) + an(26)  * x_local(16,3)    &
     &           + an(27)  * x_local(27,3)
!
!
      end subroutine interporate_one_position_lag
!
! ----------------------------------------------------------------------
!
      subroutine interporate_one_position_by_4(x_out, x_local, an)
!
      real (kind=kreal), intent(in) :: x_local(4,3)
      real (kind=kreal), intent(in) :: an(4)
!
      real (kind=kreal), intent(inout) :: x_out(3)
!
!
        x_out(1) = an( 1)  * x_local( 1,1) + an( 2)  * x_local( 2,1)    &
     &           + an( 3)  * x_local( 3,1) + an( 4)  * x_local( 4,1)
!
        x_out(2) = an( 1)  * x_local( 1,2) + an( 2)  * x_local( 2,2)    &
     &           + an( 3)  * x_local( 3,2) + an( 4)  * x_local( 4,2)
!
        x_out(3) = an( 1)  * x_local( 1,3) + an( 2)  * x_local( 2,3)    &
     &           + an( 3)  * x_local( 3,3) + an( 4)  * x_local( 4,3)
!
!
      end subroutine interporate_one_position_by_4
!
! ----------------------------------------------------------------------
!
      subroutine interporate_one_position_by_9(x_out, x_local, an)
!
      real (kind=kreal), intent(in) :: x_local(9,3)
      real (kind=kreal), intent(in) :: an(9)
!
      real (kind=kreal), intent(inout) :: x_out(3)
!
!
        x_out(1) = an( 1)  * x_local( 1,1) + an( 2)  * x_local( 2,1)    &
     &           + an( 3)  * x_local( 3,1) + an( 4)  * x_local( 4,1)    &
     &           + an( 5)  * x_local( 5,1) + an( 6)  * x_local( 6,1)    &
     &           + an( 7)  * x_local( 7,1) + an( 8)  * x_local( 8,1)    &
     &           + an( 9)  * x_local( 9,1)
!
        x_out(2) = an( 1)  * x_local( 1,2) + an( 2)  * x_local( 2,2)    &
     &           + an( 3)  * x_local( 3,2) + an( 4)  * x_local( 4,2)    &
     &           + an( 5)  * x_local( 5,2) + an( 6)  * x_local( 6,2)    &
     &           + an( 7)  * x_local( 7,2) + an( 8)  * x_local( 8,2)    &
     &           + an( 9)  * x_local( 9,2)
!
        x_out(3) = an( 1)  * x_local( 1,3) + an( 2)  * x_local( 2,3)    &
     &           + an( 3)  * x_local( 3,3) + an( 4)  * x_local( 4,3)    &
     &           + an( 5)  * x_local( 5,3) + an( 6)  * x_local( 6,3)    &
     &           + an( 7)  * x_local( 7,3) + an( 8)  * x_local( 8,3)    &
     &           + an( 9)  * x_local( 9,3)
!
!
      end subroutine interporate_one_position_by_9
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine interporate_one_position_by_2(x_out, x_local, an)
!
      real (kind=kreal), intent(in) :: x_local(2,3)
      real (kind=kreal), intent(in) :: an(2)
!
      real (kind=kreal), intent(inout) :: x_out(3)
!
!
        x_out(1) = an( 1)  * x_local( 1,1) + an( 2)  * x_local( 2,1)
        x_out(2) = an( 1)  * x_local( 1,2) + an( 2)  * x_local( 2,2)
        x_out(3) = an( 1)  * x_local( 1,3) + an( 2)  * x_local( 2,3)
!
!
      end subroutine interporate_one_position_by_2
!
! ----------------------------------------------------------------------
!
      subroutine interporate_one_position_by_3(x_out, x_local, an)
!
      real (kind=kreal), intent(in) :: x_local(3,3)
      real (kind=kreal), intent(in) :: an(3)
!
      real (kind=kreal), intent(inout) :: x_out(3)
!
!
        x_out(1) = an( 1)  * x_local( 1,1) + an( 2)  * x_local( 2,1)    &
     &           + an( 3)  * x_local( 3,1)
!
        x_out(2) = an( 1)  * x_local( 1,2) + an( 2)  * x_local( 2,2)    &
     &           + an( 3)  * x_local( 3,2)
!
        x_out(3) = an( 1)  * x_local( 1,3) + an( 2)  * x_local( 2,3)    &
     &           + an( 3)  * x_local( 3,3)
!
!
      end subroutine interporate_one_position_by_3
!
! ----------------------------------------------------------------------
!
      end module interpolate_position_in_ele
