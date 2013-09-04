!>@file   compare_indices.f90
!!@brief  module compare_indices
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui on Apr., 2006
!
!>@brief compare integers if these are same
!!
!!@verbatim
!!      integer function check_4_on_3(i1, i2, i3, j1, j2, j3, j4)
!!      integer function check_3_on_2(i1, i2, j1, j2, j3)
!!      integer function check_2_on_1(i1, j1, j2)
!!@endverbatim
!!
!!@param i1   1st compared integer
!!@param i2   2nd compared integer
!!@param i3   3rd compared integer
!!@param j1   1st reference integer
!!@param j2   2nd reference integer
!!@param j3   3rd reference integer
!!@param j4   4th reference integer
!
!
      module compare_indices
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
      integer function check_4_on_3(i1, i2, i3, j1, j2, j3, j4)
!
      integer (kind = kint), intent(in) :: i1, i2, i3, j1, j2, j3, j4
!
!
      if ( i1 .eq. j1 ) then
       check_4_on_3 = check_3_on_2(i2, i3, j2, j3, j4)
      else if ( i1 .eq. j2 ) then
       check_4_on_3 = check_3_on_2(i2, i3, j3, j4, j1)
      else if ( i1 .eq. j3 ) then
       check_4_on_3 = check_3_on_2(i2, i3, j4, j1, j2)
      else if ( i1 .eq. j4 ) then
       check_4_on_3 = check_3_on_2(i2, i3, j1, j2, j3)
      else
       check_4_on_3 = 0
      end if
!
      end function check_4_on_3
!
!------------------------------------------------------------------
!
      integer function check_3_on_2(i1, i2, j1, j2, j3)
!
      integer (kind = kint), intent(in) :: i1, i2, j1, j2, j3
!
      if ( i1 .eq. j1 ) then
       check_3_on_2 =  check_2_on_1(i2, j2, j3)
      else if ( i1 .eq. j2 ) then
       check_3_on_2 =  check_2_on_1(i2, j3, j1)
      else if ( i1 .eq. j3 ) then
       check_3_on_2 =  check_2_on_1(i2, j1, j2)
      else
       check_3_on_2 = 0
      end if
!
      end function check_3_on_2
!
!------------------------------------------------------------------
!
      integer function check_2_on_1(i1, j1, j2)
!
      integer (kind = kint), intent(in) :: i1, j1, j2
!
!
      if ( i1 .eq. j1 ) then
       check_2_on_1 = 1
      else if ( i1 .eq. j2 ) then
       check_2_on_1 = 1
      else
       check_2_on_1 = 0
      end if
!
      end function check_2_on_1
!
!------------------------------------------------------------------
!
      end module compare_indices
