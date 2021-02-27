!>@file   quicksort.f90
!!        module quicksort
!!
!!@author H. Okuda and H. Matsui
!!@date  Programmed by H. Okuda in 2000
!!@n     Modified by H. Matsui in Mar. 2005
!!@n     Modified by H. Matsui in July, 2006
!!
!>@brief  Quick sort subroutines
!!
!!@verbatim
!!      subroutine quicksort_int(n,ix,l,r)
!!      subroutine quicksort_w_index(n,ix,l,r,idx)
!!      subroutine quicksort_int8_w_index(n,i8x,l,r,idx)
!!      subroutine quicksort_real_w_index(n,x,l,r,idx)
!!@endverbatim
!!
!!@n @param  n        size of array
!!@n @param  l start address
!!@n @param  r end address
!!@n @param  x(n)     real to be sorted
!!@n @param  ix(n)    integer to be sorted
!!@n @param  idx(n)   integer list moved with ix
!
      module quicksort
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine quicksort_int(n,ix,l,r)
!
      use isort_with_int
!
      integer (kind=kint), intent(in)  :: n,l,r
      integer (kind=kint), intent(inout) :: ix(n)
!
      integer (kind=kint) :: num
      integer (kind=kint), allocatable :: i8y(:)
!
!
      num = int(r-l+1)
      allocate(i8y(num))
      call ISORT_w_INT(ix(l), i8y(1), num, ione)
      deallocate(i8y)
!
      end subroutine quicksort_int
!
!-----------------------------------------------------------------------
!
      subroutine quicksort_w_index(n,ix,l,r,idx)
!
      use isort_with_int
!
      integer (kind=kint), intent(in)  :: n,l,r
      integer (kind=kint), intent(inout) :: ix(n)
      integer (kind=kint), intent(inout) :: idx(n)
!
      integer (kind=kint) :: num
!
!
      num = int(r-l+1)
      call ISORT_w_INT(ix(l), idx(l), num, itwo)
!
      end subroutine quicksort_w_index
!
! ----------------------------------------------------------------------
!
      subroutine quicksort_int8_w_index(n,i8x,l,r,idx)
!
      use i8sort_with_int
!
      integer (kind=kint), intent(in)  :: n,l,r
      integer (kind=kint_gl), intent(inout) ::   i8x(n)
      integer (kind=kint), intent(inout) :: idx(n)
!
      integer (kind=kint) :: num
!
!
      num = int(r-l+1)
      call I8SORT_W_INT(i8x(l), idx(l), num, itwo)
!
      end subroutine quicksort_int8_w_index
!
! ----------------------------------------------------------------------
!
      subroutine quicksort_real_w_index(n,x,l,r,idx)
!
      use dsort_with_int
!
      integer (kind=kint), intent(in)  :: n,l,r
      real (kind=kreal), intent(inout) ::   x(n)
      integer (kind=kint), intent(inout) :: idx(n)
!
      integer (kind=kint) :: num
!
      num = int(r-l+1)
      call DSORT_W_INT(x(l), idx(l), num, itwo)
!
      end subroutine quicksort_real_w_index
!
! ----------------------------------------------------------------------
!
      end module quicksort
