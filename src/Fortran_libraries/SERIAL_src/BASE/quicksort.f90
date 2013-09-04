!>@file   quicksort.f90
!!@brief  module quicksort
!!
!!@author H. Okuda and H. Matsui
!!@date  Programmed by H. Okuda in 2000
!!@n     Modified by H. Matsui in Mar. 2005
!!@n     Modified by H. Matsui in July, 2006
!
!>@brief  quick sort subroutines
!!
!!@verbatim
!!      recursive subroutine quicksort_int(n,ix,l,r)
!!      recursive subroutine quicksort_w_index(n,ix,l,r,idx)
!!      recursive subroutine quicksort_real_w_index(n,x,l,r,idx)
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
!
      implicit none
!
      private :: swap, swap_real
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      recursive subroutine quicksort_int(n,ix,l,r)
!
      integer (kind=kint), intent(in)  :: n,l,r
      integer (kind=kint), intent(inout) :: ix(n)
      integer (kind=kint) :: c, ls, i
!
!
      if (l < r) then
        c=(l+r)/2
        call swap(ix(l),ix(c))
        ls=l
        do i=l+1, r
          if (ix(i) < ix(l)) then
            ls=ls+1
            call swap (ix(ls),ix(i))
          end if
        end do
        call swap(ix(ls),ix(l))
        call quicksort_int(n,ix,l,ls-1)
        call quicksort_int(n,ix,ls+1,r)
      end if
!
      end subroutine quicksort_int
!
!-----------------------------------------------------------------------
!
      recursive subroutine quicksort_w_index(n,ix,l,r,idx)
!
      integer (kind=kint), intent(in)  :: n,l,r
      integer (kind=kint), intent(inout) :: ix(n)
      integer (kind=kint), intent(inout) :: idx(n)
      integer (kind=kint) :: c, ls, i
!
!
      if (l < r) then
        c=(l+r)/2
        call swap(ix(l),ix(c))
        call swap(idx(l),idx(c))
        ls=l
        do i=l+1, r
          if (ix(i) < ix(l)) then
            ls=ls+1
            call swap (ix(ls),ix(i))
            call swap (idx(ls),idx(i))
          end if
        end do
        call swap(ix(ls),ix(l))
        call swap(idx(ls),idx(l))
        call quicksort_w_index(n,ix,l,ls-1,idx)
        call quicksort_w_index(n,ix,ls+1,r,idx)
      end if
!
      end subroutine quicksort_w_index
!
! ----------------------------------------------------------------------
!
      recursive subroutine quicksort_real_w_index(n,x,l,r,idx)
!
      integer (kind=kint), intent(in)  :: n,l,r
      real (kind=kreal), intent(inout) ::   x(n)
      integer (kind=kint), intent(inout) :: idx(n)
      integer (kind=kint) :: c, ls, i
!
!
      if (l < r) then
        c=(l+r)/2
        call swap_real(x(l),x(c))
        call swap(idx(l),idx(c))
        ls=l
        do i=l+1, r
          if (x(i) < x(l)) then
            ls=ls+1
            call swap_real (x(ls),x(i))
            call swap (idx(ls),idx(i))
          end if
        end do
        call swap_real(x(ls),x(l))
        call swap(idx(ls),idx(l))
        call quicksort_real_w_index(n,x,l,ls-1,idx)
        call quicksort_real_w_index(n,x,ls+1,r,idx)
      end if
!
      end subroutine quicksort_real_w_index
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine swap(a,b)
!
      integer(kind = kint), intent(inout) :: a,b
      integer(kind = kint) :: w
!
      w = a
      a = b
      b = w
!
      end subroutine swap
!
!-----------------------------------------------------------------------
!
      subroutine swap_real(a,b)
!
      real(kind = kreal), intent(inout) :: a, b
      real(kind = kreal) :: w
!
      w = a
      a = b
      b = w
!
      end subroutine swap_real
!
!-----------------------------------------------------------------------
!
      end module quicksort
