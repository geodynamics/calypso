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
!!      recursive subroutine quicksort_int8_w_index(n,i8x,l,r,idx)
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
      private :: swap, swap_real, comp_coord
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
      recursive subroutine quicksort_int8_w_index(n,i8x,l,r,idx)
!
      integer (kind=kint), intent(in)  :: n,l,r
      integer (kind=kint_gl), intent(inout) ::   i8x(n)
      integer (kind=kint), intent(inout) :: idx(n)
      integer (kind=kint) :: c, ls, i
!
!
      if (l < r) then
        c=(l+r)/2
        call swap_i8(i8x(l),i8x(c))
        call swap(idx(l),idx(c))
        ls=l
        do i=l+1, r
          if (i8x(i) < i8x(l)) then
            ls=ls+1
            call swap_i8(i8x(ls),i8x(i))
            call swap (idx(ls),idx(i))
          end if
        end do
        call swap_i8(i8x(ls),i8x(l))
        call swap(idx(ls),idx(l))
        call quicksort_int8_w_index(n,i8x,l,ls-1,idx)
        call quicksort_int8_w_index(n,i8x,ls+1,r,idx)
      end if
!
      end subroutine quicksort_int8_w_index
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
      subroutine swap_i8(a,b)
!
      integer(kind = kint_gl), intent(inout) :: a, b
      integer(kind = kint_gl) :: w
!
      w = a
      a = b
      b = w
!
      end subroutine swap_i8
!
!-----------------------------------------------------------------------
!
      subroutine swap_coord(a,b)
!
      real(kind = kreal), intent(inout) :: a(3), b(3)
      real(kind = kreal) :: w
      integer(kind = kint) :: i
!
      do i = 1, 3
        w = a(i)
        a(i) = b(i)
        b(i) = w
      end do
!
      end subroutine swap_coord
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function comp_coord(a,b)
!
      real(kind = kreal), intent(in) :: a(3), b(3)
      integer(kind = kint) :: i
!
      do i = 1, 3
        if(a(i) < b(i)) then
          comp_coord = -1
          exit
        else if(a(i) > b(i)) then
          comp_coord = 1
          exit
        else if(a(i) .eq. b(i)) then
          continue
        end if
      end do
      comp_coord = 0
!
      end function comp_coord
!
!-----------------------------------------------------------------------
!
      recursive subroutine quicksort_coord_w_index(n,xx,l,r,idx)
!
      integer (kind=kint), intent(in)  :: n,l,r
      real (kind=kreal), intent(inout) ::   xx(n,3)
      integer (kind=kint), intent(inout) :: idx(n)
      integer (kind=kint) :: c, ls, i
!
!
      if (l < r) then
        c=(l+r)/2
        call swap_coord(xx(l,1),xx(c,1))
        call swap(idx(l),idx(c))
        ls=l
        do i=l+1, r
          if (comp_coord(xx(i,1),xx(l,1)) < 0) then
            ls=ls+1
            call swap_coord(xx(ls,1),xx(i,1))
            call swap (idx(ls),idx(i))
          end if
        end do
        call swap_coord(xx(ls,1),xx(l,1))
        call swap(idx(ls),idx(l))
        call quicksort_real_w_index(n,xx,l,ls-1,idx)
        call quicksort_real_w_index(n,xx,ls+1,r,idx)
      end if
!
      end subroutine quicksort_coord_w_index
!
!-----------------------------------------------------------------------
!

      end module quicksort
