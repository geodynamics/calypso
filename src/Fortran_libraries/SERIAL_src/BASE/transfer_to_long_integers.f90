!>@file  transfer_to_long_integers.f90
!!       module transfer_to_long_integers
!!
!!@author H. Matsui
!!@date   Programmed in Mar., 2018
!
!> @brief Cast 4byte integer to 8byte integers
!!
!!@verbatim
!!      integer(kind = kint_gl) function cast_long(i_in)
!!      integer(kind = kint) function cast_short(i8_in)
!!      integer(kind = kint) function cast_kint(i4_in)
!!
!!      subroutine alloc_1d_i8array(n1, tmp)
!!      subroutine dealloc_1d_i8array(tmp)
!!      subroutine dup_from_short_array(n1, i4_out, tmp)
!!      subroutine dup_to_short_array(tmp, i4_out)
!!        type(tmp_i8_array), intent(inout) :: tmp
!!
!!      subroutine alloc_2d_i8array(n1, n2, tmp)
!!      subroutine dealloc_2d_i8array(tmp)
!!      subroutine dup_from_short_darray(n1, n2, i4_out, tmp)
!!      subroutine dup_to_short_darray(tmp, i4_out)
!!        type(tmp_i8_2darray), intent(inout) :: tmp
!!@endverbatim
!
      module transfer_to_long_integers
!
      use m_precision
!
      implicit none
!
      type tmp_i8_array
        integer(kind = kint_gl) :: n1
        integer(kind = kint_gl), allocatable :: id_a(:)
      end type tmp_i8_array
!
      type tmp_i8_2darray
        integer(kind = kint_gl) :: n1
        integer(kind = kint_gl) :: n2
        integer(kind = kint_gl), allocatable :: id_da(:,:)
      end type tmp_i8_2darray
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      integer(kind = kint_gl) function cast_long(i_in)
!
      integer(kind = kint), intent(in) :: i_in
!
      cast_long = i_in
!
      end function cast_long
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function cast_short(i8_in)
!
      integer(kind = kint_gl) :: i8_in
!
      cast_short = i8_in
!
      end function cast_short
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function cast_kint(i4_in)
!
      integer :: i4_in
!
      cast_kint = int(i4_in,KIND(cast_kint))
!
      end function cast_kint
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_1d_i8array(n1, tmp)
!
      integer(kind = kint_gl), intent(in) :: n1
      type(tmp_i8_array), intent(inout) :: tmp
!
!
      tmp%n1 = n1
      allocate(tmp%id_a(tmp%n1))
!
      end subroutine alloc_1d_i8array
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_1d_i8array(tmp)
!
      type(tmp_i8_array), intent(inout) :: tmp
!
!
      deallocate(tmp%id_a)
!
      end subroutine dealloc_1d_i8array
!
!-----------------------------------------------------------------------
!
      subroutine dup_from_short_array(n1, i4_out, tmp)
!
      integer(kind = kint_gl), intent(in) :: n1
      integer(kind = kint), intent(in) :: i4_out(n1)
      type(tmp_i8_array), intent(inout) :: tmp
!
      call alloc_1d_i8array(n1, tmp)
!
      if(tmp%n1 .gt. 0) then
!$omp parallel workshare
        tmp%id_a(1:tmp%n1) = i4_out(1:tmp%n1)
!$omp end parallel workshare
      end if
!
      end subroutine dup_from_short_array
!
!-----------------------------------------------------------------------
!
      subroutine dup_to_short_array(tmp, i4_out)
!
      type(tmp_i8_array), intent(inout) :: tmp
      integer(kind = kint), intent(inout) :: i4_out(tmp%n1)
!
!
      if(tmp%n1 .gt. 0) then
!$omp parallel workshare
        i4_out(1:tmp%n1) = tmp%id_a(1:tmp%n1)
!$omp end parallel workshare
      end if
!
      call dealloc_1d_i8array(tmp)
!
      end subroutine dup_to_short_array
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_2d_i8array(n1, n2, tmp)
!
      integer(kind = kint_gl), intent(in) :: n1, n2
      type(tmp_i8_2darray), intent(inout) :: tmp
!
!
      tmp%n1 = n1
      tmp%n2 = n2
      allocate(tmp%id_da(tmp%n1,tmp%n2))
!
      end subroutine alloc_2d_i8array
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_2d_i8array(tmp)
!
      type(tmp_i8_2darray), intent(inout) :: tmp
!
!
      deallocate(tmp%id_da)
!
      end subroutine dealloc_2d_i8array
!
!-----------------------------------------------------------------------
!
      subroutine dup_from_short_darray(n1, n2, i4_out, tmp)
!
      integer(kind = kint_gl), intent(in) :: n1, n2
      integer(kind = kint), intent(in) :: i4_out(n1,n2)
      type(tmp_i8_2darray), intent(inout) :: tmp
!
!
      call alloc_2d_i8array(n1, n2, tmp)
!
      if(tmp%n1*tmp%n2 .gt. 0) then
!$omp parallel workshare
        tmp%id_da(1:tmp%n1,1:tmp%n2) = i4_out(1:tmp%n1,1:tmp%n2)
!$omp end parallel workshare
      end if
!
      end subroutine dup_from_short_darray
!
!-----------------------------------------------------------------------
!
      subroutine dup_to_short_darray(tmp, i4_out)
!
      type(tmp_i8_2darray), intent(inout) :: tmp
      integer(kind = kint), intent(inout) :: i4_out(tmp%n1,tmp%n2)
!
!
      if(tmp%n1*tmp%n2 .gt. 0) then
!$omp parallel workshare
        i4_out(1:tmp%n1,1:tmp%n2) = tmp%id_da(1:tmp%n1,1:tmp%n2)
!$omp end parallel workshare
      end if
!
      call dealloc_2d_i8array(tmp)
!
      end subroutine dup_to_short_darray
!
!-----------------------------------------------------------------------
!
      end module transfer_to_long_integers
