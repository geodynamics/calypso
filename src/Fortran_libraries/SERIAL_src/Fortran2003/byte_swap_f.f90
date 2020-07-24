!>@file   byte_swap_f.f90
!!@brief   byte_swap_f
!!
!!@authorH.Matsui and H.Okuda
!!@date Programmed  by  H. Matsui in  Aug., 2016 
!
!>@brief swap byte endian
!!
!!@verbatim
!!      subroutine byte_swap_real_f(num, real_dat)
!!        integer(kind = kint_gl), intent(in) :: num
!!        real(kind = kreal), target, intent(inout) :: real_dat(num)
!!      subroutine byte_swap_int_f(num, int_dat)
!!        integer(kind = kint_gl), intent(in) :: num
!!        integer(kind = kint), target, intent(inout) :: int_dat(num)
!!      subroutine byte_swap_int8_f(num, int8_dat)
!!        integer(kind = kint_gl), intent(in) :: num
!!        integer(kind = kint_gl), target, intent(inout) :: int8_dat(num)
!!      subroutine byte_swap_int4_f(num, int4_dat)
!!        integer(kind = kint_gl), intent(in) :: num
!!        integer(kind = kint_4b), target, intent(inout) :: int4_dat(num)
!!
!!      subroutine byte_swap_64bit_f(l8_byte, array)
!!      subroutine byte_swap_32bit_f(l8_byte, array)
!!         l8_byte :: byte length of array (defined by 8-byte integer)
!!         array ::   array to be transfered (call by using pointer!)
!!
!!      subroutine integer_from_charabuffer(num, buffer, int_dat)
!!      subroutine int8_from_charabuffer(num, buffer, int8_dat)
!!      subroutine real_from_charabuffer(num, buffer, real_dat)
!!
!!      subroutine integer_to_charabuffer(num, int_dat, buffer)
!!      subroutine int8_to_charabuffer(num, int8_dat, buffer)
!!      subroutine real_to_charabuffer(num, real_dat, buffer)
!!@endverbatim
!!
      module byte_swap_f

      use m_precision
      implicit none
!
      private :: byte_swap_32bit_f, byte_swap_64bit_f
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine byte_swap_real_f(num, real_dat)
!
      use ISO_C_BINDING
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), target, intent(inout) :: real_dat(num)
!
      integer(kind = kint_gl) :: l8_byte
      character(len = 1), pointer :: cbuf(:)
!
!
      l8_byte = num * kreal
      call c_f_pointer(C_LOC(real_dat(1)), cbuf, [l8_byte])
      call byte_swap_64bit_f(l8_byte, cbuf)
!
      end subroutine byte_swap_real_f
!
! -----------------------------------------------------------------------
!
      subroutine byte_swap_int_f(num, int_dat)
!
      use ISO_C_BINDING
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), target, intent(inout) :: int_dat(num)
!
      integer(kind = kint_gl) :: l_byte
      character(len = 1), pointer :: cbuf(:)
!
!
      l_byte = num * kint
      call c_f_pointer(C_LOC(int_dat(1)), cbuf, [l_byte])
!
      if(kint .eq. kint_gl) then
        call byte_swap_64bit_f(l_byte, cbuf)
      else
        call byte_swap_32bit_f(l_byte, cbuf)
      end if
!
      end subroutine byte_swap_int_f
!
! -----------------------------------------------------------------------
!
      subroutine byte_swap_int8_f(num, int8_dat)
!
      use ISO_C_BINDING
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), target, intent(inout) :: int8_dat(num)
!
      integer(kind = kint_gl) :: l8_byte
      character(len = 1), pointer :: cbuf(:)
!
!
      l8_byte = num * kint_gl
      call c_f_pointer(C_LOC(int8_dat(1)), cbuf, [l8_byte])
      call byte_swap_64bit_f(l8_byte, cbuf)
!
      end subroutine byte_swap_int8_f
!
! -----------------------------------------------------------------------
!
      subroutine byte_swap_int4_f(num, int4_dat)
!
      use ISO_C_BINDING
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_4b), target, intent(inout) :: int4_dat(num)
!
      integer(kind = kint_gl) :: l4_byte
      character(len = 1), pointer :: cbuf(:)
!
!
      l4_byte = num * kint_4b
      call c_f_pointer(C_LOC(int4_dat(1)), cbuf, [l4_byte])
      call byte_swap_32bit_f(l4_byte, cbuf)
!
      end subroutine byte_swap_int4_f
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine byte_swap_64bit_f(l8_byte, array)
!
      integer(kind = kint_gl), intent(in) :: l8_byte
      character(len=1), intent(inout) :: array(l8_byte)
!
      integer(kind = kint_gl) :: i8
      character(len=1) :: tmp1, tmp2, tmp3, tmp4
!
!
!$omp parallel do private(i8,tmp1,tmp2,tmp3,tmp4)
      do i8 = 8, l8_byte, 8
        tmp1 = array(i8-7)
        tmp2 = array(i8-6)
        tmp3 = array(i8-5)
        tmp4 = array(i8-4)
!
        array(i8-7) = array(i8  )
        array(i8-6) = array(i8-1)
        array(i8-5) = array(i8-2)
        array(i8-4) = array(i8-3)
!
        array(i8-3) = tmp4
        array(i8-2) = tmp3
        array(i8-1) = tmp2
        array(i8  ) = tmp1
      end do
!$omp end parallel do
!
      end subroutine byte_swap_64bit_f
!
! -----------------------------------------------------------------------
!
      subroutine byte_swap_32bit_f(l8_byte, array)
!
      use m_precision
      implicit none
!
      integer(kind = kint_gl), intent(in) :: l8_byte
      character(len=1), intent(inout) :: array(l8_byte)
!
      integer(kind = kint_gl) :: i4
      character(len=1) :: tmp1, tmp2
!
!
!$omp parallel do private(i4,tmp1,tmp2)
      do i4 = 4, l8_byte, 4
        tmp1 = array(i4-3)
        tmp2 = array(i4-2)
!
        array(i4-3) = array(i4  )
        array(i4-2) = array(i4-1)
!
        array(i4-1) = tmp2
        array(i4  ) = tmp1
      end do
!$omp end parallel do
!
      end subroutine byte_swap_32bit_f
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine integer_from_charabuffer(num, buffer, int_dat)
!
      use m_precision
      implicit none
!
      integer(kind = kint_gl), intent(in) :: num
      character(len = 1), intent(in) :: buffer(kint*num)
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint_gl) :: i
      integer(kind = kint) :: itmp
      character(len = 1) :: ctmp(kint)
      equivalence(itmp,ctmp)
!
!
!!$omp parallel do private(i,itmp,ctmp)
      do i = 1, num
        ctmp(1:kint) = buffer(kint*i-kint+1:kint*i)
        int_dat(i) = itmp
      end do
!!$omp end parallel do
!
      end subroutine integer_from_charabuffer
!
! -----------------------------------------------------------------------
!
      subroutine int8_from_charabuffer(num, buffer, int8_dat)
!
      use m_precision
      implicit none
!
      integer(kind = kint_gl), intent(in) :: num
      character(len = 1), intent(in) :: buffer(kint_gl*num)
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      integer(kind = kint_gl) :: i
      integer(kind = kint_gl) :: itmp8
      character(len = 1) :: ctmp(kint_gl)
      equivalence(itmp8,ctmp)
!
!
!!$omp parallel do private(i,itmp8,ctmp)
      do i = 1, num
        ctmp(1:kint_gl) = buffer(kint_gl*i-kint_gl+1:kint_gl*i)
        int8_dat(i) = itmp8
      end do
!!$omp end parallel do
!
      end subroutine int8_from_charabuffer
!
! -----------------------------------------------------------------------
!
      subroutine real_from_charabuffer(num, buffer, real_dat)
!
      use m_precision
      implicit none
!
      integer(kind = kint_gl), intent(in) :: num
      character(len = 1), intent(in) :: buffer(kreal*num)
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = kint_gl) :: i
      real(kind = kreal) :: rtmp
      character(len = 1) :: ctmp(kreal)
      equivalence(rtmp,ctmp)
!
!
!!$omp parallel do private(i,rtmp,ctmp)
      do i = 1, num
        ctmp(1:kreal) = buffer(kreal*i-kreal+1:kreal*i)
        real_dat(i) = rtmp
      end do
!!$omp end parallel do
!
      end subroutine real_from_charabuffer
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine integer_to_charabuffer(num, int_dat, buffer)
!
      use m_precision
      implicit none
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
      character(len = 1), intent(inout) :: buffer(kint*num)
!
      integer(kind = kint_gl) :: i
      integer(kind = kint) :: itmp
      character(len = 1) :: ctmp(kint)
      equivalence(itmp,ctmp)
!
!
!!$omp parallel do private(i,itmp,ctmp)
      do i = 1, num
        itmp = int_dat(i)
        buffer(kint*i-kint+1:kint*i) = ctmp(1:kint)
      end do
!!$omp end parallel do
!
      end subroutine integer_to_charabuffer
!
! -----------------------------------------------------------------------
!
      subroutine int8_to_charabuffer(num, int8_dat, buffer)
!
      use m_precision
      implicit none
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
      character(len = 1), intent(inout) :: buffer(kint_gl*num)
!
      integer(kind = kint_gl) :: i
      integer(kind = kint_gl) :: itmp8
      character(len = 1) :: ctmp(kint_gl)
      equivalence(itmp8,ctmp)
!
!
!!$omp parallel do private(i,itmp8,ctmp)
      do i = 1, num
        itmp8 = int8_dat(i)
        buffer(kint_gl*i-kint_gl+1:kint_gl*i) = ctmp(1:kint_gl)
      end do
!!$omp end parallel do
!
      end subroutine int8_to_charabuffer
!
! -----------------------------------------------------------------------
!
      subroutine real_to_charabuffer(num, real_dat, buffer)
!
      use m_precision
      implicit none
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
      character(len = 1), intent(inout) :: buffer(kreal*num)
!
      integer(kind = kint_gl) :: i
      real(kind = kreal) :: rtmp
      character(len = 1) :: ctmp(kreal)
      equivalence(rtmp,ctmp)
!
!
!!$omp parallel do private(i,rtmp,ctmp)
      do i = 1, num
        rtmp = real_dat(i)
        buffer(kreal*i-kreal+1:kreal*i) = ctmp(1:kreal)
      end do
!!$omp end parallel do
!
      end subroutine real_to_charabuffer
!
! -----------------------------------------------------------------------
!
      end module byte_swap_f
