!>@file   byte_swap_f.f90
!!@brief  module byte_swap_f
!!
!!@authorH.Matsui and H.Okuda
!!@date Programmed  by  H. Matsui in  Aug., 2016 
!
!>@brief swap byte endian
!!
!!@verbatim
!!      subroutine byte_swap_f(l8_byte, array)
!!         l8_byte :: byte length of array (defined by 8-byte integer)
!!         array ::   array to be transfered (call by using pointer!)
!!      subroutine binary_read_and_swap_f                               &
!!     &         (iflag_endian, id_file, l8_byte, array)
!!      subroutine binary_write_f(id_file, l8_byte, array)
!!@endverbatim
!
      subroutine byte_swap_f(l8_byte, array)
!
      use m_precision
!
      integer(kind = kint_gl), intent(in) :: l8_byte
      character(len=1), intent(inout) :: array(l8_byte)
!
      integer(kind = kint_gl) :: i8
      character(len=1) :: tmp1, tmp2
!
!
!$omp parallel do private(i8,tmp1,tmp2)
      do i8 = 4, l8_byte, 4
        tmp1 = array(4*i8-3)
        tmp2 = array(4*i8-2)
        array(i-3) = array(i  )
        array(i-2) = array(i-1)
        array(i-1) = tmp2
        array(i  ) = tmp1
      end do
!$omp end parallel do
!
      end subroutine byte_swap_f
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine integer_from_charabuffer(num, buffer, int_dat)
!
      use m_precision
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
        ctmp(1:kint) = buffer(kint*i-kint+1:kint*k)
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
        ctmp(1:kint_gl) = buffer(kint_gl*i-kint_gl+1:kint_gl*k)
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
        ctmp(1:kreal) = buffer(kreal*i-kreal+1:kreal*k)
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
        buffer(kint*i-kint+1:kint*k) = ctmp(1:kint)
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
        buffer(kint_gl*i-kint_gl+1:kint_gl*k) = ctmp(1:kint_gl)
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
        buffer(kreal*i-kreal+1:kreal*k) = ctmp(1:kreal)
      end do
!!$omp end parallel do
!
      end subroutine real_to_charabuffer
!
! -----------------------------------------------------------------------
!
