!>@file   t_fft_test_data.f90
!!@brief  module t_fft_test_data
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Structure and IO for FFT test program
!!
!!@verbatim
!!      subroutine init_fft_test_data(ngrid, ftst)
!!      subroutine dealloc_fft_test_data(ftst)
!!        type(fft_test_data), intent(inout) :: ftst
!!
!!      subroutine write_fft_test_data(file_name, ftst)
!!        character(len = *), intent(in) :: file_name
!!        type(fft_test_data), intent(in) :: ftst
!!      subroutine read_alloc_fft_test_data(file_name, ftst)
!!        character(len = *), intent(in) :: file_name
!!        type(fft_test_data), intent(inout) :: ftst
!!      integer(kind = kint) function compare_FFT_tests(ftst_1, ftst_2)
!!        type(fft_test_data), intent(in) :: ftst_1, ftst_2
!!@endverbatim
!
      module t_fft_test_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      type fft_test_data
        integer(kind = kint) :: nfld = 6
        integer(kind = kint) :: ngrd = 128
        integer(kind = kint), allocatable :: nstack(:)
        real(kind = kreal), allocatable :: x(:,:)
        real(kind = kreal), allocatable :: y(:,:)
        real(kind = kreal), allocatable :: z(:,:)
      end type fft_test_data
!
      private :: alloc_fft_test_data
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_fft_test_data(ngrid, ftst)
!
      integer(kind = kint), intent(in) ::  ngrid
      type(fft_test_data), intent(inout) :: ftst
!
      real(kind = kreal) :: pi
      integer(kind = kint) :: i
      integer(kind = kint), parameter :: nfld = 6
!
      pi = four*atan(one)
!
      call alloc_fft_test_data(nfld, ngrid, ftst)
!
!$omp parallel do
      do i = 1, ftst%ngrd
        ftst%x(1,i) = 10.0d0
        ftst%x(2,i) = 2.0d0 * sin(two*pi * dble(i-1) / dble(ftst%ngrd))
        ftst%x(3,i) = 3.0d0 * cos(two*pi * dble(i-1) / dble(ftst%ngrd))
        ftst%x(4,i)                                                     &
     &        = -4.0d0 * sin(3.0d0*two*pi * dble(i-1)/dble(ftst%ngrd))
        ftst%x(5,i)                                                     &
     &        = -5.0d0 * cos(4.0d0*two*pi * dble(i-1)/dble(ftst%ngrd))
        ftst%x(6,i)                                                     &
     &        =  6.0d0 * sin(10.0d0*two*pi * dble(i-1)/dble(ftst%ngrd)) &
     &         + 1.5d0 * cos( 8.0d0*two*pi * dble(i-1)/dble(ftst%ngrd))
      end do
!$omp end parallel do
!
      end subroutine init_fft_test_data
!
! ------------------------------------------------------------------
!
      subroutine dealloc_fft_test_data(ftst)
!
      type(fft_test_data), intent(inout) :: ftst
!
      deallocate(ftst%x, ftst%y, ftst%z, ftst%nstack)
!
      end subroutine dealloc_fft_test_data
!
! ------------------------------------------------------------------
!
      subroutine alloc_fft_test_data(nfld, ngrid, ftst)
!
      integer(kind = kint), intent(in) :: ngrid, nfld
      type(fft_test_data), intent(inout) :: ftst
      integer(kind = kint) :: i
!
      ftst%ngrd = ngrid
      ftst%nfld = nfld
      allocate(ftst%x(ftst%nfld,ftst%ngrd))
      allocate(ftst%y(ftst%nfld,ftst%ngrd))
      allocate(ftst%z(ftst%nfld,ftst%ngrd))
!
      np_smp = 2
      allocate(ftst%nstack(0:np_smp))
!
      ftst%nstack(0) = 0
      do i = 1, np_smp
        ftst%nstack(i) = i * ftst%nfld / np_smp
      end do
!
      end subroutine alloc_fft_test_data
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine write_fft_test_data(file_name, ftst)
!
      character(len = *), intent(in) :: file_name
      type(fft_test_data), intent(in) :: ftst
!
      integer(kind = kint) :: i, j, k
      integer(kind = kint) :: nfld, ngrid
!
!
      open(15,file=file_name)
      write(15,'(a)') 'Num_of_field, Nlength'
      write(15,'(2i5)')  ftst%nfld, ftst%ngrd
      do j = 1, ftst%nfld
          write(15,'(a,i5)')                                            &
     &         'i, k, result_back, result_fw, original', j
        do i = 1, ftst%ngrd
          k = ((i+1)/2-1) * (-1)**mod((i-ione),itwo)
          write(15,'(2i5,1p3E25.15e3)')                                 &
     &          i, k, ftst%z(j,i), ftst%y(j,i), ftst%x(j,i)
        end do
      end do
      close(15)
!
      end subroutine write_fft_test_data
!
! ------------------------------------------------------------------
!
      subroutine read_alloc_fft_test_data(file_name, ftst)
!
      character(len = *), intent(in) :: file_name
      type(fft_test_data), intent(inout) :: ftst
!
      integer(kind = kint) :: i, j, k
      integer(kind = kint) :: ngrid, nfld
      integer(kind = kint) :: itmp
      character(len=kchara) :: tmpchara
!
      np_smp = 2
!
      open(15,file=file_name)
      read(15,*) tmpchara
      read(15,*)  nfld, ngrid
      call alloc_fft_test_data(nfld, ngrid, ftst)
!
      do j = 1, ftst%nfld
          read(15,*) tmpchara
        do i = 1, ftst%ngrd
          read(15,*) itmp, itmp, ftst%z(j,i), ftst%y(j,i), ftst%x(j,i)
        end do
      end do
      close(15)
!
      end subroutine read_alloc_fft_test_data
!
! ------------------------------------------------------------------
!
      integer(kind = kint) function compare_FFT_tests(ftst_1, ftst_2)
!
      type(fft_test_data), intent(in) :: ftst_1, ftst_2
!
      integer(kind = kint) :: i, j, k
      real(kind = kreal) :: diff
!
!
      compare_FFT_tests = 0
      if(ftst_1%nfld .gt. ftst_2%nfld) then
        write(*,*) 'Inconsistent in number of field'
        compare_FFT_tests = 1
        return
      end if
      if(ftst_1%nfld .gt. ftst_2%nfld) then
        write(*,*) 'Inconsistent in number of length'
        compare_FFT_tests = 1
        return
      end if
!
      do j = 1, ftst_1%nfld
        do i = 1, ftst_2%ngrd
          diff = ftst_2%x(j,i) - ftst_1%x(j,i)
          if(abs(diff) .gt. TINY) then
            write(*,*) 'Inconsistent result_back in ',                  &
     &                j, '-th field at ', i
            compare_FFT_tests = 1
          end if
!
          diff = ftst_2%y(j,i) - ftst_1%y(j,i)
          if(abs(diff) .gt. TINY) then
            write(*,*) 'Inconsistent result_fw in ',                    &
     &                j, '-th field at ', i
            compare_FFT_tests = 1
          end if
!
          diff = ftst_2%z(j,i) - ftst_1%z(j,i)
          if(abs(diff) .gt. TINY) then
            write(*,*) 'Inconsistent input data in ',                   &
     &                j, '-th field at ', i
            compare_FFT_tests = 1
          end if
        end do
      end do
!
      end function compare_FFT_tests
!
! ------------------------------------------------------------------
!
      end module t_fft_test_data
