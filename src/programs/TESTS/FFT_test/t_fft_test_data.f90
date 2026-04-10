!>@file   t_fft_test_data.f90
!!@brief  module t_fft_test_data
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Structure and IO for FFT test program
!!
!!@verbatim
!!      subroutine init_fft_test_data(nfld, ngrid, ftst)
!!      subroutine swap_fft_test_input_to_pin(ftst)
!!      subroutine swap_fft_test_data_to_pout(ftst)
!!      subroutine dealloc_fft_test_data(ftst)
!!        integer(kind = kint), intent(in) ::  nfld, ngrid
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
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      type fft_test_data
        integer(kind = kint) :: nfld = 8
        integer(kind = kint) :: ngrd = 128
        integer(kind = kint), allocatable :: nstack(:)
        real(kind = kreal), allocatable :: org(:,:)
        real(kind = kreal), allocatable :: s_k(:,:)
        real(kind = kreal), allocatable :: f_x(:,:)
!
        real(kind = kreal) :: start = zero
        real(kind = kreal) :: elapsed(8)
      end type fft_test_data
!
      private :: alloc_pout_fft_test_data, alloc_pin_fft_test_data
      private :: init_fft_test_smp_stack
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_fft_test_data(nfld, ngrid, ftst)
!
      integer(kind = kint), intent(in) ::  nfld, ngrid
      type(fft_test_data), intent(inout) :: ftst
!
      real(kind = kreal) :: pi
      integer(kind = kint) :: i, nd
!
!
      ftst%elapsed(1:8) = 0.0d0
!
      np_smp = omp_get_max_threads()
      call alloc_pout_fft_test_data(nfld, ngrid, ftst)
!
      pi = four*atan(one)
!
      if(ftst%nfld .lt. 1) return
      call RANDOM_NUMBER(ftst%org)
!
!$omp parallel do
      do i = 1, ftst%ngrd
        ftst%org(1,i) = 10.0d0
      end do
!$omp end parallel do
!
      if(ftst%nfld .lt. 2) return
!$omp parallel do
      do i = 1, ftst%ngrd
        ftst%org(2,i) =  (-1)**i
      end do
!$omp end parallel do
!
      if(ftst%nfld .lt. 3) return
!$omp parallel do
      do i = 1, ftst%ngrd
        ftst%org(3,i)                                                   &
     &     = 2.0d0 * sin(two*pi * dble(i-1) / dble(ftst%ngrd))
      end do
!$omp end parallel do
!
      if(ftst%nfld .lt. 4) return
!$omp parallel do
      do i = 1, ftst%ngrd
        ftst%org(4,i)                                                   &
     &     = 3.0d0 * cos(1.0d0*two*pi * dble(i-1) / dble(ftst%ngrd))
      end do
!$omp end parallel do
!
      if(ftst%nfld .lt. 5) return
!$omp parallel do
      do i = 1, ftst%ngrd
        ftst%org(5,i)                                                   &
     &     = -4.0d0 * sin(3.0d0*two*pi * dble(i-1)/dble(ftst%ngrd))
      end do
!$omp end parallel do
!
      if(ftst%nfld .lt. 6) return
!$omp parallel do
      do i = 1, ftst%ngrd
        ftst%org(6,i)                                                   &
     &     = -5.0d0 * cos(4.0d0*two*pi * dble(i-1)/dble(ftst%ngrd))
      end do
!$omp end parallel do
!
      if(ftst%nfld .lt. 7) return
!$omp parallel do
      do i = 1, ftst%ngrd
        ftst%org(7,i)                                                   &
     &     =  6.0d0 * sin(10.0d0*two*pi * dble(i-1)/dble(ftst%ngrd))    &
     &      + 1.5d0 * cos( 8.0d0*two*pi * dble(i-1)/dble(ftst%ngrd))
      end do
!$omp end parallel do
!
      if(ftst%nfld .lt. 8) return
!$omp parallel do
      do i = 1, ftst%ngrd
        ftst%org(8,i)                                                   &
     &     =  8.0d0 * sin(16.0d0*two*pi * dble(i-1)/dble(ftst%ngrd))    &
     &      - 7.0d0 * cos( 5.0d0*two*pi * dble(i-1)/dble(ftst%ngrd))    &
     &      - 3.0d0 * sin( 3.0d0*two*pi * dble(i-1)/dble(ftst%ngrd))    &
     &      - 6.0d0 * cos( 2.0d0*two*pi * dble(i-1)/dble(ftst%ngrd))    &
     &      + 3.0d0 * sin( 9.0d0*two*pi * dble(i-1)/dble(ftst%ngrd))
      end do
!$omp end parallel do
!
      if(ftst%nfld .lt. 9) return
      do nd = 9, ftst%nfld
        do i = 1, ftst%ngrd
          ftst%org(nd,i) = 2.0d0 * ftst%org(nd,i) - 1.0d0
        end do
      end do
!
      end subroutine init_fft_test_data
!
! ------------------------------------------------------------------
!
      subroutine swap_fft_test_input_to_pin(ftst)
!
      type(fft_test_data), intent(inout) :: ftst
!
      type(fft_test_data) :: tmp
      integer(kind = kint) :: nd
!
      call alloc_pout_fft_test_data(ftst%nfld, ftst%ngrd, tmp)
!
!$omp parallel workshare
      tmp%org(1:ftst%nfld,1:ftst%ngrd)                                  &
     &       = ftst%org(1:ftst%nfld,1:ftst%ngrd)
!$omp end parallel workshare
!
      call dealloc_fft_test_data(ftst)
      call alloc_pin_fft_test_data(tmp%nfld, tmp%ngrd, ftst)
!
!$omp parallel do private(nd)
      do nd = 1, ftst%nfld
        ftst%org(1:ftst%ngrd,nd) = tmp%org(nd,1:ftst%ngrd)
      end do
!$omp end parallel do
!
      end subroutine swap_fft_test_input_to_pin
!
! ------------------------------------------------------------------
!
      subroutine swap_fft_test_data_to_pout(ftst)
!
      type(fft_test_data), intent(inout) :: ftst
!
      type(fft_test_data) :: tmp
      integer(kind = kint) :: i
!
      call alloc_pout_fft_test_data(ftst%nfld, ftst%ngrd, tmp)
!
!$omp parallel do private(i)
      do i = 1, ftst%ngrd
        tmp%org(1:ftst%nfld,i) = ftst%org(i,1:ftst%nfld)
        tmp%s_k(1:ftst%nfld,i) = ftst%s_k(i,1:ftst%nfld)
        tmp%f_x(1:ftst%nfld,i) = ftst%f_x(i,1:ftst%nfld)
      end do
!$omp end parallel do
!
      call dealloc_fft_test_data(ftst)
      call alloc_pout_fft_test_data(tmp%nfld, tmp%ngrd, ftst)
!
!
!$omp parallel workshare
      ftst%org(1:ftst%nfld,1:ftst%ngrd)                                 &
     &    = tmp%org(1:ftst%nfld,1:ftst%ngrd)
      ftst%s_k(1:ftst%nfld,1:ftst%ngrd)                                 &
     &    = tmp%s_k(1:ftst%nfld,1:ftst%ngrd)
      ftst%f_x(1:ftst%nfld,1:ftst%ngrd)                                 &
     &    = tmp%f_x(1:ftst%nfld,1:ftst%ngrd)
!$omp end parallel workshare
!
      end subroutine swap_fft_test_data_to_pout
!
! ------------------------------------------------------------------
!
      subroutine dealloc_fft_test_data(ftst)
!
      type(fft_test_data), intent(inout) :: ftst
!
      deallocate(ftst%org, ftst%s_k, ftst%f_x, ftst%nstack)
!
      end subroutine dealloc_fft_test_data
!
! ------------------------------------------------------------------
!
      subroutine alloc_pout_fft_test_data(nfld, ngrid, ftst)
!
      integer(kind = kint), intent(in) :: ngrid, nfld
      type(fft_test_data), intent(inout) :: ftst
!
      ftst%ngrd = ngrid
      ftst%nfld = nfld
      allocate(ftst%org(ftst%nfld,ftst%ngrd))
      allocate(ftst%s_k(ftst%nfld,ftst%ngrd))
      allocate(ftst%f_x(ftst%nfld,ftst%ngrd))
!
      call init_fft_test_smp_stack(ftst)
!
      end subroutine alloc_pout_fft_test_data
!
! ------------------------------------------------------------------
!
      subroutine alloc_pin_fft_test_data(nfld, ngrid, ftst)
!
      integer(kind = kint), intent(in) :: ngrid, nfld
      type(fft_test_data), intent(inout) :: ftst
!
      ftst%ngrd = ngrid
      ftst%nfld = nfld
      allocate(ftst%org(ftst%ngrd,ftst%nfld))
      allocate(ftst%s_k(ftst%ngrd,ftst%nfld))
      allocate(ftst%f_x(ftst%ngrd,ftst%nfld))
!
      call init_fft_test_smp_stack(ftst)
!
      end subroutine alloc_pin_fft_test_data
!
! ------------------------------------------------------------------
!
      subroutine init_fft_test_smp_stack(ftst)
!
      type(fft_test_data), intent(inout) :: ftst
      integer(kind = kint) :: i
!
      if(np_smp .le. 0) np_smp = 1
      allocate(ftst%nstack(0:np_smp))
!
      ftst%nstack(0) = 0
      do i = 1, np_smp - 1
        ftst%nstack(i) = (i * ftst%nfld) / np_smp
      end do
      ftst%nstack(np_smp) = ftst%nfld
!
      end subroutine init_fft_test_smp_stack
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
!
!
      write(*,*) 'Save FFT test data into ', trim(file_name)
      open(15,file=file_name)
      write(15,'(a)') '# Num_of_field, Nlength'
      write(15,'(2i5)')  ftst%nfld, ftst%ngrd
      write(15,'(a)') '# Num_of_threads'
      write(15,'(2i5)')  np_smp
      do j = 1, ftst%nfld
          write(15,'(a,i5)') 'Field Index:', j
          write(15,'(a)')                                               &
     &         'index, mode, Original, Fwd_Back_Trans, Spectr'
        do i = 1, ftst%ngrd
          k = ((i+1)/2-1) * (-1)**mod((i-ione),itwo)
          if(i .eq. 2) k = (ftst%ngrd + 1) / 2
          write(15,'(2i5,1p3E25.15e3)')                                 &
     &          i, k, ftst%org(j,i), ftst%f_x(j,i), ftst%s_k(j,i)
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
      integer(kind = kint) :: i, j
      integer(kind = kint) :: ngrid, nfld
      integer(kind = kint) :: itmp
      character(len=kchara) :: tmpchara
!
      open(15,file=file_name)
      read(15,*) tmpchara
      read(15,*)  nfld, ngrid
      read(15,*) tmpchara
      read(15,*)  itmp
      call alloc_pout_fft_test_data(nfld, ngrid, ftst)
!
      do j = 1, ftst%nfld
          read(15,*) tmpchara
          read(15,*) tmpchara
        do i = 1, ftst%ngrd
          read(15,*) itmp, itmp, ftst%org(j,i),                         &
     &              ftst%f_x(j,i), ftst%s_k(j,i)
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
      integer(kind = kint) :: i, j
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
          diff = ftst_2%org(j,i) - ftst_1%org(j,i)
          if(abs(diff) .gt. TINY) then
            write(*,*) 'Inconsistent result_back in ',                  &
     &                j, '-th field at ', i
            compare_FFT_tests = 1
          end if
!
          diff = ftst_2%s_k(j,i) - ftst_1%s_k(j,i)
          if(abs(diff) .gt. TINY) then
            write(*,*) 'Inconsistent result_fw in ',                    &
     &                j, '-th field at ', i
            compare_FFT_tests = 1
          end if
!
          diff = ftst_2%f_x(j,i) - ftst_1%f_x(j,i)
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
