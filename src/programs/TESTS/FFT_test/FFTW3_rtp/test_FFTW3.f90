!>@file   test_FFTW3.f90
!!@brief  module test_FFTW3
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!!      Modified in Aug., 2026
!
!> @brief Test program of FFTW3 with outer series array
!!
!!@verbatim
!! ----------------------------------------------------------------------
!!     Control file example
!! ----------------------------------------------------------------------
!!  begin FFT_test_ctl
!!    output_file_name    'mul_fftw_test.dat'
!!
!!    FFT_length_ctl         128
!!    num_series_ctl          24
!!    num_test_loop_ctl       10
!!  end FFT_test_ctl
!!
!! ----------------------------------------------------------------------
!!@endverbatim
      program test_FFTW3
!
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_parameters_FFT_tests
      use t_ctl_data_4_FFT_tests
      use t_multi_FFTW_wrapper
      use t_fft_test_data
      use calypso_multi_FFTW3
      use multi_pout_FFTW3_smp
!
      implicit none
!
      character(len = kchara), parameter                                &
     &                        :: test_name = 'rtp_FFTW3'
      character(len = kchara), parameter                                &
     &                        :: def_fname = 'mul_fftw_test.dat'
!
      character(len = kchara) :: ctl_file_name
      type(FFT_tests_ctl), save :: fft_c1
      type(FFT_test_parameters), save :: fft_test_p1
!
      type(working_mul_FFTW) :: WK_MUL_FFTW_t
      type(fft_test_data) :: ft3
      integer(kind = kint) :: iloop = 0
!
!
!
      write(*,'(a)') '-----  Test FFTW with outer series data -----'
      call default_FFT_test_parameters(test_name, def_fname,            &
     &                                 fft_test_p1)
      if(command_argument_count() .ge. 1) then
        call get_command_argument(1, ctl_file_name)
        call read_control_file_FFT_tests(ctl_file_name, fft_c1)
        call set_FFT_test_parameters(fft_c1, fft_test_p1)
      else
        write(*,*) 'No control file name in command: Use default'
      end if
      fft_test_p1%FFT_name = test_name
!
      call init_fft_test_data                                           &
     &   (fft_test_p1%Ncomp_test, fft_test_p1%Nfft_test, ft3)
!
      ft3%start = OMP_GET_WTIME()
      call init_FFTW_mul_type                                           &
     &   (np_smp, ft3%nstack, ft3%ngrd, WK_MUL_FFTW_t)
      ft3%elapsed(1) = ft3%elapsed(1) + OMP_GET_WTIME() - ft3%start
!
      do iloop = 1, fft_test_p1%nloop_test
        if(mod(iloop, 20) .eq. 0) write(*,*) 'loop count: ', iloop
!
        ft3%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft3%s_k(1:ft3%nfld,1:ft3%ngrd) = ft3%org(1:ft3%nfld,1:ft3%ngrd)
!$omp end parallel workshare
        ft3%elapsed(3) = ft3%elapsed(3) + OMP_GET_WTIME() - ft3%start
!
        call multi_pout_fwd_FFTW3(ft3%nfld, ft3%ngrd, ft3%s_k,          &
     &      WK_MUL_FFTW_t, ft3%elapsed(2), ft3%elapsed(3))
!
        ft3%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft3%f_x(1:ft3%nfld,1:ft3%ngrd) = ft3%s_k(1:ft3%nfld,1:ft3%ngrd)
!$omp end parallel workshare
        ft3%elapsed(3) = ft3%elapsed(3) + OMP_GET_WTIME() - ft3%start
!
        call multi_pout_bwd_FFTW3(ft3%nfld, ft3%ngrd, ft3%f_x,          &
     &      WK_MUL_FFTW_t, ft3%elapsed(2), ft3%elapsed(3))
      end do
!
      ft3%start = OMP_GET_WTIME()
      if(fft_test_p1%nloop_test .eq. 1) then
        call write_fft_test_data(fft_test_p1%file_name, ft3)
      end if
      call dealloc_fft_test_data(ft3)
      ft3%elapsed(1) = ft3%elapsed(1) + OMP_GET_WTIME() - ft3%start
!
      call write_fft_test_elapsed(fft_test_p1, ft3%elapsed(1))
!
      stop 'finish'
      end program test_FFTW3

