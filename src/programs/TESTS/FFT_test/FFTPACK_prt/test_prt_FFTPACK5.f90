!>@file   test_prt_FFTPACK5.f90
!!@brief  module test_prt_FFTPACK5
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!!      Modified in Aug., 2026
!
!> @brief Test program of FFTPACK5 with inner series array
!!
!!@verbatim
!! ----------------------------------------------------------------------
!!     Control file example
!! ----------------------------------------------------------------------
!!  begin FFT_test_ctl
!!    output_file_name    'rtp_test_FFTPACK5.dat'
!!
!!    FFT_length_ctl         128
!!    num_series_ctl          24
!!    num_test_loop_ctl       10
!!  end FFT_test_ctl
!!
!! ----------------------------------------------------------------------
!!@endverbatim
      program test_prt_FFTPACK5
!
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_fft_test_data
      use t_FFTPACK5_wrapper
      use t_parameters_FFT_tests
      use t_ctl_data_4_FFT_tests
!
      use calypso_multi_fftpack
      use multi_pin_FFTPACK_smp
!
      implicit none
!
      character(len = kchara), parameter                                &
     &                        :: test_name = 'prt_FFTPACK5'
      character(len = kchara), parameter                                &
     &                        :: def_fname = 'prt_fftpack_test.dat'
!
      character(len = kchara) :: ctl_file_name
      type(FFT_tests_ctl), save :: fft_c1
      type(FFT_test_parameters), save :: fft_test_p1
!
      type(working_FFTPACK) :: WK_FFTPACK_T
      type(fft_test_data) :: ft1
      integer(kind = kint) :: iloop = 0
!
!
      write(*,'(a)') '-----  Test FFTPACK with inner series loop -----'
!
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
     &   (fft_test_p1%Ncomp_test, fft_test_p1%Nfft_test, ft1)
      call swap_fft_test_input_to_pin(ft1)
!
      ft1%start = OMP_GET_WTIME()
      call init_WK_FFTPACK_t                                            &
     &   (np_smp, ft1%nstack, ft1%ngrd, WK_FFTPACK_T)
      ft1%elapsed(1) = ft1%elapsed(1) + OMP_GET_WTIME() - ft1%start
!
      do iloop = 1, fft_test_p1%nloop_test
        if(mod(iloop, 20) .eq. 0) write(*,*) 'loop count: ', iloop
!
        ft1%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%s_k(1:ft1%ngrd,1:ft1%nfld) = ft1%org(1:ft1%ngrd,1:ft1%nfld)
!$omp end parallel workshare
        ft1%elapsed(3) = ft1%elapsed(3) + OMP_GET_WTIME() - ft1%start
!
        call calypso_pin_RFFTMF(ft1%nfld, ft1%ngrd, ft1%s_k,            &
     &      WK_FFTPACK_T, ft1%elapsed(2), ft1%elapsed(3))
!
        ft1%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%f_x(1:ft1%ngrd,1:ft1%nfld) = ft1%s_k(1:ft1%ngrd,1:ft1%nfld)
!$omp end parallel workshare
        ft1%elapsed(3) = ft1%elapsed(3) + OMP_GET_WTIME() - ft1%start
!
        call calypso_pin_RFFTMB(ft1%nfld, ft1%ngrd, ft1%f_x,            &
     &      WK_FFTPACK_T, ft1%elapsed(2), ft1%elapsed(3))
      end do
!
      if(fft_test_p1%nloop_test .eq. 1) then
        call swap_fft_test_data_to_pout(ft1)
        call write_fft_test_data(fft_test_p1%file_name, ft1)
      end if
      call dealloc_fft_test_data(ft1)
!
      call write_fft_test_elapsed(fft_test_p1, ft1%elapsed(1))
!
      stop 'finish'
      end program test_prt_FFTPACK5

