!
      program test_single_FFTW3
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_FFTW_wrapper
      use t_fft_test_data
!
      implicit none
!
      character(len = kchara) :: sgl_fftw_test = 'sgl_fftw_test.dat'
!
      type(working_FFTW) ::     WK_FFTW_t
      type(fft_test_data) :: ft3
!
      integer(kind = kint), parameter ::  ngrid = 128
!
!
      iflag_debug = 1
      np_smp = 2
      call init_fft_test_data(ngrid, ft3)
!
        write(*,*) 'Test single transform in FFTW'
        call init_FFTW_type                                             &
     &     (ft3%nstack(np_smp), ft3%ngrd, WK_FFTW_t)
!
!$omp parallel workshare
      ft3%z(1:ft3%nfld,1:ft3%ngrd) = ft3%x(1:ft3%nfld,1:ft3%ngrd)
!$omp end parallel workshare
!
      call FFTW_forward_type(np_smp, ft3%nstack,                        &
     &    ft3%nfld, ft3%ngrd, ft3%x, WK_FFTW_t)
!
!$omp parallel workshare
      ft3%y(1:ft3%nfld,1:ft3%ngrd) = ft3%x(1:ft3%nfld,1:ft3%ngrd)
!$omp end parallel workshare
!
      call FFTW_backward_type(np_smp, ft3%nstack,                       &
     &    ft3%nfld, ft3%ngrd, ft3%x, WK_FFTW_t)
!
      call write_fft_test_data(sgl_fftw_test, ft3)
      call dealloc_fft_test_data(ft3)
!
      stop
      end program test_single_FFTW3

