!
      program test_FFTPACK5
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_fft_test_data
      use t_FFTPACK5_wrapper
!
      implicit none
!
      type(working_FFTPACK) :: WK_FFTPACK_T
      type(fft_test_data) :: ft1
      integer(kind = kint), parameter ::  ngrid = 128
!
!
      np_smp = 2
      call init_fft_test_data(ngrid, ft1)
!
!$omp parallel workshare
      ft1%z(1:ft1%nfld,1:ft1%ngrd) = ft1%x(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
!
      call init_WK_FFTPACK_t                                            &
     &   (np_smp, ft1%nstack, ft1%ngrd, WK_FFTPACK_T)
!
      call CALYPSO_RFFTMF_t(np_smp, ft1%nstack, ft1%nfld, ft1%ngrd,     &
     &                      ft1%x, WK_FFTPACK_T)
!
!$omp parallel workshare
      ft1%y(1:ft1%nfld,1:ft1%ngrd) = ft1%x(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
!
      call CALYPSO_RFFTMB_t(np_smp, ft1%nstack, ft1%nfld, ft1%ngrd,     &
     &                      ft1%x, WK_FFTPACK_T)
!
      call write_fft_test_data('fftpack_test.dat', ft1)
      call dealloc_fft_test_data(ft1)
!
      stop
      end program test_FFTPACK5

