!
      program test_FFTW3
!
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_FFT_size
!
      use t_multi_FFTW_wrapper
      use t_fft_test_data
!
      implicit none
!
      character(len = kchara), parameter                                &
     &                       :: file_name = 'mul_fftw_test.dat'
!
      type(working_mul_FFTW) :: WK_MUL_FFTW_t
      type(fft_test_data) :: ft3
      integer(kind = kint) :: iloop = 0
!
!
      write(*,'(a)') '-----  Test FFTW  -----'
      iflag_debug = 1
      call init_fft_test_data(n_field, ngrid, ft3)
!
      ft3%start = OMP_GET_WTIME()
      call init_FFTW_mul_type                                           &
     &   (np_smp, ft3%nstack, ft3%ngrd, WK_MUL_FFTW_t)
      ft3%elapsed(1) = ft3%elapsed(1) + OMP_GET_WTIME() - ft3%start
!
      do iloop = 1, n_loop
        if(mod(iloop, 20) .eq. 0) write(*,*) 'loop count: ', iloop
!
        ft3%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft3%s_k(1:ft3%nfld,1:ft3%ngrd) = ft3%org(1:ft3%nfld,1:ft3%ngrd)
!$omp end parallel workshare
        ft3%elapsed(3) = ft3%elapsed(3) + OMP_GET_WTIME() - ft3%start
!
        ft3%start = OMP_GET_WTIME()
        call FFTW_mul_forward_type(np_smp, ft3%nstack,                  &
     &      ft3%nfld, ft3%ngrd, ft3%s_k, WK_MUL_FFTW_t)
        ft3%elapsed(2) = ft3%elapsed(2) + OMP_GET_WTIME() - ft3%start
!
        ft3%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft3%f_x(1:ft3%nfld,1:ft3%ngrd) = ft3%s_k(1:ft3%nfld,1:ft3%ngrd)
!$omp end parallel workshare
        ft3%elapsed(3) = ft3%elapsed(3) + OMP_GET_WTIME() - ft3%start
!
        ft3%start = OMP_GET_WTIME()
        call FFTW_mul_backward_type(np_smp, ft3%nstack,                 &
     &      ft3%nfld, ft3%ngrd, ft3%f_x, WK_MUL_FFTW_t)
        ft3%elapsed(2) = ft3%elapsed(2) + OMP_GET_WTIME() - ft3%start
      end do
!
      ft3%start = OMP_GET_WTIME()
      if(n_loop .eq. 1) call write_fft_test_data(file_name, ft3)
      call dealloc_fft_test_data(ft3)
      ft3%elapsed(1) = ft3%elapsed(1) + OMP_GET_WTIME() - ft3%start
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')                                               &
     &        "Num (point, field, loop): ", ngrid, n_field, n_loop
      write(*, '("Initialize:      ",1pE16.6e3)') ft3%elapsed(1)
      write(*, '("Wrapped FFTPACK: ",1pE16.6e3)') ft3%elapsed(2)
      write(*, '("Data copy:       ",1pE16.6e3)') ft3%elapsed(3)
!
      stop 'finish'
      end program test_FFTW3

