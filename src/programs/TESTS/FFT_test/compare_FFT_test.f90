!
      program compare_FFT_test
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_fft_test_data
!
      implicit none
!
      type(fft_test_data) :: ftst_1, ftst_2
      character(len=kchara) :: file_name_1, file_name_2
!
!
      write(*,*) 'Input reference data file name'
      read(*,*) file_name_1
      write(*,*) 'Input target data file name'
      read(*,*) file_name_2
!
      call read_alloc_fft_test_data(file_name_1, ftst_1)
      call read_alloc_fft_test_data(file_name_2, ftst_2)
!
      if(compare_FFT_tests(ftst_1, ftst_2) .eq. 0) then
        write(*,*) 'Data in two files have consistency.'
      end if
!
      call dealloc_fft_test_data(ftst_1)
      call dealloc_fft_test_data(ftst_2)
!
      stop
      end program compare_FFT_test

