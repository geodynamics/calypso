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
      integer(kind = kint) :: iflag
!
!
      type(fft_test_data) :: ftst_1, ftst_2
      character(len=kchara) :: file_name_1, file_name_2
!
!
      if(command_argument_count() .le. 1) then
        write(*,*) 'compare_fft_test FFT_TEST_RESULT_1 FFT_TEST_RESULT_2'
        stop
      end if
      call get_command_argument(1, file_name_1)
      call get_command_argument(2, file_name_2)
!
      write(*,*) 'Reference data file name: ', trim(file_name_1)
      write(*,*) 'Compared data file name:  ', trim(file_name_2)
!
      call read_alloc_fft_test_data(file_name_1, ftst_1)
      call read_alloc_fft_test_data(file_name_2, ftst_2)
!
      iflag = compare_FFT_tests(ftst_1, ftst_2)
      if(iflag .eq. 0) then
        write(*,*) 'Data in two files have consistency.'
      end if
      write(*,'(i1)') iflag
!
      call dealloc_fft_test_data(ftst_1)
      call dealloc_fft_test_data(ftst_2)
!
      stop
      end program compare_FFT_test

