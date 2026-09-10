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
      type(fft_test_data) :: ft1, ft2
      character(len=kchara) :: file_name_1, file_name_2
!
!
      if(command_argument_count() .lt. 2) then
        write(*,*) 'compare_fft_test FFT_TEST_RESULT_1 FFT_TEST_RESULT_2'
        stop
      end if
      call get_command_argument(1, file_name_1)
      call get_command_argument(2, file_name_2)
!
      write(*,*) 'Reference data file name: ', trim(file_name_1)
      write(*,*) 'Compared data file name:  ', trim(file_name_2)
!
      call read_alloc_fft_test_data(file_name_1, ft1)
      call read_alloc_fft_test_data(file_name_2, ft2)
!
      iflag = compare_FFT_tests(ft1, ft2)
      if(iflag .eq. 0) then
        write(*,*) 'Data in two files have consistency.'
      end if
      write(*,'(i1)') iflag
!
      call dealloc_fft_test_data(ft1)
      call dealloc_fft_test_data(ft2)
!
      stop
!
! ------------------------------------------------------------------
!
      contains
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
            write(*,*) 'Inconsistent input data in ',                   &
     &                j, '-th field at ', i
            compare_FFT_tests = 1
          end if
!
          diff = ftst_2%s_k(j,i) - ftst_1%s_k(j,i)
          if(abs(diff) .gt. TINY) then
            write(*,*) 'Inconsistent result spectr in ',                &
     &                j, '-th field at ', i
            compare_FFT_tests = 1
          end if
!
          diff = ftst_2%f_x(j,i) - ftst_1%f_x(j,i)
          if(abs(diff) .gt. TINY) then
            write(*,*) 'Inconsistent backward tranfer in ',             &
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
      end program compare_FFT_test

