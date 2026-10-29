!>@file   t_parameters_FFT_tests.f90
!!@brief  module t_parameters_FFT_tests
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for FFT tests
!!
!!@verbatim
!!      subroutine default_FFT_test_parameters(test_name, def_fname,    &
!!     &                                       fft_test_p)
!!        character(len = kchara), intent(in) :: test_name, def_fname
!!        type(FFT_test_parameters), intent(inout) :: fft_test_p
!!      subroutine set_FFT_test_parameters(fft_c, fft_test_p)
!!        type(FFT_tests_ctl), intent(in) :: fft_c
!!        type(FFT_test_parameters), intent(inout) :: fft_test_p
!!
!!      subroutine write_fft_test_elapsed(fft_test_p, elapsed)
!!        type(FFT_test_parameters), intent(in) :: fft_test_p
!!        real(kind = kreal), intent(in) :: elapsed(3)
!!      subroutine write_rocFFT_test_elapsed(fft_test_p, elapsed)
!!        type(FFT_test_parameters), intent(in) :: fft_test_p
!!        real(kind = kreal), intent(in) :: elapsed(4)
!!      subroutine write_sharing_FFT_test_elapsed(fft_test_p, elapsed)
!!        type(FFT_test_parameters), intent(in) :: fft_test_p
!!        real(kind = kreal), intent(in) :: elapsed(9)
!!@endverbatim
!
      module t_parameters_FFT_tests
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!
      integer(kind = kint), parameter, private ::  ngrid =    128
      integer(kind = kint), parameter, private ::  n_field =    8
      integer(kind = kint), parameter, private ::  n_loop =     1
!
      type FFT_test_parameters
!>        FFT library name
        character(len = kchara) :: test_name
!>        output file name
        character(len = kchara) :: file_name
!
!>        Integer flag for FFT selection
        integer(kind = kint) :: iflag_FFT
!>        Integer flag for CPU FFT selection
        integer(kind = kint) :: iflag_CPU_FFT
!>        FFT name
        character(len = kchara) :: FFT_name
!>        CPU FFT name
        character(len = kchara) :: CPU_FFT_name
!
!>        Length of FFT
        integer(kind = kint) :: Nfft_test =  ngrid
!>        number of date series for FFT
        integer(kind = kint) :: Ncomp_test = n_field
!>        Number of iteration of test
        integer(kind = kint) :: Nloop_test = n_loop
!
!>        Ratio of Number of FFT on GPU
        real(kind = kreal) :: ratio_rocFFT = 0.5
!>        number of date series for FFT on CPU
        integer(kind = kint) :: nfld_CPU = n_field / 2
!>        number of date series for FFT on GPU
        integer(kind = kint) :: nfld_GPU = n_field / 2
      end type FFT_test_parameters
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine default_FFT_test_parameters(test_name, def_fname,      &
     &                                       fft_test_p)
!
      character(len = kchara), intent(in) :: test_name, def_fname
      type(FFT_test_parameters), intent(inout) :: fft_test_p
!
!
      fft_test_p%FFT_name =  test_name
      fft_test_p%test_name = test_name
      fft_test_p%file_name = def_fname
!
      end subroutine default_FFT_test_parameters
!
!  ---------------------------------------------------------------------
!
      subroutine set_FFT_test_parameters(fft_c, fft_test_p)
!
      use m_FFT_labels
      use t_ctl_data_4_FFT_tests
      use skip_comment_f
!
      type(FFT_tests_ctl), intent(in) :: fft_c
      type(FFT_test_parameters), intent(inout) :: fft_test_p
!
!
      iflag_debug = 0
      if((fft_c%debug_flag_ctl%iflag .gt. 0)                            &
     &   .and. yes_flag(fft_c%debug_flag_ctl%charavalue)) then
        iflag_debug = 1
      end if
!
      if(fft_c%FFT_test_output_ctl%iflag .gt. 0) then
        fft_test_p%file_name = fft_c%FFT_test_output_ctl%charavalue
      end if
      if(fft_c%FFT_length_ctl%iflag .gt. 0) then
        fft_test_p%Nfft_test = fft_c%FFT_length_ctl%intvalue
      end if
      if(fft_c%num_series_ctl%iflag .gt. 0) then
        fft_test_p%Ncomp_test = fft_c%num_series_ctl%intvalue
      end if
      if(fft_c%loop_counts_ctl%iflag .gt. 0) then
        fft_test_p%nloop_test = fft_c%loop_counts_ctl%intvalue
      end if
      if(fft_c%GPU_ratio_ctl%iflag .gt. 0) then
        fft_test_p%ratio_rocFFT = fft_c%GPU_ratio_ctl%realvalue
      end if
!
      fft_test_p%nfld_GPU                                               &
     &        = fft_test_p%Ncomp_test * fft_test_p%ratio_rocFFT
      fft_test_p%nfld_CPU = fft_test_p%Ncomp_test - fft_test_p%nfld_GPU
!
      fft_test_p%iflag_FFT                                              &
     &     = set_fft_library_ctl(fft_c%FFT_lib_ctl%iflag,               &
     &                           fft_c%FFT_lib_ctl%charavalue)
      fft_test_p%FFT_name = find_FFT_label(fft_test_p%iflag_FFT)
!
      fft_test_p%iflag_CPU_FFT                                          &
     &     = set_fft_library_ctl(fft_c%second_FFT_lib_ctl%iflag,        &
     &                           fft_c%second_FFT_lib_ctl%charavalue)
      fft_test_p%CPU_FFT_name                                           &
     &     = find_FFT_label(fft_test_p%iflag_CPU_FFT)
!
      end subroutine set_FFT_test_parameters
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_fft_test_elapsed(fft_test_p, elapsed)
!
      use m_machine_parameter
!
      type(FFT_test_parameters), intent(in) :: fft_test_p
      real(kind = kreal), intent(in) :: elapsed(3)
!
!
      write(*, '(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i7)')  'Num (point, field, loop): ',                &
     &                   fft_test_p%nfft_test, fft_test_p%Ncomp_test,   &
     &                   fft_test_p%nloop_test
      write(*, '(a,1pE16.6e3)') 'Initialize:      ', elapsed(1)
      write(*, '(2a,1pE16.6e3)') trim(fft_test_p%FFT_name),             &
     &                                         ': ', elapsed(2)
      write(*, '(a,1pE16.6e3)') 'Data copy:       ', elapsed(3)
      write(*, '(a,1pE16.6e3)') 'Total FFT:       ',                    &
     &                                 (elapsed(2) + elapsed(3))
      write(*,'(a)') '-----------------------------'
      write(*,'(a)') ' '
!
      end subroutine write_fft_test_elapsed
!
!  ---------------------------------------------------------------------
!
      subroutine write_rocFFT_test_elapsed(fft_test_p, elapsed)
!
      use m_machine_parameter
!
      type(FFT_test_parameters), intent(in) :: fft_test_p
      real(kind = kreal), intent(in) :: elapsed(4)
!
!
      write(*, '(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i7)')  'Num (point, field, loop): ',                &
     &                   fft_test_p%nfft_test, fft_test_p%Ncomp_test,   &
     &                   fft_test_p%nloop_test
      write(*, '(a,1pE16.6e3)') 'Initialize:      ', elapsed(1)
      write(*, '(2a,1pE16.6e3)') trim(fft_test_p%FFT_name),             &
     &                                         ': ', elapsed(2)
      write(*, '(a,1pE16.6e3)')                                         &
     &                'Time for rocFFT w/o first: ', elapsed(4)
      write(*, '(a,1pE16.6e3)') 'Data copy:       ', elapsed(3)
      write(*, '(a,1pE16.6e3)') 'Total FFT:       ',                    &
     &                                 (elapsed(2) + elapsed(3))
      write(*,'(a)') '-----------------------------'
      write(*,'(a)') ' '
!
      end subroutine write_rocFFT_test_elapsed
!
!  ---------------------------------------------------------------------
!
      subroutine write_sharing_FFT_test_elapsed(fft_test_p, elapsed)
!
      use m_machine_parameter
!
      type(FFT_test_parameters), intent(in) :: fft_test_p
      real(kind = kreal), intent(in) :: elapsed(9)
!
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*,'(a,3i7)')  'Num (point, field, loop): ',                 &
     &                   fft_test_p%nfft_test, fft_test_p%Ncomp_test,   &
     &                   fft_test_p%nloop_test
      write(*,'(a,3i7)')  '(for GPU, for CPU): ',                       &
     &                   fft_test_p%nfld_GPU, fft_test_p%nfld_CPU
      write(*,'(a,1pE16.6e3)') 'Initialize:      ',  elapsed(1)
      write(*,'(4a,1pE16.6e3)') trim(fft_test_p%FFT_name), '_',         &
     &         trim(fft_test_p%CPU_FFT_name), ': ',  elapsed(2)
      write(*,'(a,1pE16.6e3)') 'FFT on GPU:       ', elapsed(5)
      write(*,'(a,1pE16.6e3)') 'FFT on CPU:       ', elapsed(4)
      write(*,'(a,1pE16.6e3)') 'Time for FFT w/o first: ', elapsed(6)
      write(*,'(a,1pE16.6e3)') 'FFT on GPU w/o first: ',   elapsed(9)
      write(*,'(a,1pE16.6e3)') 'FFT on CPU w/o first: ',   elapsed(8)
      write(*,'(a,1pE16.6e3)') 'Data copy:            ',   elapsed(3)
      write(*,'(a,1pE16.6e3)') 'Total FFT:            ',                &
     &                                 (elapsed(2) + elapsed(3))
      write(*,'(a)') '-----------------------------'
      write(*,'(a)') ' '
!
      end subroutine write_sharing_FFT_test_elapsed
!
!  ---------------------------------------------------------------------
!
      end module t_parameters_FFT_tests
