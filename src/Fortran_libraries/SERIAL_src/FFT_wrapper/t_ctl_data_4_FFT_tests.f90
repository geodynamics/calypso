!>@file   t_ctl_data_4_FFT_tests.f90
!!@brief  module t_ctl_data_4_FFT_tests
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for FFT tests
!!
!!@verbatim
!!      subroutine read_control_file_FFT_tests(file_name, fft_c)
!!        character(len=kchara), intent(in) :: file_name
!!        type(FFT_tests_ctl), intent(inout) :: fft_c
!!      subroutine write_control_file_FFT_tests(file_name, fft_c)
!!        character(len=kchara), intent(in) :: file_name
!!        type(FFT_tests_ctl), intent(inout) :: fft_c
!!      subroutine reset_FFT_tests_ctl(fft_c)
!!        type(FFT_tests_ctl), intent(inout) :: fft_c
!! ----------------------------------------------------------------------
!!
!!  begin FFT_test_ctl
!!    debug_flag_ctl     ON
!!
!!    output_file_name    'test_ISPACK3.dat'
!!
!!    FFT_library_ctl        'rocFFT_real'
!!    2nd_FFT_library_ctl    'ISPACK3'
!!
!!    FFT_length_ctl         128
!!    num_series_ctl          24
!!    num_test_loop_ctl       10
!!
!!    split_ratio_ctl         0.3
!!  end FFT_test_ctl
!!
!! ----------------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_4_FFT_tests
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_real
      use t_control_array_integer
      use t_control_array_character
!
      implicit none
!
      integer(kind = kint), private :: id_control_file = 11
!
      type FFT_tests_ctl
!>        Block name
        character(len=kchara) :: block_name = 'FFT_test_ctl'
!
!>        Debug flag
        type(read_character_item) :: debug_flag_ctl
!
!>        Structure for output file name
        type(read_character_item) :: FFT_test_output_ctl
!
!>        Structure for FFT library
        type(read_character_item) :: FFT_lib_ctl
!>        Structure for 2nd FFT library
        type(read_character_item) :: second_FFT_lib_ctl
!
!>        Structure for length of time series
        type(read_integer_item) :: FFT_length_ctl
!>        Structure for Number of time series
        type(read_integer_item) :: num_series_ctl
!>        Structure for loop counts for test
        type(read_integer_item) :: loop_counts_ctl
!>        Structure for ratio of data for first FFT
        type(read_real_item) ::    split_ratio_ctl
!
        integer(kind = kint) :: i_FFT_tests_ctl = 0
      end type FFT_tests_ctl
!
!
      character(len=kchara), parameter, private                         &
     &      :: hd_FFT_tests_ctll = 'FFT_test_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_debug_flag_ctl =      'debug_flag_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_FFT_test_output_ctl = 'output_file_name'
      character(len=kchara), parameter, private                         &
     &      :: hd_FFT_lib_ctl  =        'FFT_library_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_2nd_FFT_lib_ctl  =    '2nd_FFT_library_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_FFT_length_ctl  =     'FFT_length_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_num_series_ctl  =     'num_series_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_loop_counts_ctl  =    'num_test_loop_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_split_ratio_ctl  =    'split_ratio_ctl'
!
      private :: read_FFT_tests_ctl, write_FFT_tests_ctl
      private :: init_FFT_tests_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_file_FFT_tests(file_name, fft_c)
!
      character(len=kchara), intent(in) :: file_name
      type(FFT_tests_ctl), intent(inout) :: fft_c
!
      type(buffer_for_control), save :: c_buf1
!
!
      c_buf1%level = 1
      call init_FFT_tests_ctl(hd_FFT_tests_ctll, fft_c)
      open(id_control_file, file = file_name, status='old' )
!
      do
        call load_one_line_from_control(id_control_file,                &
     &                                  hd_FFT_tests_ctll, c_buf1)
        if(c_buf1%iend .gt. 0) exit
!
        call read_FFT_tests_ctl(id_control_file, hd_FFT_tests_ctll,     &
     &                          fft_c, c_buf1)
        if(fft_c%i_FFT_tests_ctl .gt. 0) exit
      end do
      close(id_control_file)
      c_buf1%level = c_buf1%level - 1
!
      end subroutine read_control_file_FFT_tests
!
! ----------------------------------------------------------------------
!
      subroutine write_control_file_FFT_tests(file_name, fft_c)
!
      use delete_data_files
!
      character(len=kchara), intent(in) :: file_name
      type(FFT_tests_ctl), intent(inout) :: fft_c
!
      integer(kind = kint) :: level1
!
!
      if(check_file_exist(file_name)) then
        write(*,*) 'File ', trim(file_name), ' exist. Continue?'
        read(*,*)
      end if
!
      write(*,*) 'Write FFT test control file: ', trim(file_name)
      level1 = 0
      open(id_control_file, file = file_name)
      call write_FFT_tests_ctl(id_control_file, hd_FFT_tests_ctll,      &
     &                         fft_c, level1)
      close(id_control_file)
!
      end subroutine write_control_file_FFT_tests
!
! ----------------------------------------------------------------------
!
      subroutine reset_FFT_tests_ctl(fft_c)
!
      type(FFT_tests_ctl), intent(inout) :: fft_c
!
!
      fft_c%debug_flag_ctl%iflag =      0
      fft_c%FFT_test_output_ctl%iflag = 0
!
      fft_c%FFT_lib_ctl%iflag =         0
      fft_c%second_FFT_lib_ctl%iflag =  0
!
      fft_c%FFT_length_ctl%iflag =      0
      fft_c%num_series_ctl%iflag =      0
      fft_c%loop_counts_ctl%iflag =     0
!
      fft_c%split_ratio_ctl%iflag =     0
!
      fft_c%i_FFT_tests_ctl = 0
!
      end subroutine reset_FFT_tests_ctl
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_FFT_tests_ctl(id_control, hd_block,               &
     &                              fft_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(FFT_tests_ctl), intent(inout) :: fft_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(fft_c%i_FFT_tests_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_debug_flag_ctl,              &
     &                           fft_c%debug_flag_ctl)
        call read_chara_ctl_type(c_buf, hd_FFT_test_output_ctl,         &
     &                           fft_c%FFT_test_output_ctl)
!
        call read_chara_ctl_type(c_buf, hd_FFT_lib_ctl,                 &
     &                           fft_c%FFT_lib_ctl)
        call read_chara_ctl_type(c_buf, hd_2nd_FFT_lib_ctl,             &
     &                           fft_c%second_FFT_lib_ctl)
!
        call read_integer_ctl_type(c_buf, hd_FFT_length_ctl,            &
     &                             fft_c%FFT_length_ctl)
        call read_integer_ctl_type(c_buf, hd_num_series_ctl,            &
     &                             fft_c%num_series_ctl)
        call read_integer_ctl_type(c_buf, hd_loop_counts_ctl,           &
     &                             fft_c%loop_counts_ctl)
!
        call read_real_ctl_type(c_buf, hd_split_ratio_ctl,              &
     &                          fft_c%split_ratio_ctl)
      end do
      fft_c%i_FFT_tests_ctl = 1
!
      end subroutine read_FFT_tests_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_FFT_tests_ctl(id_control, hd_block,              &
     &                               fft_c, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(FFT_tests_ctl), intent(in) :: fft_c
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(fft_c%i_FFT_tests_ctl .le. 0) return
!
      maxlen = len_trim(hd_debug_flag_ctl)
      maxlen = max(maxlen, len_trim(hd_FFT_test_output_ctl))
      maxlen = max(maxlen, len_trim(hd_FFT_lib_ctl))
      maxlen = max(maxlen, len_trim(hd_2nd_FFT_lib_ctl))
      maxlen = max(maxlen, len_trim(hd_FFT_length_ctl))
      maxlen = max(maxlen, len_trim(hd_num_series_ctl))
      maxlen = max(maxlen, len_trim(hd_loop_counts_ctl))
      maxlen = max(maxlen, len_trim(hd_split_ratio_ctl))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                            fft_c%debug_flag_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                            fft_c%FFT_test_output_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                            fft_c%FFT_lib_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                            fft_c%second_FFT_lib_ctl)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &                            fft_c%FFT_length_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &                            fft_c%num_series_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &                            fft_c%loop_counts_ctl)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &                         fft_c%split_ratio_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_FFT_tests_ctl
!
! -----------------------------------------------------------------------
!
      subroutine init_FFT_tests_ctl(hd_block, fft_c)
!
      character(len=kchara), intent(in) :: hd_block
      type(FFT_tests_ctl), intent(inout) :: fft_c
!
!
      fft_c%block_name = trim(hd_block)
      call init_chara_ctl_item_label(hd_debug_flag_ctl,                 &
     &                               fft_c%debug_flag_ctl)
      call init_chara_ctl_item_label(hd_FFT_test_output_ctl,            &
     &                               fft_c%FFT_test_output_ctl)
!
      call init_chara_ctl_item_label(hd_FFT_lib_ctl, fft_c%FFT_lib_ctl)
      call init_chara_ctl_item_label(hd_2nd_FFT_lib_ctl,                &
     &                               fft_c%second_FFT_lib_ctl)
!
      call init_int_ctl_item_label(hd_FFT_length_ctl,                   &
     &                             fft_c%FFT_length_ctl)
      call init_int_ctl_item_label(hd_num_series_ctl,                   &
     &                             fft_c%num_series_ctl)
      call init_int_ctl_item_label(hd_loop_counts_ctl,                  &
     &                             fft_c%loop_counts_ctl)
!
      call init_real_ctl_item_label(hd_split_ratio_ctl,                 &
     &                              fft_c%split_ratio_ctl)
!
      end subroutine init_FFT_tests_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_4_FFT_tests
