!>@file   t_ctl_data_tave_sph_monitor.f90
!!        module t_ctl_data_tave_sph_monitor
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine read_control_file_sph_monitor(my_rank, file_name,    &
!!     &                                         tave_sph_ctl)
!!      subroutine read_ctl_tave_sph_monitor                            &
!!        character(len=kchara), intent(in) :: file_name
!!        type(tave_sph_monitor_ctl), intent(inout) :: tave_sph_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine dealloc_ctl_tave_sph_monitor(tave_sph_ctl)
!!        type(tave_sph_monitor_ctl), intent(inout) :: tave_sph_ctl
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin time_averaging_sph_monitor
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    old_format_flag     'Off'
!!    degree_range_ctl     1   12
!!
!!    begin monitor_data_list_ctl
!!      array volume_integrate_prefix
!!        volume_integrate_prefix     'sph_ave_volume'
!!        volume_integrate_prefix     'sph_pwr_volume_s'
!!        volume_integrate_prefix     'sph_pwr_volume_m0'
!!      end array volume_integrate_prefix
!!
!!      array volume_sph_spectr_prefix
!!        volume_sph_spectr_prefix     'sph_pwr_volume_l'
!!        volume_sph_spectr_prefix     'sph_pwr_volume_m'
!!        volume_sph_spectr_prefix     'sph_pwr_volume_lm'
!!      end array volume_sph_spectr_prefix
!!
!!      array sphere_integrate_prefix
!!        sphere_integrate_prefix     'sph_pwr_layer_s'
!!        sphere_integrate_prefix     'sph_pwr_layer_m0'
!!      end array sphere_integrate_prefix
!!
!!      array layer_sph_spectr_prefix
!!        layer_sph_spectr_prefix     'sph_pwr_layer_l'
!!        layer_sph_spectr_prefix     'sph_pwr_layer_m'
!!        layer_sph_spectr_prefix     'sph_pwr_layer_lm'
!!      end array layer_sph_spectr_prefix
!!
!!      array picked_sph_prefix
!!        picked_sph_prefix        'monitor/picked_mode'
!!        picked_sph_prefix        'monitor/picked_mode_l2_m0c'
!!      end array picked_sph_prefix
!!    end monitor_data_list_ctl
!!  end time_averaging_sph_monitor
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_tave_sph_monitor
!
      use m_precision
!
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer2
      use t_ctl_data_sph_monitor_list
!
      implicit  none
!
!>        Control file ID
      integer(kind = kint), parameter, private :: id_control = 11
!
!
      type tave_sph_monitor_ctl
!>        Structure for start time
        type(read_real_item) :: start_time_ctl
!>        Structure for end time
        type(read_real_item) :: end_time_ctl
!
!>        Character flag for old format
        type(read_character_item) :: old_format_ctl
!>        Range of spherical harmonics degree
        type(read_int2_item) :: degree_range_ctl
!>        file prefix list
        type(sph_monitor_files_ctl) :: monitor_list_ctl
!
        integer (kind = kint) :: i_time_ave_sph = 0
      end type tave_sph_monitor_ctl
!
!
!    label for entry
!
      character(len=kchara), parameter, private                         &
     &             :: hd_tave_spectr =  'time_averaging_sph_monitor'
!
      character(len=kchara), parameter, private                         &
     &           :: hd_start_time_ctl = 'start_time_ctl'
      character(len=kchara), parameter, private                         &
     &           :: hd_end_time_ctl = 'end_time_ctl'
      character(len=kchara), parameter, private                         &
     &           :: hd_old_format =   'old_format_flag'
      character(len=kchara), parameter, private                         &
     &           :: hd_degree_range = 'degree_range_ctl'
!
      character(len=kchara), parameter, private                         &
     &            :: hd_monitor_data_list = 'monitor_data_list_ctl'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_file_sph_monitor(my_rank, file_name,      &
     &                                         tave_sph_ctl)
!
      use skip_comment_f
!
      integer, intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
      type(tave_sph_monitor_ctl), intent(inout) :: tave_sph_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        open(id_control, file = file_name, status='old')
        do
          call load_one_line_from_control                               &
     &       (id_control, hd_tave_spectr, c_buf1)
          if(c_buf1%iend .gt. 0) exit
!
          call read_ctl_tave_sph_monitor                                &
     &       (id_control, hd_tave_spectr, tave_sph_ctl, c_buf1)
          if(tave_sph_ctl%i_time_ave_sph .gt. 0) exit
        end do
        close(id_control)
      end if
      if(c_buf1%iend .gt. 0) stop 'control file is broken'
!
      end subroutine read_control_file_sph_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ctl_tave_sph_monitor                              &
     &         (id_control, hd_block, tave_sph_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(tave_sph_monitor_ctl), intent(inout) :: tave_sph_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(tave_sph_ctl%i_time_ave_sph  .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_start_time_ctl, tave_sph_ctl%start_time_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_end_time_ctl, tave_sph_ctl%end_time_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_old_format, tave_sph_ctl%old_format_ctl)
!
        call read_integer2_ctl_type                                     &
     &     (c_buf, hd_degree_range, tave_sph_ctl%degree_range_ctl)
!
        call read_ctl_sph_monitor_list                                  &
     &    (id_control, hd_monitor_data_list,                            &
     &     tave_sph_ctl%monitor_list_ctl, c_buf)
      end do
      tave_sph_ctl%i_time_ave_sph = 1
!
      end subroutine read_ctl_tave_sph_monitor
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ctl_tave_sph_monitor(tave_sph_ctl)
!
      type(tave_sph_monitor_ctl), intent(inout) :: tave_sph_ctl
!
!
      tave_sph_ctl%start_time_ctl%iflag =   0
      tave_sph_ctl%end_time_ctl%iflag =     0
      tave_sph_ctl%old_format_ctl%iflag =   0
      tave_sph_ctl%degree_range_ctl%iflag = 0
!
      call dealloc_ctl_sph_monitor_list(tave_sph_ctl%monitor_list_ctl)
!
      tave_sph_ctl%i_time_ave_sph = 0
!
      end subroutine dealloc_ctl_tave_sph_monitor
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_tave_sph_monitor
