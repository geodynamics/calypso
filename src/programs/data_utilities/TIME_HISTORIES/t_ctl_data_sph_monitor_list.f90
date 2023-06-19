!>@file   t_ctl_data_sph_monitor_list.f90
!!        module t_ctl_data_sph_monitor_list
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine read_ctl_sph_monitor_list                            &
!!     &         (id_control, hd_block, monitor_list_ctl, c_buf)
!!        type(sph_monitor_files_ctl), intent(inout) :: monitor_list_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine dealloc_ctl_sph_monitor_list(monitor_list_ctl)
!!        type(sph_monitor_files_ctl), intent(inout) :: monitor_list_ctl
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin monitor_data_list_ctl
!!    array volume_integrate_prefix
!!      volume_integrate_prefix     'sph_ave_volume'
!!      volume_integrate_prefix     'sph_pwr_volume_s'
!!      volume_integrate_prefix     'sph_pwr_volume_m0'
!!    end array volume_integrate_prefix
!!
!!    array volume_sph_spectr_prefix
!!      volume_sph_spectr_prefix     'sph_pwr_volume_l'
!!      volume_sph_spectr_prefix     'sph_pwr_volume_m'
!!      volume_sph_spectr_prefix     'sph_pwr_volume_lm'
!!    end array volume_sph_spectr_prefix
!!
!!    array sphere_integrate_prefix
!!      sphere_integrate_prefix     'sph_pwr_layer_s'
!!      sphere_integrate_prefix     'sph_pwr_layer_m0'
!!    end array sphere_integrate_prefix
!!
!!    array layer_sph_spectr_prefix
!!      layer_sph_spectr_prefix     'sph_pwr_layer_l'
!!      layer_sph_spectr_prefix     'sph_pwr_layer_m'
!!      layer_sph_spectr_prefix     'sph_pwr_layer_lm'
!!    end array layer_sph_spectr_prefix
!!
!!    array picked_sph_prefix
!!      picked_sph_prefix        'monitor/picked_mode'
!!      picked_sph_prefix        'monitor/picked_mode_l2_m0c'
!!    end array picked_sph_prefix
!!  end monitor_data_list_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_sph_monitor_list
!
      use m_precision
!
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer2
      use t_ctl_data_dimless_numbers
!
      implicit  none
!
      type sph_monitor_files_ctl
!>        file prefix for volume time series
        type(ctl_array_chara) :: volume_series_file_ctl
!>        file prefix for volume spectr time series
        type(ctl_array_chara) :: volume_spec_file_ctl
!
!>        file prefix for layered time series
        type(ctl_array_chara) :: layered_series_file_ctl
!>        file prefix for layered spectr time series
        type(ctl_array_chara) :: layered_spec_file_ctl
!>        Structure for picked spectrum file prefix
        type(ctl_array_chara) :: picked_mode_file_ctl
!
!>        Structure for Nusselt number
        type(read_character_item) :: Nusselt_file_prefix
!>        Structure for dipolarity
        type(read_character_item) :: dipolarity_file_prefix
!>        Structure for Elsasser number
        type(read_character_item) :: Elsasser_file_prefix
!>        Structure for gauss coefficient file prefix
        type(read_character_item) :: gauss_coefs_prefix
!
        integer (kind = kint) :: i_time_ave_sph = 0
      end type sph_monitor_files_ctl
!
!
!    label for entry
!
      character(len=kchara), parameter, private                         &
     &            :: hd_vol_time_series = 'volume_integrate_prefix'
      character(len=kchara), parameter, private                         &
     &            :: hd_vol_spec_series = 'volume_sph_spectr_prefix'
!
      character(len=kchara), parameter, private                         &
     &          :: hd_layer_time_series = 'sphere_integrate_prefix'
      character(len=kchara), parameter, private                         &
     &          :: hd_layer_spec_series = 'layer_sph_spectr_prefix'
!
      character(len=kchara), parameter, private                         &
     &           :: hd_gauss_coefs_head = 'gauss_coefs_prefix'
      character(len=kchara), parameter, private                         &
     &           :: hd_picked_mode_head = 'picked_sph_prefix'
      character(len=kchara), parameter, private                         &
     &           :: hd_Nusselt_file_head = 'nusselt_number_prefix'
      character(len=kchara), parameter, private                         &
     &           :: hd_dipolarity_file_head = 'dipolarity_file_prefix'
!
      character(len=kchara), parameter, private                         &
     &           :: hd_Elsasser_file_head = 'elsasser_numbers_prefix'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_ctl_sph_monitor_list                              &
     &         (id_control, hd_block, monitor_list_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(sph_monitor_files_ctl), intent(inout) :: monitor_list_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(monitor_list_ctl%i_time_ave_sph  .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_Nusselt_file_head,           &
     &      monitor_list_ctl%Nusselt_file_prefix)
        call read_chara_ctl_type(c_buf, hd_dipolarity_file_head,        &
     &      monitor_list_ctl%dipolarity_file_prefix)
        call read_chara_ctl_type(c_buf, hd_Elsasser_file_head,          &
     &      monitor_list_ctl%Elsasser_file_prefix)
!
        call read_control_array_c1(id_control,  hd_vol_time_series,     &
     &      monitor_list_ctl%volume_series_file_ctl, c_buf)
        call read_control_array_c1(id_control, hd_vol_spec_series,      &
     &      monitor_list_ctl%volume_spec_file_ctl, c_buf)
!
        call read_control_array_c1(id_control,  hd_layer_time_series,   &
     &      monitor_list_ctl%layered_series_file_ctl, c_buf)
        call read_control_array_c1(id_control, hd_layer_spec_series,    &
     &      monitor_list_ctl%layered_spec_file_ctl, c_buf)
!
        call read_control_array_c1(id_control, hd_picked_mode_head,     &
     &      monitor_list_ctl%picked_mode_file_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_gauss_coefs_head,            &
     &      monitor_list_ctl%gauss_coefs_prefix)
      end do
      monitor_list_ctl%i_time_ave_sph = 1
!
      end subroutine read_ctl_sph_monitor_list
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ctl_sph_monitor_list(monitor_list_ctl)
!
      type(sph_monitor_files_ctl), intent(inout) :: monitor_list_ctl
!
!
      call dealloc_control_array_chara                                  &
     &   (monitor_list_ctl%volume_spec_file_ctl)
      call dealloc_control_array_chara                                  &
     &   (monitor_list_ctl%volume_series_file_ctl)
!
      call dealloc_control_array_chara                                  &
     &   (monitor_list_ctl%layered_spec_file_ctl)
      call dealloc_control_array_chara                                  &
     &   (monitor_list_ctl%layered_series_file_ctl)
!
      call dealloc_control_array_chara                                  &
     &   (monitor_list_ctl%picked_mode_file_ctl)
!
      monitor_list_ctl%Nusselt_file_prefix%iflag =  0
      monitor_list_ctl%dipolarity_file_prefix%iflag = 0
      monitor_list_ctl%Elsasser_file_prefix%iflag = 0
      monitor_list_ctl%gauss_coefs_prefix%iflag =   0
!
      monitor_list_ctl%i_time_ave_sph = 0
!
      end subroutine dealloc_ctl_sph_monitor_list
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_sph_monitor_list
