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
!!      subroutine read_control_file_sph_monitor(my_rank, tave_sph_ctl)
!!      subroutine read_control_file_sph_elsasser(my_rank, tave_sph_ctl)
!!      subroutine read_ctl_tave_sph_monitor                            &
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
!!    array vol_integrate_prefix
!!      vol_integrate_prefix     'sph_ave_volume'
!!      vol_integrate_prefix     'sph_pwr_volume_s'
!!      vol_integrate_prefix     'sph_pwr_volume_m0'
!!    end array vol_integrate_prefix
!!
!!    array vol_spectr_prefix
!!      vol_spectr_prefix     'sph_pwr_volume_l'
!!      vol_spectr_prefix     'sph_pwr_volume_m'
!!      vol_spectr_prefix     'sph_pwr_volume_lm'
!!    end array vol_spectr_prefix
!!
!!    array sph_integrate_prefix
!!      sph_integrate_prefix     'sph_pwr_layer_s'
!!      sph_integrate_prefix     'sph_pwr_layer_m0'
!!    end array sph_integrate_prefix
!!
!!    array layer_sph_spectr_prefix
!!      layer_sph_spectr_prefix     'sph_pwr_layer_l'
!!      layer_sph_spectr_prefix     'sph_pwr_layer_m'
!!      layer_sph_spectr_prefix     'sph_pwr_layer_lm'
!!    end array layer_sph_spectr_prefix
!!
!!    gauss_coefs_prefix           'sph_spectr/gauss_coefs'
!!    picked_sph_prefix            'sph_spectr/picked_mode'
!!    nusselt_number_prefix        'Nusselt'
!!    dipolarity_file_prefix       'dipolarity'
!!  end time_averaging_sph_monitor
!!
!! -----------------------------------------------------------------
!!
!!  begin sph_elsasser_numbers
!!    start_time_ctl     2.0
!!    end_time_ctl       2.5
!!  
!!    vol_degree_spectr_prefix     'sph_pwr_volume_l'
!!    vol_order_spectr_prefix      'sph_pwr_volume_m'
!!    old_format_flag     'Off'
!!    elsasser_numbers_prefix      'Elsasser'
!!
!!    begin dimensionless_ctl
!!      array dimless_ctl
!!        dimless_ctl  Prandtl_number                   1.0e-0
!!        dimless_ctl  modified_Rayleigh_number         1.0E+2
!!        dimless_ctl  Ekman_number                     1.0e-3
!!        dimless_ctl  magnetic_Prandtl_number          5.0e+0
!!        dimless_ctl  Composite_Rayleigh_number        1.0E+2
!!        dimless_ctl  Composite_Prandtl_number         1.0E+2
!!      end array dimless_ctl
!!    end  dimensionless_ctl
!!
!!    array mag_to_kin_energy_ratio
!!      mag_to_kin_energy_ratio   Ekman              -1.0
!!      mag_to_kin_energy_ratio   magnetic_Prandtl   -1.0
!!    end array mag_to_kin_energy_ratio
!!    array magnetic_Reynolds_ratio
!!      magnetic_Reynolds_ratio   magnetic_Prandtl   1.0
!!    end array magnetic_Reynolds_ratio
!!    array Coefs_for_Elsasser
!!      Coefs_for_Elsasser   Ekman               1.0
!!      Coefs_for_Elsasser   magnetic_Prandtl    1.0
!!    end array Coefs_for_Elsasser
!!
!!  end sph_elsasser_numbers
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
      use t_ctl_data_dimless_numbers
!
      implicit  none
!
!>        Control file name
      character(len = kchara), parameter, private                       &
     &           :: fname_ctl_tave_sph_mtr = 'control_sph_time_average'
!>        Control file name
      character(len = kchara), parameter, private                       &
     &           :: fname_ctl_sph_elsasser = 'control_sph_elsasser'
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
!
!>        file prefix for volume time series
        type(ctl_array_chara) :: volume_series_file_ctl
!>        file prefix for volume spectr time series
        type(ctl_array_chara) :: volume_spec_file_ctl
!
!>        file prefix for layered time series
        type(ctl_array_chara) :: layered_series_file_ctl
!>        file prefix for layered spectr time series
        type(ctl_array_chara) :: layered_spec_file_ctl
!
!>        Structure for Nusselt number
        type(read_character_item) :: Nusselt_file_prefix
!>        Structure for dipolarity
        type(read_character_item) :: dipolarity_file_prefix
!>        Structure for Elsasser number
        type(read_character_item) :: Elsasser_file_prefix
!>        Structure for gauss coefficient file prefix
        type(read_character_item) :: gauss_coefs_prefix
!>        Structure for picked spectrum file prefix
        type(read_character_item) :: picked_mode_head_ctl
!
!>        Structure for Elsasser number
        type(read_character_item) :: vol_degree_spectr_prefix
!>        Structure for Elsasser number
        type(read_character_item) :: vol_order_spectr_prefix
!
!>        Structure for list of dimensionless numbers
        type(dimless_control) :: dless_ctl
!
!>        Ratio of Magnetic energy to kinetic energy
        type(ctl_array_cr) :: ME_to_KE_ctl
!>        Coefficients to construct magnetic Reynolds number
        type(ctl_array_cr) :: mag_Re_coefs
!>        Coefficients to construct Elsasser number
        type(ctl_array_cr) :: Elsasser_coefs
!
        integer (kind = kint) :: i_time_ave_sph = 0
      end type tave_sph_monitor_ctl
!
!
!    label for entry
!
      character(len=kchara), parameter, private                         &
     &             :: hd_tave_spectr =  'time_averaging_sph_monitor'
      character(len=kchara), parameter, private                         &
     &             :: hd_gen_elsasser = 'sph_elsasser_numbers'
!
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
     &            :: hd_vol_time_series = 'vol_integrate_prefix'
      character(len=kchara), parameter, private                         &
     &            :: hd_vol_spec_series = 'vol_spectr_prefix'
!
      character(len=kchara), parameter, private                         &
     &          :: hd_layer_time_series = 'sph_integrate_prefix'
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
     &           :: hd_vol_spec_l_head = 'vol_degree_spectr_prefix'
      character(len=kchara), parameter, private                         &
     &           :: hd_vol_spec_m_head = 'vol_order_spectr_prefix'
      character(len=kchara), parameter, private                         &
     &           :: hd_Elsasser_file_head = 'elsasser_numbers_prefix'
      character(len=kchara), parameter, private                         &
     &           :: hd_dimless_list = 'dimensionless_ctl'
!
      character(len=kchara), parameter, private                         &
     &        :: hd_mag_to_kin_ratio = 'mag_to_kin_energy_ratio'
      character(len=kchara), parameter, private                         &
     &        :: hd_mag_reynolds_coefs = 'magnetic_Reynolds_ratio'
      character(len=kchara), parameter, private                         &
     &        :: hd_elsasser_coefs =   'Coefs_for_Elsasser'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_file_sph_monitor(my_rank, tave_sph_ctl)
!
      use skip_comment_f
!
      integer, intent(in) :: my_rank
      type(tave_sph_monitor_ctl), intent(inout) :: tave_sph_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(id_control, file = fname_ctl_tave_sph_mtr, status='old')
        do
          call load_one_line_from_control(id_control, c_buf1)
          call read_ctl_tave_sph_monitor                                &
     &       (id_control, hd_tave_spectr, tave_sph_ctl, c_buf1)
          if(tave_sph_ctl%i_time_ave_sph .gt. 0) exit
        end do
        close(id_control)
      end if
!
      end subroutine read_control_file_sph_monitor
!
! -----------------------------------------------------------------------
!
      subroutine read_control_file_sph_elsasser(my_rank, tave_sph_ctl)
!
      use skip_comment_f
!
      integer, intent(in) :: my_rank
      type(tave_sph_monitor_ctl), intent(inout) :: tave_sph_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(id_control, file = fname_ctl_sph_elsasser, status='old')
        do
          call load_one_line_from_control(id_control, c_buf1)
          call read_ctl_tave_sph_monitor                                &
     &       (id_control, hd_gen_elsasser, tave_sph_ctl, c_buf1)
          if(tave_sph_ctl%i_time_ave_sph .gt. 0) exit
        end do
        close(id_control)
      end if
!
      end subroutine read_control_file_sph_elsasser
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
        call load_one_line_from_control(id_control, c_buf)
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
        call read_chara_ctl_type(c_buf, hd_Nusselt_file_head,           &
     &      tave_sph_ctl%Nusselt_file_prefix)
        call read_chara_ctl_type(c_buf, hd_dipolarity_file_head,        &
     &      tave_sph_ctl%dipolarity_file_prefix)
        call read_chara_ctl_type(c_buf, hd_Elsasser_file_head,          &
     &      tave_sph_ctl%Elsasser_file_prefix)
!
        call read_chara_ctl_type(c_buf, hd_vol_spec_l_head,             &
     &      tave_sph_ctl%vol_degree_spectr_prefix)
        call read_chara_ctl_type(c_buf, hd_vol_spec_m_head,             &
     &      tave_sph_ctl%vol_order_spectr_prefix)
!
        call read_control_array_c1(id_control,  hd_vol_time_series,     &
     &      tave_sph_ctl%volume_series_file_ctl, c_buf)
        call read_control_array_c1(id_control, hd_vol_spec_series,      &
     &      tave_sph_ctl%volume_spec_file_ctl, c_buf)
!
        call read_control_array_c1(id_control,  hd_layer_time_series,   &
     &      tave_sph_ctl%layered_series_file_ctl, c_buf)
        call read_control_array_c1(id_control, hd_layer_spec_series,    &
     &      tave_sph_ctl%layered_spec_file_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_gauss_coefs_head,            &
     &      tave_sph_ctl%gauss_coefs_prefix)
        call read_chara_ctl_type(c_buf, hd_picked_mode_head,            &
     &      tave_sph_ctl%picked_mode_head_ctl)
!
        call read_dimless_ctl                                           &
     &     (id_control, hd_dimless_list, tave_sph_ctl%dless_ctl, c_buf)
!
        call read_control_array_c_r(id_control, hd_mag_to_kin_ratio,    &
     &      tave_sph_ctl%ME_to_KE_ctl, c_buf)
        call read_control_array_c_r(id_control, hd_mag_reynolds_coefs,  &
     &      tave_sph_ctl%mag_Re_coefs, c_buf)
        call read_control_array_c_r(id_control, hd_elsasser_coefs,      &
     &      tave_sph_ctl%Elsasser_coefs, c_buf)
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
      call dealloc_dimless_ctl(tave_sph_ctl%dless_ctl)
!
      call dealloc_control_array_chara                                  &
     &   (tave_sph_ctl%volume_spec_file_ctl)
      call dealloc_control_array_chara                                  &
     &   (tave_sph_ctl%volume_series_file_ctl)
!
      call dealloc_control_array_chara                                  &
     &   (tave_sph_ctl%layered_spec_file_ctl)
      call dealloc_control_array_chara                                  &
     &   (tave_sph_ctl%layered_series_file_ctl)
!
      tave_sph_ctl%Nusselt_file_prefix%iflag =  0
      tave_sph_ctl%dipolarity_file_prefix%iflag = 0
      tave_sph_ctl%Elsasser_file_prefix%iflag = 0
      tave_sph_ctl%gauss_coefs_prefix%iflag =   0
      tave_sph_ctl%picked_mode_head_ctl%iflag = 0
!
      tave_sph_ctl%vol_degree_spectr_prefix%iflag = 0
      tave_sph_ctl%vol_order_spectr_prefix%iflag =  0
!
      call dealloc_control_array_c_r(tave_sph_ctl%ME_to_KE_ctl)
      call dealloc_control_array_c_r(tave_sph_ctl%mag_Re_coefs)
      call dealloc_control_array_c_r(tave_sph_ctl%Elsasser_coefs)
!
      tave_sph_ctl%i_time_ave_sph = 0
!
      end subroutine dealloc_ctl_tave_sph_monitor
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_tave_sph_monitor
