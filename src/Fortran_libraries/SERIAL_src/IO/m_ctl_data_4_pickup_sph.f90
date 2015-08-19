!>@file   m_ctl_data_4_pickup_sph.f90
!!        module m_ctl_data_4_pickup_sph
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine deallocate_num_spec_layer_ctl
!!      subroutine deallocate_num_pick_layer_ctl
!!
!!      subroutine deallocate_pick_sph_ctl
!!      subroutine deallocate_pick_sph_l_ctl
!!      subroutine deallocate_pick_sph_m_ctl
!!
!!      subroutine deallocate_pick_gauss_ctl
!!      subroutine deallocate_pick_gauss_l_ctl
!!      subroutine deallocate_pick_gauss_m_ctl
!!
!!      subroutine read_pickup_sph_ctl
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin sph_monitor_ctl
!!    volume_average_prefix        'sph_ave_volume'
!!    volume_pwr_spectr_prefix     'sph_pwr_volume'
!!    layered_pwr_spectr_prefix    'sph_pwr_layer'
!!
!!    degree_spectr_switch         'On'
!!    order_spectr_switch          'On'
!!    diff_lm_spectr_switch        'On'
!!
!!    nusselt_number_prefix        'Nusselt'
!!    picked_sph_prefix            'sph_spectr/picked_mode'
!!    gauss_coefs_prefix           'sph_spectr/gauss_coefs'
!!
!!    gauss_coefs_radius_ctl        2.91
!!
!!   if num_pick_layer_ctl = 0 or negative: 
!!           No output
!!    array spectr_layer_ctl  1
!!      spectr_layer_ctl  62
!!    end array spectr_layer_ctl
!!   if pick_layer_ctl = 0 or negative:
!!           output all layer and volume average
!!    array pick_layer_ctl  1
!!      pick_layer_ctl  62
!!    end array
!!
!!    array pick_sph_spectr_ctl  2
!!      pick_sph_spectr_ctl   2  -2
!!      pick_sph_spectr_ctl   2   2
!!    end array pick_sph_spectr_ctl
!!
!!    array pick_sph_degree_ctl  2
!!      pick_sph_degree_ctl   2
!!      pick_sph_degree_ctl   2
!!    end array pick_sph_degree_ctl
!!
!!    array pick_sph_order_ctl  2
!!      pick_sph_order_ctl  -2
!!      pick_sph_order_ctl   2
!!    end array pick_sph_order_ctl
!!
!!
!!    array pick_gauss_coefs_ctl  2
!!      pick_gauss_coefs_ctl   2  -2
!!      pick_gauss_coefs_ctl   2   2
!!    end array pick_gauss_coefs_ctl
!!
!!    array pick_gauss_coef_degree_ctl  2
!!      pick_gauss_coef_degree_ctl   2
!!      pick_gauss_coef_degree_ctl   2
!!    end array pick_gauss_coef_degree_ctl
!!
!!    array pick_gauss_coef_order_ctl  2
!!      pick_gauss_coef_order_ctl   -2
!!      pick_gauss_coef_order_ctl    2
!!    end array pick_gauss_coef_order_ctl
!!
!!    pick_circle_coord_ctl         spherical
!!    nphi_mid_eq_ctl               500
!!    pick_cylindrical_radius_ctl   0.75
!!    pick_vertical_position_ctl    0.6
!!  end sph_monitor_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module m_ctl_data_4_pickup_sph
!
      use m_precision
!
      use t_control_elements
      use t_read_control_arrays
      use skip_comment_f
!
      implicit  none
!
!>      Structure for layered spectrum file prefix
      type(read_character_item), save :: volume_average_prefix
!
!>      Structure for layered spectrum file prefix
      type(read_character_item), save :: volume_pwr_spectr_prefix
!
!>      Structure for layered spectrum file prefix
      type(read_character_item), save :: layered_pwr_spectr_prefix
!
!>      Structure for picked spectrum file prefix
      type(read_character_item), save :: Nusselt_file_prefix
!
!>      Structure for picked spectrum file prefix
      type(read_character_item), save :: picked_mode_head_ctl
!
!>      Structure for gauss coefficient file prefix
      type(read_character_item), save :: gauss_coefs_prefix
!
!>      Structure for reference radus 
      type(read_real_item), save :: gauss_coefs_radius_ctl
!
!
!>      Structure for degree spectrum switch
      type(read_character_item), save :: degree_spectr_switch
!
!>      Structure for order spectrum switch
      type(read_character_item), save :: order_spectr_switch
!
!>      Structure for l-m spectrum switch
      type(read_character_item), save :: diff_lm_spectr_switch
!
!
!>      Structure for list of radial grid of spectr energy data output
!!@n      idx_spec_layer_ctl%num:   Number of grid
!!@n      idx_spec_layer_ctl%ivec: list of radial ID of spectr data
       type(ctl_array_int), save :: idx_spec_layer_ctl
!>      Structure for list of radial grid of spectr data output
!!@n      idx_pick_layer_ctl%num:   Number of grid
!!@n      idx_pick_layer_ctl%ivec: list of radial ID of spectr data
      type(ctl_array_int), save :: idx_pick_layer_ctl
!
!>      Structure for list of mode of spectr data output
!!@n      idx_pick_sph_l_ctl%num:   Number of mode
!!@n      idx_pick_sph_l_ctl%int1: list of degree of spectr data
!!@n      idx_pick_sph_l_ctl%int2: list of order of spectr data
      type(ctl_array_i2), save :: idx_pick_sph_ctl
!
!>      Structure for list of degree of spectr data output
!!@n      idx_pick_sph_l_ctl%num:   Number of degree
!!@n      idx_pick_sph_l_ctl%ivec: list of degree of spectr data
      type(ctl_array_int), save :: idx_pick_sph_l_ctl
!
!>      Structure for list of order of spectr data output
!!@n      idx_pick_sph_m_ctl%num:   Number of order
!!@n      idx_pick_sph_m_ctl%ivec: list of order of spectr data
      type(ctl_array_int), save :: idx_pick_sph_m_ctl
!
!
!>      Structure for list of mode of Gauss coefficients output
!!@n      idx_gauss_ctl%num:   Number of mode
!!@n      idx_gauss_ctl%int1: list of degree of Gauss coefficients
!!@n      idx_gauss_ctl%int2: list of order of Gauss coefficients
      type(ctl_array_i2), save :: idx_gauss_ctl
!
!>      Structure for list of degree of Gauss coefficient output
!!@n      idx_gauss_l_ctl%num:   Number of degree
!!@n      idx_gauss_l_ctl%ivec: list of degree of gauss coefficient
      type(ctl_array_int), save :: idx_gauss_l_ctl
!
!>      Structure for list of order of Gauss coefficient output
!!@n      idx_gauss_m_ctl%num:   Number of order
!!@n      idx_gauss_m_ctl%ivec: list of order of gauss coefficient
      type(ctl_array_int), save :: idx_gauss_m_ctl
!
!
!>      Structure for coordiniate system for circled data
      type(read_character_item), save :: pick_circle_coord_ctl
!
!>      Structure for Number of zonal points for benchamek check
      type(read_integer_item), save :: nphi_mid_eq_ctl
!
!>      Structure for position for s
      type(read_real_item), save :: pick_s_ctl
!
!>      Structure for position for z
      type(read_real_item), save :: pick_z_ctl
!
!    label for entry
!
      character(len=kchara), parameter                                  &
     &                     :: hd_pick_sph = 'sph_monitor_ctl'
      integer(kind = kint) :: i_pick_sph = 0
!
!   labels for item
!
      character(len=kchara), parameter                                  &
     &           :: hd_voume_ave_head = 'volume_average_prefix'
      character(len=kchara), parameter                                  &
     &           :: hd_voume_rms_head = 'volume_pwr_spectr_prefix'
      character(len=kchara), parameter                                  &
     &           :: hd_layer_rms_head = 'layered_pwr_spectr_prefix'
      character(len=kchara), parameter                                  &
     &           :: hd_picked_mode_head = 'picked_sph_prefix'
      character(len=kchara), parameter                                  &
     &           :: hd_gauss_coefs_head = 'gauss_coefs_prefix'
      character(len=kchara), parameter                                  &
     &           :: hd_Nusselt_file_head = 'nusselt_number_prefix'
!
      character(len=kchara), parameter                                  &
     &           :: hd_degree_spectr_switch = 'degree_spectr_switch'
      character(len=kchara), parameter                                  &
     &           :: hd_order_spectr_switch = 'order_spectr_switch'
      character(len=kchara), parameter                                  &
     &           :: hd_diff_lm_spectr_switch = 'diff_lm_spectr_switch'
!
      character(len=kchara), parameter                                  &
     &           :: hd_gauss_coefs_r =    'gauss_coefs_radius_ctl'
!
      character(len=kchara), parameter                                  &
     &           :: hd_spctr_layer = 'spectr_layer_ctl'
      character(len=kchara), parameter                                  &
     &           :: hd_pick_layer =  'pick_layer_ctl'
!
      character(len=kchara), parameter                                  &
     &            :: hd_pick_sph_lm =   'pick_sph_spectr_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_sph_l =     'pick_sph_degree_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_sph_m =     'pick_sph_order_ctl'
!
      character(len=kchara), parameter                                  &
     &            :: hd_pick_gauss_lm =   'pick_gauss_coefs_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_gauss_l = 'pick_gauss_coef_degree_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_gauss_m = 'pick_gauss_coef_order_ctl'
!
      character(len=kchara), parameter                                  &
     &            :: hd_nphi_mid_eq = 'nphi_mid_eq_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_s_ctl = 'pick_cylindrical_radius_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_z_ctl =  'pick_vertical_position_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_circle_coord = 'pick_circle_coord_ctl'
!
!
      private :: hd_pick_sph, i_pick_sph, hd_pick_layer, hd_spctr_layer
      private :: hd_gauss_coefs_head, hd_gauss_coefs_r
      private :: hd_picked_mode_head, hd_Nusselt_file_head
      private :: hd_pick_sph_lm, hd_pick_gauss_lm
      private :: hd_pick_sph_l, hd_pick_sph_m
      private :: hd_pick_gauss_l, hd_pick_gauss_m
      private :: hd_voume_ave_head, hd_voume_rms_head
      private :: hd_layer_rms_head, hd_nphi_mid_eq
      private :: hd_pick_s_ctl, hd_pick_z_ctl, hd_diff_lm_spectr_switch
      private :: hd_degree_spectr_switch, hd_order_spectr_switch
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_spec_layer_ctl
!
      call dealloc_control_array_int(idx_spec_layer_ctl)
!
      end subroutine deallocate_num_spec_layer_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_pick_layer_ctl
!
      call dealloc_control_array_int(idx_pick_layer_ctl)
!
      end subroutine deallocate_num_pick_layer_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_sph_ctl
!
      call dealloc_control_array_i2(idx_pick_sph_ctl)
!
      end subroutine deallocate_pick_sph_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_sph_l_ctl
!
      call dealloc_control_array_int(idx_pick_sph_l_ctl)
!
      end subroutine deallocate_pick_sph_l_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_sph_m_ctl
!
      call dealloc_control_array_int(idx_pick_sph_m_ctl)
!
      end subroutine deallocate_pick_sph_m_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_gauss_ctl
!
      call dealloc_control_array_i2(idx_gauss_ctl)
!
      end subroutine deallocate_pick_gauss_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_gauss_l_ctl
!
      call dealloc_control_array_int(idx_gauss_l_ctl)
!
      end subroutine deallocate_pick_gauss_l_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_gauss_m_ctl
!
      call dealloc_control_array_int(idx_gauss_m_ctl)
!
      end subroutine deallocate_pick_gauss_m_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_pickup_sph_ctl
!
!
      if(right_begin_flag(hd_pick_sph) .eq. 0) return
      if (i_pick_sph .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_pick_sph, i_pick_sph)
        if(i_pick_sph .gt. 0) exit
!
!
        call read_control_array_i1(hd_spctr_layer, idx_spec_layer_ctl)
        call read_control_array_i1(hd_pick_layer, idx_pick_layer_ctl)
!
        call read_control_array_i2(hd_pick_sph_lm, idx_pick_sph_ctl)
        call read_control_array_i1(hd_pick_sph_l, idx_pick_sph_l_ctl)
        call read_control_array_i1(hd_pick_sph_m, idx_pick_sph_m_ctl)
!
!
        call read_control_array_i2(hd_pick_gauss_lm, idx_gauss_ctl)
        call read_control_array_i1(hd_pick_gauss_l, idx_gauss_l_ctl)
        call read_control_array_i1(hd_pick_gauss_m, idx_gauss_m_ctl)
!
!
        call read_real_ctl_type(hd_gauss_coefs_r,                       &
     &      gauss_coefs_radius_ctl)
        call read_real_ctl_type(hd_pick_s_ctl, pick_s_ctl)
        call read_real_ctl_type(hd_pick_z_ctl, pick_z_ctl)
!
        call read_integer_ctl_type(hd_nphi_mid_eq, nphi_mid_eq_ctl)
!
        call read_chara_ctl_type(hd_gauss_coefs_head,                   &
     &          gauss_coefs_prefix)
        call read_chara_ctl_type(hd_picked_mode_head,                   &
     &          picked_mode_head_ctl)
!
        call read_chara_ctl_type(hd_Nusselt_file_head,                  &
     &          Nusselt_file_prefix)
!
        call read_chara_ctl_type(hd_voume_ave_head,                     &
     &          volume_average_prefix)
        call read_chara_ctl_type(hd_voume_rms_head,                     &
     &          volume_pwr_spectr_prefix)
        call read_chara_ctl_type(hd_layer_rms_head,                     &
     &          layered_pwr_spectr_prefix)
!
        call read_chara_ctl_type(hd_degree_spectr_switch,               &
     &          degree_spectr_switch)
        call read_chara_ctl_type(hd_order_spectr_switch,                &
     &          order_spectr_switch)
        call read_chara_ctl_type(hd_diff_lm_spectr_switch,              &
     &          diff_lm_spectr_switch)
!
        call read_chara_ctl_type(hd_circle_coord,                       &
     &          pick_circle_coord_ctl)
      end do
!
      end subroutine read_pickup_sph_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_4_pickup_sph
