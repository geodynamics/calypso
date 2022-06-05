!>@file   t_ctl_data_pick_sph_spectr.f90
!!        module t_ctl_data_pick_sph_spectr
!!
!! @author H. Matsui
!! @date   Programmed in 2016
!!
!!
!> @brief Control data for spectr data monitoring
!!
!!@verbatim
!!      subroutine dealloc_pick_spectr_control(pspec_ctl)
!!      subroutine read_pickup_spectr_ctl                               &
!!     &         (id_control, hd_block, pspec_ctl, c_buf)
!!        type(pick_spectr_control), intent(inout) :: pspec_ctl
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spectr data
!!
!!  begin pickup_spectr_ctl
!!    picked_sph_prefix            'sph_spectr/picked_mode'
!!!
!!!     if pick_layer_ctl = 0 or negative:
!!!           output all layer and volume average
!!    array pick_layer_ctl  1
!!      pick_layer_ctl  62
!!    end array pick_layer_ctl
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
!!  end pickup_spectr_ctl
!!
!!
!!    begin gauss_coefficient_ctl
!!      gauss_coefs_prefix           'sph_spectr/gauss_coefs'
!!      gauss_coefs_radius_ctl    2.82
!!
!!      array pick_gauss_coefs_ctl  2
!!        pick_gauss_coefs_ctl   2  -2
!!        pick_gauss_coefs_ctl   2   2
!!      end array pick_gauss_coefs_ctl
!!
!!      array pick_gauss_coef_degree_ctl  2
!!        pick_gauss_coef_degree_ctl   2
!!        pick_gauss_coef_degree_ctl   2
!!      end array pick_gauss_coef_degree_ctl
!!
!!      array pick_gauss_coef_order_ctl  2
!!        pick_gauss_coef_order_ctl   -2
!!        pick_gauss_coef_order_ctl    2
!!      end array pick_gauss_coef_order_ctl
!!    end   gauss_coefficient_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_pick_sph_spectr
!
      use m_precision
!
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_control_array_integer2
      use skip_comment_f
!
      implicit  none
!
!
!>      Structure for spectr data pickup
      type pick_spectr_control
!>        Structure for picked spectrum file prefix
        type(read_character_item) :: picked_mode_head_ctl
!
!>        Structure for list of radial grid of spectr data output
!!@n        idx_pick_layer_ctl%num:   Number of grid
!!@n        idx_pick_layer_ctl%ivec: list of radial ID of spectr data
        type(ctl_array_int) :: idx_pick_layer_ctl
!
!>        Structure for list of mode of spectr data output
!!@n        idx_pick_sph_ctl%num:   Number of mode
!!@n        idx_pick_sph_ctl%int1: list of degree of spectr data
!!@n        idx_pick_sph_ctl%int2: list of order of spectr data
        type(ctl_array_i2) :: idx_pick_sph_ctl
!
!>        Structure for list of degree of spectr data output
!!@n        idx_pick_sph_l_ctl%num:   Number of degree
!!@n        idx_pick_sph_l_ctl%ivec: list of degree of spectr data
        type(ctl_array_int) :: idx_pick_sph_l_ctl
!
!>        Structure for list of order of spectr data output
!!@n        idx_pick_sph_m_ctl%num:   Number of order
!!@n        idx_pick_sph_m_ctl%ivec: list of order of spectr data
        type(ctl_array_int) :: idx_pick_sph_m_ctl
!
        integer (kind = kint) :: i_pick_sph = 0
      end type pick_spectr_control
!
!   labels for item
!
      character(len=kchara), parameter, private                         &
     &           :: hd_picked_mode_head = 'picked_sph_prefix'
!
      character(len=kchara), parameter, private                         &
     &           :: hd_pick_layer =  'pick_layer_ctl'
!
      character(len=kchara), parameter, private                         &
     &            :: hd_pick_sph_lm =   'pick_sph_spectr_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_pick_sph_l =     'pick_sph_degree_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_pick_sph_m =     'pick_sph_order_ctl'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_spectr_control(pspec_ctl)
!
      type(pick_spectr_control), intent(inout) :: pspec_ctl
!
!
      call dealloc_control_array_i2(pspec_ctl%idx_pick_sph_ctl)
      call dealloc_control_array_int(pspec_ctl%idx_pick_sph_m_ctl)
      call dealloc_control_array_int(pspec_ctl%idx_pick_sph_l_ctl)
      call dealloc_control_array_int(pspec_ctl%idx_pick_layer_ctl)
!
      pspec_ctl%picked_mode_head_ctl%iflag = 0
      pspec_ctl%i_pick_sph = 0
!
      end subroutine dealloc_pick_spectr_control
!
! -----------------------------------------------------------------------
!
      subroutine read_pickup_spectr_ctl                                 &
     &         (id_control, hd_block, pspec_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pick_spectr_control), intent(inout) :: pspec_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pspec_ctl%i_pick_sph .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_array_i1(id_control,                          &
     &      hd_pick_layer, pspec_ctl%idx_pick_layer_ctl, c_buf)
!
        call read_control_array_i2(id_control,                          &
     &      hd_pick_sph_lm, pspec_ctl%idx_pick_sph_ctl, c_buf)
        call read_control_array_i1(id_control,                          &
     &      hd_pick_sph_l, pspec_ctl%idx_pick_sph_l_ctl, c_buf)
        call read_control_array_i1(id_control,                          &
     &      hd_pick_sph_m, pspec_ctl%idx_pick_sph_m_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_picked_mode_head,            &
     &      pspec_ctl%picked_mode_head_ctl)
      end do
      pspec_ctl%i_pick_sph = 1
!
      end subroutine read_pickup_spectr_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_pick_sph_spectr