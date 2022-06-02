!>@file   t_ctl_data_gauss_coefs.f90
!!        module t_ctl_data_gauss_coefs
!!
!! @author H. Matsui
!! @date   Programmed in 2016
!!
!!
!> @brief Control data for spectr data monitoring
!!
!!@verbatim
!!      subroutine dealloc_gauss_spectr_control(g_pwr)
!!      subroutine read_gauss_spectr_ctl                                &
!!     &         (id_control, hd_block, iflag, g_pwr, c_buf)
!!        type(gauss_spectr_control), intent(inout) :: g_pwr
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spectr data
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
      module t_ctl_data_gauss_coefs
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
      type gauss_spectr_control
!>        Structure for gauss coefficient file prefix
        type(read_character_item) :: gauss_coefs_prefix
!
!>        Structure for reference radus 
        type(read_real_item) :: gauss_coefs_radius_ctl
!
!>        Structure for list of mode of Gauss coefficients output
!!@n        idx_gauss_ctl%num:   Number of mode
!!@n        idx_gauss_ctl%int1: list of degree of Gauss coefficients
!!@n        idx_gauss_ctl%int2: list of order of Gauss coefficients
        type(ctl_array_i2) :: idx_gauss_ctl
!
!>        Structure for list of degree of Gauss coefficient output
!!@n        idx_gauss_l_ctl%num:   Number of degree
!!@n        idx_gauss_l_ctl%ivec: list of degree of gauss coefficient
        type(ctl_array_int) :: idx_gauss_l_ctl
!
!>        Structure for list of order of Gauss coefficient output
!!@n        idx_gauss_m_ctl%num:   Number of order
!!@n        idx_gauss_m_ctl%ivec: list of order of gauss coefficient
        type(ctl_array_int) :: idx_gauss_m_ctl
!
        integer (kind = kint) :: i_gauss_coef_ctl = 0
      end type gauss_spectr_control
!
!
!   labels for item
!
      character(len=kchara), parameter, private                         &
     &           :: hd_gauss_coefs_head = 'gauss_coefs_prefix'
      character(len=kchara), parameter, private                         &
     &           :: hd_gauss_coefs_r =    'gauss_coefs_radius_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_pick_gauss_lm =   'pick_gauss_coefs_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_pick_gauss_l = 'pick_gauss_coef_degree_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_pick_gauss_m = 'pick_gauss_coef_order_ctl'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_gauss_spectr_control(g_pwr)
!
      type(gauss_spectr_control), intent(inout) :: g_pwr
!
!
      call dealloc_control_array_i2(g_pwr%idx_gauss_ctl)
      call dealloc_control_array_int(g_pwr%idx_gauss_l_ctl)
      call dealloc_control_array_int(g_pwr%idx_gauss_m_ctl)
!
      g_pwr%gauss_coefs_radius_ctl%iflag = 0
      g_pwr%gauss_coefs_prefix%iflag =     0
      g_pwr%i_gauss_coef_ctl = 0
!
      end subroutine dealloc_gauss_spectr_control
!
! -----------------------------------------------------------------------
!
      subroutine read_gauss_spectr_ctl                                  &
     &         (id_control, hd_block, g_pwr, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(gauss_spectr_control), intent(inout) :: g_pwr
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(g_pwr%i_gauss_coef_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_array_i2(id_control,                          &
     &      hd_pick_gauss_lm, g_pwr%idx_gauss_ctl, c_buf)
        call read_control_array_i1(id_control,                          &
     &      hd_pick_gauss_l, g_pwr%idx_gauss_l_ctl, c_buf)
        call read_control_array_i1(id_control,                          &
     &      hd_pick_gauss_m, g_pwr%idx_gauss_m_ctl, c_buf)
!
        call read_real_ctl_type(c_buf, hd_gauss_coefs_r,                &
     &      g_pwr%gauss_coefs_radius_ctl)
        call read_chara_ctl_type(c_buf, hd_gauss_coefs_head,            &
     &      g_pwr%gauss_coefs_prefix)
      end do
      g_pwr%i_gauss_coef_ctl = 1
!
      end subroutine read_gauss_spectr_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_gauss_coefs
