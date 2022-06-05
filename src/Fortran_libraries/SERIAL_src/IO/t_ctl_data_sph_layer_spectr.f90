!>@file   t_ctl_data_sph_layer_spectr.f90
!!        module t_ctl_data_sph_layer_spectr
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!> @brief control date for volume averaged spectr data
!!
!!@verbatim
!!      subroutine read_layerd_spectr_ctl                               &
!!     &         (id_control, hd_block, lp_ctl, c_buf)
!!      subroutine dealloc_num_spec_layer_ctl(lp_ctl)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(layerd_spectr_control), intent(inout) :: lp_ctl
!!        type(buffer_for_control), intent(inout) :: c_buf
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin layered_spectrum_ctl
!!    layered_pwr_spectr_prefix    'sph_pwr_layer'
!!
!!    degree_spectr_switch         'On'
!!    order_spectr_switch          'On'
!!    diff_lm_spectr_switch        'On'
!!    axisymmetric_spectr_switch   'On'
!!
!!   if number of spectr_layer_ctl = 0 or negative: No output
!!   if first item of spectr_layer_ctl[1] = -1: Output all layers
!!    array spectr_layer_ctl  1
!!      spectr_layer_ctl  62
!!    end array spectr_layer_ctl
!!
!!  end layered_spectrum_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_sph_layer_spectr
!
      use m_precision
!
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use skip_comment_f
!
      implicit  none
!
!
      type layerd_spectr_control
!>        Structure for layered spectrum file prefix
        type(read_character_item) :: layered_pwr_spectr_prefix
!
!>        Structure for degree spectrum switch
        type(read_character_item) :: degree_spectr_switch
!>        Structure for order spectrum switch
        type(read_character_item) :: order_spectr_switch
!>        Structure for l-m spectrum switch
        type(read_character_item) :: diff_lm_spectr_switch
!>        Structure for l-m spectrum switch
        type(read_character_item) :: axis_spectr_switch
!
!>        Structure for list of radial grid of spectr energy data output
!!@n        idx_spec_layer_ctl%num:   Number of grid
!!@n        idx_spec_layer_ctl%ivec: list of radial ID of spectr data
         type(ctl_array_int) :: idx_spec_layer_ctl
!
        integer (kind = kint) :: i_layer_spectr_ctl = 0
      end type layerd_spectr_control
!
!
!   labels for item
!
      character(len=kchara), parameter, private                         &
     &           :: hd_layer_rms_head = 'layered_pwr_spectr_prefix'
!
      character(len=kchara), parameter, private                         &
     &           :: hd_spctr_layer = 'spectr_layer_ctl'
!
      character(len=kchara), parameter, private                         &
     &           :: hd_degree_spectr_switch = 'degree_spectr_switch'
      character(len=kchara), parameter, private                         &
     &           :: hd_order_spectr_switch = 'order_spectr_switch'
      character(len=kchara), parameter, private                         &
     &           :: hd_diff_lm_spectr_switch                            &
     &                              = 'axisymmetric_spectr_switch'
      character(len=kchara), parameter, private                         &
     &           :: hd_axis_spectr_switch = 'diff_lm_spectr_switch'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_layerd_spectr_ctl                                 &
     &         (id_control, hd_block, lp_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(layerd_spectr_control), intent(inout) :: lp_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if (lp_ctl%i_layer_spectr_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_i1(id_control,                          &
     &      hd_spctr_layer, lp_ctl%idx_spec_layer_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_layer_rms_head,              &
     &      lp_ctl%layered_pwr_spectr_prefix)
!
        call read_chara_ctl_type(c_buf, hd_degree_spectr_switch,        &
     &      lp_ctl%degree_spectr_switch)
        call read_chara_ctl_type(c_buf, hd_order_spectr_switch,         &
     &      lp_ctl%order_spectr_switch)
        call read_chara_ctl_type(c_buf, hd_diff_lm_spectr_switch,       &
     &      lp_ctl%diff_lm_spectr_switch)
        call read_chara_ctl_type(c_buf, hd_axis_spectr_switch,          &
     &      lp_ctl%axis_spectr_switch)
      end do
      lp_ctl%i_layer_spectr_ctl = 1
!
      end subroutine read_layerd_spectr_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_num_spec_layer_ctl(lp_ctl)
!
      type(layerd_spectr_control), intent(inout) :: lp_ctl
!
!
      call dealloc_control_array_int(lp_ctl%idx_spec_layer_ctl)
!
      lp_ctl%layered_pwr_spectr_prefix%iflag = 0
      lp_ctl%degree_spectr_switch%iflag =  0
      lp_ctl%order_spectr_switch%iflag =   0
      lp_ctl%diff_lm_spectr_switch%iflag = 0
      lp_ctl%axis_spectr_switch%iflag =    0
      lp_ctl%i_layer_spectr_ctl = 0
!
      end subroutine dealloc_num_spec_layer_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_sph_layer_spectr