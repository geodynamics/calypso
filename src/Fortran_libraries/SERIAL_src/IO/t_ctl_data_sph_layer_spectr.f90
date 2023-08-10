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
!!      subroutine init_layerd_spectr_ctl_labels(hd_block, lp_ctl)
!!      subroutine read_layerd_spectr_ctl                               &
!!     &         (id_control, hd_block, lp_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(layerd_spectr_control), intent(inout) :: lp_ctl
!!        type(buffer_for_control), intent(inout) :: c_buf
!!      subroutine write_layerd_spectr_ctl(id_control, lp_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        type(layerd_spectr_control), intent(in) :: lp_ctl
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine dealloc_num_spec_layer_ctl(lp_ctl)
!!        type(layerd_spectr_control), intent(inout) :: lp_ctl
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin layered_spectrum_ctl
!!    layered_pwr_spectr_prefix    'sph_pwr_layer'
!!    layered_pwr_spectr_format    'gzip'
!!
!!    degree_spectra_switch         'On'
!!    order_spectra_switch          'On'
!!    diff_lm_spectra_switch        'On'
!!    axisymmetric_power_switch     'On'
!!
!!   if number of spectr_layer_ctl = 0 or negative: No output
!!   if first item of spectr_layer_ctl[1] = -1: Output all layers
!!    array spectr_layer_ctl
!!      spectr_layer_ctl  62
!!    end array spectr_layer_ctl
!!
!!    array spectr_radius_ctl
!!      spectr_radius_ctl  1.0538
!!    end array spectr_radius_ctl
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
!>        Block name
        character(len=kchara) :: block_name = 'layered_spectrum_ctl'
!
!>        Structure for layered spectrum file prefix
        type(read_character_item) :: layered_pwr_spectr_prefix
!>        Structure for layered spectrum file format
        type(read_character_item) :: layered_pwr_spectr_format
!
!>        Structure for degree spectrum switch
        type(read_character_item) :: degree_spectra_switch
!>        Structure for order spectrum switch
        type(read_character_item) :: order_spectra_switch
!>        Structure for l-m spectrum switch
        type(read_character_item) :: diff_lm_spectra_switch
!>        Structure for l-m spectrum switch
        type(read_character_item) :: axis_power_switch
!
!>        Structure for list of radial grid of spectr energy data output
!!@n        idx_spec_layer_ctl%num:   Number of grid
!!@n        idx_spec_layer_ctl%ivec: list of radial ID of spectr data
         type(ctl_array_int) :: idx_spec_layer_ctl
!>        Structure for list of radial grid of spectr energy data output
!!@n        layer_radius_ctl%num:   Number of grid
!!@n        layer_radius_ctl%ivec: list of radius of spectr data
         type(ctl_array_real) :: layer_radius_ctl
!
        integer (kind = kint) :: i_layer_spectr_ctl = 0
      end type layerd_spectr_control
!
!
!   labels for item
!
      character(len=kchara), parameter, private                         &
     &           :: hd_layer_rms_head = 'layered_pwr_spectr_prefix'
      character(len=kchara), parameter, private                         &
     &           :: hd_layer_rms_fmt =  'layered_pwr_spectr_format'
!
      character(len=kchara), parameter, private                         &
     &           :: hd_spctr_layer =  'spectr_layer_ctl'
      character(len=kchara), parameter, private                         &
     &           :: hd_spctr_radius = 'spectr_radius_ctl'
!
      character(len=kchara), parameter, private                         &
     &           :: hd_degree_spectr_switch = 'degree_spectra_switch'
      character(len=kchara), parameter, private                         &
     &           :: hd_order_spectr_switch = 'order_spectra_switch'
      character(len=kchara), parameter, private                         &
     &           :: hd_diff_lm_spectr_switch = 'diff_lm_spectra_switch'
      character(len=kchara), parameter, private                         &
     &           :: hd_axis_spectr_switch = 'axisymmetric_power_switch'
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
      if (lp_ctl%i_layer_spectr_ctl .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_i1(id_control,                          &
     &      hd_spctr_layer, lp_ctl%idx_spec_layer_ctl, c_buf)
        call read_control_array_r1(id_control,                          &
     &      hd_spctr_radius, lp_ctl%layer_radius_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_layer_rms_head,              &
     &      lp_ctl%layered_pwr_spectr_prefix)
        call read_chara_ctl_type(c_buf, hd_layer_rms_fmt,               &
     &      lp_ctl%layered_pwr_spectr_format)
!
        call read_chara_ctl_type(c_buf, hd_degree_spectr_switch,        &
     &      lp_ctl%degree_spectra_switch)
        call read_chara_ctl_type(c_buf, hd_order_spectr_switch,         &
     &      lp_ctl%order_spectra_switch)
        call read_chara_ctl_type(c_buf, hd_diff_lm_spectr_switch,       &
     &      lp_ctl%diff_lm_spectra_switch)
        call read_chara_ctl_type(c_buf, hd_axis_spectr_switch,          &
     &      lp_ctl%axis_power_switch)
      end do
      lp_ctl%i_layer_spectr_ctl = 1
!
      end subroutine read_layerd_spectr_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_layerd_spectr_ctl(id_control, lp_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(layerd_spectr_control), intent(in) :: lp_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(lp_ctl%i_layer_spectr_ctl .le. 0) return
!
      maxlen = len_trim(hd_spctr_layer)
      maxlen = max(maxlen, len_trim(hd_spctr_radius))
      maxlen = max(maxlen, len_trim(hd_layer_rms_head))
      maxlen = max(maxlen, len_trim(hd_layer_rms_fmt))
      maxlen = max(maxlen, len_trim(hd_degree_spectr_switch))
      maxlen = max(maxlen, len_trim(hd_order_spectr_switch))
      maxlen = max(maxlen, len_trim(hd_diff_lm_spectr_switch))
      maxlen = max(maxlen, len_trim(hd_axis_spectr_switch))
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 lp_ctl%block_name)
      call write_control_array_i1(id_control, level,                    &
     &    lp_ctl%idx_spec_layer_ctl)
      call write_control_array_r1(id_control, level,                    &
     &    lp_ctl%layer_radius_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    lp_ctl%layered_pwr_spectr_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    lp_ctl%layered_pwr_spectr_format)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    lp_ctl%degree_spectra_switch)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    lp_ctl%order_spectra_switch)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    lp_ctl%diff_lm_spectra_switch)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    lp_ctl%axis_power_switch)
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                lp_ctl%block_name)
!
      end subroutine write_layerd_spectr_ctl
!
! -----------------------------------------------------------------------
!
      subroutine init_layerd_spectr_ctl_labels(hd_block, lp_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(layerd_spectr_control), intent(inout) :: lp_ctl
!
!
      lp_ctl%block_name = hd_block
        call init_int_ctl_array_label                                   &
     &     (hd_spctr_layer, lp_ctl%idx_spec_layer_ctl)
        call init_real_ctl_array_label                                  &
     &     (hd_spctr_radius, lp_ctl%layer_radius_ctl)
!
        call init_chara_ctl_item_label(hd_layer_rms_head,               &
     &      lp_ctl%layered_pwr_spectr_prefix)
        call init_chara_ctl_item_label(hd_layer_rms_fmt,                &
     &      lp_ctl%layered_pwr_spectr_format)
!
        call init_chara_ctl_item_label(hd_degree_spectr_switch,         &
     &      lp_ctl%degree_spectra_switch)
        call init_chara_ctl_item_label(hd_order_spectr_switch,          &
     &      lp_ctl%order_spectra_switch)
        call init_chara_ctl_item_label(hd_diff_lm_spectr_switch,        &
     &      lp_ctl%diff_lm_spectra_switch)
        call init_chara_ctl_item_label(hd_axis_spectr_switch,           &
     &      lp_ctl%axis_power_switch)
!
      end subroutine init_layerd_spectr_ctl_labels
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_num_spec_layer_ctl(lp_ctl)
!
      type(layerd_spectr_control), intent(inout) :: lp_ctl
!
!
      call dealloc_control_array_int(lp_ctl%idx_spec_layer_ctl)
      call dealloc_control_array_real(lp_ctl%layer_radius_ctl)
!
      lp_ctl%layered_pwr_spectr_prefix%iflag = 0
      lp_ctl%layered_pwr_spectr_format%iflag = 0
      lp_ctl%degree_spectra_switch%iflag =  0
      lp_ctl%order_spectra_switch%iflag =   0
      lp_ctl%diff_lm_spectra_switch%iflag = 0
      lp_ctl%axis_power_switch%iflag =    0
      lp_ctl%i_layer_spectr_ctl = 0
!
      end subroutine dealloc_num_spec_layer_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_sph_layer_spectr
