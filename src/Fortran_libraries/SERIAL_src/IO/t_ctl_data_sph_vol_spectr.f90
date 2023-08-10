!>@file   t_ctl_data_sph_vol_spectr.f90
!!        module t_ctl_data_sph_vol_spectr
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!> @brief control date for volume averaged spectr data
!!
!!@verbatim
!!      subroutine copy_volume_spectr_control(org_vpwr, new_vpwr)
!!        type(volume_spectr_control), intent(in) :: org_vpwr
!!        type(volume_spectr_control), intent(inout) :: new_vpwr
!!
!!      subroutine init_each_vol_spectr_labels(hd_block, v_pwr)
!!        type(volume_spectr_control), intent(inout) :: v_pwr
!!      subroutine read_each_vol_spectr_ctl                             &
!!     &         (id_control, hd_block, v_pwr, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(volume_spectr_control), intent(inout) :: v_pwr
!!        type(buffer_for_control), intent(inout) :: c_buf
!!      subroutine write_each_vol_spectr_ctl(id_control, v_pwr, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        type(volume_spectr_control), intent(in) :: v_pwr
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine reset_volume_spectr_control(v_pwr)
!!        type(volume_spectr_control), intent(inout) :: v_pwr
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!
!!    array volume_spectrum_ctl
!!      begin volume_spectrum_ctl
!!        volume_average_prefix        'sph_ave_convective'
!!        volume_pwr_spectr_prefix     'sph_pwr_convective'
!!        volume_pwr_spectr_format     'ASCII'
!!        inner_radius_ctl           0.55
!!        outer_radius_ctl           1.4
!!      end volume_spectrum_ctl
!!
!!      begin volume_spectrum_ctl
!!        volume_average_prefix        'sph_ave_inner_core'
!!        volume_pwr_spectr_prefix     'sph_pwr_outer_core'
!!        volume_pwr_spectr_format     'gzip'
!!
!!        degree_spectra_switch         'On'
!!        order_spectra_switch          'On'
!!        diff_lm_spectra_switch        'On'
!!        axisymmetric_power_switch     'On'
!!
!!        inner_radius_ctl           0.0
!!        outer_radius_ctl           0.538
!!      end volume_spectrum_ctl
!!    end array volume_spectrum_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_sph_vol_spectr
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
      type volume_spectr_control
!>        Block name
        character(len=kchara) :: block_name = 'volume_spectrum_ctl'
!>        file name for volume mean square
        type(read_character_item) :: volume_spec_file_ctl
!>        file name for volume average
        type(read_character_item) :: volume_ave_file_ctl
!>        file format for volume mean square
        type(read_character_item) :: volume_spec_format_ctl
!
!>        Structure for degree spectrum switch
        type(read_character_item) :: degree_v_spectra_switch
!>        Structure for order spectrum switch
        type(read_character_item) :: order_v_spectra_switch
!>        Structure for l-m spectrum switch
        type(read_character_item) :: diff_v_lm_spectra_switch
!>        Structure for l-m spectrum switch
        type(read_character_item) :: axis_v_power_switch
!
!>        Structure for inner boundary radius
        type(read_real_item) :: inner_radius_ctl
!>        Structure for outer boundary radius
        type(read_real_item) :: outer_radius_ctl
!
        integer (kind = kint) :: i_vol_spectr_ctl = 0
      end type volume_spectr_control
!
!
!   labels for item
!
      character(len=kchara), parameter, private                         &
     &            :: hd_vol_pwr = 'volume_pwr_spectr_prefix'
      character(len=kchara), parameter, private                         &
     &            :: hd_vol_ave = 'volume_average_prefix'
      character(len=kchara), parameter, private                         &
     &            :: hd_vol_fmt = 'volume_pwr_spectr_format'
      character(len=kchara), parameter, private                         &
     &            :: hd_inner_r = 'inner_radius_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_outer_r = 'outer_radius_ctl'
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
      subroutine copy_volume_spectr_control(org_vpwr, new_vpwr)
!
      type(volume_spectr_control), intent(in) :: org_vpwr
      type(volume_spectr_control), intent(inout) :: new_vpwr
!
!
      call copy_chara_ctl(org_vpwr%volume_spec_file_ctl,                &
     &    new_vpwr%volume_spec_file_ctl)
      call copy_chara_ctl(org_vpwr%volume_ave_file_ctl,                 &
     &    new_vpwr%volume_ave_file_ctl)
      call copy_chara_ctl(org_vpwr%volume_spec_format_ctl,              &
     &    new_vpwr%volume_spec_format_ctl)
!
      call copy_chara_ctl(org_vpwr%degree_v_spectra_switch,             &
     &    new_vpwr%degree_v_spectra_switch)
      call copy_chara_ctl(org_vpwr%order_v_spectra_switch,              &
     &    new_vpwr%order_v_spectra_switch)
      call copy_chara_ctl(org_vpwr%diff_v_lm_spectra_switch,            &
     &    new_vpwr%diff_v_lm_spectra_switch)
      call copy_chara_ctl(org_vpwr%axis_v_power_switch,                 &
     &    new_vpwr%axis_v_power_switch)
!
      call copy_real_ctl(org_vpwr%inner_radius_ctl,                     &
     &    new_vpwr%inner_radius_ctl)
      call copy_real_ctl(org_vpwr%outer_radius_ctl,                     &
     &    new_vpwr%outer_radius_ctl)
!
      new_vpwr%i_vol_spectr_ctl = org_vpwr%i_vol_spectr_ctl
      new_vpwr%block_name = org_vpwr%block_name
!
      end subroutine copy_volume_spectr_control
!
! -----------------------------------------------------------------------
!
      subroutine read_each_vol_spectr_ctl                               &
     &         (id_control, hd_block, v_pwr, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(volume_spectr_control), intent(inout) :: v_pwr
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(v_pwr%i_vol_spectr_ctl .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_vol_pwr,                     &
     &      v_pwr%volume_spec_file_ctl)
        call read_chara_ctl_type(c_buf, hd_vol_fmt,                     &
     &      v_pwr%volume_spec_format_ctl)
!
        call read_chara_ctl_type(c_buf, hd_degree_spectr_switch,        &
     &      v_pwr%degree_v_spectra_switch)
        call read_chara_ctl_type(c_buf, hd_order_spectr_switch,         &
     &      v_pwr%order_v_spectra_switch)
        call read_chara_ctl_type(c_buf, hd_diff_lm_spectr_switch,       &
     &      v_pwr%diff_v_lm_spectra_switch)
        call read_chara_ctl_type(c_buf, hd_axis_spectr_switch,          &
     &      v_pwr%axis_v_power_switch)
!
        call read_chara_ctl_type(c_buf, hd_vol_ave,                     &
     &      v_pwr%volume_ave_file_ctl)
        call read_real_ctl_type(c_buf, hd_inner_r,                      &
     &      v_pwr%inner_radius_ctl)
        call read_real_ctl_type(c_buf, hd_outer_r,                      &
     &      v_pwr%outer_radius_ctl)
      end do
      v_pwr%i_vol_spectr_ctl = 1
!
      end subroutine read_each_vol_spectr_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_each_vol_spectr_ctl(id_control, v_pwr, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(volume_spectr_control), intent(in) :: v_pwr
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(v_pwr%i_vol_spectr_ctl .le. 0) return
!
      maxlen = len_trim(hd_vol_pwr)
      maxlen = max(maxlen, len_trim(hd_vol_fmt))
      maxlen = max(maxlen, len_trim(hd_vol_ave))
      maxlen = max(maxlen, len_trim(hd_inner_r))
      maxlen = max(maxlen, len_trim(hd_outer_r))
      maxlen = max(maxlen, len_trim(hd_degree_spectr_switch))
      maxlen = max(maxlen, len_trim(hd_order_spectr_switch))
      maxlen = max(maxlen, len_trim(hd_diff_lm_spectr_switch))
      maxlen = max(maxlen, len_trim(hd_axis_spectr_switch))
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 v_pwr%block_name)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    v_pwr%volume_spec_file_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    v_pwr%volume_spec_format_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    v_pwr%degree_v_spectra_switch)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    v_pwr%order_v_spectra_switch)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    v_pwr%diff_v_lm_spectra_switch)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    v_pwr%axis_v_power_switch)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    v_pwr%volume_ave_file_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    v_pwr%inner_radius_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    v_pwr%outer_radius_ctl)
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                v_pwr%block_name)
!
      end subroutine write_each_vol_spectr_ctl
!
! -----------------------------------------------------------------------
!
      subroutine init_each_vol_spectr_labels(hd_block, v_pwr)
!
      use write_control_elements
!
      character(len=kchara), intent(in) :: hd_block
      type(volume_spectr_control), intent(inout) :: v_pwr
!
!
      v_pwr%block_name = hd_block
        call init_chara_ctl_item_label(hd_vol_pwr,                      &
     &      v_pwr%volume_spec_file_ctl)
        call init_chara_ctl_item_label(hd_vol_fmt,                      &
     &      v_pwr%volume_spec_format_ctl)
!
        call init_chara_ctl_item_label(hd_degree_spectr_switch,         &
     &      v_pwr%degree_v_spectra_switch)
        call init_chara_ctl_item_label(hd_order_spectr_switch,          &
     &      v_pwr%order_v_spectra_switch)
        call init_chara_ctl_item_label(hd_diff_lm_spectr_switch,        &
     &      v_pwr%diff_v_lm_spectra_switch)
        call init_chara_ctl_item_label(hd_axis_spectr_switch,           &
     &      v_pwr%axis_v_power_switch)
!
        call init_chara_ctl_item_label(hd_vol_ave,                      &
     &      v_pwr%volume_ave_file_ctl)
        call init_real_ctl_item_label(hd_inner_r,                       &
     &      v_pwr%inner_radius_ctl)
        call init_real_ctl_item_label(hd_outer_r,                       &
     &      v_pwr%outer_radius_ctl)
!
      end subroutine init_each_vol_spectr_labels
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine reset_volume_spectr_control(v_pwr)
!
      type(volume_spectr_control), intent(inout) :: v_pwr
!
      v_pwr%volume_spec_file_ctl%iflag =   0
      v_pwr%volume_ave_file_ctl%iflag =    0
      v_pwr%volume_spec_format_ctl%iflag = 0
!
      v_pwr%degree_v_spectra_switch%iflag =  0
      v_pwr%order_v_spectra_switch%iflag =   0
      v_pwr%diff_v_lm_spectra_switch%iflag = 0
      v_pwr%axis_v_power_switch%iflag =      0
!
      v_pwr%inner_radius_ctl%iflag =       0
      v_pwr%outer_radius_ctl%iflag =       0
      v_pwr%i_vol_spectr_ctl = 0
!
      end subroutine reset_volume_spectr_control
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_sph_vol_spectr
