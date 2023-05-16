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
!!      subroutine copy_volume_spectr_ctls(num_ctl, org_vpwr, new_vpwr)
!!        type(volume_spectr_control), intent(in) :: org_vpwr(num_ctl)
!!        type(volume_spectr_control), intent(inout) :: new_vpwr(num_ctl)
!!      subroutine copy_volume_spectr_control(org_vpwr, new_vpwr)
!!        type(volume_spectr_control), intent(in) :: org_vpwr
!!        type(volume_spectr_control), intent(inout) :: new_vpwr
!!
!!      subroutine read_each_vol_spectr_ctl                             &
!!     &         (id_control, hd_block, v_pwr, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(volume_spectr_control), intent(inout) :: v_pwr
!!        type(buffer_for_control), intent(inout) :: c_buf
!!      subroutine write_each_vol_spectr_ctl                            &
!!     &         (id_control, hd_block, v_pwr, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
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
!>        file name for volume mean square
        type(read_character_item) :: volume_spec_file_ctl
!>        file name for volume average
        type(read_character_item) :: volume_ave_file_ctl
!>        file format for volume mean square
        type(read_character_item) :: volume_spec_format_ctl
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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_volume_spectr_ctls(num_ctl, org_vpwr, new_vpwr)
!
      integer(kind = kint), intent(in) :: num_ctl
      type(volume_spectr_control), intent(in) :: org_vpwr(num_ctl)
      type(volume_spectr_control), intent(inout) :: new_vpwr(num_ctl)
!
      integer(kind = kint) :: i
!
      do i = 1, num_ctl
        call copy_volume_spectr_control(org_vpwr(i), new_vpwr(i))
      end do
!
      end subroutine copy_volume_spectr_ctls
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
      call copy_real_ctl(org_vpwr%inner_radius_ctl,                     &
     &    new_vpwr%inner_radius_ctl)
      call copy_real_ctl(org_vpwr%outer_radius_ctl,                     &
     &    new_vpwr%outer_radius_ctl)
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
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(v_pwr%i_vol_spectr_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_vol_pwr,                     &
     &      v_pwr%volume_spec_file_ctl)
        call read_chara_ctl_type(c_buf, hd_vol_fmt,                     &
     &      v_pwr%volume_spec_format_ctl)
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
      subroutine write_each_vol_spectr_ctl                              &
     &         (id_control, hd_block, v_pwr, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
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
!
      write(id_control,'(a1)') '!'
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_vol_pwr, v_pwr%volume_spec_file_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_vol_fmt, v_pwr%volume_spec_format_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_vol_ave, v_pwr%volume_ave_file_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_inner_r, v_pwr%inner_radius_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_outer_r, v_pwr%outer_radius_ctl)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_each_vol_spectr_ctl
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
      v_pwr%inner_radius_ctl%iflag =       0
      v_pwr%outer_radius_ctl%iflag =       0
      v_pwr%i_vol_spectr_ctl = 0
!
      end subroutine reset_volume_spectr_control
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_sph_vol_spectr
