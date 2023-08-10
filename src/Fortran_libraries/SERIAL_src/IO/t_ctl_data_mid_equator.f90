!>@file   t_ctl_data_mid_equator.f90
!!        module t_ctl_data_mid_equator
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!>@brief control date for volume averaged spectr data
!!
!!@verbatim
!!      subroutine reset_mid_equator_control(meq_ctl)
!!        type(mid_equator_control), intent(inout) :: meq_ctl
!!      subroutine init_mid_eq_monitor_ctl_label(hd_block, meq_ctl)
!!      subroutine read_mid_eq_monitor_ctl                              &
!!     &         (id_control, hd_block, meq_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mid_equator_control), intent(inout) :: meq_ctl
!!        type(buffer_for_control), intent(inout) :: c_buf
!!      subroutine write_mid_eq_monitor_ctl(id_control, meq_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        type(mid_equator_control), intent(in) :: meq_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dup_mid_equator_control(org_meq_ctl, new_meq_ctl)
!!        type(mid_equator_control), intent(in) :: org_meq_ctl
!!        type(mid_equator_control), intent(inout) :: new_meq_ctl
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin fields_on_circle_ctl
!!    field_on_circle_prefix         'monitor/dbench_field'
!!    spectr_on_circle_prefix        'monitor/dbench_spectr'
!!    field_on_circle_format         'gzip'
!!
!!    pick_circle_coord_ctl         spherical
!!    nphi_mid_eq_ctl               500
!!    pick_cylindrical_radius_ctl   0.75
!!    pick_vertical_position_ctl    0.6
!!  end fields_on_circle_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_mid_equator
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
      type mid_equator_control
!>        Block name
        character(len=kchara) :: block_name = 'fields_on_circle_ctl'
!>        Structure for field on circle data file prefix
        type(read_character_item) :: circle_field_file_ctl
!>        Structure for spectr on circle data file prefix
        type(read_character_item) :: circle_spectr_file_ctl
!>        Structure for data file on fircle prefix
        type(read_character_item) :: circle_file_format_ctl
!
!>        Structure for coordiniate system for circled data
        type(read_character_item) :: pick_circle_coord_ctl
!
!>        Structure for Number of zonal points for benchamek check
        type(read_integer_item) :: nphi_mid_eq_ctl
!
!>        Structure for position for s
        type(read_real_item) :: pick_s_ctl
!
!>        Structure for position for z
        type(read_real_item) :: pick_z_ctl
!
        integer (kind = kint) :: i_mid_equator_ctl = 0
      end type mid_equator_control
!
!
!   labels for item
!
      character(len=kchara), parameter, private                         &
     &            :: hd_fld_on_circ_prefix = 'field_on_circle_prefix'
      character(len=kchara), parameter, private                         &
     &            :: hd_spec_on_circ_prefix = 'spectr_on_circle_prefix'
      character(len=kchara), parameter, private                         &
     &            :: hd_fld_on_circ_format = 'field_on_circle_format'
!
      character(len=kchara), parameter, private                         &
     &            :: hd_nphi_mid_eq = 'nphi_mid_eq_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_pick_s_ctl = 'pick_cylindrical_radius_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_pick_z_ctl =  'pick_vertical_position_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_circle_coord = 'pick_circle_coord_ctl'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine reset_mid_equator_control(meq_ctl)
!
      type(mid_equator_control), intent(inout) :: meq_ctl
!
      meq_ctl%circle_field_file_ctl%iflag =  0
      meq_ctl%circle_spectr_file_ctl%iflag = 0
      meq_ctl%circle_file_format_ctl%iflag = 0
!
      meq_ctl%pick_circle_coord_ctl%iflag = 0
      meq_ctl%nphi_mid_eq_ctl%iflag = 0
      meq_ctl%pick_s_ctl%iflag =    0
      meq_ctl%pick_z_ctl%iflag =    0
      meq_ctl%i_mid_equator_ctl =   0
!
      end subroutine reset_mid_equator_control
!
! -----------------------------------------------------------------------
!
      subroutine dup_mid_equator_control(org_meq_ctl, new_meq_ctl)
!
      type(mid_equator_control), intent(in) :: org_meq_ctl
      type(mid_equator_control), intent(inout) :: new_meq_ctl
!
      call copy_chara_ctl(org_meq_ctl%circle_field_file_ctl,            &
     &                    new_meq_ctl%circle_field_file_ctl)
      call copy_chara_ctl(org_meq_ctl%circle_spectr_file_ctl,           &
     &                    new_meq_ctl%circle_spectr_file_ctl)
      call copy_chara_ctl(org_meq_ctl%circle_file_format_ctl,           &
     &                    new_meq_ctl%circle_file_format_ctl)
!
      call copy_chara_ctl(org_meq_ctl%pick_circle_coord_ctl,            &
     &                    new_meq_ctl%pick_circle_coord_ctl)
      call copy_integer_ctl(org_meq_ctl%nphi_mid_eq_ctl,                &
     &                      new_meq_ctl%nphi_mid_eq_ctl)
      call copy_real_ctl(org_meq_ctl%pick_s_ctl,                        &
     &                   new_meq_ctl%pick_s_ctl)
      call copy_real_ctl(org_meq_ctl%pick_z_ctl,                        &
     &                   new_meq_ctl%pick_z_ctl)
!
      new_meq_ctl%block_name = org_meq_ctl%block_name
      new_meq_ctl%i_mid_equator_ctl = org_meq_ctl%i_mid_equator_ctl
!
      end subroutine dup_mid_equator_control
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_mid_eq_monitor_ctl                                &
     &         (id_control, hd_block, meq_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(mid_equator_control), intent(inout) :: meq_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(meq_ctl%i_mid_equator_ctl .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_fld_on_circ_prefix,          &
     &      meq_ctl%circle_field_file_ctl)
        call read_chara_ctl_type(c_buf, hd_spec_on_circ_prefix,         &
     &      meq_ctl%circle_spectr_file_ctl)
        call read_chara_ctl_type(c_buf, hd_fld_on_circ_format,          &
     &      meq_ctl%circle_file_format_ctl)
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_pick_s_ctl, meq_ctl%pick_s_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_pick_z_ctl, meq_ctl%pick_z_ctl)
!
        call read_integer_ctl_type(c_buf, hd_nphi_mid_eq,               &
     &      meq_ctl%nphi_mid_eq_ctl)
!
        call read_chara_ctl_type(c_buf, hd_circle_coord,                &
     &      meq_ctl%pick_circle_coord_ctl)
      end do
      meq_ctl%i_mid_equator_ctl = 1
!
      end subroutine read_mid_eq_monitor_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_mid_eq_monitor_ctl(id_control, meq_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(mid_equator_control), intent(in) :: meq_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(meq_ctl%i_mid_equator_ctl .le. 0) return
!
      maxlen = len_trim(hd_pick_s_ctl)
      maxlen = max(maxlen, len_trim(hd_pick_z_ctl))
      maxlen = max(maxlen, len_trim(hd_nphi_mid_eq))
      maxlen = max(maxlen, len_trim(hd_circle_coord))
      maxlen = max(maxlen, len_trim(hd_fld_on_circ_prefix))
      maxlen = max(maxlen, len_trim(hd_spec_on_circ_prefix))
      maxlen = max(maxlen, len_trim(hd_fld_on_circ_format))
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 meq_ctl%block_name)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    meq_ctl%circle_field_file_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    meq_ctl%circle_spectr_file_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    meq_ctl%circle_file_format_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    meq_ctl%pick_circle_coord_ctl)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    meq_ctl%nphi_mid_eq_ctl)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    meq_ctl%pick_s_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    meq_ctl%pick_z_ctl)
!
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                 meq_ctl%block_name)
!
      end subroutine write_mid_eq_monitor_ctl
!
! -----------------------------------------------------------------------
!
      subroutine init_mid_eq_monitor_ctl_label(hd_block, meq_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(mid_equator_control), intent(inout) :: meq_ctl
!
!
      meq_ctl%block_name = hd_block
        call init_chara_ctl_item_label(hd_fld_on_circ_prefix,           &
     &      meq_ctl%circle_field_file_ctl)
        call init_chara_ctl_item_label(hd_spec_on_circ_prefix,          &
     &      meq_ctl%circle_spectr_file_ctl)
        call init_chara_ctl_item_label(hd_fld_on_circ_format,           &
     &      meq_ctl%circle_file_format_ctl)
!
        call init_real_ctl_item_label                                   &
     &     (hd_pick_s_ctl, meq_ctl%pick_s_ctl)
        call init_real_ctl_item_label                                   &
     &     (hd_pick_z_ctl, meq_ctl%pick_z_ctl)
!
        call init_int_ctl_item_label(hd_nphi_mid_eq,                    &
     &      meq_ctl%nphi_mid_eq_ctl)
!
        call init_chara_ctl_item_label(hd_circle_coord,                 &
     &      meq_ctl%pick_circle_coord_ctl)
!
      end subroutine init_mid_eq_monitor_ctl_label
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_mid_equator
