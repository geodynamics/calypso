!>@file   t_ctl_data_4_streo_view.f90
!!@brief  module t_ctl_data_4_streo_view
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR streo parameter
!!
!!@verbatim
!!      subroutine init_stereo_view_ctl_label(hd_block, streo)
!!      subroutine read_stereo_view_ctl                                 &
!!     &         (id_control, hd_block, streo, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(streo_view_ctl), intent(inout) :: streo
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_stereo_view_ctl                                &
!!     &         (id_control, hd_block, streo, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(streo_view_ctl), intent(in) :: streo
!!        integer(kind = kint), intent(inout) :: level
!!      logical function cmp_streo_view_ctl(streo1, streo2)
!!        type(streo_view_ctl), intent(in) :: streo1, streo2
!!
!!      subroutine reset_stereo_view_ctl(streo)
!!        type(streo_view_ctl), intent(inout) :: streo
!!      subroutine copy_stereo_view_ctl(org_streo, new_streo)
!!        type(streo_view_ctl), intent(in) :: org_streo
!!        type(streo_view_ctl), intent(inout) :: new_streo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Input example
!!
!!    begin stereo_view_parameter_ctl
!!      focal_distance_ctl       40.0
!!      eye_separation_ctl        0.5
!!      eye_separation_angle      35.0
!!      eye_separation_step_by_angle     ON
!!    end stereo_view_parameter_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
!
      module t_ctl_data_4_streo_view
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
!
      implicit  none
!
!>      Structure of streo view parameters
      type streo_view_ctl
!>        Control block name
        character(len = kchara) :: block_name                           &
     &                         = 'stereo_view_parameter_ctl'
!>        Structure of focal point
        type(read_real_item) :: focalpoint_ctl
!>        Structure of eye separation
        type(read_real_item) :: eye_separation_ctl
!>        Structure of eye separation angle (degree)
        type(read_real_item) :: eye_sep_angle_ctl
!>        Switch to eye moving step by angle
        type(read_character_item) :: step_eye_sep_angle_ctl
!
        integer (kind=kint) :: i_stereo_view = 0
      end type streo_view_ctl
!
!     4th level for stereo view
      character(len=kchara), parameter, private                         &
     &             :: hd_focaldistance =  'focal_distance_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_eye_separation = 'eye_separation_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_eye_sep_angle = 'eye_separation_angle'
      character(len=kchara), parameter, private                         &
     &             :: hd_eye_step_mode = 'eye_separation_step_by_angle'
!
!       Old label
      character(len=kchara), parameter, private                         &
     &             :: hd_focalpoint =     'focal_point_ctl'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_stereo_view_ctl                                   &
     &         (id_control, hd_block, streo, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(streo_view_ctl), intent(inout) :: streo
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if (streo%i_stereo_view.gt.0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_real_ctl_type(c_buf, hd_focaldistance,                &
     &      streo%focalpoint_ctl)
        call read_real_ctl_type(c_buf, hd_focalpoint,                   &
     &      streo%focalpoint_ctl)
!
        call read_real_ctl_type(c_buf, hd_eye_separation,               &
     &      streo%eye_separation_ctl)
        call read_real_ctl_type(c_buf, hd_eye_sep_angle,                &
     &      streo%eye_sep_angle_ctl)
!
        call read_chara_ctl_type(c_buf, hd_eye_step_mode,               &
     &      streo%step_eye_sep_angle_ctl)
      end do
      streo%i_stereo_view = 1
!
      end subroutine read_stereo_view_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_stereo_view_ctl                                  &
     &         (id_control, hd_block, streo, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(streo_view_ctl), intent(in) :: streo
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(streo%i_stereo_view .le. 0) return
!
      maxlen = len_trim(hd_focaldistance)
      maxlen = max(maxlen, len_trim(hd_eye_separation))
      maxlen = max(maxlen, len_trim(hd_eye_sep_angle))
      maxlen = max(maxlen, len_trim(hd_eye_step_mode))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    streo%focalpoint_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    streo%eye_separation_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    streo%eye_sep_angle_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    streo%step_eye_sep_angle_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_stereo_view_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine init_stereo_view_ctl_label(hd_block, streo)
!
      character(len=kchara), intent(in) :: hd_block
      type(streo_view_ctl), intent(inout) :: streo
!
!
      streo%block_name = hd_block
        call init_real_ctl_item_label(hd_focaldistance,                 &
     &      streo%focalpoint_ctl)
        call init_real_ctl_item_label(hd_focalpoint,                    &
     &      streo%focalpoint_ctl)
!
        call init_real_ctl_item_label(hd_eye_separation,                &
     &      streo%eye_separation_ctl)
        call init_real_ctl_item_label(hd_eye_sep_angle,                 &
     &      streo%eye_sep_angle_ctl)
!
        call init_chara_ctl_item_label(hd_eye_step_mode,                &
     &      streo%step_eye_sep_angle_ctl)
!
      end subroutine init_stereo_view_ctl_label
!
!  ---------------------------------------------------------------------
!
      logical function cmp_streo_view_ctl(streo1, streo2)
!
      use skip_comment_f
!
      type(streo_view_ctl), intent(in) :: streo1, streo2
!
      cmp_streo_view_ctl = .FALSE.
      if(streo1%i_stereo_view .ne. streo2%i_stereo_view) return
      if(cmp_no_case(trim(streo1%block_name),                           &
     &               trim(streo2%block_name)) .eqv. .FALSE.) return
!
      if(cmp_read_real_item(streo1%focalpoint_ctl,                      &
     &                      streo2%focalpoint_ctl)                      &
     &                                          .eqv. .FALSE.) return
      if(cmp_read_real_item(streo1%eye_separation_ctl,                  &
     &                      streo2%eye_separation_ctl)                  &
     &                                          .eqv. .FALSE.) return
      if(cmp_read_real_item(streo1%eye_sep_angle_ctl,                   &
     &                      streo2%eye_sep_angle_ctl)                   &
     &                                          .eqv. .FALSE.) return
      if(cmp_read_chara_item(streo1%step_eye_sep_angle_ctl,             &
     &                       streo2%step_eye_sep_angle_ctl)             &
     &                                          .eqv. .FALSE.) return
      cmp_streo_view_ctl = .TRUE.
!
      end function cmp_streo_view_ctl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine reset_stereo_view_ctl(streo)
!
      type(streo_view_ctl), intent(inout) :: streo
!
!
      streo%focalpoint_ctl%iflag =         0
      streo%eye_separation_ctl%iflag =     0
      streo%eye_sep_angle_ctl%iflag =      0
      streo%step_eye_sep_angle_ctl%iflag = 0
!
      streo%i_stereo_view = 0
!
      end subroutine reset_stereo_view_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_stereo_view_ctl(org_streo, new_streo)
!
      type(streo_view_ctl), intent(in) :: org_streo
      type(streo_view_ctl), intent(inout) :: new_streo
!
!
      new_streo%block_name =    org_streo%block_name
      new_streo%i_stereo_view = org_streo%i_stereo_view
!
      call copy_real_ctl(org_streo%focalpoint_ctl,                      &
     &                   new_streo%focalpoint_ctl)
      call copy_real_ctl(org_streo%eye_separation_ctl,                  &
     &                   new_streo%eye_separation_ctl)
      call copy_real_ctl(org_streo%eye_sep_angle_ctl,                   &
     &                   new_streo%eye_sep_angle_ctl)
      call copy_chara_ctl(org_streo%step_eye_sep_angle_ctl,             &
     &                    new_streo%step_eye_sep_angle_ctl)
!
      end subroutine copy_stereo_view_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_4_streo_view
