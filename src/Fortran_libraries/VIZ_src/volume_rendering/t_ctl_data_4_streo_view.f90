!>@file   t_ctl_data_4_streo_view.f90
!!@brief  module t_ctl_data_4_streo_view
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR streo parameter
!!
!!@verbatim
!!      subroutine read_stereo_view_ctl                                 &
!!     &         (id_control, hd_block, streo, c_buf)
!!      subroutine reset_stereo_view_ctl(streo)
!!      subroutine bcast_stereo_view_ctl(streo)
!!        type(streo_view_ctl), intent(inout) :: streo
!!      subroutine copy_stereo_view_ctl(org_streo, new_streo)
!!        type(streo_view_ctl), intent(in) :: org_streo
!!        type(streo_view_ctl), intent(inout) :: new_streo
!!
!!      integer(kind = kint) function num_label_pvr_streo()
!!      subroutine set_label_pvr_streo(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Input example
!!
!!    begin stereo_view_parameter_ctl
!!      focal_point_ctl           40.0
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
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use skip_comment_f
!
      implicit  none
!
!>      Structure of streo view parameters
      type streo_view_ctl
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
      integer(kind = kint), parameter, private                          &
     &             :: n_label_pvr_streo =       4
      character(len=kchara), parameter, private                         &
     &             :: hd_focalpoint =     'focal_point_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_eye_separation = 'eye_separation_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_eye_sep_angle = 'eye_separation_angle'
      character(len=kchara), parameter, private                         &
     &             :: hd_eye_step_mode = 'eye_separation_step_by_angle'
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
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_real_ctl_type(c_buf, hd_focalpoint,                   &
     &      streo%focalpoint_ctl)
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
      subroutine bcast_stereo_view_ctl(streo)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(streo_view_ctl), intent(inout) :: streo
!
!
      call calypso_mpi_bcast_one_int(streo%i_stereo_view, 0)
!
      call bcast_ctl_type_r1(streo%focalpoint_ctl)
      call bcast_ctl_type_r1(streo%eye_separation_ctl)
      call bcast_ctl_type_r1(streo%eye_sep_angle_ctl)
      call bcast_ctl_type_c1(streo%step_eye_sep_angle_ctl)
!
      end subroutine bcast_stereo_view_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_stereo_view_ctl(org_streo, new_streo)
!
      type(streo_view_ctl), intent(in) :: org_streo
      type(streo_view_ctl), intent(inout) :: new_streo
!
!
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
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_label_pvr_streo()
      num_label_pvr_streo = n_label_pvr_streo
      return
      end function num_label_pvr_streo
!
! ----------------------------------------------------------------------
!
      subroutine set_label_pvr_streo(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_pvr_streo)
!
!
      call set_control_labels(hd_focalpoint,     names( 1))
      call set_control_labels(hd_eye_separation, names( 2))
      call set_control_labels(hd_eye_sep_angle,  names( 3))
      call set_control_labels(hd_eye_step_mode,  names( 4))
!
      end subroutine set_label_pvr_streo
!
! ----------------------------------------------------------------------
!
      end module t_ctl_data_4_streo_view
