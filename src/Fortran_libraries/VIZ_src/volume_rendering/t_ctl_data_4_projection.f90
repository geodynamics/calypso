!>@file   t_ctl_data_4_projection.f90
!!@brief  module t_ctl_data_4_projection
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR projection and streo parameter
!!
!!@verbatim
!!      subroutine read_projection_mat_ctl                              &
!!     &         (id_control, hd_block, proj, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(projection_ctl), intent(inout) :: proj
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_projection_mat_ctl                             &
!!     &         (id_control, hd_block, proj, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(projection_ctl), intent(in) :: proj
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine reset_projection_view_ctl(proj)
!!        type(projection_ctl), intent(inout) :: proj
!!      subroutine copy_projection_mat_ctl(org_proj, new_proj)
!!        type(projection_ctl), intent(in) :: org_proj
!!        type(projection_ctl), intent(inout) :: new_proj
!!
!!      integer(kind = kint) function num_label_pvr_projection()
!!      subroutine set_label_pvr_projection(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Input example
!!
!!    begin projection_matrix_ctl
!!      perspective_angle_ctl     10.0
!!      perspective_xy_ratio_ctl   1.0
!!      perspective_near_ctl       0.5
!!      perspective_far_ctl     1000.0
!!
!!      horizontal_range_ctl       -2.4   2.4
!!      vertical_range_ctl         -1.2   1.2
!!    end projection_matrix_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
!
      module t_ctl_data_4_projection
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_real
      use t_control_array_real2
      use skip_comment_f
!
      implicit  none
!
!
!>      Structure of projection parameters
      type projection_ctl
!>        Structure of perspective view angle
        type(read_real_item) :: perspective_angle_ctl
!>        Structure of aspect ration of screen
        type(read_real_item) :: perspective_xy_ratio_ctl
!>        Structure of Near point of view
        type(read_real_item) :: perspective_near_ctl
!>        Structure of Far point of view
        type(read_real_item) :: perspective_far_ctl
!
!>        Structure of horizontal screen range
        type(read_real2_item) :: horizontal_range_ctl
!>        Structure of vertical screen range
        type(read_real2_item) :: vertical_range_ctl

        integer (kind=kint) :: i_project_mat = 0
      end type projection_ctl
!
!     4th level for projection_matrix
      integer(kind = kint), parameter, private                          &
     &             :: n_label_pvr_projection = 6
      character(len=kchara), parameter, private                         &
     &             :: hd_perspect_angle = 'perspective_angle_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_perspect_xy =    'perspective_xy_ratio_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_perspect_near =  'perspective_near_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_perspect_far =  'perspective_far_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_horizontal_range =  'horizontal_range_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_vertical_range =    'vertical_range_ctl'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_projection_mat_ctl                                &
     &         (id_control, hd_block, proj, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(projection_ctl), intent(inout) :: proj
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if (proj%i_project_mat.gt.0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_real_ctl_type(c_buf, hd_perspect_angle,               &
     &      proj%perspective_angle_ctl)
        call read_real_ctl_type(c_buf, hd_perspect_xy,                  &
     &      proj%perspective_xy_ratio_ctl)
        call read_real_ctl_type(c_buf, hd_perspect_near,                &
     &      proj%perspective_near_ctl)
        call read_real_ctl_type(c_buf, hd_perspect_far,                 &
     &      proj%perspective_far_ctl)
!
        call read_real2_ctl_type(c_buf, hd_horizontal_range,            &
     &      proj%horizontal_range_ctl)
        call read_real2_ctl_type(c_buf, hd_vertical_range,              &
     &      proj%vertical_range_ctl)
      end do
      proj%i_project_mat = 1
!
      end subroutine read_projection_mat_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_projection_mat_ctl                               &
     &         (id_control, hd_block, proj, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(projection_ctl), intent(in) :: proj
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(proj%i_project_mat .le. 0) return
!
      maxlen = len_trim(hd_perspect_angle)
      maxlen = max(maxlen, len_trim(hd_perspect_xy))
      maxlen = max(maxlen, len_trim(hd_perspect_near))
      maxlen = max(maxlen, len_trim(hd_perspect_far))
      maxlen = max(maxlen, len_trim(hd_horizontal_range))
      maxlen = max(maxlen, len_trim(hd_vertical_range))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_perspect_angle, proj%perspective_angle_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_perspect_xy, proj%perspective_xy_ratio_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_perspect_near, proj%perspective_near_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_perspect_far, proj%perspective_far_ctl)
!
      call write_real2_ctl_type(id_control, level, maxlen,              &
     &    hd_horizontal_range, proj%horizontal_range_ctl)
      call write_real2_ctl_type(id_control, level, maxlen,              &
     &    hd_vertical_range, proj%vertical_range_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_projection_mat_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_projection_view_ctl(proj)
!
      type(projection_ctl), intent(inout) :: proj
!
!
      proj%perspective_angle_ctl%iflag =    0
      proj%perspective_xy_ratio_ctl%iflag = 0
      proj%perspective_near_ctl%iflag =     0
      proj%perspective_far_ctl%iflag =      0
      proj%horizontal_range_ctl%iflag =     0
      proj%vertical_range_ctl%iflag =       0
!
      proj%i_project_mat = 0
!
      end subroutine reset_projection_view_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_projection_mat_ctl(org_proj, new_proj)
!
      type(projection_ctl), intent(in) :: org_proj
      type(projection_ctl), intent(inout) :: new_proj
!
!
      new_proj%i_project_mat = org_proj%i_project_mat
!
      call copy_real_ctl(org_proj%perspective_angle_ctl,                &
     &                   new_proj%perspective_angle_ctl)
      call copy_real_ctl(org_proj%perspective_xy_ratio_ctl,             &
     &                   new_proj%perspective_xy_ratio_ctl)
      call copy_real_ctl(org_proj%perspective_near_ctl,                 &
     &                   new_proj%perspective_near_ctl)
      call copy_real_ctl(org_proj%perspective_far_ctl,                  &
     &                   new_proj%perspective_far_ctl)
!
      call copy_real2_ctl(org_proj%horizontal_range_ctl,                &
     &                   new_proj%horizontal_range_ctl)
      call copy_real2_ctl(org_proj%vertical_range_ctl,                  &
     &                   new_proj%vertical_range_ctl)
!
      end subroutine copy_projection_mat_ctl
!
!  ---------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_label_pvr_projection()
      num_label_pvr_projection = n_label_pvr_projection
      return
      end function num_label_pvr_projection
!
! ----------------------------------------------------------------------
!
      subroutine set_label_pvr_projection(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_pvr_projection)
!
!
      call set_control_labels(hd_perspect_angle, names( 1))
      call set_control_labels(hd_perspect_xy,    names( 2))
      call set_control_labels(hd_perspect_near,  names( 3))
      call set_control_labels(hd_perspect_far,   names( 4))
!
      call set_control_labels(hd_horizontal_range, names( 5))
      call set_control_labels(hd_vertical_range,   names( 6))
!
      end subroutine set_label_pvr_projection
!
! ----------------------------------------------------------------------
!
      end module t_ctl_data_4_projection
