!>@file  cal_pvr_projection_mat.f90
!!       module cal_pvr_projection_mat
!!
!!@author H. Matsui
!!@date   Programmed in May. 2009
!
!> @brief Evaluate projection matirx
!!
!!@verbatim
!!      subroutine set_pvr_projection_matrix(view_param, projection_mat)
!!      subroutine set_pvr_step_projection_mat                          &
!!     &         (i_img, view_param, stereo_def, projection_step)
!!      subroutine set_pvr_projection_left_mat                          &
!!     &          (view_param, stereo_def, projection_left)
!!      subroutine set_pvr_projection_right_mat                         &
!!     &          (view_param, stereo_def, projection_right)
!!        type(pvr_view_parameter), intent(in) :: view_param
!!        type(pvr_stereo_parameter), intent(in) :: stereo_def
!!@endverbatim
!
      module cal_pvr_projection_mat
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_control_params_4_pvr
      use t_control_params_stereo_pvr
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_pvr_projection_matrix(view_param, projection_mat)
!
      use set_projection_matrix
!
      type(pvr_view_parameter), intent(in) :: view_param
      real(kind = kreal), intent(inout) :: projection_mat(4,4)
!
      integer(kind = kint) :: i
!
!
      call set_perspective_mat_by_angle(view_param%perspective_angle,   &
     &    view_param%perspective_xy_ratio, view_param%perspective_near, &
     &     view_param%perspective_far, projection_mat)
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'projection_mat for PVR '
        do i = 1, 4
          write(*,'(1p4e16.7)') projection_mat(i,1:4)
        end do
      end if
!
      end subroutine set_pvr_projection_matrix
!
! -----------------------------------------------------------------------
!
      subroutine set_pvr_step_projection_mat                            &
     &         (i_img, view_param, stereo_def, projection_step)
!
      use set_projection_matrix
!
      integer(kind = kint), intent(in) :: i_img
      type(pvr_view_parameter), intent(in) :: view_param
      type(pvr_stereo_parameter), intent(in) :: stereo_def
      real(kind = kreal), intent(inout) :: projection_step(4,4)
!
      integer(kind = kint) :: i
!
      real(kind = kreal) :: pi_180, wd2, ndfl, each_eye
      real(kind = kreal) :: view_right, view_left
      real(kind = kreal) :: view_top, view_bottom
      real(kind = kreal) :: view_far, view_near
!
!
      view_near = view_param%perspective_near
      view_far =  view_param%perspective_far
!
      pi_180 = four * atan(one) / 180.0d0
      wd2 =  view_near * tan(view_param%perspective_angle*pi_180*half)
      ndfl = view_near / stereo_def%focalLength
!
      each_eye = each_eye_from_middle(i_img, stereo_def)
!
      view_bottom = - wd2
      view_top =      wd2
      view_left  = - view_param%perspective_xy_ratio * wd2              &
     &              + each_eye * ndfl
      view_right =   view_param%perspective_xy_ratio * wd2              &
     &              + each_eye * ndfl
!
      call set_perspective_mat_by_area(view_left, view_right,           &
     &    view_bottom, view_top, view_near, view_far,                   &
     &    projection_step)
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'projection_step for PVR ', i_img
        do i = 1, 4
          write(*,'(1p4e16.7)') projection_step(i,1:4)
        end do
      end if
!
      end subroutine set_pvr_step_projection_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_pvr_projection_left_mat                            &
     &          (view_param, stereo_def, projection_left)
!
      use set_projection_matrix
!
      type(pvr_view_parameter), intent(in) :: view_param
      type(pvr_stereo_parameter), intent(in) :: stereo_def
      real(kind = kreal), intent(inout) :: projection_left(4,4)
!
      integer(kind = kint) :: i
!
      real(kind = kreal) :: pi_180, wd2, ndfl, each_eye
      real(kind = kreal) :: view_right, view_left
      real(kind = kreal) :: view_top, view_bottom
      real(kind = kreal) :: view_far, view_near
!
!
      view_near = view_param%perspective_near
      view_far =  view_param%perspective_far
!
      pi_180 = four * atan(one) / 180.0d0
      wd2 =  view_near * tan(view_param%perspective_angle*pi_180*half)
      ndfl = view_near / stereo_def%focalLength
!
      each_eye = each_eye_from_middle(ione, stereo_def)
!
        view_bottom = - wd2
        view_top =      wd2
        view_left  = - view_param%perspective_xy_ratio * wd2            &
     &                + each_eye * ndfl
        view_right =   view_param%perspective_xy_ratio * wd2            &
     &                + each_eye * ndfl
!
        call set_perspective_mat_by_area(view_left, view_right,         &
     &      view_bottom, view_top, view_near, view_far,                 &
     &      projection_left)
!
        if (iflag_debug .gt. 0) then
          write(*,*) 'projection_left for PVR '
          do i = 1, 4
            write(*,'(1p4e16.7)') projection_left(i,1:4)
          end do
        end if
!
      end subroutine set_pvr_projection_left_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_pvr_projection_right_mat                           &
     &          (view_param, stereo_def, projection_right)
!
      use set_projection_matrix
!
      type(pvr_view_parameter), intent(in) :: view_param
      type(pvr_stereo_parameter), intent(in) :: stereo_def
      real(kind = kreal), intent(inout) :: projection_right(4,4)
!
      integer(kind = kint) :: i
!
      real(kind = kreal) :: pi_180, wd2, ndfl, each_eye
      real(kind = kreal) :: view_right, view_left
      real(kind = kreal) :: view_top, view_bottom
      real(kind = kreal) :: view_far, view_near
!
!
      view_near = view_param%perspective_near
      view_far =  view_param%perspective_far
!
      pi_180 = four * atan(one) / 180.0d0
      wd2 =  view_near * tan(view_param%perspective_angle*pi_180*half)
      ndfl = view_near / stereo_def%focalLength
!
      each_eye = each_eye_from_middle(ione, stereo_def)
!
        view_bottom = - wd2
        view_top =      wd2
        view_left  = - view_param%perspective_xy_ratio * wd2            &
     &                - each_eye * ndfl
        view_right =   view_param%perspective_xy_ratio * wd2            &
     &                - each_eye * ndfl
!
        call set_perspective_mat_by_area(view_left, view_right,         &
     &      view_bottom, view_top, view_near, view_far,                 &
     &      projection_right)
!
        if (iflag_debug .gt. 0) then
          write(*,*) 'projection_right for PVR '
          do i = 1, 4
            write(*,'(1p4e16.7)') projection_right(i,1:4)
          end do
        end if
!
      end subroutine set_pvr_projection_right_mat
!
! -----------------------------------------------------------------------
!
      end module cal_pvr_projection_mat
