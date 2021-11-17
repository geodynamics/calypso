!>@file  set_projection_matrix.f90
!!       module set_projection_matrix
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Evaluate projection matirx
!!
!!@verbatim
!
!!      subroutine set_perspective_mat_by_angle(view_angle, aspect,     &
!!     &          view_near, view_far, project_mat)
!!      subroutine set_perspective_mat_by_area(view_left, view_right,   &
!!     &        view_bottom, view_top, view_near, view_far, project_mat)
!!      subroutine set_perspective_mat_half_area(view_right, view_top,  &
!!     &          view_near, view_far, project_mat)
!!        range: of view after transdorm: -w<x,y,z<w
!!
!!      subroutine set_orthogonal_mat_half_area(view_right, view_top,   &
!!     &          view_near, view_far, project_mat)
!!      subroutine set__orthogonal_mat_by_area(view_left, view_right,   &
!!     &        view_bottom, view_top, view_near, view_far, project_mat)
!!        range: of view after transdorm: -w<x,y,z<w
!!
!!      subroutine set_pixel_points_on_project(num_pixel_x, num_pixel_y,&
!!     &          pixel_point_x, pixel_point_y)
!!         range: of view in window: -1 < x,y < 1
!!@endverbatim
      module set_projection_matrix
!
      use m_precision
      use m_constants
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_perspective_mat_by_angle(view_angle, aspect,       &
     &          view_near, view_far, project_mat)
!
      real(kind = kreal), intent(in) :: view_angle, aspect
      real(kind = kreal), intent(in) :: view_far, view_near
      real(kind = kreal), intent(inout) :: project_mat(4,4)
      real(kind = kreal) :: pi, view_rad
!
      pi = four * atan(one)
      view_rad = half*view_angle*pi / dble(180)
      project_mat(1,1) = one / (aspect*tan(view_rad))
      project_mat(1,2) = zero
      project_mat(1,3) = zero
      project_mat(1,4) = zero
!
      project_mat(2,1) = zero
      project_mat(2,2) = one / tan(view_rad)
      project_mat(2,3) = zero
      project_mat(2,4) = zero
!
      project_mat(3,1) = zero
      project_mat(3,2) = zero
      project_mat(3,3) =-(view_far+view_near) / (view_far-view_near)
      project_mat(3,4) =-two*view_far*view_near / (view_far-view_near)
!
      project_mat(4,1) = zero
      project_mat(4,2) = zero
      project_mat(4,3) = -one
      project_mat(4,4) = zero
!
      end subroutine set_perspective_mat_by_angle
!
!  ---------------------------------------------------------------------
!
      subroutine set_perspective_mat_by_area(view_left, view_right,     &
     &        view_bottom, view_top, view_near, view_far, project_mat)
!
      real(kind = kreal), intent(in) :: view_right, view_left
      real(kind = kreal), intent(in) :: view_top, view_bottom
      real(kind = kreal), intent(in) :: view_far, view_near
      real(kind = kreal), intent(inout) :: project_mat(4,4)
!
      project_mat(1,1) = two*view_near / (view_right-view_left)
      project_mat(1,2) = zero
      project_mat(1,3) = (view_right+view_left) / (view_right-view_left)
      project_mat(1,4) = zero
!
      project_mat(2,1) = zero
      project_mat(2,2) = two*view_near / (view_top-view_bottom)
      project_mat(2,3) = (view_top+view_bottom) / (view_top-view_bottom)
      project_mat(2,4) = zero
!
      project_mat(3,1) = zero
      project_mat(3,2) = zero
      project_mat(3,3) =-(view_far+view_near) / (view_far-view_near)
      project_mat(3,4) =-two*view_far*view_near / (view_far-view_near)
!
      project_mat(4,1) = zero
      project_mat(4,2) = zero
      project_mat(4,3) = -one
      project_mat(4,4) = zero
!
      end subroutine set_perspective_mat_by_area
!
!  ---------------------------------------------------------------------
!
      subroutine set_perspective_mat_half_area(view_right, view_top,    &
     &          view_near, view_far, project_mat)
!
      real(kind = kreal), intent(in) :: view_right, view_top
      real(kind = kreal), intent(in) :: view_far, view_near
      real(kind = kreal), intent(inout) :: project_mat(4,4)
!
      project_mat(1,1) = view_near / view_right
      project_mat(1,2) = zero
      project_mat(1,3) = zero
      project_mat(1,4) = zero
!
      project_mat(2,1) = zero
      project_mat(2,2) = view_near / view_top
      project_mat(2,3) = zero
      project_mat(2,4) = zero
!
      project_mat(3,1) = zero
      project_mat(3,2) = zero
      project_mat(3,3) =-(view_far+view_near) / (view_far-view_near)
      project_mat(3,4) =-two*view_far*view_near / (view_far-view_near)
!
      project_mat(4,1) = zero
      project_mat(4,2) = zero
      project_mat(4,3) = -one
      project_mat(4,4) = zero
!
      end subroutine set_perspective_mat_half_area
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set__orthogonal_mat_by_area(view_left, view_right,     &
     &        view_bottom, view_top, view_near, view_far, project_mat)
!
      real(kind = kreal), intent(in) :: view_right, view_left
      real(kind = kreal), intent(in) :: view_top, view_bottom
      real(kind = kreal), intent(in) :: view_far, view_near
      real(kind = kreal), intent(inout) :: project_mat(4,4)
!
!
      project_mat(1,1) = two / (view_right-view_left)
      project_mat(1,2) = zero
      project_mat(1,3) = zero
      project_mat(1,4) =-(view_right+view_left) / (view_right-view_left)
!
      project_mat(2,1) = zero
      project_mat(2,2) = two / (view_top-view_bottom)
      project_mat(2,3) = zero
      project_mat(2,4) = -(view_right+view_left) / (view_top-view_bottom)
!
      project_mat(3,1) = zero
      project_mat(3,2) = zero
      project_mat(3,3) =-two / (view_far-view_near)
      project_mat(3,4) =-(view_far+view_near) / (view_far-view_near)
!
      project_mat(4,1) = zero
      project_mat(4,2) = zero
      project_mat(4,3) = zero
      project_mat(4,4) = one
!
      end subroutine set__orthogonal_mat_by_area
!
!  ---------------------------------------------------------------------
!
      subroutine set_orthogonal_mat_half_area(view_right, view_top,     &
     &          view_near, view_far, project_mat)
!
      real(kind = kreal), intent(in) :: view_right, view_top
      real(kind = kreal), intent(in) :: view_far, view_near
      real(kind = kreal), intent(inout) :: project_mat(4,4)
!
!
      project_mat(1,1) = one / view_right
      project_mat(1,2) = zero
      project_mat(1,3) = zero
      project_mat(1,4) = zero
!
      project_mat(2,1) = zero
      project_mat(2,2) = one / view_top
      project_mat(2,3) = zero
      project_mat(2,4) = zero
!
      project_mat(3,1) = zero
      project_mat(3,2) = zero
      project_mat(3,3) =-two / (view_far-view_near)
      project_mat(3,4) = zero
!
      project_mat(4,1) = zero
      project_mat(4,2) = zero
      project_mat(4,3) = zero
      project_mat(4,4) = one
!
      end subroutine set_orthogonal_mat_half_area
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_pixel_points_on_project(num_pixel_x, num_pixel_y,  &
     &          pixel_point_x, pixel_point_y)
!
      integer(kind = kint), intent(in) :: num_pixel_x, num_pixel_y
      real(kind = kreal), intent(inout) :: pixel_point_x(num_pixel_x)
      real(kind = kreal), intent(inout) :: pixel_point_y(num_pixel_y)
      real(kind = kreal) :: dx, dy
      integer(kind = kint) :: i, j
!
      dx = one / num_pixel_x
      dy = one / num_pixel_y
      do i = 1, num_pixel_x
        pixel_point_x(i) =  dble(2*i-1)*dx - one
      end do
      do j = 1, num_pixel_y
        pixel_point_y(j) =  dble(2*j-1)*dy - one
      end do
!      window_dx(1) = two * dx
!      window_dx(2) = two * dy
!
      end subroutine set_pixel_points_on_project
!
!  ---------------------------------------------------------------------
!
      end module set_projection_matrix
