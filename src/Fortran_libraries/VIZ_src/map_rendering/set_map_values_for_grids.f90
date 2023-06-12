!>@file   set_map_values_for_grids.f90
!!@brief  module set_map_values_for_grids
!!
!!@author H. Matsui
!!@date Programmed in July, 2023
!
!>@brief Subroutines to draw lines on map
!!
!!@verbatim
!!      subroutine map_value_to_projected_x                             &
!!     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          nxpixel, nypixel, x_map)
!!      subroutine map_value_to_projected_r                             &
!!     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          nxpixel, nypixel, r_map)
!!      subroutine map_value_to_longitude                               &
!!     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          nxpixel, nypixel, phi_map)
!!      subroutine map_value_to_colatitude                              &
!!     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          nxpixel, nypixel, theta_map)
!!        real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
!!        real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(inout) :: r_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(inout) :: phi_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(inout) :: theta_map(nxpixel*nypixel)
!!@endverbatim
      module set_map_values_for_grids
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
      subroutine map_value_to_projected_x                               &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, x_map)
!
      use aitoff
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
!
      real(kind = kreal), intent(inout) :: x_map(nxpixel*nypixel)
!
      integer(kind = kint) :: i_img, i, j
      real(kind = kreal) :: x_pix1
      real(kind = kreal) :: theta(1), phi(1)
!
!
!$omp parallel do private(i,j,x_pix1,i_img,theta,phi)
      do j = 1, nypixel-1
        do i = 1, nxpixel-1
          x_pix1 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i-1) / dble(nxpixel-1)
          i_img = i + (j-1) * nxpixel
          x_map(i_img) = x_pix1
        end do
      end do
!$omp end parallel do
!
      end subroutine map_value_to_projected_x
!
!  ---------------------------------------------------------------------
!
      subroutine map_value_to_projected_r                               &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, r_map)
!
      use aitoff
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
!
      real(kind = kreal), intent(inout) :: r_map(nxpixel*nypixel)
!
      integer(kind = kint) :: i_img, i, j
      real(kind = kreal) :: x_pix1, y_pix1
      real(kind = kreal) :: theta(1), phi(1)
!
!
!$omp parallel do private(i,j,x_pix1,y_pix1,i_img,theta,phi)
      do j = 1, nypixel-1
        do i = 1, nxpixel-1
          x_pix1 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i-1) / dble(nxpixel-1)
          y_pix1 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j-1) / dble(nypixel-1)
          i_img = i + (j-1) * nxpixel
          r_map(i_img) = sqrt(x_pix1**2 + y_pix1**2)
        end do
      end do
!$omp end parallel do
!
      end subroutine map_value_to_projected_r
!
!  ---------------------------------------------------------------------
!
      subroutine map_value_to_longitude                                 &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, phi_map)
!
      use aitoff
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
!
      real(kind = kreal), intent(inout) :: phi_map(nxpixel*nypixel)
!
      integer(kind = kint) :: i_img, i, j
      real(kind = kreal) :: x_pix1, y_pix1
      real(kind = kreal) :: theta(1), phi(1)
!
!
!$omp parallel do private(i,j,x_pix1,y_pix1,i_img,theta,phi)
      do j = 1, nypixel
        do i = 1, nxpixel
          x_pix1 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i-1) / dble(nxpixel-1)
          y_pix1 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j-1) / dble(nypixel-1)
          i_img = i + (j-1) * nxpixel
          call reverse_aitoff(x_pix1, y_pix1, theta(1), phi(1))
          phi_map(i_img) = phi(1)
        end do
      end do
!$omp end parallel do
!
      end subroutine map_value_to_longitude
!
!  ---------------------------------------------------------------------
!
      subroutine map_value_to_colatitude                                &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, theta_map)
!
      use aitoff
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
!
      real(kind = kreal), intent(inout) :: theta_map(nxpixel*nypixel)
!
      integer(kind = kint) :: i_img, i, j
      real(kind = kreal) :: x_pix1, y_pix1
      real(kind = kreal) :: theta(1), phi(1)
!
!
!$omp parallel do private(i,j,x_pix1,y_pix1,i_img,theta,phi)
      do j = 1, nypixel
        do i = 1, nxpixel
          x_pix1 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i-1) / dble(nxpixel-1)
          y_pix1 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j-1) / dble(nypixel-1)
          i_img = i + (j-1) * nxpixel
          call reverse_aitoff(x_pix1, y_pix1, theta(1), phi(1))
          theta_map(i_img) = theta(1)
        end do
      end do
!$omp end parallel do
!
      end subroutine map_value_to_colatitude
!
!  ---------------------------------------------------------------------
!
      end module set_map_values_for_grids
