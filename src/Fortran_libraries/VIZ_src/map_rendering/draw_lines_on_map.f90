!>@file   draw_lines_on_map.f90
!!@brief  module draw_lines_on_map
!!
!!@author H. Matsui
!!@date Programmed in July, 2023
!
!>@brief Subroutines to draw lines on map
!!
!!@verbatim
!!      subroutine map_value_to_rgb(color_param, nxpixel, nypixel, npix,&
!!     &                            d_map, rgba)
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!!        real(kind = kreal), intent(in) :: d_map(npix)
!!        real(kind = kreal), intent(inout) :: rgba(4,npix)
!!      subroutine draw_aitoff_map_frame                                &
!!     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          nxpixel, nypixel, npix, rgba)
!!        real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
!!        real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!!        real(kind = kreal), intent(inout) :: rgba(4,npix)
!!      subroutine draw_aitoff_lat_line                                 &
!!     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          theta_ref, rgba_in, nxpixel, nypixel, npix, rgba)
!!        real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
!!        real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!!        real(kind = kreal), intent(in) :: theta_ref
!!        real(kind = kreal), intent(in) :: rgba_in(4)
!!        real(kind = kreal), intent(inout) :: rgba(4,npix)
!!@endverbatim
      module draw_lines_on_map
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
      subroutine map_value_to_rgb(color_param, nxpixel, nypixel, npix,  &
     &                            d_map, rgba)
!
      use t_pvr_colormap_parameter
      use set_color_4_pvr
!
      type(pvr_colormap_parameter), intent(in) :: color_param
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
      real(kind = kreal), intent(in) :: d_map(npix)
!
      real(kind = kreal), intent(inout) :: rgba(4,npix)
!
      integer(kind = kint) :: i_img, i, j
!
!
!$omp parallel do private(i,j,i_img)
      do j = 1, nypixel
        do i = 1, nxpixel
          i_img = i + (j-1) * nxpixel
          if(rgba(4,i_img) .eq. zero) cycle
!
          call value_to_rgb(color_param%id_pvr_color(2),                &
     &                      color_param%id_pvr_color(1),                &
     &                      color_param%num_pvr_datamap_pnt,            &
     &                      color_param%pvr_datamap_param,              &
     &                      d_map(i_img), rgba(1,i_img))
        end do
      end do
!$omp end parallel do
!
      end subroutine map_value_to_rgb
!
!  ---------------------------------------------------------------------
!
      subroutine draw_aitoff_map_frame                                  &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, npix, rgba)
!
      use aitoff
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!
      real(kind = kreal), intent(inout) :: rgba(4,npix)
!
      integer(kind = kint) :: ii, jj
      integer(kind = kint) :: i_img, i, j
      real(kind = kreal) :: x_pix1, x_pix2, y_pix1, y_pix2
      real(kind = kreal) :: phi_ref, theta_ref, pi
      real(kind = kreal) :: theta(4), phi(4)
!
!
      pi = four * atan(one)
!$omp parallel do private(i,j,x_pix1,x_pix2,y_pix1,y_pix2,ii,jj,i_img,  &
!$omp&                    phi_ref,theta_ref,theta,phi)
      do j = 1, nypixel-1
        do i = 1, nxpixel-1
          if(mod(j,6).ge.3 .and. mod(i,6).lt.3) cycle
          if(mod(j,6).lt.3 .and. mod(i,6).ge.3) cycle
!
!
          x_pix1 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i-1) / dble(nxpixel-1)
          x_pix2 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i) /   dble(nxpixel-1)
          y_pix1 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j-1) / dble(nypixel-1)
          y_pix2 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j) / dble(nypixel-1)
          call reverse_aitoff(x_pix1, y_pix1, theta(1), phi(1))
          call reverse_aitoff(x_pix2, y_pix1, theta(2), phi(2))
          call reverse_aitoff(x_pix1, y_pix2, theta(3), phi(3))
          call reverse_aitoff(x_pix2, y_pix2, theta(4), phi(4))
!
          if(    (theta(1)*theta(2)) .le. 0.0d0                         &
     &      .or. (theta(1)*theta(3)) .le. 0.0d0                         &
     &      .or. (theta(1)*theta(4)) .le. 0.0d0) then
            i_img = i + (j-1) * nxpixel
            rgba(1:4,i_img) = one
            rgba(4,  i_img) = one
          end if
!
          if(theta(1) .le. zero) cycle
          if(theta(2) .le. zero) cycle
          if(theta(3) .le. zero) cycle
          if(theta(4) .le. zero) cycle
          do ii = 1, 5
            phi_ref = pi * dble(ii-3) / 3.0d0
            if(     (phi(1)-phi_ref)*(phi(2)-phi_ref) .le. zero         &
     &         .or. (phi(1)-phi_ref)*(phi(3)-phi_ref) .le. zero         &
     &         .or. (phi(1)-phi_ref)*(phi(4)-phi_ref) .le. zero) then
              i_img = i + (j-1) * nxpixel
              rgba(1:4,i_img) = zero
              rgba(4,  i_img) = one
!              rgba(1:4,i_img+1,j) = zero
!              rgba(4,  i_img+1,j) = one
            end if
          end do
!
          do jj = 1, 5
            theta_ref = pi * dble(jj) / 6.0d0
            if(    (theta(1)-theta_ref)*(theta(2)-theta_ref) .le. zero  &
     &        .or. (theta(1)-theta_ref)*(theta(3)-theta_ref) .le. zero  &
     &        .or. (theta(1)-theta_ref)*(theta(4)-theta_ref) .le. zero) &
     &         then
              i_img = i + (j-1) * nxpixel
              rgba(1:4,i_img) = zero
              rgba(4,  i_img) = one
!              rgba(1:4,i_img+nxpixel) = zero
!              rgba(4,  i_img+nxpixel) = one
            end if
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine draw_aitoff_map_frame
!
!  ---------------------------------------------------------------------
!
      subroutine draw_aitoff_lat_line                                   &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          theta_ref, rgba_in, nxpixel, nypixel, npix, rgba)
!
      use aitoff
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
      real(kind = kreal), intent(in) :: theta_ref
      real(kind = kreal), intent(in) :: rgba_in(4)
!
      real(kind = kreal), intent(inout) :: rgba(4,npix)
!
      integer(kind = kint) :: ii, jj
      integer(kind = kint) :: i_img, i, j
      real(kind = kreal) :: x_pix1, x_pix2, y_pix1, y_pix2
      real(kind = kreal) :: phi_ref, pi
      real(kind = kreal) :: theta(4), phi(4)
!
!
      pi = four * atan(one)
!$omp parallel do private(i,j,x_pix1,x_pix2,y_pix1,y_pix2,ii,jj,i_img,  &
!$omp&                    phi_ref,theta,phi)
      do j = 2, nypixel-1
        do i = 2, nxpixel-1
          if(mod(i,12).ge.6) cycle
!
          x_pix1 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i-1) / dble(nxpixel-1)
          x_pix2 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i) /   dble(nxpixel-1)
          y_pix1 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j-1) / dble(nypixel-1)
          y_pix2 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j) / dble(nypixel-1)
          call reverse_aitoff(x_pix1, y_pix1, theta(1), phi(1))
          call reverse_aitoff(x_pix2, y_pix1, theta(2), phi(2))
          call reverse_aitoff(x_pix1, y_pix2, theta(3), phi(3))
          call reverse_aitoff(x_pix2, y_pix2, theta(4), phi(4))
!
          if(    (theta(1)-theta_ref)*(theta(2)-theta_ref) .le. zero    &
     &      .or. (theta(1)-theta_ref)*(theta(3)-theta_ref) .le. zero    &
     &      .or. (theta(1)-theta_ref)*(theta(4)-theta_ref) .le. zero)   &
     &       then
             i_img = i + (j-1) * nxpixel
             rgba(1:3,i_img) = rgba_in(1:3)
             rgba(4,  i_img) = one
             if(rgba(4,i_img-nxpixel).gt.0) then
               rgba(1:3,i_img-nxpixel) = rgba_in(1:3)
             end if
             if(rgba(4,i_img+nxpixel).gt.0) then
               rgba(1:3,i_img+nxpixel) = rgba_in(1:3)
             end if
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine draw_aitoff_lat_line
!
!  ---------------------------------------------------------------------
!
      end module draw_lines_on_map
