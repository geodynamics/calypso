!>@file   draw_pixels_on_map.f90
!!@brief  module draw_pixels_on_map
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!
!>@brief Fraw pixels on projected image
!!
!!@verbatim
!!      subroutine fill_triangle_data_on_image(color_param,             &
!!     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          nxpixel, nypixel, k_ymin, k_ymid, k_ymax,             &
!!     &          xy_patch, d_patch, rgba)
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
!!        real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        integer(kind = kint), intent(in) :: k_ymin, k_ymid, k_ymax
!!        real(kind = kreal), intent(in) :: xy_patch(2,3)
!!        real(kind = kreal), intent(in) :: d_patch(3)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!      subroutine fill_map_one_color(nxpixel, nypixel, bg_rgba, rgba)
!!      subroutine fill_background(nxpixel, nypixel, bg_rgba, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: bg_rgba(4)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!@endverbatim
      module draw_pixels_on_map
!
      use m_precision
      use m_constants
      use m_geometry_constants
!
      implicit  none
!
      private :: find_map_path_orientation
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine fill_triangle_data_on_image(color_param,               &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, xy_patch, d_patch, rgba)
!
      use t_pvr_colormap_parameter
      use set_color_4_pvr
!
      type(pvr_colormap_parameter), intent(in) :: color_param
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: xy_patch(2,3)
      real(kind = kreal), intent(in) :: d_patch(3)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint) :: k_ymin, k_ymid, k_ymax
      integer(kind = kint) :: ix, iy, i_img
      integer(kind = kint) :: ix_min, ix_max
      integer(kind = kint) :: iy_min, iy_mid, iy_max
      integer(kind = kint) :: kmin, kmax
      real(kind = kreal) :: x(2), d(2), d_map
      real(kind = kreal) :: ratio_ymid, ratio_ymax, ratio_x
!
!
      call find_map_path_orientation(xy_patch, k_ymin, k_ymid, k_ymax)
!
      iy_min = int(1 + dble(nypixel-1)                                  &
     &                * (xy_patch(2,k_ymin) - ymin_frame)               &
     &                  / (ymax_frame - ymin_frame))
      iy_mid = int(1 + dble(nypixel-1)                                  &
     &                * (xy_patch(2,k_ymid) - ymin_frame)               &
     &                  / (ymax_frame - ymin_frame))
      iy_max = int(1 + dble(nypixel-1)                                  &
     &                * (xy_patch(2,k_ymax) - ymin_frame)               &
     &                  / (ymax_frame - ymin_frame))
!
      iy_min = max(iy_min,1)
      iy_mid = max(iy_mid,0)
      iy_max = max(iy_max,0)
      do iy = iy_min, iy_mid
        if(iy_max.eq.iy_min .or. iy_mid.eq.iy_min) then
          x(1) = xy_patch(1,k_ymin)
          x(2) = xy_patch(1,k_ymid)
          d(1) = d_patch(k_ymin)
          d(2) = d_patch(k_ymid)
        else
          ratio_ymid = dble(iy-iy_min) / dble(iy_mid-iy_min)
          ratio_ymax = dble(iy-iy_min) / dble(iy_max-iy_min)
          x(1) = (one-ratio_ymid) * xy_patch(1,k_ymin)                  &
     &              + ratio_ymid *  xy_patch(1,k_ymid)
          x(2) = (one-ratio_ymax) * xy_patch(1,k_ymin)                  &
     &              + ratio_ymax *  xy_patch(1,k_ymax)
          d(1) = (one-ratio_ymid) * d_patch(k_ymin)                     &
     &              + ratio_ymid *  d_patch(k_ymid)
          d(2) = (one-ratio_ymax) * d_patch(k_ymin)                     &
     &              + ratio_ymax *  d_patch(k_ymax)
        end if
        if(x(1) .le. x(2)) then
          kmin = 1
          kmax = 2
        else
          kmin = 2
          kmax = 1
        end if
        ix_min = int(1 + dble(nxpixel-1)*(x(kmin) - xmin_frame)         &
     &                  / (xmax_frame - xmin_frame))
        ix_max = int(1 + dble(nxpixel-1)*(x(kmax) - xmin_frame)         &
     &                  / (xmax_frame - xmin_frame))
        ix_min = max(ix_min,1)
        ix_max = max(ix_max,0)
!
        if(ix_max .gt. 0) then
          i_img = ix_min + (iy-1) * nxpixel
          call value_to_rgb(color_param%id_pvr_color(2),                &
     &                      color_param%id_pvr_color(1),                &
     &                      color_param%num_pvr_datamap_pnt,            &
     &                      color_param%pvr_datamap_param,              &
     &                      d(kmin), rgba(1,i_img))
          rgba(4,i_img) = one
        end if
!
        do ix = ix_min+1, ix_max
          i_img = ix + (iy-1) * nxpixel
          ratio_x = dble(ix-ix_min) / dble(ix_max-ix_min)
          d_map = (one - ratio_x) * d(kmin) + ratio_x * d(kmax)
          call value_to_rgb(color_param%id_pvr_color(2),                &
     &                      color_param%id_pvr_color(1),                &
     &                      color_param%num_pvr_datamap_pnt,            &
     &                      color_param%pvr_datamap_param,              &
     &                      d_map, rgba(1,i_img))
          rgba(4,i_img) = one
        end do
      end do
!
      do iy = iy_mid+1, iy_max
        if(iy_max.eq.iy_min) then
          x(1) = xy_patch(1,k_ymid)
          x(2) = xy_patch(1,k_ymax)
          d(1) = d_patch(k_ymid)
          d(2) = d_patch(k_ymax)
        else
          ratio_ymid = dble(iy-iy_mid) / dble(iy_max-iy_mid)
          ratio_ymax = dble(iy-iy_min) / dble(iy_max-iy_min)
          x(1) = (one-ratio_ymid) * xy_patch(1,k_ymid)                  &
     &              + ratio_ymid *  xy_patch(1,k_ymax)
          x(2) = (one-ratio_ymax) * xy_patch(1,k_ymin)                  &
     &              + ratio_ymax *  xy_patch(1,k_ymax)
          d(1) = (one-ratio_ymid) * d_patch(k_ymid)                     &
     &              + ratio_ymid *  d_patch(k_ymax)
          d(2) = (one-ratio_ymax) * d_patch(k_ymin)                     &
     &              + ratio_ymax *  d_patch(k_ymax)
        end if
        if(x(1) .le. x(2)) then
          kmin = 1
          kmax = 2
        else
          kmin = 2
          kmax = 1
        end if
        ix_min = int(1 + dble(nxpixel-1)*(x(kmin) - xmin_frame)         &
     &                  / (xmax_frame - xmin_frame))
        ix_max = int(1 + dble(nxpixel-1)*(x(kmax) - xmin_frame)         &
     &                  / (xmax_frame - xmin_frame))
        ix_min = max(ix_min,1)
        ix_max = max(ix_max,0)
!
        if(ix_max .gt. 0) then
          i_img = ix_min + (iy-1) * nxpixel
          call value_to_rgb(color_param%id_pvr_color(2),                &
     &                      color_param%id_pvr_color(1),                &
     &                      color_param%num_pvr_datamap_pnt,            &
     &                      color_param%pvr_datamap_param,              &
     &                      d(kmin), rgba(1,i_img))
          rgba(4,i_img) = one
        end if
!
        do ix = ix_min+1, ix_max
          i_img = ix + (iy-1) * nxpixel
          ratio_x = dble(ix-ix_min) / dble(ix_max-ix_min)
          d_map = (one - ratio_x) * d(kmin) + ratio_x * d(kmax)
          call value_to_rgb(color_param%id_pvr_color(2),                &
     &                      color_param%id_pvr_color(1),                &
     &                      color_param%num_pvr_datamap_pnt,            &
     &                      color_param%pvr_datamap_param,              &
     &                      d_map, rgba(1,i_img))
          rgba(4,i_img) = one
        end do
      end do
!
      end subroutine fill_triangle_data_on_image
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine fill_map_one_color(nxpixel, nypixel, bg_rgba, rgba)
!
      use t_pvr_colormap_parameter
      use set_color_4_pvr
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: bg_rgba(4)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint) :: i_img, i, j
!
!
!$omp parallel do private(i,j,i_img)
      do j = 1, nypixel
        do i = 1, nxpixel
          i_img = i + (j-1) * nxpixel
          if(rgba(4,i_img) .gt. zero) rgba(1:4,i_img) = bg_rgba(1:4)
        end do
      end do
!$omp end parallel do
!
      end subroutine fill_map_one_color
!
!  ---------------------------------------------------------------------
!
      subroutine fill_background(nxpixel, nypixel, bg_rgba, rgba)
!
      use t_pvr_colormap_parameter
      use set_color_4_pvr
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: bg_rgba(4)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint) :: i_img, i, j
!
!
!$omp parallel do private(i,j,i_img)
      do j = 1, nypixel
        do i = 1, nxpixel
          i_img = i + (j-1) * nxpixel
          if(rgba(4,i_img) .eq. zero) rgba(1:4,i_img) = bg_rgba(1:4)
        end do
      end do
!$omp end parallel do
!
      end subroutine fill_background
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine find_map_path_orientation                              &
     &         (xy_map, k_ymin, k_ymid, k_ymax)
!
      real(kind = kreal), intent(in) :: xy_map(2,num_triangle)
      integer(kind = kint), intent(inout) :: k_ymin, k_ymid, k_ymax
!
!
      if(      xy_map(2,1) .le. xy_map(2,2)                             &
     &   .and. xy_map(2,1) .le. xy_map(2,3)) then
        k_ymin = 1
        if(xy_map(2,2) .le. xy_map(2,3)) then
          k_ymid = 2
          k_ymax = 3
        else
          k_ymid = 3
          k_ymax = 2
        end if
      else if( xy_map(2,2) .le. xy_map(2,3)                             &
     &   .and. xy_map(2,2) .le. xy_map(2,1)) then
        k_ymin = 2
        if(xy_map(2,3) .le. xy_map(2,1)) then
          k_ymid = 3
          k_ymax = 1
        else
          k_ymid = 1
          k_ymax = 3
        end if
      else
        k_ymin = 3
        if(xy_map(2,1) .le. xy_map(2,2)) then
          k_ymid = 1
          k_ymax = 2
        else
          k_ymid = 2
          k_ymax = 1
        end if
      end if
!
      end subroutine find_map_path_orientation
!
!-----------------------------------------------------------------------
!
      end module draw_pixels_on_map
