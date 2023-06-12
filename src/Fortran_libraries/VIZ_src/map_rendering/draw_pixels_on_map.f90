!>@file   draw_pixels_on_map.f90
!!@brief  module draw_pixels_on_map
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!
!>@brief Fraw pixels on projected image
!!
!!@verbatim
!!      subroutine fill_triangle_data_on_image                          &
!!     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          nxpixel, nypixel, k_ymin, k_ymid, k_ymax,             &
!!     &          xy_patch, d_patch, d_map, rgba)
!!        real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
!!        real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        integer(kind = kint), intent(in) :: k_ymin, k_ymid, k_ymax
!!        real(kind = kreal), intent(in) :: xy_patch(2,3)
!!        real(kind = kreal), intent(in) :: d_patch(3)
!!        real(kind = kreal), intent(inout) :: d_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!      subroutine map_value_to_rgb(color_param, nxpixel, nypixel,      &
!!     &                            d_map, rgba)
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: d_map(npix)
!!        real(kind = kreal), intent(inout) :: rgba(4,npix)
!!      subroutine fill_background(nxpixel, nypixel, bg_rgba, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: bg_rgba(4)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!
!!      subroutine draw_isoline_on_pixel(nxpixel, nypixel, nwidth,      &
!!     &          idots, d_ref, color_ref, d_map, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        integer(kind = kint), intent(in) :: nwidth, idots
!!        real(kind = kreal), intent(in) :: d_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(in) :: color_ref(4)
!!        real(kind = kreal), intent(in) :: d_ref
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!@endverbatim
      module draw_pixels_on_map
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
      subroutine fill_triangle_data_on_image                            &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, k_ymin, k_ymid, k_ymax,               &
     &          xy_patch, d_patch, d_map, rgba)
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      integer(kind = kint), intent(in) :: k_ymin, k_ymid, k_ymax
      real(kind = kreal), intent(in) :: xy_patch(2,3)
      real(kind = kreal), intent(in) :: d_patch(3)
!
      real(kind = kreal), intent(inout) :: d_map(nxpixel*nypixel)
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint) :: ix, iy, i_img
      integer(kind = kint) :: ix_min, ix_max
      integer(kind = kint) :: iy_min, iy_mid, iy_max
      integer(kind = kint) :: kmin, kmax
      real(kind = kreal) :: x(2), d(2)
      real(kind = kreal) :: ratio_ymid, ratio_ymax, ratio_x
!
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
!
        i_img = ix_min + (iy-1) * nxpixel
        d_map(i_img) =  d(kmin)
        rgba(4,i_img) = one
!
        do ix = ix_min+1, ix_max
          i_img = ix + (iy-1) * nxpixel
          ratio_x = dble(ix-ix_min) / dble(ix_max-ix_min)
          d_map(i_img) = (one - ratio_x) * d(kmin) + ratio_x *  d(kmax)
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
!
        i_img = ix_min + (iy-1) * nxpixel
        d_map(i_img) =  d(kmin)
        rgba(4,i_img) = one
!
        do ix = ix_min+1, ix_max
          i_img = ix + (iy-1) * nxpixel
          ratio_x = dble(ix-ix_min) / dble(ix_max-ix_min)
          d_map(i_img) = (one - ratio_x) * d(kmin) + ratio_x *  d(kmax)
          rgba(4,i_img) = one
        end do
      end do
!
      end subroutine fill_triangle_data_on_image
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine map_value_to_rgb(color_param, nxpixel, nypixel,        &
     &                            d_map, rgba)
!
      use t_pvr_colormap_parameter
      use set_color_4_pvr
!
      type(pvr_colormap_parameter), intent(in) :: color_param
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: d_map(nxpixel*nypixel)
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
      subroutine draw_isoline_on_pixel(nxpixel, nypixel, nwidth,        &
     &          idots, d_ref, color_ref, d_map, rgba)
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      integer(kind = kint), intent(in) :: nwidth, idots
      real(kind = kreal), intent(in) :: d_map(nxpixel*nypixel)
      real(kind = kreal), intent(in) :: color_ref(4)
      real(kind = kreal), intent(in) :: d_ref
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint) :: i_img, i, j, icou, isq, i1, i2, i3, i4
      integer(kind = kint) :: nwidth_xn, nwidth_xp
      integer(kind = kint) :: nwidth_yt, nwidth_yn, nwidth_yp
!
      isq = 2*idots
      nwidth_xn = -(nwidth-1) / 4
      nwidth_xp =  (nwidth+2) / 4
      nwidth_yt = -(nwidth-1) / 2
      nwidth_yn =  (nwidth_yt-1) / 2 + 1
      nwidth_yp =  nwidth / 4
!
!$omp parallel do private(i,j,i_img,icou,i1,i2,i3,i4)
      do j = 1, nypixel
        do i = 1, nxpixel
          i_img = i + (j-1) * nxpixel
          if(rgba(4,i_img) .eq. zero) cycle
          if(isq .gt. 0) then
            if(mod(j,isq).ge.idots .and. mod(i,isq).lt.idots) cycle
            if(mod(j,isq).lt.idots .and. mod(i,isq).ge.idots) cycle
          end if
!
          do icou = nwidth_xn, nwidth_xp
            i1 = max(i+icou,  1) +       (j-1) * nxpixel
            i2 = min(i+icou+1,nxpixel) + (j-1) * nxpixel
            i3 = i + max(j+icou-1,1) *     nxpixel
            i4 = i + min(j+icou,nypixel) * nxpixel
            if((rgba(4,i1)*rgba(4,i2)*rgba(4,i3)*rgba(4,i4))            &
     &                                                 .eq. zero) cycle
            if(   ((d_map(i1)-d_ref)*(d_map(i2)-d_ref)) .le. zero       &
     &       .or. ((d_map(i1)-d_ref)*(d_map(i3)-d_ref)) .le. zero       &
     &       .or. ((d_map(i1)-d_ref)*(d_map(i4)-d_ref)) .le. zero) then
              rgba(1:4,i_img) = color_ref(1:4)
            end if
          end do
!
          do icou = nwidth_yn, nwidth_yp
            i1 = i + max(j+icou-1,1) *     nxpixel
            i2 = i + min(j+icou,nypixel) * nxpixel
            i3 = i + max(j+icou-1,1) *     nxpixel
            i4 = i + min(j+icou,nypixel) * nxpixel
            if((rgba(4,i1)*rgba(4,i2)*rgba(4,i3)*rgba(4,i4))            &
     &                                                 .eq. zero) cycle
            if(   ((d_map(i1)-d_ref)*(d_map(i2)-d_ref)) .le. zero       &
     &       .or. ((d_map(i1)-d_ref)*(d_map(i3)-d_ref)) .le. zero       &
     &       .or. ((d_map(i1)-d_ref)*(d_map(i4)-d_ref)) .le. zero) then
              rgba(1:4,i_img) = color_ref(1:4)
            end if
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine draw_isoline_on_pixel
!
!  ---------------------------------------------------------------------
!
      end module draw_pixels_on_map
