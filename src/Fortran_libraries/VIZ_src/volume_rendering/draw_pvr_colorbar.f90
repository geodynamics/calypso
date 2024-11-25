!>@file   draw_pvr_colorbar.f90
!!@brief  module draw_pvr_colorbar
!!
!!@author H. Matsui
!!@date Programmed in 2008
!
!
!>@brief  Draw color bar for PVR
!!
!!@verbatim
!!      subroutine set_pvr_timelabel(time, num_pixel, n_pvr_pixel,      &
!!     &                             cbar_param, rgba_gl)
!!      subroutine set_pvr_colorbar(num_pixel, n_pvr_pixel,             &
!!     &         color_param, cbar_param, rgba_gl)
!!       type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_colorbar_parameter), intent(in) :: cbar_param
!!@endverbatim
!
      module  draw_pvr_colorbar
!
      use m_precision
!
      use m_constants
      use t_pvr_colormap_parameter
!
      implicit none
!
!
      private :: draw_bottom_pvr_colorbar, gen_bottom_colormark
      private :: draw_left_pvr_colorbar, gen_right_colormark
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_timelabel(time, num_pixel, n_pvr_pixel,        &
     &                             cbar_param, rgba_gl)
!
      use draw_pvr_colorbar_nums
!
      integer(kind = kint), intent(in) :: num_pixel
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
      real(kind = kreal), intent(in) :: time
!
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
!
      real(kind = kreal), intent(inout)  :: rgba_gl(4,num_pixel)
!
!
      call gen_time_label(cbar_param%iscale_font, time,                 &
     &                    n_pvr_pixel, num_pixel, rgba_gl)
!
      end subroutine set_pvr_timelabel
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_colorbar(num_pixel, n_pvr_pixel,               &
     &         color_param, cbar_param, rgba_gl)
!
      integer(kind = kint), intent(in) :: num_pixel
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
!
!      type(pvr_domain_outline), intent(in) :: outline
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
!
      real(kind = kreal), intent(inout)  :: rgba_gl(4,num_pixel)
!
!
!      if(cbar_param%cbar_range(2) .le. cbar_param%cbar_range(1)) then
!        cbar_param%cbar_range(1:2) = outline%d_minmax_pvr(1:2)
!      end if
!
      if(cbar_param%flag_pvr_cbar_bottom) then
        call draw_bottom_pvr_colorbar(cbar_param%iflag_opacity,         &
     &      cbar_param%iflag_pvr_cbar_nums,                             &
     &      cbar_param%iflag_pvr_zero_mark,                             &
     &      cbar_param%iscale_font, cbar_param%ntick_pvr_colorbar,      &
     &      cbar_param%cbar_range(1), n_pvr_pixel,                      &
     &      num_pixel, color_param, rgba_gl)
       return
      else
        call draw_left_pvr_colorbar(cbar_param%iflag_opacity,           &
     &      cbar_param%iflag_pvr_cbar_nums,                             &
     &      cbar_param%iflag_pvr_zero_mark,                             &
     &      cbar_param%iscale_font, cbar_param%ntick_pvr_colorbar,      &
     &      cbar_param%cbar_range(1), n_pvr_pixel,                      &
     &      num_pixel, color_param, rgba_gl)
      end if
!
      end subroutine set_pvr_colorbar
!
!  ---------------------------------------------------------------------
!
      subroutine draw_bottom_pvr_colorbar                               &
     &         (iflag_opacity, iflag_cbar_numeric, iflag_zero_mark,     &
     &         iscale, num_of_scale, c_minmax, npix_img,                &
     &         ntot_pix, color_param, dimage)
!
      use draw_pvr_colorbar_nums
!
      integer(kind = kint), intent(in) :: iflag_opacity
      integer(kind = kint), intent(in) :: iflag_cbar_numeric
      integer(kind = kint), intent(in) :: iflag_zero_mark
      real(kind = kreal), intent(in) :: c_minmax(2)
      integer(kind = kint), intent(in) :: npix_img(2)
      integer(kind = kint), intent(in) :: ntot_pix
!
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      integer(kind = kint), intent(in) :: num_of_scale
      integer(kind = kint), intent(in) :: iscale
!
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      integer(kind = kint):: isleeve_bar
!
!
      isleeve_bar = l_bar_width() + 6 + 8 * 9
      isleeve_bar = isleeve_bar + 8                                     &
     &                  - mod((isleeve_bar-ione),ifour)
!
      call gen_bottom_colormark(iscale, c_minmax, npix_img,             &
     &    isleeve_bar, ntot_pix, iflag_opacity, dimage, color_param)
!
      if(iflag_cbar_numeric .gt. 0) then
        call gen_bottom_cbar_label(iscale, num_of_scale, c_minmax,      &
     &     npix_img, isleeve_bar, ntot_pix, dimage)
!
        if(iflag_zero_mark .gt. 0) then
          call gen_bottom_zero_label(iscale, c_minmax, npix_img,        &
     &        isleeve_bar, ntot_pix, dimage)
        end if
      end if
!
      end  subroutine draw_bottom_pvr_colorbar
!
!  ---------------------------------------------------------------------
!
      subroutine draw_left_pvr_colorbar                                 &
     &         (iflag_opacity, iflag_cbar_numeric, iflag_zero_mark,     &
     &         iscale, num_of_scale, c_minmax, npix_img,                &
     &         ntot_pix, color_param, dimage)
!
      use draw_pvr_colorbar_nums
!
      integer(kind = kint), intent(in) :: iflag_opacity
      integer(kind = kint), intent(in) :: iflag_cbar_numeric
      integer(kind = kint), intent(in) :: iflag_zero_mark
      real(kind = kreal), intent(in) :: c_minmax(2)
      integer(kind = kint), intent(in) :: npix_img(2)
      integer(kind = kint), intent(in) :: ntot_pix
!
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      integer(kind = kint), intent(in) :: num_of_scale
      integer(kind = kint), intent(in) :: iscale
!
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      integer(kind = kint):: isleeve_bar
!
!
      isleeve_bar = (l_bar_width() + 6 + 8 * 9) * iscale
      isleeve_bar = isleeve_bar + ithree                                &
     &                  - mod((isleeve_bar-ione),ifour) 
!
      call gen_right_colormark(iscale, c_minmax, npix_img,              &
     &    isleeve_bar, ntot_pix, iflag_opacity, dimage, color_param)
!
      if(iflag_cbar_numeric .gt. 0) then
        call gen_right_cbar_label(iscale, num_of_scale, c_minmax,       &
     &      npix_img, isleeve_bar, ntot_pix, dimage)
!
        if(iflag_zero_mark .gt. 0) then
          call gen_right_zero_label(iscale, c_minmax, npix_img,         &
     &        isleeve_bar, ntot_pix, dimage)
        end if
      end if
!
      end  subroutine draw_left_pvr_colorbar
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gen_bottom_colormark                                   &
     &         (iscale, c_minmax, npix_img, isleeve_bar,                &
     &          ntot_pix, iflag_opacity, dimage, color_param)
!
      use set_color_4_pvr
      use set_rgba_4_each_pixel
      use draw_pvr_colorbar_nums
!
      integer(kind = kint), intent(in) :: iflag_opacity
      integer(kind = kint), intent(in) :: iscale
      real(kind = kreal), intent(in) :: c_minmax(2)
      integer(kind = kint), intent(in) :: isleeve_bar
      integer(kind = kint), intent(in) :: npix_img(2)
      integer(kind = kint), intent(in) :: ntot_pix
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      real(kind = kreal) :: anb_opacity, opa_current
      real(kind = kreal) :: value, color(3)
      integer(kind = kint) :: i, j, inod
      integer(kind = kint) :: num_of_features
!
      integer(kind = kint) :: ist_h, jst_h, ied_h, jed_h
!
!
      call corners_4_bottom_colorbar                                    &
     &   (iscale, npix_img, isleeve_bar, ist_h, jst_h, ied_h, jed_h)
!
      num_of_features = color_param%num_opacity_pnt
      anb_opacity = color_param%pvr_opacity_param(1,num_of_features)
      do i = ist_h, ied_h
        value = c_minmax(1) + (c_minmax(2)-c_minmax(1))                 &
     &                       * dble(i-ist_h) / dble(ied_h-ist_h)
!
        if(iflag_opacity .gt. 0) then
          call compute_opacity                                          &
     &     (color_param%id_pvr_color(3), anb_opacity,                   &
     &      color_param%num_opacity_pnt, color_param%pvr_opacity_param, &
     &      value, opa_current)
          opa_current = opa_current / color_param%pvr_max_opacity
        else
          opa_current = 1.0d0
        end if
!
        call value_to_rgb                                               &
     &     (color_param%id_pvr_color(2), color_param%id_pvr_color(1),   &
     &      color_param%num_pvr_datamap_pnt,                            &
     &      color_param%pvr_datamap_param, value, color)
!
        do j = jst_h, jst_h+iscale*l_bar_width()/2-1
          inod = (j-1)*npix_img(1) + i
          dimage(1:3,inod) = color(1:3) * opa_current
          dimage(4,inod) = one
        end do
        do j = jst_h+iscale*l_bar_width()/2, jst_h+iscale*l_bar_width()
          inod = (j-1)*npix_img(1) + i
          dimage(1:3,inod) = color(1:3)
          dimage(4,inod) = one
        end do
      end do
!
      do i = -iscale/4, (iscale+1)/4
        do j = jst_h, jed_h
          inod = (j-1)*npix_img(1) + ist_h+i
          dimage(1:4,inod) = one
          inod = (j-1)*npix_img(1) + ied_h+i
          dimage(1:4,inod) = one
        end do
      end do
!
      do j = -iscale/4, (iscale+1)/4
        do i = ist_h, ied_h
          inod = i + (jst_h+j-1)*npix_img(1)
          dimage(1:4,inod) = one
          inod = i + (jed_h+j  )*npix_img(1)
          dimage(1:4,inod) = one
        end do
      end do
!
      end subroutine gen_bottom_colormark
!
!  ---------------------------------------------------------------------
!
      subroutine gen_right_colormark                                    &
     &         (iscale, c_minmax, npix_img, isleeve_bar,                &
     &          ntot_pix, iflag_opacity, dimage, color_param)
!
      use set_color_4_pvr
      use set_rgba_4_each_pixel
      use draw_pvr_colorbar_nums
!
      integer(kind = kint), intent(in) :: iflag_opacity
      integer(kind = kint), intent(in) :: iscale
      real(kind = kreal), intent(in) :: c_minmax(2)
      integer(kind = kint), intent(in) :: isleeve_bar
      integer(kind = kint), intent(in) :: npix_img(2)
      integer(kind = kint), intent(in) :: ntot_pix
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      real(kind = kreal) :: anb_opacity, opa_current
      real(kind = kreal) :: value, color(3)
      integer(kind = kint) :: i, j, inod
      integer(kind = kint) :: num_of_features
      integer(kind = kint) :: ist, jst, ied, jed
!
!
      call corners_4_right_colorbar                                     &
     &   (iscale, npix_img, isleeve_bar, ist, jst, ied, jed)
!
      num_of_features = color_param%num_opacity_pnt
      anb_opacity = color_param%pvr_opacity_param(1,num_of_features)
      do j = jst, jed
        value = c_minmax(1) + (c_minmax(2)-c_minmax(1))                 &
     &                         * dble(j-jst) / dble(jed-jst)
!
        if(iflag_opacity .gt. 0) then
          call compute_opacity                                          &
     &     (color_param%id_pvr_color(3), anb_opacity,                   &
     &      color_param%num_opacity_pnt, color_param%pvr_opacity_param, &
     &      value, opa_current)
          opa_current = opa_current / color_param%pvr_max_opacity
        else
          opa_current = 1.0d0
        end if
!
        call value_to_rgb                                               &
     &     (color_param%id_pvr_color(2), color_param%id_pvr_color(1),   &
     &      color_param%num_pvr_datamap_pnt,                            &
     &      color_param%pvr_datamap_param, value, color)
!
        do i = ist, ist+iscale*l_bar_width()/2-1
          inod = j*npix_img(1) + i + 1
          dimage(1:3,inod) = color(1:3)
          dimage(4,inod) = one
        end do
        do i = ist+iscale*l_bar_width()/2, ist+iscale*l_bar_width()-1
          inod = j*npix_img(1) + i + 1
          dimage(1:3,inod) = color(1:3) * opa_current
          dimage(4,inod) = one
        end do
      end do
!
      do i = -iscale/4, (iscale+1)/4
        do j = jst, jed
          inod = j*npix_img(1) + ist + i
          dimage(1:4,inod) = one
          inod = j*npix_img(1) + ied + i + 1
          dimage(1:4,inod) = one
        end do
      end do
!
      do j = -iscale/4, (iscale+1)/4
        do i = ist-1, ied
          inod = (jst+j)*npix_img(1) + i + 1
          dimage(1:4,inod) = one
          inod = (jed+j)*npix_img(1) + i + 1
          dimage(1:4,inod) = one
        end do
      end do
!
      end subroutine gen_right_colormark
!
!  ---------------------------------------------------------------------
!
      end module  draw_pvr_colorbar
