!>@file   draw_lines_on_map.f90
!!@brief  module draw_lines_on_map
!!
!!@author H. Matsui
!!@date Programmed in July, 2023
!
!>@brief Subroutines to draw lines on map
!!
!!@verbatim
!!      subroutine draw_isolines(nxpixel, nypixel, color_param,         &
!!     &                         nline, d_map, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel, nline
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        real(kind = kreal), intent(in) :: d_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!      subroutine draw_zeroline(nxpixel, nypixel, d_map, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: d_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!
!!      subroutine draw_radius_grid(nxpixel, nypixel, ref_r, r_map, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: ref_r
!!        real(kind = kreal), intent(in) :: r_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!      subroutine draw_mapflame(nxpixel, nypixel, phi_map, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: phi_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!      subroutine draw_longitude_grid(nxpixel, nypixel, phi_map, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: phi_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!      subroutine draw_latitude_grid(nxpixel, nypixel, theta_map, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: theta_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!
!!      subroutine draw_map_tangent_cyl_grid                            &
!!     &         (nxpixel, nypixel, theta_ref, theta_map, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: theta_ref(2)
!!        real(kind = kreal), intent(in) :: theta_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!      subroutine draw_med_tangent_cyl_grid                            &
!!     &         (nxpixel, nypixel, radius_ICB, x_map, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: radius_ICB
!!        real(kind = kreal), intent(in) :: x_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
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
      subroutine draw_isolines(nxpixel, nypixel, color_param,           &
     &                         nline, d_map, rgba)
!
      use t_pvr_colormap_parameter
      use set_color_4_pvr
      use draw_pixels_on_map
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel, nline
      type(pvr_colormap_parameter), intent(in) :: color_param
      real(kind = kreal), intent(in) :: d_map(nxpixel*nypixel)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: nwidth = 3
      integer(kind = kint) :: idots
      integer(kind = kint) :: iline, imax
      real(kind = kreal) :: color_ref(4)
      real(kind = kreal) :: d_ref, dmin, dmax
!
!
      imax = color_param%num_pvr_datamap_pnt
      do iline = 1, nline
        dmin = color_param%pvr_datamap_param(1,1)
        dmax = color_param%pvr_datamap_param(1,imax)
        d_ref = dble(iline-1) * (dmax - dmin) / dble(nline-1)
        if(d_ref .le. zero) then
          idots = 0
        else
          idots = 2
        end if
        call value_to_rgb(color_param%id_pvr_color(2),                  &
     &                    color_param%id_pvr_color(1),                  &
     &                    color_param%num_pvr_datamap_pnt,              &
     &                    color_param%pvr_datamap_param,                &
     &                    d_ref, color_ref(1))
        color_ref(1:3) = half * color_ref(1:3)
        call draw_isoline_on_pixel(nxpixel, nypixel, nwidth,            &
     &      idots, d_ref, color_ref, d_map, rgba)
      end do
!
      end subroutine draw_isolines
!
!  ---------------------------------------------------------------------
!
      subroutine draw_zeroline(nxpixel, nypixel, d_map, rgba)
!
      use draw_pixels_on_map
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: d_map(nxpixel*nypixel)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: idots =  0
      integer(kind = kint), parameter :: nwidth = 4
      real(kind = kreal), parameter                                     &
     &                   :: color_ref(4) = (/zero,zero,zero,one/)
!
!
      call draw_isoline_on_pixel(nxpixel, nypixel, nwidth,              &
     &    idots, zero, color_ref, d_map, rgba)
!
      end subroutine draw_zeroline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine draw_radius_grid(nxpixel, nypixel, ref_r, r_map, rgba)
!
      use draw_pixels_on_map
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: ref_r
      real(kind = kreal), intent(in) :: r_map(nxpixel*nypixel)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: idots =  0
      integer(kind = kint), parameter :: nwidth = 2
      real(kind = kreal), parameter                                     &
     &                   :: color_ref(4) = (/zero,zero,zero,one/)
!
!
      call draw_isoline_on_pixel(nxpixel, nypixel, nwidth, idots,       &
     &    ref_r, color_ref, r_map, rgba)
!
      end subroutine draw_radius_grid
!
!  ---------------------------------------------------------------------
!
      subroutine draw_mapflame(nxpixel, nypixel, phi_map, rgba)
!
      use draw_pixels_on_map
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: phi_map(nxpixel*nypixel)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: idots =  0
      integer(kind = kint), parameter :: nwidth = 2
      real(kind = kreal), parameter                                     &
     &                   :: color_ref(4) = (/zero,zero,zero,one/)
      real(kind = kreal) :: pi
!
!
      pi = four * atan(one)
      call draw_isoline_on_pixel(nxpixel, nypixel, nwidth,              &
     &    idots, (-pi), color_ref, phi_map, rgba)
      call draw_isoline_on_pixel(nxpixel, nypixel, nwidth,              &
     &    idots, pi, color_ref, phi_map, rgba)
!
      end subroutine draw_mapflame
!
!  ---------------------------------------------------------------------
!
      subroutine draw_longitude_grid(nxpixel, nypixel, phi_map, rgba)
!
      use draw_pixels_on_map
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: phi_map(nxpixel*nypixel)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: idots =  3
      integer(kind = kint), parameter :: nwidth = 1
      real(kind = kreal), parameter                                     &
     &                   :: color_ref(4) = (/zero,zero,zero,one/)
      integer(kind = kint) :: ii
      real(kind = kreal) :: phi_ref, pi
!
!
      pi = four * atan(one)
      do ii = 1, 5
        phi_ref = pi * dble(ii-3) / 3.0d0
        call draw_isoline_on_pixel(nxpixel, nypixel, nwidth, idots,     &
     &      phi_ref, color_ref, phi_map, rgba)
      end do
!
      end subroutine draw_longitude_grid
!
!  ---------------------------------------------------------------------
!
      subroutine draw_latitude_grid(nxpixel, nypixel, theta_map, rgba)
!
      use draw_pixels_on_map
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: theta_map(nxpixel*nypixel)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: idots =  3
      integer(kind = kint), parameter :: nwidth = 1
      real(kind = kreal), parameter                                     &
     &                   :: color_ref(4) = (/zero,zero,zero,one/)
      integer(kind = kint) :: jj
      real(kind = kreal) :: theta_ref, pi
!
!
      pi = four * atan(one)
      do jj = 1, 5
        theta_ref = pi * dble(jj) / 6.0d0
        call draw_isoline_on_pixel(nxpixel, nypixel, nwidth, idots,     &
     &      theta_ref, color_ref, theta_map, rgba)
      end do
!
      end subroutine draw_latitude_grid
!
!  ---------------------------------------------------------------------
!
      subroutine draw_map_tangent_cyl_grid                              &
     &         (nxpixel, nypixel, theta_ref, theta_map, rgba)
!
      use draw_pixels_on_map
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: theta_ref(2)
      real(kind = kreal), intent(in) :: theta_map(nxpixel*nypixel)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: idots =  4
      integer(kind = kint), parameter :: nwidth = 3
      real(kind = kreal), parameter                                     &
     &                   :: color_ref(4) = (/zero,zero,zero,one/)
!
!
      call draw_isoline_on_pixel(nxpixel, nypixel, nwidth, idots,       &
     &    theta_ref(1), color_ref, theta_map, rgba)
      call draw_isoline_on_pixel(nxpixel, nypixel, nwidth, idots,       &
     &    theta_ref(2), color_ref, theta_map, rgba)
!
      end subroutine draw_map_tangent_cyl_grid
!
!  ---------------------------------------------------------------------
!
      subroutine draw_med_tangent_cyl_grid                              &
     &         (nxpixel, nypixel, radius_ICB, x_map, rgba)
!
      use draw_pixels_on_map
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: radius_ICB
      real(kind = kreal), intent(in) :: x_map(nxpixel*nypixel)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: idots =  4
      integer(kind = kint), parameter :: nwidth = 3
      real(kind = kreal), parameter                                     &
     &                   :: color_ref(4) = (/zero,zero,zero,one/)
!
!
      call draw_isoline_on_pixel(nxpixel, nypixel, nwidth, idots,       &
     &    (-radius_ICB), color_ref, x_map, rgba)
      call draw_isoline_on_pixel(nxpixel, nypixel, nwidth, idots,       &
     &    radius_ICB, color_ref, x_map, rgba)
!
      end subroutine draw_med_tangent_cyl_grid
!
!  ---------------------------------------------------------------------
!
      end module draw_lines_on_map
