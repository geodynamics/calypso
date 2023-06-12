!>@file   set_scalar_on_xyz_plane.f90
!!@brief  module set_scalar_on_xyz_plane
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!
!>@brief Subroutines for plane projection
!!@verbatim
!!      subroutine set_scalar_on_xy_plane(psf_nod, psf_ele, psf_phys,   &
!!     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          nxpixel, nypixel, npix, d_map, rgba, map_e)
!!      subroutine set_scalar_on_xz_plane(psf_nod, psf_ele, psf_phys,   &
!!     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          nxpixel, nypixel, npix, d_map, rgba, map_e)
!!      subroutine set_scalar_on_yz_plane(psf_nod, psf_ele, psf_phys,   &
!!     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          nxpixel, nypixel, npix, d_map, rgba, map_e)
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        type(phys_data), intent(in) :: psf_phys
!!        real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
!!        real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!!        real(kind = kreal), intent(inout) :: d_map(npix)
!!        real(kind = kreal), intent(inout) :: rgba(4,npix)
!!        type(map_patches_for_1patch), intent(inout) :: map_e
!!        integer(kind = kint) :: iele
!!        integer(kind = kint) :: k_ymin, k_ymid, k_ymax
!!@endverbatim
      module set_scalar_on_xyz_plane
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_phys_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_scalar_on_xy_plane(psf_nod, psf_ele, psf_phys,     &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, npix, d_map, rgba, map_e)
!
      use t_map_patch_from_1patch
      use set_xyz_plot_from_1patch
      use map_patch_from_1patch
      use draw_pixels_on_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(phys_data), intent(in) :: psf_phys
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!
      real(kind = kreal), intent(inout) :: d_map(npix)
      real(kind = kreal), intent(inout) :: rgba(4,npix)
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele, k_ymin, k_ymid, k_ymax
!
!
      do iele = 1, psf_ele%numele
        call set_xy_plot_from_1patch(psf_nod, psf_ele, psf_phys, iele,  &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1))
        call find_map_path_orientation(map_e%xy_map(1,1,1),             &
     &                                   k_ymin, k_ymid, k_ymax)
        call fill_triangle_data_on_image                                &
     &       (xmin_frame, xmax_frame, ymin_frame, ymax_frame,           &
     &        nxpixel, nypixel, k_ymin, k_ymid, k_ymax,                 &
     &        map_e%xy_map(1,1,1), map_e%d_map_patch(1,1),              &
     &        d_map, rgba)
      end do
!
      end subroutine set_scalar_on_xy_plane
!
!  ---------------------------------------------------------------------
!
      subroutine set_scalar_on_xz_plane(psf_nod, psf_ele, psf_phys,     &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, npix, d_map, rgba, map_e)
!
      use t_map_patch_from_1patch
      use set_xyz_plot_from_1patch
      use map_patch_from_1patch
      use draw_pixels_on_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(phys_data), intent(in) :: psf_phys
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!
      real(kind = kreal), intent(inout) :: d_map(npix)
      real(kind = kreal), intent(inout) :: rgba(4,npix)
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele, k_ymin, k_ymid, k_ymax
!
!
      do iele = 1, psf_ele%numele
        call set_xz_plot_from_1patch(psf_nod, psf_ele, psf_phys, iele,  &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1))
        call find_map_path_orientation(map_e%xy_map(1,1,1),             &
     &                                   k_ymin, k_ymid, k_ymax)
        call fill_triangle_data_on_image                                &
     &       (xmin_frame, xmax_frame, ymin_frame, ymax_frame,           &
     &        nxpixel, nypixel, k_ymin, k_ymid, k_ymax,                 &
     &        map_e%xy_map(1,1,1), map_e%d_map_patch(1,1),              &
     &        d_map, rgba)
      end do
!
      end subroutine set_scalar_on_xz_plane
!
!  ---------------------------------------------------------------------
!
      subroutine set_scalar_on_yz_plane(psf_nod, psf_ele, psf_phys,     &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, npix, d_map, rgba, map_e)
!
      use t_map_patch_from_1patch
      use set_xyz_plot_from_1patch
      use map_patch_from_1patch
      use draw_pixels_on_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(phys_data), intent(in) :: psf_phys
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!
      real(kind = kreal), intent(inout) :: d_map(npix)
      real(kind = kreal), intent(inout) :: rgba(4,npix)
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele, k_ymin, k_ymid, k_ymax
!
!
      do iele = 1, psf_ele%numele
        call set_yz_plot_from_1patch(psf_nod, psf_ele, psf_phys, iele,  &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1))
        call find_map_path_orientation(map_e%xy_map(1,1,1),             &
     &                                   k_ymin, k_ymid, k_ymax)
        call fill_triangle_data_on_image                                &
     &       (xmin_frame, xmax_frame, ymin_frame, ymax_frame,           &
     &        nxpixel, nypixel, k_ymin, k_ymid, k_ymax,                 &
     &        map_e%xy_map(1,1,1), map_e%d_map_patch(1,1),              &
     &        d_map, rgba)
      end do
!
      end subroutine set_scalar_on_yz_plane
!
!  ---------------------------------------------------------------------
!
      end module set_scalar_on_xyz_plane
