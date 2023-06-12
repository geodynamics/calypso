!>@file   draw_aitoff_map.f90
!!@brief  module draw_aitoff_map
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!      subroutine set_scalar_on_map_image(psf_nod, psf_ele, psf_phys,  &
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
!!@endverbatim
      module draw_aitoff_map
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
      subroutine set_scalar_on_map_image(psf_nod, psf_ele, psf_phys,    &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, npix, d_map, rgba, map_e)
!
      use t_geometry_data
      use t_phys_data
      use t_map_patch_from_1patch
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
      integer(kind = kint) :: iele, i
      integer(kind = kint) :: k_ymin, k_ymid, k_ymax
!
!
      do iele = 1, psf_ele%numele
        call s_set_map_patch_from_1patch(iele,                          &
     &      psf_nod%numnod, psf_ele%numele, psf_nod%xx, psf_ele%ie,     &
     &      ione, psf_phys%d_fld(1,1), map_e%n_map_patch,               &
     &      map_e%x_map_patch, map_e%d_map_patch(1,1))
!
        do i = 1, map_e%n_map_patch
          call set_sph_position_4_map_patch                             &
     &       (map_e%x_map_patch(1,1,i), map_e%rtp_map_patch(1,1,i))
          call patch_to_aitoff(map_e%rtp_map_patch(1,1,i),              &
     &                         map_e%xy_map(1,1,i))
!
          call find_map_path_orientation(map_e%xy_map(1,1,i),           &
     &                                   k_ymin, k_ymid, k_ymax)
          call fill_triangle_data_on_image                              &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, k_ymin, k_ymid, k_ymax,               &
     &          map_e%xy_map(1,1,i), map_e%d_map_patch(1,i),            &
     &          d_map, rgba)
        end do
      end do
!
      end subroutine set_scalar_on_map_image
!
!  ---------------------------------------------------------------------
!
      end module draw_aitoff_map
