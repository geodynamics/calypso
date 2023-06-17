!>@file   set_scalar_on_xyz_plane.f90
!!@brief  module set_scalar_on_xyz_plane
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!
!>@brief Subroutines for plane projection
!!@verbatim
!!      subroutine sel_scalar_on_xyz_plane(psf_nod, psf_ele, d_scalar,  &
!!     &                                   map_data, pvr_rgb, map_e)
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!!        type(map_rendering_data), intent(inout) :: map_data
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!        type(map_patches_for_1patch), intent(inout) :: map_e
!!@endverbatim
      module set_scalar_on_xyz_plane
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_phys_data
      use t_map_rendering_data
      use t_map_patch_from_1patch
      use t_pvr_colormap_parameter
      use t_pvr_image_array
!
      implicit  none
!
      private :: set_scalar_on_xy_plane, set_scalar_on_xz_plane
      private :: set_scalar_on_yz_plane
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_scalar_on_xyz_plane(color_param,                   &
     &          psf_nod, psf_ele, d_scalar, map_data, pvr_rgb, map_e)
!
      use draw_pixels_on_map
!
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!
      type(map_rendering_data), intent(inout) :: map_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
!
      if(map_data%iflag_2d_projection_mode .eq. iflag_xy_plane)  then
        call set_scalar_on_xy_plane(color_param, psf_nod, psf_ele,      &
     &     d_scalar, map_data, pvr_rgb, map_e)
      else if(map_data%iflag_2d_projection_mode .eq. iflag_xz_plane)    &
     &                                                            then
        call set_scalar_on_xz_plane(color_param, psf_nod, psf_ele,      &
     &     d_scalar, map_data, pvr_rgb, map_e)
      else if(map_data%iflag_2d_projection_mode .eq. iflag_yz_plane)    &
     &                                                            then
        call set_scalar_on_yz_plane(color_param, psf_nod, psf_ele,      &
     &     d_scalar, map_data, pvr_rgb, map_e)
      end if
!
      end subroutine sel_scalar_on_xyz_plane
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_scalar_on_xy_plane(color_param, psf_nod, psf_ele,  &
     &          d_scalar, map_data, pvr_rgb, map_e)
!
      use set_xyz_plot_from_1patch
      use map_patch_from_1patch
      use draw_pixels_on_map
!
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
!
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele
!
!
      do iele = 1, psf_ele%numele
        call set_xy_plot_from_1patch(psf_nod, psf_ele, d_scalar, iele,  &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1))
        call fill_triangle_data_on_image(color_param,                   &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1),                &
     &      pvr_rgb%rgba_real_gl)
      end do
!
      end subroutine set_scalar_on_xy_plane
!
!  ---------------------------------------------------------------------
!
      subroutine set_scalar_on_xz_plane(color_param, psf_nod, psf_ele,  &
     &          d_scalar, map_data, pvr_rgb, map_e)
!
      use t_map_patch_from_1patch
      use set_xyz_plot_from_1patch
      use map_patch_from_1patch
      use draw_pixels_on_map
!
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
!
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele
!
!
      do iele = 1, psf_ele%numele
        call set_xz_plot_from_1patch(psf_nod, psf_ele, d_scalar, iele,  &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1))
        call fill_triangle_data_on_image(color_param,                   &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1),                &
     &      pvr_rgb%rgba_real_gl)
      end do
!
      end subroutine set_scalar_on_xz_plane
!
!  ---------------------------------------------------------------------
!
      subroutine set_scalar_on_yz_plane(color_param, psf_nod, psf_ele,  &
     &          d_scalar, map_data, pvr_rgb, map_e)
!
      use t_map_patch_from_1patch
      use set_xyz_plot_from_1patch
      use map_patch_from_1patch
      use draw_pixels_on_map
!
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
!
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele
!
!
      do iele = 1, psf_ele%numele
        call set_yz_plot_from_1patch(psf_nod, psf_ele, d_scalar, iele,  &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1))
        call fill_triangle_data_on_image(color_param,                   &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1),                &
     &      pvr_rgb%rgba_real_gl)
      end do
!
      end subroutine set_scalar_on_yz_plane
!
!  ---------------------------------------------------------------------
!
      end module set_scalar_on_xyz_plane
