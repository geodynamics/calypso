!>@file   draw_xyz_plane_isolines.f90
!!@brief  module draw_xyz_plane_isolines
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!
!>@brief Subroutines for plane projection
!!@verbatim
!!      subroutine s_draw_xyz_plane_isolines(psf_nod, psf_ele, d_scalar,&
!!     &          map_data, color_param, pvr_rgb, map_e)
!!      subroutine draw_xyz_plane_zeroline(psf_nod, psf_ele, d_scalar,  &
!!     &          map_data, color_ref, pvr_rgb, map_e)
!!      subroutine sel_draw_isoline_on_xyz_plane                        &
!!     &         (psf_nod, psf_ele, d_scalar, nwidth, idots,            &
!!     &          map_data, d_ref, color_ref, pvr_rgb, map_e)
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!!        integer(kind = kint), intent(in) :: nwidth, idots
!!        real(kind = kreal), intent(in) :: d_ref
!!        real(kind = kreal), intent(in) :: color_ref(4)
!!        type(map_rendering_data), intent(in) :: map_data
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!        type(map_patches_for_1patch), intent(inout) :: map_e
!!@endverbatim
      module draw_xyz_plane_isolines
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
      private :: draw_isoline_on_xy_plane, draw_isoline_on_xz_plane
      private :: draw_isoline_on_yz_plane
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_draw_xyz_plane_isolines(psf_nod, psf_ele, d_scalar,  &
     &          map_data, color_param, pvr_rgb, map_e)
!
      use set_color_4_pvr
      use draw_pixels_on_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!
      type(map_rendering_data), intent(in) :: map_data
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: idots
      integer(kind = kint) :: iline
      real(kind = kreal) :: color_ref(4)
      real(kind = kreal) :: d_min, d_max, d_ref
!
!
      if(map_data%flag_fixed_isoline_range) then
        d_min = map_data%dmin_isoline
        d_max = map_data%dmax_isoline
      else
        d_min = minval(d_scalar)
        d_max = maxval(d_scalar)
      end if
!
      do iline = 0, map_data%num_line-1
        d_ref = d_min + (d_max - d_min)                                 &
     &                 * dble(iline) / dble(map_data%num_line-1)
        if(d_ref .ge. zero) then
          idots = 0
        else
          idots = int(2 * map_data%width_isoline)
        end if
!
        if(map_data%iflag_isoline_color .eq. iflag_white) then
          color_ref(1:4) =   one
        else if(map_data%iflag_isoline_color .eq. iflag_black) then
          color_ref(1:4) =   zero
        else
          call value_to_rgb(color_param%id_pvr_color(2),                &
     &                      color_param%id_pvr_color(1),                &
     &                      color_param%num_pvr_datamap_pnt,            &
     &                      color_param%pvr_datamap_param,              &
     &                      d_ref, color_ref(1))
        end if
        color_ref(4) =   one
!
        call sel_draw_isoline_on_xyz_plane                              &
     &     (psf_nod, psf_ele, d_scalar, int(map_data%width_isoline),    &
     &      idots, map_data, d_ref, color_ref, pvr_rgb, map_e)
      end do
!
      end subroutine s_draw_xyz_plane_isolines
!
!  ---------------------------------------------------------------------
!
      subroutine draw_xyz_plane_zeroline(psf_nod, psf_ele, d_scalar,    &
     &          map_data, color_ref, pvr_rgb, map_e)
!
      use set_color_4_pvr
      use draw_pixels_on_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!
      type(map_rendering_data), intent(in) :: map_data
      real(kind = kreal), intent(in) :: color_ref(4)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: nwidth
!
      nwidth = int(2 * map_data%width_isoline)
      call sel_draw_isoline_on_xyz_plane                                &
     &   (psf_nod, psf_ele, d_scalar, nwidth, izero, map_data,          &
     &    zero, color_ref, pvr_rgb, map_e)
!
      end subroutine draw_xyz_plane_zeroline
!
!  ---------------------------------------------------------------------
!
      subroutine sel_draw_isoline_on_xyz_plane                          &
     &         (psf_nod, psf_ele, d_scalar, nwidth, idots,              &
     &          map_data, d_ref, color_ref, pvr_rgb, map_e)
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
      integer(kind = kint), intent(in) :: nwidth, idots
      real(kind = kreal), intent(in) :: d_ref
      real(kind = kreal), intent(in) :: color_ref(4)
      type(map_rendering_data), intent(in) :: map_data
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
!
      if(map_data%iflag_2d_projection_mode .eq. iflag_xy_plane)  then
        call draw_isoline_on_xy_plane                                   &
     &     (psf_nod, psf_ele, d_scalar, nwidth, idots, map_data,        &
     &      d_ref, color_ref, pvr_rgb, map_e)
      else if(map_data%iflag_2d_projection_mode .eq. iflag_xz_plane)    &
     &                                                            then
        call draw_isoline_on_xz_plane                                   &
     &     (psf_nod, psf_ele, d_scalar, nwidth, idots, map_data,        &
     &      d_ref, color_ref, pvr_rgb, map_e)
      else if(map_data%iflag_2d_projection_mode .eq. iflag_yz_plane)    &
     &                                                            then
        call draw_isoline_on_yz_plane                                   &
     &     (psf_nod, psf_ele, d_scalar, nwidth, idots, map_data,        &
     &      d_ref, color_ref, pvr_rgb, map_e)
      end if
!
      end subroutine sel_draw_isoline_on_xyz_plane
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine draw_isoline_on_xy_plane                               &
     &         (psf_nod, psf_ele, d_scalar, nwidth, idots, map_data,    &
     &          d_ref, color_ref, pvr_rgb, map_e)
!
      use set_xyz_plot_from_1patch
      use draw_isoline_in_triangle
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
!
      integer(kind = kint), intent(in) :: nwidth, idots
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
      real(kind = kreal), intent(in) :: d_ref
      real(kind = kreal), intent(in) :: color_ref(4)
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
        call s_draw_isoline_in_triangle(nwidth, idots,                  &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1),                &
     &      d_ref, color_ref, pvr_rgb%rgba_real_gl)
      end do
!
      end subroutine draw_isoline_on_xy_plane
!
!  ---------------------------------------------------------------------
!
      subroutine draw_isoline_on_xz_plane                               &
     &         (psf_nod, psf_ele, d_scalar, nwidth, idots, map_data,    &
     &          d_ref, color_ref, pvr_rgb, map_e)
!
      use t_map_patch_from_1patch
      use set_xyz_plot_from_1patch
      use draw_isoline_in_triangle
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
!
      integer(kind = kint), intent(in) :: nwidth, idots
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
      real(kind = kreal), intent(in) :: d_ref
      real(kind = kreal), intent(in) :: color_ref(4)
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
        call s_draw_isoline_in_triangle(nwidth, idots,                  &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1),                &
     &      d_ref, color_ref, pvr_rgb%rgba_real_gl)
      end do
!
      end subroutine draw_isoline_on_xz_plane
!
!  ---------------------------------------------------------------------
!
      subroutine draw_isoline_on_yz_plane                               &
     &         (psf_nod, psf_ele, d_scalar, nwidth, idots, map_data,    &
     &          d_ref, color_ref, pvr_rgb, map_e)
!
      use t_map_patch_from_1patch
      use set_xyz_plot_from_1patch
      use draw_isoline_in_triangle
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
!
      integer(kind = kint), intent(in) :: nwidth, idots
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
      real(kind = kreal), intent(in) :: d_ref
      real(kind = kreal), intent(in) :: color_ref(4)
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
        call s_draw_isoline_in_triangle(nwidth, idots,                  &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1),                &
     &      d_ref, color_ref, pvr_rgb%rgba_real_gl)
      end do
!
      end subroutine draw_isoline_on_yz_plane
!
!  ---------------------------------------------------------------------
!
      end module draw_xyz_plane_isolines
