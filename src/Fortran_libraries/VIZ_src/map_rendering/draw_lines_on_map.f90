!>@file   draw_lines_on_map.f90
!!@brief  module draw_lines_on_map
!!
!!@author H. Matsui
!!@date Programmed in July, 2023
!
!>@brief Subroutines to draw lines on map
!!
!!@verbatim
!!      subroutine draw_radius_grid(psf_nod, psf_ele, map_data,         &
!!     &          bg_color, ref_r, pvr_rgb, map_e)
!!      subroutine draw_mapflame(psf_nod, psf_ele, phi_shift, map_data, &
!!     &          bg_color, pvr_rgb, map_e)
!!      subroutine draw_longitude_grid(psf_nod, psf_ele, phi_shift,     &
!!     &          map_data, bg_color, pvr_rgb, map_e)
!!      subroutine draw_latitude_grid(psf_nod, psf_ele, map_data,       &
!!     &          bg_color, pvr_rgb, map_e)
!!      subroutine draw_map_tangent_cyl_grid(psf_nod, psf_ele, map_data,&
!!     &          bg_color, theta_ref, pvr_rgb, map_e)
!!      subroutine draw_med_tangent_cyl_grid(psf_nod, psf_ele, map_data,&
!!     &          bg_color, radius_ICB, pvr_rgb, map_e)
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        type(map_rendering_data), intent(in) :: map_data
!!        real(kind = kreal), intent(in) :: ref_r
!!        real(kind = kreal), intent(in) :: theta_ref(2)
!!        real(kind = kreal), intent(in) :: radius_ICB
!!        real(kind = kreal), intent(in) :: bg_color(4)
!!        real(kind = kreal), intent(in) :: phi_shift(psf_nod%numnod)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!        type(map_patches_for_1patch), intent(inout) :: map_e
!!
!!      subroutine set_flame_color(flag_fill, bg_color, flame_color)
!!        logical, intent(in) :: flag_fill
!!        real(kind = kreal), intent(in) :: bg_color(4)
!!        real(kind = kreal), intent(inout) :: flame_color(4)
!!@endverbatim
      module draw_lines_on_map
!
      use m_precision
      use m_constants
      use t_geometry_data
      use t_map_patch_from_1patch
      use t_map_rendering_data
      use t_pvr_image_array
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine draw_radius_grid(psf_nod, psf_ele, map_data,           &
     &          bg_color, ref_r, pvr_rgb, map_e)
!
      use draw_xyz_plane_isolines
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
      real(kind = kreal), intent(in) :: ref_r
      real(kind = kreal), intent(in) :: bg_color(4)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: nwidth
      real(kind = kreal) :: color_ref(4)
!
      nwidth = int(2 * map_data%width_grid)
      call set_flame_color(map_data%fill_flag, bg_color, color_ref)
      call sel_draw_isoline_on_xyz_plane                                &
     &   (psf_nod, psf_ele, psf_nod%rr(1), nwidth, izero,               &
     &    map_data, ref_r, color_ref, pvr_rgb, map_e)
!
      end subroutine draw_radius_grid
!
!  ---------------------------------------------------------------------
!
      subroutine draw_mapflame(psf_nod, psf_ele, phi_shift, map_data,   &
     &          bg_color, pvr_rgb, map_e)
!
      use draw_aitoff_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
      real(kind = kreal), intent(in) :: phi_shift(psf_nod%numnod)
      real(kind = kreal), intent(in) :: bg_color(4)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: nwidth
      real(kind = kreal) :: pi
      real(kind = kreal) :: color_ref(4)
!
!
      nwidth = int(2 * map_data%width_grid)
      call set_flame_color(map_data%fill_flag, bg_color, color_ref)
!
      pi = four * atan(one)
      call draw_isoline_on_map_image                                    &
     &   (psf_nod, psf_ele, phi_shift(1), map_data, nwidth, izero,      &
     &    (-pi), color_ref, pvr_rgb, map_e)
      call draw_isoline_on_map_image                                    &
     &   (psf_nod, psf_ele, phi_shift(1), map_data, nwidth, izero,      &
     &    pi, color_ref, pvr_rgb, map_e)
!
      end subroutine draw_mapflame
!
!  ---------------------------------------------------------------------
!
      subroutine draw_longitude_grid(psf_nod, psf_ele, phi_shift,       &
     &          map_data, bg_color, pvr_rgb, map_e)
!
      use draw_aitoff_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
      real(kind = kreal), intent(in) :: phi_shift(psf_nod%numnod)
      real(kind = kreal), intent(in) :: bg_color(4)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: idots
      integer(kind = kint) :: ii
      real(kind = kreal) :: phi_ref, pi
      real(kind = kreal) :: color_ref(4)
!
!
      idots = int(2 * map_data%width_grid)
      call set_flame_color(map_data%fill_flag, bg_color, color_ref)
!
      pi = four * atan(one)
      do ii = 1, 5
        phi_ref = pi * dble(ii-3) / 3.0d0
        call draw_isoline_on_map_image                                  &
     &     (psf_nod, psf_ele, phi_shift(1), map_data,                   &
     &      int(map_data%width_grid), idots, phi_ref, color_ref,        &
     &      pvr_rgb, map_e)
      end do
!
      end subroutine draw_longitude_grid
!
!  ---------------------------------------------------------------------
!
      subroutine draw_latitude_grid(psf_nod, psf_ele, map_data,         &
     &          bg_color, pvr_rgb, map_e)
!
      use draw_aitoff_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
      real(kind = kreal), intent(in) :: bg_color(4)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: idots
      integer(kind = kint) :: jj
      real(kind = kreal) :: theta_ref, pi
      real(kind = kreal) :: color_ref(4)
!
      idots = int(2* map_data%width_grid)
      call set_flame_color(map_data%fill_flag, bg_color, color_ref)
!
      pi = four * atan(one)
      do jj = 1, 5
        theta_ref = pi * dble(jj) / 6.0d0
        call draw_isoline_on_map_image                                  &
     &    (psf_nod, psf_ele, psf_nod%theta(1), map_data,                &
     &     int(map_data%width_grid), idots, theta_ref, color_ref,       &
     &     pvr_rgb, map_e)
      end do
!
      end subroutine draw_latitude_grid
!
!  ---------------------------------------------------------------------
!
      subroutine draw_map_tangent_cyl_grid(psf_nod, psf_ele, map_data,  &
     &          bg_color, theta_ref, pvr_rgb, map_e)
!
      use draw_aitoff_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
      real(kind = kreal), intent(in) :: theta_ref(2)
      real(kind = kreal), intent(in) :: bg_color(4)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: idots, nwidth
      real(kind = kreal) :: color_ref(4)
!
      call set_flame_color(map_data%fill_flag, bg_color, color_ref)
!
      nwidth = int(2 * map_data%width_grid)
      idots =  int(4 * map_data%width_grid)
      call draw_isoline_on_map_image                                    &
     &   (psf_nod, psf_ele, psf_nod%theta(1), map_data, nwidth, idots,  &
     &     theta_ref(1), color_ref, pvr_rgb, map_e)
      call draw_isoline_on_map_image                                    &
     &   (psf_nod, psf_ele, psf_nod%theta(1), map_data, nwidth, idots,  &
     &     theta_ref(2), color_ref, pvr_rgb, map_e)
!
      end subroutine draw_map_tangent_cyl_grid
!
!  ---------------------------------------------------------------------
!
      subroutine draw_med_tangent_cyl_grid(psf_nod, psf_ele, map_data,  &
     &          bg_color, radius_ICB, pvr_rgb, map_e)
!
      use draw_xyz_plane_isolines
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
      real(kind = kreal), intent(in) :: radius_ICB
      real(kind = kreal), intent(in) :: bg_color(4)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: idots, nwidth
      real(kind = kreal) :: color_ref(4)
!
      nwidth = int(2 * map_data%width_grid)
      idots =  int(4 * map_data%width_grid)
      call set_flame_color(map_data%fill_flag, bg_color, color_ref)
      call sel_draw_isoline_on_xyz_plane                                &
     &   (psf_nod, psf_ele, psf_nod%xx(1,1), nwidth, idots,             &
     &    map_data, radius_ICB, color_ref, pvr_rgb, map_e)
!
      end subroutine draw_med_tangent_cyl_grid
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_flame_color(flag_fill, bg_color, flame_color)
!
      logical, intent(in) :: flag_fill
      real(kind = kreal), intent(in) :: bg_color(4)
      real(kind = kreal), intent(inout) :: flame_color(4)
!
      if(flag_fill) then
        flame_color(1:3) = zero
        flame_color(4) =   one
      else
        flame_color(1:3) = bg_color(1:3) + (one - two*bg_color(1:3))
        flame_color(4) = one
      end if
!
      end subroutine set_flame_color
!
!  ---------------------------------------------------------------------
!
      end module draw_lines_on_map
