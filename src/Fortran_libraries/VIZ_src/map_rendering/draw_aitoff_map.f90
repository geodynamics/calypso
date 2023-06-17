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
!!      subroutine set_scalar_on_map_image(color_param,                 &
!!     &          psf_nod, psf_ele, d_scalar, map_data, pvr_rgb, map_e)
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        type(map_rendering_data), intent(in) :: map_data
!!        real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!        type(map_patches_for_1patch), intent(inout) :: map_e
!!      subroutine draw_aitoff_map_isolines(psf_nod, psf_ele, d_scalar, &
!!     &          map_data, color_param, pvr_rgb, map_e)
!!      subroutine draw_aitoff_map_zeroline(psf_nod, psf_ele, d_scalar, &
!!     &          map_data, color_ref, pvr_rgb, map_e)
!!      subroutine draw_isoline_on_map_image                            &
!!     &         (psf_nod, psf_ele, d_scalar, map_data, nwidth, idots,  &
!!     &          d_ref, color_ref, pvr_rgb, map_e)
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        type(map_rendering_data), intent(in) :: map_data
!!        real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!!        integer(kind = kint), intent(in) :: nwidth, idots
!!        real(kind = kreal), intent(in) :: d_ref
!!        real(kind = kreal), intent(in) :: color_ref(4)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!        type(map_patches_for_1patch), intent(inout) :: map_e
!!@endverbatim
      module draw_aitoff_map
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_phys_data
      use t_pvr_image_array
      use t_map_rendering_data
      use t_map_patch_from_1patch
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_scalar_on_map_image(color_param,                   &
     &          psf_nod, psf_ele, d_scalar, map_data, pvr_rgb, map_e)
!
      use map_patch_from_1patch
      use draw_pixels_on_map
!
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele, i
!
!
      do iele = 1, psf_ele%numele
        call s_set_map_patch_from_1patch(iele,                          &
     &      psf_nod%numnod, psf_ele%numele, psf_nod%xx, psf_ele%ie,     &
     &      ione, d_scalar, map_e%n_map_patch,                          &
     &      map_e%x_map_patch, map_e%d_map_patch(1,1))
!
        do i = 1, map_e%n_map_patch
          call set_sph_position_4_map_patch                             &
     &       (map_e%x_map_patch(1,1,i), map_e%rtp_map_patch(1,1,i))
          call patch_to_aitoff(map_e%rtp_map_patch(1,1,i),              &
     &                         map_e%xy_map(1,1,i))
          call fill_triangle_data_on_image(color_param,                 &
     &        map_data%xmin_frame, map_data%xmax_frame,                 &
     &        map_data%ymin_frame, map_data%ymax_frame,                 &
     &        pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),             &
     &        map_e%xy_map(1,1,i), map_e%d_map_patch(1,i),              &
     &        pvr_rgb%rgba_real_gl)
        end do
      end do
!
      end subroutine set_scalar_on_map_image
!
!  ---------------------------------------------------------------------
!
      subroutine draw_aitoff_map_isolines(psf_nod, psf_ele, d_scalar,   &
     &          map_data, color_param, pvr_rgb, map_e)
!
      use set_color_4_pvr
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
!
        color_ref(4) =   one
        call draw_isoline_on_map_image(psf_nod, psf_ele, d_scalar,      &
     &      map_data, int(map_data%width_isoline), idots,               &
     &      d_ref, color_ref, pvr_rgb, map_e)
      end do
!
      end subroutine draw_aitoff_map_isolines
!
!  ---------------------------------------------------------------------
!
      subroutine draw_aitoff_map_zeroline(psf_nod, psf_ele, d_scalar,   &
     &          map_data, color_ref, pvr_rgb, map_e)
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
      call draw_isoline_on_map_image(psf_nod, psf_ele, d_scalar,        &
     &    map_data, nwidth, izero, zero, color_ref, pvr_rgb, map_e)
!
      end subroutine draw_aitoff_map_zeroline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine draw_isoline_on_map_image                              &
     &         (psf_nod, psf_ele, d_scalar, map_data, nwidth, idots,    &
     &          d_ref, color_ref, pvr_rgb, map_e)
!
      use map_patch_from_1patch
      use draw_isoline_in_triangle
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!
      integer(kind = kint), intent(in) :: nwidth, idots
      real(kind = kreal), intent(in) :: d_ref
      real(kind = kreal), intent(in) :: color_ref(4)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele, i
!
      real(kind=kreal) :: xy_map(2,3,3)
      real(kind=kreal) :: d_map_patch(3,3)
!
!
      do iele = 1, psf_ele%numele
        call s_set_map_patch_from_1patch(iele,                          &
     &      psf_nod%numnod, psf_ele%numele, psf_nod%xx, psf_ele%ie,     &
     &      ione, d_scalar(1), map_e%n_map_patch,                       &
     &      xy_map(1,1,1), d_map_patch(1,1))
!
        do i = 1, map_e%n_map_patch
          call set_sph_position_4_map_patch                             &
     &       (xy_map(1,1,i), map_e%rtp_map_patch(1,1,i))
          call patch_to_aitoff(map_e%rtp_map_patch(1,1,i),              &
     &                         map_e%xy_map(1,1,i))
!
          call s_draw_isoline_in_triangle(nwidth, idots,                &
     &        map_data%xmin_frame, map_data%xmax_frame,                 &
     &        map_data%ymin_frame, map_data%ymax_frame,                 &
     &        pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),             &
     &        map_e%xy_map(1,1,i), d_map_patch(1,i), d_ref, color_ref,  &
     &        pvr_rgb%rgba_real_gl)
        end do
      end do
!
      end subroutine draw_isoline_on_map_image
!
!  ---------------------------------------------------------------------
!
      end module draw_aitoff_map
