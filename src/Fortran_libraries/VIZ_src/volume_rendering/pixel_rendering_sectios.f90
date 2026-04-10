!>@file   pixel_rendering_sectios.f90
!!        module pixel_rendering_sectios
!!
!!@author H. Matsui
!!@date Programmed in 2024
!!
!>@brief Rendering section surfaces
!!
!!@verbatim
!!      subroutine rendering_sections                                   &
!!     &         (viewpoint_vec, draw_param, color_param,               &
!!     &          xx4_st, xx4_tgt, c_org, c_tgt,                        &
!!     &          rgba_ray, rgba_now, iflag_hit)
!!        real(kind = kreal), intent(in) :: viewpoint_vec(3)
!!        type(rendering_parameter), intent(in) :: draw_param
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        real(kind = kreal), intent(in) :: xx4_st(4)
!!        real(kind = kreal), intent(in) :: xx4_tgt(4)
!!        real(kind = kreal), intent(in) :: c_tgt(1), c_org(1)
!!        real(kind = kreal), intent(inout) :: rgba_ray(4), rgba_now(4)
!!        integer(kind = kint), intent(inout) :: iflag_hit
!!      subroutine rendering_isosurfaces(iele, viewpoint_vec, field_pvr,&
!!     &                                draw_param, color_param,        &
!!     &                                xx4_tgt, c_org, c_tgt, rgba_ray)
!!        integer(kind = kint), intent(in) :: iele
!!        real(kind = kreal), intent(in) :: viewpoint_vec(3)
!!        type(pvr_field_data), intent(in) :: field_pvr
!!        type(rendering_parameter), intent(in) :: draw_param
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        real(kind = kreal), intent(in) :: xx4_tgt(4)
!!        real(kind = kreal), intent(in) :: c_tgt(1), c_org(1)
!!        real(kind = kreal), intent(inout) :: rgba_ray(4), rgba_now(4)
!!      subroutine rendering_surace_group                               &
!!     &         (isurf_end, surf, surf_grp, sf_grp_4_sf,               &
!!     &          viewpoint_vec, modelview_mat, draw_param, color_param,&
!!     &          xx4_tgt, rgba_ray, rgba_now)
!!        integer(kind = kint), intent(in) :: isurf_end
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        real(kind = kreal), intent(in) :: viewpoint_vec(3)
!!        real(kind = kreal), intent(in) :: modelview_mat(4,4)
!!        type(rendering_parameter), intent(in) :: draw_param
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        real(kind = kreal), intent(in) :: xx4_tgt(4)
!!        real(kind = kreal), intent(inout) :: rgba_ray(4), rgba_now(4)
!!@endverbatim
!
      module pixel_rendering_sectios
!
      use m_precision
!
      use t_geometries_in_pvr_screen
      use t_pvr_colormap_parameter
      use t_pvr_field_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine rendering_sections                                     &
     &         (viewpoint_vec, draw_param, color_param,                 &
     &          xx4_st, xx4_tgt, c_org, c_tgt,                          &
     &          rgba_ray, rgba_now, iflag_hit)
!
      use set_coefs_of_sections
      use set_rgba_4_each_pixel
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
!
      type(rendering_parameter), intent(in) :: draw_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      real(kind = kreal), intent(in) :: xx4_st(4)
      real(kind = kreal), intent(in) :: xx4_tgt(4)
      real(kind = kreal), intent(in) :: c_tgt(1), c_org(1)
!
      real(kind = kreal), intent(inout) :: rgba_ray(4), rgba_now(4)
      integer(kind = kint), intent(inout) :: iflag_hit
!
      integer(kind = kint) :: i_psf
      real(kind = kreal) :: grad_tgt(3), rflag1, rflag2
      logical :: flag_sect
!
!
      do i_psf = 1, draw_param%num_sections
        rflag1 = side_of_plane(draw_param%coefs(1:10,i_psf), xx4_st(1))
        rflag2 = side_of_plane(draw_param%coefs(1:10,i_psf), xx4_tgt(1))
!
        flag_sect = .FALSE.
        if     (rflag1 .ge. -TINY9 .and. rflag2 .le. TINY9) then
          flag_sect = .TRUE.
          iflag_hit = 1
        else if(rflag1 .le. TINY9 .and. rflag2 .ge. -TINY9) then
          flag_sect = .TRUE.
          iflag_hit = 1
        end if

        if(flag_sect) then
          call cal_normal_of_plane                                      &
     &           (draw_param%coefs(1:10,i_psf), xx4_tgt(1), grad_tgt)
          call color_plane_with_light                                   &
     &           (viewpoint_vec, xx4_tgt, c_tgt(1), grad_tgt,           &
     &            draw_param%sect_opacity(i_psf), color_param,          &
     &            rgba_ray, rgba_now)
          if(draw_param%iflag_psf_zeoline(i_psf) .gt. 0                 &
     &            .and. c_org(1)*c_tgt(1) .le. TINY9) then
            call black_plane_with_light                                 &
     &         (viewpoint_vec, xx4_tgt, grad_tgt,                       &
     &          draw_param%sect_opacity(i_psf), color_param,            &
     &          rgba_ray, rgba_now)
          end if
        end if
      end do
!
      end subroutine rendering_sections
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_isosurfaces(iele, viewpoint_vec, field_pvr,  &
     &          draw_param, color_param, xx4_tgt, c_org, c_tgt,         &
     &          rgba_ray, rgba_now)
!
      use set_rgba_4_each_pixel
!
      integer(kind = kint), intent(in) :: iele
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
!
      type(pvr_field_data), intent(in) :: field_pvr
      type(rendering_parameter), intent(in) :: draw_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      real(kind = kreal), intent(in) :: xx4_tgt(4)
      real(kind = kreal), intent(in) :: c_tgt(1), c_org(1)
!
      real(kind = kreal), intent(inout) :: rgba_ray(4), rgba_now(4)
!
      integer(kind = kint) :: i_iso
      real(kind = kreal) :: grad_tgt(3), rflag
!
!
      do i_iso = 1, draw_param%num_isosurf
        rflag =  (c_org(1) - draw_param%iso_value(i_iso))               &
     &         * (c_tgt(1) - draw_param%iso_value(i_iso))
        if((c_tgt(1) - draw_param%iso_value(i_iso)) .eq. zero           &
     &    .or. rflag .lt. zero) then
          grad_tgt(1:3) = field_pvr%grad_ele(iele,1:3)                  &
     &                   * dble(draw_param%itype_isosurf(i_iso))
          call color_plane_with_light(viewpoint_vec, xx4_tgt,           &
     &        draw_param%iso_value(i_iso), grad_tgt,                    &
     &        draw_param%iso_opacity(i_iso), color_param,               &
     &        rgba_ray, rgba_now)
        end if
      end do
!
      end subroutine rendering_isosurfaces
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_surace_group                                 &
     &         (isurf_end, surf, surf_grp, sf_grp_4_sf,                 &
     &          viewpoint_vec, modelview_mat, draw_param, color_param,  &
     &          xx4_tgt, rgba_ray, rgba_now)
!
      use t_surface_data
      use t_group_data
      use t_surf_grp_list_each_surf
      use t_control_params_4_pvr
      use set_rgba_4_each_pixel
      use pvr_surface_enhancement
!
      integer(kind = kint), intent(in) :: isurf_end
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: surf_grp
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: modelview_mat(4,4)
!
      type(rendering_parameter), intent(in) :: draw_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      real(kind = kreal), intent(in) :: xx4_tgt(4)
!
      real(kind = kreal), intent(inout) :: rgba_ray(4), rgba_now(4)
!
      real(kind = kreal) :: grad_tgt(3), opacity_bc
!
!
      opacity_bc = opacity_by_surf_grp(isurf_end, surf, surf_grp,       &
     &                                 sf_grp_4_sf, modelview_mat,      &
     &                                 draw_param%iflag_enhanse,        &
     &                                 draw_param%enhansed_opacity)
      if(opacity_bc .gt. SMALL_RAY_TRACE) then
        grad_tgt(1:3) = surf%vnorm_surf(isurf_end,1:3)
        call plane_rendering_with_light                                 &
     &     (viewpoint_vec, xx4_tgt, grad_tgt, opacity_bc, color_param,  &
     &      rgba_ray, rgba_now)
      end if
!
      end subroutine rendering_surace_group
!
! ----------------------------------------------------------------------
!
      end module pixel_rendering_sectios
