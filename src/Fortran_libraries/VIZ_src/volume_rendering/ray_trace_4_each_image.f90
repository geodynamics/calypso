!>@file   ray_trace_4_each_image.f90
!!@brief  module ray_trace_4_each_image
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2011
!
!>@brief structure of control data for multiple volume rendering
!!
!!@verbatim
!!      subroutine s_ray_trace_4_each_image(mesh, group, tracer, fline, &
!!     &          sf_grp_4_sf, field_pvr, pvr_screen,                   &
!!     &          draw_param, color_param, pvr_start)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(tracer_module), intent(in) :: tracer
!!        type(fieldline_module), intent(in) :: fline
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(pvr_field_data), intent(in) :: field_pvr
!!        type(pvr_projected_position), intent(in) :: pvr_screen
!!        type(rendering_parameter), intent(in) :: draw_param
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_ray_start_type), intent(inout) :: pvr_start
!!@endverbatim
      module ray_trace_4_each_image
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use calypso_mpi
      use set_rgba_4_each_pixel
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_particle_trace
      use t_fieldline
      use t_surf_grp_list_each_surf
      use t_control_params_4_pvr
      use t_pvr_colormap_parameter
      use t_pvr_field_data
      use t_geometries_in_pvr_screen
!
      implicit  none
!
      private :: ray_trace_each_pixel
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_ray_trace_4_each_image(mesh, group, tracer, fline,   &
     &          sf_grp_4_sf, field_pvr, pvr_screen,                     &
     &          draw_param, color_param, pvr_start)
!
      use t_pvr_ray_startpoints
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!
      type(pvr_field_data), intent(in) :: field_pvr
      type(pvr_projected_position), intent(in) :: pvr_screen
      type(rendering_parameter), intent(in) :: draw_param
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
      integer(kind = kint) :: inum, iflag_comm, icount_line_int
      real(kind = kreal) :: rgba_tmp(4)
!
!
      icount_line_int = 0
!$omp parallel do private(inum,iflag_comm,rgba_tmp)                     &
!$omp& reduction(+:icount_line_int)
      do inum = 1, pvr_start%num_pvr_ray
        rgba_tmp(1:4) = zero
        call ray_trace_each_pixel                                       &
     &     (mesh%node, mesh%ele, mesh%surf, group%surf_grp,             &
     &      sf_grp_4_sf, tracer, fline, pvr_screen%viewpoint_vec,       &
     &      pvr_screen%modelview_mat, pvr_screen%projection_mat,        &
     &      field_pvr, draw_param, color_param, ray_vec4,               &
     &      pvr_start%id_pixel_check(inum),                             &
     &      pvr_start%isf_pvr_ray_start(1,inum),                        &
     &      pvr_start%xx4_pvr_ray_start(1,inum),                        &
     &      pvr_start%xx4_pvr_start(1,inum),                            &
     &      pvr_start%xi_pvr_start(1,inum),                             &
     &      rgba_tmp(1), icount_line_int, iflag_comm)
        pvr_start%rgba_ray(1:4,inum) = rgba_tmp(1:4)
      end do
!$omp end parallel do
!
      end subroutine s_ray_trace_4_each_image
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine ray_trace_each_pixel                                   &
     &         (node, ele, surf, surf_grp, sf_grp_4_sf, tracer, fline,  &
     &          viewpoint_vec, modelview_mat, projection_mat,           &
     &          field_pvr, draw_param, color_param, ray_vec4,           &
     &          iflag_check, isurf_org, screen4_st, xx4_st, xi,         &
     &          rgba_ray, icount_line_int, iflag_comm)
!
      use set_position_pvr_screen
      use cal_field_on_surf_viz
      use cal_fline_in_cube
      use set_coefs_of_sections
      use pvr_surface_enhancement
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: surf_grp
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
      integer(kind = kint), intent(in) :: iflag_check
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: modelview_mat(4,4)
      real(kind = kreal), intent(in) :: projection_mat(4,4)
      real(kind = kreal), intent(in) :: ray_vec4(4)
!
      type(pvr_field_data), intent(in) :: field_pvr
      type(rendering_parameter), intent(in) :: draw_param
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      integer(kind = kint), intent(inout) :: isurf_org(3)
      integer(kind = kint), intent(inout) :: icount_line_int
      integer(kind = kint), intent(inout) :: iflag_comm
      real(kind = kreal), intent(inout) :: screen4_st(4)
      real(kind = kreal), intent(inout) :: xx4_st(4), xi(2)
      real(kind = kreal), intent(inout) :: rgba_ray(4)
!
      integer(kind = kint) :: iflag_notrace
      integer(kind = kint) :: isf_tgt, isurf_end, iele, isf_org
      integer(kind = kint) :: iflag_hit
      real(kind = kreal) :: screen4_tgt(4), c_tgt(1), c_org(1)
      real(kind = kreal) :: xx4_model_sf(4,num_linear_sf,nsurf_4_ele)
      real(kind = kreal) :: grad_tgt(3), xx4_tgt(4)
!
!
      if(isurf_org(1) .eq. 0) return
!
      iflag_notrace = 1
      iele =    isurf_org(1)
      isf_org = isurf_org(2)
      isurf_end = abs(surf%isf_4_ele(iele,isf_org))
      call cal_field_on_surf_vect4                                      &
     &   (node%numnod, surf%numsurf, surf%nnod_4_surf,                  &
     &    surf%ie_surf, isurf_end, xi, node%xx, xx4_st)
      call cal_field_on_surf_scalar                                     &
     &   (node%numnod, surf%numsurf, surf%nnod_4_surf,                  &
     &    surf%ie_surf, isurf_end, xi, field_pvr%d_pvr, c_org(1) )
!
      if(iflag_check .gt. 0) then
        iflag_hit = 0
      end if
!
!        Set color if starting surface is colourd
      if(ele%interior_ele(iele) .gt. 0) then
        call rendering_surace_group                                     &
     &     (isurf_end, surf, surf_grp, sf_grp_4_sf,                     &
     &      viewpoint_vec, modelview_mat, draw_param, color_param,      &
     &      xx4_st, rgba_ray)
      end if
!
      do
        icount_line_int = icount_line_int + 1
        iele =    isurf_org(1)
        isf_org = isurf_org(2)
!
        if(draw_param%iflag_used_ele(iele).eq.0) then
          iflag_comm = 2
          exit
        end if
!
!   extend to surface of element
!
        call position_on_each_ele_sfs_wone                              &
     &     (surf, node%numnod, node%xx, iele, xx4_model_sf)
        call project_once_each_element(modelview_mat, projection_mat,   &
     &      (num_linear_sf*nsurf_4_ele), xx4_model_sf(1,1,1))
        call find_line_end_in_1ele(iflag_forward_line,                  &
     &      isf_org, ray_vec4, screen4_st, xx4_model_sf,                &
     &      isf_tgt, screen4_tgt, xi)
!        if(iflag_check .gt. 0) write(*,*) 'screen_tgt',                &
!     &         my_rank, screen4_tgt(1:4), ele%interior_ele(iele)
!
        if(isf_tgt .eq. 0) then
          iflag_comm = -1
          exit
        end if
!
!   set backside element and surface 
!
        iflag_notrace = 0
        isurf_end = abs(surf%isf_4_ele(iele,isf_tgt))
!
        if(surf%isf_4_ele(iele,isf_tgt) .lt. 0) then
          isurf_org(1) = surf%iele_4_surf(isurf_end,1,1)
          isurf_org(2) = surf%iele_4_surf(isurf_end,1,2)
        else
          isurf_org(1) = surf%iele_4_surf(isurf_end,2,1)
          isurf_org(2) = surf%iele_4_surf(isurf_end,2,2)
        end if
!
        call cal_field_on_surf_vect4                                    &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf,                &
     &      surf%ie_surf, isurf_end, xi, node%xx, xx4_tgt)
        call cal_field_on_surf_scalar                                   &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf,                &
     &      surf%ie_surf, isurf_end, xi, field_pvr%d_pvr, c_tgt(1))
!
        if(ele%interior_ele(iele) .gt. 0) then
!    Set color if exit surface is colourd
          call rendering_surace_group                                   &
     &       (isurf_end, surf, surf_grp, sf_grp_4_sf,                   &
     &        viewpoint_vec, modelview_mat, draw_param, color_param,    &
     &        xx4_tgt, rgba_ray)
!
          call rendering_sections                                       &
     &       (viewpoint_vec, draw_param, color_param,                   &
     &        xx4_st, xx4_tgt, c_org(1), c_tgt(1), rgba_ray, iflag_hit)
          call rendering_isosurfaces(iele, viewpoint_vec, field_pvr,    &
     &                               draw_param, color_param,           &
     &                               xx4_tgt, c_org, c_tgt, rgba_ray)
!
          call rendering_tracers                                        &
     &       (viewpoint_vec, color_param, draw_param%tracer_pvr_prm,    &
     &        tracer%num_trace, tracer%fline_lc,                        &
     &        xx4_tgt, c_tgt, rgba_ray)
          call rendering_fieldlines                                     &
     &       (viewpoint_vec, color_param, draw_param%fline_pvr_prm,     &
     &        fline%num_fline, fline%fline_lc,                          &
     &        xx4_tgt, c_tgt, rgba_ray)
!
          grad_tgt(1:3) = field_pvr%grad_ele(iele,1:3)
          c_tgt(1) = half*(c_tgt(1) + c_org(1))
          call s_set_rgba_4_each_pixel                                  &
     &       (viewpoint_vec, xx4_st, xx4_tgt,                           &
     &        c_tgt(1), grad_tgt, color_param, rgba_ray)
        end if
!
        if(isurf_org(1).eq.0) then
          iflag_comm = 0
          exit
        end if
!
        screen4_st(1:4) = screen4_tgt(1:4)
        xx4_st(1:4) = xx4_tgt(1:4)
        c_org(1) =   c_tgt(1)
      end do
!
!      if(iflag_check*draw_param%num_sections .gt. 0) then
!        if(iflag_hit .eq. 0) then
!          write(*,*) 'surface does not hit: ', my_rank, rgba_ray(1:4)
!        else
!          write(*,*) 'surface  hit in: ', my_rank, rgba_ray(1:4)
!        end if
!      end if
!
      end subroutine ray_trace_each_pixel
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_sections                                     &
     &         (viewpoint_vec, draw_param, color_param,                 &
     &          xx4_st, xx4_tgt, c_org, c_tgt, rgba_ray, iflag_hit)
!
      use set_coefs_of_sections
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
!
      type(rendering_parameter), intent(in) :: draw_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      real(kind = kreal), intent(in) :: xx4_st(4)
      real(kind = kreal), intent(in) :: xx4_tgt(4)
      real(kind = kreal), intent(in) :: c_tgt(1), c_org(1)
!
      real(kind = kreal), intent(inout) :: rgba_ray(4)
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
     &            rgba_ray)
          if(draw_param%iflag_psf_zeoline(i_psf) .gt. 0                 &
     &            .and. c_org(1)*c_tgt(1) .le. TINY9) then
            call black_plane_with_light                                 &
     &         (viewpoint_vec, xx4_tgt, grad_tgt,                       &
     &          draw_param%sect_opacity(i_psf), color_param, rgba_ray)
          end if
        end if
      end do
!
      end subroutine rendering_sections
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_isosurfaces(iele, viewpoint_vec, field_pvr,  &
     &                                draw_param, color_param,          &
     &                                xx4_tgt, c_org, c_tgt, rgba_ray)
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
      real(kind = kreal), intent(inout) :: rgba_ray(4)
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
     &        draw_param%iso_opacity(i_iso), color_param, rgba_ray)
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
     &          xx4_tgt, rgba_ray)
!
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
      real(kind = kreal), intent(inout) :: rgba_ray(4)
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
        call plane_rendering_with_light(viewpoint_vec,                  &
     &      xx4_tgt, grad_tgt, opacity_bc, color_param, rgba_ray)
      end if
!
      end subroutine rendering_surace_group
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine rendering_tracers(viewpoint_vec, color_param,          &
     &          tracer_pvr_prm, num_tracer, particle_lc,                &
     &          xx4_tgt, c_tgt, rgba_ray)
!
      use t_local_fline
      use t_geometries_in_pvr_screen
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      type(tracer_render_param), intent(in) :: tracer_pvr_prm
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      integer(kind = kint), intent(in) :: num_tracer
      type(local_fieldline), intent(in) :: particle_lc(num_tracer)
!
      real(kind = kreal), intent(in) :: xx4_tgt(4)
      real(kind = kreal), intent(in) :: c_tgt(1)
!
      real(kind = kreal), intent(inout) :: rgba_ray(4)
!
      integer(kind = kint) :: i_fln, inum, increment
      integer(kind = kint_gl) :: i_global
      real(kind = kreal) :: grad_tgt(3), radius, distance
      real(kind = kreal) :: rgb_color(3), opacity
!
      if(tracer_pvr_prm%num_pvr_tracer .le. 0) return
      do i_fln = 1, tracer_pvr_prm%num_pvr_tracer
        increment =      tracer_pvr_prm%increment(i_fln)
        radius =         tracer_pvr_prm%rendering_radius(i_fln)
        opacity =        tracer_pvr_prm%tracer_opacity(i_fln)
        rgb_color(1:3) = tracer_pvr_prm%tracer_RGB(1:3,i_fln)
        do inum = 1, particle_lc(i_fln)%nnod_line_l
          i_global = particle_lc(i_fln)%iglobal_fline(inum)
          if(mod(i_global-1,increment) .ne. 0) cycle
!
          distance = distance_from_tracer(xx4_tgt, inum,                &
     &                                    particle_lc(i_fln))
          if(distance .ge. radius) cycle
!
!          opacity = opacity * (one - sqrt(distance / radius))
          call normal_of_single_tracer                                  &
     &       (xx4_tgt, inum, particle_lc(i_fln), grad_tgt)
!
          if(tracer_pvr_prm%iflag_color_mode(i_fln)                     &
     &                          .eq. iflag_single_color) then
            call surface_rendering_with_light                           &
     &         (viewpoint_vec, xx4_tgt, grad_tgt, rgb_color,            &
     &          opacity, color_param, rgba_ray)
          else
            call color_plane_with_light                                 &
     &         (viewpoint_vec, xx4_tgt, c_tgt(1), grad_tgt,             &
     &          opacity, color_param, rgba_ray)
          end if
!
        end do
      end do
!
      end subroutine rendering_tracers
!
! ----------------------------------------------------------------------
!
      subroutine rendering_fieldlines(viewpoint_vec, color_param,       &
     &          fline_pvr_prm, num_fline, fline_lc,                     &
     &          xx4_tgt, c_tgt, rgba_ray)
!
      use t_local_fline
      use t_geometries_in_pvr_screen
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(tracer_render_param), intent(in) :: fline_pvr_prm
!
      integer(kind = kint), intent(in) :: num_fline
      type(local_fieldline), intent(in) :: fline_lc(num_fline)
      real(kind = kreal), intent(in) :: xx4_tgt(4)
      real(kind = kreal), intent(in) :: c_tgt(1)
!
      real(kind = kreal), intent(inout) :: rgba_ray(4)
!
      integer(kind = kint) :: i_fln, iedge, inod, increment
      integer(kind = kint_gl) :: i_global
      real(kind = kreal) :: grad_tgt(3), radius, distance
      real(kind = kreal) :: rgb_color(3), opacity
!
!
      if(fline_pvr_prm%num_pvr_tracer .le. 0) return
      do i_fln = 1, fline_pvr_prm%num_pvr_tracer
        increment = fline_pvr_prm%increment(i_fln)
        radius =    fline_pvr_prm%rendering_radius(i_fln)
        opacity =   fline_pvr_prm%tracer_opacity(i_fln)
        rgb_color(1:3) = fline_pvr_prm%tracer_RGB(1:3,i_fln)
        do iedge = 1, fline_lc(i_fln)%nele_line_l
          inod =     fline_lc(i_fln)%iedge_line_l(1,iedge)
          i_global = fline_lc(i_fln)%iglobal_fline(inod)
          if(mod(i_global-1,increment) .ne. 0) cycle
!
          distance = distance_from_fline_segment(xx4_tgt, iedge,        &
     &                                           fline_lc(i_fln))
          if(distance .ge. radius) cycle
!
!          opacity = opacity * (one - sqrt(distance / radius))
          call normal_of_single_fline                                   &
     &       (xx4_tgt, iedge, fline_lc(i_fln), grad_tgt)
!
          if(fline_pvr_prm%iflag_color_mode(i_fln)                      &
     &                          .eq. iflag_single_color) then
            call surface_rendering_with_light                           &
     &         (viewpoint_vec, xx4_tgt, grad_tgt, rgb_color,            &
     &          opacity, color_param, rgba_ray)
          else
            call color_plane_with_light                                 &
     &         (viewpoint_vec, xx4_tgt, c_tgt(1), grad_tgt,             &
     &          opacity, color_param, rgba_ray)
          end if
!
        end do
      end do
!
      end subroutine rendering_fieldlines
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      real(kind = kreal) function distance_from_tracer(point,           &
     &                                                 inum, fline_lc)
!
      use t_local_fline
!
      real(kind = kreal), intent(in) :: point(4)
      integer(kind = kint), intent(in) :: inum
      type(local_fieldline), intent(in) :: fline_lc
!
      real(kind = kreal) :: xyzw(4)
!
      xyzw(1:3) = fline_lc%xx_line_l(1:3,inum)
      xyzw(4) =   one
      distance_from_tracer = distance_from_point(point, xyzw(1))
!
      end function distance_from_tracer
!
! ----------------------------------------------------------------------
!
      real(kind = kreal) function distance_from_fline_segment           &
     &                                     (point, iedge, fline_lc)
!
      use t_local_fline
!
      real(kind = kreal), intent(in) :: point(4)
      integer(kind = kint), intent(in) :: iedge
      type(local_fieldline), intent(in) :: fline_lc
!
      real(kind = kreal) :: xyzw_1(4), xyzw_2(4)
      integer(kind = kint) :: i1, i2
!
      i1 = fline_lc%iedge_line_l(1,iedge)
      i2 = fline_lc%iedge_line_l(2,iedge)
      xyzw_1(1:3) = fline_lc%xx_line_l(1:3,i1)
      xyzw_1(4) =   one
      xyzw_2(1:3) = fline_lc%xx_line_l(1:3,i2)
      xyzw_2(4) =   one
!
      distance_from_fline_segment                                       &
     &     = distance_from_line_segment(point, xyzw_1(1), xyzw_2(1))
!
      end function distance_from_fline_segment
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine normal_of_single_tracer                                &
     &         (xx4_tgt, inum, fline_lc, norm)
!
      use t_local_fline
!
      real(kind = kreal), intent(in) :: xx4_tgt(4)
      integer(kind = kint), intent(in) :: inum
      type(local_fieldline), intent(in) :: fline_lc
      real(kind = kreal), intent(inout) :: norm(3)
!
      norm(1:3) = xx4_tgt(1:3) - fline_lc%xx_line_l(1:3,inum)
      call single_normalize_vector(norm)
!
      end subroutine normal_of_single_tracer
!
! ----------------------------------------------------------------------
!
      subroutine normal_of_single_fline                                 &
     &         (xx4_tgt, iedge, fline_lc, norm)
!
      use t_local_fline
!
      real(kind = kreal), intent(in) :: xx4_tgt(4)
      integer(kind = kint), intent(in) :: iedge
      type(local_fieldline), intent(in) :: fline_lc
      real(kind = kreal), intent(inout) :: norm(3)
!
      integer(kind = kint) :: i1, i2
!
      i1 = fline_lc%iedge_line_l(1,iedge)
      i2 = fline_lc%iedge_line_l(2,iedge)
      norm(1:3) = xx4_tgt(1:3) - half * (fline_lc%xx_line_l(1:3,i1)     &
     &                                 + fline_lc%xx_line_l(1:3,i1))
      call single_normalize_vector(norm)
!
      end subroutine normal_of_single_fline
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      real(kind = kreal) function distance_from_point(point, xyzw1)
!
      real(kind = kreal), intent(in) :: point(4), xyzw1(4)
      real(kind = kreal) :: x_line(1:4)
!
      x_line(1:4) = xyzw1(1:4) - point(1:4)
      distance_from_point = single_dot_product(x_line(1), x_line(1))
!
      end function distance_from_point
!
! ----------------------------------------------------------------------
!
      real(kind = kreal) function distance_from_line_segment            &
     &                  (point, xyzw1, xyzw2)
!
      real(kind = kreal), intent(in) :: point(4), xyzw1(4), xyzw2(4)
!
      real(kind = kreal) :: vec1(1:4), vec2(1:4)
      real(kind = kreal) :: x_line(1:4), c_prod(1:4)
      real(kind = kreal) :: dot1, dot2, area
      real(kind = kreal) :: seg_len, dist_line
!
      vec1(1:4) =   point(1:4) - xyzw1(1:4)
      vec2(1:4) =   xyzw2(1:4) - xyzw1(1:4)
      dot1 = single_dot_product(vec1(1), vec2(1))
      vec1(1:4) =   point(1:4) - xyzw2(1:4)
      vec2(1:4) =   xyzw1(1:4) - xyzw2(1:4)
      dot2 = single_dot_product(vec1(1), vec2(1))
!
      if     (dot1 .le. zero) then
        vec1(1:4) =   point(1:4) - xyzw1(1:4)
        dist_line = single_dot_product(vec1(1), vec1(1))
      else if(dot2 .le. zero) then
        vec2(1:4) =   point(1:4) - xyzw2(1:4)
        dist_line = single_dot_product(vec2(1), vec2(1))
      else
        x_line(1:4) = xyzw2(1:4) - xyzw1(1:4)
        seg_len = single_dot_product(x_line(1), x_line(1))
        call single_cross_product(vec1(1), vec2(1), c_prod)
        area =    single_dot_product(c_prod(1), c_prod(1))
        dist_line = area / seg_len
      end if
      distance_from_line_segment = dist_line
!
      end function distance_from_line_segment
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine single_normalize_vector(vector)
!
      real (kind=kreal), intent(inout) :: vector(3)
      real (kind=kreal) :: length
!
      length = max(single_dot_product(vector(1), vector(1)), TINY)
      vector(1:3) = vector(1:3) / length
!
      end subroutine single_normalize_vector
!
! ----------------------------------------------------------------------
!
      real(kind = kreal) function single_dot_product(vect1, vect2)
!
      real (kind=kreal), intent(in) :: vect1(3), vect2(3)
      real (kind=kreal) :: prod
!
      prod = vect1(1)*vect2(1) + vect1(2)*vect2(2) + vect1(3)*vect2(3)
      single_dot_product = prod
!
      end function single_dot_product
!
! ----------------------------------------------------------------------
!
      subroutine single_cross_product(vect1, vect2, prod)
!
      real (kind=kreal), intent(in) :: vect1(3), vect2(3)
      real (kind=kreal), intent(inout) :: prod(3)
!
      prod(1) = (vect1(2)*vect2(3) - vect1(3)*vect2(2))
      prod(2) = (vect1(3)*vect2(1) - vect1(1)*vect2(3))
      prod(3) = (vect1(1)*vect2(2) - vect1(2)*vect2(1))
!
      end subroutine single_cross_product
!
! ----------------------------------------------------------------------
!
      end module ray_trace_4_each_image
