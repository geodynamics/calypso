!>@file  t_rendering_vr_image.f90
!!       module t_rendering_vr_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine alloc_multi_view_parameters(num_views, pvr_param)
!!      subroutine dealloc_multi_view_parameters(num_views, pvr_param)
!!        integer(kind = kint), intent(in) :: num_views
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!      subroutine set_fixed_view_and_image(i_img, num_stereo,          &
!!     &          mesh, pvr_param, pvr_rgb, pvr_bound, pvr_proj,        &
!!     &          SR_sig, SR_r, SR_i)
!!      subroutine rendering_with_fixed_view                            &
!!     &         (istep_pvr, time, mesh, group, sf_grp_4_sf,            &
!!     &          field_pvr, pvr_param, pvr_proj, pvr_rgb, SR_sig, SR_r)
!!      subroutine flush_rendering_4_fixed_view(pvr_proj)
!!
!!      subroutine rendering_at_once(istep_pvr, time, i_img, i_rot,     &
!!     &          mesh, group, sf_grp_4_sf, field_pvr, pvr_param,       &
!!     &          pvr_bound, pvr_proj, pvr_rgb, SR_sig, SR_r, SR_i)
!!        integer(kind = kint), intent(in) :: i_img, i_rot
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        real(kind = kreal), intent(in) :: time
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(pvr_field_data), intent(in) :: field_pvr
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(PVR_projection_data), intent(inout) :: pvr_proj
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!@endverbatim
!
      module t_rendering_vr_image
!
      use m_precision
      use m_machine_parameter
      use m_constants
      use m_work_time
!
      use calypso_mpi
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surf_grp_list_each_surf
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_pvr_stencil_buffer
      use t_pvr_field_data
      use t_control_params_stereo_pvr
      use t_solver_SR
      use t_solver_SR_int
      use generate_vr_image
!
      implicit  none
!
!>      Structure of PVR control parameters
      type PVR_control_params
!>        Structure for rendering area by element group
        type(viz_area_parameter) :: area_def
!>        Structure for field parameter for PVR
        type(pvr_field_parameter) :: field_def
!
!>        Parameters for image pixels
        type(pvr_pixel_position_type) :: pixel
!>        Structure for rough serch of subdomains
        type(pvr_domain_outline) :: outline
!>        Field data for volume rendering
        type(rendering_parameter) :: draw_param
!>        Structure for PVR colormap
        type(pvr_colorbar_parameter):: colorbar
!
!>        Color paramter for volume rendering
        type(pvr_colormap_parameter) :: color
!>        Movie parameters
        type(pvr_movie_parameter) :: movie_def
!>        Stereo view parameters
        type(pvr_stereo_parameter) :: stereo_def
!
!>        Logical flag to use multi view paramter from quilt block
        logical :: flag_mulview_quilt = .FALSE.
!>        Logical flag to use multi view paramter from movie block
        logical :: flag_mulview_movie = .FALSE.
!>        Number of mulitple view parameters
        integer(kind = kint) :: num_multi_views = 0
!>        Multiple viewer coordinate information
        type(pvr_view_parameter), allocatable :: multi_view(:)
      end type PVR_control_params
!
!
!>      Structure for projection data
      type PVR_projection_data
!>        Data on screen coordinate
        type(pvr_projected_position) :: screen
!>        Parallel stencil buffer
        type(pvr_stencil_buffer) :: stencil
!>        Start point structure for volume rendering with fixed view
        type(pvr_ray_start_type) :: start_fix
!
!>        Start point structure for volume rendering with fixed view
        type(pvr_ray_start_type) :: start_save
      end type PVR_projection_data
!
!
      private :: sel_fix_view_projection_mats
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_multi_view_parameters(num_views, pvr_param)
!
      integer(kind = kint), intent(in) :: num_views
      type(PVR_control_params), intent(inout) :: pvr_param
!
      pvr_param%num_multi_views = num_views
      allocate(pvr_param%multi_view(pvr_param%num_multi_views))
!
      end subroutine alloc_multi_view_parameters
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_multi_view_parameters(pvr_param)
!
      type(PVR_control_params), intent(inout) :: pvr_param
!
      if(allocated(pvr_param%multi_view) .eqv. .FALSE.) return
      deallocate(pvr_param%multi_view)
      pvr_param%num_multi_views = 0
!
      end subroutine dealloc_multi_view_parameters
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_view_and_image(i_img, num_stereo,            &
     &          mesh, pvr_param, pvr_rgb, pvr_bound, pvr_proj,          &
     &          SR_sig, SR_r, SR_i)
!
      use cal_pvr_projection_mat
      use cal_pvr_modelview_mat
      use t_pvr_stencil_buffer
!
      integer(kind = kint), intent(in) :: i_img, num_stereo
      type(mesh_geometry), intent(in) :: mesh
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_image_type), intent(in) :: pvr_rgb
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      call sel_fix_view_projection_mats(i_img, num_stereo,              &
     &                                  pvr_param, pvr_proj%screen)
!
      call transfer_to_screen(mesh%node, mesh%surf,                     &
     &    pvr_param%pixel, pvr_param%multi_view(1)%n_pvr_pixel,         &
     &    pvr_bound, pvr_proj%screen, pvr_proj%start_fix)
      call const_pvr_stencil_buffer                                     &
     &   (pvr_rgb, pvr_proj%start_fix, pvr_proj%stencil,                &
     &    SR_sig, SR_r, SR_i)
!
      call allocate_item_pvr_ray_start                                  &
     &   (pvr_proj%start_fix%num_pvr_ray, pvr_proj%start_save)
      call copy_item_pvr_ray_start                                      &
     &   (pvr_proj%start_fix, pvr_proj%start_save)
!
      end subroutine set_fixed_view_and_image
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_with_fixed_view                              &
     &         (istep_pvr, time, mesh, group, sf_grp_4_sf,              &
     &          field_pvr, pvr_param, pvr_proj, pvr_rgb, SR_sig, SR_r)
!
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(pvr_field_data), intent(in) :: field_pvr
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call copy_item_pvr_ray_start                                      &
     &   (pvr_proj%start_save, pvr_proj%start_fix)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image'
      call rendering_image(istep_pvr, time, mesh, group, sf_grp_4_sf,   &
     &    pvr_param%color, pvr_param%colorbar, field_pvr,               &
     &    pvr_param%draw_param, pvr_proj%screen, pvr_proj%start_fix,    &
     &    pvr_proj%stencil, pvr_rgb, SR_sig, SR_r)
!
      end subroutine rendering_with_fixed_view
!
!  ---------------------------------------------------------------------
!
      subroutine flush_rendering_4_fixed_view(pvr_proj)
!
      type(PVR_projection_data), intent(inout) :: pvr_proj
!
!
      call dealloc_pvr_stencil_buffer(pvr_proj%stencil)
!
      end subroutine flush_rendering_4_fixed_view
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine rendering_at_once(istep_pvr, time, i_img, i_rot,       &
     &          mesh, group, sf_grp_4_sf, field_pvr, pvr_param,         &
     &          pvr_bound, pvr_proj, pvr_rgb, SR_sig, SR_r, SR_i)
!
      use cal_pvr_projection_mat
      use cal_pvr_modelview_mat
      use write_PVR_image
      use t_pvr_stencil_buffer
!
      integer(kind = kint), intent(in) :: i_img, i_rot
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(pvr_field_data), intent(in) :: field_pvr
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      call sel_rot_view_projection_mats(i_img, i_rot,                   &
     &                                  pvr_param, pvr_proj%screen)
!
      call transfer_to_screen(mesh%node, mesh%surf,                     &
     &    pvr_param%pixel, pvr_param%multi_view(1)%n_pvr_pixel,         &
     &    pvr_bound, pvr_proj%screen, pvr_proj%start_fix)
      call const_pvr_stencil_buffer                                     &
     &   (pvr_rgb, pvr_proj%start_fix, pvr_proj%stencil,                &
     &    SR_sig, SR_r, SR_i)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image'
      call rendering_image(istep_pvr, time, mesh, group, sf_grp_4_sf,   &
     &    pvr_param%color, pvr_param%colorbar, field_pvr,               &
     &    pvr_param%draw_param, pvr_proj%screen, pvr_proj%start_fix,    &
     &    pvr_proj%stencil, pvr_rgb, SR_sig, SR_r)
      call deallocate_pvr_ray_start(pvr_proj%start_fix)
      call dealloc_pvr_stencil_buffer(pvr_proj%stencil)
!
      end subroutine rendering_at_once
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_rot_view_projection_mats(i_img, i_rot,             &
     &                                        pvr_param, screen)
!
      use cal_pvr_modelview_mat
      use cal_pvr_projection_mat
!
      integer(kind = kint), intent(in) :: i_img, i_rot
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(pvr_projected_position), intent(inout) :: screen
!
!
      if(pvr_param%flag_mulview_movie) then
        call set_pvr_projection_matrix(pvr_param%multi_view(i_rot),     &
     &                                 screen%projection_mat)
        call cal_pvr_modelview_matrix(ione, izero, pvr_param%outline,   &
     &      pvr_param%movie_def, pvr_param%stereo_def,                  &
     &      pvr_param%multi_view(i_rot), screen%viewpoint_vec,          &
     &      screen%modelview_mat)
      else if(pvr_param%flag_mulview_quilt) then
        call set_pvr_projection_matrix(pvr_param%multi_view(i_img),     &
     &                                 screen%projection_mat)
        call cal_pvr_modelview_matrix(ione, i_rot, pvr_param%outline,   &
     &      pvr_param%movie_def, pvr_param%stereo_def,                  &
     &      pvr_param%multi_view(i_img), screen%viewpoint_vec,          &
     &      screen%modelview_mat)
      else if(pvr_param%movie_def%iflag_movie_mode                      &
     &                                 .eq. I_LOOKINGLASS) then
        call set_pvr_step_projection_mat                                &
     &     (i_rot, pvr_param%movie_def%num_frame,                       &
     &      pvr_param%multi_view(1), pvr_param%stereo_def,              &
     &      screen%projection_mat)
        call cal_pvr_modelview_matrix(i_rot, izero,                     &
     &      pvr_param%outline, pvr_param%movie_def,                     &
     &      pvr_param%stereo_def, pvr_param%multi_view(1),              &
     &      screen%viewpoint_vec, screen%modelview_mat)
      else
        call set_pvr_projection_matrix                                  &
     &     (pvr_param%multi_view(1), screen%projection_mat)
        call cal_pvr_modelview_matrix(i_img, i_rot,                     &
     &      pvr_param%outline, pvr_param%movie_def,                     &
     &      pvr_param%stereo_def, pvr_param%multi_view(1),              &
     &      screen%viewpoint_vec, screen%modelview_mat)
      end if
!
      end subroutine sel_rot_view_projection_mats
!
!  ---------------------------------------------------------------------
!
      subroutine sel_fix_view_projection_mats(i_img, num_stereo,       &
     &                                        pvr_param, screen)
!
      use cal_pvr_modelview_mat
      use cal_pvr_projection_mat
!
      integer(kind = kint), intent(in) :: i_img, num_stereo
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(pvr_projected_position), intent(inout) :: screen
!
!
      if(pvr_param%flag_mulview_quilt) then
        call set_pvr_projection_matrix(pvr_param%multi_view(i_img),     &
     &                                 screen%projection_mat)
        call cal_pvr_modelview_matrix(i_img, izero, pvr_param%outline,  &
     &      pvr_param%movie_def, pvr_param%stereo_def,                  &
     &      pvr_param%multi_view(i_img), screen%viewpoint_vec,          &
     &      screen%modelview_mat)
      else if(num_stereo .gt. 1) then
        call set_pvr_step_projection_mat(i_img, num_stereo,             &
     &      pvr_param%multi_view(1), pvr_param%stereo_def,              &
     &      screen%projection_mat)
        call cal_pvr_modelview_matrix(i_img, izero,                     &
     &      pvr_param%outline, pvr_param%movie_def,                     &
     &      pvr_param%stereo_def, pvr_param%multi_view(1),              &
     &      screen%viewpoint_vec, screen%modelview_mat)
      else
        call set_pvr_projection_matrix                                  &
     &     (pvr_param%multi_view(1), screen%projection_mat)
        call cal_pvr_modelview_matrix(i_img, izero,                     &
     &      pvr_param%outline, pvr_param%movie_def,                     &
     &      pvr_param%stereo_def, pvr_param%multi_view(1),              &
     &      screen%viewpoint_vec, screen%modelview_mat)
      end if
!
      end subroutine sel_fix_view_projection_mats
!
!  ---------------------------------------------------------------------
!
      end module t_rendering_vr_image
