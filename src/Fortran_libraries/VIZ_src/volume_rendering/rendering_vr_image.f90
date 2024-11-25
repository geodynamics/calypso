!>@file  rendering_vr_image.f90
!!       module rendering_vr_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine set_fixed_view_and_image(elps_PVR, mesh,             &
!!     &          pvr_param, pvr_rgb, pvr_bound, pvr_proj, m_SR)
!!      subroutine rendering_with_fixed_view(istep_pvr, time, elps_PVR, &
!!     &          mesh, group, tracer, fline, sf_grp_4_sf,              &
!!     &          field_pvr, pvr_param, pvr_proj, pvr_rgb,              &
!!     &          SR_sig, SR_r)
!!
!!      subroutine rendering_at_once                                    &
!!     &         (istep_pvr, time, elps_PVR, mesh, group, tracer, fline,&
!!     &          sf_grp_4_sf, field_pvr, pvr_param, pvr_bound,         &
!!     &          pvr_proj, pvr_rgb, SR_sig, SR_r, SR_i)
!!        integer(kind = kint), intent(in) :: i_img, i_rot
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        type(elapsed_lables), intent(in) :: elps_PVR
!!        real(kind = kreal), intent(in) :: time
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(tracer_module), intent(in) :: tracer
!!        type(fieldline_module), intent(in) :: fline
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(pvr_field_data), intent(in) :: field_pvr
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(PVR_projection_data), intent(inout) :: pvr_proj
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module rendering_vr_image
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
      use t_particle_trace
      use t_fieldline
!
      use t_surf_grp_list_each_surf
      use t_control_params_4_pvr
      use t_pvr_colormap_parameter
      use t_geometries_in_pvr_screen
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_pvr_stencil_buffer
      use t_pvr_field_data
      use t_rendering_vr_image
      use t_control_params_stereo_pvr
      use t_mesh_SR
      use generate_vr_image
!
      implicit  none
!
      private :: rendering_image
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_view_and_image(elps_PVR, mesh,               &
     &          pvr_param, pvr_rgb, pvr_bound, pvr_proj, m_SR)
!
      use cal_pvr_projection_mat
      use cal_pvr_modelview_mat
      use t_pvr_stencil_buffer
!
      type(elapsed_lables), intent(in) :: elps_PVR
      type(mesh_geometry), intent(in) :: mesh
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_image_type), intent(in) :: pvr_rgb
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call transfer_to_screen(mesh%node, mesh%surf,                     &
     &    pvr_param%pixel, pvr_param%multi_view(1)%n_pvr_pixel,         &
     &    pvr_bound, pvr_proj%screen, pvr_proj%start_fix)
      call const_pvr_stencil_buffer                                     &
     &   (elps_PVR, pvr_rgb, pvr_proj%start_fix, pvr_proj%stencil,      &
     &    m_SR%SR_sig, m_SR%SR_r, m_SR%SR_i)
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
      subroutine rendering_with_fixed_view(istep_pvr, time, elps_PVR,   &
     &          mesh, group, tracer, fline, sf_grp_4_sf,                &
     &          field_pvr, pvr_param, pvr_proj, pvr_rgb,                &
     &          SR_sig, SR_r)
!
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(elapsed_lables), intent(in) :: elps_PVR
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
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
      call rendering_image(istep_pvr, time, elps_PVR,                   &
     &    mesh, group, tracer, fline, sf_grp_4_sf,                      &
     &    pvr_param%color, pvr_param%colorbar, field_pvr,               &
     &    pvr_param%draw_param, pvr_proj%screen, pvr_proj%start_fix,    &
     &    pvr_proj%stencil, pvr_rgb, SR_sig, SR_r)
!
      end subroutine rendering_with_fixed_view
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_at_once                                      &
     &         (istep_pvr, time, elps_PVR, mesh, group, tracer, fline,  &
     &          sf_grp_4_sf, field_pvr, pvr_param, pvr_bound,           &
     &          pvr_proj, pvr_rgb, SR_sig, SR_r, SR_i)
!
      use cal_pvr_projection_mat
      use cal_pvr_modelview_mat
      use write_PVR_image
      use t_pvr_stencil_buffer
!
      integer(kind = kint), intent(in) :: istep_pvr
      type(elapsed_lables), intent(in) :: elps_PVR
      real(kind = kreal), intent(in) :: time
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
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
      call transfer_to_screen(mesh%node, mesh%surf,                     &
     &    pvr_param%pixel, pvr_param%multi_view(1)%n_pvr_pixel,         &
     &    pvr_bound, pvr_proj%screen, pvr_proj%start_fix)
      call const_pvr_stencil_buffer(elps_PVR, pvr_rgb,                  &
     &    pvr_proj%start_fix, pvr_proj%stencil, SR_sig, SR_r, SR_i)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image'
      call rendering_image(istep_pvr, time, elps_PVR,                   &
     &    mesh, group, tracer, fline, sf_grp_4_sf,                      &
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
      subroutine rendering_image                                        &
     &         (istep_pvr, time, elps_PVR, mesh, group, tracer, fline,  &
     &          sf_grp_4_sf, color_param, cbar_param, field_pvr,        &
     &          draw_param, pvr_screen, pvr_start, pvr_stencil,         &
     &          pvr_rgb, SR_sig, SR_r)
!
      use m_geometry_constants
      use t_solver_SR
!
      use ray_trace_4_each_image
      use draw_pvr_colorbar
      use pvr_axis_label
!      use composit_by_segmentad_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(elapsed_lables), intent(in) :: elps_PVR
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(pvr_field_data), intent(in) :: field_pvr
      type(rendering_parameter), intent(in) :: draw_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
      type(pvr_projected_position), intent(in) :: pvr_screen
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!      type(pvr_segmented_img), intent(inout) :: pvr_img
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(elps_PVR%flag_elapsed)                                         &
     &         call start_elapsed_time(elps_PVR%ist_elapsed+3)
      if(iflag_debug .gt. 0) write(*,*) 's_ray_trace_4_each_image'
      call s_ray_trace_4_each_image                                     &
     &   (mesh, group, tracer, fline, sf_grp_4_sf,                      &
     &    field_pvr, pvr_screen, draw_param, color_param, pvr_start)
      if(elps_PVR%flag_elapsed)                                         &
     &          call end_elapsed_time(elps_PVR%ist_elapsed+3)
!
      if(elps_PVR%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_PVR%ist_elapsed+4)
      if(iflag_debug .gt. 0) write(*,*) 'collect_rendering_image'
      call collect_rendering_image(pvr_start,                           &
     &    pvr_rgb%num_pixel_actual, pvr_rgb%rgba_real_gl, pvr_stencil,  &
     &    SR_sig, SR_r)
      if(elps_PVR%flag_elapsed)                                         &
     &          call end_elapsed_time(elps_PVR%ist_elapsed+4)
!
!      call s_composit_by_segmentad_image(istep_pvr, elps_PVR,          &
!     &    pvr_start, pvr_stencil, pvr_img, pvr_rgb)
!
      if(my_rank .eq. pvr_rgb%irank_image_file) then
        if(elps_PVR%flag_elapsed)                                       &
     &          call start_elapsed_time(elps_PVR%ist_elapsed+3)
        if(cbar_param%flag_pvr_colorbar) then
          call set_pvr_colorbar                                         &
     &       (pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,                 &
     &        color_param, cbar_param, pvr_rgb%rgba_real_gl)
        end if
!
        if(cbar_param%flag_draw_time) then
          call set_pvr_timelabel                                        &
     &       (time, pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,           &
     &        cbar_param, pvr_rgb%rgba_real_gl)
        end if
!
        if(cbar_param%flag_pvr_axis) then
          call set_pvr_axislabel                                        &
     &       (pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,                 &
     &        cbar_param%iscale_font, pvr_screen, pvr_rgb%rgba_real_gl)
        end if
        if(elps_PVR%flag_elapsed)                                       &
     &          call end_elapsed_time(elps_PVR%ist_elapsed+3)
      end if
!
      end subroutine rendering_image
!
!  ---------------------------------------------------------------------
!
      end module rendering_vr_image
