!>@file   each_volume_rendering.f90
!!@brief  module each_volume_rendering
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module for each volume rendering
!!
!!@verbatim
!!      subroutine init_each_PVR_image(num_img, pvr_param, pvr_rgb)
!!      subroutine each_PVR_initialize(mesh, group,                     &
!!     &                               pvr_param, pvr_bound)
!!        integer(kind = kint), intent(in) :: i_pvr, num_img
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!      subroutine dealloc_PVR_initialize                               &
!!     &         (num_proj, pvr_param, pvr_bound, pvr_proj)
!!        integer(kind = kint), intent(in) :: num_proj
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(PVR_projection_data), intent(inout) :: pvr_proj(num_proj)
!!
!!      subroutine each_PVR_rendering                                   &
!!     &         (istep_pvr, time, num_img, elps_PVR,  geofem, jacs,    &
!!     &          nod_fld, tracer, fline, sf_grp_4_sf, field_pvr,       &
!!     &          pvr_param, pvr_proj, pvr_rgb, SR_sig, SR_r)
!!      subroutine each_PVR_rendering_w_rot(istep_pvr, time, elps_PVR,  &
!!     &          geofem, jacs, nod_fld, tracer, fline, sf_grp_4_sf,    &
!!     &          field_pvr, pvr_param, pvr_bound, pvr_rgb, pvr_proj,   &
!!     &          SR_sig, SR_r, SR_i)
!!      subroutine each_PVR_quilt_rendering_w_rot                       &
!!     &         (istep_pvr, time, num_img, elps_PVR, geofem, jacs,     &
!!     &          nod_fld, tracer, fline, sf_grp_4_sf, field_pvr,       &
!!     &          pvr_param, pvr_bound, pvr_proj, pvr_rgb,              &
!!     &          SR_sig, SR_r, SR_i)
!!        type(elapsed_lables), intent(in) :: elps_PVR, elps_LIC
!!        type(mesh_data), intent(in) :: geofem
!!        type(viz_area_parameter), intent(in) :: area_def
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(tracer_module), intent(in) :: tracer
!!        type(fieldline_module), intent(in) :: fline
!!        type(jacobians_type), intent(in) :: jacs
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(pvr_field_data), intent(inout) :: field_pvr
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(PVR_projection_data), intent(inout) :: pvr_proj(2)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!@endverbatim
!
      module each_volume_rendering
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
!
      use t_mesh_data
      use t_phys_data
      use t_jacobians
      use t_particle_trace
      use t_fieldline
!
      use t_surf_grp_list_each_surf
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_geometries_in_pvr_screen
      use t_pvr_field_data
      use t_mesh_SR
!
      use set_default_pvr_params
      use set_position_pvr_screen
      use mesh_outline_4_pvr
      use generate_vr_image
      use rendering_streo_vr_image
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_each_PVR_image(num_img, pvr_param, pvr_rgb)
!
      integer(kind = kint), intent(in) :: num_img
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_img)
!
      integer(kind = kint) :: i_img
!
!
      do i_img = 1, num_img
        call alloc_pvr_image_array                                      &
     &     (pvr_param%multi_view(1)%n_pvr_pixel, pvr_rgb(i_img))
      end do
!
      end subroutine init_each_PVR_image
!
!  ---------------------------------------------------------------------
!
      subroutine each_PVR_initialize(mesh, group,                       &
     &                               pvr_param, pvr_bound)
!
      use t_control_data_pvr_sections
      use set_pvr_control
      use find_pvr_surf_domain
      use set_iflag_for_used_ele
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!
!
      call alloc_iflag_pvr_used_ele(mesh%ele, pvr_param%draw_param)
      call s_set_iflag_for_used_ele(mesh%ele, group%ele_grp,            &
     &    pvr_param%area_def%nele_grp_area_pvr,                         &
     &    pvr_param%area_def%id_ele_grp_area_pvr,                       &
     &    pvr_param%draw_param%iflag_used_ele)
!
      call find_each_pvr_surf_domain(mesh%ele, mesh%surf,               &
     &                               pvr_param%draw_param, pvr_bound)
!
      call pvr_mesh_outline(mesh%node, pvr_param%outline)
      call check_pvr_parameters(pvr_param%outline,                      &
     &    pvr_param%num_multi_views, pvr_param%multi_view,              &
     &    pvr_param%color)
!
      call set_pixel_on_pvr_screen(pvr_param%multi_view(1),             &
     &                             pvr_param%pixel)
!
      end subroutine each_PVR_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_PVR_initialize                                 &
     &         (num_proj, pvr_param, pvr_bound, pvr_proj)
!
      integer(kind = kint), intent(in) :: num_proj
      type(PVR_control_params), intent(inout) :: pvr_param
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj(num_proj)
!
      integer(kind = kint) :: i_proj
!
      do i_proj = 1, num_proj
        call deallocate_item_pvr_ray_start(pvr_proj(i_proj)%start_save)
        call deallocate_pvr_ray_start(pvr_proj(i_proj)%start_fix)
        call dealloc_pvr_stencil_buffer(pvr_proj(i_proj)%stencil)
      end do
!
      call dealloc_pvr_surf_domain_item(pvr_bound)
      call dealloc_pixel_position_pvr(pvr_param%pixel)
      call dealloc_iflag_pvr_used_ele(pvr_param%draw_param)
!
      end subroutine dealloc_PVR_initialize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine each_PVR_rendering                                     &
     &         (istep_pvr, time, num_img, elps_PVR,  geofem, jacs,      &
     &          nod_fld, tracer, fline, sf_grp_4_sf, field_pvr,         &
     &          pvr_param, pvr_proj, pvr_rgb, SR_sig, SR_r)
!
      use cal_pvr_modelview_mat
      use rendering_vr_image
!
      integer(kind = kint), intent(in) :: num_img
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(elapsed_lables), intent(in) :: elps_PVR
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
      type(jacobians_type), intent(in) :: jacs
!
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(pvr_field_data), intent(inout) :: field_pvr
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_projection_data), intent(inout) :: pvr_proj(num_img)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_img)
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: i_img
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call cal_field_4_each_pvr(geofem%mesh%node, geofem%mesh%ele,      &
     &    jacs%g_FEM, jacs%jac_3d, nod_fld,                             &
     &    pvr_param%field_def, field_pvr)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_param%color)
!
      do i_img = 1, num_img
        call rendering_with_fixed_view(istep_pvr, time, elps_PVR,       &
     &      geofem%mesh, geofem%group, tracer, fline, sf_grp_4_sf,      &
     &      field_pvr, pvr_param, pvr_proj(i_img), pvr_rgb(i_img),      &
     &      SR_sig, SR_r)
      end do
!
      end subroutine each_PVR_rendering
!
!  ---------------------------------------------------------------------
!
      subroutine each_PVR_rendering_w_rot(istep_pvr, time, elps_PVR,    &
     &          geofem, jacs, nod_fld, tracer, fline, sf_grp_4_sf,      &
     &          field_pvr, pvr_param, pvr_bound, pvr_rgb, pvr_proj,     &
     &          SR_sig, SR_r, SR_i)
!
      use cal_pvr_modelview_mat
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(elapsed_lables), intent(in) :: elps_PVR
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
      type(jacobians_type), intent(in) :: jacs
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!
      type(pvr_field_data), intent(inout) :: field_pvr
      type(PVR_control_params), intent(inout) :: pvr_param
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call calypso_mpi_barrier
      call cal_field_4_each_pvr(geofem%mesh%node, geofem%mesh%ele,      &
     &    jacs%g_FEM, jacs%jac_3d, nod_fld,                             &
     &    pvr_param%field_def, field_pvr)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_param%color)
!
!
      call rendering_with_rotation                                      &
     &   (istep_pvr, time, elps_PVR, geofem%mesh, geofem%group,         &
     &    tracer, fline, sf_grp_4_sf, field_pvr, pvr_rgb, pvr_param,    &
     &    pvr_bound, pvr_proj, SR_sig, SR_r, SR_i)
!
      end subroutine each_PVR_rendering_w_rot
!
!  ---------------------------------------------------------------------
!
      subroutine each_PVR_quilt_rendering_w_rot                         &
     &         (istep_pvr, time, num_img, elps_PVR, geofem, jacs,       &
     &          nod_fld, tracer, fline, sf_grp_4_sf, field_pvr,         &
     &          pvr_param, pvr_bound, pvr_proj, pvr_rgb,                &
     &          SR_sig, SR_r, SR_i)
!
      use m_work_time
      use set_PVR_view_and_image
      use rendering_vr_image
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: num_img
!
      type(elapsed_lables), intent(in) :: elps_PVR
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
      type(jacobians_type), intent(in) :: jacs
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!
      type(pvr_field_data), intent(inout) :: field_pvr
      type(PVR_control_params), intent(inout) :: pvr_param
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj(num_img)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_img)
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint) :: i_img, i_rot
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call calypso_mpi_barrier
      call cal_field_4_each_pvr(geofem%mesh%node, geofem%mesh%ele,      &
     &    jacs%g_FEM, jacs%jac_3d, nod_fld,                             &
     &    pvr_param%field_def, field_pvr)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_param%color)
!
      do i_rot = 1, pvr_param%movie_def%num_frame
        do i_img = 1, num_img
          call rot_multi_view_projection_mats(i_img, i_rot,             &
     &        pvr_param, pvr_proj(i_img)%screen)
          call rendering_at_once(istep_pvr, time, elps_PVR,             &
     &        geofem%mesh, geofem%group, tracer, fline, sf_grp_4_sf,    &
     &        field_pvr, pvr_param, pvr_bound, pvr_proj(i_img),         &
     &        pvr_rgb(i_img), SR_sig, SR_r, SR_i)
        end do
        if(elps_PVR%flag_elapsed)                                       &
     &           call end_elapsed_time(elps_PVR%ist_elapsed+1)
!
        if(elps_PVR%flag_elapsed)                                       &
     &           call start_elapsed_time(elps_PVR%ist_elapsed+2)
        call set_output_rot_sequence_image(istep_pvr, i_rot,            &
     &      pvr_rgb(1)%id_pvr_file_type, pvr_rgb(1)%pvr_prefix,         &
     &      num_img, pvr_param%stereo_def%n_column_row_view,            &
     &      pvr_rgb)
        if(elps_PVR%flag_elapsed)                                       &
     &           call end_elapsed_time(elps_PVR%ist_elapsed+2)
        if(elps_PVR%flag_elapsed)                                       &
     &           call start_elapsed_time(elps_PVR%ist_elapsed+1)
      end do
!
      end subroutine each_PVR_quilt_rendering_w_rot
!
!  ---------------------------------------------------------------------
!
      end module each_volume_rendering
