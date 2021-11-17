!>@file   each_anaglyph_PVR.f90
!!@brief  module each_anaglyph_PVR
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module for each volume rendering
!!
!!@verbatim
!!      subroutine each_anaglyph_PVR_init(i_pvr, mesh, group,           &
!!     &          pvr_rgb, pvr_param, pvr_bound, pvr_proj,              &
!!     &          SR_sig, SR_r, SR_i)
!!        integer(kind = kint), intent(in) :: i_pvr, num_img
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(pvr_image_type), intent(in) :: pvr_rgb
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(PVR_projection_data), intent(inout) :: pvr_proj(num_img)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!
!!      subroutine each_PVR_anaglyph                                    &
!!     &         (istep_pvr, time, geofem, jacs, nod_fld, sf_grp_4_sf,  &
!!     &          field_pvr, pvr_param, pvr_proj, pvr_rgb, SR_sig, SR_r)
!!        type(mesh_data), intent(in) :: geofem
!!        type(viz_area_parameter), intent(in) :: area_def
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(pvr_field_data), intent(inout) :: field_pvr
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(PVR_projection_data), intent(inout) :: pvr_proj(2)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module each_anaglyph_PVR
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
!
      use t_surf_grp_list_each_surf
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_geometries_in_pvr_screen
      use t_pvr_field_data
      use t_solver_SR
      use t_solver_SR_int
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
      subroutine each_anaglyph_PVR_init(i_pvr, mesh, group,             &
     &          pvr_rgb, pvr_param, pvr_bound, pvr_proj,                &
     &          SR_sig, SR_r, SR_i)
!
      use t_control_data_pvr_sections
      use set_pvr_control
      use cal_pvr_projection_mat
      use find_pvr_surf_domain
      use set_iflag_for_used_ele
!
      integer(kind = kint), intent(in) :: i_pvr
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(pvr_image_type), intent(in) :: pvr_rgb
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
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
      if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_left'
      call set_pvr_projection_left_mat                                  &
     &   (pvr_param%multi_view(1), pvr_param%stereo_def,                &
     &    pvr_proj(1)%screen%projection_mat)
      if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_right'
      call set_pvr_projection_right_mat                                 &
     &   (pvr_param%multi_view(1), pvr_param%stereo_def,                &
     &    pvr_proj(2)%screen%projection_mat)
!
!
      if(pvr_param%movie_def%iflag_movie_mode                           &
     &                                 .ne. IFLAG_NO_MOVIE) return
      if(iflag_debug.gt.0) write(*,*) 'set_fixed_view_and_image'
      call set_fixed_view_and_image(ione, itwo, mesh, pvr_param,        &
     &    pvr_rgb, pvr_bound, pvr_proj(1), SR_sig, SR_r, SR_i)
      call set_fixed_view_and_image(itwo, itwo, mesh, pvr_param,        &
     &    pvr_rgb, pvr_bound, pvr_proj(2), SR_sig, SR_r, SR_i)
!
      end subroutine each_anaglyph_PVR_init
!
!  ---------------------------------------------------------------------
!
      subroutine each_PVR_anaglyph                                      &
     &         (istep_pvr, time, geofem, jacs, nod_fld, sf_grp_4_sf,    &
     &          field_pvr, pvr_param, pvr_proj, pvr_rgb, SR_sig, SR_r)
!
      use cal_pvr_modelview_mat
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(pvr_field_data), intent(inout) :: field_pvr
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
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
!   Left eye
      call rendering_with_fixed_view(istep_pvr, time,                   &
     &    geofem%mesh, geofem%group, sf_grp_4_sf, field_pvr,            &
     &    pvr_param, pvr_proj(1), pvr_rgb, SR_sig, SR_r)
      call store_left_eye_image(pvr_rgb)
!
!   right eye
      call rendering_with_fixed_view(istep_pvr, time,                   &
     &    geofem%mesh, geofem%group, sf_grp_4_sf, field_pvr,            &
     &    pvr_param, pvr_proj(2), pvr_rgb, SR_sig, SR_r)
      call add_left_eye_image(pvr_rgb)
!
      end subroutine each_PVR_anaglyph
!
!  ---------------------------------------------------------------------
!
      end module each_anaglyph_PVR
