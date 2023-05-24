!>@file   each_anaglyph_PVR.f90
!!@brief  module each_anaglyph_PVR
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module for each volume rendering
!!
!!@verbatim
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
      subroutine each_PVR_anaglyph                                      &
     &         (istep_pvr, time, geofem, jacs, nod_fld, sf_grp_4_sf,    &
     &          field_pvr, pvr_param, pvr_proj, pvr_rgb, SR_sig, SR_r)
!
      use cal_pvr_modelview_mat
      use rendering_vr_image
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
      type(pvr_image_type), intent(inout) :: pvr_rgb(2)
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
     &    pvr_param, pvr_proj(1), pvr_rgb(1), SR_sig, SR_r)
      call store_left_eye_image(pvr_rgb(1))
!
!   right eye
      call rendering_with_fixed_view(istep_pvr, time,                   &
     &    geofem%mesh, geofem%group, sf_grp_4_sf, field_pvr,            &
     &    pvr_param, pvr_proj(2), pvr_rgb(1), SR_sig, SR_r)
      call add_left_eye_image(pvr_rgb(1))
!
      end subroutine each_PVR_anaglyph
!
!  ---------------------------------------------------------------------
!
      end module each_anaglyph_PVR
