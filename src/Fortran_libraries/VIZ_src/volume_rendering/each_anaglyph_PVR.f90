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
!!     &         (istep_pvr, time, elps_PVR, mesh, group, jacs,         &
!!     &          nod_fld, tracer, fline, sf_grp_4_sf, field_pvr,       &
!!     &          pvr_param, pvr_proj, pvr_rgb, SR_sig, SR_r)
!!      subroutine anaglyph_rendering_w_rotation(istep_pvr, time,       &
!!     &          elps_PVR, mesh, group, jacs, nod_fld, tracer, fline,  &
!!     &          sf_grp_4_sf, field_pvr, pvr_param, pvr_bound,         &
!!     &          pvr_proj, pvr_rgb, SR_sig, SR_r, SR_i)
!!        type(elapsed_lables), intent(in) :: elps_PVR
!!        type(mesh_data), intent(in) :: geofem
!!        type(viz_area_parameter), intent(in) :: area_def
!!        type(phys_data), intent(in) :: nod_fld
!!        type(tracer_module), intent(in) :: tracer
!!        type(fieldline_module), intent(in) :: fline
!!        type(jacobians_type), intent(in) :: jacs
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(pvr_field_data), intent(inout) :: field_pvr
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(PVR_projection_data), intent(inout) :: pvr_proj(2)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
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
      subroutine each_PVR_anaglyph                                      &
     &         (istep_pvr, time, elps_PVR, mesh, group, jacs,           &
     &          nod_fld, tracer, fline, sf_grp_4_sf, field_pvr,         &
     &          pvr_param, pvr_proj, pvr_rgb, SR_sig, SR_r)
!
      use cal_pvr_modelview_mat
      use rendering_vr_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(elapsed_lables), intent(in) :: elps_PVR
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
      type(jacobians_type), intent(in) :: jacs
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!
      type(pvr_field_data), intent(inout) :: field_pvr
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call cal_field_4_each_pvr                                         &
     &   (mesh%node, mesh%ele, jacs%g_FEM, jacs%jac_3d, nod_fld,        &
     &    pvr_param%field_def, field_pvr)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_param%color)
!
!   Left eye
      call alloc_pvr_left_eye_image(pvr_rgb)
      call rendering_with_fixed_view                                    &
     &   (istep_pvr, time, elps_PVR, mesh, group, tracer, fline,        &
     &    sf_grp_4_sf, field_pvr, pvr_param, pvr_proj(1), pvr_rgb,      &
     &    SR_sig, SR_r)
      call store_left_eye_image(pvr_rgb)
!
!   right eye
      call rendering_with_fixed_view                                    &
     &   (istep_pvr, time, elps_PVR, mesh, group, tracer, fline,        &
     &    sf_grp_4_sf, field_pvr, pvr_param, pvr_proj(2), pvr_rgb,      &
     &    SR_sig, SR_r)
      call add_left_eye_image(pvr_rgb)
      call dealloc_pvr_left_eye_image(pvr_rgb)
!
      end subroutine each_PVR_anaglyph
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_rendering_w_rotation(istep_pvr, time,         &
     &          elps_PVR, mesh, group, jacs, nod_fld, tracer, fline,    &
     &          sf_grp_4_sf, field_pvr, pvr_param, pvr_bound,           &
     &          pvr_proj, pvr_rgb, SR_sig, SR_r, SR_i)
!
      use m_work_time
      use t_rotation_pvr_images
      use write_multi_PVR_image
      use set_PVR_view_and_image
      use set_default_pvr_params
      use output_image_sel_4_png
      use rendering_vr_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(elapsed_lables), intent(in) :: elps_PVR
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
      type(jacobians_type), intent(in) :: jacs
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!
      type(pvr_field_data), intent(inout) :: field_pvr
      type(PVR_control_params), intent(inout) :: pvr_param
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint) :: i_rot
      type(rotation_pvr_images) :: rot_imgs1
!
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call cal_field_4_each_pvr                                         &
     &   (mesh%node, mesh%ele, jacs%g_FEM, jacs%jac_3d, nod_fld,        &
     &    pvr_param%field_def, field_pvr)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_param%color)
!
      if(my_rank .eq. 0) write(*,*) 'init_rot_pvr_image_arrays'
      call init_rot_pvr_image_arrays                                    &
     &   (pvr_param%movie_def, pvr_rgb, rot_imgs1)
!
!
      call alloc_pvr_left_eye_image(pvr_rgb)
      do i_rot = 1, pvr_param%movie_def%num_frame
!    Left eye
        call rot_multi_view_projection_mats(ione, i_rot,                &
     &      pvr_param, pvr_proj(1)%screen)
        call rendering_at_once                                          &
     &     (istep_pvr, time, elps_PVR, mesh, group, tracer, fline,      &
     &      sf_grp_4_sf, field_pvr, pvr_param, pvr_bound,               &
     &      pvr_proj(1), pvr_rgb, SR_sig, SR_r, SR_i)
        call store_left_eye_image(pvr_rgb)
!
!    Right eye
        call rot_multi_view_projection_mats(itwo, i_rot,                &
     &      pvr_param, pvr_proj(2)%screen)
        call rendering_at_once                                          &
     &     (istep_pvr, time, elps_PVR, mesh, group, tracer, fline,      &
     &      sf_grp_4_sf, field_pvr, pvr_param, pvr_bound,               &
     &      pvr_proj(2), pvr_rgb, SR_sig, SR_r, SR_i)
        call add_left_eye_image(pvr_rgb)
        call copy_pvr_image_data(pvr_rgb, rot_imgs1%rot_pvr_rgb(i_rot))
      end do
      call dealloc_pvr_left_eye_image(pvr_rgb)
      if(elps_PVR%flag_elapsed)                                         &
     &          call end_elapsed_time(elps_PVR%ist_elapsed+1)
!
      if(elps_PVR%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_PVR%ist_elapsed+2)
      call output_rotation_PVR_images(istep_pvr,                        &
     &    pvr_param%movie_def%num_frame, rot_imgs1%rot_pvr_rgb(1))
      call dealloc_rot_pvr_image_arrays(pvr_param%movie_def, rot_imgs1)
      if(elps_PVR%flag_elapsed)                                         &
     &          call end_elapsed_time(elps_PVR%ist_elapsed+2)
      if(elps_PVR%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_PVR%ist_elapsed+1)
!
      end subroutine anaglyph_rendering_w_rotation
!
!  ---------------------------------------------------------------------
!
      end module each_anaglyph_PVR
