!>@file  rendering_streo_vr_image.f90
!!       module rendering_streo_vr_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine rendering_with_rotation                              &
!!     &         (istep_pvr, time, elps_PVR, mesh, group, tracer, fline,&
!!     &          sf_grp_4_sf, field_pvr, pvr_rgb, pvr_param,           &
!!     &          pvr_bound, pvr_proj, SR_sig, SR_r, SR_i)
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        real(kind = kreal), intent(in) :: time
!!        type(elapsed_lables), intent(in) :: elps_PVR
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(tracer_module), intent(in) :: tracer
!!        type(fieldline_module), intent(in) :: fline
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(pvr_field_data), intent(in) :: field_pvr
!!        type(pvr_image_type), intent(in) :: pvr_rgb
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(PVR_projection_data), intent(inout) :: pvr_proj
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!@endverbatim
!
      module rendering_streo_vr_image
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
      use t_phys_data
      use t_jacobians
      use t_surf_grp_list_each_surf
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_pvr_field_data
      use t_solver_SR
      use t_solver_SR_int
      use generate_vr_image
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_with_rotation                                &
     &         (istep_pvr, time, elps_PVR, mesh, group, tracer, fline,  &
     &          sf_grp_4_sf, field_pvr, pvr_rgb, pvr_param,             &
     &          pvr_bound, pvr_proj, SR_sig, SR_r, SR_i)
!
      use t_rotation_pvr_images
      use set_PVR_view_and_image
      use write_multi_PVR_image
      use output_image_sel_4_png
      use rendering_vr_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(elapsed_lables), intent(in) :: elps_PVR
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(pvr_field_data), intent(in) :: field_pvr
      type(pvr_image_type), intent(in) :: pvr_rgb
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint) :: i_rot
      type(rotation_pvr_images) :: rot_imgs1
!
!
      call init_rot_pvr_image_arrays                                    &
     &   (pvr_param%movie_def, pvr_rgb, rot_imgs1)
!
      do i_rot = 1, pvr_param%movie_def%num_frame
        call rotation_view_projection_mats(i_rot, pvr_param,            &
     &                                     pvr_proj%screen)
        call rendering_at_once(istep_pvr, time, elps_PVR,               &
     &      mesh, group, tracer, fline, sf_grp_4_sf,                    &
     &      field_pvr, pvr_param, pvr_bound, pvr_proj,                  &
     &      rot_imgs1%rot_pvr_rgb(i_rot), SR_sig, SR_r, SR_i)
      end do
      if(elps_PVR%flag_elapsed)                                         &
     &         call end_elapsed_time(elps_PVR%ist_elapsed+1)
!
      if(elps_PVR%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_PVR%ist_elapsed+2)
      call output_rotation_PVR_images(istep_pvr,                        &
     &    pvr_param%movie_def%num_frame, rot_imgs1%rot_pvr_rgb(1))
      call dealloc_rot_pvr_image_arrays(pvr_param%movie_def, rot_imgs1)
      if(elps_PVR%flag_elapsed)                                         &
     &         call end_elapsed_time(elps_PVR%ist_elapsed+2)
      if(elps_PVR%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_PVR%ist_elapsed+1)
!
      end subroutine rendering_with_rotation
!
!  ---------------------------------------------------------------------
!
      end module rendering_streo_vr_image
