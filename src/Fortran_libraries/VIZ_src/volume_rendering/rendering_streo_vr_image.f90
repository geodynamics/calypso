!>@file  rendering_streo_vr_image.f90
!!       module rendering_streo_vr_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine rendering_with_rotation(istep_pvr, time, mesh, group,&
!!     &          sf_grp_4_sf, field_pvr, pvr_rgb, pvr_param, pvr_bound,&
!!     &          pvr_proj, SR_sig, SR_r, SR_i)
!!      subroutine anaglyph_rendering_w_rotation(istep_pvr, time,       &
!!     &           mesh, group, nod_fld, jacs, sf_grp_4_sf, pvr_rgb,    &
!!     &           field_pvr, pvr_param, pvr_bound, pvr_proj,           &
!!     &           SR_sig, SR_r, SR_i)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(pvr_image_type), intent(in) :: pvr_rgb
!!        type(pvr_field_data), intent(inout) :: field_pvr
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
      subroutine rendering_with_rotation(istep_pvr, time, mesh, group,  &
     &          sf_grp_4_sf, field_pvr, pvr_rgb, pvr_param, pvr_bound,  &
     &          pvr_proj, SR_sig, SR_r, SR_i)
!
      use t_rotation_pvr_images
      use m_elapsed_labels_4_VIZ
      use write_PVR_image
      use output_image_sel_4_png
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
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
      integer(kind = kint) :: i_rot, iflag_img_fmt
      type(rotation_pvr_images) :: rot_imgs1
!
!
      call init_rot_pvr_image_arrays                                    &
     &   (pvr_param%movie_def, pvr_rgb, rot_imgs1)
!
      do i_rot = 1, pvr_param%movie_def%num_frame
        call rendering_at_once(istep_pvr, time, izero, i_rot,           &
     &      mesh, group, sf_grp_4_sf, field_pvr, pvr_param, pvr_bound,  &
     &      pvr_proj, rot_imgs1%rot_pvr_rgb(i_rot), SR_sig, SR_r, SR_i)
      end do
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+1)
!
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+2)
      if(pvr_param%movie_def%iflag_movie_fmt                            &
     &                     .eq. iflag_UNDEFINED) then
        iflag_img_fmt = pvr_rgb%id_pvr_file_type
      else
        iflag_img_fmt = pvr_param%movie_def%iflag_movie_fmt
      end if
!
      call set_output_rot_sequence_image(istep_pvr, iflag_img_fmt,      &
     &    pvr_rgb%pvr_prefix, pvr_param%movie_def%num_frame,            &
     &    pvr_param%movie_def%n_column_row_movie,                       &
     &    rot_imgs1%rot_pvr_rgb)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+2)
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+1)
      call dealloc_rot_pvr_image_arrays(pvr_param%movie_def, rot_imgs1)
!
      end subroutine rendering_with_rotation
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_rendering_w_rotation(istep_pvr, time,         &
     &           mesh, group, nod_fld, jacs, sf_grp_4_sf, pvr_rgb,      &
     &           field_pvr, pvr_param, pvr_bound, pvr_proj,             &
     &           SR_sig, SR_r, SR_i)
!
      use t_rotation_pvr_images
      use m_elapsed_labels_4_VIZ
      use write_PVR_image
      use set_default_pvr_params
      use output_image_sel_4_png
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(pvr_image_type), intent(in) :: pvr_rgb
!
      type(pvr_field_data), intent(inout) :: field_pvr
      type(PVR_control_params), intent(inout) :: pvr_param
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint) :: i_rot, iflag_img_fmt
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
      do i_rot = 1, pvr_param%movie_def%num_frame
!    Left eye
        call rendering_at_once(istep_pvr, time, ione, i_rot,            &
     &      mesh, group, sf_grp_4_sf, field_pvr, pvr_param, pvr_bound,  &
     &      pvr_proj(1), rot_imgs1%rot_pvr_rgb(i_rot),                  &
     &      SR_sig, SR_r, SR_i)
        call store_left_eye_image(rot_imgs1%rot_pvr_rgb(i_rot))
!
!    Right eye
        call rendering_at_once (istep_pvr, time, itwo, i_rot,           &
     &      mesh, group, sf_grp_4_sf, field_pvr, pvr_param, pvr_bound,  &
     &      pvr_proj(2), rot_imgs1%rot_pvr_rgb(i_rot),                  &
     &      SR_sig, SR_r, SR_i)
        call add_left_eye_image(rot_imgs1%rot_pvr_rgb(i_rot))
      end do
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+1)
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+2)
      if(pvr_param%movie_def%iflag_movie_fmt.eq.iflag_UNDEFINED         &
     &  .or. pvr_param%movie_def%iflag_movie_fmt.eq.iflag_QUILT_BMP     &
     &  .or. pvr_param%movie_def%iflag_movie_fmt                        &
     &                        .eq. iflag_QUILT_BMP_GZ) then
        iflag_img_fmt = pvr_rgb%id_pvr_file_type
      else
        iflag_img_fmt = pvr_param%movie_def%iflag_movie_fmt
      end if
!
      call set_output_rot_sequence_image(istep_pvr, iflag_img_fmt,      &
     &    pvr_rgb%pvr_prefix, pvr_param%movie_def%num_frame,            &
     &    pvr_param%movie_def%n_column_row_movie,                       &
     &    rot_imgs1%rot_pvr_rgb)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+2)
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+1)
      call dealloc_rot_pvr_image_arrays(pvr_param%movie_def, rot_imgs1)
!
      end subroutine anaglyph_rendering_w_rotation
!
!  ---------------------------------------------------------------------
!
      end module rendering_streo_vr_image
