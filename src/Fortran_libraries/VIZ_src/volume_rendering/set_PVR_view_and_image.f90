!>@file   set_PVR_view_and_image.f90
!!@brief  module set_PVR_view_and_image
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module for each volume rendering
!!
!!@verbatim
!!      subroutine single_PVR_view_matrices(elps_PVR, mesh,             &
!!     &          pvr_rgb, pvr_param, pvr_bound, pvr_proj, m_SR)
!!      subroutine quilt_PVR_view_matrices(num_img, elps_PVR, mesh,     &
!!     &          pvr_rgb, pvr_param, pvr_bound, pvr_proj, m_SR)
!!      subroutine anaglyph_PVR_view_matrices(elps_PVR, mesh,           &
!!     &          pvr_rgb, pvr_param, pvr_bound, pvr_proj, m_SR)
!!        type(elapsed_lables), intent(in) :: elps_PVR
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(pvr_image_type), intent(in) :: pvr_rgb
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(PVR_projection_data), intent(inout) :: pvr_proj
!!        type(mesh_SR), intent(inout) :: m_SR
!!
!!      subroutine rotation_view_projection_mats(i_rot,                 &
!!     &                                         pvr_param, screen)
!!      subroutine rot_multi_view_projection_mats(i_img, i_rot,         &
!!     &                                          pvr_param, screen)
!!        integer(kind = kint), intent(in) :: i_rot
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(pvr_projected_position), intent(inout) :: screen
!!@endverbatim
!
      module set_PVR_view_and_image
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_work_time
!
      use t_mesh_data
      use t_pvr_image_array
      use t_rendering_vr_image
      use t_surf_grp_4_pvr_domain
      use t_geometries_in_pvr_screen
      use t_mesh_SR
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine single_PVR_view_matrices(elps_PVR, mesh,               &
     &          pvr_rgb, pvr_param, pvr_bound, pvr_proj, m_SR)
!
      use rendering_vr_image
!
      type(elapsed_lables), intent(in) :: elps_PVR
      type(mesh_geometry), intent(in) :: mesh
      type(pvr_image_type), intent(in) :: pvr_rgb
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call rotation_view_projection_mats(izero, pvr_param,              &
     &                                   pvr_proj%screen)
      call set_fixed_view_and_image(elps_PVR, mesh, pvr_param, pvr_rgb, &
     &                              pvr_bound, pvr_proj, m_SR)
!
      end subroutine single_PVR_view_matrices
!
!  ---------------------------------------------------------------------
!
      subroutine quilt_PVR_view_matrices(num_img, elps_PVR, mesh,       &
     &          pvr_rgb, pvr_param, pvr_bound, pvr_proj, m_SR)
!
      use rendering_vr_image
!
      integer(kind = kint), intent(in) :: num_img
      type(elapsed_lables), intent(in) :: elps_PVR
      type(mesh_geometry), intent(in) :: mesh
      type(pvr_image_type), intent(in) :: pvr_rgb(num_img)
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj(num_img)
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_img
!
!
      do i_img = 1, num_img
        call rot_multi_view_projection_mats(i_img, izero, pvr_param,    &
     &                                        pvr_proj(i_img)%screen)
        call set_fixed_view_and_image(elps_PVR, mesh, pvr_param,        &
     &      pvr_rgb(i_img), pvr_bound, pvr_proj(i_img), m_SR)
      end do
!
      end subroutine quilt_PVR_view_matrices
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_PVR_view_matrices(elps_PVR, mesh,             &
     &          pvr_rgb, pvr_param, pvr_bound, pvr_proj, m_SR)
!
      use rendering_vr_image
!
      type(elapsed_lables), intent(in) :: elps_PVR
      type(mesh_geometry), intent(in) :: mesh
      type(pvr_image_type), intent(in) :: pvr_rgb
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call rot_multi_view_projection_mats(ione, izero, pvr_param,       &
     &                                    pvr_proj(1)%screen)
      call rot_multi_view_projection_mats(itwo, izero, pvr_param,       &
     &                                    pvr_proj(2)%screen)
      call set_fixed_view_and_image(elps_PVR, mesh, pvr_param, pvr_rgb, &
     &                              pvr_bound, pvr_proj(1), m_SR)
      call set_fixed_view_and_image(elps_PVR, mesh, pvr_param, pvr_rgb, &
     &                              pvr_bound, pvr_proj(2), m_SR)
!
      end subroutine anaglyph_PVR_view_matrices
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine rotation_view_projection_mats(i_rot,                   &
     &                                         pvr_param, screen)
!
      use cal_pvr_modelview_mat
      use cal_pvr_projection_mat
!
      integer(kind = kint), intent(in) :: i_rot
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(pvr_projected_position), intent(inout) :: screen
!
!
      call set_pvr_projection_matrix                                    &
     &   (pvr_param%multi_view(1), screen%projection_mat)
      call cal_pvr_modelview_matrix(ione, i_rot,                        &
     &    pvr_param%outline, pvr_param%movie_def,                       &
     &    pvr_param%stereo_def, pvr_param%multi_view(1),                &
     &    screen%viewpoint_vec, screen%modelview_mat)
!
      end subroutine rotation_view_projection_mats
!
!  ---------------------------------------------------------------------
!
      subroutine rot_multi_view_projection_mats(i_img, i_rot,           &
     &                                          pvr_param, screen)
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
      call set_pvr_step_projection_mat                                  &
     &   (i_img, pvr_param%multi_view(1), pvr_param%stereo_def,         &
     &    screen%projection_mat)
      call cal_pvr_modelview_matrix(i_img, i_rot,                       &
     &    pvr_param%outline, pvr_param%movie_def,                       &
     &    pvr_param%stereo_def, pvr_param%multi_view(1),                &
     &    screen%viewpoint_vec, screen%modelview_mat)
!
      end subroutine rot_multi_view_projection_mats
!
!  ---------------------------------------------------------------------
!
      end module set_PVR_view_and_image
