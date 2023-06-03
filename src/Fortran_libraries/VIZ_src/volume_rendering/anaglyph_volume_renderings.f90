!>@file   anaglyph_volume_renderings.f90
!!@brief  module anaglyph_volume_renderings
!!
!!@date  Programmed by H.Matsui in May. 2006
!!       Modified by H.Matsui in May, 2021
!
!>@brief Main routines for volume renderings
!!
!!@verbatim
!!      subroutine PVR_anaglyph_view_and_images                         &
!!     &         (num_pvr, num_pvr_images, mesh, PVR_sort, pvr_rgb,     &
!!     &          pvr_param, pvr_bound, pvr_proj, m_SR)
!!        integer(kind = kint), intent(in) :: num_pvr, num_pvr_images
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(sort_PVRs_by_type), intent(in) :: PVR_sort
!!        type(pvr_image_type), intent(in) :: pvr_rgb(num_pvr_images)
!!        type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound(num_pvr)
!!        type(PVR_projection_data), intent(inout)                      &
!!     &                            :: pvr_proj(num_pvr_images)
!!        type(mesh_SR), intent(inout) :: m_SR
!!
!!      subroutine PVR_anaglyph_rendering(istep_pvr, time, geofem, jacs,&
!!     &                                  nod_fld, pvr, m_SR)
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        real(kind = kreal), intent(in) :: time
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(volume_rendering_module), intent(inout) :: pvr
!!        type(mesh_SR), intent(inout) :: m_SR
!!
!!      subroutine PVR_movie_anaglyph_visualize                         &
!!     &         (istep_pvr, time, geofem, jacs, nod_fld, pvr, m_SR)
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        real(kind = kreal), intent(in) :: time
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(volume_rendering_module), intent(inout) :: pvr
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module anaglyph_volume_renderings
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_work_time
      use m_elapsed_labels_4_VIZ
!
      use t_mesh_data
      use t_phys_data
      use t_jacobians
!
      use t_volume_rendering
      use t_surf_grp_list_each_surf
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_pvr_field_data
      use t_geometries_in_pvr_screen
      use t_control_data_pvrs
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
      subroutine PVR_anaglyph_view_and_images                           &
     &         (num_pvr, num_pvr_images, mesh, PVR_sort, pvr_rgb,       &
     &          pvr_param, pvr_bound, pvr_proj, m_SR)
!
      use set_PVR_view_and_image
!
      integer(kind = kint), intent(in) :: num_pvr, num_pvr_images
      type(mesh_geometry), intent(in) :: mesh
      type(sort_PVRs_by_type), intent(in) :: PVR_sort
      type(pvr_image_type), intent(in) :: pvr_rgb(num_pvr_images)
      type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound(num_pvr)
      type(PVR_projection_data), intent(inout)                          &
     &                            :: pvr_proj(num_pvr_images)
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_pvr, ist_pvr, ied_pvr, ist_img
!
!      Anaglyph with fixed view
      ist_pvr = PVR_sort%istack_PVR_modes(4) + 1
      ied_pvr = PVR_sort%istack_PVR_modes(5)
      do i_pvr = ist_pvr, ied_pvr
        ist_img = PVR_sort%istack_pvr_images(i_pvr-1)
        call anaglyph_PVR_view_matrices                                 &
     &     (mesh, pvr_rgb(ist_img+1), pvr_param(i_pvr),                 &
     &      pvr_bound(i_pvr), pvr_proj(ist_img+1), m_SR)
      end do
!
      end subroutine PVR_anaglyph_view_and_images
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine PVR_anaglyph_rendering(istep_pvr, time, geofem, jacs,  &
     &                                  nod_fld, pvr, m_SR)
!
      use each_volume_rendering
      use each_anaglyph_PVR
      use write_multi_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(volume_rendering_module), intent(inout) :: pvr
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_pvr, ist_pvr, ied_pvr, ist_img
!
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+1)
      ist_pvr = pvr%PVR_sort%istack_PVR_modes(4) + 1
      ied_pvr = pvr%PVR_sort%istack_PVR_modes(5)
      do i_pvr = ist_pvr, ied_pvr
        ist_img = pvr%PVR_sort%istack_pvr_images(i_pvr-1)
        call each_PVR_anaglyph(istep_pvr, time,                         &
     &      geofem%mesh, geofem%group, jacs, nod_fld, pvr%sf_grp_4_sf,  &
     &      pvr%field_pvr(i_pvr), pvr%pvr_param(i_pvr),                 &
     &      pvr%pvr_proj(ist_img+1), pvr%pvr_rgb(ist_img+1),            &
     &      m_SR%SR_sig, m_SR%SR_r)
      end do
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+1)
!
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+2)
      call output_PVR_images(istep_pvr, pvr%num_pvr, ist_pvr, ied_pvr,  &
     &    pvr%num_pvr_images, pvr%PVR_sort%istack_pvr_images,           &
     &    pvr%pvr_rgb)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+2)
!
      end subroutine PVR_anaglyph_rendering
!
!  ---------------------------------------------------------------------
!
      subroutine PVR_movie_anaglyph_visualize                           &
     &         (istep_pvr, time, geofem, jacs, nod_fld, pvr, m_SR)
!
      use each_anaglyph_PVR
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(volume_rendering_module), intent(inout) :: pvr
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_pvr, ist_pvr, ied_pvr, ist_img
!
!
      ist_pvr = pvr%PVR_sort%istack_PVR_modes(5) + 1
      ied_pvr = pvr%PVR_sort%istack_PVR_modes(6)
      do i_pvr = ist_pvr, ied_pvr
        ist_img = pvr%PVR_sort%istack_pvr_images(i_pvr-1)
        call anaglyph_rendering_w_rotation(istep_pvr, time,             &
     &      geofem%mesh, geofem%group, nod_fld, jacs, pvr%sf_grp_4_sf,  &
     &      pvr%field_pvr(i_pvr), pvr%pvr_param(i_pvr),                 &
     &      pvr%pvr_bound(i_pvr), pvr%pvr_proj(ist_img+1),              &
     &      pvr%pvr_rgb(ist_img+1), m_SR%SR_sig, m_SR%SR_r, m_SR%SR_i)
      end do
!
      end subroutine PVR_movie_anaglyph_visualize
!
!  ---------------------------------------------------------------------
!
      end module anaglyph_volume_renderings
