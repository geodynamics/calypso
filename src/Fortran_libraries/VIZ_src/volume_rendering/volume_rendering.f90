!>@file   volume_rendering.f90
!!@brief  module volume_rendering
!!
!!@date  Programmed by H.Matsui in May. 2006
!!       Modified by H.Matsui in May, 2021
!
!>@brief Main routines for volume renderings
!!
!!@verbatim
!!      subroutine PVR_initialize(increment_pvr, geofem, nod_fld,       &
!!     &                          pvr_ctls, pvr, m_SR)
!!      subroutine PVR_visualize(istep_pvr, time, geofem, jacs,         &
!!     &                         nod_fld, pvr, m_SR)
!!        type(mesh_data), intent(in) :: geofem
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(volume_rendering_controls), intent(inout) :: pvr_ctls
!!        type(volume_rendering_module), intent(inout) :: pvr
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module volume_rendering
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
      subroutine PVR_initialize(increment_pvr, geofem, nod_fld,         &
     &                          pvr_ctls, pvr, m_SR)
!
      use m_work_time
      use m_elapsed_labels_4_VIZ
      use t_control_data_pvr_sections
      use set_pvr_control
      use multi_volume_renderings
      use anaglyph_volume_renderings
!
      integer(kind = kint), intent(in) :: increment_pvr
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
      type(volume_rendering_module), intent(inout) :: pvr
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_pvr, ist_img, num_img
!
!
      pvr%num_pvr = pvr_ctls%num_pvr_ctl
      if(increment_pvr .le. 0) pvr%num_pvr = 0
!
      if(pvr%num_pvr .le. 0) then
        pvr%num_pvr = 0
        return
      end if
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+5)
      call bcast_pvr_controls(pvr%num_pvr,                              &
     &    pvr_ctls%pvr_ctl_type, pvr%cflag_update)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+5)
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+6)
      call set_from_PVR_control(geofem, nod_fld, pvr_ctls, pvr)
!
      call dealloc_pvr_ctl_struct(pvr_ctls)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+6)
!      do i_pvr = 1, pvr_ctls%num_pvr_ctl
!        if((no_file_flag(pvr_ctls%fname_pvr_ctl(i_pvr)) .eqv. .FALSE.) &
!     &      .or. my_rank .ne. 0) then
!          call deallocate_cont_dat_pvr(pvr_ctls%pvr_ctl_type(i_pvr))
!        end if
!      end do
!
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+7)
      call init_sf_grp_list_each_surf                                   &
     &   (geofem%mesh%surf, geofem%group%surf_grp, pvr%sf_grp_4_sf)
      do i_pvr = 1, pvr%num_pvr
        ist_img = pvr%PVR_sort%istack_pvr_images(i_pvr-1)
        num_img = pvr%PVR_sort%istack_pvr_images(i_pvr  ) - ist_img
        call init_each_PVR_image(num_img, pvr%pvr_param(i_pvr),         &
     &                           pvr%pvr_rgb(ist_img+1))
        call each_PVR_initialize(geofem%mesh, geofem%group,             &
     &      pvr%pvr_param(i_pvr), pvr%pvr_bound(i_pvr))
      end do
!
!
      call set_PVR_view_and_images(pvr%num_pvr, pvr%num_pvr_images,     &
     &    geofem%mesh, pvr%PVR_sort, pvr%pvr_rgb, pvr%pvr_param,        &
     &    pvr%pvr_bound, pvr%pvr_proj, m_SR)
      call PVR_anaglyph_view_and_images                                 &
     &   (pvr%num_pvr, pvr%num_pvr_images, geofem%mesh,                 &
     &    pvr%PVR_sort, pvr%pvr_rgb, pvr%pvr_param,                     &
     &    pvr%pvr_bound, pvr%pvr_proj, m_SR)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+7)
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
!
      end subroutine PVR_initialize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine PVR_visualize(istep_pvr, time, geofem, jacs,           &
     &                         nod_fld, pvr, m_SR)
!
      use cal_pvr_modelview_mat
      use multi_volume_renderings
      use anaglyph_volume_renderings
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
      integer(kind = kint) :: ist_pvr, ied_pvr
!
!
      if(pvr%num_pvr.le.0 .or. istep_pvr.le.0) return
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+1)
      call PVR_fixview_rendering(istep_pvr, time, geofem, jacs,         &
     &                           nod_fld, pvr, m_SR)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+1)
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+2)
      ist_pvr = pvr%PVR_sort%istack_PVR_modes(0) + 1
      ied_pvr = pvr%PVR_sort%istack_PVR_modes(1)
      call output_PVR_images(istep_pvr, pvr%num_pvr, ist_pvr, ied_pvr,  &
     &    pvr%num_pvr_images, pvr%PVR_sort%istack_pvr_images,           &
     &    pvr%pvr_rgb)
!
      ist_pvr = pvr%PVR_sort%istack_PVR_modes(1) + 1
      ied_pvr = pvr%PVR_sort%istack_PVR_modes(2)
      call output_quilt_PVR_images                                      &
     &   (istep_pvr, pvr%num_pvr, ist_pvr, ied_pvr,                     &
     &    pvr%num_pvr_images, pvr%PVR_sort%istack_pvr_images,           &
     &    pvr%pvr_param, pvr%pvr_rgb)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+2)
!
!      generate snapshot movie images
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+1)
      call PVR_movie_visualize(istep_pvr, time, geofem, jacs,           &
     &                         nod_fld, pvr, m_SR)
!
!      generate snapshot quilt movie images
      call PVR_quilt_movie_visualize(istep_pvr, time, geofem, jacs,     &
     &                               nod_fld, pvr, m_SR)
!
      call PVR_anaglyph_rendering(istep_pvr, time, geofem, jacs,        &
     &                            nod_fld, pvr, m_SR)
      call PVR_movie_anaglyph_visualize(istep_pvr, time, geofem, jacs,  &
     &                                  nod_fld, pvr, m_SR)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+1)
!
      end subroutine PVR_visualize
!
!  ---------------------------------------------------------------------
!
      end module volume_rendering
