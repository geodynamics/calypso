!>@file   multi_volume_renderings.f90
!!@brief  module multi_volume_renderings
!!
!!@date  Programmed by H.Matsui in May. 2006
!!       Modified by H.Matsui in May, 2021
!
!>@brief Main routines for volume renderings
!!
!!@verbatim
!!      subroutine set_PVR_view_and_images(num_pvr, num_pvr_images,     &
!!     &          elps_PVR, mesh, PVR_sort, pvr_rgb, pvr_param,         &
!!     &          pvr_bound, pvr_proj, m_SR)
!!        integer(kind = kint), intent(in) :: num_pvr, num_pvr_images
!!        type(elapsed_lables), intent(in) :: elps_PVR
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(sort_PVRs_by_type), intent(in) :: PVR_sort
!!        type(pvr_image_type), intent(in) :: pvr_rgb(num_pvr_images)
!!        type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound(num_pvr)
!!        type(PVR_projection_data), intent(inout)                      &
!!     &                            :: pvr_proj(num_pvr_images)
!!        type(mesh_SR), intent(inout) :: m_SR
!!
!!      subroutine PVR_fixview_rendering(istep_pvr, time, elps_PVR,     &
!!     &                                 geofem, jacs, nod_fld,         &
!!     &                                 tracer, fline, pvr, m_SR)
!!      subroutine PVR_movie_visualize(istep_pvr, time, elps_PVR,       &
!!     &                               geofem, jacs, nod_fld,           &
!!     &                               tracer, fline, pvr, m_SR)
!!      subroutine PVR_quilt_movie_visualize                            &
!!     &         (istep_pvr, time, elps_PVR, geofem, jacs,              &
!!     &          nod_fld, tracer, fline, pvr, m_SR)
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        real(kind = kreal), intent(in) :: time
!!        type(elapsed_lables), intent(in) :: elps_PVR
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(tracer_module), intent(in) :: tracer
!!        type(fieldline_module), intent(in) :: fline
!!        type(jacobians_type), intent(in) :: jacs
!!        type(volume_rendering_module), intent(inout) :: pvr
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module multi_volume_renderings
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
      use t_phys_data
      use t_jacobians
      use t_particle_trace
      use t_fieldline
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
      subroutine set_PVR_view_and_images(num_pvr, num_pvr_images,       &
     &          elps_PVR, mesh, PVR_sort, pvr_rgb, pvr_param,           &
     &          pvr_bound, pvr_proj, m_SR)
!
      use set_PVR_view_and_image
!
      integer(kind = kint), intent(in) :: num_pvr, num_pvr_images
      type(elapsed_lables), intent(in) :: elps_PVR
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
      integer(kind = kint) :: i_pvr, ist_pvr, ied_pvr
      integer(kind = kint) :: ist_img, num_img
!
!
!       single image
      ist_pvr = PVR_sort%istack_PVR_modes(0) + 1
      ied_pvr = PVR_sort%istack_PVR_modes(1)
      do i_pvr = ist_pvr, ied_pvr
        ist_img = PVR_sort%istack_pvr_images(i_pvr-1)
        call single_PVR_view_matrices                                   &
     &     (elps_PVR, mesh, pvr_rgb(ist_img+1), pvr_param(i_pvr),       &
     &      pvr_bound(i_pvr), pvr_proj(ist_img+1), m_SR)
      end do
!
!       stereo rendering
      ist_pvr = PVR_sort%istack_PVR_modes(1) + 1
      ied_pvr = PVR_sort%istack_PVR_modes(2)
      do i_pvr = ist_pvr, ied_pvr
        ist_img = PVR_sort%istack_pvr_images(i_pvr-1)
        num_img = PVR_sort%istack_pvr_images(i_pvr  ) - ist_img
        call quilt_PVR_view_matrices(num_img, elps_PVR, mesh,           &
     &      pvr_rgb(ist_img+1), pvr_param(i_pvr),                       &
     &      pvr_bound(i_pvr), pvr_proj(ist_img+1), m_SR)
      end do
!
      end subroutine set_PVR_view_and_images
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine PVR_fixview_rendering(istep_pvr, time, elps_PVR,       &
     &                                 geofem, jacs, nod_fld,           &
     &                                 tracer, fline, pvr, m_SR)
!
      use cal_pvr_modelview_mat
      use each_volume_rendering
      use each_anaglyph_PVR
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(elapsed_lables), intent(in) :: elps_PVR
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
      type(jacobians_type), intent(in) :: jacs
!
      type(volume_rendering_module), intent(inout) :: pvr
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_pvr, ist_pvr, ied_pvr
      integer(kind = kint) :: ist_img, num_img
!
!
      if(elps_PVR%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_PVR%ist_elapsed+1)
      ist_pvr = pvr%PVR_sort%istack_PVR_modes(0) + 1
      ied_pvr = pvr%PVR_sort%istack_PVR_modes(2)
      do i_pvr = ist_pvr, ied_pvr
        ist_img = pvr%PVR_sort%istack_pvr_images(i_pvr-1)
        num_img = pvr%PVR_sort%istack_pvr_images(i_pvr  ) - ist_img
        if(pvr%pvr_param(i_pvr)%movie_def%iflag_movie_mode              &
     &                                 .ne. IFLAG_NO_MOVIE) cycle
!
        call each_PVR_rendering(istep_pvr, time, num_img, elps_PVR,     &
     &      geofem, jacs, nod_fld, tracer, fline, pvr%sf_grp_4_sf,      &
     &      pvr%field_pvr(i_pvr), pvr%pvr_param(i_pvr),                 &
     &      pvr%pvr_proj(ist_img+1), pvr%pvr_rgb(ist_img+1),            &
     &      m_SR%SR_sig, m_SR%SR_r)
      end do
      if(elps_PVR%flag_elapsed)                                         &
     &          call end_elapsed_time(elps_PVR%ist_elapsed+1)
!
      end subroutine PVR_fixview_rendering
!
!  ---------------------------------------------------------------------
!
      subroutine PVR_movie_visualize(istep_pvr, time, elps_PVR,         &
     &                               geofem, jacs, nod_fld,             &
     &                               tracer, fline, pvr, m_SR)
!
      use each_volume_rendering
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(elapsed_lables), intent(in) :: elps_PVR
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
      type(jacobians_type), intent(in) :: jacs
!
      type(volume_rendering_module), intent(inout) :: pvr
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_pvr, ist_pvr, ied_pvr, ist_img
!
!
      ist_pvr = pvr%PVR_sort%istack_PVR_modes(2) + 1
      ied_pvr = pvr%PVR_sort%istack_PVR_modes(3)
      do i_pvr = ist_pvr, ied_pvr
        ist_img = pvr%PVR_sort%istack_pvr_images(i_pvr-1)
        call each_PVR_rendering_w_rot(istep_pvr, time, elps_PVR,        &
     &      geofem, jacs, nod_fld, tracer, fline, pvr%sf_grp_4_sf,      &
     &      pvr%field_pvr(i_pvr), pvr%pvr_param(i_pvr),                 &
     &      pvr%pvr_bound(i_pvr), pvr%pvr_rgb(ist_img+1),               &
     &      pvr%pvr_proj(ist_img+1), m_SR%SR_sig, m_SR%SR_r, m_SR%SR_i)
      end do
!
      end subroutine PVR_movie_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine PVR_quilt_movie_visualize                              &
     &         (istep_pvr, time, elps_PVR, geofem, jacs,                &
     &          nod_fld, tracer, fline, pvr, m_SR)
!
      use each_volume_rendering
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(elapsed_lables), intent(in) :: elps_PVR
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
      type(jacobians_type), intent(in) :: jacs
!
      type(volume_rendering_module), intent(inout) :: pvr
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_pvr, ist_pvr, ied_pvr
      integer(kind = kint) :: ist_img, num_img
!
!
      ist_pvr = pvr%PVR_sort%istack_PVR_modes(3) + 1
      ied_pvr = pvr%PVR_sort%istack_PVR_modes(4)
      do i_pvr = ist_pvr, ied_pvr
        ist_img = pvr%PVR_sort%istack_pvr_images(i_pvr-1)
        num_img = pvr%PVR_sort%istack_pvr_images(i_pvr  ) - ist_img
        if(pvr%pvr_param(i_pvr)%movie_def%iflag_movie_mode              &
     &                                 .eq. IFLAG_NO_MOVIE) cycle
        if(pvr%pvr_param(i_pvr)%stereo_def%flag_quilt) then
!
          call each_PVR_quilt_rendering_w_rot                           &
     &       (istep_pvr, time, num_img, elps_PVR, geofem, jacs,         &
     &        nod_fld, tracer, fline, pvr%sf_grp_4_sf,                  &
     &        pvr%field_pvr(i_pvr), pvr%pvr_param(i_pvr),               &
     &        pvr%pvr_bound(i_pvr), pvr%pvr_proj(ist_img+1),            &
     &        pvr%pvr_rgb(ist_img+1), m_SR%SR_sig,                      &
     &        m_SR%SR_r, m_SR%SR_i)
        end if
      end do
!
      end subroutine PVR_quilt_movie_visualize
!
!  ---------------------------------------------------------------------
!
      end module multi_volume_renderings
