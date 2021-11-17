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
      use t_control_data_pvr_sections
      use set_pvr_control
      use rendering_and_image_nums
      use each_volume_rendering
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
      call set_from_PVR_control(geofem, nod_fld, pvr_ctls, pvr)
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+6)
      call count_num_rendering_and_images(pvr%num_pvr, pvr%pvr_param,   &
     &    pvr%num_pvr_rendering, pvr%num_pvr_images,                    &
     &    pvr%istack_pvr_images)
      call alloc_pvr_images(pvr)
!
      call set_rendering_and_image_pes                                  &
     &   (nprocs, pvr%num_pvr, pvr%pvr_param, pvr_ctls%pvr_ctl_type,    &
     &    pvr%num_pvr_images, pvr%istack_pvr_images, pvr%pvr_rgb)
!
      do i_pvr = 1, pvr_ctls%num_pvr_ctl
        if(pvr_ctls%fname_pvr_ctl(i_pvr) .ne. 'NO_FILE'                 &
     &      .or. my_rank .ne. 0) then
          call deallocate_cont_dat_pvr(pvr_ctls%pvr_ctl_type(i_pvr))
        end if
      end do
!
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+7)
      call init_sf_grp_list_each_surf                                   &
     &   (geofem%mesh%surf, geofem%group%surf_grp, pvr%sf_grp_4_sf)
!
      do i_pvr = 1, pvr%num_pvr
        ist_img = pvr%istack_pvr_images(i_pvr-1)
        num_img = pvr%istack_pvr_images(i_pvr  ) - ist_img
        call init_each_PVR_image(num_img, pvr%pvr_param(i_pvr),         &
     &                           pvr%pvr_rgb(ist_img+1))
        call each_PVR_initialize                                        &
     &     (i_pvr, num_img, geofem%mesh, geofem%group,                  &
     &      pvr%pvr_rgb(ist_img+1), pvr%pvr_param(i_pvr),               &
     &      pvr%pvr_bound(i_pvr), pvr%pvr_proj(ist_img+1),              &
     &      m_SR%SR_sig, m_SR%SR_r, m_SR%SR_i)
      end do
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
      use write_PVR_image
      use each_volume_rendering
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
      integer(kind = kint) :: i_pvr
      integer(kind = kint) :: i_img, ist_img, ied_img, num_img
!
!
      if(pvr%num_pvr.le.0 .or. istep_pvr.le.0) return
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+1)
      do i_pvr = 1, pvr%num_pvr
        if(pvr%pvr_param(i_pvr)%movie_def%iflag_movie_mode              &
     &                                 .ne. IFLAG_NO_MOVIE) cycle
!
        ist_img = pvr%istack_pvr_images(i_pvr-1)
        num_img = pvr%istack_pvr_images(i_pvr  ) - ist_img
        call each_PVR_rendering(istep_pvr, time, num_img,               &
     &      geofem, jacs, nod_fld, pvr%sf_grp_4_sf,                     &
     &      pvr%field_pvr(i_pvr), pvr%pvr_param(i_pvr),                 &
     &      pvr%pvr_proj(ist_img+1), pvr%pvr_rgb(ist_img+1),            &
     &      m_SR%SR_sig, m_SR%SR_r)
      end do
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+1)
!
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+2)
      do i_pvr = 1, pvr%num_pvr
        ist_img = pvr%istack_pvr_images(i_pvr-1) + 1
        if(pvr%pvr_param(i_pvr)%movie_def%iflag_movie_mode              &
     &                                 .ne. IFLAG_NO_MOVIE) cycle
        if(pvr%pvr_param(i_pvr)%stereo_def%flag_quilt) cycle
!
        ied_img = pvr%istack_pvr_images(i_pvr  )
        do i_img = ist_img, ied_img
          call sel_write_pvr_image_file(istep_pvr, pvr%pvr_rgb(i_img))
        end do
      end do
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+2)
      do i_pvr = 1, pvr%num_pvr
        ist_img = pvr%istack_pvr_images(i_pvr-1)
        num_img = pvr%istack_pvr_images(i_pvr  ) - ist_img
        if(pvr%pvr_param(i_pvr)%movie_def%iflag_movie_mode              &
     &                                 .ne. IFLAG_NO_MOVIE) cycle
        if(pvr%pvr_param(i_pvr)%stereo_def%flag_quilt) then
          call set_output_rot_sequence_image(istep_pvr,                 &
     &        pvr%pvr_rgb(ist_img+1)%id_pvr_file_type,                  &
     &        pvr%pvr_rgb(ist_img+1)%pvr_prefix, num_img,               &
     &        pvr%pvr_param(i_pvr)%stereo_def%n_column_row_view,        &
     &        pvr%pvr_rgb(ist_img+1))
        end if
      end do
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+2)
!
!      generate snapshot movie images
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+1)
      do i_pvr = 1, pvr%num_pvr
        if(pvr%pvr_param(i_pvr)%movie_def%iflag_movie_mode              &
     &                                 .eq. IFLAG_NO_MOVIE) cycle
!
        ist_img = pvr%istack_pvr_images(i_pvr-1)
        num_img = pvr%istack_pvr_images(i_pvr  ) - ist_img
        call each_PVR_rendering_w_rot(istep_pvr, time, num_img,         &
     &      geofem, jacs, nod_fld, pvr%sf_grp_4_sf,                     &
     &      pvr%field_pvr(i_pvr), pvr%pvr_param(i_pvr),                 &
     &      pvr%pvr_bound(i_pvr), pvr%pvr_proj(ist_img+1),              &
     &      pvr%pvr_rgb(ist_img+1), m_SR%SR_sig, m_SR%SR_r, m_SR%SR_i)
      end do
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+1)
!
      end subroutine PVR_visualize
!
!  ---------------------------------------------------------------------
!
      end module volume_rendering
