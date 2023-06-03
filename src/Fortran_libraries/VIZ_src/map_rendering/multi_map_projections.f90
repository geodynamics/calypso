!>@file   multi_map_projections.f90
!!@brief  module multi_map_projections
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief loop for map projections
!!
!!@verbatim
!!      subroutine init_multi_map_projections(num_map, view_param,      &
!!     &          psf_mesh, psf_dat, map_data, map_rgb, SR_sig)
!!        integer(kind= kint), intent(in) :: num_map
!!        type(psf_local_data), intent(in) :: psf_mesh(num_map)
!!        type(pvr_view_parameter), intent(in):: view_param(num_map)
!!        type(psf_results), intent(inout) :: psf_dat(num_map)
!!        type(map_rendering_data), intent(inout) :: map_data(num_map)
!!        type(pvr_image_type), intent(inout) :: map_rgb(num_map)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!      subroutine s_multi_map_projections                              &
!!     &         (num_map, istep_psf, time_d, psf_mesh, color_param,    &
!!     &          cbar_param, psf_dat, map_data, map_rgb, SR_sig)
!!        integer(kind= kint), intent(in) :: num_map
!!        integer(kind= kint), intent(in) ::  istep_psf
!!        type(time_data), intent(in) :: time_d
!!        type(psf_local_data), intent(in) :: psf_mesh(num_map)
!!        type(pvr_colormap_parameter), intent(in)                      &
!!     &                                :: color_param(num_map)
!!        type(pvr_colorbar_parameter), intent(in) :: cbar_param(num_map)
!!        type(psf_results), intent(inout) :: psf_dat(num_map)
!!        type(map_rendering_data), intent(inout) :: map_data(num_map)
!!        type(pvr_image_type), intent(inout) :: map_rgb(num_map)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
      module multi_map_projections
!
      use calypso_mpi
      use m_precision
!
      use t_geometry_data
      use t_phys_data
      use t_solver_SR
      use t_control_params_4_pvr
      use t_pvr_colormap_parameter
      use t_map_rendering_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_multi_map_projections(num_map, view_param,        &
     &          psf_mesh, psf_dat, map_data, map_rgb, SR_sig)
!
      use m_elapsed_labels_4_VIZ
      use t_psf_patch_data
      use t_psf_results
      use t_pvr_image_array
!
      use collect_psf_mesh_field
!
      integer(kind= kint), intent(in) :: num_map
      type(psf_local_data), intent(in) :: psf_mesh(num_map)
      type(pvr_view_parameter), intent(in):: view_param(num_map)
!
      type(psf_results), intent(inout) :: psf_dat(num_map)
      type(map_rendering_data), intent(inout) :: map_data(num_map)
      type(pvr_image_type), intent(inout) :: map_rgb(num_map)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind= kint) :: i_map
!
!
      do i_map = 1, num_map
        map_rgb(i_map)%irank_image_file = mod(i_map,nprocs)
        call alloc_pvr_image_array(view_param(i_map)%n_pvr_pixel,       &
     &                             map_rgb(i_map))
        call init_map_rendering_data(view_param(i_map),                 &
     &                               map_rgb(i_map), map_data(i_map))
        call alloc_scalar_on_map(map_rgb(i_map)%num_pixel_xy,           &
     &                           map_data(i_map))
      end do
!
      if(iflag_MAP_time) call start_elapsed_time(ist_elapsed_MAP+1)
      do i_map = 1, num_map
        call init_merge_psf_mesh                                        &
     &     (map_rgb(i_map)%irank_image_file, psf_mesh(i_map),           &
     &      psf_dat(i_map)%psf_nod, psf_dat(i_map)%psf_ele,             &
     &      psf_dat(i_map)%psf_phys, SR_sig)
      end do
      if(iflag_MAP_time) call end_elapsed_time(ist_elapsed_MAP+1)
!
      end subroutine init_multi_map_projections
!
!  ---------------------------------------------------------------------
!
      subroutine s_multi_map_projections                                &
     &         (num_map, istep_psf, time_d, psf_mesh, color_param,      &
     &          cbar_param, psf_dat, map_data, map_rgb, SR_sig)
!
      use m_elapsed_labels_4_VIZ
      use t_psf_patch_data
      use t_psf_results
      use t_pvr_image_array
!
      use collect_psf_mesh_field
      use write_PVR_image
!
      integer(kind= kint), intent(in) :: num_map
      integer(kind= kint), intent(in) ::  istep_psf
      type(time_data), intent(in) :: time_d
      type(psf_local_data), intent(in) :: psf_mesh(num_map)
      type(pvr_colormap_parameter), intent(in)                          &
     &                                :: color_param(num_map)
      type(pvr_colorbar_parameter), intent(in) :: cbar_param(num_map)
!
      type(psf_results), intent(inout) :: psf_dat(num_map)
      type(map_rendering_data), intent(inout) :: map_data(num_map)
      type(pvr_image_type), intent(inout) :: map_rgb(num_map)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind= kint) :: i_map
!
!
      if(iflag_MAP_time) call start_elapsed_time(ist_elapsed_MAP+1)
      do i_map = 1, num_map
        call collect_psf_scalar(map_rgb(i_map)%irank_image_file, ione,  &
     &      psf_mesh(i_map)%node, psf_mesh(i_map)%field,                &
     &      psf_dat(i_map)%psf_phys%d_fld(1,1), SR_sig)
      end do
      if(iflag_MAP_time) call end_elapsed_time(ist_elapsed_MAP+1)
!
      if(iflag_MAP_time) call start_elapsed_time(ist_elapsed_MAP+2)
      do i_map = 1, num_map
        call cal_map_rendering_data                                     &
     &     (time_d, psf_dat(i_map)%psf_nod, psf_dat(i_map)%psf_ele,     &
     &      psf_dat(i_map)%psf_phys, color_param(i_map),                &
     &      cbar_param(i_map), map_data(i_map), map_rgb(i_map))
      end do
      if(iflag_MAP_time) call end_elapsed_time(ist_elapsed_MAP+2)
!
      if(iflag_MAP_time) call start_elapsed_time(ist_elapsed_MAP+3)
      do i_map = 1, num_map
        call sel_write_pvr_image_file(istep_psf, -1, map_rgb(i_map))
      end do
      if(iflag_MAP_time) call end_elapsed_time(ist_elapsed_MAP+3)
!
      end subroutine s_multi_map_projections
!
!  ---------------------------------------------------------------------
!
      end module multi_map_projections
