!>@file   t_SPH_MHD_zonal_mean_viz.f90
!!@brief  module t_SPH_MHD_zonal_mean_viz
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2012
!
!>@brief  Make zonal mean sections
!!
!!@verbatim
!!      subroutine init_zonal_mean_vizs(elps_VIZ, viz_step,             &
!!     &          geofem, edge_comm, nod_fld, zm_ctls, zmeans, m_SR)
!!        type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!!        type(sph_zonal_mean_viz), intent(inout) :: zmeans
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine SPH_MHD_zmean_vizs(elps_VIZ, viz_step, time_d,       &
!!     &          sph, geofem, WK, nod_fld, zmeans, m_SR)
!!        type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(sph_grids), intent(in) :: sph
!!        type(mesh_data), intent(in) :: geofem
!!        type(works_4_sph_trans_MHD), intent(in) :: WK
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(sph_zonal_mean_viz), intent(inout) :: zmeans
!!        type(mesh_SR), intent(inout) :: m_SR
!!
!!      subroutine SPH_MHD_zonal_mean_vizs(viz_step, time_d,            &
!!     &          sph, geofem, nod_fld, zm_psf, m_SR)
!!        type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(sph_grids), intent(in) :: sph
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(sectioning_module), intent(inout) :: zm_psf
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module t_SPH_MHD_zonal_mean_viz
!
      use m_precision
!
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_elapsed_labels_4_VIZ
      use t_time_data
      use t_comm_table
      use t_mesh_data
      use t_phys_data
      use t_map_projection
      use t_spheric_parameter
      use t_sph_trans_arrays_MHD
      use t_cross_section
      use t_mesh_SR
      use t_VIZ_step_parameter
!
      implicit  none
!
!>      Structures of zonal mean controls
      type sph_zonal_mean_viz
!>        Structures of zonal mean sectioning controls
        type(sectioning_module) :: zm_psf
!>        Structures of zonal RMS sectioning controls
        type(sectioning_module) :: zrms_psf
!
!>        Structures of zonal mean rendering controls
        type(map_rendering_module) :: zm_maps
!>        Structures of zonal RMS rendering controls
        type(map_rendering_module) :: zRMS_maps
      end type sph_zonal_mean_viz
!
      private :: SPH_MHD_zonal_RMS_vizs
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_zonal_mean_vizs(elps_VIZ, viz_step,               &
     &          geofem, edge_comm, nod_fld, zm_ctls, zmeans, m_SR)
!
      use t_control_data_dynamo_vizs
      use map_projection
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: edge_comm
      type(phys_data), intent(in) :: nod_fld
!
      type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
      type(sph_zonal_mean_viz), intent(inout) :: zmeans
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+1)
      call SECTIONING_initialize                                        &
     &   (viz_step%PSF_t%increment, elps_VIZ%elps_PSF,                  &
     &    geofem, edge_comm, nod_fld, zm_ctls%zm_psf_ctls,              &
     &    zmeans%zm_psf, m_SR%SR_sig, m_SR%SR_il)
      call SECTIONING_initialize                                        &
     &   (viz_step%PSF_t%increment, elps_VIZ%elps_PSF,                  &
     &    geofem, edge_comm, nod_fld, zm_ctls%zRMS_psf_ctls,            &
     &    zmeans%zrms_psf, m_SR%SR_sig, m_SR%SR_il)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+1)
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+5)
      call MAP_PROJECTION_initialize(viz_step%MAP_t%increment,          &
     &    elps_VIZ%elps_PSF, elps_VIZ%elps_MAP, geofem, edge_comm,      &
     &    nod_fld, zm_ctls%zm_map_ctls, zmeans%zm_maps,                 &
     &    m_SR%SR_sig, m_SR%SR_il)
      call MAP_PROJECTION_initialize(viz_step%MAP_t%increment,          &
     &    elps_VIZ%elps_PSF, elps_VIZ%elps_MAP, geofem, edge_comm,      &
     &     nod_fld, zm_ctls%zRMS_map_ctls, zmeans%zRMS_maps,            &
     &    m_SR%SR_sig, m_SR%SR_il)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+5)
!
      end subroutine init_zonal_mean_vizs
!
!  ---------------------------------------------------------------------
!
      subroutine SPH_MHD_zmean_vizs(elps_VIZ, viz_step, time_d,         &
     &          sph, geofem, WK, nod_fld, zmeans, m_SR)
!
      use FEM_analyzer_sph_MHD
      use nod_phys_send_recv
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(VIZ_step_params), intent(in) :: viz_step
      type(sph_grids), intent(in) :: sph
!
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
      type(works_4_sph_trans_MHD), intent(in) :: WK
!
      type(phys_data), intent(inout) :: nod_fld
      type(sph_zonal_mean_viz), intent(inout) :: zmeans
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call SPH_MHD_zonal_mean_vizs(elps_VIZ, viz_step, time_d,          &
     &    sph, geofem, nod_fld, zmeans%zm_psf, zmeans%zm_maps, m_SR)
      call SPH_MHD_zonal_RMS_vizs(elps_VIZ, viz_step, time_d,           &
     &    sph, geofem, WK, nod_fld, zmeans%zrms_psf, zmeans%zRMS_maps,  &
     &    m_SR)
!
      end subroutine SPH_MHD_zmean_vizs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SPH_MHD_zonal_mean_vizs(elps_VIZ, viz_step, time_d,    &
     &          sph, geofem, nod_fld, zm_psf, zm_maps, m_SR)
!
      use sph_rtp_zonal_rms_data
      use nod_phys_send_recv
      use map_projection
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(VIZ_step_params), intent(in) :: viz_step
      type(sph_grids), intent(in) :: sph
!
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
!
      type(phys_data), intent(inout) :: nod_fld
      type(sectioning_module), intent(inout) :: zm_psf
      type(map_rendering_module), intent(inout) :: zm_maps
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if((zm_psf%num_psf+zm_maps%num_map) .le. 0) return
!
      if (iflag_debug.gt.0) write(*,*) 'zonal_mean_all_rtp_field'
      call zonal_mean_all_rtp_field                                     &
     &   (sph%sph_rtp, geofem%mesh%node, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(geofem%mesh, nod_fld,                   &
     &                          m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
      if(zm_psf%num_psf .gt. 0) then
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+2)
        if (iflag_debug.gt.0) write(*,*) 'SECTIONING_visualize zmean'
        call SECTIONING_visualize                                       &
     &     (viz_step%istep_psf, elps_VIZ%elps_PSF,                      &
     &      time_d, geofem, nod_fld, zm_psf)
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+2)
      end if
!
      if(zm_maps%num_map .gt. 0) then
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+6)
        call MAP_PROJECTION_visualize                                   &
     &     (viz_step%istep_map, elps_VIZ%elps_PSF, elps_VIZ%elps_MAP,   &
     &      time_d,geofem, nod_fld, zm_maps, m_SR%SR_sig)
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+6)
      end if
!
      end subroutine SPH_MHD_zonal_mean_vizs
!
!  ---------------------------------------------------------------------
!
      subroutine SPH_MHD_zonal_RMS_vizs(elps_VIZ, viz_step, time_d,     &
     &          sph, geofem, WK, nod_fld, zrms_psf, zrms_maps, m_SR)
!
      use FEM_analyzer_sph_MHD
      use sph_rtp_zonal_rms_data
      use nod_phys_send_recv
      use map_projection
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(VIZ_step_params), intent(in) :: viz_step
      type(sph_grids), intent(in) :: sph
!
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
      type(works_4_sph_trans_MHD), intent(in) :: WK
!
      type(phys_data), intent(inout) :: nod_fld
      type(sectioning_module), intent(inout) :: zrms_psf
      type(map_rendering_module), intent(inout) :: zrms_maps
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if((zrms_psf%num_psf+zrms_maps%num_map) .le. 0) return
!
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_MHD'
      call SPH_to_FEM_bridge_MHD(sph, WK, geofem, nod_fld)
      call zonal_rms_all_rtp_field                                      &
     &   (sph%sph_rtp, geofem%mesh%node, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(geofem%mesh, nod_fld,                   &
     &                          m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
      if(zrms_psf%num_psf .gt. 0) then
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+2)
        if (iflag_debug.gt.0) write(*,*) 'SECTIONING_visualize RMS'
        call SECTIONING_visualize                                       &
     &     (viz_step%istep_psf, elps_VIZ%elps_PSF,                      &
     &      time_d, geofem, nod_fld, zrms_psf)
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+2)
      end if
!
      if(zrms_maps%num_map .gt. 0) then
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+6)
        call MAP_PROJECTION_visualize                                   &
     &     (viz_step%istep_map, elps_VIZ%elps_PSF, elps_VIZ%elps_MAP,   &
     &      time_d, geofem, nod_fld, zRMS_maps, m_SR%SR_sig)
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+6)
      end if
!
      end subroutine SPH_MHD_zonal_RMS_vizs
!
!  ---------------------------------------------------------------------
!
      end module t_SPH_MHD_zonal_mean_viz
