!>@file   t_four_visualizers.f90
!!@brief  module t_four_visualizers
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module to access surfaceing, isosurfaceing,
!!       fieldline, and volume rendering modules
!!
!!@verbatim
!!      subroutine init_four_visualize(elps_VIZ, viz_step,              &
!!     &          geofem, nod_fld, VIZ_DAT, viz4_ctls, vizs, m_SR)
!!      subroutine visualize_four(elps_VIZ, viz_step, time_d, geofem,   &
!!     &                          nod_fld, VIZ_DAT, vizs, m_SR)
!!        type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!!        type(vis4_controls), intent(inout) :: viz4_ctls
!!        type(four_visualize_modules), intent(inout) :: vizs
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module t_four_visualizers
!
      use m_precision
!
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_elapsed_labels_4_VIZ
      use t_VIZ_step_parameter
      use t_time_data
      use t_mesh_data
      use t_comm_table
      use t_phys_data
      use t_next_node_ele_4_node
      use t_VIZ_mesh_field
      use t_mesh_SR
!
      use t_control_data_viz4
      use t_cross_section
      use t_isosurface
      use t_map_projection
      use t_volume_rendering
      use t_fieldline
      use t_particle_trace
!
      implicit  none
!
      type four_visualize_modules
        type(sectioning_module) :: psf
        type(isosurface_module) :: iso
        type(map_rendering_module) :: maps
        type(volume_rendering_module) :: pvr
        type(fieldline_module) :: fline
      end type four_visualize_modules
!
      type(tracer_module), save, private :: dummy_tracer
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_four_visualize(elps_VIZ, viz_step,                &
     &          geofem, nod_fld, VIZ_DAT, viz4_ctls, vizs, m_SR)
!
      use volume_rendering
      use map_projection
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!
      type(vis4_controls), intent(inout) :: viz4_ctls
      type(four_visualize_modules), intent(inout) :: vizs
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+1)
      call SECTIONING_initialize                                        &
     &   (viz_step%PSF_t%increment, elps_VIZ%elps_PSF,                  &
     &    geofem, VIZ_DAT%edge_comm, nod_fld, viz4_ctls%psf_ctls,       &
     &    vizs%psf, m_SR%SR_sig, m_SR%SR_il)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+1)
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+3)
      call ISOSURF_initialize                                           &
     &   (viz_step%ISO_t%increment, geofem, nod_fld,                    &
     &    viz4_ctls%iso_ctls, vizs%iso)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+3)
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+5)
      call MAP_PROJECTION_initialize(viz_step%MAP_t%increment,          &
     &    elps_VIZ%elps_PSF, elps_VIZ%elps_MAP,                         &
     &    geofem, VIZ_DAT%edge_comm, nod_fld,                           &
     &    viz4_ctls%map_ctls, vizs%maps, m_SR%SR_sig, m_SR%SR_il)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+5)
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+11)
      call FLINE_initialize(viz_step%FLINE_t%increment, geofem,         &
     &    nod_fld, dummy_tracer, viz4_ctls%fline_ctls, vizs%fline)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+11)
!
      dummy_tracer%num_trace = 0
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+7)
      call PVR_initialize                                               &
     &   (viz_step%PVR_t%increment, elps_VIZ%elps_PVR, geofem, nod_fld, &
     &    dummy_tracer, vizs%fline, viz4_ctls%pvr_ctls, vizs%pvr, m_SR)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+7)
!
!
      call calypso_mpi_barrier
      call dealloc_viz4_controls(viz4_ctls)
!
      end subroutine init_four_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_four(elps_VIZ, viz_step, time_d, geofem,     &
     &                          nod_fld, VIZ_DAT, vizs, m_SR)
!
      use volume_rendering
      use map_projection
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(time_data), intent(in) :: time_d
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: geofem
      type(VIZ_mesh_field), intent(in) :: VIZ_DAT
      type(phys_data), intent(in) :: nod_fld
!
      type(four_visualize_modules), intent(inout) :: vizs
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+2)
      call SECTIONING_visualize(viz_step%istep_psf, elps_VIZ%elps_PSF,  &
     &                          time_d, geofem, nod_fld, vizs%psf)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+2)
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+4)
      call ISOSURF_visualize(viz_step%istep_iso, elps_VIZ%elps_ISO,     &
     &    time_d, geofem, VIZ_DAT%edge_comm, nod_fld, vizs%iso,         &
     &    m_SR%SR_sig, m_SR%SR_il)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+4)
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+6)
      call MAP_PROJECTION_visualize                                     &
     &   (viz_step%istep_map, elps_VIZ%elps_PSF, elps_VIZ%elps_MAP,     &
     &    time_d, geofem, nod_fld, vizs%maps, m_SR%SR_sig)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+6)
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+12)
      call FLINE_visualize                                              &
     &   (viz_step%istep_fline, elps_VIZ%elps_FLINE, time_d, geofem,    &
     &    VIZ_DAT%para_surf, nod_fld, dummy_tracer, vizs%fline, m_SR)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+12)
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+8)
      call PVR_visualize                                                &
     &   (viz_step%istep_pvr, time_d%time, elps_VIZ%elps_PVR,           &
     &    geofem, VIZ_DAT%jacobians, nod_fld, dummy_tracer,             &
     &    vizs%fline, vizs%pvr, m_SR)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+8)
!
      call calypso_mpi_barrier
!
      end subroutine visualize_four
!
!  ---------------------------------------------------------------------
!
      end module t_four_visualizers
