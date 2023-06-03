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
!!      subroutine init_four_visualize(viz_step, geofem, nod_fld,       &
!!     &                               VIZ_DAT, viz_ctls, vizs, m_SR)
!!      subroutine visualize_four(viz_step, time_d, geofem,             &
!!     &                          nod_fld, VIZ_DAT, vizs, m_SR)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!!        type(vis4_controls), intent(inout) :: viz_ctls
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
      use m_elapsed_labels_4_VIZ
      use calypso_mpi
!
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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_four_visualize(viz_step, geofem, nod_fld,         &
     &                               VIZ_DAT, viz_ctls, vizs, m_SR)
!
      use volume_rendering
      use map_projection
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!
      type(vis4_controls), intent(inout) :: viz_ctls
      type(four_visualize_modules), intent(inout) :: vizs
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+1)
      call SECTIONING_initialize                                        &
     &   (viz_step%PSF_t%increment, geofem, VIZ_DAT%edge_comm, nod_fld, &
     &    viz_ctls%psf_ctls, vizs%psf, m_SR%SR_sig, m_SR%SR_il)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+1)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+3)
      call ISOSURF_initialize                                           &
     &   (viz_step%ISO_t%increment, geofem, nod_fld,                    &
     &    viz_ctls%iso_ctls, vizs%iso)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+3)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+5)
      call MAP_PROJECTION_initialize                                    &
     &   (viz_step%MAP_t%increment, geofem, VIZ_DAT%edge_comm, nod_fld, &
     &    viz_ctls%map_ctls, vizs%maps, m_SR%SR_sig, m_SR%SR_il)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+5)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+7)
      call PVR_initialize(viz_step%PVR_t%increment,                     &
     &    geofem, nod_fld, viz_ctls%pvr_ctls, vizs%pvr, m_SR)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+7)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+11)
      call FLINE_initialize(viz_step%FLINE_t%increment,                 &
     &    geofem, nod_fld, viz_ctls%fline_ctls, vizs%fline)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+11)
!
      call calypso_mpi_barrier
      call dealloc_viz4_controls(viz_ctls)
!
      end subroutine init_four_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_four(viz_step, time_d, geofem,               &
     &                          nod_fld, VIZ_DAT, vizs, m_SR)
!
      use volume_rendering
      use map_projection
!
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
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+2)
      call SECTIONING_visualize                                         &
     &   (viz_step%istep_psf, time_d, geofem, nod_fld, vizs%psf)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+2)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+4)
      call ISOSURF_visualize(viz_step%istep_iso, time_d,                &
     &    geofem, VIZ_DAT%edge_comm, nod_fld, vizs%iso,                 &
     &    m_SR%SR_sig, m_SR%SR_il)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+4)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+6)
      call MAP_PROJECTION_visualize                                     &
     &   (viz_step%istep_map, time_d, geofem, nod_fld, vizs%maps,       &
     &    m_SR%SR_sig)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+6)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+8)
      call PVR_visualize(viz_step%istep_pvr, time_d%time,               &
     &    geofem, VIZ_DAT%jacobians, nod_fld, vizs%pvr, m_SR)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+8)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+12)
      call FLINE_visualize(viz_step%istep_fline, geofem,                &
     &    VIZ_DAT%next_tbl, nod_fld, vizs%fline)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+12)
!
      call calypso_mpi_barrier
!
      end subroutine visualize_four
!
!  ---------------------------------------------------------------------
!
      end module t_four_visualizers
