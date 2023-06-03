!>@file   t_three_visualizers.f90
!!@brief  module t_three_visualizers
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module to access surfaceing, isosurfaceing,
!!      and volume rendering modules
!!
!!@verbatim
!!      subroutine init_three_visualize(viz_step, geofem, nod_fld,      &
!!     &                                VIZ_DAT, viz3_ctls, vizs, m_SR)
!!      subroutine visualize_three(viz_step, time_d, geofem,            &
!!     &                           nod_fld, VIZ_DAT, vizs, m_SR)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!!        type(vis3_controls), intent(inout) :: viz3_ctls
!!        type(three_visualize_modules), intent(inout) :: vizs
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module t_three_visualizers
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
      use t_control_data_viz3
      use t_cross_section
      use t_isosurface
      use t_map_projection
      use t_volume_rendering
      use t_fieldline
!
      implicit  none
!
      type three_visualize_modules
        type(sectioning_module) :: psf
        type(isosurface_module) :: iso
        type(map_rendering_module) :: maps
        type(volume_rendering_module) :: pvr
      end type three_visualize_modules
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_three_visualize(viz_step, geofem, nod_fld,        &
     &                                VIZ_DAT, viz3_ctls, vizs, m_SR)
!
      use volume_rendering
      use map_projection
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!
      type(vis3_controls), intent(inout) :: viz3_ctls
      type(three_visualize_modules), intent(inout) :: vizs
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+1)
      call SECTIONING_initialize                                        &
     &   (viz_step%PSF_t%increment, geofem, VIZ_DAT%edge_comm, nod_fld, &
     &    viz3_ctls%psf_ctls, vizs%psf, m_SR%SR_sig, m_SR%SR_il)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+1)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+3)
      call ISOSURF_initialize                                           &
     &   (viz_step%ISO_t%increment, geofem, nod_fld,                    &
     &    viz3_ctls%iso_ctls, vizs%iso)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+3)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+5)
      call MAP_PROJECTION_initialize                                    &
     &   (viz_step%MAP_t%increment, geofem, VIZ_DAT%edge_comm, nod_fld, &
     &    viz3_ctls%map_ctls, vizs%maps, m_SR%SR_sig, m_SR%SR_il)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+5)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+7)
      call PVR_initialize(viz_step%PVR_t%increment,                     &
     &    geofem, nod_fld, viz3_ctls%pvr_ctls, vizs%pvr, m_SR)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+7)
!
      call calypso_mpi_barrier
      call dealloc_viz3_controls(viz3_ctls)
!
      end subroutine init_three_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_three(viz_step, time_d, geofem,              &
     &                           nod_fld, VIZ_DAT, vizs, m_SR)
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
      type(three_visualize_modules), intent(inout) :: vizs
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
      call calypso_mpi_barrier
!
      end subroutine visualize_three
!
!  ---------------------------------------------------------------------
!
      end module t_three_visualizers
