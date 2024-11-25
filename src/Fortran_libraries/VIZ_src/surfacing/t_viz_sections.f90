!>@file   t_viz_sections.f90
!!@brief  module t_viz_sections
!!
!!@auther   Hiroaki Matsui
!!@date  Programmed by H.Matsui in Apr., 2012
!
!>@brief Top routine for sectiong
!!
!!@verbatim
!!      subroutine init_visualize_surface                               &
!!     &         (elps_SECT, viz_step, geofem, edge_comm,               &
!!     &          nod_fld, surfacing_ctls, viz_psfs, m_SR)
!!      subroutine visualize_surface(elps_SECT, viz_step, time_d,       &
!!     &          geofem, edge_comm, nod_fld, viz_psfs, m_SR)
!!        type(elapsed_labels_4_SECTIONS), intent(in) :: elps_SECT
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: edge_comm
!!        type(phys_data), intent(in) :: nod_fld
!!        type(surfacing_controls), intent(inout) :: surfacing_ctls
!!        type(surfacing_modules), intent(inout) :: viz_psfs
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module t_viz_sections
!
      use m_precision
!
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_VIZ_step_parameter
      use t_time_data
      use t_mesh_data
      use t_comm_table
      use t_phys_data
      use t_time_data
      use t_cross_section
      use t_isosurface
      use t_mesh_SR
      use t_elapsed_labels_4_SECTIONS
!
      implicit  none
!
!>      Structure of sectioning and isosurfaceing modules
      type surfacing_modules
!>        Structure of cross sections
        type(sectioning_module) :: psf
!>        Structure of isosurfaces
        type(isosurface_module) :: iso
      end type surfacing_modules
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize_surface                                 &
     &         (elps_SECT, viz_step, geofem, edge_comm,                 &
     &          nod_fld, surfacing_ctls, viz_psfs, m_SR)
!
      use t_control_data_surfacings
!
      type(elapsed_labels_4_SECTIONS), intent(in) :: elps_SECT
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: edge_comm
      type(phys_data), intent(in) :: nod_fld
!
      type(surfacing_controls), intent(inout) :: surfacing_ctls
      type(surfacing_modules), intent(inout) :: viz_psfs
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(elps_SECT%flag_elapsed_S)                                      &
     &           call start_elapsed_time(elps_SECT%ist_elapsed_S+1)
      call SECTIONING_initialize                                        &
     &   (viz_step%PSF_t%increment, elps_SECT%elps_PSF,                 &
     &    geofem, edge_comm, nod_fld, surfacing_ctls%psf_s_ctls,        &
     &    viz_psfs%psf, m_SR%SR_sig, m_SR%SR_il)
      if(elps_SECT%flag_elapsed_S)                                      &
     &           call end_elapsed_time(elps_SECT%ist_elapsed_S+1)
!
      if(elps_SECT%flag_elapsed_S)                                      &
     &           call start_elapsed_time(elps_SECT%ist_elapsed_S+3)
      call ISOSURF_initialize                                           &
     &    (viz_step%ISO_t%increment, geofem, nod_fld,                   &
     &     surfacing_ctls%iso_s_ctls, viz_psfs%iso)
      if(elps_SECT%flag_elapsed_S)                                      &
     &           call end_elapsed_time(elps_SECT%ist_elapsed_S+3)
!
      call dealloc_surfacing_controls(surfacing_ctls)
!
      end subroutine init_visualize_surface
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_surface(elps_SECT, viz_step, time_d,         &
     &          geofem, edge_comm, nod_fld, viz_psfs, m_SR)
!
      type(elapsed_labels_4_SECTIONS), intent(in) :: elps_SECT
      type(VIZ_step_params), intent(in) :: viz_step
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: edge_comm
      type(phys_data), intent(in) :: nod_fld
!
      type(surfacing_modules), intent(inout) :: viz_psfs
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(elps_SECT%flag_elapsed_S)                                      &
     &           call start_elapsed_time(elps_SECT%ist_elapsed_S+2)
      call SECTIONING_visualize(viz_step%istep_psf, elps_SECT%elps_PSF, &
     &                          time_d, geofem, nod_fld, viz_psfs%psf)
      if(elps_SECT%flag_elapsed_S)                                      &
     &           call end_elapsed_time(elps_SECT%ist_elapsed_S+2)
!
      if(elps_SECT%flag_elapsed_S)                                      &
     &           call start_elapsed_time(elps_SECT%ist_elapsed_S+4)
      call ISOSURF_visualize(viz_step%istep_iso,  elps_SECT%elps_ISO,   &
     &    time_d, geofem, edge_comm, nod_fld, viz_psfs%iso,             &
     &    m_SR%SR_sig, m_SR%SR_il)
      if(elps_SECT%flag_elapsed_S)                                      &
     &           call end_elapsed_time(elps_SECT%ist_elapsed_S+4)
!
      end subroutine visualize_surface
!
!  ---------------------------------------------------------------------
!
      end module t_viz_sections
