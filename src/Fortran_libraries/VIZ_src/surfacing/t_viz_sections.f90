!>@file   t_viz_sections.f90
!!@brief  module t_viz_sections
!!
!!@auther   Hiroaki Matsui
!!@date  Programmed by H.Matsui in Apr., 2012
!
!>@brief Top routine for sectiong
!!
!!@verbatim
!!      subroutine init_visualize_surface(viz_step, geofem, edge_comm,  &
!!     &          nod_fld, surfacing_ctls, viz_psfs)
!!      subroutine visualize_surface                                    &
!!     &        (viz_step, time_d, geofem, edge_comm, nod_fld, viz_psfs)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: edge_comm
!!        type(phys_data), intent(in) :: nod_fld
!!        type(surfacing_controls), intent(inout) :: surfacing_ctls
!!        type(surfacing_modules), intent(inout) :: viz_psfs
!!@endverbatim
!
      module t_viz_sections
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
      use t_time_data
      use t_cross_section
      use t_isosurface
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
      subroutine init_visualize_surface(viz_step, geofem, edge_comm,    &
     &          nod_fld, surfacing_ctls, viz_psfs)
!
      use t_control_data_surfacings
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: edge_comm
      type(phys_data), intent(in) :: nod_fld
!
      type(surfacing_controls), intent(inout) :: surfacing_ctls
      type(surfacing_modules), intent(inout) :: viz_psfs
!
!
      if(viz_step%PSF_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+1)
        call SECTIONING_initialize(geofem, edge_comm, nod_fld,          &
     &      surfacing_ctls%psf_s_ctls, viz_psfs%psf)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+1)
      end if
!
      if(viz_step%ISO_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+2)
        call ISOSURF_initialize                                         &
     &     (geofem, nod_fld, surfacing_ctls%iso_s_ctls, viz_psfs%iso)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+2)
      end if
!
      call dealloc_surfacing_controls(surfacing_ctls)
!
      end subroutine init_visualize_surface
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_surface                                      &
     &        (viz_step, time_d, geofem, edge_comm, nod_fld, viz_psfs)
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: edge_comm
      type(phys_data), intent(in) :: nod_fld
!
      type(surfacing_modules), intent(inout) :: viz_psfs
!
!
      if(viz_step%PSF_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+6)
        call SECTIONING_visualize                                       &
     &     (viz_step%istep_psf, time_d, geofem, nod_fld, viz_psfs%psf)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+6)
      end if
!
      if(viz_step%ISO_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+7)
        call ISOSURF_visualize(viz_step%istep_iso, time_d,              &
     &                         geofem, edge_comm, nod_fld, viz_psfs%iso)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+7)
      end if
!
      end subroutine visualize_surface
!
!  ---------------------------------------------------------------------
!
      end module t_viz_sections
