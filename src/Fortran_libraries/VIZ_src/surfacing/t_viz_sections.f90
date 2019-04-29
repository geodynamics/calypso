!t_viz_sections.f90
!      module t_viz_sections
!
!      Written by H. Matsui on Apr., 2012
!
!!      subroutine init_visualize_surface(femmesh, ele_mesh, nod_fld,   &
!!     &          psf_ctls, iso_ctls, viz_psfs)
!!      subroutine visualize_surface                                    &
!!     &        (viz_step, time_d, femmesh, ele_mesh, nod_fld, viz_psfs)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: femmesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(phys_data), intent(in) :: nod_fld
!!        type(section_controls), intent(inout) :: psf_ctls
!!        type(isosurf_controls), intent(inout) :: iso_ctls
!!        type(surfacing_modules), intent(inout) :: viz_psfs
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
      use t_phys_data
      use t_time_data
      use t_cross_section
      use t_isosurface
!
      implicit  none
!
!>      Structure of sectioning and isosurfaceing modules
      type surfacing_modules
        type(sectioning_module) :: psf
        type(isosurface_module) :: iso
      end type surfacing_modules
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize_surface(femmesh, ele_mesh, nod_fld,     &
     &          psf_ctls, iso_ctls, viz_psfs)
!
      use t_control_data_sections
!
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
!
      type(section_controls), intent(inout) :: psf_ctls
      type(isosurf_controls), intent(inout) :: iso_ctls
      type(surfacing_modules), intent(inout) :: viz_psfs
!
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+1)
      call SECTIONING_initialize                                        &
     &   (femmesh, ele_mesh, nod_fld, psf_ctls, viz_psfs%psf)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+1)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+2)
      call ISOSURF_initialize                                           &
     &   (femmesh, ele_mesh, nod_fld, iso_ctls, viz_psfs%iso)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+2)
!
      end subroutine init_visualize_surface
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_surface                                      &
     &        (viz_step, time_d, femmesh, ele_mesh, nod_fld, viz_psfs)
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
!
      type(surfacing_modules), intent(inout) :: viz_psfs
!
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+6)
      call SECTIONING_visualize(viz_step%PSF_t%istep_file, time_d,      &
     &    ele_mesh, nod_fld, viz_psfs%psf)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+6)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+7)
      call ISOSURF_visualize(viz_step%ISO_t%istep_file, time_d,         &
     &    femmesh, ele_mesh, nod_fld, viz_psfs%iso)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+7)
!
      end subroutine visualize_surface
!
!  ---------------------------------------------------------------------
!
      end module t_viz_sections
