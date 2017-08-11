!sections_for_1st.f90
!      module sections_for_1st
!
!      Written by H. Matsui on Apr., 2012
!
!!      subroutine init_visualize_surface(femmesh, ele_mesh, nod_fld)
!!      subroutine visualize_surface                                    &
!!     &         (viz_step, time_d, femmesh, ele_mesh, nod_fld)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: femmesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(phys_data), intent(in) :: nod_fld
!
      module sections_for_1st
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
      use t_phys_data
      use t_time_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize_surface(femmesh, ele_mesh, nod_fld)
!
      use m_cross_section
      use m_isosurface
!
      use set_psf_case_table
!
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
!
      type(phys_data), intent(in) :: nod_fld
!
!
      call start_elapsed_time(60)
      if (iflag_debug.eq.1)  write(*,*) 'set_sectioning_case_table'
      call set_sectioning_case_table
!
      call SECTIONING_initialize                                        &
     &   (femmesh%mesh, femmesh%group, ele_mesh, nod_fld)
      call end_elapsed_time(60)
!
      call start_elapsed_time(61)
      call ISOSURF_initialize                                           &
     &   (femmesh%mesh, femmesh%group, ele_mesh, nod_fld)
      call end_elapsed_time(61)
!
      end subroutine init_visualize_surface
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_surface                                      &
     &         (viz_step, time_d, femmesh, ele_mesh, nod_fld)
!
      use m_cross_section
      use m_isosurface
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
!
!
      call start_elapsed_time(65)
      call SECTIONING_visualize                                         &
     &   (viz_step%PSF_t%istep_file, time_d, ele_mesh, nod_fld)
      call end_elapsed_time(65)
!
      call start_elapsed_time(66)
      call ISOSURF_visualize(viz_step%ISO_t%istep_file, time_d,         &
     &    femmesh%mesh, ele_mesh, nod_fld)
      call end_elapsed_time(66)
!
      end subroutine visualize_surface
!
!  ---------------------------------------------------------------------
!
      end module sections_for_1st
