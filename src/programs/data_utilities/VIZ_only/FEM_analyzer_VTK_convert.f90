!>@file   FEM_analyzer_VTK_convert.f90
!!@brief  module FEM_analyzer_VTK_convert
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief FEM top routines for surfacing
!!
!!@verbatim
!!      subroutine FEM_initialize_VTK_convert(init_d, sfcing)
!!        type(time_data), intent(in) :: init_d
!!        type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!!      subroutine FEM_analyze_VTK_convert                              &
!!     &         (i_step, time_d, ucd_step, sfcing)
!!        integer (kind =kint), intent(in) :: i_step
!!        type(time_data), intent(inout) :: time_d
!!        type(IO_step_param), intent(inout) :: ucd_step
!!        type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!!@endverbatim
!
      module FEM_analyzer_VTK_convert
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_surfacing
      use t_time_data
      use t_VIZ_step_parameter
      use t_IO_step_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_VTK_convert(init_d, sfcing)
!
      use load_mesh_and_field_4_viz
!
      type(time_data), intent(in) :: init_d
      type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ(sfcing%mesh_file_IO, sfcing%ucd_file_IO,    &
     &    init_d, sfcing%geofem, sfcing%ucd_time, sfcing%ucd_in,        &
     &    sfcing%nod_fld)
      call deallocate_surface_geom_type(sfcing%geofem%mesh%surf)
!
      end subroutine FEM_initialize_VTK_convert
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_VTK_convert                                &
     &         (i_step, time_d, ucd_step, sfcing)
!
      use load_mesh_and_field_4_viz
!
      integer (kind =kint), intent(in) :: i_step
      type(time_data), intent(inout) :: time_d
      type(IO_step_param), intent(inout) :: ucd_step
      type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!
!
      call istep_file_w_fix_dt(i_step, ucd_step)
      call set_field_data_4_VIZ                                         &
     &   (ucd_step%istep_file, i_step, sfcing%ucd_file_IO,              &
     &    sfcing%geofem, sfcing%ucd_time, sfcing%ucd_in, time_d,        &
     &    sfcing%nod_fld)
!
      end subroutine FEM_analyze_VTK_convert
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_VTK_convert
