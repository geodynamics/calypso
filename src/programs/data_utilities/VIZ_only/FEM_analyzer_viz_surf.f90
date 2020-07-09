!>@file   FEM_analyzer_viz_surf.f90
!!@brief  module FEM_analyzer_viz_surf
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief FEM top routines for surfacing
!!
!!@verbatim
!!      subroutine FEM_initialize_surface(ucd_step, init_d, sfcing)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(in) :: init_d
!!        type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!!      subroutine FEM_analyze_surface(i_step, ucd_step, time_d, sfcing)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(time_data), intent(inout) :: time_d
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!
      module FEM_analyzer_viz_surf
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
      subroutine FEM_initialize_surface(ucd_step, init_d, sfcing)
!
      use load_mesh_and_field_4_viz
!
      type(IO_step_param), intent(in) :: ucd_step
      type(time_data), intent(in) :: init_d
      type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ(init_d, ucd_step, sfcing%ucd_file_IO,       &
     &    sfcing%mesh_file_IO, sfcing%geofem, sfcing%ucd_time,          &
     &    sfcing%ucd_in, sfcing%nod_fld)
      call deallocate_surface_geom_type(sfcing%geofem%mesh%surf)
!
      end subroutine FEM_initialize_surface
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_surface(i_step, ucd_step, time_d, sfcing)
!
      use load_mesh_and_field_4_viz
!
      integer (kind =kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: ucd_step
      type(time_data), intent(inout) :: time_d
      type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!
!
      call set_field_data_4_VIZ                                         &
     &   (i_step, ucd_step, sfcing%ucd_file_IO, sfcing%geofem,          &
     &    sfcing%ucd_time, sfcing%ucd_in, time_d, sfcing%nod_fld)
!
      end subroutine FEM_analyze_surface
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_surf
