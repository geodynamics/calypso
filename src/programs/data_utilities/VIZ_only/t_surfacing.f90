!>@file   t_surfacing.f90
!!@brief  module t_surfacing
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for visualizers
!!
!!@verbatim
!!      subroutine set_control_params_4_viz                             &
!!     &         (tctl, plt, mesh_file, ucd_param, ierr)
!!        type(time_data_control), intent(in) :: tctl
!!        type(platform_data_control), intent(in) :: plt
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(field_IO_params), intent(inout) :: ucd_param
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!@endverbatim
!
      module t_surfacing
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_ucd_data
      use t_file_IO_parameter
!
      implicit none
!
!
!>      Structure of mesh and field for sectioning only
      type FEM_mesh_field_4_surfacing
!>        Structure for mesh file IO paramters
        type(field_IO_params) :: mesh_file_IO
!>        Structure for field file IO paramters
        type(field_IO_params) :: ucd_file_IO
!>        Structure for VTK file output paramters
        type(field_IO_params) :: vtk_file_IO
!
!>       Structure for mesh data
!>        (position, connectivity, group, and communication)
        type(mesh_data) :: geofem
!>         Structure for nodal field data
        type(phys_data) :: nod_fld
!
!>          time data from data input
        type(time_data) :: ucd_time
!>          FEM field data IO
        type(ucd_data) :: ucd_in
      end type FEM_mesh_field_4_surfacing
!
      end module t_surfacing
