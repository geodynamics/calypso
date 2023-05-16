!>@file   FEM_analyzer_viz_surf.f90
!!@brief  module FEM_analyzer_viz_surf
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief FEM top routines for surfacing
!!
!!@verbatim
!!      subroutine FEM_initialize_surface                               &
!!     &         (ucd_step, init_d, FEM_viz, edge_comm, m_SR)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(in) :: init_d
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine FEM_analyze_surface(i_step, ucd_step, time_d,        &
!!     &                               FEM_viz, m_SR)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(inout) :: time_d
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(mesh_SR), intent(inout) :: m_SR
!!
!!      subroutine FEM_initialize_VTK_convert(ucd_step, init_d,         &
!!     &                                      FEM_viz, m_SR)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(in) :: init_d
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(mesh_SR), intent(inout) :: m_SR
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
      use t_time_data
      use t_field_list_for_vizs
      use t_VIZ_step_parameter
      use t_IO_step_parameter
      use t_FEM_mesh_field_4_viz
      use t_mesh_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_surface                                 &
     &         (ucd_step, init_d, FEM_viz, edge_comm, m_SR)
!
      use t_field_list_for_vizs
!
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use set_parallel_file_name
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use const_element_comm_tables
!
      type(IO_step_param), intent(in) :: ucd_step
      type(time_data), intent(in) :: init_d
!
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(communication_table), intent(inout) :: edge_comm
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: istep_ucd
!
!   --------------------------------
!       setup mesh and field information
!   --------------------------------
!
      call mpi_input_mesh(FEM_viz%mesh_file_IO, nprocs, FEM_viz%geofem)
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_comm_initialization(FEM_viz%geofem%mesh, m_SR)
      call FEM_mesh_initialization                                      &
     &   (FEM_viz%geofem%mesh, FEM_viz%geofem%group,                    &
     &    m_SR%SR_sig, m_SR%SR_i)
!
      if(iflag_debug .gt. 0) write(*,*) 'const_edge_comm_table'
      call const_edge_comm_table                                        &
     &   (FEM_viz%geofem%mesh%node, FEM_viz%geofem%mesh%nod_comm,       &
     &    edge_comm, FEM_viz%geofem%mesh%edge, m_SR)
!
!     ---------------------
!
      FEM_viz%ucd_in%nnod = FEM_viz%geofem%mesh%node%numnod
      istep_ucd = IO_step_exc_zero_inc(init_d%i_time_step, ucd_step)
      call sel_read_parallel_udt_param(istep_ucd,                       &
     &    FEM_viz%ucd_file_IO, FEM_viz%ucd_time, FEM_viz%ucd_in)
      call alloc_phys_name_type_by_output(FEM_viz%ucd_in,               &
     &                                    FEM_viz%field)
!
      call add_field_in_viz_controls(FEM_viz%viz_fld_list,              &
     &                               FEM_viz%field)
      call dealloc_field_lists_for_vizs(FEM_viz%viz_fld_list)
!
      call alloc_phys_data(FEM_viz%geofem%mesh%node%numnod,             &
     &                     FEM_viz%field)
!
      end subroutine FEM_initialize_surface
!
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_surface(i_step, ucd_step, time_d,          &
     &                               FEM_viz, m_SR)
!
      use output_parallel_ucd_file
      use nod_phys_send_recv
!
      integer (kind =kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: ucd_step
!
      type(time_data), intent(inout) :: time_d
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: istep_ucd
!
!
      istep_ucd = IO_step_exc_zero_inc(i_step, ucd_step)
      call set_data_by_read_ucd(istep_ucd, FEM_viz%ucd_file_IO,         &
     &    FEM_viz%ucd_time, FEM_viz%ucd_in, FEM_viz%field)
      call copy_time_step_size_data(FEM_viz%ucd_time, time_d)
!
      if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(FEM_viz%geofem%mesh, FEM_viz%field,     &
     &                          m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
      end subroutine FEM_analyze_surface
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_VTK_convert(ucd_step, init_d,           &
     &                                      FEM_viz, m_SR)
!
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use set_parallel_file_name
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
!
      type(IO_step_param), intent(in) :: ucd_step
      type(time_data), intent(in) :: init_d
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: istep_ucd
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
      call mpi_input_mesh(FEM_viz%mesh_file_IO, nprocs, FEM_viz%geofem)
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_comm_initialization(FEM_viz%geofem%mesh, m_SR)
      call FEM_mesh_initialization                                      &
     &   (FEM_viz%geofem%mesh, FEM_viz%geofem%group,                    &
     &    m_SR%SR_sig, m_SR%SR_i)
!
!     ---------------------
!
      FEM_viz%ucd_in%nnod = FEM_viz%geofem%mesh%node%numnod
      istep_ucd = IO_step_exc_zero_inc(init_d%i_time_step, ucd_step)
      call sel_read_parallel_udt_param(istep_ucd,                       &
     &    FEM_viz%ucd_file_IO, FEM_viz%ucd_time, FEM_viz%ucd_in)
      call alloc_phys_name_type_by_output(FEM_viz%ucd_in,               &
     &                                    FEM_viz%field)
      call alloc_phys_data(FEM_viz%geofem%mesh%node%numnod,             &
     &                     FEM_viz%field)
!
      end subroutine FEM_initialize_VTK_convert
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_surf
