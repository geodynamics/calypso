!>@file   FEM_analyzer_four_vizs.f90
!!@brief  module FEM_analyzer_four_vizs
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for visualizers
!!
!!@verbatim
!!      subroutine FEM_initialize_four_vizs                             &
!!     &         (elps_VIZ, init_d, ucd_step, viz_step,                 &
!!     &          FEM_viz, pvr, m_SR)
!!        type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(in) :: init_d
!!        type(VIZ_step_params), intent(inout) :: viz_step
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(VIZ_mesh_field), intent(inout) :: pvr
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine FEM_analyze_four_vizs                                &
!!     &         (istep, ucd_step, time_d, FEM_viz, m_SR)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(inout) :: time_d
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module FEM_analyzer_four_vizs
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_step_parameter
      use t_time_data
      use t_FEM_mesh_field_4_viz
      use t_ucd_data
      use t_next_node_ele_4_node
      use t_shape_functions
      use t_jacobians
      use t_file_IO_parameter
      use t_field_list_for_vizs
      use t_VIZ_step_parameter
      use t_mesh_SR
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine FEM_initialize_four_vizs                               &
     &         (elps_VIZ, init_d, ucd_step, viz_step,                   &
     &          FEM_viz, pvr, m_SR)
!
      use t_elapsed_labels_4_VIZ
      use t_VIZ_mesh_field
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use set_parallel_file_name
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use FEM_to_VIZ_bridge
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(IO_step_param), intent(in) :: ucd_step
      type(time_data), intent(in) :: init_d
!
      type(VIZ_step_params), intent(inout) :: viz_step
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(VIZ_mesh_field), intent(inout) :: pvr
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: istep_ucd
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mpi_input_mesh(FEM_viz%mesh_file_IO, nprocs, FEM_viz%geofem)
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_comm_initialization'
      call FEM_comm_initialization(FEM_viz%geofem%mesh, m_SR)
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
     &                                 FEM_viz%field)
      call dealloc_field_lists_for_vizs(FEM_viz%viz_fld_list)
!
      call alloc_phys_data(FEM_viz%geofem%mesh%node%numnod,             &
     &                     FEM_viz%field)
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      if(iflag_debug.gt.0) write(*,*) 'init_FEM_to_VIZ_bridge'
      call init_FEM_to_VIZ_bridge(elps_VIZ, viz_step,                   &
     &                            FEM_viz%geofem, pvr, m_SR)
      call calypso_mpi_barrier
!
      end subroutine FEM_initialize_four_vizs
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_four_vizs                                  &
     &         (istep, ucd_step, time_d, FEM_viz, m_SR)
!
      use output_parallel_ucd_file
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: istep
      type(IO_step_param), intent(in) :: ucd_step
!
      type(time_data), intent(inout) :: time_d
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: istep_ucd
!
!
      istep_ucd = IO_step_exc_zero_inc(istep, ucd_step)
      call set_data_by_read_ucd(istep_ucd, FEM_viz%ucd_file_IO,         &
     &    FEM_viz%ucd_time, FEM_viz%ucd_in, FEM_viz%field)
      call copy_time_step_size_data(FEM_viz%ucd_time, time_d)
!
      if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(FEM_viz%geofem%mesh, FEM_viz%field,     &
     &                          m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
      end subroutine FEM_analyze_four_vizs
!
! ----------------------------------------------------------------------
!
      end module FEM_analyzer_four_vizs
