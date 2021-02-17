!>@file   FEM_analyzer_viz_surf.f90
!!@brief  module FEM_analyzer_viz_surf
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief FEM top routines for surfacing
!!
!!@verbatim
!!      subroutine set_control_params_4_sections                        &
!!     &         (sec_viz_ctl, sfcing, t_viz_param, ierr)
!!        type(control_data_section_only), intent(in) :: sec_viz_ctl
!!        type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!      subroutine FEM_initialize_surface(ucd_step, init_d, sfcing)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(in) :: init_d
!!        type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!!      subroutine FEM_analyze_surface(i_step, ucd_step, time_d, sfcing)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(inout) :: time_d
!!        type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!!
!!      subroutine FEM_initialize_VTK_convert(ucd_step, init_d, sfcing)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(in) :: init_d
!!        type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
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
      use t_mesh_data
      use t_phys_data
      use t_ucd_data
      use t_file_IO_parameter
      use t_field_list_for_vizs
      use t_VIZ_step_parameter
      use t_IO_step_parameter
      use t_vector_for_solver
!
      implicit none
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
!>        Structure for communicatiors for solver
        type(vectors_4_solver) :: v_sol
!
!>          time data from data input
        type(time_data) :: ucd_time
!>          FEM field data IO
        type(ucd_data) :: ucd_in
!
!>      structure of field list for visualization
        type(visulize_field_list) :: viz_fld_list
      end type FEM_mesh_field_4_surfacing
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_control_params_4_sections                          &
     &         (sec_viz_ctl, sfcing, t_viz_param, ierr)
!
      use t_viz_sections
      use t_control_data_section_only
      use t_VIZ_only_step_parameter
!
      use m_file_format_switch
      use m_default_file_prefix
      use set_control_platform_item
      use set_control_platform_data
      use ucd_IO_select
!
      type(control_data_section_only), intent(in) :: sec_viz_ctl
!
      type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
      integer(kind = kint), intent(inout) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank, sec_viz_ctl%sect_plt)
      call set_control_smp_def(my_rank, sec_viz_ctl%sect_plt)
      call set_control_mesh_def(sec_viz_ctl%sect_plt,                   &
     &                          sfcing%mesh_file_IO)
      call set_ucd_file_define(sec_viz_ctl%sect_plt,                    &
     &    sfcing%ucd_file_IO)
!
      call set_viz_field_list_control(sec_viz_ctl%viz_field_ctl,        &
     &                                sfcing%viz_fld_list)
!
      call set_fixed_t_step_params_w_viz                                &
     &   (sec_viz_ctl%t_sect_ctl, t_viz_param, ierr, e_message)
      call copy_delta_t(t_viz_param%init_d, t_viz_param%time_d)
!
      end subroutine set_control_params_4_sections
!
! ----------------------------------------------------------------------
!
      subroutine FEM_initialize_surface(ucd_step, init_d, sfcing)
!
      use t_field_list_for_vizs
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use set_parallel_file_name
      use set_ucd_data_to_type
      use ucd_IO_select
!
      type(IO_step_param), intent(in) :: ucd_step
      type(time_data), intent(in) :: init_d
      type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!
      integer(kind = kint) :: istep_ucd
!
!   --------------------------------
!       setup mesh and field information
!   --------------------------------
!
      call mpi_input_mesh(sfcing%mesh_file_IO, nprocs, sfcing%geofem)
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_comm_initialization(sfcing%geofem%mesh, sfcing%v_sol)
      call FEM_mesh_initialization(sfcing%geofem%mesh,                  &
     &                             sfcing%geofem%group)
!
!     ---------------------
!
      sfcing%ucd_in%nnod = sfcing%geofem%mesh%node%numnod
      istep_ucd = IO_step_exc_zero_inc(init_d%i_time_step, ucd_step)
      call sel_read_udt_param(my_rank, istep_ucd, sfcing%ucd_file_IO,   &
     &                        sfcing%ucd_time, sfcing%ucd_in)
      call alloc_phys_name_type_by_output(sfcing%ucd_in,                &
     &                                    sfcing%nod_fld)
      call add_field_in_viz_controls(sfcing%viz_fld_list,               &
     &                               sfcing%nod_fld)
      call alloc_phys_data(sfcing%geofem%mesh%node%numnod,              &
     &                     sfcing%nod_fld)
      call deallocate_surface_geom_type(sfcing%geofem%mesh%surf)
!
      end subroutine FEM_initialize_surface
!
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_surface(i_step, ucd_step, time_d, sfcing)
!
      use output_parallel_ucd_file
      use nod_phys_send_recv
!
      integer (kind =kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: ucd_step
      type(time_data), intent(inout) :: time_d
      type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!
      integer(kind = kint) :: istep_ucd
!
!
      istep_ucd = IO_step_exc_zero_inc(i_step, ucd_step)
      call set_data_by_read_ucd(istep_ucd, sfcing%ucd_file_IO,          &
     &    sfcing%ucd_time, sfcing%ucd_in, sfcing%nod_fld)
      call copy_time_step_size_data(sfcing%ucd_time, time_d)
!
      if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(sfcing%geofem%mesh,                     &
     &                          sfcing%nod_fld, sfcing%v_sol)
!
      end subroutine FEM_analyze_surface
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_VTK_convert(ucd_step, init_d, sfcing)
!
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use set_parallel_file_name
      use set_ucd_data_to_type
      use ucd_IO_select
!
      type(IO_step_param), intent(in) :: ucd_step
      type(time_data), intent(in) :: init_d
      type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!
      integer(kind = kint) :: istep_ucd
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
      call mpi_input_mesh(sfcing%mesh_file_IO, nprocs, sfcing%geofem)
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_comm_initialization(sfcing%geofem%mesh, sfcing%v_sol)
      call FEM_mesh_initialization(sfcing%geofem%mesh,                  &
     &                             sfcing%geofem%group)
!
!     ---------------------
!
      sfcing%ucd_in%nnod = sfcing%geofem%mesh%node%numnod
      istep_ucd = IO_step_exc_zero_inc(init_d%i_time_step, ucd_step)
      call sel_read_udt_param(my_rank, istep_ucd, sfcing%ucd_file_IO,   &
     &                        sfcing%ucd_time, sfcing%ucd_in)
      call alloc_phys_name_type_by_output(sfcing%ucd_in,                &
     &                                    sfcing%nod_fld)
      call alloc_phys_data(sfcing%geofem%mesh%node%numnod,              &
     &                     sfcing%nod_fld)
      call deallocate_surface_geom_type(sfcing%geofem%mesh%surf)
!
      end subroutine FEM_initialize_VTK_convert
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_surf
