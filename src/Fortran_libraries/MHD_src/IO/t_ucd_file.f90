!>@file   t_ucd_file.f90
!!@brief  module t_ucd_file
!!
!!@author H. Matsui
!!@date  Programmed in July 2000 (ver 1.1)
!!@n      Modified in Aug., 2007
!!@n      Modified in Dec., 2015
!
!>@brief Strucures for field data output
!!
!!@verbatim
!!      subroutine s_output_ucd_file_control                            &
!!     &         (ucd_param, i_step, time_d, ucd_step, fem_ucd)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(time_data), intent(in) :: time_d
!!        type(IO_step_param), intent(inout) :: ucd_step
!!        type(ucd_file_data), intent(inout) :: fem_ucd
!!
!!      subroutine output_grd_file_4_snapshot                           &
!!     &         (ucd_param, ucd_step, mesh, nod_fld, fem_ucd)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(phys_data), intent(in) :: nod_fld
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(ucd_file_data), intent(inout) :: fem_ucd
!!      subroutine read_udt_4_snap                                      &
!!     &         (i_step, udt_file_param, nod_fld, t_IO, ucd_step)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(field_IO_params), intent(in) :: udt_file_param
!!        type(phys_data),intent(inout) :: nod_fld
!!      subroutine finalize_output_ucd(ucd_param, fem_ucd)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(ucd_file_data), intent(inout) :: fem_ucd
!!@endverbatim
!
      module t_ucd_file
!
      use m_precision
      use m_constants
!
      use t_time_data
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_ucd_data
      use t_phys_data
      use t_file_IO_parameter
      use t_IO_step_parameter
!
      implicit none
!
!
      type ucd_file_data
        type(time_data) :: time_IO
!>        Instance for FEM field data IO
        type(ucd_data) :: ucd
!>        Instance for numbers of FEM mesh for merged IO
        type(merged_ucd_data) :: m_ucd
      end type ucd_file_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_output_ucd_file_control                              &
     &         (ucd_param, i_step, time_d, ucd_step, fem_ucd)
!
      use calypso_mpi
      use parallel_ucd_IO_select
!
      integer(kind = kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(in) :: time_d
      type(IO_step_param), intent(inout) :: ucd_step
      type(ucd_file_data), intent(inout) :: fem_ucd
!
!
      if(ucd_param%iflag_format .lt. 0) return
      if(output_IO_flag(i_step,ucd_step) .ne. 0) return
!
      call set_IO_step_flag(i_step,ucd_step)
      call copy_time_step_size_data(time_d, fem_ucd%time_IO)
      call sel_write_parallel_ucd_file(ucd_step%istep_file,             &
     &    ucd_param, fem_ucd%time_IO, fem_ucd%ucd, fem_ucd%m_ucd)
!      call output_range_data(node, nod_fld, ucd_step%istep_file, time)
!
      end subroutine s_output_ucd_file_control
!
! ----------------------------------------------------------------------
!
      subroutine output_grd_file_4_snapshot                             &
     &         (ucd_param, ucd_step, mesh, nod_fld, fem_ucd)
!
      use output_parallel_ucd_file
!
      type(field_IO_params), intent(in) :: ucd_param
      type(IO_step_param), intent(in) :: ucd_step
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data),intent(in) :: nod_fld
      type(ucd_file_data), intent(inout) :: fem_ucd
!
!
      if(ucd_param%iflag_format .lt. 0) return
      if(ucd_step%increment .eq. 0) return
      call link_output_grd_file(mesh%node, mesh%ele, mesh%nod_comm,     &
     &    nod_fld, ucd_param, fem_ucd%ucd, fem_ucd%m_ucd)
!
      end subroutine output_grd_file_4_snapshot
!
! ----------------------------------------------------------------------
!
      subroutine read_udt_4_snap                                        &
     &         (i_step, udt_file_param, nod_fld, t_IO, ucd_step)
!
      use calypso_mpi
      use set_ucd_data_to_type
!
      integer(kind = kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: udt_file_param
      type(phys_data),intent(inout) :: nod_fld
      type(time_data), intent(inout) :: t_IO
      type(IO_step_param), intent(inout) :: ucd_step
!
!
      if(output_IO_flag(i_step,ucd_step) .ne. izero) return
      call set_IO_step_flag(i_step,ucd_step)
      call set_data_by_read_ucd_once(my_rank, ucd_step%istep_file,      &
    &     udt_file_param, nod_fld, t_IO)
!
      end subroutine read_udt_4_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine finalize_output_ucd(ucd_param, fem_ucd)
!
      use output_parallel_ucd_file
!
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_file_data), intent(inout) :: fem_ucd
!
!
      call finalize_ucd_file_output(ucd_param, fem_ucd%m_ucd)
!
      end subroutine finalize_output_ucd
!
!-----------------------------------------------------------------------
!
      end module t_ucd_file
