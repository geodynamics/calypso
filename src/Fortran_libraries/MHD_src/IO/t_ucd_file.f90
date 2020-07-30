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
!!     &         (ucd_param, i_step, ucd_step, time_d, ucd)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(time_data), intent(in) :: time_d
!!        type(ucd_data), intent(in) :: ucd
!!
!!      subroutine output_grd_file_4_snapshot                           &
!!     &         (ucd_param, ucd_step, mesh, nod_fld, ucd)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(phys_data), intent(in) :: nod_fld
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(ucd_data), intent(inout) :: ucd
!!      subroutine read_udt_4_snap                                      &
!!     &         (i_step, ucd_step, udt_file_param, nod_fld, t_IO)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(field_IO_params), intent(in) :: udt_file_param
!!        type(phys_data),intent(inout) :: nod_fld
!!        type(time_data), intent(inout) :: t_IO
!!      subroutine finalize_output_ucd(ucd_param, ucd)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(ucd_data), intent(inout) :: ucd
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
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_output_ucd_file_control                              &
     &         (ucd_param, i_step, ucd_step, time_d, ucd)
!
      use calypso_mpi
      use parallel_ucd_IO_select
!
      integer(kind = kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: ucd_step
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(in) :: time_d
      type(ucd_data), intent(in) :: ucd
!
      integer(kind = kint) :: istep_ucd
!
!
      if(ucd_param%iflag_format .lt. 0) return
      if(output_IO_flag(i_step,ucd_step) .eqv. .FALSE.) return
!
      istep_ucd = IO_step_exc_zero_inc(i_step, ucd_step)
      call sel_write_parallel_ucd_file                                  &
     &   (istep_ucd, ucd_param, time_d, ucd)
!      call output_range_data(node, nod_fld, istep_ucd, time)
!
      end subroutine s_output_ucd_file_control
!
! ----------------------------------------------------------------------
!
      subroutine output_grd_file_4_snapshot                             &
     &         (ucd_param, ucd_step, mesh, nod_fld, ucd)
!
      use output_parallel_ucd_file
!
      type(field_IO_params), intent(in) :: ucd_param
      type(IO_step_param), intent(in) :: ucd_step
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data),intent(in) :: nod_fld
      type(ucd_data), intent(inout) :: ucd
!
!
      if(ucd_param%iflag_format .lt. 0) return
      if(ucd_step%increment .eq. 0) return
      call link_output_grd_file(mesh%node, mesh%ele, mesh%nod_comm,     &
     &    nod_fld, ucd_param, ucd)
!
      end subroutine output_grd_file_4_snapshot
!
! ----------------------------------------------------------------------
!
      subroutine read_udt_4_snap                                        &
     &         (i_step, ucd_step, udt_file_param, nod_fld, t_IO)
!
      use calypso_mpi
      use output_parallel_ucd_file
!
      integer(kind = kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: ucd_step
      type(field_IO_params), intent(in) :: udt_file_param
      type(phys_data),intent(inout) :: nod_fld
      type(time_data), intent(inout) :: t_IO
!
      integer(kind = kint) :: istep_ucd
!
!
      if(output_IO_flag(i_step,ucd_step) .eqv. .FALSE.) return
      istep_ucd = IO_step_exc_zero_inc(i_step, ucd_step)
      call set_data_by_read_ucd_once                                    &
    &    (my_rank, istep_ucd, udt_file_param, nod_fld, t_IO)
!
      end subroutine read_udt_4_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine finalize_output_ucd(ucd_param, ucd)
!
      use output_parallel_ucd_file
!
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_data), intent(inout) :: ucd
!
!
      call finalize_ucd_file_output(ucd_param, ucd)
!
      end subroutine finalize_output_ucd
!
!-----------------------------------------------------------------------
!
      end module t_ucd_file
