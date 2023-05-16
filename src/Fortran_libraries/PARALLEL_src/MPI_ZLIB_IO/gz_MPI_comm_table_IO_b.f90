!>@file  gz_MPI_comm_table_IO_b.f90
!!      module gz_MPI_comm_table_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine gz_mpi_read_comm_table_b(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: comm_IO
!!      subroutine gz_mpi_write_comm_table_b(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(in) :: comm_IO
!!
!!      subroutine gz_mpi_read_calypso_comm_tbl_b                       &
!!     &         (IO_param, import_IO, export_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: import_IO
!!        type(communication_table), intent(inout) :: export_IO
!!      subroutine gz_mpi_write_calypso_comm_tbl_b                      &
!!     &         (IO_param, import_IO, export_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(in) :: import_IO, export_IO
!!@endverbatim
!
      module gz_MPI_comm_table_IO_b
!
      use m_precision
!
      use t_geometry_data
      use t_read_mesh_data
      use t_comm_table
      use t_surf_edge_IO
      use m_fem_surface_labels
!
      use gz_MPI_domain_data_IO_b
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_comm_table_b(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call gz_mpi_read_domain_info_b(IO_param, comm_IO)
!
      call gz_mpi_read_import_data_b(IO_param, comm_IO)
      call gz_mpi_read_export_data_b(IO_param, comm_IO)
!
      end subroutine gz_mpi_read_comm_table_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_comm_table_b(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: comm_IO
!
!
      call gz_mpi_write_domain_info_b(IO_param, comm_IO)
!
      call gz_mpi_write_import_data_b(IO_param, comm_IO)
      call gz_mpi_write_export_data_b(IO_param, comm_IO)
!
      end subroutine gz_mpi_write_comm_table_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_calypso_comm_tbl_b                         &
     &         (IO_param, import_IO, export_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: import_IO, export_IO
!
!
      call gz_mpi_read_domain_info_b(IO_param, import_IO)
      call gz_mpi_read_import_data_b(IO_param, import_IO)
!
      call gz_mpi_read_domain_info_b(IO_param, export_IO)
      call gz_mpi_read_export_data_b(IO_param, export_IO)
!
      end subroutine gz_mpi_read_calypso_comm_tbl_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_calypso_comm_tbl_b                        &
     &         (IO_param, import_IO, export_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: import_IO, export_IO
!
!
      call gz_mpi_write_domain_info_b(IO_param, import_IO)
      call gz_mpi_write_import_data_b(IO_param, import_IO)
!
      call gz_mpi_write_domain_info_b(IO_param, export_IO)
      call gz_mpi_write_export_data_b(IO_param, export_IO)
!
      end subroutine gz_mpi_write_calypso_comm_tbl_b
!
!------------------------------------------------------------------
!
      end module gz_MPI_comm_table_IO_b
