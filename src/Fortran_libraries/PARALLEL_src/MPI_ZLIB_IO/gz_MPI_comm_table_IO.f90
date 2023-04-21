!>@file  gz_MPI_comm_table_IO.f90
!!      module gz_MPI_comm_table_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine gz_mpi_read_comm_table(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: comm_IO
!!      subroutine gz_mpi_write_comm_table(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(in) :: comm_IO
!!
!!      subroutine gz_mpi_read_calypso_comm_tbl                         &
!!     &         (IO_param, import_IO, export_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: import_IO
!!        type(communication_table), intent(inout) :: export_IO
!!      subroutine gz_mpi_write_calypso_comm_tbl                        &
!!     &         (IO_param, import_IO, export_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(in) :: import_IO, export_IO
!!@endverbatim
!
      module gz_MPI_comm_table_IO
!
      use m_precision
!
      use t_geometry_data
      use t_read_mesh_data
      use t_comm_table
      use t_surf_edge_IO
      use m_fem_surface_labels
!
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_comm_table(IO_param, comm_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call gz_mpi_skip_header(IO_param, len(hd_fem_para()))
      call gz_mpi_read_domain_info(IO_param, comm_IO)
!
      call gz_mpi_skip_header(IO_param, len(hd_ecomm_import()))
      call gz_mpi_read_import_data(IO_param, comm_IO)
!
      call gz_mpi_skip_header(IO_param, len(hd_ecomm_export()))
      call gz_mpi_read_export_data(IO_param, comm_IO)
!
      end subroutine gz_mpi_read_comm_table
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_comm_table(IO_param, comm_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: comm_IO
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_para()), hd_fem_para())
      call gz_mpi_write_domain_info(IO_param, comm_IO)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ecomm_import()), hd_ecomm_import())
      call gz_mpi_write_import_data(IO_param, comm_IO)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ecomm_export()), hd_ecomm_export())
      call gz_mpi_write_export_data(IO_param, comm_IO)
!
      end subroutine gz_mpi_write_comm_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_calypso_comm_tbl                           &
     &         (IO_param, import_IO, export_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: import_IO
      type(communication_table), intent(inout) :: export_IO
!
!
      call gz_mpi_skip_header(IO_param, len(hd_fem_para()))
      call gz_mpi_read_domain_info(IO_param, import_IO)
      call gz_mpi_skip_header(IO_param, len(hd_ecomm_import()))
      call gz_mpi_read_import_data(IO_param, import_IO)
!
      call gz_mpi_skip_header(IO_param, len(hd_fem_para()))
      call gz_mpi_read_domain_info(IO_param, export_IO)
      call gz_mpi_skip_header(IO_param, len(hd_ecomm_export()))
      call gz_mpi_read_export_data(IO_param, export_IO)
!
      end subroutine gz_mpi_read_calypso_comm_tbl
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_calypso_comm_tbl                          &
     &         (IO_param, import_IO, export_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: import_IO, export_IO
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_para()), hd_fem_para())
      call gz_mpi_write_domain_info(IO_param, import_IO)
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ecomm_import()), hd_ecomm_import())
      call gz_mpi_write_import_data(IO_param, import_IO)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_para()), hd_fem_para())
      call gz_mpi_write_domain_info(IO_param, export_IO)
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ecomm_export()), hd_ecomm_export())
      call gz_mpi_write_export_data(IO_param, export_IO)
!
      end subroutine gz_mpi_write_calypso_comm_tbl
!
!------------------------------------------------------------------
!
      end module gz_MPI_comm_table_IO
