!>@file  MPI_comm_table_IO.f90
!!      module MPI_comm_table_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine mpi_read_comm_table(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: comm_IO
!!      subroutine mpi_write_comm_table(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(in) :: comm_IO
!!
!!      subroutine mpi_read_calypso_comm_tbl                            &
!!     &         (IO_param, import_IO, export_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: import_IO
!!        type(communication_table), intent(inout) :: export_IO
!!      subroutine mpi_write_calypso_comm_tbl                           &
!!     &         (IO_param, import_IO, export_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(in) :: import_IO, export_IO
!!@endverbatim
!
      module MPI_comm_table_IO
!
      use m_precision
!
      use t_geometry_data
      use t_read_mesh_data
      use t_comm_table
      use t_surf_edge_IO
      use m_fem_surface_labels
!
      use MPI_ascii_data_IO
      use MPI_domain_data_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine mpi_read_comm_table(IO_param, comm_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call mpi_skip_read(IO_param, len(hd_fem_para()))
      call mpi_read_domain_info(IO_param, comm_IO)
!
      call mpi_skip_read(IO_param, len(hd_ecomm_import()))
      call mpi_read_import_data(IO_param, comm_IO)
!
      call mpi_skip_read(IO_param, len(hd_ecomm_export()))
      call mpi_read_export_data(IO_param, comm_IO)
!
      end subroutine mpi_read_comm_table
!
!------------------------------------------------------------------
!
      subroutine mpi_write_comm_table(IO_param, comm_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: comm_IO
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_para()), hd_fem_para())
      call mpi_write_domain_info(IO_param, comm_IO)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_ecomm_import()), hd_ecomm_import())
      call mpi_write_import_data(IO_param, comm_IO)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_ecomm_export()), hd_ecomm_export())
      call mpi_write_export_data(IO_param, comm_IO)
!
      end subroutine mpi_write_comm_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_read_calypso_comm_tbl                         &
     &         (IO_param, import_IO, export_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: import_IO
      type(communication_table), intent(inout) :: export_IO
!
!
      call mpi_skip_read(IO_param, len(hd_fem_para()))
      call mpi_read_domain_info(IO_param, import_IO)
      call mpi_skip_read(IO_param, len(hd_ecomm_import()))
      call mpi_read_import_data(IO_param, import_IO)
!
      call mpi_skip_read(IO_param, len(hd_fem_para()))
      call mpi_read_domain_info(IO_param, export_IO)
      call mpi_skip_read(IO_param, len(hd_ecomm_export()))
      call mpi_read_export_data(IO_param, export_IO)
!
      end subroutine mpi_read_calypso_comm_tbl
!
!------------------------------------------------------------------
!
      subroutine mpi_write_calypso_comm_tbl                        &
     &         (IO_param, import_IO, export_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: import_IO, export_IO
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_para()), hd_fem_para())
      call mpi_write_domain_info(IO_param, import_IO)
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_ecomm_import()), hd_ecomm_import())
      call mpi_write_import_data(IO_param, import_IO)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_para()), hd_fem_para())
      call mpi_write_domain_info(IO_param, export_IO)
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_ecomm_export()), hd_ecomm_export())
      call mpi_write_export_data(IO_param, export_IO)
!
      end subroutine mpi_write_calypso_comm_tbl
!
!------------------------------------------------------------------
!
      end module MPI_comm_table_IO
