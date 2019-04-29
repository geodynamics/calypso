!>@file  MPI_element_data_IO.f90
!!      module MPI_element_data_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine mpi_read_element_comm_table(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: comm_IO
!!      subroutine mpi_write_element_comm_table(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(in) :: comm_IO
!!
!!      subroutine mpi_read_element_geometry(IO_param, nod_IO, sfed_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine mpi_write_element_geometry(IO_param, nod_IO, sfed_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!@endverbatim
!
      module MPI_element_data_IO
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
      use MPI_node_geometry_IO
      use MPI_element_connect_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine mpi_read_element_comm_table(IO_param, comm_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call mpi_skip_read(IO_param, len(hd_ecomm_para()))
      call mpi_skip_read(IO_param, len(hd_fem_para()))
      call mpi_read_domain_info(IO_param, comm_IO)
!
      call mpi_skip_read(IO_param, len(hd_ecomm_import()))
      call mpi_read_import_data(IO_param, comm_IO)
!
      call mpi_skip_read(IO_param, len(hd_ecomm_export()))
      call mpi_read_export_data(IO_param, comm_IO)
!
      end subroutine mpi_read_element_comm_table
!
!------------------------------------------------------------------
!
      subroutine mpi_write_element_comm_table(IO_param, comm_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: comm_IO
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_ecomm_para()), hd_ecomm_para())
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
      end subroutine mpi_write_element_comm_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_read_element_geometry(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call mpi_skip_read(IO_param, len(hd_ecomm_point()))
      call mpi_read_number_of_node(IO_param, nod_IO)
      call mpi_read_geometry_info(IO_param, nod_IO)
!
      call mpi_skip_read(IO_param, len(hd_ecomm_vol()))
      call mpi_read_scl_in_ele(IO_param, nod_IO, sfed_IO)
!
      end subroutine mpi_read_element_geometry
!
!------------------------------------------------------------------
!
      subroutine mpi_write_element_geometry(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_ecomm_point()), hd_ecomm_point())
      call mpi_write_geometry_info(IO_param, nod_IO)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_ecomm_vol()), hd_ecomm_vol())
      call mpi_write_scl_in_ele(IO_param, nod_IO, sfed_IO)
!
      end subroutine mpi_write_element_geometry
!
!------------------------------------------------------------------
!
      end module MPI_element_data_IO
