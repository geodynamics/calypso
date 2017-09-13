!>@file  MPI_edge_data_IO_b.f90
!!      module MPI_edge_data_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief data IO orutines for edge
!!
!!@verbatim
!!      subroutine mpi_read_edge_connection_b                           &
!!     &         (IO_param, comm_IO, ele_IO, sfed_IO)
!!      subroutine mpi_write_edge_connection_b                          &
!!     &         (IO_param, comm_IO, ele_IO, sfed_IO)
!!
!!      subroutine mpi_read_edge_geometry_b(IO_param, nod_IO, sfed_IO)
!!      subroutine mpi_write_edge_geometry_b(IO_param, nod_IO, sfed_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(node_data), intent(inout) :: nod_IO
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
!
      module MPI_edge_data_IO_b
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_read_mesh_data
      use t_surf_edge_IO
      use m_fem_surface_labels
!
      use MPI_domain_data_IO_b
      use MPI_node_geometry_IO_b
      use MPI_element_connect_IO_b
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine mpi_read_edge_connection_b                             &
     &         (IO_param, comm_IO, ele_IO, sfed_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
      type(element_data), intent(inout) :: ele_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call mpi_read_domain_info_b(IO_param, comm_IO)
!
      call mpi_read_element_info_b(IO_param, ele_IO)
!
      call mpi_read_surface_4_element_b(IO_param, sfed_IO)
      call mpi_read_edge_4_element_b(IO_param, sfed_IO)
!
!
      call mpi_read_import_data_b(IO_param, comm_IO)
      call mpi_read_export_data_b(IO_param, comm_IO)
!
      end subroutine mpi_read_edge_connection_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_edge_connection_b                            &
     &         (IO_param, comm_IO, ele_IO, sfed_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
      type(element_data), intent(inout) :: ele_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call mpi_write_domain_info_b(IO_param, comm_IO)
!
      call mpi_write_element_info_b(IO_param, ele_IO)
!
      call mpi_write_surface_4_element_b(IO_param, sfed_IO)
      call mpi_write_edge_4_element_b(IO_param, sfed_IO)
!
!
      call mpi_write_import_data_b(IO_param, comm_IO)
      call mpi_write_export_data_b(IO_param, comm_IO)
!
      end subroutine mpi_write_edge_connection_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_read_edge_geometry_b(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call mpi_read_number_of_node_b(IO_param, nod_IO)
      call mpi_read_geometry_info_b(IO_param, nod_IO)
!
      call mpi_read_vect_in_ele_b(IO_param, nod_IO, sfed_IO)
      call mpi_read_scl_in_ele_b(IO_param, nod_IO, sfed_IO)
!
      end subroutine mpi_read_edge_geometry_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_edge_geometry_b(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call mpi_write_geometry_info_b(IO_param, nod_IO)
!
      call mpi_write_vect_in_ele_b(IO_param, nod_IO, sfed_IO)
      call mpi_write_scl_in_ele_b(IO_param, nod_IO, sfed_IO)
!
      end subroutine mpi_write_edge_geometry_b
!
!------------------------------------------------------------------
!
      end module MPI_edge_data_IO_b
