!>@file  gz_MPI_element_data_IO_b.f90
!!      module gz_MPI_element_data_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine gz_mpi_read_ele_comm_table_b(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: comm_IO
!!      subroutine gz_mpi_write_ele_comm_table_b(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(in) :: comm_IO
!!
!!      subroutine gz_mpi_read_ele_geometry_b(IO_param, nod_IO, sfed_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine gz_mpi_write_ele_geometry_b                          &
!!     &         (IO_param, nod_IO, sfed_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!@endverbatim
!
      module gz_MPI_element_data_IO_b
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
      use gz_MPI_node_geometry_IO_b
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_ele_comm_table_b(IO_param, comm_IO)
!
      use m_fem_mesh_labels
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
      end subroutine gz_mpi_read_ele_comm_table_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_ele_comm_table_b(IO_param, comm_IO)
!
      use m_fem_mesh_labels
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
      end subroutine gz_mpi_write_ele_comm_table_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_ele_geometry_b(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_mpi_read_number_of_node_b(IO_param, nod_IO)
      call gz_mpi_read_geometry_info_b(IO_param, nod_IO)
!
      call gz_mpi_read_scl_in_ele_b(IO_param, nod_IO, sfed_IO)
!
      end subroutine gz_mpi_read_ele_geometry_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_ele_geometry_b                            &
     &         (IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      call gz_mpi_write_geometry_info_b(IO_param, nod_IO)
      call gz_mpi_write_scl_in_ele_b(IO_param, nod_IO, sfed_IO)
!
      end subroutine gz_mpi_write_ele_geometry_b
!
!------------------------------------------------------------------
!
      end module gz_MPI_element_data_IO_b
