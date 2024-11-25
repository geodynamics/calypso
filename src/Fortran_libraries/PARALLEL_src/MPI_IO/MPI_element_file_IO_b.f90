!>@file  MPI_element_file_IO_b.f90
!!      module MPI_element_file_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine mpi_input_element_file_b                             &
!!     &         (num_pe, id_rank, file_name, ele_mesh_IO)
!!      subroutine mpi_input_surface_file_b                             &
!!     &         (num_pe, id_rank, file_name, surf_mesh_IO)
!!      subroutine mpi_input_edge_file_b                                &
!!     &         (num_pe, id_rank, file_name, edge_mesh_IO)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!
!!      subroutine mpi_output_element_file_b(file_name, ele_mesh_IO)
!!      subroutine mpi_output_surface_file_b(file_name, surf_mesh_IO)
!!      subroutine mpi_output_edge_file_b(file_name, edge_mesh_IO)
!!        type(surf_edge_IO_file), intent(in) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(in) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(in) :: edge_mesh_IO
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module MPI_element_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_calypso_mpi_IO_param
      use t_read_mesh_data
!
      implicit none
!
      type(calypso_MPI_IO_params), save, private :: IO_param
!
      private :: mpi_read_element_geometry_b
      private :: mpi_write_element_geometry_b
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine mpi_input_element_file_b                               &
     &         (num_pe, id_rank, file_name, ele_mesh_IO)
!
      use MPI_comm_table_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read merged binary element comm file: ', trim(file_name)
!
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param)
!
      call mpi_read_comm_table_b(IO_param, ele_mesh_IO%comm)
!      call mpi_read_element_geometry_b(IO_param,                       &
!     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_input_element_file_b
!
!------------------------------------------------------------------
!
      subroutine mpi_input_surface_file_b                               &
     &         (num_pe, id_rank, file_name, surf_mesh_IO)
!
      use MPI_surface_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read merged binary surface mesh file: ', trim(file_name)
!
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param)
!
      call mpi_read_surface_connection_b(IO_param, surf_mesh_IO%comm,   &
     &   surf_mesh_IO%ele, surf_mesh_IO%sfed)
!      call mpi_read_surface_geometry_b(IO_param,                       &
!     &    surf_mesh_IO%node, surf_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_input_surface_file_b
!
!------------------------------------------------------------------
!
      subroutine mpi_input_edge_file_b                                  &
     &         (num_pe, id_rank, file_name, edge_mesh_IO)
!
      use MPI_edge_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read merged binary edge mesh file: ', trim(file_name)
!
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param)
!
      call mpi_read_edge_connection_b(IO_param, edge_mesh_IO%comm,      &
     &    edge_mesh_IO%ele, edge_mesh_IO%sfed)
!      call mpi_read_edge_geometry_b(IO_param,                          &
!     &    edge_mesh_IO%node, edge_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_input_edge_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_output_element_file_b(file_name, ele_mesh_IO)
!
      use MPI_comm_table_IO_b
!
      character(len=kchara), intent(in) :: file_name
      type(surf_edge_IO_file), intent(in) :: ele_mesh_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write merged binary element comm file: ', trim(file_name)
!
      call open_write_mpi_file_b(file_name, IO_param)
      call mpi_write_comm_table_b(IO_param, ele_mesh_IO%comm)
!      call mpi_write_element_geometry_b(IO_param,                      &
!     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_output_element_file_b
!
!------------------------------------------------------------------
!
      subroutine mpi_output_surface_file_b(file_name, surf_mesh_IO)
!
      use MPI_surface_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      type(surf_edge_IO_file), intent(in) :: surf_mesh_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write merged binary surface mesh file: ', trim(file_name)
!
      call open_write_mpi_file_b(file_name, IO_param)
      call mpi_write_surface_connection_b(IO_param, surf_mesh_IO%comm,  &
     &   surf_mesh_IO%ele, surf_mesh_IO%sfed)
!      call mpi_write_surface_geometry_b(IO_param,                      &
!     &    surf_mesh_IO%node, surf_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_output_surface_file_b
!
!------------------------------------------------------------------
!
      subroutine mpi_output_edge_file_b(file_name, edge_mesh_IO)
!
      use MPI_edge_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      type(surf_edge_IO_file), intent(in) :: edge_mesh_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write merged binary edge mesh file: ', trim(file_name)
!
      call open_write_mpi_file_b(file_name, IO_param)
      call mpi_write_edge_connection_b(IO_param, edge_mesh_IO%comm,     &
     &   edge_mesh_IO%ele, edge_mesh_IO%sfed)
!      call mpi_write_edge_geometry_b(IO_param,                         &
!     &   edge_mesh_IO%node, edge_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_output_edge_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_read_element_geometry_b                            &
     &         (IO_param, nod_IO, sfed_IO)
!
      use MPI_node_geometry_IO_b
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call mpi_read_geometry_info_b(IO_param, nod_IO)
!
      call mpi_read_scl_in_ele_b(IO_param, nod_IO, sfed_IO)
!
      end subroutine mpi_read_element_geometry_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_element_geometry_b                           &
     &         (IO_param, nod_IO, sfed_IO)
!
      use MPI_node_geometry_IO_b
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      call mpi_write_geometry_info_b(IO_param, nod_IO)
      call mpi_write_scl_in_ele_b(IO_param, nod_IO, sfed_IO)
!
      end subroutine mpi_write_element_geometry_b
!
!------------------------------------------------------------------
!
      end module MPI_element_file_IO_b
