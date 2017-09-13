!>@file  element_file_IO_b.f90
!!      module element_file_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine input_element_file_b                                 &
!!     &         (my_rank_IO, file_name, ele_mesh_IO, ierr)
!!      subroutine input_surface_file_b                                 &
!!     &         (my_rank_IO, file_name, surf_mesh_IO, ierr)
!!      subroutine input_edge_file_b                                    &
!!     &         (my_rank_IO, file_name, edge_mesh_IO, ierr)
!!
!!      subroutine output_element_file_b                                &
!!     &         (my_rank_IO, ele_mesh_IO)
!!      subroutine output_surface_file_b                                &
!!     &         (my_rank_IO, file_name, surf_mesh_IO)
!!      subroutine output_edge_file_b                                   &
!!     &         (my_rank_IO, file_name, edge_mesh_IO)
!!@endverbatim
!!
!!@param my_rank_IO  MPI rank
!
      module element_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_read_mesh_data
      use set_mesh_file_names
      use binary_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine input_element_file_b                                   &
     &         (my_rank_IO, file_name, ele_mesh_IO, ierr)
!
      use element_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Read binary element comm file: ', trim(file_name)
!
      call open_read_binary_file(file_name, my_rank_IO)
      call read_element_comm_table_b                                    &
     &   (my_rank_IO, ele_mesh_IO%comm, ierr)
!      call read_element_geometry_b(ele_mesh_IO%node, ele_mesh_IO%sfed)
      call close_binary_file
!
      end subroutine input_element_file_b
!
!------------------------------------------------------------------
!
      subroutine input_surface_file_b                                   &
     &         (my_rank_IO, file_name, surf_mesh_IO, ierr)
!
      use surface_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Read binary surface mesh file: ', trim(file_name)
!
      call open_read_binary_file(file_name, my_rank_IO)
      call read_surface_connection_b(my_rank_IO, surf_mesh_IO%comm,     &
     &   surf_mesh_IO%ele, surf_mesh_IO%sfed, ierr)
!      call read_surface_geometry_b                                     &
!     &   (surf_mesh_IO%node, surf_mesh_IO%sfed)
      call close_binary_file
!
      end subroutine input_surface_file_b
!
!------------------------------------------------------------------
!
      subroutine input_edge_file_b                                      &
     &         (my_rank_IO, file_name, edge_mesh_IO, ierr)
!
      use edge_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Read binary edge mesh file: ', trim(file_name)
!
      call open_read_binary_file(file_name, my_rank_IO)
      call read_edge_connection_b(my_rank_IO, edge_mesh_IO%comm,        &
     &    edge_mesh_IO%ele, edge_mesh_IO%sfed, ierr)
!      call read_edge_geometry_b(edge_mesh_IO%node, edge_mesh_IO%sfed)
      call close_binary_file
!
      end subroutine input_edge_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_element_file_b                                  &
     &         (my_rank_IO, file_name, ele_mesh_IO)
!
      use element_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Write binary element comm file: ', trim(file_name)
!
      call open_write_binary_file(file_name)
      call write_element_comm_table_b(my_rank_IO, ele_mesh_IO%comm)
!      call write_element_geometry_b(ele_mesh_IO%node, ele_mesh_IO%sfed)
      call close_binary_file
!
      end subroutine output_element_file_b
!
!------------------------------------------------------------------
!
      subroutine output_surface_file_b                                  &
     &         (my_rank_IO, file_name, surf_mesh_IO)
!
      use surface_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Write binary surface mesh file: ', trim(file_name)
!
      call open_write_binary_file(file_name)
      call write_surface_connection_b(my_rank_IO, surf_mesh_IO%comm,    &
     &   surf_mesh_IO%ele, surf_mesh_IO%sfed)
!      call write_surface_geometry_b                                    &
!     &   (surf_mesh_IO%node, surf_mesh_IO%sfed)
      call close_binary_file
!
      end subroutine output_surface_file_b
!
!------------------------------------------------------------------
!
      subroutine output_edge_file_b                                     &
     &         (my_rank_IO, file_name, edge_mesh_IO)
!
      use edge_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Write binary edge mesh file: ', trim(file_name)
!
      call open_write_binary_file(file_name)
      call write_edge_connection_b(my_rank_IO, edge_mesh_IO%comm,       &
     &   edge_mesh_IO%ele, edge_mesh_IO%sfed)
!      call write_edge_geometry_b(edge_mesh_IO%node, edge_mesh_IO%sfed)
      call close_binary_file
!
      end subroutine output_edge_file_b
!
!------------------------------------------------------------------
!
      end module element_file_IO_b
