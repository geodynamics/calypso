!>@file  surface_data_IO_b.f90
!!       module surface_data_IO_b
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in Aug., 2006
!
!> @brief routines for surface mesh data IO
!!
!!@verbatim
!!      subroutine read_surface_connection_b                            &
!!     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO, ierr)
!!      subroutine write_surface_connection_b                           &
!!     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(node_data), intent(inout) :: nod_IO
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!
!!      subroutine read_surface_geometry_b(nod_IO, sfed_IO)
!!      subroutine write_surface_geometry_b(nod_IO, sfed_IO)
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module surface_data_IO_b
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_read_mesh_data
      use t_surf_edge_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine read_surface_connection_b                              &
     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO, ierr)
!
      use m_fem_mesh_labels
      use domain_data_IO_b
      use element_connect_IO_b
!
      integer (kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      type(element_data), intent(inout) :: ele_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call read_domain_info_b(my_rank_IO, comm_IO, ierr)
!
      call read_number_of_element_b(ele_IO)
      call read_element_info_b(ele_IO)
      call read_surface_4_element_b(sfed_IO)
!
      call read_import_data_b(comm_IO)
      call read_export_data_b(comm_IO)
!
      end subroutine read_surface_connection_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surface_connection_b                             &
     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO)
!
      use m_fem_mesh_labels
      use domain_data_IO_b
      use element_connect_IO_b
!
      integer (kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      type(element_data), intent(inout) :: ele_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call write_domain_info_b(my_rank_IO, comm_IO)
!
      call write_element_info_b(ele_IO)
      call write_surface_4_element_b(sfed_IO)
!
      call write_import_data_b(comm_IO)
      call write_export_data_b(comm_IO)
!
      end subroutine write_surface_connection_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_surface_geometry_b(nod_IO, sfed_IO)
!
      use node_geometry_IO_b
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call read_number_of_node_b(nod_IO)
      call read_geometry_info_b(nod_IO)
      call read_vector_in_element_b(nod_IO, sfed_IO)
      call read_scalar_in_element_b(nod_IO, sfed_IO)
!
      end subroutine read_surface_geometry_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surface_geometry_b(nod_IO, sfed_IO)
!
      use node_geometry_IO_b
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call write_geometry_info_b(nod_IO)
      call write_vector_in_element_b(nod_IO, sfed_IO)
      call write_scalar_in_element_b(nod_IO, sfed_IO)
!
      end subroutine write_surface_geometry_b
!
!------------------------------------------------------------------
!
      end module surface_data_IO_b
