!>@file  gz_surface_data_IO_b.f90
!!       module gz_surface_data_IO_b
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in Aug., 2006
!
!> @brief routines for surface mesh data IO
!!
!!@verbatim
!!      subroutine gz_read_surface_connection_b                         &
!!     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO, ierr)
!!      subroutine gz_write_surface_connection_b                        &
!!     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(node_data), intent(inout) :: nod_IO
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!
!!      subroutine gz_read_surface_geometry_b(nod_IO, sfed_IO)
!!      subroutine gz_write_surface_geometry_b(nod_IO, sfed_IO)
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module gz_surface_data_IO_b
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_read_mesh_data
      use t_surf_edge_IO
      use m_fem_surface_labels
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_read_surface_connection_b                           &
     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO, ierr)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO_b
      use gz_element_connect_IO_b
!
      integer (kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      type(element_data), intent(inout) :: ele_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!      textbuf = hd_surf_para() // char(0)
!      textbuf = hd_fem_para() // char(0)
      call gz_read_domain_info_b(my_rank_IO, comm_IO, ierr)
!
!      textbuf = hd_surf_connect() // char(0)
      call gz_read_number_of_element_b(ele_IO)
      call gz_read_element_info_b(ele_IO)
!
!      textbuf = hd_surf_on_ele() // char(0)
      call gz_read_surface_4_element_b(sfed_IO)
!
!
!
!      textbuf = hd_surf_import() // char(0)
      call gz_read_import_data_b(comm_IO)
!
!      textbuf = hd_surf_export() // char(0)
      call gz_read_export_data_b(comm_IO)
!
      end subroutine gz_read_surface_connection_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_surface_connection_b                          &
     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO_b
      use gz_element_connect_IO_b
!
      integer (kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      type(element_data), intent(inout) :: ele_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
!      textbuf = hd_surf_para() // char(0)
!      textbuf = hd_fem_para() // char(0)
      call gz_write_domain_info_b(my_rank_IO, comm_IO)
!
!      textbuf = hd_surf_connect() // char(0)
      call gz_write_element_info_b(ele_IO)
!
!      textbuf = hd_surf_on_ele() // char(0)
      call gz_write_surface_4_element_b(sfed_IO)
!
!
!
!      textbuf = hd_surf_import() // char(0)
      call gz_write_import_data_b(comm_IO)
!
!      textbuf = hd_surf_export() // char(0)
      call gz_write_export_data_b(comm_IO)
!
      end subroutine gz_write_surface_connection_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_surface_geometry_b(nod_IO, sfed_IO)
!
      use gz_node_geometry_IO_b
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
!      textbuf = hd_surf_point() // char(0)
      call gz_read_number_of_node_b(nod_IO)
      call gz_read_geometry_info_b(nod_IO)
!
!      textbuf = hd_surf_norm() // char(0)
      call gz_read_vector_in_element_b(nod_IO, sfed_IO)
!
!      textbuf = hd_surf_area() // char(0)
      call gz_read_scalar_in_element_b(nod_IO, sfed_IO)
!
      end subroutine gz_read_surface_geometry_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_surface_geometry_b(nod_IO, sfed_IO)
!
      use gz_node_geometry_IO_b
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
!      textbuf = hd_surf_point() // char(0)
      call gz_write_geometry_info_b(nod_IO)
!
!      textbuf = hd_surf_norm() // char(0)
      call gz_write_vector_in_element_b(nod_IO, sfed_IO)
!
!      textbuf = hd_surf_area() // char(0)
      call gz_write_scalar_in_element_b(nod_IO, sfed_IO)
!
      end subroutine gz_write_surface_geometry_b
!
!------------------------------------------------------------------
!
      end module gz_surface_data_IO_b
