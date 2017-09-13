!>@file  gz_edge_data_IO.f90
!!      module gz_edge_data_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief data IO orutines for edge
!!
!!@verbatim
!!      subroutine gz_read_edge_connection                              &
!!     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO, ierr)
!!      subroutine gz_write_edge_connection                             &
!!     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO)
!!
!!      subroutine gz_read_edge_geometry(nod_IO, sfed_IO)
!!      subroutine gz_write_edge_geometry(nod_IO, sfed_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(node_data), intent(inout) :: nod_IO
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
!
      module gz_edge_data_IO
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
      subroutine gz_read_edge_connection                                &
     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO, ierr)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO
      use gz_element_connect_IO
!
      integer (kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      type(element_data), intent(inout) :: ele_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!      write(id_file,'(a,a1)') '!' , char(0)
!      write(id_file,'(a,a1)') '!  edge connectivity ', char(0)
!      write(id_file,'(a,a1)') '!  and communication table ', char(0)
!      write(id_file,'(a,a1)') '!' , char(0)
!      write(id_file,'(a,a1)', advance='NO') hd_fem_para(), char(0)
!
      call gz_read_domain_info(my_rank_IO, comm_IO, ierr)
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '!  2  edge connectivity', char(0)
!      write(id_file,'(a,a1)') '!  2.1  edge connectivity ', char(0)
!      write(id_file,'(a,a1)') '!      (type and connection) ', char(0)
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_number_of_element(ele_IO)
      call gz_read_element_info(ele_IO)
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '!  2.2  edge id for each surface',      &
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_surface_4_element(sfed_IO)
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '!  2.3   edge id for each element',     &
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_edge_4_element(sfed_IO)
!
!
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '! 3.import / export information ',      &
!      write(id_file,'(a,a1)') '! 3.1 edge ID for import ', char(0)
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_import_data(comm_IO)
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '! 3.2 edge ID for export ', char(0)
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_export_data(comm_IO)
!
      end subroutine gz_read_edge_connection
!
!------------------------------------------------------------------
!
      subroutine gz_write_edge_connection                               &
     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO
      use gz_element_connect_IO
!
      integer (kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      type(element_data), intent(inout) :: ele_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      textbuf = hd_edge_para() // char(0)
      call gz_write_textbuf_no_lf
      textbuf = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_domain_info(my_rank_IO, comm_IO)
!
      textbuf = hd_edge_connect() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_element_info(ele_IO)
!
      textbuf = hd_edge_on_surf() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_surface_4_element(sfed_IO)
!
      textbuf = hd_edge_on_ele() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_edge_4_element(sfed_IO)
!
!
!
      textbuf = hd_edge_import() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_import_data(comm_IO)
!
      textbuf = hd_edge_export() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_export_data(comm_IO)
!
      end subroutine gz_write_edge_connection
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_edge_geometry(nod_IO, sfed_IO)
!
      use gz_node_geometry_IO
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '! 4.   geometry of edge', char(0)
!      write(id_file,'(a,a1)') '!  4.1. center of edge', char(0)
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_number_of_node(nod_IO)
      call gz_read_geometry_info(nod_IO)
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '!  4.2  direction of edge', char(0)
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_vector_in_element(nod_IO, sfed_IO)
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '!  4.3  length of edge', char(0)
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_scalar_in_element(nod_IO, sfed_IO)
!
      end subroutine gz_read_edge_geometry
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_edge_geometry(nod_IO, sfed_IO)
!
      use gz_node_geometry_IO
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      textbuf = hd_edge_point() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_geometry_info(nod_IO)
!
      textbuf = hd_edge_dir() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_vector_in_element(nod_IO, sfed_IO)
!
      textbuf = hd_edge_length() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_scalar_in_element(nod_IO, sfed_IO)
!
      end subroutine gz_write_edge_geometry
!
!------------------------------------------------------------------
!
      end module gz_edge_data_IO
