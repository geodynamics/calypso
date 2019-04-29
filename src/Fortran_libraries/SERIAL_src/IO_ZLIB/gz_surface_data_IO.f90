!>@file  gz_surface_data_IO.f90
!!       module gz_surface_data_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in Aug., 2006
!
!> @brief routines for surface mesh data IO
!!
!!@verbatim
!!      subroutine gz_read_surface_connection                           &
!!     &         (id_rank, comm_IO, ele_IO, sfed_IO, ierr)
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine gz_write_surface_connection                          &
!!     &         (id_rank, comm_IO, ele_IO, sfed_IO)
!!        type(communication_table), intent(in) :: comm_IO
!!        type(element_data), intent(in) :: ele_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!
!!      subroutine gz_read_surface_geometry(nod_IO, sfed_IO)
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!      subroutine gz_write_surface_geometry(nod_IO, sfed_IO)
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!@endverbatim
!
      module gz_surface_data_IO
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
      subroutine gz_read_surface_connection                             &
     &         (id_rank, comm_IO, ele_IO, sfed_IO, ierr)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO
      use gz_element_connect_IO
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(inout) :: comm_IO
      type(element_data), intent(inout) :: ele_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!      write(id_file,'(a)') '!' 
!      write(id_file,'(a)') '!  surface connectivity '
!      write(id_file,'(a)') '!  and communication table '
!      write(id_file,'(a)') '!' 
!      write(id_file,'(a)', advance='NO') hd_fem_para()
!
      call gz_read_domain_info(id_rank, comm_IO, ierr)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '!  2  surface connectivity'
!      write(id_file,'(a)') '!  2.1  surface connectivity '
!      write(id_file,'(a)') '!      (type and connection) '
!      write(id_file,'(a)') '!'
!
      call gz_read_number_of_element(ele_IO)
      call gz_read_element_info(ele_IO)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '!  2.2 surface id for each element'
!      write(id_file,'(a)') '!        positive: outward normal'
!      write(id_file,'(a)') '!        normal: inward normal'
!      write(id_file,'(a)') '!'
!
      call gz_read_surface_4_element(sfed_IO)
!
!
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 3.import / export information '
!      write(id_file,'(a)') '! 3.1 surface ID for import '
!      write(id_file,'(a)') '!'
!
      call gz_read_import_data(comm_IO)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 3.2 surface ID for export '
!      write(id_file,'(a)') '!'
!
      call gz_read_export_data(comm_IO)
!
      end subroutine gz_read_surface_connection
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_surface_connection                            &
     &         (id_rank, comm_IO, ele_IO, sfed_IO)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO
      use gz_element_connect_IO
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(element_data), intent(in) :: ele_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      textbuf = hd_surf_para() // char(0)
      call gz_write_textbuf_no_lf
      textbuf = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_domain_info(id_rank, comm_IO)
!
      textbuf = hd_surf_connect() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_element_info(ele_IO)
!
      textbuf = hd_surf_on_ele() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_surface_4_element(sfed_IO)
!
!
!
      textbuf = hd_surf_import() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_import_data(comm_IO)
!
      textbuf = hd_surf_export() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_export_data(comm_IO)
!
      end subroutine gz_write_surface_connection
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_surface_geometry(nod_IO, sfed_IO)
!
      use gz_node_geometry_IO
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 4.  geometry of surface'
!      write(id_file,'(a)') '! 4.1 center of surface'
!      write(id_file,'(a)') '!'
!
      call gz_read_number_of_node(nod_IO)
      call gz_read_geometry_info(nod_IO)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '!  4.2 normal vector of surface'
!      write(id_file,'(a)') '!'
!
      call gz_read_vector_in_element(nod_IO, sfed_IO)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 4.3 area of surface'
!      write(id_file,'(a)') '!'
!
      call gz_read_scalar_in_element(nod_IO, sfed_IO)
!
      end subroutine gz_read_surface_geometry
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_surface_geometry(nod_IO, sfed_IO)
!
      use gz_node_geometry_IO
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      textbuf = hd_surf_point() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_geometry_info(nod_IO)
!
      textbuf = hd_surf_norm() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_vector_in_element(nod_IO, sfed_IO)
!
      textbuf = hd_surf_area() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_scalar_in_element(nod_IO, sfed_IO)
!
      end subroutine gz_write_surface_geometry
!
!------------------------------------------------------------------
!
      end module gz_surface_data_IO
