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
!!     &         (FPz_f, id_rank, comm_IO, ele_IO, sfed_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine gz_write_edge_connection                             &
!!     &         (FPz_f, id_rank, comm_IO, ele_IO, sfed_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(communication_table), intent(in) :: comm_IO
!!        type(element_data), intent(in) :: ele_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!
!!      subroutine gz_read_edge_geometry(FPz_f, nod_IO, sfed_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine gz_write_edge_geometry(FPz_f, nod_IO, sfed_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
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
      use t_buffer_4_gzip
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
     &         (FPz_f, id_rank, comm_IO, ele_IO, sfed_IO, zbuf, ierr)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO
      use gz_element_connect_IO
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(communication_table), intent(inout) :: comm_IO
      type(element_data), intent(inout) :: ele_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
!      write(id_file,'(a,a1)') '!' , char(0)
!      write(id_file,'(a,a1)') '!  edge connectivity ', char(0)
!      write(id_file,'(a,a1)') '!  and communication table ', char(0)
!      write(id_file,'(a,a1)') '!' , char(0)
!      write(id_file,'(a,a1)', advance='NO') hd_fem_para(), char(0)
!
      call gz_read_domain_info(FPz_f, id_rank, comm_IO, zbuf, ierr)
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '!  2  edge connectivity', char(0)
!      write(id_file,'(a,a1)') '!  2.1  edge connectivity ', char(0)
!      write(id_file,'(a,a1)') '!      (type and connection) ', char(0)
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_number_of_element(FPz_f, ele_IO, zbuf)
      call gz_read_element_info(FPz_f, ele_IO, zbuf)
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '!  2.2  edge id for each surface',      &
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_surface_4_element(FPz_f, sfed_IO, zbuf)
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '!  2.3   edge id for each element',     &
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_edge_4_element(FPz_f, sfed_IO, zbuf)
!
!
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '! 3.import / export information ',      &
!      write(id_file,'(a,a1)') '! 3.1 edge ID for import ', char(0)
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_import_data(FPz_f, comm_IO, zbuf)
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '! 3.2 edge ID for export ', char(0)
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_export_data(FPz_f, comm_IO, zbuf)
!
      end subroutine gz_read_edge_connection
!
!------------------------------------------------------------------
!
      subroutine gz_write_edge_connection                               &
     &         (FPz_f, id_rank, comm_IO, ele_IO, sfed_IO, zbuf)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO
      use gz_element_connect_IO
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(element_data), intent(in) :: ele_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%fixbuf(1) = hd_edge_para() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
      zbuf%fixbuf(1) = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_domain_info(FPz_f, id_rank, comm_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_edge_connect() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_element_info(FPz_f, ele_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_edge_on_surf() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_surface_4_element(FPz_f, sfed_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_edge_on_ele() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_edge_4_element(FPz_f, sfed_IO, zbuf)
!
!
      zbuf%fixbuf(1) = hd_edge_import() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call s(FPz_f, comm_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_edge_export() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_export_data(FPz_f, comm_IO, zbuf)
!
      end subroutine gz_write_edge_connection
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_edge_geometry(FPz_f, nod_IO, sfed_IO, zbuf)
!
      use gz_node_geometry_IO
!
      character, pointer, intent(in) :: FPz_f
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '! 4.   geometry of edge', char(0)
!      write(id_file,'(a,a1)') '!  4.1. center of edge', char(0)
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_geometry_info(FPz_f, nod_IO, zbuf)
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '!  4.2  direction of edge', char(0)
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_vector_in_element(FPz_f, nod_IO, sfed_IO, zbuf)
!
!      write(id_file,'(a,a1)') '!', char(0)
!      write(id_file,'(a,a1)') '!  4.3  length of edge', char(0)
!      write(id_file,'(a,a1)') '!', char(0)
!
      call gz_read_scalar_in_element(FPz_f, nod_IO, sfed_IO, zbuf)
!
      end subroutine gz_read_edge_geometry
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_edge_geometry(FPz_f, nod_IO, sfed_IO, zbuf)
!
      use gz_node_geometry_IO
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%fixbuf(1) = hd_edge_point() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_geometry_info(FPz_f, nod_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_edge_dir() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_vector_in_element(FPz_f, nod_IO, sfed_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_edge_length() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_scalar_in_element(FPz_f, nod_IO, sfed_IO, zbuf)
!
      end subroutine gz_write_edge_geometry
!
!------------------------------------------------------------------
!
      end module gz_edge_data_IO
