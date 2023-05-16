!>@file  gz_surface_data_IO.f90
!!       module gz_surface_data_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in Aug., 2006
!
!> @brief routines for surface mesh data IO
!!
!!@verbatim
!!      subroutine gz_read_surface_connection(FPz_f, id_rank,           &
!!     &          comm_IO, ele_IO, sfed_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine gz_write_surface_connection                          &
!!     &         (FPz_f, id_rank, comm_IO, ele_IO, sfed_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(communication_table), intent(in) :: comm_IO
!!        type(element_data), intent(in) :: ele_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_read_surface_geometry                             &
!!     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!      subroutine gz_write_surface_geometry                            &
!!     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
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
      subroutine gz_read_surface_connection(FPz_f, id_rank,             &
     &          comm_IO, ele_IO, sfed_IO, zbuf, ierr)
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
!      write(id_file,'(a)') '!' 
!      write(id_file,'(a)') '!  surface connectivity '
!      write(id_file,'(a)') '!  and communication table '
!      write(id_file,'(a)') '!' 
!      write(id_file,'(a)', advance='NO') hd_fem_para()
!
      call gz_read_domain_info(FPz_f, id_rank, comm_IO, zbuf, ierr)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '!  2  surface connectivity'
!      write(id_file,'(a)') '!  2.1  surface connectivity '
!      write(id_file,'(a)') '!      (type and connection) '
!      write(id_file,'(a)') '!'
!
      call gz_read_number_of_element(FPz_f, ele_IO, zbuf)
      call gz_read_element_info(FPz_f, ele_IO, zbuf)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '!  2.2 surface id for each element'
!      write(id_file,'(a)') '!        positive: outward normal'
!      write(id_file,'(a)') '!        normal: inward normal'
!      write(id_file,'(a)') '!'
!
      call gz_read_surface_4_element(FPz_f, sfed_IO, zbuf)
!
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 3.import / export information '
!      write(id_file,'(a)') '! 3.1 surface ID for import '
!      write(id_file,'(a)') '!'
!
      call gz_read_import_data(FPz_f, comm_IO, zbuf)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 3.2 surface ID for export '
!      write(id_file,'(a)') '!'
!
      call gz_read_export_data(FPz_f, comm_IO, zbuf)
!
      end subroutine gz_read_surface_connection
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_surface_connection                            &
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
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%fixbuf(1) = hd_surf_para() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
      zbuf%fixbuf(1) = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_domain_info(FPz_f, id_rank, comm_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_surf_connect() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_element_info(FPz_f, ele_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_surf_on_ele() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_surface_4_element(FPz_f, sfed_IO, zbuf)
!
!
!
      zbuf%fixbuf(1) = hd_surf_import() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_import_data(FPz_f, comm_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_surf_export() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_export_data(FPz_f, comm_IO, zbuf)
!
      end subroutine gz_write_surface_connection
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_surface_geometry                               &
     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!
      use gz_node_geometry_IO
!
      character, pointer, intent(in) :: FPz_f
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 4.  geometry of surface'
!      write(id_file,'(a)') '! 4.1 center of surface'
!      write(id_file,'(a)') '!'
!
      call gz_read_number_of_node(FPz_f, nod_IO, zbuf)
      call gz_read_geometry_info(FPz_f, nod_IO, zbuf)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '!  4.2 normal vector of surface'
!      write(id_file,'(a)') '!'
!
      call gz_read_vector_in_element(FPz_f, nod_IO, sfed_IO, zbuf)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 4.3 area of surface'
!      write(id_file,'(a)') '!'
!
      call gz_read_scalar_in_element(FPz_f, nod_IO, sfed_IO, zbuf)
!
      end subroutine gz_read_surface_geometry
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_surface_geometry                              &
     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!
      use gz_node_geometry_IO
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%fixbuf(1) = hd_surf_point() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_geometry_info(FPz_f, nod_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_surf_norm() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_vector_in_element(FPz_f, nod_IO, sfed_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_surf_area() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_scalar_in_element(FPz_f, nod_IO, sfed_IO, zbuf)
!
      end subroutine gz_write_surface_geometry
!
!------------------------------------------------------------------
!
      end module gz_surface_data_IO
