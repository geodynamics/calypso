!>@file  gz_element_file_IO_b.f90
!!      module gz_element_file_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine gz_input_element_file_b                              &
!!     &         (id_rank, file_name, ele_mesh_IO, ierr)
!!      subroutine gz_input_surface_file_b                              &
!!     &         (id_rank, file_name, surf_mesh_IO, ierr)
!!      subroutine gz_input_edge_file_b                                 &
!!     &         (id_rank, file_name, edge_mesh_IO, ierr)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!
!!      subroutine gz_output_element_file_b                             &
!!     &         (id_rank, ele_mesh_IO)
!!      subroutine gz_output_surface_file_b                             &
!!     &         (id_rank, file_name, surf_mesh_IO)
!!      subroutine gz_output_edge_file_b                                &
!!     &         (id_rank, file_name, edge_mesh_IO)
!!        type(surf_edge_IO_file), intent(in) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(in) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(in) :: edge_mesh_IO
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module gz_element_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_read_mesh_data
      use t_buffer_4_gzip
      use set_mesh_file_names
      use binary_IO
!
      implicit none
!
      type(buffer_4_gzip), private, save :: zbuf_ele
      character, pointer, private, save :: FPz_ele
!
      private :: gz_read_element_geometry_b
      private :: gz_write_element_geometry_b
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_input_element_file_b                                &
     &         (id_rank, file_name, ele_mesh_IO, ierr)
!
      use gzip_file_access
      use gz_comm_table_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read gzipped binary element comm file: ', trim(file_name)
!
      call open_rd_gzfile_b(FPz_ele, file_name, id_rank, zbuf_ele)
      if(zbuf_ele%ierr_zlib .ne. 0) go to 99
      call gz_read_comm_table_b(FPz_ele, id_rank,                       &
     &                          zbuf_ele, ele_mesh_IO%comm)
      if(zbuf_ele%ierr_zlib .ne. 0) go to 99
!      call gz_read_element_geometry_b                                  &
!     &   (FPz_ele, zbuf_ele, ele_mesh_IO%node, ele_mesh_IO%sfed)
!
  99  continue
      call close_gzfile_b(FPz_ele)
      ierr = zbuf_ele%ierr_zlib
!
      end subroutine gz_input_element_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_input_surface_file_b                                &
     &         (id_rank, file_name, surf_mesh_IO, ierr)
!
      use gzip_file_access
      use gz_surface_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read gzipped binary surface mesh file: ', trim(file_name)
!
      call open_rd_gzfile_b(FPz_ele, file_name, id_rank, zbuf_ele)
      if(zbuf_ele%ierr_zlib .ne. 0) go to 99
      call gz_read_surface_connection_b(FPz_ele, id_rank, zbuf_ele,     &
     &    surf_mesh_IO%comm, surf_mesh_IO%ele, surf_mesh_IO%sfed)
      if(zbuf_ele%ierr_zlib .ne. 0) go to 99
!      call gz_read_surface_geometry_b                                  &
!     &   (FPz_ele, zbuf_ele, surf_mesh_IO%node, surf_mesh_IO%sfed)
!
  99  continue
      call close_gzfile_b(FPz_ele)
      ierr = zbuf_ele%ierr_zlib
!
      end subroutine gz_input_surface_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_input_edge_file_b                                   &
     &         (id_rank, file_name, edge_mesh_IO, ierr)
!
      use gzip_file_access
      use gz_edge_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read gzipped binary edge mesh file: ', trim(file_name)
!
      call open_rd_gzfile_b(FPz_ele, file_name, id_rank, zbuf_ele)
      if(zbuf_ele%ierr_zlib .ne. 0) go to 99
      call gz_read_edge_connection_b(FPz_ele, id_rank, zbuf_ele,        &
     &    edge_mesh_IO%comm, edge_mesh_IO%ele, edge_mesh_IO%sfed)
      if(zbuf_ele%ierr_zlib .ne. 0) go to 99
!      call gz_read_edge_geometry_b                                     &
!     &   (FPz_ele, zbuf_ele, edge_mesh_IO%node, edge_mesh_IO%sfed)
!
  99  continue
      call close_gzfile_b(FPz_ele)
      ierr = zbuf_ele%ierr_zlib
!
      end subroutine gz_input_edge_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_output_element_file_b                               &
     &         (id_rank, file_name, ele_mesh_IO)
!
      use gzip_file_access
      use gz_comm_table_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(in) :: ele_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped binary element comm file: ', trim(file_name)
!
      call open_wt_gzfile_b(FPz_ele, file_name, zbuf_ele)
      call gz_write_comm_table_b(FPz_ele, id_rank,                      &
     &                           ele_mesh_IO%comm, zbuf_ele)
!      call gz_write_element_geometry_b                                 &
!     &   (FPz_ele, ele_mesh_IO%node, ele_mesh_IO%sfed, zbuf_ele)
      call close_gzfile_b(FPz_ele)
!
      end subroutine gz_output_element_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_output_surface_file_b                               &
     &         (id_rank, file_name, surf_mesh_IO)
!
      use gzip_file_access
      use gz_surface_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(in) :: surf_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped binary surface mesh file: ', trim(file_name)
!
      call open_wt_gzfile_b(FPz_ele, file_name, zbuf_ele)
      call gz_write_surface_connection_b                                &
     &   (FPz_ele, id_rank, surf_mesh_IO%comm,                          &
     &    surf_mesh_IO%ele, surf_mesh_IO%sfed, zbuf_ele)
!      call gz_write_surface_geometry_b                                 &
!     &   (FPz_ele, surf_mesh_IO%node, surf_mesh_IO%sfed, zbuf_ele)
      call close_gzfile_b(FPz_ele)
!
      end subroutine gz_output_surface_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_output_edge_file_b                                  &
     &         (id_rank, file_name, edge_mesh_IO)
!
      use gzip_file_access
      use gz_edge_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(in) :: edge_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped binary edge mesh file: ', trim(file_name)
!
      call open_wt_gzfile_b(FPz_ele, file_name, zbuf_ele)
      call gz_write_edge_connection_b                                   &
     &   (FPz_ele, id_rank, edge_mesh_IO%comm,                          &
     &    edge_mesh_IO%ele, edge_mesh_IO%sfed, zbuf_ele)
!      call gz_write_edge_geometry_b                                    &
!     &   (FPz_ele, edge_mesh_IO%node, edge_mesh_IO%sfed, zbuf_ele)
      call close_gzfile_b(FPz_ele)
!
      end subroutine gz_output_edge_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_element_geometry_b                             &
     &         (FPz_f, zbuf, nod_IO, sfed_IO)
!
      use gz_node_geometry_IO_b
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
!      write(textbuf,'(a,a1)') '!', char(0)
!      write(textbuf,'(a,a1)') '! 3.element information', char(0)
!      write(textbuf,'(a,a1)') '!', char(0)
!      write(textbuf,'(a,a1)') '! 3.1 center of element (position) ',   &
!     &                       char(0)
!      write(textbuf,'(a,a1)') '!', char(0)
!
      call gz_read_geometry_info_b(FPz_f, zbuf, nod_IO)
      if(zbuf%ierr_zlib .ne. 0) return
!
!      write(textbuf,'(a,a1)') '!', char(0)
!      write(textbuf,'(a,a1)') '! 3.2 Volume of element ', char(0)
!      write(textbuf,'(a,a1)') '!', char(0)
!
      call gz_read_scalar_in_element_b(FPz_f, zbuf, nod_IO, sfed_IO)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_read_element_geometry_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_element_geometry_b                            &
     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!
      use gz_node_geometry_IO_b
!
      character, pointer, intent(in) :: FPz_f
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!      textbuf = hd_ecomm_point() // char(0)
      call gz_write_geometry_info_b(FPz_f, nod_IO, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
!      textbuf = hd_ecomm_vol() // char(0)
      call gz_write_scalar_in_element_b(FPz_f, nod_IO, sfed_IO, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_write_element_geometry_b
!
!------------------------------------------------------------------
!
      end module gz_element_file_IO_b
