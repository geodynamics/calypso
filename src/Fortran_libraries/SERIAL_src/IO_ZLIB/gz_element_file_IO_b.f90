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
      type(buffer_4_gzip), private :: zbuf_ele
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
      use gz_element_data_IO_b
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
      call open_rd_gzfile_b(file_name, id_rank, zbuf_ele)
      if(zbuf_ele%ierr_zlib .ne. 0) go to 99
      call gz_read_element_comm_table_b                                 &
     &   (id_rank, zbuf_ele, ele_mesh_IO%comm)
      if(zbuf_ele%ierr_zlib .ne. 0) go to 99
!      call gz_read_element_geometry_b                                  &
!     &   (zbuf_ele, ele_mesh_IO%node, ele_mesh_IO%sfed)
!
  99  continue
      call close_gzfile_b
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
      call open_rd_gzfile_b(file_name, id_rank, zbuf_ele)
      if(zbuf_ele%ierr_zlib .ne. 0) go to 99
      call gz_read_surface_connection_b(id_rank, zbuf_ele,              &
     &    surf_mesh_IO%comm, surf_mesh_IO%ele, surf_mesh_IO%sfed)
      if(zbuf_ele%ierr_zlib .ne. 0) go to 99
!      call gz_read_surface_geometry_b                                  &
!     &   (zbuf_ele, surf_mesh_IO%node, surf_mesh_IO%sfed)
!
  99  continue
      call close_gzfile_b
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
      call open_rd_gzfile_b(file_name, id_rank, zbuf_ele)
      if(zbuf_ele%ierr_zlib .ne. 0) go to 99
      call gz_read_edge_connection_b(id_rank, zbuf_ele,                 &
     &    edge_mesh_IO%comm, edge_mesh_IO%ele, edge_mesh_IO%sfed)
      if(zbuf_ele%ierr_zlib .ne. 0) go to 99
!      call gz_read_edge_geometry_b                                     &
!     &   (zbuf_ele, edge_mesh_IO%node, edge_mesh_IO%sfed)
!
  99  continue
      call close_gzfile_b
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
      use gz_element_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(in) :: ele_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped binary element comm file: ', trim(file_name)
!
      call open_wt_gzfile_b(file_name, zbuf_ele)
      call gz_write_element_comm_table_b                                &
     &   (id_rank, ele_mesh_IO%comm, zbuf_ele)
!      call gz_write_element_geometry_b                                 &
!     &   (ele_mesh_IO%node, ele_mesh_IO%sfed, zbuf_ele)
      call close_gzfile_b
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
      call open_wt_gzfile_b(file_name, zbuf_ele)
      call gz_write_surface_connection_b(id_rank, surf_mesh_IO%comm,    &
     &    surf_mesh_IO%ele, surf_mesh_IO%sfed, zbuf_ele)
!      call gz_write_surface_geometry_b                                 &
!     &   (surf_mesh_IO%node, surf_mesh_IO%sfed, zbuf_ele)
      call close_gzfile_b
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
      call open_wt_gzfile_b(file_name, zbuf_ele)
      call gz_write_edge_connection_b(id_rank, edge_mesh_IO%comm,       &
     &    edge_mesh_IO%ele, edge_mesh_IO%sfed, zbuf_ele)
!      call gz_write_edge_geometry_b                                    &
!     &   (edge_mesh_IO%node, edge_mesh_IO%sfed, zbuf_ele)
      call close_gzfile_b
!
      end subroutine gz_output_edge_file_b
!
!------------------------------------------------------------------
!
      end module gz_element_file_IO_b
