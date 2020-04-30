!>@file  gz_element_data_IO.f90
!!      module gz_element_data_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine gz_read_element_comm_table                           &
!!     &         (id_rank, comm_IO, zbuf, ierr)
!!      subroutine gz_write_element_comm_table(id_rank, comm_IO, zbuf)
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_read_element_geometry(nod_IO, sfed_IO, zbuf)
!!      subroutine gz_write_element_geometry(nod_IO, sfed_IO, zbuf)
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module gz_element_data_IO
!
      use m_precision
!
      use t_geometry_data
      use t_read_mesh_data
      use t_comm_table
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
      subroutine gz_read_element_comm_table                             &
     &         (id_rank, comm_IO, zbuf, ierr)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(inout) :: comm_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
!      write(zbuf%fixbuf(1),'(a,a1)', advance='NO')                     &
!     &                                   hd_fem_para(), char(0)
      call gz_read_domain_info(id_rank, comm_IO, zbuf, ierr)
      if(ierr .ne. 0) return
!
!      write(zbuf%fixbuf(1),'(a,a1)')                                   &
!     &                  '! 2.1 element ID for import ', char(0)
      call gz_read_import_data(comm_IO, zbuf)
!
!      write(zbuf%fixbuf(1),'(a,a1)')                                   &
!     &                  '! 2.2 element ID for export ', char(0)
      call gz_read_export_data(comm_IO, zbuf)
!
      end subroutine gz_read_element_comm_table
!
!------------------------------------------------------------------
!
      subroutine gz_write_element_comm_table(id_rank, comm_IO, zbuf)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO
      use gzip_file_access
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%fixbuf(1) = hd_ecomm_para() // char(0)
      call gz_write_textbuf_no_lf(zbuf)
      zbuf%fixbuf(1) = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call gz_write_domain_info(id_rank, comm_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_ecomm_import() // char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call gz_write_import_data(comm_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_ecomm_export() // char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call gz_write_export_data(comm_IO, zbuf)
!
      end subroutine gz_write_element_comm_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_element_geometry(nod_IO, sfed_IO, zbuf)
!
      use gz_node_geometry_IO
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
!      write(zbuf%fixbuf(1),'(a,a1)') '! 3.element information',        &
!     &                                char(0)
      call gz_read_number_of_node(nod_IO, zbuf)
      call gz_read_geometry_info(nod_IO, zbuf)
!
!      write(zbuf%fixbuf(1),'(a,a1)') '! 3.2 Volume of element ',       &
!     &                               char(0)
!
      call gz_read_scalar_in_element(nod_IO, sfed_IO, zbuf)
!
      end subroutine gz_read_element_geometry
!
!------------------------------------------------------------------
!
      subroutine gz_write_element_geometry(nod_IO, sfed_IO, zbuf)
!
      use gz_node_geometry_IO
      use gzip_file_access
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%fixbuf(1) = hd_ecomm_point() // char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call gz_write_geometry_info(nod_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_ecomm_vol() // char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call gz_write_scalar_in_element(nod_IO, sfed_IO, zbuf)
!
      end subroutine gz_write_element_geometry
!
!------------------------------------------------------------------
!
      end module gz_element_data_IO
