!>@file  gz_element_data_IO.f90
!!      module gz_element_data_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine gz_read_element_comm_table(my_rank_IO, comm_IO, ierr)
!!      subroutine gz_write_element_comm_table(my_rank_IO, comm_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine gz_read_element_geometry(nod_IO, sfed_IO)
!!      subroutine gz_write_element_geometry(nod_IO, sfed_IO)
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
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
      subroutine gz_read_element_comm_table(my_rank_IO, comm_IO, ierr)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO
!
      integer (kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!      write(textbuf,'(a,a1)') '!' , char(0)
!      write(textbuf,'(a,a1)') '!  element position ', char(0)
!      write(textbuf,'(a,a1)') '!  and communication table ', char(0)
!      write(textbuf,'(a,a1)') '!' , char(0)
!      write(textbuf,'(a,a1)', advance='NO') hd_fem_para(), char(0)
!
      call gz_read_domain_info(my_rank_IO, comm_IO, ierr)
      if(ierr .ne. 0) return
!
!      write(textbuf,'(a,a1)') '!', char(0)
!      write(textbuf,'(a,a1)') '! 2.import / export information ',      &
!     &                       char(0)
!      write(textbuf,'(a,a1)') '! 2.1 element ID for import ', char(0)
!      write(textbuf,'(a,a1)') '!', char(0)
!
      call gz_read_import_data(comm_IO)
!
!      write(textbuf,'(a,a1)') '!', char(0)
!      write(textbuf,'(a,a1)') '! 2.2 element ID for export ', char(0)
!      write(textbuf,'(a,a1)') '! ', char(0)
!
      call gz_read_export_data(comm_IO)
!
      end subroutine gz_read_element_comm_table
!
!------------------------------------------------------------------
!
      subroutine gz_write_element_comm_table(my_rank_IO, comm_IO)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO
!
      integer (kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
!
!
      textbuf = hd_ecomm_para() // char(0)
      call gz_write_textbuf_no_lf
      textbuf = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_domain_info(my_rank_IO, comm_IO)
!
      textbuf = hd_ecomm_import() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_import_data(comm_IO)
!
      textbuf = hd_ecomm_export() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_export_data(comm_IO)
!
      end subroutine gz_write_element_comm_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_element_geometry(nod_IO, sfed_IO)
!
      use gz_node_geometry_IO
!
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
      call gz_read_number_of_node(nod_IO)
      call gz_read_geometry_info(nod_IO)
!
!      write(textbuf,'(a,a1)') '!', char(0)
!      write(textbuf,'(a,a1)') '! 3.2 Volume of element ', char(0)
!      write(textbuf,'(a,a1)') '!', char(0)
!
      call gz_read_scalar_in_element(nod_IO, sfed_IO)
!
      end subroutine gz_read_element_geometry
!
!------------------------------------------------------------------
!
      subroutine gz_write_element_geometry(nod_IO, sfed_IO)
!
      use gz_node_geometry_IO
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      textbuf = hd_ecomm_point() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_geometry_info(nod_IO)
!
      textbuf = hd_ecomm_vol() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_scalar_in_element(nod_IO, sfed_IO)
!
      end subroutine gz_write_element_geometry
!
!------------------------------------------------------------------
!
      end module gz_element_data_IO
