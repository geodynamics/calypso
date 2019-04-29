!>@file  gz_element_data_IO_b.f90
!!      module gz_element_data_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine gz_read_element_comm_table_b                         &
!!     &         (id_rank, bflag, comm_IO)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(communication_table), intent(inout) :: comm_IO
!!      subroutine gz_write_element_comm_table_b                        &
!!     &         (id_rank, comm_IO, bflag)
!!        type(communication_table), intent(in) :: comm_IO
!!
!!      subroutine gz_read_element_geometry_b(bflag, nod_IO, sfed_IO)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine gz_write_element_geometry_b(nod_IO, sfed_IO, bflag)
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!        type(binary_IO_flags), intent(inout) :: bflag
!!@endverbatim
!
      module gz_element_data_IO_b
!
      use m_precision
!
      use t_geometry_data
      use t_read_mesh_data
      use t_comm_table
      use t_surf_edge_IO
      use binary_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_read_element_comm_table_b                           &
     &         (id_rank, bflag, comm_IO)
!
      use gz_domain_data_IO_b
!
      integer, intent(in) :: id_rank
      type(binary_IO_flags), intent(inout) :: bflag
      type(communication_table), intent(inout) :: comm_IO
!
!
!      write(textbuf,'(a,a1)') '!' , char(0)
!      write(textbuf,'(a,a1)') '!  element position ', char(0)
!      write(textbuf,'(a,a1)') '!  and communication table ', char(0)
!      write(textbuf,'(a,a1)') '!' , char(0)
!      write(textbuf,'(a,a1)', advance='NO') hd_fem_para(), char(0)
!
      call gz_read_domain_info_b(id_rank, bflag, comm_IO)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(textbuf,'(a,a1)') '!', char(0)
!      write(textbuf,'(a,a1)') '! 2.import / export information ',      &
!     &                       char(0)
!      write(textbuf,'(a,a1)') '! 2.1 element ID for import ', char(0)
!      write(textbuf,'(a,a1)') '!', char(0)
!
      call gz_read_import_data_b(bflag, comm_IO)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(textbuf,'(a,a1)') '!', char(0)
!      write(textbuf,'(a,a1)') '! 2.2 element ID for export ', char(0)
!      write(textbuf,'(a,a1)') '! ', char(0)
!
      call gz_read_export_data_b(bflag, comm_IO)
!
      end subroutine gz_read_element_comm_table_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_element_comm_table_b                          &
     &         (id_rank, comm_IO, bflag)
!
      use gz_domain_data_IO_b
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
!      textbuf = hd_ecomm_para() // char(0)
!      textbuf = hd_fem_para() // char(0)
      call gz_write_domain_info_b(id_rank, comm_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      textbuf = hd_ecomm_import() // char(0)
      call gz_write_import_data_b(comm_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      textbuf = hd_ecomm_export() // char(0)
      call gz_write_export_data_b(comm_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_element_comm_table_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_element_geometry_b(bflag, nod_IO, sfed_IO)
!
      use gz_node_geometry_IO_b
!
      type(binary_IO_flags), intent(inout) :: bflag
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
      call gz_read_number_of_node_b(bflag, nod_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_geometry_info_b(bflag, nod_IO)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(textbuf,'(a,a1)') '!', char(0)
!      write(textbuf,'(a,a1)') '! 3.2 Volume of element ', char(0)
!      write(textbuf,'(a,a1)') '!', char(0)
!
      call gz_read_scalar_in_element_b(bflag, nod_IO, sfed_IO)
!
      end subroutine gz_read_element_geometry_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_element_geometry_b(nod_IO, sfed_IO, bflag)
!
      use gz_node_geometry_IO_b
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!      textbuf = hd_ecomm_point() // char(0)
      call gz_write_geometry_info_b(nod_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      textbuf = hd_ecomm_vol() // char(0)
      call gz_write_scalar_in_element_b(nod_IO, sfed_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_element_geometry_b
!
!------------------------------------------------------------------
!
      end module gz_element_data_IO_b
