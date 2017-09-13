!>@file  element_data_IO_b.f90
!!      module element_data_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine read_element_comm_table_b(my_rank_IO, comm_IO, ierr)
!!      subroutine write_element_comm_table_b(my_rank_IO, comm_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine read_element_geometry_b(nod_IO, sfed_IO)
!!      subroutine write_element_geometry_b(nod_IO, sfed_IO)
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module element_data_IO_b
!
      use m_precision
!
      use t_geometry_data
      use t_read_mesh_data
      use t_comm_table
      use t_surf_edge_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine read_element_comm_table_b                              &
     &         (my_rank_IO, comm_IO, ierr)
!
      use m_fem_mesh_labels
      use domain_data_IO_b
!
      integer (kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call read_domain_info_b(my_rank_IO, comm_IO, ierr)
      if(ierr .ne. 0) return
!
! ----  import & export 
!
      call read_import_data_b(comm_IO)
      call read_export_data_b(comm_IO)
!
      end subroutine read_element_comm_table_b
!
!------------------------------------------------------------------
!
      subroutine write_element_comm_table_b(my_rank_IO, comm_IO)
!
      use m_fem_mesh_labels
      use domain_data_IO_b
!
      integer (kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
!
!
      call write_domain_info_b(my_rank_IO, comm_IO)
!
! ----  import & export 
!
      call write_import_data_b(comm_IO)
      call write_export_data_b(comm_IO)
!
      end subroutine write_element_comm_table_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_element_geometry_b(nod_IO, sfed_IO)
!
      use node_geometry_IO_b
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call read_number_of_node_b(nod_IO)
      call read_geometry_info_b(nod_IO)
      call read_scalar_in_element_b(nod_IO, sfed_IO)
!
      end subroutine read_element_geometry_b
!
!------------------------------------------------------------------
!
      subroutine write_element_geometry_b(nod_IO, sfed_IO)
!
      use node_geometry_IO_b
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call write_geometry_info_b(nod_IO)
      call write_scalar_in_element_b( nod_IO, sfed_IO)
!
      end subroutine write_element_geometry_b
!
!------------------------------------------------------------------
!
      end module element_data_IO_b
