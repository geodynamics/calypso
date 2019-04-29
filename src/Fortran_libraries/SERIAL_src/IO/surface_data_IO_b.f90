!>@file  surface_data_IO_b.f90
!!       module surface_data_IO_b
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in Aug., 2006
!
!> @brief routines for surface mesh data IO
!!
!!@verbatim
!!      subroutine read_surface_connection_b                            &
!!     &         (id_rank, bflag, comm_IO, ele_IO, sfed_IO)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine write_surface_connection_b                           &
!!     &         (id_rank, comm_IO, ele_IO, sfed_IO, bflag)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(communication_table), intent(in) :: comm_IO
!!        type(element_data), intent(in) :: ele_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!
!!      subroutine read_surface_geometry_b(bflag, nod_IO, sfed_IO)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine write_surface_geometry_b(nod_IO, sfed_IO, bflag)
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!@endverbatim
!
      module surface_data_IO_b
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_read_mesh_data
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
      subroutine read_surface_connection_b                              &
     &         (id_rank, bflag, comm_IO, ele_IO, sfed_IO)
!
      use m_fem_mesh_labels
      use domain_data_IO_b
      use element_connect_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(communication_table), intent(inout) :: comm_IO
      type(element_data), intent(inout) :: ele_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call read_domain_info_b(id_rank, bflag, comm_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      call read_number_of_element_b(bflag, ele_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      call read_element_info_b(bflag, ele_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      call read_surface_4_element_b(bflag, sfed_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      call read_import_data_b(bflag, comm_IO)
      if(bflag%ierr_IO .ne. 0) return
      call read_export_data_b(bflag, comm_IO)
!
      end subroutine read_surface_connection_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surface_connection_b                             &
     &         (id_rank, comm_IO, ele_IO, sfed_IO, bflag)
!
      use m_fem_mesh_labels
      use domain_data_IO_b
      use element_connect_IO_b
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(element_data), intent(in) :: ele_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call write_domain_info_b(id_rank, comm_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call write_element_info_b(ele_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_surface_4_element_b(sfed_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call write_import_data_b(comm_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_export_data_b(comm_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_surface_connection_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_surface_geometry_b(bflag, nod_IO, sfed_IO)
!
      use node_geometry_IO_b
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call read_number_of_node_b(bflag, nod_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      call read_geometry_info_b(bflag, nod_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      call read_vector_in_element_b(bflag, nod_IO, sfed_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      call read_scalar_in_element_b(bflag, nod_IO, sfed_IO)
!
      end subroutine read_surface_geometry_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surface_geometry_b(nod_IO, sfed_IO, bflag)
!
      use node_geometry_IO_b
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call write_geometry_info_b(nod_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_vector_in_element_b(nod_IO, sfed_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_scalar_in_element_b(nod_IO, sfed_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_surface_geometry_b
!
!------------------------------------------------------------------
!
      end module surface_data_IO_b
