!>@file  element_file_IO_b.f90
!!      module element_file_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine input_element_file_b                                 &
!!     &         (id_rank, file_name, ele_mesh_IO, ierr)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!      subroutine input_surface_file_b                                 &
!!     &         (id_rank, file_name, surf_mesh_IO, ierr)
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!      subroutine input_edge_file_b                                    &
!!     &         (id_rank, file_name, edge_mesh_IO, ierr)
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!
!!      subroutine output_element_file_b                                &
!!     &         (id_rank, file_name, ele_mesh_IO, ierr)
!!      subroutine output_surface_file_b                                &
!!     &         (id_rank, file_name, surf_mesh_IO, ierr)
!!      subroutine output_edge_file_b                                   &
!!     &         (id_rank, file_name, edge_mesh_IO, ierr)
!!        type(surf_edge_IO_file), intent(in) :: edge_mesh_IO
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module element_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_read_mesh_data
      use set_mesh_file_names
      use binary_IO
!
      implicit none
!
      type(binary_IO_flags), private :: bin_emeshflags
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine input_element_file_b                                   &
     &         (id_rank, file_name, ele_mesh_IO, ierr)
!
      use element_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read binary element comm file: ', trim(file_name)
!
      call open_read_binary_file(file_name, id_rank, bin_emeshflags)
      call read_element_comm_table_b                                    &
     &   (id_rank, bin_emeshflags, ele_mesh_IO%comm)
!      call read_element_geometry_b                                     &
!     &   (bin_emeshflags, ele_mesh_IO%node, ele_mesh_IO%sfed)
      call close_binary_file
      ierr = bin_emeshflags%ierr_IO
!
      end subroutine input_element_file_b
!
!------------------------------------------------------------------
!
      subroutine input_surface_file_b                                   &
     &         (id_rank, file_name, surf_mesh_IO, ierr)
!
      use surface_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read binary surface mesh file: ', trim(file_name)
!
      call open_read_binary_file(file_name, id_rank, bin_emeshflags)
      call read_surface_connection_b(id_rank, bin_emeshflags,           &
     &    surf_mesh_IO%comm, surf_mesh_IO%ele, surf_mesh_IO%sfed)
!      call read_surface_geometry_b                                     &
!     &   (bin_emeshflags, surf_mesh_IO%node, surf_mesh_IO%sfed)
      call close_binary_file
      ierr = bin_emeshflags%ierr_IO
!
      end subroutine input_surface_file_b
!
!------------------------------------------------------------------
!
      subroutine input_edge_file_b                                      &
     &         (id_rank, file_name, edge_mesh_IO, ierr)
!
      use edge_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read binary edge mesh file: ', trim(file_name)
!
      call open_read_binary_file(file_name, id_rank, bin_emeshflags)
      call read_edge_connection_b(id_rank, bin_emeshflags,              &
     &    edge_mesh_IO%comm, edge_mesh_IO%ele, edge_mesh_IO%sfed)
!      call read_edge_geometry_b                                        &
!     &    (bin_emeshflags, edge_mesh_IO%node, edge_mesh_IO%sfed)
      call close_binary_file
      ierr = bin_emeshflags%ierr_IO
!
      end subroutine input_edge_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_element_file_b                                  &
     &         (id_rank, file_name, ele_mesh_IO, ierr)
!
      use element_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(in) :: ele_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write binary element comm file: ', trim(file_name)
!
      call open_write_binary_file(file_name, bin_emeshflags)
      if(bin_emeshflags%ierr_IO .ne. 0) ierr = ierr_file
      call write_element_comm_table_b                                   &
     &   (id_rank, ele_mesh_IO%comm, bin_emeshflags)
      if(bin_emeshflags%ierr_IO .ne. 0) ierr = ierr_file
!      call write_element_geometry_b                                    &
!     &   (ele_mesh_IO%node, ele_mesh_IO%sfed, bin_emeshflags)
!      if(bin_emeshflags%ierr_IO .ne. 0) ierr = ierr_file
      call close_binary_file
!
      end subroutine output_element_file_b
!
!------------------------------------------------------------------
!
      subroutine output_surface_file_b                                  &
     &         (id_rank, file_name, surf_mesh_IO, ierr)
!
      use surface_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(in) :: surf_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write binary surface mesh file: ', trim(file_name)
!
      call open_write_binary_file(file_name, bin_emeshflags)
      if(bin_emeshflags%ierr_IO .ne. 0) ierr = ierr_file
      call write_surface_connection_b(id_rank, surf_mesh_IO%comm,       &
     &    surf_mesh_IO%ele, surf_mesh_IO%sfed, bin_emeshflags)
      if(bin_emeshflags%ierr_IO .ne. 0) ierr = ierr_file
!      call write_surface_geometry_b                                    &
!     &   (surf_mesh_IO%node, surf_mesh_IO%sfed, bin_flags)
!      if(bin_emeshflags%ierr_IO .ne. 0) ierr = ierr_file
      call close_binary_file
!
      end subroutine output_surface_file_b
!
!------------------------------------------------------------------
!
      subroutine output_edge_file_b                                     &
     &         (id_rank, file_name, edge_mesh_IO, ierr)
!
      use edge_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(in) :: edge_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write binary edge mesh file: ', trim(file_name)
!
      call open_write_binary_file(file_name, bin_emeshflags)
      if(bin_emeshflags%ierr_IO .ne. 0) ierr = ierr_file
      call write_edge_connection_b(id_rank, edge_mesh_IO%comm,          &
     &    edge_mesh_IO%ele, edge_mesh_IO%sfed, bin_emeshflags)
      if(bin_emeshflags%ierr_IO .ne. 0) ierr = ierr_file
!      call write_edge_geometry_b                                       &
!     &   (edge_mesh_IO%node, edge_mesh_IO%sfed, bin_emeshflags)
!      if(bin_emeshflags%ierr_IO .ne. 0) ierr = ierr_file
      call close_binary_file
!
      end subroutine output_edge_file_b
!
!------------------------------------------------------------------
!
      end module element_file_IO_b
