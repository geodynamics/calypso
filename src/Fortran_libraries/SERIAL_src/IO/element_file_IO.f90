!>@file  element_file_IO.f90
!!      module element_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine input_element_file                                   &
!!     &         (id_rank, file_name, ele_mesh_IO, ierr)
!!      subroutine input_surface_file                                   &
!!     &         (id_rank, file_name, surf_mesh_IO, ierr)
!!      subroutine input_edge_file                                      &
!!     &         (id_rank, file_name, edge_mesh_IO, ierr)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!
!!      subroutine output_element_file                                  &
!!     &         (id_rank, file_name, ele_mesh_IO)
!!      subroutine output_surface_file                                  &
!!     &         (id_rank, file_name, surf_mesh_IO)
!!      subroutine output_edge_file                                     &
!!     &         (id_rank, file_name, edge_mesh_IO)
!!        type(surf_edge_IO_file), intent(in) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(in) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(in) :: edge_mesh_IO
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module element_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_read_mesh_data
      use set_mesh_file_names
!
      implicit none
!
!   mesh file code
      integer(kind = kint), parameter ::  input_file_code = 14
!
      private :: input_file_code
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine input_element_file                                     &
     &         (id_rank, file_name, ele_mesh_IO, ierr)
!
      use element_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read ascii element comm file: ', trim(file_name)
!
      open(input_file_code, file = file_name, form = 'formatted')
      call read_element_comm_table                                      &
     &   (input_file_code, id_rank, ele_mesh_IO%comm, ierr)
!      call read_element_geometry(input_file_code,                      &
!     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      close(input_file_code)
!
      end subroutine input_element_file
!
!------------------------------------------------------------------
!
      subroutine input_surface_file                                     &
     &         (id_rank, file_name, surf_mesh_IO, ierr)
!
      use surface_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read ascii surface mesh file: ', trim(file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
      call read_surface_connection                                      &
     &  (input_file_code, id_rank, surf_mesh_IO%comm,                   &
     &   surf_mesh_IO%ele, surf_mesh_IO%sfed, ierr)
!      call read_surface_geometry(input_file_code,                      &
!     &    surf_mesh_IO%node, surf_mesh_IO%sfed)
      close (input_file_code)
!
      end subroutine input_surface_file
!
!------------------------------------------------------------------
!
      subroutine input_edge_file                                        &
     &         (id_rank, file_name, edge_mesh_IO, ierr)
!
      use edge_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read ascii edge mesh file: ', trim(file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
      call read_edge_connection                                         &
     &   (input_file_code, id_rank, edge_mesh_IO%comm,                  &
     &    edge_mesh_IO%ele, edge_mesh_IO%sfed, ierr)
!      call read_edge_geometry(input_file_code,                         &
!     &    edge_mesh_IO%node, edge_mesh_IO%sfed)
      close (input_file_code)
!
      end subroutine input_edge_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_element_file                                    &
     &         (id_rank, file_name, ele_mesh_IO)
!
      use element_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(in) :: ele_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write ascii element comm file: ', trim(file_name)
!
      open(input_file_code, file = file_name, form = 'formatted')
      call write_element_comm_table                                     &
     &   (input_file_code, id_rank, ele_mesh_IO%comm)
!      call write_element_geometry(input_file_code,                     &
!     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      close(input_file_code)
!
      end subroutine output_element_file
!
!------------------------------------------------------------------
!
      subroutine output_surface_file                                    &
     &         (id_rank, file_name, surf_mesh_IO)
!
      use surface_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(in) :: surf_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write ascii surface mesh file: ', trim(file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
      call write_surface_connection                                     &
     &  (input_file_code, id_rank, surf_mesh_IO%comm,                   &
     &   surf_mesh_IO%ele, surf_mesh_IO%sfed)
!      call write_surface_geometry(input_file_code,                     &
!     &    surf_mesh_IO%node, surf_mesh_IO%sfed)
      close (input_file_code)
!
      end subroutine output_surface_file
!
!------------------------------------------------------------------
!
      subroutine output_edge_file                                       &
     &         (id_rank, file_name, edge_mesh_IO)
!
      use edge_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(in) :: edge_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write ascii edge mesh file: ', trim(file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
      call write_edge_connection                                        &
     &  (input_file_code, id_rank, edge_mesh_IO%comm,                   &
     &   edge_mesh_IO%ele, edge_mesh_IO%sfed)
!      call write_edge_geometry(input_file_code,                        &
!     &   edge_mesh_IO%node, edge_mesh_IO%sfed)
      close(input_file_code)
!
      end subroutine output_edge_file
!
!------------------------------------------------------------------
!
      end module element_file_IO
