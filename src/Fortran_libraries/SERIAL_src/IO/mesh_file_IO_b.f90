!>@file   mesh_file_IO_b.f90
!!@brief  module mesh_file_IO_b
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Mesh file IO for binary format
!!
!!@verbatim
!!      subroutine read_mesh_file_b(my_rank_IO, file_name, fem_IO, ierr)
!!        type(mesh_data), intent(inout) :: fem_IO
!!
!!      subroutine read_mesh_geometry_b                                 &
!!     &         (my_rank_IO, file_name, mesh_IO, ierr)
!!      subroutine read_node_size_b                                     &
!!     &         (my_rank_IO, file_name, mesh_IO, ierr)
!!      subroutine read_geometry_size_b                                 &
!!     &         (my_rank_IO, file_name, mesh_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine write_mesh_file_b(my_rank_IO, file_name, fem_IO)
!!        type(mesh_data), intent(inout) :: fem_IO
!!@endverbatim
!
      module mesh_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use binary_IO
      use mesh_data_IO_b
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_mesh_file_b(my_rank_IO, file_name, fem_IO, ierr)
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_data), intent(inout) :: fem_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read binary mesh file: ', trim(file_name)
!
      call open_read_binary_file(file_name, my_rank_IO)
!
      call read_geometry_data_b(my_rank_IO, fem_IO%mesh, ierr)
      call read_mesh_groups_b(fem_IO%group)
!
      call close_binary_file
!
      end subroutine read_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine read_mesh_geometry_b                                   &
     &         (my_rank_IO, file_name, mesh_IO, ierr)
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read binary mesh file: ', trim(file_name)
!
      call open_read_binary_file(file_name, my_rank_IO)
      call read_geometry_data_b(my_rank_IO, mesh_IO, ierr)
      call close_binary_file
!
      end subroutine read_mesh_geometry_b
!
!  ---------------------------------------------------------------------
!
      subroutine read_node_size_b                                       &
     &         (my_rank_IO, file_name, mesh_IO, ierr)
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read binary mesh file: ', trim(file_name)
!
      call open_read_binary_file(file_name, my_rank_IO)
      call read_num_node_b(my_rank_IO, mesh_IO, ierr)
      call close_binary_file
!
      end subroutine read_node_size_b
!
!------------------------------------------------------------------
!
      subroutine read_geometry_size_b                                   &
     &         (my_rank_IO, file_name, mesh_IO, ierr)
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read binary mesh file: ', trim(file_name)
!
      call open_read_binary_file(file_name, my_rank_IO)
      call read_num_node_ele_b(my_rank_IO, mesh_IO, ierr)
      call close_binary_file
!
      end subroutine read_geometry_size_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_mesh_file_b(my_rank_IO, file_name, fem_IO)
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_data), intent(inout) :: fem_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Write binary mesh file: ', trim(file_name)
!
      call open_write_binary_file(file_name)
      call write_geometry_data_b(my_rank_IO, fem_IO%mesh)
      call write_mesh_groups_b(fem_IO%group)
      call close_binary_file
!
      end subroutine write_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      end module mesh_file_IO_b
