!>@file   mesh_file_IO_b.f90
!!@brief  module mesh_file_IO_b
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Mesh file IO for binary format
!!
!!@verbatim
!!      subroutine read_mesh_file_b                                     &
!!     &         (id_rank, file_name, mesh_IO, group_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   group_IO
!!
!!      subroutine read_mesh_geometry_b                                 &
!!     &         (id_rank, file_name, mesh_IO, ierr)
!!      subroutine read_node_size_b                                     &
!!     &         (id_rank, file_name, mesh_IO, ierr)
!!      subroutine read_geometry_size_b                                 &
!!     &         (id_rank, file_name, mesh_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine write_mesh_file_b                                    &
!!     &         (id_rank, file_name, mesh_IO, group_IO, ierr)
!!        type(mesh_geometry), intent(in) :: mesh_IO
!!        type(mesh_groups), intent(in) ::   group_IO
!!@endverbatim
!
      module mesh_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_binary_IO_buffer
      use binary_IO
      use mesh_data_IO_b
!
      implicit none
!
      integer(kind = kint), parameter :: id_read_mesh =  21
      integer(kind = kint), parameter :: id_write_mesh = 22
      type(binary_IO_buffer) :: bbuf_mesh
      private :: id_read_mesh, id_write_mesh, bbuf_mesh
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_mesh_file_b                                       &
     &         (id_rank, file_name, mesh_IO, group_IO, ierr)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      type(mesh_groups), intent(inout) ::   group_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary mesh file: ', trim(file_name)
!
      bbuf_mesh%id_binary = id_read_mesh
      call open_read_binary_file(file_name, id_rank, bbuf_mesh)
      if(bbuf_mesh%ierr_bin .ne. 0) goto 99
!
      call read_geometry_data_b(id_rank, bbuf_mesh, mesh_IO)
      if(bbuf_mesh%ierr_bin .ne. 0) go to 99
!
      call read_mesh_groups_b(bbuf_mesh, group_IO)
!
  99  continue
      call close_binary_file(bbuf_mesh)
      ierr = bbuf_mesh%ierr_bin
!
      end subroutine read_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine read_mesh_geometry_b                                   &
     &         (id_rank, file_name, mesh_IO, ierr)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary mesh file: ', trim(file_name)
!
      bbuf_mesh%id_binary = id_read_mesh
      call open_read_binary_file(file_name, id_rank, bbuf_mesh)
      if(bbuf_mesh%ierr_bin .ne. 0) goto 99
      call read_geometry_data_b(id_rank, bbuf_mesh, mesh_IO)
!
  99  continue
      call close_binary_file(bbuf_mesh)
      ierr = bbuf_mesh%ierr_bin
!
      end subroutine read_mesh_geometry_b
!
!  ---------------------------------------------------------------------
!
      subroutine read_node_size_b                                       &
     &         (id_rank, file_name, mesh_IO, ierr)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary mesh file: ', trim(file_name)
!
      bbuf_mesh%id_binary = id_read_mesh
      call open_read_binary_file(file_name, id_rank, bbuf_mesh)
      if(bbuf_mesh%ierr_bin .ne. 0) goto 99
      call read_num_node_b(id_rank, bbuf_mesh, mesh_IO)
!
  99  continue
      call close_binary_file(bbuf_mesh)
      ierr = bbuf_mesh%ierr_bin
!
      end subroutine read_node_size_b
!
!------------------------------------------------------------------
!
      subroutine read_geometry_size_b                                   &
     &         (id_rank, file_name, mesh_IO, ierr)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary mesh file: ', trim(file_name)
!
      bbuf_mesh%id_binary = id_read_mesh
      call open_read_binary_file(file_name, id_rank, bbuf_mesh)
      if(bbuf_mesh%ierr_bin .ne. 0) goto 99
      call read_num_node_ele_b(id_rank, bbuf_mesh, mesh_IO)
!
  99  continue
      call close_binary_file(bbuf_mesh)
      ierr = bbuf_mesh%ierr_bin
!
      end subroutine read_geometry_size_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_mesh_file_b                                      &
     &         (id_rank, file_name, mesh_IO, group_IO, ierr)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(mesh_geometry), intent(in) :: mesh_IO
      type(mesh_groups), intent(in) ::   group_IO
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write binary mesh file: ', trim(file_name)
!
      bbuf_mesh%id_binary = id_write_mesh
      call open_write_binary_file(file_name, bbuf_mesh)
      if(bbuf_mesh%ierr_bin .gt. 0) go to 99
      call write_geometry_data_b(id_rank, mesh_IO, bbuf_mesh)
      if(bbuf_mesh%ierr_bin .gt. 0) go to 99
      call write_mesh_groups_b(group_IO, bbuf_mesh)
!
  99  continue
      call close_binary_file(bbuf_mesh)
      ierr = bbuf_mesh%ierr_bin
!
      end subroutine write_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      end module mesh_file_IO_b
