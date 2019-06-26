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
      use binary_IO
      use mesh_data_IO_b
!
      implicit none
!
      type(binary_IO_flags), private :: bin_meshflags
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
      call open_read_binary_file(file_name, id_rank, bin_meshflags)
      if(bin_meshflags%ierr_IO .ne. 0) return
!
      call read_geometry_data_b(id_rank, bin_meshflags, mesh_IO)
      if(bin_meshflags%ierr_IO .ne. 0) return
!
      call read_mesh_groups_b(bin_meshflags, group_IO)
!
      call close_binary_file
      ierr = bin_meshflags%ierr_IO
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
      call open_read_binary_file(file_name, id_rank, bin_meshflags)
      if(bin_meshflags%ierr_IO .ne. 0) return
      call read_geometry_data_b(id_rank, bin_meshflags, mesh_IO)
!
      call close_binary_file
      ierr = bin_meshflags%ierr_IO
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
      call open_read_binary_file(file_name, id_rank, bin_meshflags)
      call read_num_node_b(id_rank, bin_meshflags, mesh_IO)
      call close_binary_file
      ierr = bin_meshflags%ierr_IO
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
      call open_read_binary_file(file_name, id_rank, bin_meshflags)
      call read_num_node_ele_b(id_rank, bin_meshflags, mesh_IO)
      call close_binary_file
      ierr = bin_meshflags%ierr_IO
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
      call open_write_binary_file(file_name, bin_meshflags)
      if(bin_meshflags%ierr_IO .ne. 0) ierr = ierr_file
      call write_geometry_data_b(id_rank, mesh_IO, bin_meshflags)
      if(bin_meshflags%ierr_IO .ne. 0) ierr = ierr_file
      call write_mesh_groups_b(group_IO, bin_meshflags)
      if(bin_meshflags%ierr_IO .ne. 0) ierr = ierr_file
      call close_binary_file
!
      end subroutine write_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      end module mesh_file_IO_b
