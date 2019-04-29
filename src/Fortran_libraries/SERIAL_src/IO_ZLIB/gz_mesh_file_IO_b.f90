!gz_mesh_file_IO_b.f90
!      module gz_mesh_file_IO_b
!
!      Written by H. Matsui on Apr., 2006
!
!>@file   gz_mesh_file_IO_b.f90
!!@brief  module gz_mesh_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief gzipped binary mesh file IO routines
!!
!!@verbatim
!!      subroutine gz_read_mesh_file_b                                  &
!!     &         (id_rank, file_name, fem_IO, ierr)
!!        type(mesh_data), intent(inout) :: fem_IO
!!
!!      subroutine gz_read_mesh_geometry_b                              &
!!     &         (id_rank, file_name, mesh_IO, ierr)
!!       subroutine gz_read_node_size_b                                 &
!!      &         (id_rank, file_name, mesh_IO, ierr)
!!       subroutine gz_read_geometry_size_b                             &
!!      &         (id_rank, file_name, mesh_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine gz_write_mesh_file_b(id_rank, file_name, fem_IO)
!!        type(mesh_data), intent(in) :: fem_IO
!!@endverbatim
!
      module gz_mesh_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use binary_IO
      use gz_mesh_data_IO_b
      use gz_binary_IO
      use skip_gz_comment
!
      implicit none
!
      type(binary_IO_flags) :: gz_meshflags
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_mesh_file_b                                    &
     &         (id_rank, file_name, fem_IO, ierr)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_data), intent(inout) :: fem_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary mesh file: ', trim(file_name)
!
      call open_rd_gzfile_b(file_name, id_rank, gz_meshflags)
      if(gz_meshflags%ierr_IO .ne. 0) goto 99
!
      call gz_read_geometry_data_b                                      &
     &   (id_rank, gz_meshflags, fem_IO%mesh)
      if(gz_meshflags%ierr_IO .ne. 0) goto 99
      call gz_read_mesh_groups_b(gz_meshflags, fem_IO%group)
!
  99  continue
      call close_gzfile_f
      ierr = gz_meshflags%ierr_IO
!
      end subroutine gz_read_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_mesh_geometry_b                                &
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
     &   'Read gzipped binary mesh file: ', trim(file_name)
!
      call open_rd_gzfile_b(file_name, id_rank, gz_meshflags)
      if(gz_meshflags%ierr_IO .ne. 0) goto 99
!
      call gz_read_geometry_data_b(id_rank, gz_meshflags, mesh_IO)
!
  99  continue
      call close_gzfile_f
      ierr = gz_meshflags%ierr_IO
!
      end subroutine gz_read_mesh_geometry_b
!
!  ---------------------------------------------------------------------
!
       subroutine gz_read_node_size_b                                   &
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
     &   'Read gzipped binary mesh file: ', trim(file_name)
!
      call open_rd_gzfile_b(file_name, id_rank, gz_meshflags)
      if(gz_meshflags%ierr_IO .ne. 0) goto 99
!
      call gz_read_num_node_b(id_rank, gz_meshflags, mesh_IO)
!
  99  continue
      call close_gzfile_f
      ierr = gz_meshflags%ierr_IO
!
      end subroutine gz_read_node_size_b
!
!------------------------------------------------------------------
!
       subroutine gz_read_geometry_size_b                               &
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
     &   'Read gzipped binary mesh file: ', trim(file_name)
!
      call open_rd_gzfile_b(file_name, id_rank, gz_meshflags)
      if(gz_meshflags%ierr_IO .ne. 0) goto 99
!
      call gz_read_num_node_ele_b(id_rank, gz_meshflags, mesh_IO)
!
  99  continue
      call close_gzfile_f
      ierr = gz_meshflags%ierr_IO
!
      end subroutine gz_read_geometry_size_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_mesh_file_b(id_rank, file_name, fem_IO)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_data), intent(in) :: fem_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped binary mesh file: ', trim(file_name)
!
      call open_wt_gzfile_b(file_name, gz_meshflags)
      call gz_write_geometry_data_b(id_rank, fem_IO%mesh, gz_meshflags)
      call gz_write_mesh_groups_b(fem_IO%group, gz_meshflags)
      call close_gzfile_f
!
      end subroutine gz_write_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      end module gz_mesh_file_IO_b
