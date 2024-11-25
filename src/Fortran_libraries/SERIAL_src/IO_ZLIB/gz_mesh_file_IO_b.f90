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
!!     &         (id_rank, file_name, mesh_IO, group_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   group_IO
!!
!!      subroutine gz_read_mesh_geometry_b                              &
!!     &         (id_rank, file_name, mesh_IO, ierr)
!!      subroutine gz_read_node_size_b                                  &
!!     &         (id_rank, file_name, mesh_IO, ierr)
!!      subroutine gz_read_geometry_size_b                              &
!!     &         (id_rank, file_name, mesh_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine gz_write_mesh_file_b                                 &
!!     &         (id_rank, file_name, mesh_IO, group_IO)
!!        type(mesh_geometry), intent(in) :: mesh_IO
!!        type(mesh_groups), intent(in) ::   group_IO
!!@endverbatim
!
      module gz_mesh_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_buffer_4_gzip
      use binary_IO
      use gz_mesh_data_IO_b
      use gz_binary_IO
!
      implicit none
!
      type(buffer_4_gzip), private, save :: zbuf_mesh
      character, pointer, private, save :: FPz_msh
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_mesh_file_b                                    &
     &         (id_rank, file_name, mesh_IO, group_IO, ierr)
!
      use gzip_file_access
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
     &   'Read gzipped binary mesh file: ', trim(file_name)
!
      call open_rd_gzfile_b(FPz_msh, file_name, id_rank, zbuf_mesh)
      if(zbuf_mesh%ierr_zlib .ne. 0) go to 99
!
      call gz_read_geometry_data_b(FPz_msh, id_rank,                    &
     &                             zbuf_mesh, mesh_IO)
      if(zbuf_mesh%ierr_zlib .ne. 0) go to 99
      call gz_read_mesh_groups_b(FPz_msh, zbuf_mesh, group_IO)
!
  99  continue
      call close_gzfile_b(FPz_msh)
      ierr = zbuf_mesh%ierr_zlib
!
      end subroutine gz_read_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_mesh_geometry_b                                &
     &         (id_rank, file_name, mesh_IO, ierr)
!
      use gzip_file_access
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
      call open_rd_gzfile_b(FPz_msh, file_name, id_rank, zbuf_mesh)
      if(zbuf_mesh%ierr_zlib .ne. 0) goto 99
!
      call gz_read_geometry_data_b(FPz_msh, id_rank,                    &
     &                             zbuf_mesh, mesh_IO)
!
  99  continue
      call close_gzfile_b(FPz_msh)
      ierr = zbuf_mesh%ierr_zlib
!
      end subroutine gz_read_mesh_geometry_b
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_node_size_b                                    &
     &         (id_rank, file_name, mesh_IO, ierr)
!
      use gzip_file_access
      use gz_domain_data_IO_b
      use gz_node_geometry_IO_b
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
      call open_rd_gzfile_b(FPz_msh, file_name, id_rank, zbuf_mesh)
      if(zbuf_mesh%ierr_zlib .ne. 0) goto 99
!
      call gz_read_domain_info_b(FPz_msh, id_rank, zbuf_mesh,           &
     &                           mesh_IO%nod_comm)
      if(zbuf_mesh%ierr_zlib .ne. 0) return
      call gz_read_number_of_node_b(FPz_msh, zbuf_mesh, mesh_IO%node)
      if(zbuf_mesh%ierr_zlib .ne. 0) return
!
  99  continue
      call close_gzfile_b(FPz_msh)
      ierr = zbuf_mesh%ierr_zlib
!
      end subroutine gz_read_node_size_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_geometry_size_b                                &
     &         (id_rank, file_name, mesh_IO, ierr)
!
      use gzip_file_access
      use gz_domain_data_IO_b
      use gz_node_geometry_IO_b
      use gz_element_connect_IO_b
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
      call open_rd_gzfile_b(FPz_msh, file_name, id_rank, zbuf_mesh)
      if(zbuf_mesh%ierr_zlib .ne. 0) goto 99
!
      call gz_read_domain_info_b(FPz_msh, id_rank, zbuf_mesh,           &
     &                           mesh_IO%nod_comm)
      if(zbuf_mesh%ierr_zlib .ne. 0) return
      call gz_read_geometry_info_b(FPz_msh, zbuf_mesh, mesh_IO%node)
      if(zbuf_mesh%ierr_zlib .ne. 0) return
      call gz_read_number_of_element_b(FPz_msh, zbuf_mesh, mesh_IO%ele)
      if(zbuf_mesh%ierr_zlib .ne. 0) return
!
  99  continue
      call close_gzfile_b(FPz_msh)
      ierr = zbuf_mesh%ierr_zlib
!
      end subroutine gz_read_geometry_size_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_mesh_file_b                                   &
     &         (id_rank, file_name, mesh_IO, group_IO)
!
      use gzip_file_access
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(mesh_geometry), intent(in) :: mesh_IO
      type(mesh_groups), intent(in) ::   group_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped binary mesh file: ', trim(file_name)
!
      call open_wt_gzfile_b(FPz_msh, file_name, zbuf_mesh)
      call gz_write_geometry_data_b(FPz_msh, id_rank,                   &
     &                              mesh_IO, zbuf_mesh)
      call gz_write_mesh_groups_b(FPz_msh, group_IO, zbuf_mesh)
      call close_gzfile_b(FPz_msh)
!
      end subroutine gz_write_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      end module gz_mesh_file_IO_b
