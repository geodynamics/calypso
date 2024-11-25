!>@file   gz_mesh_data_IO_b.f90
!!@brief  module gz_mesh_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for gzipped binary mesh data IO
!!
!!@verbatim
!!      subroutine gz_write_geometry_data_b(FPz_f, id_rank,             &
!!     &                                    mesh_IO, zbuf)
!!      subroutine gz_write_mesh_groups_b(FPz_f, mesh_group_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(mesh_geometry), intent(in) :: mesh_IO
!!        type(mesh_groups), intent(in) ::   mesh_group_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_read_geometry_data_b                              &
!!     &         (FPz_f, id_rank, zbuf, mesh_IO)
!!      subroutine gz_read_mesh_groups_b(FPz_f, zbuf, mesh_group_IO)
!!        character, pointer, intent(in) :: FPz_f
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!@endverbatim
!
      module gz_mesh_data_IO_b
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_buffer_4_gzip
      use binary_IO
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_write_geometry_data_b(FPz_f, id_rank,               &
     &                                    mesh_IO, zbuf)
!
      use gz_domain_data_IO_b
      use gz_node_geometry_IO_b
      use gz_element_connect_IO_b
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(mesh_geometry), intent(in) :: mesh_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call gz_write_domain_info_b(FPz_f, id_rank,                       &
     &                            mesh_IO%nod_comm, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_write_geometry_info_b(FPz_f, mesh_IO%node, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_element_info_b(FPz_f,mesh_IO%ele, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_write_import_data_b(FPz_f, mesh_IO%nod_comm, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_export_data_b(FPz_f, mesh_IO%nod_comm, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_write_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_mesh_groups_b(FPz_f, mesh_group_IO, zbuf)
!
      use gz_groups_IO_b
!
      character, pointer, intent(in) :: FPz_f
      type(mesh_groups), intent(in) ::   mesh_group_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!   write node group
      call gz_write_grp_data_b(FPz_f, mesh_group_IO%nod_grp, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!  write element group
      call gz_write_grp_data_b(FPz_f, mesh_group_IO%ele_grp, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!  write surface group
      call gz_write_surf_grp_data_b                                     &
     &   (FPz_f, mesh_group_IO%surf_grp, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_write_mesh_groups_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_geometry_data_b                                &
     &         (FPz_f, id_rank, zbuf, mesh_IO)
!
      use gz_domain_data_IO_b
      use gz_node_geometry_IO_b
      use gz_element_connect_IO_b
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
!
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_read_domain_info_b(FPz_f, id_rank, zbuf,                  &
     &                           mesh_IO%nod_comm)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_read_geometry_info_b(FPz_f, zbuf, mesh_IO%node)
      if(zbuf%ierr_zlib .ne. 0) return
!
!  ----  read element data -------
!
      call gz_read_element_info_b(FPz_f, zbuf, mesh_IO%ele)
      if(zbuf%ierr_zlib .ne. 0) return
!
! ----  import & export 
!
      call gz_read_import_data_b(FPz_f, zbuf, mesh_IO%nod_comm)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_read_export_data_b(FPz_f, zbuf, mesh_IO%nod_comm)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_read_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_mesh_groups_b(FPz_f, zbuf, mesh_group_IO)
!
      use gz_groups_IO_b
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   read node group
      call gz_read_group_data_b(FPz_f, zbuf, mesh_group_IO%nod_grp)
      if(zbuf%ierr_zlib .ne. 0) return
!   read element group
      call gz_read_group_data_b(FPz_f, zbuf, mesh_group_IO%ele_grp)
      if(zbuf%ierr_zlib .ne. 0) return
!   read surface group
      call gz_read_surf_grp_data_b(FPz_f, zbuf, mesh_group_IO%surf_grp)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_read_mesh_groups_b
!
!------------------------------------------------------------------
!
      end module gz_mesh_data_IO_b
