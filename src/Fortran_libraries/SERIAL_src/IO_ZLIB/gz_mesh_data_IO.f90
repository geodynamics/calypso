!>@file   gz_mesh_data_IO.f90
!!@brief  module gz_mesh_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief gzipped mesh data IO routines
!!
!!@verbatim
!!      subroutine gz_write_geometry_data(FPz_f, id_rank, mesh_IO, zbuf)
!!      subroutine gz_write_mesh_groups(FPz_f, mesh_group_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(mesh_geometry), intent(in) :: mesh_IO
!!        type(mesh_groups), intent(in) ::   mesh_group_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_read_geometry_data(FPz_f, id_rank,                &
!!     &                                 mesh_IO, zbuf, ierr)
!!      subroutine gz_read_mesh_groups(FPz_f, mesh_group_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_write_filter_geometry                             &
!!     &         (FPz_f, id_rank, comm_IO, nod_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(communication_table), intent(in) :: comm_IO
!!        type(node_data), intent(in) :: nod_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine gz_read_filter_geometry                              &
!!      &         (FPz_f, id_rank, comm_IO, nod_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(node_data), intent(inout) :: nod_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim

!
      module gz_mesh_data_IO
!
      use m_precision
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_buffer_4_gzip
      use gz_domain_data_IO
      use gz_node_geometry_IO
      use gz_element_connect_IO
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_write_geometry_data(FPz_f, id_rank, mesh_IO, zbuf)
!
      use m_fem_mesh_labels
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(mesh_geometry), intent(in) :: mesh_IO
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%fixbuf(1) = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
      call gz_write_domain_info(FPz_f, id_rank, mesh_IO%nod_comm, zbuf)
!
!
      zbuf%fixbuf(1) = hd_fem_node() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
      call gz_write_geometry_info(FPz_f, mesh_IO%node, zbuf)
!
!
      zbuf%fixbuf(1) = hd_fem_elem() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_element_info(FPz_f, mesh_IO%ele, zbuf)
!
!
      zbuf%fixbuf(1) = hd_fem_import() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_import_data(FPz_f, mesh_IO%nod_comm, zbuf)
!
!
      zbuf%fixbuf(1) = hd_fem_export() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_export_data(FPz_f, mesh_IO%nod_comm, zbuf)
!
      end subroutine gz_write_geometry_data
!
!------------------------------------------------------------------
!
      subroutine gz_write_mesh_groups(FPz_f, mesh_group_IO, zbuf)
!
      use m_fem_mesh_labels
      use gz_group_data_IO
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      type(mesh_groups), intent(in) ::   mesh_group_IO
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!   write node group
      zbuf%fixbuf(1) = hd_fem_nodgrp() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
      call write_group_data_gz(FPz_f, mesh_group_IO%nod_grp, zbuf)
!
!  write element group
      zbuf%fixbuf(1) = hd_fem_elegrp() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
      call write_group_data_gz(FPz_f, mesh_group_IO%ele_grp, zbuf)
!
!  write surface group
      zbuf%fixbuf(1) = hd_fem_sfgrp() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
      call write_surf_group_data_gz                                     &
     &   (FPz_f, mesh_group_IO%surf_grp, zbuf)
!
      end subroutine gz_write_mesh_groups
!
!------------------------------------------------------------------
!
      subroutine gz_write_filter_geometry                               &
     &         (FPz_f, id_rank, comm_IO, nod_IO, zbuf)
!
      use m_fem_mesh_labels
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(node_data), intent(in) :: nod_IO
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%fixbuf(1) = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
      call gz_write_domain_info(FPz_f, id_rank, comm_IO, zbuf)
!
!
      zbuf%fixbuf(1) = hd_fem_node() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_geometry_info(FPz_f, nod_IO, zbuf)
!
!
      zbuf%fixbuf(1) = hd_fem_import() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_import_data(FPz_f, comm_IO, zbuf)
!
!
      zbuf%fixbuf(1) = hd_fem_export() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_export_data(FPz_f, comm_IO, zbuf)
!
      end subroutine gz_write_filter_geometry
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_geometry_data(FPz_f, id_rank,                  &
     &                                 mesh_IO, zbuf, ierr)
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
!      write(*,*) 'gz_read_domain_info'
      call gz_read_domain_info                                          &
     &   (FPz_f, id_rank, mesh_IO%nod_comm, zbuf, ierr)
      if(ierr .gt. 0) return
!
      call gz_read_geometry_info(FPz_f, mesh_IO%node, zbuf)
!
!  ----  read element data -------
!
!      write(*,*) 'gz_read_number_of_element'
      call gz_read_number_of_element(FPz_f, mesh_IO%ele, zbuf)
!      write(*,*) 'gz_read_element_info'
      call gz_read_element_info(FPz_f, mesh_IO%ele, zbuf)
!
! ----  import & export 
!
!      write(*,*) 'gz_read_import_data'
      call gz_read_import_data(FPz_f, mesh_IO%nod_comm, zbuf)
!      write(*,*) 'gz_read_export_data'
      call gz_read_export_data(FPz_f, mesh_IO%nod_comm, zbuf)
!
      end subroutine gz_read_geometry_data
!
!------------------------------------------------------------------
!
      subroutine gz_read_mesh_groups(FPz_f, mesh_group_IO, zbuf)
!
      use m_fem_mesh_labels
      use gz_group_data_IO
!
      character, pointer, intent(in) :: FPz_f
      type(mesh_groups), intent(inout) ::   mesh_group_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
!   read node group
      call read_group_data_gz(FPz_f, mesh_group_IO%nod_grp, zbuf)
!   read element group
      call read_group_data_gz(FPz_f, mesh_group_IO%ele_grp, zbuf)
!   read surface group
      call read_surface_group_data_gz                                   &
     &   (FPz_f, mesh_group_IO%surf_grp, zbuf)
!
      end subroutine gz_read_mesh_groups
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
       subroutine gz_read_filter_geometry                               &
      &         (FPz_f, id_rank, comm_IO, nod_IO, zbuf, ierr)
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
!
      type(node_data), intent(inout) :: nod_IO
      type(communication_table), intent(inout) :: comm_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
!        write(*,*) 'gz_read_domain_info'
        call gz_read_domain_info(FPz_f, id_rank, comm_IO, zbuf, ierr)

!        write(*,*) 'gz_read_geometry_info'
        call gz_read_geometry_info(FPz_f, nod_IO, zbuf)
!
! ----  import & export 
!
!        write(*,*) 'gz_read_import_data'
        call gz_read_import_data(FPz_f, comm_IO, zbuf)
!        write(*,*) 'gz_read_export_data'
        call gz_read_export_data(FPz_f, comm_IO, zbuf)
!
       end subroutine gz_read_filter_geometry
!
!------------------------------------------------------------------
!
      end module gz_mesh_data_IO
