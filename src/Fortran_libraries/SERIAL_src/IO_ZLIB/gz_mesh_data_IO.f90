!>@file   gz_mesh_data_IO.f90
!!@brief  module gz_mesh_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief gzipped mesh data IO routines
!!
!!@verbatim
!!      subroutine gz_write_geometry_data(id_rank, mesh_IO)
!!      subroutine gz_write_mesh_groups(mesh_group_IO)
!!        type(mesh_geometry), intent(in) :: mesh_IO
!!        type(mesh_groups), intent(in) ::   mesh_group_IO
!!
!!      subroutine gz_read_num_node(id_rank, mesh_IO, ierr)
!!      subroutine gz_read_num_node_ele(id_rank, mesh_IO, ierr)
!!      subroutine gz_read_geometry_data(id_rank, mesh_IO, ierr)
!!      subroutine gz_read_mesh_groups(mesh_group_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!
!!      subroutine gz_write_filter_geometry(id_rank, comm_IO, nod_IO)
!!        type(communication_table), intent(in) :: comm_IO
!!        type(node_data), intent(in) :: nod_IO
!!      subroutine gz_read_filter_geometry                              &
!!      &         (id_rank, comm_IO, nod_IO, ierr)
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(node_data), intent(inout) :: nod_IO
!!@endverbatim

!
      module gz_mesh_data_IO
!
      use m_precision
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use skip_gz_comment
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
      subroutine gz_write_geometry_data(id_rank, mesh_IO)
!
      use m_fem_mesh_labels
!
      integer, intent(in) :: id_rank
      type(mesh_geometry), intent(in) :: mesh_IO
!
!
      textbuf = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf
      call gz_write_domain_info(id_rank, mesh_IO%nod_comm)
!
!
      textbuf = hd_fem_node() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_geometry_info(mesh_IO%node)
!
!
      textbuf = hd_fem_elem() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_element_info(mesh_IO%ele)
!
!
      textbuf = hd_fem_import() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_import_data(mesh_IO%nod_comm)
!
!
      textbuf = hd_fem_export() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_export_data(mesh_IO%nod_comm)
!
      end subroutine gz_write_geometry_data
!
!------------------------------------------------------------------
!
      subroutine gz_write_mesh_groups(mesh_group_IO)
!
      use m_fem_mesh_labels
      use gz_groups_IO
!
      type(mesh_groups), intent(in) ::   mesh_group_IO
!
!
!   write node group
      textbuf = hd_fem_nodgrp() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(mesh_group_IO%nod_grp)
!
!  write element group
      textbuf = hd_fem_elegrp() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(mesh_group_IO%ele_grp)
!
!  write surface group
      textbuf = hd_fem_sfgrp() // char(0)
      call gz_write_textbuf_no_lf
      call write_surf_grp_data_gz(mesh_group_IO%surf_grp)
!
      end subroutine gz_write_mesh_groups
!
!------------------------------------------------------------------
!
      subroutine gz_write_filter_geometry(id_rank, comm_IO, nod_IO)
!
      use m_fem_mesh_labels
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(node_data), intent(in) :: nod_IO
!
!
      textbuf = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf
      call gz_write_domain_info(id_rank, comm_IO)
!
!
      textbuf = hd_fem_node() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_geometry_info(nod_IO)
!
!
      textbuf = hd_fem_import() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_import_data(comm_IO)
!
!
      textbuf = hd_fem_export() // char(0)
      call gz_write_textbuf_no_lf
!
      call gz_write_export_data(comm_IO)
!
      end subroutine gz_write_filter_geometry
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_num_node(id_rank, mesh_IO, ierr)
!
      integer, intent(in) :: id_rank
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!      write(*,*) 'gz_read_domain_info'
       call gz_read_domain_info(id_rank, mesh_IO%nod_comm, ierr)
!      write(*,*) 'gz_read_number_of_node'
       call gz_read_number_of_node(mesh_IO%node)
!
       end subroutine gz_read_num_node
!
!------------------------------------------------------------------
!
      subroutine gz_read_num_node_ele(id_rank, mesh_IO, ierr)
!
      integer, intent(in) :: id_rank
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call gz_read_num_node(id_rank, mesh_IO, ierr)
!      write(*,*) 'gz_read_geometry_info'
      call gz_read_geometry_info(mesh_IO%node)
!
!  ----  read element data -------
!
!      write(*,*) 'gz_read_number_of_element'
      call gz_read_number_of_element(mesh_IO%ele)
!
      end subroutine gz_read_num_node_ele
!
!------------------------------------------------------------------
!
      subroutine gz_read_geometry_data(id_rank, mesh_IO, ierr)
!
      integer, intent(in) :: id_rank
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call gz_read_num_node_ele(id_rank, mesh_IO, ierr)
!
!  ----  read element data -------
!
!      write(*,*) 'gz_read_element_info'
      call gz_read_element_info(mesh_IO%ele)
!
! ----  import & export 
!
!      write(*,*) 'gz_read_import_data'
      call gz_read_import_data(mesh_IO%nod_comm)
!      write(*,*) 'gz_read_export_data'
      call gz_read_export_data(mesh_IO%nod_comm)
!
      end subroutine gz_read_geometry_data
!
!------------------------------------------------------------------
!
      subroutine gz_read_mesh_groups(mesh_group_IO)
!
      use m_fem_mesh_labels
      use gz_groups_IO
!
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   read node group
      call read_group_data_gz(mesh_group_IO%nod_grp)
!   read element group
      call read_group_data_gz(mesh_group_IO%ele_grp)
!   read surface group
      call read_surf_grp_data_gz(mesh_group_IO%surf_grp)
!
      end subroutine gz_read_mesh_groups
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
       subroutine gz_read_filter_geometry                               &
      &         (id_rank, comm_IO, nod_IO, ierr)
!
      integer, intent(in) :: id_rank
!
      type(node_data), intent(inout) :: nod_IO
      type(communication_table), intent(inout) :: comm_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!        write(*,*) 'gz_read_domain_info'
        call gz_read_domain_info(id_rank, comm_IO, ierr)
!        write(*,*) 'gz_read_number_of_node'
        call gz_read_number_of_node(nod_IO)
!        write(*,*) 'gz_read_geometry_info'
        call gz_read_geometry_info(nod_IO)
!
! ----  import & export 
!
!        write(*,*) 'gz_read_import_data'
        call gz_read_import_data(comm_IO)
!        write(*,*) 'gz_read_export_data'
        call gz_read_export_data(comm_IO)
!
       end subroutine gz_read_filter_geometry
!
!------------------------------------------------------------------
!
      end module gz_mesh_data_IO
