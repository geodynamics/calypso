!>@file   gz_mesh_data_IO.f90
!!@brief  module gz_mesh_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief gzipped mesh data IO routines
!!
!!@verbatim
!!      subroutine write_geometry_data_gz(my_rank_IO, mesh_IO)
!!      subroutine write_mesh_groups_gz(mesh_group_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!
!!      subroutine read_geometry_data_gz(my_rank_IO, mesh_IO, ierr)
!!      subroutine read_mesh_groups_gz(mesh_group_IO)
!!      subroutine read_num_node_ele_gz(my_rank_IO, mesh_IO, ierr)
!!      subroutine read_num_node_gz(my_rank_IO, mesh_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!
!!      subroutine write_filter_geometry_gz(my_rank_IO, comm_IO, nod_IO)
!!       subroutine read_filter_geometry_gz                             &
!!      &         (my_rank_IO, comm_IO, nod_IO, ierr)
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
      subroutine write_geometry_data_gz(my_rank_IO, mesh_IO)
!
      use m_fem_mesh_labels
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      textbuf = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf
      call write_domain_info_gz(my_rank_IO, mesh_IO%nod_comm)
!
!
      textbuf = hd_fem_node() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_geometry_info_gz(mesh_IO%node)
!
!
      textbuf = hd_fem_elem() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_element_info_gz(mesh_IO%ele)
!
!
      textbuf = hd_fem_import() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_import_data_gz(mesh_IO%nod_comm)
!
!
      textbuf = hd_fem_export() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_export_data_gz(mesh_IO%nod_comm)
!
      end subroutine write_geometry_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_mesh_groups_gz(mesh_group_IO)
!
      use m_fem_mesh_labels
      use gz_sph_rj_groups_IO
!
      type(mesh_groups), intent(inout) ::   mesh_group_IO
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
      end subroutine write_mesh_groups_gz
!
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_gz(my_rank_IO, comm_IO, nod_IO)
!
      use m_fem_mesh_labels
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      type(node_data), intent(inout) :: nod_IO
!
!
      textbuf = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf
      call write_domain_info_gz(my_rank_IO, comm_IO)
!
!
      textbuf = hd_fem_node() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_geometry_info_gz(nod_IO)
!
!
      textbuf = hd_fem_import() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_import_data_gz(comm_IO)
!
!
      textbuf = hd_fem_export() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_export_data_gz(comm_IO)
!
      end subroutine write_filter_geometry_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
       subroutine read_geometry_data_gz(my_rank_IO, mesh_IO, ierr)
!
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!        write(*,*) 'read_domain_info_gz'
        call read_domain_info_gz(my_rank_IO, mesh_IO%nod_comm, ierr)
!        write(*,*) 'read_number_of_node_gz'
        call read_number_of_node_gz(mesh_IO%node)
!        write(*,*) 'read_geometry_info_gz'
        call read_geometry_info_gz(mesh_IO%node)
!
!  ----  read element data -------
!
!        write(*,*) 'read_number_of_element_gz'
        call read_number_of_element_gz(mesh_IO%ele)
!        write(*,*) 'read_element_info_gz'
        call read_element_info_gz(mesh_IO%ele)
!
! ----  import & export 
!
!        write(*,*) 'read_import_data_gz'
        call read_import_data_gz(mesh_IO%nod_comm)
!        write(*,*) 'read_export_data_gz'
        call read_export_data_gz(mesh_IO%nod_comm)
!
       end subroutine read_geometry_data_gz
!
!------------------------------------------------------------------
!
      subroutine read_mesh_groups_gz(mesh_group_IO)
!
      use m_fem_mesh_labels
      use gz_sph_rj_groups_IO
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
      end subroutine read_mesh_groups_gz
!
!------------------------------------------------------------------
!
       subroutine read_num_node_ele_gz(my_rank_IO, mesh_IO, ierr)
!
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!        write(*,*) 'read_domain_info_gz'
        call read_domain_info_gz(my_rank_IO, mesh_IO%nod_comm, ierr)
!        write(*,*) 'read_number_of_node_gz'
        call read_number_of_node_gz(mesh_IO%node)
!        write(*,*) 'read_geometry_info_gz'
        call read_geometry_info_gz(mesh_IO%node)
!
!  ----  read element data -------
!
!        write(*,*) 'read_number_of_element_gz'
        call read_number_of_element_gz(mesh_IO%ele)
!
       end subroutine read_num_node_ele_gz
!
!------------------------------------------------------------------
!
       subroutine read_num_node_gz(my_rank_IO, mesh_IO, ierr)
!
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!        write(*,*) 'read_domain_info_gz'
        call read_domain_info_gz(my_rank_IO, mesh_IO%nod_comm, ierr)
!        write(*,*) 'read_number_of_node_gz'
        call read_number_of_node_gz(mesh_IO%node)
!
       end subroutine read_num_node_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
       subroutine read_filter_geometry_gz                               &
      &         (my_rank_IO, comm_IO, nod_IO, ierr)
!
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(node_data), intent(inout) :: nod_IO
      type(communication_table), intent(inout) :: comm_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!        write(*,*) 'read_domain_info_gz'
        call read_domain_info_gz(my_rank_IO, comm_IO, ierr)
!        write(*,*) 'read_number_of_node_gz'
        call read_number_of_node_gz(nod_IO)
!        write(*,*) 'read_geometry_info_gz'
        call read_geometry_info_gz(nod_IO)
!
! ----  import & export 
!
!        write(*,*) 'read_import_data_gz'
        call read_import_data_gz(comm_IO)
!        write(*,*) 'read_export_data_gz'
        call read_export_data_gz(comm_IO)
!
       end subroutine read_filter_geometry_gz
!
!------------------------------------------------------------------
!
      end module gz_mesh_data_IO
