!>@file   mesh_data_IO.f90
!!@brief  module mesh_data_IO
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for ASCII mesh data IO
!!
!!@verbatim
!!      subroutine write_geometry_data(id_file, my_rank_IO, mesh_IO)
!!      subroutine write_mesh_groups(id_file, mesh_group_IO)
!!
!!      subroutine read_geometry_data(id_file, my_rank_IO, mesh_IO, ierr)
!!      subroutine read_mesh_groups(id_file, mesh_group_IO)
!!      subroutine read_num_node_ele(id_file, my_rank_IO, mesh_IO, ierr)
!!      subroutine read_num_node(id_file, my_rank_IO, mesh_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!
!!      subroutine write_filter_geometry                                &
!!     &         (id_file, my_rank_IO, comm_IO, nod_IO)
!!      subroutine read_filter_geometry                                 &
!!     &         (id_file, my_rank_IO, comm_IO, nod_IO, ierr)
!!
!!      subroutine output_node_sph_geometry                             &
!!     &         (id_file, my_rank_IO, mesh_IO)
!!      subroutine output_node_cyl_geometry                             &
!!     &         (id_file, my_rank_IO, mesh_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!@endverbatim
!
      module mesh_data_IO
!
      use m_precision
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use domain_data_IO
      use node_geometry_IO
      use element_connect_IO
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_geometry_data(id_file, my_rank_IO, mesh_IO)
!
      use m_fem_mesh_labels
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      write(id_file,'(a)', advance='NO') hd_fem_para()
      call write_domain_info(id_file, my_rank_IO, mesh_IO%nod_comm)
!
      write(id_file,'(a)', advance='NO') hd_fem_node()
      call write_geometry_info(id_file, mesh_IO%node)
!
!
      write(id_file,'(a)', advance='NO') hd_fem_elem()
      call write_element_info(id_file, mesh_IO%ele)
!
!
      write(id_file,'(a)', advance='NO') hd_fem_import()
      call write_import_data(id_file, mesh_IO%nod_comm)
!
!
      write(id_file,'(a)', advance='NO') hd_fem_export()
      call write_export_data(id_file, mesh_IO%nod_comm)
!
      end subroutine write_geometry_data
!
!------------------------------------------------------------------
!
      subroutine write_mesh_groups(id_file, mesh_group_IO)
!
      use m_fem_mesh_labels
      use groups_IO
!
      integer(kind = kint), intent(in) :: id_file
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   write node group
      write(id_file,'(a)', advance='NO') hd_fem_nodgrp()
      call write_grp_data(id_file, mesh_group_IO%nod_grp)
!
!  write element group
      write(id_file,'(a)', advance='NO') hd_fem_elegrp()
      call write_grp_data(id_file, mesh_group_IO%ele_grp)
!
!  write surface group
      write(id_file,'(a)', advance='NO') hd_fem_sfgrp()
      call write_surf_grp_data(id_file, mesh_group_IO%surf_grp)
!
      end subroutine write_mesh_groups
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_geometry_data(id_file, my_rank_IO, mesh_IO, ierr)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!        write(*,*) 'read_domain_info'
      call read_domain_info                                             &
     &   (id_file, my_rank_IO, mesh_IO%nod_comm, ierr)
      if(ierr .ne. 0) return
!
!        write(*,*) 'read_number_of_node'
      call read_number_of_node(id_file, mesh_IO%node)
!        write(*,*) 'read_geometry_info'
      call read_geometry_info(id_file, mesh_IO%node)
!
!  ----  read element data -------
!
!        write(*,*) 'read_number_of_element'
      call read_number_of_element(id_file, mesh_IO%ele)
!        write(*,*) 'read_element_info'
      call read_element_info(id_file, mesh_IO%ele)
!
! ----  import & export 
!
!        write(*,*) 'read_import_data'
      call read_import_data(id_file, mesh_IO%nod_comm)
!        write(*,*) 'read_export_data'
      call read_export_data(id_file, mesh_IO%nod_comm)
!
      end subroutine read_geometry_data
!
!------------------------------------------------------------------
!
      subroutine read_mesh_groups(id_file, mesh_group_IO)
!
      use groups_IO
!
      integer(kind = kint), intent(in) :: id_file
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   read node group
!      write(*,*) 'read_group_data node'
      call read_group_data(id_file, mesh_group_IO%nod_grp)
!  read element group
!      write(*,*) 'read_group_data ele'
      call read_group_data(id_file, mesh_group_IO%ele_grp)
!  read surface group
!      write(*,*) 'read_surf_grp_data surf'
      call read_surf_grp_data(id_file, mesh_group_IO%surf_grp)
!
      end subroutine read_mesh_groups
!
!------------------------------------------------------------------
!
      subroutine read_num_node_ele(id_file, my_rank_IO, mesh_IO, ierr)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!        write(*,*) 'read_domain_info'
      call read_domain_info                                             &
     &   (id_file, my_rank_IO, mesh_IO%nod_comm, ierr)
!        write(*,*) 'read_number_of_node'
      call read_number_of_node(id_file, mesh_IO%node)
!        write(*,*) 'read_geometry_info'
      call read_geometry_info(id_file, mesh_IO%node)
!
!  ----  read element data -------
!
!        write(*,*) 'read_number_of_element'
      call read_number_of_element(id_file, mesh_IO%ele)
!
      end subroutine read_num_node_ele
!
!------------------------------------------------------------------
!
      subroutine read_num_node(id_file, my_rank_IO, mesh_IO, ierr)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!        write(*,*) 'read_domain_info'
      call read_domain_info                                             &
     &   (id_file, my_rank_IO, mesh_IO%nod_comm, ierr)
!        write(*,*) 'read_number_of_node'
      call read_number_of_node(id_file, mesh_IO%node)
!
      end subroutine read_num_node
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_filter_geometry                                  &
     &         (id_file, my_rank_IO, comm_IO, nod_IO)
!
      use m_fem_mesh_labels
      use domain_data_IO
      use node_geometry_IO
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
      type(node_data), intent(inout) :: nod_IO
      type(communication_table), intent(inout) :: comm_IO
!
!
      write(id_file,'(a)', advance='NO') hd_fem_para()
      call write_domain_info(id_file, my_rank_IO, comm_IO)
!
      write(id_file,'(a)', advance='NO') hd_fem_node()
      call write_geometry_info(id_file, nod_IO)
!
!
      write(id_file,'(a)', advance='NO') hd_fem_import()
      call write_import_data(id_file, comm_IO)
!
      write(id_file,'(a)', advance='NO') hd_fem_export()
      call write_export_data(id_file, comm_IO)
!
      end subroutine write_filter_geometry
!
!------------------------------------------------------------------
!
      subroutine read_filter_geometry                                   &
     &         (id_file, my_rank_IO, comm_IO, nod_IO, ierr)
!
      use domain_data_IO
      use node_geometry_IO
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(communication_table), intent(inout) :: comm_IO
      type(node_data), intent(inout) :: nod_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!        write(*,*) 'read_domain_info'
        call read_domain_info(id_file, my_rank_IO, comm_IO, ierr)
!        write(*,*) 'read_number_of_node'
        call read_number_of_node(id_file, nod_IO)
!        write(*,*) 'read_geometry_info'
        call read_geometry_info(id_file, nod_IO)
!
! ----  import & export 
!
!        write(*,*) 'read_import_data'
       call read_import_data(id_file, comm_IO)
!        write(*,*) 'read_export_data'
       call read_export_data(id_file, comm_IO)
!
       end subroutine read_filter_geometry
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_node_sph_geometry                               &
     &         (id_file, my_rank_IO, mesh_IO)
!
      use m_fem_mesh_labels
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      write(id_file,'(a)', advance='NO') hd_fem_para_sph()
      write(id_file,'(a)', advance='NO') hd_fem_para()
      call write_domain_info(id_file, my_rank_IO, mesh_IO%nod_comm)
!
      write(id_file,'(a)', advance='NO') hd_fem_node_sph()
      call write_geometry_info(id_file, mesh_IO%node)
!
      end subroutine output_node_sph_geometry
!
!  ---------------------------------------------------------------------
!
      subroutine output_node_cyl_geometry                               &
     &         (id_file, my_rank_IO, mesh_IO)
!
      use m_fem_mesh_labels
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      write(id_file,'(a)', advance='NO') hd_fem_para_cyl()
      write(id_file,'(a)', advance='NO') hd_fem_para()
!
!
      call write_domain_info(id_file, my_rank_IO, mesh_IO%nod_comm)
!
      write(id_file,'(a)', advance='NO') hd_fem_node_cyl()
      call write_geometry_info(id_file, mesh_IO%node)
!
      end subroutine output_node_cyl_geometry
!
!------------------------------------------------------------------
!
      end module mesh_data_IO
