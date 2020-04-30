!>@file   mesh_data_IO_b.f90
!!@brief  module mesh_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for Binary mesh data IO
!!
!!@verbatim
!!      subroutine write_geometry_data_b(id_rank, mesh_IO, bbuf)
!!      subroutine write_mesh_groups_b(mesh_group_IO, bbuf)
!!        type(mesh_geometry), intent(in) :: mesh_IO
!!        type(mesh_groups), intent(in) ::   mesh_group_IO
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!
!!      subroutine read_num_node_b(id_rank, bbuf, mesh_IO)
!!      subroutine read_num_node_ele_b(id_rank, bbuf, mesh_IO)
!!      subroutine read_geometry_data_b(id_rank, bbuf, mesh_IO)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine read_mesh_groups_b(bbuf, mesh_group_IO)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!
!!      subroutine write_filter_geometry_b                              &
!!     &         (id_rank, comm_IO, nod_IO, bbuf)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(communication_table), intent(in) :: comm_IO
!!        type(node_data), intent(in) :: nod_IO
!!      subroutine read_filter_geometry_b                               &
!!     &         (id_rank, bbuf, comm_IO, nod_IO)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(node_data), intent(inout) :: nod_IO
!!@endverbatim
!
      module mesh_data_IO_b
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_binary_IO_buffer
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_geometry_data_b(id_rank, mesh_IO, bbuf)
!
      use domain_data_IO_b
      use node_geometry_IO_b
      use element_connect_IO_b
!
      integer, intent(in) :: id_rank
      type(mesh_geometry), intent(in) :: mesh_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_domain_info_b(id_rank, mesh_IO%nod_comm, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      call write_geometry_info_b(mesh_IO%node, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_element_info_b(mesh_IO%ele, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      call write_import_data_b(mesh_IO%nod_comm, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_export_data_b(mesh_IO%nod_comm, bbuf)
!
      end subroutine write_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine write_mesh_groups_b(mesh_group_IO, bbuf)
!
      use groups_IO_b
!
      type(mesh_groups), intent(in) ::   mesh_group_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
!   write node group
      call write_grp_data_b(mesh_group_IO%nod_grp, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!  write element group
      call write_grp_data_b(mesh_group_IO%ele_grp, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!  write surface group
      call write_surf_grp_data_b(mesh_group_IO%surf_grp, bbuf)
!
      end subroutine write_mesh_groups_b
!
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_b                                &
     &         (id_rank, comm_IO, nod_IO, bbuf)
!
      use domain_data_IO_b
      use node_geometry_IO_b
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(node_data), intent(in) :: nod_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_domain_info_b(id_rank, comm_IO, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      call write_geometry_info_b(nod_IO, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      call write_import_data_b(comm_IO, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_export_data_b(comm_IO, bbuf)
!
      end subroutine write_filter_geometry_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_num_node_b(id_rank, bbuf, mesh_IO)
!
      use domain_data_IO_b
      use node_geometry_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call read_domain_info_b(id_rank, bbuf, mesh_IO%nod_comm)
      if(bbuf%ierr_bin .gt. 0) return
!
      call read_number_of_node_b(bbuf, mesh_IO%node)
!
      end subroutine read_num_node_b
!
!------------------------------------------------------------------
!
      subroutine read_num_node_ele_b(id_rank, bbuf, mesh_IO)
!
      use domain_data_IO_b
      use node_geometry_IO_b
      use element_connect_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call read_num_node_b(id_rank, bbuf, mesh_IO)
      if(bbuf%ierr_bin .gt. 0) return
!
      call read_geometry_info_b(bbuf, mesh_IO%node)
      if(bbuf%ierr_bin .ne. 0) return
!
!  ----  read element data -------
!
      call read_number_of_element_b(bbuf, mesh_IO%ele)
!
      end subroutine read_num_node_ele_b
!
!------------------------------------------------------------------
!
      subroutine read_geometry_data_b(id_rank, bbuf, mesh_IO)
!
      use domain_data_IO_b
      use node_geometry_IO_b
      use element_connect_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call read_num_node_ele_b(id_rank, bbuf, mesh_IO)
      if(bbuf%ierr_bin .ne. 0) return
!
!  ----  read element data -------
!
      call read_element_info_b(bbuf, mesh_IO%ele)
      if(bbuf%ierr_bin .gt. 0) return
!
! ----  import & export 
!
      call read_import_data_b(bbuf, mesh_IO%nod_comm)
      if(bbuf%ierr_bin .gt. 0) return
!
      call read_export_data_b(bbuf, mesh_IO%nod_comm)
!
      end subroutine read_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine read_mesh_groups_b(bbuf, mesh_group_IO)
!
      use groups_IO_b
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   read node group
      call read_group_data_b(bbuf, mesh_group_IO%nod_grp)
      if(bbuf%ierr_bin .gt. 0) return
!   read element group
      call read_group_data_b(bbuf, mesh_group_IO%ele_grp)
      if(bbuf%ierr_bin .gt. 0) return
!   read surface group
      call read_surf_grp_data_b(bbuf, mesh_group_IO%surf_grp)
!
      end subroutine read_mesh_groups_b
!
!------------------------------------------------------------------
!
      subroutine read_filter_geometry_b                                 &
     &         (id_rank, bbuf, comm_IO, nod_IO)
!
      use domain_data_IO_b
      use node_geometry_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(communication_table), intent(inout) :: comm_IO
      type(node_data), intent(inout) :: nod_IO
!
!
      call read_domain_info_b(id_rank, bbuf, comm_IO)
      if(bbuf%ierr_bin .gt. 0) return
!
      call read_number_of_node_b(bbuf, nod_IO)
      if(bbuf%ierr_bin .gt. 0) return
!
      call read_geometry_info_b(bbuf, nod_IO)
      if(bbuf%ierr_bin .ne. 0) return
!
! ----  import & export 
!
      call read_import_data_b(bbuf, comm_IO)
      if(bbuf%ierr_bin .gt. 0) return
      call read_export_data_b(bbuf, comm_IO)
!
      end subroutine read_filter_geometry_b
!
!------------------------------------------------------------------
!
      end module mesh_data_IO_b
