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
!!      subroutine write_geometry_data_b(id_rank, mesh_IO, bin_flags)
!!      subroutine write_mesh_groups_b(mesh_group_IO, bin_flags)
!!        type(mesh_geometry), intent(in) :: mesh_IO
!!        type(mesh_groups), intent(in) ::   mesh_group_IO
!!        type(binary_IO_flags), intent(inout) :: bin_flags
!!
!!      subroutine read_num_node_b(id_rank, bin_flags, mesh_IO)
!!      subroutine read_num_node_ele_b(id_rank, bin_flags, mesh_IO)
!!      subroutine read_geometry_data_b(id_rank, bin_flags, mesh_IO)
!!        type(binary_IO_flags), intent(inout) :: bin_flags
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine read_mesh_groups_b(bin_flags, mesh_group_IO)
!!        type(binary_IO_flags), intent(inout) :: bin_flags
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!
!!      subroutine write_filter_geometry_b                              &
!!     &         (id_rank, comm_IO, nod_IO, bin_flags)
!!        type(binary_IO_flags), intent(inout) :: bin_flags
!!        type(communication_table), intent(in) :: comm_IO
!!        type(node_data), intent(in) :: nod_IO
!!        type(binary_IO_flags), intent(inout) :: bin_flags
!!      subroutine read_filter_geometry_b                               &
!!     &         (id_rank, bin_flags, comm_IO, nod_IO)
!!        type(binary_IO_flags), intent(inout) :: bin_flags
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
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_geometry_data_b(id_rank, mesh_IO, bin_flags)
!
      use domain_data_IO_b
      use node_geometry_IO_b
      use element_connect_IO_b
!
      integer, intent(in) :: id_rank
      type(mesh_geometry), intent(in) :: mesh_IO
      type(binary_IO_flags), intent(inout) :: bin_flags
!
!
      call write_domain_info_b(id_rank, mesh_IO%nod_comm, bin_flags)
      if(bin_flags%ierr_IO .ne. 0) return
!
      call write_geometry_info_b(mesh_IO%node, bin_flags)
      if(bin_flags%ierr_IO .ne. 0) return
      call write_element_info_b(mesh_IO%ele, bin_flags)
      if(bin_flags%ierr_IO .ne. 0) return
!
      call write_import_data_b(mesh_IO%nod_comm, bin_flags)
      if(bin_flags%ierr_IO .ne. 0) return
      call write_export_data_b(mesh_IO%nod_comm, bin_flags)
      if(bin_flags%ierr_IO .ne. 0) return
!
      end subroutine write_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine write_mesh_groups_b(mesh_group_IO, bin_flags)
!
      use groups_IO_b
!
      type(mesh_groups), intent(in) ::   mesh_group_IO
      type(binary_IO_flags), intent(inout) :: bin_flags
!
!
!   write node group
      call write_grp_data_b(mesh_group_IO%nod_grp, bin_flags)
      if(bin_flags%ierr_IO .ne. 0) return
!  write element group
      call write_grp_data_b(mesh_group_IO%ele_grp, bin_flags)
      if(bin_flags%ierr_IO .ne. 0) return
!  write surface group
      call write_surf_grp_data_b(mesh_group_IO%surf_grp, bin_flags)
      if(bin_flags%ierr_IO .ne. 0) return
!
      end subroutine write_mesh_groups_b
!
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_b                                &
     &         (id_rank, comm_IO, nod_IO, bin_flags)
!
      use domain_data_IO_b
      use node_geometry_IO_b
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(node_data), intent(in) :: nod_IO
      type(binary_IO_flags), intent(inout) :: bin_flags
!
!
      call write_domain_info_b(id_rank, comm_IO, bin_flags)
      if(bin_flags%ierr_IO .ne. 0) return
!
      call write_geometry_info_b(nod_IO, bin_flags)
      if(bin_flags%ierr_IO .ne. 0) return
!
      call write_import_data_b(comm_IO, bin_flags)
      if(bin_flags%ierr_IO .ne. 0) return
      call write_export_data_b(comm_IO, bin_flags)
      if(bin_flags%ierr_IO .ne. 0) return
!
      end subroutine write_filter_geometry_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_num_node_b(id_rank, bin_flags, mesh_IO)
!
      use domain_data_IO_b
      use node_geometry_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_flags), intent(inout) :: bin_flags
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call read_domain_info_b(id_rank, bin_flags, mesh_IO%nod_comm)
      if(bin_flags%ierr_IO .ne. 0) return
!
      call read_number_of_node_b(bin_flags, mesh_IO%node)
!
      end subroutine read_num_node_b
!
!------------------------------------------------------------------
!
      subroutine read_num_node_ele_b(id_rank, bin_flags, mesh_IO)
!
      use domain_data_IO_b
      use node_geometry_IO_b
      use element_connect_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_flags), intent(inout) :: bin_flags
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call read_num_node_b(id_rank, bin_flags, mesh_IO)
      call read_geometry_info_b(bin_flags, mesh_IO%node)
      if(bin_flags%ierr_IO .ne. 0) return
!
!  ----  read element data -------
!
      call read_number_of_element_b(bin_flags, mesh_IO%ele)
!
      end subroutine read_num_node_ele_b
!
!------------------------------------------------------------------
!
      subroutine read_geometry_data_b(id_rank, bin_flags, mesh_IO)
!
      use domain_data_IO_b
      use node_geometry_IO_b
      use element_connect_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_flags), intent(inout) :: bin_flags
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call read_num_node_ele_b(id_rank, bin_flags, mesh_IO)
      if(bin_flags%ierr_IO .ne. 0) return
!
!  ----  read element data -------
!
      call read_element_info_b(bin_flags, mesh_IO%ele)
      if(bin_flags%ierr_IO .ne. 0) return
!
! ----  import & export 
!
      call read_import_data_b(bin_flags, mesh_IO%nod_comm)
      if(bin_flags%ierr_IO .ne. 0) return
!
      call read_export_data_b(bin_flags, mesh_IO%nod_comm)
!
      end subroutine read_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine read_mesh_groups_b(bin_flags, mesh_group_IO)
!
      use groups_IO_b
!
      type(binary_IO_flags), intent(inout) :: bin_flags
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   read node group
      call read_group_data_b(bin_flags, mesh_group_IO%nod_grp)
      if(bin_flags%ierr_IO .ne. 0) return
!   read element group
      call read_group_data_b(bin_flags, mesh_group_IO%ele_grp)
      if(bin_flags%ierr_IO .ne. 0) return
!   read surface group
      call read_surf_grp_data_b(bin_flags, mesh_group_IO%surf_grp)
!
      end subroutine read_mesh_groups_b
!
!------------------------------------------------------------------
!
      subroutine read_filter_geometry_b                                 &
     &         (id_rank, bin_flags, comm_IO, nod_IO)
!
      use domain_data_IO_b
      use node_geometry_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_flags), intent(inout) :: bin_flags
      type(communication_table), intent(inout) :: comm_IO
      type(node_data), intent(inout) :: nod_IO
!
!
      call read_domain_info_b(id_rank, bin_flags, comm_IO)
      if(bin_flags%ierr_IO .ne. 0) return
!
      call read_number_of_node_b(bin_flags, nod_IO)
      if(bin_flags%ierr_IO .ne. 0) return
!
      call read_geometry_info_b(bin_flags, nod_IO)
      if(bin_flags%ierr_IO .ne. 0) return
!
! ----  import & export 
!
      call read_import_data_b(bin_flags, comm_IO)
      if(bin_flags%ierr_IO .ne. 0) return
      call read_export_data_b(bin_flags, comm_IO)
!
      end subroutine read_filter_geometry_b
!
!------------------------------------------------------------------
!
      end module mesh_data_IO_b
