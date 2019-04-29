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
!!      subroutine gz_write_geometry_data_b(id_rank, mesh_IO, bflag)
!!      subroutine gz_write_mesh_groups_b(mesh_group_IO, bflag)
!!        type(mesh_geometry), intent(in) :: mesh_IO
!!        type(mesh_groups), intent(in) ::   mesh_group_IO
!!        type(binary_IO_flags), intent(inout) :: bflag
!!
!!      subroutine gz_read_num_node_b(id_rank, bflag, mesh_IO)
!!      subroutine gz_read_num_node_ele_b(id_rank, bflag, mesh_IO)
!!      subroutine gz_read_geometry_data_b(id_rank, bflag, mesh_IO)
!!      subroutine gz_read_mesh_groups_b(bflag, mesh_group_IO)
!!        type(binary_IO_flags), intent(inout) :: bflag
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
      subroutine gz_write_geometry_data_b(id_rank, mesh_IO, bflag)
!
      use gz_domain_data_IO_b
      use gz_node_geometry_IO_b
      use gz_element_connect_IO_b
!
      integer, intent(in) :: id_rank
      type(mesh_geometry), intent(in) :: mesh_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call gz_write_domain_info_b(id_rank, mesh_IO%nod_comm, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_write_geometry_info_b(mesh_IO%node, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_element_info_b(mesh_IO%ele, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_write_import_data_b(mesh_IO%nod_comm, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_export_data_b(mesh_IO%nod_comm, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_mesh_groups_b(mesh_group_IO, bflag)
!
      use gz_groups_IO_b
!
      type(mesh_groups), intent(in) ::   mesh_group_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
!   write node group
      call gz_write_grp_data_b(mesh_group_IO%nod_grp, bflag)
      if(bflag%ierr_IO .ne. 0) return
!  write element group
      call gz_write_grp_data_b(mesh_group_IO%ele_grp, bflag)
      if(bflag%ierr_IO .ne. 0) return
!  write surface group
      call gz_write_surf_grp_data_b(mesh_group_IO%surf_grp, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_mesh_groups_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_num_node_b(id_rank, bflag, mesh_IO)
!
      use gz_domain_data_IO_b
      use gz_node_geometry_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_read_domain_info_b(id_rank, bflag, mesh_IO%nod_comm)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_number_of_node_b(bflag, mesh_IO%node)
!
      end subroutine gz_read_num_node_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_num_node_ele_b(id_rank, bflag, mesh_IO)
!
      use gz_domain_data_IO_b
      use gz_node_geometry_IO_b
      use gz_element_connect_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_read_num_node_b(id_rank, bflag, mesh_IO)
      if(bflag%ierr_IO .ne. 0) return
      call gz_read_geometry_info_b(bflag, mesh_IO%node)
      if(bflag%ierr_IO .ne. 0) return
!
!  ----  read element data -------
!
      call gz_read_number_of_element_b(bflag, mesh_IO%ele)
!
      end subroutine gz_read_num_node_ele_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_geometry_data_b(id_rank, bflag, mesh_IO)
!
      use gz_domain_data_IO_b
      use gz_node_geometry_IO_b
      use gz_element_connect_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_read_num_node_ele_b(id_rank, bflag, mesh_IO)
!
!  ----  read element data -------
!
      call gz_read_element_info_b(bflag, mesh_IO%ele)
      if(bflag%ierr_IO .ne. 0) return
!
! ----  import & export 
!
      call gz_read_import_data_b(bflag, mesh_IO%nod_comm)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_export_data_b(bflag, mesh_IO%nod_comm)
!
      end subroutine gz_read_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_mesh_groups_b(bflag, mesh_group_IO)
!
      use gz_groups_IO_b
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   read node group
      call gz_read_group_data_b(bflag, mesh_group_IO%nod_grp)
      if(bflag%ierr_IO .ne. 0) return
!   read element group
      call gz_read_group_data_b(bflag, mesh_group_IO%ele_grp)
      if(bflag%ierr_IO .ne. 0) return
!   read surface group
      call gz_read_surf_grp_data_b(bflag, mesh_group_IO%surf_grp)
!
      end subroutine gz_read_mesh_groups_b
!
!------------------------------------------------------------------
!
      end module gz_mesh_data_IO_b
