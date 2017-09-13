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
!!      subroutine gz_write_geometry_data_b(my_rank_IO, mesh_IO)
!!      subroutine gz_write_mesh_groups_b(mesh_group_IO)
!!
!!      subroutine gz_read_num_node_b(my_rank_IO, mesh_IO, ierr)
!!      subroutine gz_read_num_node_ele_b(my_rank_IO, mesh_IO, ierr)
!!      subroutine gz_read_geometry_data_b(my_rank_IO, mesh_IO, ierr)
!!      subroutine gz_read_mesh_groups_b(mesh_group_IO)
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
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_write_geometry_data_b(my_rank_IO, mesh_IO)
!
      use gz_domain_data_IO_b
      use gz_node_geometry_IO_b
      use gz_element_connect_IO_b
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_write_domain_info_b(my_rank_IO, mesh_IO%nod_comm)
!
      call gz_write_geometry_info_b(mesh_IO%node)
      call gz_write_element_info_b(mesh_IO%ele)
!
      call gz_write_import_data_b(mesh_IO%nod_comm)
      call gz_write_export_data_b(mesh_IO%nod_comm)
!
      end subroutine gz_write_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_mesh_groups_b(mesh_group_IO)
!
      use gz_groups_IO_b
!
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   write node group
      call gz_write_grp_data_b(mesh_group_IO%nod_grp)
!  write element group
      call gz_write_grp_data_b(mesh_group_IO%ele_grp)
!  write surface group
      call gz_write_surf_grp_data_b(mesh_group_IO%surf_grp)
!
      end subroutine gz_write_mesh_groups_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_num_node_b(my_rank_IO, mesh_IO, ierr)
!
      use gz_domain_data_IO_b
      use gz_node_geometry_IO_b
!
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call gz_read_domain_info_b(my_rank_IO, mesh_IO%nod_comm, ierr)
      call gz_read_number_of_node_b(mesh_IO%node)
!
      end subroutine gz_read_num_node_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_num_node_ele_b(my_rank_IO, mesh_IO, ierr)
!
      use gz_domain_data_IO_b
      use gz_node_geometry_IO_b
      use gz_element_connect_IO_b
!
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call gz_read_num_node_b(my_rank_IO, mesh_IO, ierr)
      call gz_read_geometry_info_b(mesh_IO%node)
!
!  ----  read element data -------
!
      call gz_read_number_of_element_b(mesh_IO%ele)
!
      end subroutine gz_read_num_node_ele_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_geometry_data_b(my_rank_IO, mesh_IO, ierr)
!
      use gz_domain_data_IO_b
      use gz_node_geometry_IO_b
      use gz_element_connect_IO_b
!
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call gz_read_num_node_ele_b(my_rank_IO, mesh_IO, ierr)
!
!  ----  read element data -------
!
      call gz_read_element_info_b(mesh_IO%ele)
!
! ----  import & export 
!
      call gz_read_import_data_b(mesh_IO%nod_comm)
      call gz_read_export_data_b(mesh_IO%nod_comm)
!
      end subroutine gz_read_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_mesh_groups_b(mesh_group_IO)
!
      use gz_groups_IO_b
!
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   read node group
      call gz_read_group_data_b(mesh_group_IO%nod_grp)
!   read element group
      call gz_read_group_data_b(mesh_group_IO%ele_grp)
!   read surface group
      call gz_read_surf_grp_data_b(mesh_group_IO%surf_grp)
!
      end subroutine gz_read_mesh_groups_b
!
!------------------------------------------------------------------
!
      end module gz_mesh_data_IO_b
