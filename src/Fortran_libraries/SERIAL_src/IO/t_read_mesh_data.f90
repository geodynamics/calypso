!>@file   t_read_mesh_data.f90
!!@brief  module t_read_mesh_data
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui
!
!>@brief Data arry for mesh_data_IO
!!
!!@verbatim
!!      subroutine alloc_multi_mesh_data_IO(nloop, mesh_IO)
!!      subroutine alloc_multi_mesh_group_IO(mesh_IO, mesh_grp_IO)
!!      subroutine dealloc_multi_mesh_data_IO(mesh_IO)
!!      subroutine dealloc_multi_mesh_group_IO(mesh_grp_IO)
!!        type(mul_mesh_geometry), intent(inout) :: mesh_IO
!!        type(mul_mesh_data), intent(inout) :: mesh_grp_IO
!!@endverbatim
!
      module t_read_mesh_data
!
      use m_precision
      use t_comm_table
      use t_geometry_data
      use t_group_data
      use t_surf_edge_IO
!
      implicit  none
!
!>
      type surf_edge_IO_file
!>        data structure for communication table IO
        type(communication_table) :: comm
!>        structure for node data IO (position)
        type(node_data) ::    node
!>        structure for element data IO (connectivity)
        type(element_data) :: ele
!>        structure for edge andsurface data IO
        type(surf_edge_IO_data) :: sfed
      end type surf_edge_IO_file
!
!
!>     Structure for grid data
!>        (position, connectivity, and communication)
      type mul_mesh_geometry
!>      Number of subdomains in each process
        integer(kind = kint) :: nloop_IO
!
!>     Structure for node communication
        type(communication_table), allocatable :: nod_comm(:)
!>     Structure for node position
        type(node_data), allocatable ::           node(:)
!>     Structure for element position and connectivity
        type(element_data), allocatable ::        ele(:)
      end type mul_mesh_geometry
!
!>     Structure for group data (node, element, surface, and infinity)
      type mul_mesh_groups
!>     Structure for node group
        type (group_data), allocatable ::          nod_grp(:)
!>     Structure for element group
        type (group_data), allocatable ::          ele_grp(:)
!>     Structure for surface group
        type (surface_group_data), allocatable :: surf_grp(:)
      end type mul_mesh_groups
!
!>     Structure for mesh data
!>        (position, connectivity, group, and communication)
      type mul_mesh_data
!>     Structure for grid data
        type(mul_mesh_geometry) :: mesh
!>     Structure for group data
        type(mul_mesh_groups) ::   group
      end type mul_mesh_data
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine alloc_multi_mesh_data_IO(nloop, mesh_IO)
!
      integer(kind = kint), intent(in) :: nloop
      type(mul_mesh_geometry), intent(inout) :: mesh_IO
!
      mesh_IO%nloop_IO = nloop
      allocate(mesh_IO%nod_comm(mesh_IO%nloop_IO))
      allocate(mesh_IO%node(mesh_IO%nloop_IO))
      allocate(mesh_IO%ele(mesh_IO%nloop_IO))
!
      end subroutine alloc_multi_mesh_data_IO
!
!------------------------------------------------------------------
!
      subroutine alloc_multi_mesh_group_IO(mesh_IO, mesh_grp_IO)
!
      type(mul_mesh_geometry), intent(in) :: mesh_IO
      type(mul_mesh_groups), intent(inout) :: mesh_grp_IO
!
      allocate(mesh_grp_IO%nod_grp(mesh_IO%nloop_IO))
      allocate(mesh_grp_IO%ele_grp(mesh_IO%nloop_IO))
      allocate(mesh_grp_IO%surf_grp(mesh_IO%nloop_IO))
!
      end subroutine alloc_multi_mesh_group_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_multi_mesh_data_IO(mesh_IO)
!
      type(mul_mesh_geometry), intent(inout) :: mesh_IO
!
      deallocate(mesh_IO%nod_comm)
      deallocate(mesh_IO%node)
      deallocate(mesh_IO%ele)
!
      end subroutine dealloc_multi_mesh_data_IO
!
!------------------------------------------------------------------
!
      subroutine dealloc_multi_mesh_group_IO(mesh_grp_IO)
!
      type(mul_mesh_groups), intent(inout) :: mesh_grp_IO
!
      deallocate(mesh_grp_IO%nod_grp)
      deallocate(mesh_grp_IO%ele_grp)
      deallocate(mesh_grp_IO%surf_grp)
!
      end subroutine dealloc_multi_mesh_group_IO
!
!------------------------------------------------------------------
!
      end module t_read_mesh_data
