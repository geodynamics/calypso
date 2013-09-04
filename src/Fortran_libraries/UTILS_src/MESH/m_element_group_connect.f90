!m_element_group_connect.f90
!     module m_element_group_connect
!
!> @brief connectivity data for element group items
!
!     Writteg by H.Matsui on Aug., 2006
!
!      subroutine allocate_surf_stack_4_ele_grp
!      subroutine allocate_surf_id_4_ele_grp
!
!      subroutine allocate_edge_stack_4_ele_grp
!      subroutine allocate_edge_id_4_ele_grp
!
!      subroutine allocate_node_stack_4_ele_grp
!      subroutine allocate_node_id_4_ele_grp
!
!      subroutine deallocate_surf_id_4_ele_grp
!      subroutine deallocate_edge_id_4_ele_grp
!      subroutine deallocate_node_id_4_ele_grp
!
      module m_element_group_connect
!
      use m_precision
!
      implicit  none
!
!
      integer(kind=kint) :: ntot_surf_ele_grp
!<   total number of surface for element group
      integer(kind=kint), allocatable, target :: nsurf_ele_grp(:)
!<   number of surface for each element group
      integer(kind=kint), allocatable, target :: isurf_stack_ele_grp(:)
!<   end number of surface for each element group
!
      integer(kind=kint), allocatable, target :: isurf_ele_grp(:)
!<   local surface ID for element group
!
!
      integer(kind=kint) :: ntot_edge_ele_grp
!<   total number of edge for element group
      integer(kind=kint), allocatable, target :: nedge_ele_grp(:)
!<   number of edge for each element group
      integer(kind=kint), allocatable, target :: iedge_stack_ele_grp(:)
!<   end number of edge for each element group
!
      integer(kind=kint), allocatable, target :: iedge_ele_grp(:)
!<   local edge ID for element group
!
!
      integer(kind=kint) :: ntot_node_ele_grp
!<   total number of node for element group
      integer(kind=kint), allocatable, target :: nnod_ele_grp(:)
!<   number of node for each element group
      integer(kind=kint), allocatable, target :: inod_stack_ele_grp(:)
!<   end number of node for each element group
!
      integer(kind=kint), allocatable, target :: inod_ele_grp(:)
!<   local node ID for element group
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_stack_4_ele_grp
!
      use m_element_group
!
      allocate(nsurf_ele_grp(num_mat))
      allocate(isurf_stack_ele_grp(0:num_mat))
!
      nsurf_ele_grp = 0
      isurf_stack_ele_grp = 0
!
      end subroutine allocate_surf_stack_4_ele_grp
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_id_4_ele_grp
!
      allocate(isurf_ele_grp(ntot_surf_ele_grp))
      isurf_ele_grp = 0
!
      end subroutine allocate_surf_id_4_ele_grp
!
!-----------------------------------------------------------------------
!
      subroutine allocate_edge_stack_4_ele_grp
!
      use m_element_group
!
      allocate(nedge_ele_grp(num_mat))
      allocate(iedge_stack_ele_grp(0:num_mat))
!
      nedge_ele_grp = 0
      iedge_stack_ele_grp = 0
!
      end subroutine allocate_edge_stack_4_ele_grp
!
!-----------------------------------------------------------------------
!
      subroutine allocate_edge_id_4_ele_grp
!
      allocate(iedge_ele_grp(ntot_edge_ele_grp))
      iedge_ele_grp = 0
!
      end subroutine allocate_edge_id_4_ele_grp
!
!-----------------------------------------------------------------------
!
      subroutine allocate_node_stack_4_ele_grp
!
      use m_element_group
!
      allocate(nnod_ele_grp(num_mat))
      allocate(inod_stack_ele_grp(0:num_mat))
!
      nnod_ele_grp = 0
      inod_stack_ele_grp = 0
!
      end subroutine allocate_node_stack_4_ele_grp
!
!-----------------------------------------------------------------------
!
      subroutine allocate_node_id_4_ele_grp
!
      allocate(inod_ele_grp(ntot_node_ele_grp))
      inod_ele_grp = 0
!
      end subroutine allocate_node_id_4_ele_grp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_id_4_ele_grp
!
      deallocate(nsurf_ele_grp, isurf_ele_grp)
      deallocate(isurf_stack_ele_grp)
!
      end subroutine deallocate_surf_id_4_ele_grp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_edge_id_4_ele_grp
!
      deallocate(nedge_ele_grp, iedge_ele_grp)
      deallocate(iedge_stack_ele_grp)
!
      end subroutine deallocate_edge_id_4_ele_grp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_node_id_4_ele_grp
!
!
      deallocate(inod_ele_grp, nnod_ele_grp)
      deallocate(inod_stack_ele_grp)
!
      end subroutine deallocate_node_id_4_ele_grp
!
!-----------------------------------------------------------------------
!
      end module m_element_group_connect
