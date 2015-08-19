!>@file   m_element_id_4_node.f90
!!@brief  module m_element_id_4_node
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2006
!
!> @brief included element list for each node
!!
!!@verbatim
!!      subroutine set_ele_id_4_node
!!
!!      subroutine set_ele_id_4_node_comm
!!      subroutine set_surf_id_4_node
!!      subroutine set_edge_id_4_node
!!      subroutine set_layerd_ele_id_4_node(nnod, iele_start, iele_end)
!!      subroutine set_grouped_ele_id_4_node(nele_grp, iele_grp)
!!
!!      subroutine check_element_id_4_node(my_rank, numnod)
!!@endverbatim
!
      module m_element_id_4_node
!
      use m_precision
      use m_constants
      use t_next_node_ele_4_node
!
      implicit none
!
!
!>   Structure of included element list for each node
      type(element_around_node), save :: ele_4_nod1
!
!>   Structure of included element list for each node
!!     (Using for element communication table)
      type(element_around_node), save :: ele_4_nod_comm
!
!>   Structure of included surface list for each node
      type(element_around_node), save :: surf_4_nod1
!
!>   Structure of included surface list for each node
      type(element_around_node), save :: edge_4_nod1
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_ele_id_4_node
!
      use m_geometry_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
!
      call alloc_numele_belonged(node1%numnod, ele_4_nod1)
!
      call count_iele_4_node                                            &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    ione, ele1%numele, ele_4_nod1%nele_4_node)
      call s_cal_minmax_and_stacks(node1%numnod,                        &
     &    ele_4_nod1%nele_4_node, izero, ele_4_nod1%istack_4_node,      &
     &    ele_4_nod1%ntot, ele_4_nod1%nmax, ele_4_nod1%nmin)
!
!
      call alloc_iele_belonged(ele_4_nod1)
!
      call set_iele_4_node                                              &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    ione, ele1%numele, ele_4_nod1%ntot, ele_4_nod1%istack_4_node, &
     &    ele_4_nod1%nele_4_node, ele_4_nod1%iele_4_node,               &
     &    ele_4_nod1%iconn_4_node)
!
      end subroutine set_ele_id_4_node
!
!-----------------------------------------------------------------------
!
      subroutine set_ele_id_4_node_comm
!
      use m_geometry_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
!
      call alloc_numele_belonged(node1%numnod, ele_4_nod_comm)
!
      call count_iele_4_node                                            &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    ione, ele1%numele, ele_4_nod_comm%nele_4_node)
      call s_cal_minmax_and_stacks                                      &
     &   (node1%numnod, ele_4_nod_comm%nele_4_node,                     &
     &    izero, ele_4_nod_comm%istack_4_node, ele_4_nod_comm%ntot,     &
     &    ele_4_nod_comm%nmax, ele_4_nod_comm%nmin)
!
!
      call alloc_iele_belonged(ele_4_nod_comm)
!
      call set_iele_4_node(node1%numnod, ele1%numele, ele1%nnod_4_ele,  &
     &    ele1%ie, ione, ele1%numele,                                   &
     &    ele_4_nod_comm%ntot, ele_4_nod_comm%istack_4_node,            &
     &    ele_4_nod_comm%nele_4_node, ele_4_nod_comm%iele_4_node,       &
     &    ele_4_nod_comm%iconn_4_node)
!
      end subroutine set_ele_id_4_node_comm
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_id_4_node
!
      use m_geometry_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
!
      call alloc_numele_belonged(node1%numnod, surf_4_nod1)
!
      call count_iele_4_node(node1%numnod,                              &
     &   surf1%numsurf, surf1%nnod_4_surf, surf1%ie_surf,               &
     &    ione, surf1%numsurf, surf_4_nod1%nele_4_node)
      call s_cal_minmax_and_stacks(node1%numnod,                        &
     &    surf_4_nod1%nele_4_node, izero, surf_4_nod1%istack_4_node,    &
     &    surf_4_nod1%ntot, surf_4_nod1%nmax, surf_4_nod1%nmin)
!
!
      call alloc_iele_belonged(surf_4_nod1)
!
      call set_iele_4_node(node1%numnod, surf1%numsurf,                 &
     &    surf1%nnod_4_surf, surf1%ie_surf, ione, surf1%numsurf,        &
     &    surf_4_nod1%ntot, surf_4_nod1%istack_4_node,                  &
     &    surf_4_nod1%nele_4_node, surf_4_nod1%iele_4_node,             &
     &    surf_4_nod1%iconn_4_node)
!
      end subroutine set_surf_id_4_node
!
!-----------------------------------------------------------------------
!
      subroutine set_edge_id_4_node
!
      use m_geometry_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
!
      call alloc_numele_belonged(node1%numnod, edge_4_nod1)
!
      call count_iele_4_node (node1%numnod,                             &
     &    edge1%numedge, edge1%nnod_4_edge, edge1%ie_edge,              &
     &    ione, edge1%numedge, edge_4_nod1%nele_4_node)
      call s_cal_minmax_and_stacks(node1%numnod,                        &
     &    edge_4_nod1%nele_4_node, izero, edge_4_nod1%istack_4_node,    &
     &    edge_4_nod1%ntot, edge_4_nod1%nmax, edge_4_nod1%nmin)
!
!
      call alloc_iele_belonged(edge_4_nod1)
!
      call set_iele_4_node(node1%numnod, edge1%numedge,                 &
     &    edge1%nnod_4_edge, edge1%ie_edge, ione, edge1%numedge,        &
     &    edge_4_nod1%ntot, edge_4_nod1%istack_4_node,                  &
     &    edge_4_nod1%nele_4_node, edge_4_nod1%iele_4_node,             &
     &    edge_4_nod1%iconn_4_node)
!
      end subroutine set_edge_id_4_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_layerd_ele_id_4_node(nnod, iele_start, iele_end)
!
      use m_geometry_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: nnod, iele_start, iele_end
!
!
      call alloc_numele_belonged(node1%numnod, ele_4_nod1)
!
      call count_iele_4_node(node1%numnod, ele1%numele, nnod, ele1%ie,  &
     &    iele_start, iele_end, ele_4_nod1%nele_4_node)
      call s_cal_minmax_and_stacks                                      &
     &   (node1%numnod, ele_4_nod1%nele_4_node, izero,                  &
     &    ele_4_nod1%istack_4_node, ele_4_nod1%ntot,                    &
     &    ele_4_nod1%nmax, ele_4_nod1%nmin)
!
!
      call alloc_iele_belonged(ele_4_nod1)
!
      call set_iele_4_node(node1%numnod, ele1%numele, nnod, ele1%ie,    &
     &    iele_start, iele_end,                                         &
     &    ele_4_nod1%ntot, ele_4_nod1%istack_4_node,                    &
     &    ele_4_nod1%nele_4_node, ele_4_nod1%iele_4_node,               &
     &    ele_4_nod1%iconn_4_node)
!
      end subroutine set_layerd_ele_id_4_node
!
!-----------------------------------------------------------------------
!
      subroutine set_grouped_ele_id_4_node(nele_grp, iele_grp)
!
      use m_geometry_data
      use find_grp_ele_id_4_node
      use cal_minmax_and_stacks
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
!
!
      call alloc_numele_belonged(node1%numnod, ele_4_nod1)
!
      call count_grp_iele_4_node                                        &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    nele_grp, iele_grp, ele_4_nod1%nele_4_node)
      call s_cal_minmax_and_stacks                                      &
     &   (node1%numnod, ele_4_nod1%nele_4_node, izero,                  &
     &    ele_4_nod1%istack_4_node, ele_4_nod1%ntot,                    &
     &    ele_4_nod1%nmax, ele_4_nod1%nmin)
!
      call alloc_iele_belonged(ele_4_nod1)
!
      call set_grp_iele_4_node                                          &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    nele_grp, iele_grp,                                           &
     &    ele_4_nod1%ntot, ele_4_nod1%istack_4_node,                    &
     &    ele_4_nod1%nele_4_node, ele_4_nod1%iele_4_node,               &
     &    ele_4_nod1%iconn_4_node)
!
      end subroutine set_grouped_ele_id_4_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_element_id_4_node(my_rank, numnod)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
!
      call check_ele_id_4_node_type(my_rank, numnod, ele_4_nod1)
!
      end subroutine check_element_id_4_node
!
!-----------------------------------------------------------------------
!
      end module m_element_id_4_node
