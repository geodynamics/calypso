!>@file   m_belonged_element_4_node.f90
!!@brief  module m_belonged_element_4_node
!!
!!@author H. Matsui
!!@date Programmed in June, 2015
!
!> @brief Belonged element list for each node
!!
!!@verbatim
!!      subroutine set_belonged_id_4_node_1st
!!      subroutine belonged_ele_id_4_node_1(host_ele)
!!      subroutine belonged_surf_id_4_node_1(host_surf)
!!      subroutine belonged_edge_id_4_node_1(host_edge)
!!
!!      subroutine dealloc_belonged_id_4_node1st
!!@endverbatim
!
      module m_belonged_element_4_node
!
      use m_precision
      use t_belonged_element_4_node
      use t_next_node_ele_4_node
!
      implicit none
!
!
      type(belonged_table), save :: blng_tbls
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_belonged_id_4_node_1st
!
!
      call belonged_ele_id_4_node_1(blng_tbls%host_ele)
      call belonged_surf_id_4_node_1(blng_tbls%host_surf)
      call belonged_edge_id_4_node_1(blng_tbls%host_edge)
!
      end subroutine set_belonged_id_4_node_1st
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_belonged_id_4_node1st
!
!
      call dealloc_iele_belonged(blng_tbls%host_ele)
      call dealloc_iele_belonged(blng_tbls%host_surf)
      call dealloc_iele_belonged(blng_tbls%host_edge)
!
      end subroutine dealloc_belonged_id_4_node1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine belonged_ele_id_4_node_1(host_ele)
!
      use m_geometry_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(element_around_node), intent(inout) :: host_ele
!
!
      call alloc_numele_belonged(node1%numnod, host_ele)
!
      call count_belonged_ele_4_node(node1%numnod, ele1%numele,         &
     &    ele1%nnod_4_ele, ele1%ie, ione, ele1%numele,                  &
     &    host_ele%nele_4_node)
      call s_cal_minmax_and_stacks(node1%numnod,                        &
     &    host_ele%nele_4_node, izero, host_ele%istack_4_node,          &
     &    host_ele%ntot, host_ele%nmax, host_ele%nmin)
!
!
      call alloc_iele_belonged(host_ele)
!
      call set_belonged_ele_4_node(node1%numnod, ele1%numele,           &
     &    ele1%nnod_4_ele, ele1%ie, ione, ele1%numele,                  &
     &    host_ele%ntot, host_ele%istack_4_node, host_ele%nele_4_node,  &
     &    host_ele%iele_4_node, host_ele%iconn_4_node)
!
      end subroutine belonged_ele_id_4_node_1
!
!-----------------------------------------------------------------------
!
      subroutine belonged_surf_id_4_node_1(host_surf)
!
      use m_geometry_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(element_around_node), intent(inout) :: host_surf
!
!
      call alloc_numele_belonged(node1%numnod, host_surf)
!
      call count_belonged_ele_4_node(node1%numnod, surf1%numsurf,       &
     &    surf1%nnod_4_surf, surf1%ie_surf, ione, surf1%numsurf,        &
     &    host_surf%nele_4_node)
      call s_cal_minmax_and_stacks(node1%numnod,                        &
     &    host_surf%nele_4_node, izero, host_surf%istack_4_node,        &
     &    host_surf%ntot, host_surf%nmax, host_surf%nmin)
!
!
      call alloc_iele_belonged(host_surf)
!
      call set_belonged_ele_4_node(node1%numnod, surf1%numsurf,         &
     &    surf1%nnod_4_surf, surf1%ie_surf,  ione, surf1%numsurf,       &
     &    host_surf%ntot, host_surf%istack_4_node,                      &
     &    host_surf%nele_4_node, host_surf%iele_4_node,                 &
     &    host_surf%iconn_4_node)
!
      end subroutine belonged_surf_id_4_node_1
!
!-----------------------------------------------------------------------
!
      subroutine belonged_edge_id_4_node_1(host_edge)
!
      use m_geometry_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(element_around_node), intent(inout) :: host_edge
!
!
      call alloc_numele_belonged(node1%numnod, host_edge)
!
      call count_belonged_ele_4_node(node1%numnod, edge1%numedge,       &
     &    edge1%nnod_4_edge, edge1%ie_edge, ione, edge1%numedge,        &
     &    host_edge%nele_4_node)
      call s_cal_minmax_and_stacks(node1%numnod,                        &
     &    host_edge%nele_4_node, izero, host_edge%istack_4_node,        &
     &    host_edge%ntot, host_edge%nmax, host_edge%nmin)
!
!
      call alloc_iele_belonged(host_edge)
!
      call set_belonged_ele_4_node(node1%numnod, edge1%numedge,         &
     &    edge1%nnod_4_edge, edge1%ie_edge,  ione, edge1%numedge,       &
     &    host_edge%ntot, host_edge%istack_4_node,                      &
     &    host_edge%nele_4_node, host_edge%iele_4_node,                 &
     &    host_edge%iconn_4_node)
!
      end subroutine belonged_edge_id_4_node_1
!
!-----------------------------------------------------------------------
!
      end module m_belonged_element_4_node
