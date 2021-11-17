!>@file   set_element_id_4_node.f90
!!@brief  module set_element_id_4_node
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!> @brief Set belonged element list for each node
!!
!!@verbatim
!!      subroutine set_belonged_ele_and_next_nod                        &
!!     &         (mesh, neib_ele, neib_nod)
!!        type(mesh_geometry),       intent(in) :: mesh
!!        type(element_around_node), intent(inout) :: neib_ele
!!        type(next_nod_id_4_nod), intent(inout) :: neib_nod
!!      subroutine set_ele_id_4_node(node, ele, neib_ele)
!!      subroutine set_surf_id_4_node(node, surf, neib_ele)
!!      subroutine set_edge_id_4_node(node, edge, neib_ele)
!!        type(node_data),        intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(element_around_node), intent(inout) :: neib_ele
!!      subroutine set_layerd_ele_id_4_node(nnod, iele_start, iele_end, &
!!     &          node, ele, neib_ele)
!!        integer(kind = kint), intent(in) :: nnod
!!        integer(kind = kint), intent(in) :: iele_start, iele_end
!!        type(node_data),        intent(in) :: node
!!        type(element_data),       intent(in) :: ele
!!        type(element_around_node), intent(inout) :: neib_ele
!!      subroutine set_grouped_ele_id_4_node(nele_grp, iele_grp,        &
!!     &          node, ele, neib_ele)
!!        integer (kind=kint), intent(in) :: nele_grp
!!        integer (kind=kint), intent(in) :: iele_grp(nele_grp)
!!        type(node_data),        intent(in) :: node
!!        type(element_data),       intent(in) :: ele
!!        type(element_around_node), intent(inout) :: neib_ele
!!
!!      subroutine const_next_nod_id_4_node(node, ele, neib_ele,        &
!!     &                                    neib_nod)
!!        type(node_data),        intent(in) :: node
!!        type(element_data),       intent(in) :: ele
!!        type(element_around_node), intent(in) :: neib_ele
!!        type(next_nod_id_4_nod), intent(inout) :: neib_nod
!!@endverbatim
!
      module set_element_id_4_node
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_next_node_ele_4_node
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_belonged_ele_and_next_nod                          &
     &         (mesh, neib_ele, neib_nod)
!
      type(mesh_geometry),       intent(in) :: mesh
      type(element_around_node), intent(inout) :: neib_ele
      type(next_nod_id_4_nod), intent(inout) ::   neib_nod
!
!
      call set_ele_id_4_node(mesh%node, mesh%ele, neib_ele)
      call const_next_nod_id_4_node                                     &
     &   (mesh%node, mesh%ele, neib_ele, neib_nod)
!
      end subroutine set_belonged_ele_and_next_nod
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_ele_id_4_node(node, ele, neib_ele)
!
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(node_data),           intent(in) :: node
      type(element_data),        intent(in) :: ele
      type(element_around_node), intent(inout) :: neib_ele
!
!
      call alloc_numele_belonged(node%numnod, neib_ele)
!
      call count_iele_4_node(node%numnod, ele%numele, ele%nnod_4_ele,   &
     &    ele%ie, ione, ele%numele, neib_ele%nele_4_node)
      call s_cal_minmax_and_stacks(node%numnod,                         &
     &    neib_ele%nele_4_node, izero, neib_ele%istack_4_node,          &
     &    neib_ele%ntot, neib_ele%nmax, neib_ele%nmin)
!
!
      call alloc_iele_belonged(neib_ele)
!
      call set_iele_4_node(node%numnod, ele%numele,                     &
     &    ele%nnod_4_ele, ele%ie,  ione, ele%numele,                    &
     &    neib_ele%ntot, neib_ele%istack_4_node, neib_ele%nele_4_node,  &
     &    neib_ele%iele_4_node, neib_ele%iconn_4_node)
!
      end subroutine set_ele_id_4_node
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_id_4_node(node, surf, neib_ele)
!
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(element_around_node), intent(inout) :: neib_ele
!
!
      call alloc_numele_belonged(node%numnod, neib_ele)
!
      call count_iele_4_node(node%numnod, surf%numsurf,                 &
     &    surf%nnod_4_surf, surf%ie_surf, ione, surf%numsurf,           &
     &    neib_ele%nele_4_node)
      call s_cal_minmax_and_stacks(node%numnod,                         &
     &    neib_ele%nele_4_node, izero, neib_ele%istack_4_node,          &
     &    neib_ele%ntot, neib_ele%nmax, neib_ele%nmin)
!
!
      call alloc_iele_belonged(neib_ele)
!
      call set_iele_4_node(node%numnod, surf%numsurf,                   &
     &    surf%nnod_4_surf, surf%ie_surf,  ione, surf%numsurf,          &
     &    neib_ele%ntot, neib_ele%istack_4_node, neib_ele%nele_4_node,  &
     &    neib_ele%iele_4_node, neib_ele%iconn_4_node)
!
      end subroutine set_surf_id_4_node
!
!-----------------------------------------------------------------------
!
      subroutine set_edge_id_4_node(node, edge, neib_ele)
!
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(element_around_node), intent(inout) :: neib_ele
!
!
      call alloc_numele_belonged(node%numnod, neib_ele)
!
      call count_iele_4_node(node%numnod, edge%numedge,                 &
     &    edge%nnod_4_edge, edge%ie_edge, ione, edge%numedge,           &
     &    neib_ele%nele_4_node)
      call s_cal_minmax_and_stacks(node%numnod,                         &
     &    neib_ele%nele_4_node, izero, neib_ele%istack_4_node,          &
     &    neib_ele%ntot, neib_ele%nmax, neib_ele%nmin)
!
!
      call alloc_iele_belonged(neib_ele)
!
      call set_iele_4_node(node%numnod, edge%numedge,                   &
     &    edge%nnod_4_edge, edge%ie_edge,  ione, edge%numedge,          &
     &    neib_ele%ntot, neib_ele%istack_4_node, neib_ele%nele_4_node,  &
     &    neib_ele%iele_4_node, neib_ele%iconn_4_node)
!
      end subroutine set_edge_id_4_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_layerd_ele_id_4_node(nnod, iele_start, iele_end,   &
     &          node, ele, neib_ele)
!
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: iele_start, iele_end
      type(node_data),          intent(in) :: node
      type(element_data),       intent(in) :: ele
      type(element_around_node), intent(inout) :: neib_ele
!
!
      call alloc_numele_belonged(node%numnod, neib_ele)
!
      call count_iele_4_node(node%numnod, ele%numele, nnod, ele%ie,     &
     &    iele_start, iele_end, neib_ele%nele_4_node)
      call s_cal_minmax_and_stacks                                      &
     &   (node%numnod, neib_ele%nele_4_node, izero,                     &
     &    neib_ele%istack_4_node, neib_ele%ntot,                        &
     &    neib_ele%nmax, neib_ele%nmin)
!
!
      call alloc_iele_belonged(neib_ele)
!
      call set_iele_4_node(node%numnod, ele%numele, nnod, ele%ie,       &
     &    iele_start, iele_end, neib_ele%ntot,                          &
     &    neib_ele%istack_4_node, neib_ele%nele_4_node,                 &
     &    neib_ele%iele_4_node, neib_ele%iconn_4_node)
!
      end subroutine set_layerd_ele_id_4_node
!
!-----------------------------------------------------------------------
!
      subroutine set_grouped_ele_id_4_node(nele_grp, iele_grp,          &
     &          node, ele, neib_ele)
!
      use find_grp_ele_id_4_node
      use cal_minmax_and_stacks
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      type(node_data),          intent(in) :: node
      type(element_data),       intent(in) :: ele
!
      type(element_around_node), intent(inout) :: neib_ele
!
!
      call alloc_numele_belonged(node%numnod, neib_ele)
!
      call count_grp_iele_4_node                                        &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    nele_grp, iele_grp, neib_ele%nele_4_node)
      call s_cal_minmax_and_stacks                                      &
     &   (node%numnod, neib_ele%nele_4_node, izero,                     &
     &    neib_ele%istack_4_node, neib_ele%ntot,                        &
     &    neib_ele%nmax, neib_ele%nmin)
!
      call alloc_iele_belonged(neib_ele)
!
      call set_grp_iele_4_node(node%numnod, ele%numele,                 &
     &    ele%nnod_4_ele, ele%ie, nele_grp, iele_grp,                   &
     &    neib_ele%ntot, neib_ele%istack_4_node, neib_ele%nele_4_node,  &
     &    neib_ele%iele_4_node, neib_ele%iconn_4_node)
!
      end subroutine set_grouped_ele_id_4_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_next_nod_id_4_node(node, ele, neib_ele,          &
     &                                    neib_nod)
!
      use calypso_mpi
      use m_machine_parameter
!
      use find_node_4_group
      use cal_minmax_and_stacks
!
      type(node_data),          intent(in) :: node
      type(element_data),       intent(in) :: ele
      type(element_around_node), intent(in) :: neib_ele
      type(next_nod_id_4_nod), intent(inout) :: neib_nod
!
      type(work_to_find_next_node) :: find_WK1
!
!
      call alloc_num_next_node(node%numnod, neib_nod)
      call alloc_work_next_node(np_smp, node%istack_nod_smp,            &
     &    node%numnod, ele%nnod_4_ele, node%numnod,                     &
     &    neib_ele%istack_4_node,  find_WK1)
!
      call count_nod_4_grp_smp                                          &
     &   (np_smp, ele%numele, ele%nnod_4_ele, ele%ie,                   &
     &    node%istack_nod_smp, node%numnod, neib_ele%ntot,              &
     &    neib_ele%istack_4_node,  neib_ele%iele_4_node,                &
     &    neib_nod%nnod_next, find_WK1)
!
      call s_cal_minmax_and_stacks(node%numnod,                         &
     &    neib_nod%nnod_next, izero, neib_nod%istack_next,              &
     &    neib_nod%ntot, neib_nod%nmax, neib_nod%nmin)
!
!
      call alloc_inod_next_node(neib_nod)
!
      call set_nod_4_grp_smp(np_smp, ele%numele,                        &
     &    ele%nnod_4_ele, ele%ie, node%istack_nod_smp,                  &
     &    node%numnod, neib_ele%ntot, neib_ele%istack_4_node,           &
     &    neib_ele%iele_4_node, neib_nod%ntot, neib_nod%istack_next,    &
     &    neib_nod%nnod_next, neib_nod%inod_next,                       &
     &    neib_nod%iweight_next, find_WK1)
!
      call dealloc_work_next_node(find_WK1)
!
!
      neib_nod%iweight_next(1:neib_nod%ntot)                            &
     &     = - neib_nod%iweight_next(1:neib_nod%ntot)
!
      call move_myself_2_first_smp(np_smp, node%numnod,                 &
     &    neib_nod%ntot, node%istack_nod_smp, neib_nod%istack_next,     &
     &    neib_nod%inod_next, neib_nod%iweight_next)
!
      call sort_next_node_list_by_weight(np_smp, node%numnod,           &
     &    neib_nod%ntot, node%istack_nod_smp, neib_nod%istack_next,     &
     &    neib_nod%inod_next, neib_nod%iweight_next)
!
      neib_nod%iweight_next(1:neib_nod%ntot)                            &
     &     = - neib_nod%iweight_next(1:neib_nod%ntot)
!
      end subroutine const_next_nod_id_4_node
!
!-----------------------------------------------------------------------
!
      end module set_element_id_4_node
