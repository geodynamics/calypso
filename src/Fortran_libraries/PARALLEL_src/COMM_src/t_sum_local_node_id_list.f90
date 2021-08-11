!>@file   t_sum_local_node_id_list.f90
!!@brief  module t_sum_local_node_id_list
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!> @brief Set belonged element list for each node
!!
!!@verbatim
!!      subroutine dealloc_sum_of_local_id_list(sum_list)
!!
!!      subroutine set_ele_id_4_node_sum_order(node, ele, inod_dbl,     &
!!     &                                       neib_ele, sum_list)
!!      subroutine set_surf_id_4_node_sum_order(node, surf, inod_dbl,   &
!!     &                                        neib_ele, sum_list)
!!      subroutine set_edge_id_4_node_sum_order(node, edge, inod_dbl,   &
!!     &                                        neib_ele, sum_list)
!!        type(node_data),        intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        type(element_around_node), intent(inout) :: neib_ele
!!        type(sum_of_local_id_list), intent(inout) :: sum_list
!!@endverbatim
!
      module t_sum_local_node_id_list
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_para_double_numbering
      use t_next_node_ele_4_node
!
      implicit none
!
      type sum_of_local_id_list
        integer(kind = kint), allocatable :: isum_neib(:)
        integer(kind = kint), allocatable :: isum_ele(:)
      end type sum_of_local_id_list
!
      private :: alloc_sum_of_local_id_list
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_sum_of_local_id_list(sum_list)
!
      type(sum_of_local_id_list), intent(inout) :: sum_list
!
!
      deallocate(sum_list%isum_neib)
      deallocate(sum_list%isum_ele)
!
      end subroutine dealloc_sum_of_local_id_list
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_ele_id_4_node_sum_order(node, ele, inod_dbl,       &
     &                                       neib_ele, sum_list)
!
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(node_data),              intent(in) :: node
      type(element_data),           intent(in) :: ele
      type(node_ele_double_number), intent(in) :: inod_dbl
!
      type(element_around_node), intent(inout) :: neib_ele
      type(sum_of_local_id_list), intent(inout) :: sum_list
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
      call alloc_sum_of_local_id_list(ele%numele, neib_ele%ntot,        &
     &                                sum_list)
!
      call set_iele_4_node_sum_order(node%numnod, inod_dbl%index,       &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ione, ele%numele,         &
     &    neib_ele%ntot, neib_ele%istack_4_node, neib_ele%nele_4_node,  &
     &    neib_ele%iele_4_node, neib_ele%iconn_4_node,                  &
     &    sum_list%isum_neib, sum_list%isum_ele)
!
      end subroutine set_ele_id_4_node_sum_order
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_id_4_node_sum_order(node, surf, inod_dbl,     &
     &                                        neib_ele, sum_list)
!
      use t_surface_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(node_data),              intent(in) :: node
      type(surface_data),           intent(in) :: surf
      type(node_ele_double_number), intent(in) :: inod_dbl
!
      type(element_around_node), intent(inout) :: neib_ele
      type(sum_of_local_id_list), intent(inout) :: sum_list
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
      call alloc_sum_of_local_id_list(surf%numsurf, neib_ele%ntot,      &
     &                                sum_list)
!
      call set_iele_4_node_sum_order                                    &
     &   (node%numnod, inod_dbl%index, surf%numsurf, surf%nnod_4_surf,  &
     &    surf%ie_surf, ione, surf%numsurf,                             &
     &    neib_ele%ntot, neib_ele%istack_4_node, neib_ele%nele_4_node,  &
     &    neib_ele%iele_4_node, neib_ele%iconn_4_node,                  &
     &    sum_list%isum_neib, sum_list%isum_ele)
!
      end subroutine set_surf_id_4_node_sum_order
!
!-----------------------------------------------------------------------
!
      subroutine set_edge_id_4_node_sum_order(node, edge, inod_dbl,     &
     &                                        neib_ele, sum_list)
!
      use t_edge_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(node_ele_double_number), intent(in) :: inod_dbl
!
      type(element_around_node), intent(inout) :: neib_ele
      type(sum_of_local_id_list), intent(inout) :: sum_list
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
      call alloc_iele_belonged(neib_ele)
      call alloc_sum_of_local_id_list(edge%numedge, neib_ele%ntot,      &
     &                                sum_list)
!
      call set_iele_4_node_sum_order                                    &
     &   (node%numnod, inod_dbl%index, edge%numedge, edge%nnod_4_edge,  &
     &    edge%ie_edge, ione, edge%numedge,                             &
     &    neib_ele%ntot, neib_ele%istack_4_node, neib_ele%nele_4_node,  &
     &    neib_ele%iele_4_node, neib_ele%iconn_4_node,                  &
     &    sum_list%isum_neib, sum_list%isum_ele)
!
      end subroutine set_edge_id_4_node_sum_order
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_sum_of_local_id_list(numele, ntot_ele_4_node,    &
     &                                      sum_list)
!
      integer(kind = kint), intent(in) :: numele, ntot_ele_4_node
      type(sum_of_local_id_list), intent(inout) :: sum_list
!
!
      allocate(sum_list%isum_neib(ntot_ele_4_node))
      allocate(sum_list%isum_ele(numele))
!
      if(numele .gt. 0) then
!$omp parallel workshare
        sum_list%isum_ele(1:numele) = 0
!$omp end parallel workshare
      end if
      if(ntot_ele_4_node .gt. 0) then
!$omp parallel workshare
        sum_list%isum_neib(1:ntot_ele_4_node) = 0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_sum_of_local_id_list
!
!-----------------------------------------------------------------------
!
      end module t_sum_local_node_id_list
