!>@file   t_belonged_element_4_node.f90
!!@brief  module t_belonged_element_4_node
!!
!!@author H. Matsui
!!@date Programmed in June, 2015
!
!> @brief Belonged element list for each node
!!
!!@verbatim
!!      subroutine set_belonged_id_4_node(mesh, surf, edge, belongs)
!!      subroutine dealloc_belonged_id_4_node(belongs)
!!@endverbatim
!
      module t_belonged_element_4_node
!
      use m_precision
      use t_next_node_ele_4_node
!
      implicit none
!
!
      type belonged_table
!>   Structure of belonged element list for each node
        type(element_around_node) :: blng_ele
!>   Structure of belonged surface list for each node
        type(element_around_node) :: blng_surf
!>   Structure of belonged edge list for each node
        type(element_around_node) :: blng_edge
!
!>   Structure of belonged element list for each node
        type(element_around_node) :: host_ele
!>   Structure of belonged surface list for each node
        type(element_around_node) :: host_surf
!>   Structure of belonged edge list for each node
        type(element_around_node) :: host_edge
      end type belonged_table
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_belonged_id_4_node(mesh, surf, edge, belongs)
!
      use t_mesh_data
      use t_surface_data
      use t_edge_data
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(belonged_table), intent(inout) :: belongs
!
!
      call belonged_ele_id_4_node(mesh, belongs%host_ele)
      call belonged_surf_id_4_node(mesh, surf, belongs%host_surf)
      call belonged_edge_id_4_node(mesh, edge, belongs%host_edge)
!
      end subroutine set_belonged_id_4_node
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_belonged_id_4_node(belongs)
!
      type(belonged_table), intent(inout) :: belongs
!
!
      call dealloc_iele_belonged(belongs%host_ele)
      call dealloc_iele_belonged(belongs%host_surf)
      call dealloc_iele_belonged(belongs%host_edge)
!
      end subroutine dealloc_belonged_id_4_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine belonged_ele_id_4_node(mesh, host_ele)
!
      use t_mesh_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(mesh_geometry), intent(in) :: mesh
      type(element_around_node), intent(inout) :: host_ele
!
!
      call alloc_numele_belonged(mesh%node%numnod, host_ele)
!
      call count_belonged_ele_4_node(mesh%node%numnod, mesh%ele%numele, &
     &    mesh%ele%nnod_4_ele, mesh%ele%ie, ione, mesh%ele%numele,      &
     &    host_ele%nele_4_node)
      call s_cal_minmax_and_stacks(mesh%node%numnod,                    &
     &    host_ele%nele_4_node, izero, host_ele%istack_4_node,          &
     &    host_ele%ntot, host_ele%nmax, host_ele%nmin)
!
!
      call alloc_iele_belonged(host_ele)
!
      call set_belonged_ele_4_node(mesh%node%numnod, mesh%ele%numele,   &
     &    mesh%ele%nnod_4_ele, mesh%ele%ie,  ione, mesh%ele%numele,     &
     &    host_ele%ntot, host_ele%istack_4_node, host_ele%nele_4_node,  &
     &    host_ele%iele_4_node, host_ele%iconn_4_node)
!
      end subroutine belonged_ele_id_4_node
!
!-----------------------------------------------------------------------
!
      subroutine belonged_surf_id_4_node(mesh, surf, host_surf)
!
      use t_mesh_data
      use t_surface_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) :: surf
      type(element_around_node), intent(inout) :: host_surf
!
!
      call alloc_numele_belonged(mesh%node%numnod, host_surf)
!
      call count_belonged_ele_4_node(mesh%node%numnod, surf%numsurf,    &
     &    surf%nnod_4_surf, surf%ie_surf, ione, surf%numsurf,           &
     &    host_surf%nele_4_node)
      call s_cal_minmax_and_stacks(mesh%node%numnod,                    &
     &    host_surf%nele_4_node, izero, host_surf%istack_4_node,        &
     &    host_surf%ntot, host_surf%nmax, host_surf%nmin)
!
!
      call alloc_iele_belonged(host_surf)
!
      call set_belonged_ele_4_node(mesh%node%numnod, surf%numsurf,      &
     &    surf%nnod_4_surf, surf%ie_surf,  ione, surf%numsurf,          &
     &    host_surf%ntot, host_surf%istack_4_node,                      &
     &    host_surf%nele_4_node, host_surf%iele_4_node,                 &
     &    host_surf%iconn_4_node)
!
      end subroutine belonged_surf_id_4_node
!
!-----------------------------------------------------------------------
!
      subroutine belonged_edge_id_4_node(mesh, edge, host_edge)
!
      use t_mesh_data
      use t_edge_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(mesh_geometry), intent(in) :: mesh
      type(edge_data), intent(in) :: edge
      type(element_around_node), intent(inout) :: host_edge
!
!
      call alloc_numele_belonged(mesh%node%numnod, host_edge)
!
      call count_belonged_ele_4_node(mesh%node%numnod, edge%numedge,    &
     &    edge%nnod_4_edge, edge%ie_edge, ione, edge%numedge,           &
     &    host_edge%nele_4_node)
      call s_cal_minmax_and_stacks(mesh%node%numnod,                    &
     &    host_edge%nele_4_node, izero, host_edge%istack_4_node,        &
     &    host_edge%ntot, host_edge%nmax, host_edge%nmin)
!
!
      call alloc_iele_belonged(host_edge)
!
      call set_belonged_ele_4_node(mesh%node%numnod, edge%numedge,      &
     &    edge%nnod_4_edge, edge%ie_edge,  ione, edge%numedge,          &
     &    host_edge%ntot, host_edge%istack_4_node,                      &
     &    host_edge%nele_4_node, host_edge%iele_4_node,                 &
     &    host_edge%iconn_4_node)
!
      end subroutine belonged_edge_id_4_node
!
!-----------------------------------------------------------------------
!
      end module t_belonged_element_4_node
