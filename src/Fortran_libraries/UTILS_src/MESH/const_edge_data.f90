!const_edge_data.f90
!      module const_edge_data
!
!     Written by H. Matsui on Apr., 2006
!
!      subroutine construct_edge_data(my_rank)
!      subroutine const_element_list_4_edge
!      subroutine const_surface_list_4_edge
!
      module const_edge_data
!
      use m_precision
!
      use m_geometry_constants
      use m_geometry_parameter
!
      implicit none
!
      private :: construct_all_edge, construct_bc_edge
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine construct_edge_data(my_rank)
!
      use m_edge_hash
      use check_geometries
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call allocate_edge_hash(numnod, numsurf, nnod_4_edge)
!
      call construct_all_edge
!      call check_edge_data(my_rank)
!      call check_edge_hexa_data(my_rank)
!
!      call construct_bc_edge
!      call check_bc_edge(my_rank)
!
      call deallocate_edge_hash
!
      end subroutine construct_edge_data
!
!------------------------------------------------------------------
!
      subroutine const_element_list_4_edge
!
      use m_geometry_data
      use set_element_list_4_surface
!
!
      call allocate_ele_4_edge_num
      call count_ele_list_4_edge(numele, numedge, nedge_4_ele,          &
     &    iedge_4_ele, ntot_iele_4_edge, num_iele_4_edge,               &
     &    istack_iele_4_edge)
!
      call allocate_ele_4_edge_item
      call set_ele_list_4_edge(numele, numedge, nedge_4_ele,            &
     &    iedge_4_ele, ntot_iele_4_edge, num_iele_4_edge,               &
     &    istack_iele_4_edge, iele_4_edge)
!
      end subroutine const_element_list_4_edge
!
!------------------------------------------------------------------
!
      subroutine const_surface_list_4_edge
!
      use m_geometry_data
      use set_element_list_4_surface
!
!
      call allocate_surf_4_edge_num
      call count_ele_list_4_edge(numsurf, numedge, nedge_4_surf,        &
     &    iedge_4_sf, ntot_isurf_4_edge, num_isurf_4_edge,              &
     &    istack_isurf_4_edge)
!
      call allocate_surf_4_edge_item
      call set_ele_list_4_edge(numsurf, numedge, nedge_4_surf,          &
     &    iedge_4_sf, ntot_isurf_4_edge, num_isurf_4_edge,              &
     &    istack_isurf_4_edge, isurf_4_edge)
!
      end subroutine const_surface_list_4_edge
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine construct_all_edge
!
      use m_machine_parameter
      use m_geometry_data
!
      use set_edge_hash
      use mark_edge_hash
      use set_edge_data
!
      use check_geometries
!
!   set hash data for edge elements using sum of local node ID
!
      if (iflag_debug.eq.1) write(*,*) 'count_edge_hash_4_sf'
      call count_edge_hash_4_sf(numnod, numsurf, nnod_4_surf,           &
     &          nnod_4_edge, ie_surf)
!
      if (iflag_debug.eq.1) write(*,*) 'set_edge_hash_4_sf'
      call set_edge_hash_4_sf(numsurf, nnod_4_surf, ie_surf)
!
!
      if (iflag_debug.eq.1) write(*,*) 'mark_all_edges'
      call mark_all_edges(numsurf, nnod_4_surf, ie_surf)
!
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_edges'
      call count_num_edges(numedge)
!
      call allocate_edge_connect
!
      if (iflag_debug.eq.1) write(*,*) 'set_edges_connection'
      call set_edges_connection(numsurf, numedge, nnod_4_surf,          &
     &    nnod_4_edge, ie_surf, ie_edge, iedge_4_sf, node_on_edge_sf)
!
!
      call allocate_edge_4_ele
!
      if (iflag_debug.eq.1) write(*,*) 'set_edges_connect_4_ele'
      call set_edges_connect_4_ele(numele, numsurf, numedge,            &
     &    nnod_4_ele, nnod_4_edge, ie, iedge_4_sf,                      &
     &    ie_edge, iedge_4_ele)
!
      end subroutine construct_all_edge
!
!------------------------------------------------------------------
!
      subroutine construct_bc_edge
!
      use m_machine_parameter
      use m_geometry_data
!
      use set_edge_hash
      use mark_edge_hash
      use set_edge_data
!
!   set hash data for edge elements using sum of local node ID
!
      call cleear_edge_hash
!
      if (iflag_debug.eq.1) write(*,*) 'count_part_edge_hash_4_sf'
      call count_part_edge_hash_4_sf(numnod, numsurf, numsurf_iso,      &
     &          nnod_4_surf, nnod_4_edge, ie_surf, isf_isolate)
!
      if (iflag_debug.eq.1) write(*,*) 'set_part_edge_hash_4_sf'
      call set_part_edge_hash_4_sf(numsurf, numsurf_iso, nnod_4_surf,   &
     &    ie_surf, isf_isolate)
!
!
      if (iflag_debug.eq.1) write(*,*) 'mark_all_edges'
      call mark_all_edges(numsurf, nnod_4_surf, ie_surf)
!
!
      call count_num_edges(numedge_iso)
!
      call allocate_iso_edge
!
      if (iflag_debug.eq.1) write(*,*) 'set_part_edges'
      call set_part_edges(numsurf, numedge_iso,                         &
     &    iedge_4_sf, iedge_isolate)
!
      end subroutine construct_bc_edge
!
!------------------------------------------------------------------
!
      end module const_edge_data
