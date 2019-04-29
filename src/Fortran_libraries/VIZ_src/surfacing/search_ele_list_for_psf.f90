!search_ele_list_for_psf.f90
!      module search_ele_list_for_psf
!
!      Written by H. Matsui on June, 2006
!
!!      subroutine set_search_mesh_list_4_psf(num_psf,                  &
!!     &          mesh, ele_mesh, group, psf_param, psf_search)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(psf_parameters), intent(in) :: psf_param(num_psf)
!!        type(psf_search_lists), intent(inout) :: psf_search(num_psf)
!
      module search_ele_list_for_psf
!
      use m_precision
!
      use m_machine_parameter
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_psf_geometry_list
!
      implicit none
!
      private :: set_searched_element_list_4_psf
      private :: set_searched_surface_list_4_psf
      private :: set_searched_edge_list_4_psf
      private :: set_searched_node_list_4_psf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_search_mesh_list_4_psf(num_psf,                    &
     &          mesh, ele_mesh, group, psf_param, psf_search)
!
      use m_geometry_constants
      use t_psf_patch_data
!
      integer(kind = kint), intent(in) :: num_psf
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_groups), intent(in) :: group
!
      type(psf_parameters), intent(in) :: psf_param(num_psf)
!
      type(psf_search_lists), intent(inout) :: psf_search(num_psf)
!
!
      call set_searched_element_list_4_psf                              &
     &   (num_psf, mesh%ele, group%ele_grp, psf_param, psf_search)
!
      call set_searched_surface_list_4_psf                              &
     &   (num_psf, mesh%ele, ele_mesh%surf, psf_search)
!
      call set_searched_edge_list_4_psf                                 &
     &   (num_psf, ele_mesh%surf, ele_mesh%edge, psf_search)
!
      call set_searched_node_list_4_psf                                 &
     &   (num_psf, mesh%node, ele_mesh%edge, psf_search)
!
      end subroutine set_search_mesh_list_4_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_searched_element_list_4_psf                        &
     &         (num_psf, ele, ele_grp, psf_param, psf_search)
!
      use t_psf_patch_data
      use set_element_list_for_psf
!
      integer(kind = kint), intent(in) :: num_psf
      type(element_data), intent(in) :: ele
!
      type(group_data), intent(in) :: ele_grp
      type(psf_parameters), intent(in) :: psf_param(num_psf)
!
      type(psf_search_lists), intent(inout) :: psf_search(num_psf)
!
      integer(kind = kint) :: i
!
!
      call allocate_work_4_mark_psf(ele%numele)
!
      do i = 1, num_psf
        call alloc_num_psf_search_list(np_smp, psf_search(i)%elem_list)
!
        call mark_element_list_4_psf                                    &
     &     (ele%numele, ele%interior_ele, ele_grp%num_grp,              &
     &      ele_grp%num_item, ele_grp%istack_grp, ele_grp%item_grp,     &
     &      psf_param(i)%nele_grp_area, psf_param(i)%id_ele_grp_area)
        call count_element_list_4_psf                                   &
     &     (ele%istack_ele_smp, psf_search(i)%elem_list)
!
        call alloc_psf_search_list(psf_search(i)%elem_list)
        call set_element_list_4_psf                                     &
     &     (ele%istack_ele_smp, psf_search(i)%elem_list)
      end do
!
      call deallocate_work_4_mark_psf
!
      end subroutine set_searched_element_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_searched_surface_list_4_psf                        &
     &         (num_psf, ele, surf, psf_search)
!
      use m_geometry_constants
      use set_surface_list_for_psf
!
      integer(kind = kint), intent(in) :: num_psf
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(psf_search_lists), intent(inout) :: psf_search(num_psf)
!
      integer(kind = kint) :: i, ist_smp
!
!
      call allocate_work_4_mark_surf_psf(surf%numsurf)
!
      do i = 1, num_psf
        call alloc_num_psf_search_list(np_smp, psf_search(i)%surf_list)
!
        ist_smp = (i-1)*np_smp
        call mark_surface_list_4_psf(ele%numele,                        &
     &      surf%numsurf, surf%isf_4_ele, psf_search(i)%elem_list)
        call count_surf_list_4_psf                                      &
     &     (surf%istack_surf_smp, psf_search(i)%surf_list)
!
        call alloc_psf_search_list(psf_search(i)%surf_list)
        call set_surface_list_4_psf                                     &
     &     (surf%istack_surf_smp, psf_search(i)%surf_list)
      end do
!
      call deallocate_work_4_mark_surf_psf
!
      end subroutine set_searched_surface_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_searched_edge_list_4_psf                           &
     &         (num_psf, surf, edge, psf_search)
!
      use m_geometry_constants
      use set_edge_list_for_psf
!
      integer(kind = kint), intent(in) :: num_psf
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      type(psf_search_lists), intent(inout) :: psf_search(num_psf)
!
      integer(kind = kint) :: i, ist_smp
!
!
      call allocate_work_4_mark_edge_psf(edge%numedge)
!
      do i = 1, num_psf
        call alloc_num_psf_search_list(np_smp, psf_search(i)%edge_list)
!
        ist_smp = (i-1)*np_smp
        call mark_edge_list_4_psf(surf%numsurf,                         &
     &      edge%numedge, edge%iedge_4_sf, psf_search(i)%surf_list)
        call count_edge_list_4_psf                                      &
     &     (edge%istack_edge_smp, psf_search(i)%edge_list)
!
        call alloc_psf_search_list(psf_search(i)%edge_list)
        call set_edge_list_4_psf                                        &
     &     (edge%istack_edge_smp, psf_search(i)%edge_list)
      end do
!
      call deallocate_work_4_mark_edge_psf
!
      end subroutine set_searched_edge_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_searched_node_list_4_psf                           &
     &         (num_psf, node, edge, psf_search)
!
      use set_node_list_for_psf
!
      integer(kind = kint), intent(in) :: num_psf
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
!
      type(psf_search_lists), intent(inout) :: psf_search(num_psf)
!
      integer(kind = kint) :: i, ist_smp
!
!
      call allocate_work_4_mark_node_psf(node%numnod)
!
      do i = 1, num_psf
        call alloc_num_psf_search_list(np_smp, psf_search(i)%node_list)
!
        ist_smp = (i-1)*np_smp
        call mark_node_list_4_psf                                       &
     &     (node%numnod, edge%numedge, edge%nnod_4_edge,                &
     &      edge%ie_edge, psf_search(i)%edge_list)
        call count_node_list_4_psf                                      &
     &     (node%istack_nod_smp, psf_search(i)%node_list)
!
        call alloc_psf_search_list(psf_search(i)%node_list)
        call set_node_list_4_psf                                        &
     &     (node%istack_nod_smp, psf_search(i)%node_list)
      end do
!
      call deallocate_work_4_mark_node_psf
!
      end subroutine set_searched_node_list_4_psf
!
!  ---------------------------------------------------------------------
!
      end module search_ele_list_for_psf
