!set_connects_4_surf_group.f90
!     module set_connects_4_surf_group
!
!
!        programmed by H. Matsui on Dec., 2010
!
!      subroutine set_surf_id_4_surf_group(ele, surf,                   &
!     &          surf_grp, sf_grp_tbl)
!        type(element_data),       intent(in) :: ele
!        type(surface_data),       intent(in) :: surf
!        type(surface_group_data), intent(in) :: surf_grp
!        type(surface_group_table), intent(inout) :: sf_grp_tbl
!
!      subroutine set_edge_4_surf_group(surf, edge, surf_grp,           &
!     &          sf_grp_tbl)
!        type(surface_data),        intent(in) :: surf
!        type(edge_data),           intent(in) :: edge
!        type(surface_group_data), intent(in) :: surf_grp
!        type(surface_group_table), intent(inout) :: sf_grp_tbl
!
!      subroutine set_node_4_surf_group(nod, ele, surf, sf_grp, sf_nod)
!        type(node_data),    intent(in) ::       nod
!        type(element_data), intent(in) ::       ele
!        type(surface_data), intent(in) ::       surf
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surface_node_grp_data), intent(inout) :: sf_nod
!
!      subroutine cal_surf_normal_at_nod(mesh, surf, sf_grp,            &
!     &          sf_grp_v, sf_nod)
!        type(mesh_geometry),          intent(in) :: mesh
!        type(surface_data),           intent(in) :: surf
!        type(surface_group_data),     intent(in) :: sf_grp
!        type(surface_group_geometry), intent(in) :: sf_grp_v
!        type(surface_node_grp_data), intent(inout) :: sf_nod
!
!      subroutine empty_sf_ed_nod_surf_grp_type(surf_grp, sf_grp_tbl)
!        type(surface_group_data), intent(in) :: surf_grp
!        type(surface_group_table), intent(inout) :: sf_grp_tbl
!      subroutine empty_surface_node_grp_type(sf_grp, sf_nod)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surface_node_grp_data), intent(inout) :: sf_nod
!
      module set_connects_4_surf_group
!
      use m_precision
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_id_4_surf_group(ele, surf,                    &
     &          surf_grp, sf_grp_tbl)
!
      use t_geometry_data
      use t_group_data
      use t_group_connects
      use t_surface_data
      use set_surface_id_4_surf_grp
!
      type(element_data),       intent(in) :: ele
      type(surface_data),       intent(in) :: surf
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_group_table), intent(inout) :: sf_grp_tbl
!
!
      call alloc_surf_item_sf_grp_type(surf_grp%num_item, sf_grp_tbl)
!
      call set_surface_id_4_surf_group(ele%numele, surf%isf_4_ele,      &
     &    surf_grp%num_grp, surf_grp%num_item,                          &
     &    surf_grp%istack_grp, surf_grp%item_sf_grp,                    &
     &    sf_grp_tbl%isurf_grp, sf_grp_tbl%isurf_grp_n)
!
      end subroutine set_surf_id_4_surf_group
!
!-----------------------------------------------------------------------
!
      subroutine set_edge_4_surf_group(surf, edge, surf_grp,            &
     &          sf_grp_tbl)
!
      use m_geometry_constants
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_group_connects
      use set_node_4_group
!
      type(surface_data),        intent(in) :: surf
      type(edge_data),           intent(in) :: edge
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_group_table), intent(inout) :: sf_grp_tbl
!
      integer(kind=kint), allocatable :: imark_surf_grp(:)
!
!
      allocate( imark_surf_grp(edge%numedge) )
      imark_surf_grp = 0
!
      call alloc_num_other_grp(surf_grp%num_grp, sf_grp_tbl%edge)
!
      call count_nod_4_ele_grp(edge%numedge, surf%numsurf,              &
     &    nedge_4_surf, edge%iedge_4_sf,                                &
     &    surf_grp%num_grp, surf_grp%num_item,                          &
     &    surf_grp%istack_grp, sf_grp_tbl%isurf_grp,                    &
     &    sf_grp_tbl%edge%ntot_e_grp, sf_grp_tbl%edge%nitem_e_grp,      &
     &    sf_grp_tbl%edge%istack_e_grp, imark_surf_grp)
!
      call alloc_item_other_grp(sf_grp_tbl%edge)
!
      call set_nod_4_ele_grp(edge%numedge, surf%numsurf,                &
     &    nedge_4_surf, edge%iedge_4_sf,                                &
     &    surf_grp%num_grp, surf_grp%num_item,                          &
     &    surf_grp%istack_grp, sf_grp_tbl%isurf_grp,                    &
     &    sf_grp_tbl%edge%ntot_e_grp, sf_grp_tbl%edge%nitem_e_grp,      &
     &    sf_grp_tbl%edge%istack_e_grp, sf_grp_tbl%edge%item_e_grp,     &
     &    imark_surf_grp)
!
      deallocate(imark_surf_grp)
!
      end subroutine set_edge_4_surf_group
!
!-----------------------------------------------------------------------
!
      subroutine set_node_4_surf_group(nod, ele, surf, sf_grp, sf_nod)
!
      use m_machine_parameter
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_surface_group_connect
      use set_surface_node
      use set_smp_4_group_types
      use cal_minmax_and_stacks
!
      type(node_data),    intent(in) ::       nod
      type(element_data), intent(in) ::       ele
      type(surface_data), intent(in) ::       surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      call allocate_make_4_surf_nod_grp(nod%numnod)
!
      call alloc_num_surf_grp_nod(sf_grp%num_grp, sf_nod)
!
      call count_surf_nod_grp_stack(np_smp, nod%istack_nod_smp,         &
     &    ele%numele, ele%nnod_4_ele, ele%ie, surf%nnod_4_surf,         &
     &    surf%node_on_sf, sf_grp%num_grp, sf_grp%num_item,             &
     &    sf_grp%istack_grp, sf_grp%item_sf_grp,                        &
     &    sf_nod%ntot_node_sf_grp, sf_nod%nnod_sf_grp,                  &
     &    sf_nod%inod_stack_sf_grp)
!
!
      call alloc_num_surf_grp_nod_smp(sf_grp%num_grp_smp, sf_nod)
!
      call set_group_size_4_smp(np_smp,                                 &
     &    sf_grp%num_grp, sf_nod%inod_stack_sf_grp,                     &
     &    sf_nod%istack_surf_nod_smp, sf_nod%max_sf_nod_4_smp)
!
!
      call alloc_item_surf_grp_nod(sf_nod)
      if (sf_nod%ntot_node_sf_grp .gt. 0) then
        call set_surf_nod_grp_item(nod%numnod, ele%numele,              &
     &      ele%nnod_4_ele, ele%ie, surf%nnod_4_surf,                   &
     &      surf%node_on_sf, surf%node_on_sf_n,                         &
     &      sf_grp%num_grp, sf_grp%num_item,                            &
     &      sf_grp%istack_grp, sf_grp%item_sf_grp,                      &
     &      sf_nod%ntot_node_sf_grp, sf_nod%inod_stack_sf_grp,          &
     &      sf_nod%inod_surf_grp, sf_nod%surf_node_n,                   &
     &      sf_nod%num_sf_4_nod)
      end if
!
      call deallocate_make_4_surf_nod_grp
!
      end subroutine set_node_4_surf_group
!
!-----------------------------------------------------------------------
!
      subroutine cal_surf_normal_at_nod(mesh, surf, sf_grp,             &
     &          sf_grp_v, sf_nod)
!
      use t_mesh_data
      use t_surface_data
      use t_group_data
      use t_surface_group_geometry
      use t_surface_group_connect
      use set_norm_nod_4_surf_grp
!
      type(mesh_geometry),          intent(in) :: mesh
      type(surface_data),           intent(in) :: surf
      type(surface_group_data),     intent(in) :: sf_grp
      type(surface_group_geometry), intent(in) :: sf_grp_v
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      call allocate_work_norm_nod(mesh%node%numnod)
      call alloc_vect_surf_grp_nod(sf_nod)
!
      call cal_surf_grp_norm_node(mesh%ele%numele, mesh%ele%nnod_4_ele, &
     &    surf%nnod_4_surf, surf%node_on_sf, mesh%ele%ie,               &
     &    sf_grp%num_grp, sf_grp%num_item, sf_grp%istack_grp,           &
     &    sf_grp%item_sf_grp, sf_grp_v%vnorm_sf_grp,                    &
     &    sf_grp_v%a_area_sf_grp, sf_nod%ntot_node_sf_grp,              &
     &    sf_nod%inod_stack_sf_grp, sf_nod%inod_surf_grp,               &
     &    sf_nod%surf_norm_nod, sf_nod%coef_sf_nod)
!
      call deallocate_work_norm_nod
!
      end subroutine cal_surf_normal_at_nod
!
! -----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine empty_sf_ed_nod_surf_grp_type(surf_grp, sf_grp_tbl)
!
      use t_group_data
      use t_group_connects
!
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_group_table), intent(inout) :: sf_grp_tbl
!
!
      call alloc_num_other_grp(surf_grp%num_grp, sf_grp_tbl%edge)
!
      sf_grp_tbl%edge%ntot_e_grp = 0
      call alloc_surf_item_sf_grp_type(surf_grp%num_item, sf_grp_tbl)
      call alloc_item_other_grp(sf_grp_tbl%edge)
!
      end subroutine empty_sf_ed_nod_surf_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine empty_surface_node_grp_type(sf_grp, sf_nod)
!
      use t_group_data
      use t_surface_group_connect
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      sf_nod%ntot_node_sf_grp = 0
      call alloc_num_surf_grp_nod(sf_grp%num_grp, sf_nod)
!
      sf_nod%ntot_node_sf_grp = 0
      call alloc_item_surf_grp_nod(sf_nod)
!
      end subroutine empty_surface_node_grp_type
!
!-----------------------------------------------------------------------
!
      end module set_connects_4_surf_group
