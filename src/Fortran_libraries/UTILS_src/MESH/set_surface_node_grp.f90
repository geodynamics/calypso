!
!     module set_surface_node_grp
!
!        programmed by H.Matsui and H.Okuda
!        modified by H. Matsui on Aug., 2006
!        modified by H. Matsui on Dec., 2008
!
!      subroutine s_set_surface_node_grp
!
      module set_surface_node_grp
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
      subroutine s_set_surface_node_grp
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_surface_group
      use m_surface_group_connect
      use set_surface_node
      use set_smp_4_groups
!
!
      call allocate_make_4_surf_nod_grp(numnod)
!
      call allocate_node_stack_4_sf_grp
!
      call count_surf_nod_grp_stack(np_smp, inod_smp_stack,             &
     &    numele, nnod_4_ele, ie, nnod_4_surf, node_on_sf,              &
     &    num_surf, num_surf_bc, surf_istack, surf_item,                &
     &    ntot_node_sf_grp, nnod_sf_grp, inod_stack_sf_grp)
!
      call count_surf_nod_4_sheard_para
!
!
      if (ntot_node_sf_grp .gt. 0) then
!
        call allocate_surf_nod
!
        call set_surf_nod_grp_item(numnod, numele, nnod_4_ele, ie,      &
     &      nnod_4_surf, node_on_sf, node_on_sf_n, num_surf,            &
     &      num_surf_bc, surf_istack, surf_item, ntot_node_sf_grp,      &
     &      inod_stack_sf_grp, inod_surf_grp, surf_node_n,              &
     &      num_sf_4_nod)
!
      end if
!
      call deallocate_make_4_surf_nod_grp
!
      end subroutine s_set_surface_node_grp
!
!-----------------------------------------------------------------------
!
      end module set_surface_node_grp
