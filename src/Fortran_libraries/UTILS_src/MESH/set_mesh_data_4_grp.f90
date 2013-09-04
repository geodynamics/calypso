!set_mesh_data_4_grp.f90
!     module set_mesh_data_4_grp
!
!     Writteg by H.Matsui on Aug., 2006
!     Modified by H.Matsui on Dec., 2008
!
!      subroutine set_surf_4_ele_group
!      subroutine set_edge_4_ele_group
!      subroutine set_node_4_ele_group
!
!      subroutine set_edge_4_surf_group
!
!      subroutine set_surf_id_4_surf_group
!
      module set_mesh_data_4_grp
!
      use m_precision
!
      implicit none
!
      integer(kind=kint), allocatable :: imark_4_grp(:)
      private :: imark_4_grp
      private :: allocate_imark_4_grp, deallocate_imark_4_grp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_4_ele_group
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_element_group
      use m_element_group_connect
!
      use set_node_4_group
!
!
      call allocate_imark_4_grp(numsurf)
      call allocate_surf_stack_4_ele_grp
!
      call count_nod_4_ele_grp(numsurf, numele, nsurf_4_ele, isf_4_ele, &
     &    num_mat, num_mat_bc, mat_istack, mat_item,                    &
     &    ntot_surf_ele_grp, nsurf_ele_grp, isurf_stack_ele_grp,        &
     &    imark_4_grp)
!
!
      call allocate_surf_id_4_ele_grp
!
      call set_nod_4_ele_grp(numsurf, numele, nsurf_4_ele, isf_4_ele,   &
     &    num_mat, num_mat_bc, mat_istack, mat_item,                    &
     &    ntot_surf_ele_grp, nsurf_ele_grp, isurf_stack_ele_grp,        &
     &    isurf_ele_grp, imark_4_grp)
!
      call deallocate_imark_4_grp
!
      end subroutine set_surf_4_ele_group
!
!-----------------------------------------------------------------------
!
      subroutine set_edge_4_ele_group
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_element_group
      use m_element_group_connect
!
      use set_node_4_group
!
!
      call allocate_imark_4_grp(numedge)
      call allocate_edge_stack_4_ele_grp
!
      call count_nod_4_ele_grp(numedge, numele, nedge_4_ele,            &
     &    iedge_4_ele, num_mat, num_mat_bc, mat_istack, mat_item,       &
     &    ntot_edge_ele_grp, nedge_ele_grp, iedge_stack_ele_grp,        &
     &    imark_4_grp)
!
!
      call allocate_edge_id_4_ele_grp
!
      call set_nod_4_ele_grp(numedge, numele, nedge_4_ele, iedge_4_ele, &
     &    num_mat, num_mat_bc, mat_istack, mat_item,                    &
     &    ntot_edge_ele_grp, nedge_ele_grp, iedge_stack_ele_grp,        &
     &    iedge_ele_grp, imark_4_grp)
!
      call deallocate_imark_4_grp
!
      end subroutine set_edge_4_ele_group
!
!-----------------------------------------------------------------------
!
      subroutine set_node_4_ele_group
!
      use m_geometry_parameter
      use m_geometry_data
      use m_element_group
      use m_element_group_connect
!
      use set_node_4_group
!
!
      call allocate_imark_4_grp(numnod)
      call allocate_node_stack_4_ele_grp
!
      call count_nod_4_ele_grp(numnod, numele, nnod_4_ele, ie,          &
     &    num_mat, num_mat_bc, mat_istack, mat_item,                    &
     &    ntot_node_ele_grp, nnod_ele_grp, inod_stack_ele_grp,          &
     &    imark_4_grp)
!
!
      call allocate_node_id_4_ele_grp
!
      call set_nod_4_ele_grp(numnod, numele, nnod_4_ele, ie,            &
     &    num_mat, num_mat_bc, mat_istack, mat_item,                    &
     &    ntot_node_ele_grp, nnod_ele_grp, inod_stack_ele_grp,          &
     &    inod_ele_grp, imark_4_grp)
!
      call deallocate_imark_4_grp
!
      end subroutine set_node_4_ele_group
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_edge_4_surf_group
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_surface_group
      use m_surface_group_connect
!
      use set_node_4_group
!
!
      call allocate_imark_4_grp(numedge)
      call allocate_edge_stack_4_sf_grp
!
      call count_nod_4_ele_grp(numedge, numsurf, nedge_4_surf,          &
     &    iedge_4_sf, num_surf, num_surf_bc, surf_istack, isurf_grp,    &
     &    ntot_edge_sf_grp, nedge_sf_grp, iedge_stack_sf_grp,           &
     &    imark_4_grp)
!
      call allocate_edge_id_4_sf_grp
!
      call set_nod_4_ele_grp(numedge, numsurf, nedge_4_surf,            &
     &    iedge_4_sf, num_surf, num_surf_bc, surf_istack, isurf_grp,    &
     &    ntot_edge_sf_grp, nedge_sf_grp, iedge_stack_sf_grp,           &
     &    iedge_surf_grp, imark_4_grp)
!
      call deallocate_imark_4_grp
!
      end subroutine set_edge_4_surf_group
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_surf_id_4_surf_group
!
      use m_geometry_parameter
      use m_geometry_data
      use m_surface_group
      use m_surface_group_connect
      use set_surface_id_4_surf_grp
!
!
      call allocate_surf_id_4_sf_grp
!
      call set_surface_id_4_surf_group(numele, isf_4_ele,               &
     &    num_surf, num_surf_bc, surf_istack, surf_item,                &
     &    isurf_grp, isurf_grp_n)
!
      end subroutine set_surf_id_4_surf_group
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_imark_4_grp(num)
!
      integer(kind = kint), intent(in) :: num
!
      allocate(imark_4_grp(num))
      imark_4_grp = 0
!
      end subroutine allocate_imark_4_grp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_imark_4_grp
!
      deallocate(imark_4_grp)
!
      end subroutine deallocate_imark_4_grp
!
!-----------------------------------------------------------------------
!
      end module set_mesh_data_4_grp
