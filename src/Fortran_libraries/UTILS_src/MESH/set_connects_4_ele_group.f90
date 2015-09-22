!set_connects_4_ele_group.f90
!     module set_connects_4_ele_group
!
!     Writteg by H.Matsui on Dec., 2008
!
!      subroutine set_surf_4_ele_group(ele, surf, ele_grp, ele_grp_tbl)
!        type(element_data), intent(in) :: ele
!        type(surface_data), intent(in) :: surf
!        type(group_data),   intent(in) :: ele_grp
!        type(element_group_table), intent(inout) :: ele_grp_tbl
!
!      subroutine set_edge_4_ele_group(ele, edge, ele_grp, ele_grp_tbl)
!        type(element_data), intent(in) :: ele
!        type(edge_data), intent(in) :: edge
!        type(group_data),   intent(in) :: ele_grp
!        type(element_group_table), intent(inout) :: ele_grp_tbl
!
!      subroutine set_node_4_ele_group(ele, nod, ele_grp, ele_grp_tbl)
!        type(element_data), intent(in) :: ele
!        type(node_data),    intent(in) :: nod
!        type(group_data),   intent(in) :: ele_grp
!        type(element_group_table), intent(inout) :: ele_grp_tbl
!
!      subroutine empty_sf_ed_nod_ele_grp_type(ele_grp, ele_grp_tbl)
!        type(group_data),   intent(in) :: ele_grp
!        type(element_group_table), intent(inout) :: ele_grp_tbl
!
      module set_connects_4_ele_group
!
      use m_precision
!
      use t_geometry_data
      use t_group_data
      use t_group_connects
!
      use set_node_4_group
!
      implicit none
!
      integer(kind=kint), allocatable :: imark_ele_grp(:)
      private :: imark_ele_grp
      private :: allocate_imark_ele_grp
      private :: deallocate_imark_ele_grp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_4_ele_group(ele, surf, ele_grp, ele_grp_tbl)
!
      use m_geometry_constants
      use t_surface_data
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(group_data),   intent(in) :: ele_grp
      type(element_group_table), intent(inout) :: ele_grp_tbl
!
!
      call allocate_imark_ele_grp(surf%numsurf)
!
      call alloc_num_other_grp(ele_grp%num_grp, ele_grp_tbl%surf)
!
      call count_nod_4_ele_grp(surf%numsurf, ele%numele, nsurf_4_ele,   &
     &    surf%isf_4_ele, ele_grp%num_grp, ele_grp%num_item,            &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    ele_grp_tbl%surf%ntot_e_grp, ele_grp_tbl%surf%nitem_e_grp,    &
     &    ele_grp_tbl%surf%istack_e_grp, imark_ele_grp)
!
!
      call alloc_item_other_grp(ele_grp_tbl%surf)
!
      call set_nod_4_ele_grp(surf%numsurf, ele%numele, nsurf_4_ele,     &
     &    surf%isf_4_ele,  ele_grp%num_grp, ele_grp%num_item,           &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    ele_grp_tbl%surf%ntot_e_grp, ele_grp_tbl%surf%nitem_e_grp,    &
     &    ele_grp_tbl%surf%istack_e_grp, ele_grp_tbl%surf%item_e_grp,   &
     &    imark_ele_grp)
!
      call deallocate_imark_ele_grp
!
      end subroutine set_surf_4_ele_group
!
!-----------------------------------------------------------------------
!
      subroutine set_edge_4_ele_group(ele, edge, ele_grp, ele_grp_tbl)
!
      use m_geometry_constants
      use t_edge_data
!
      type(element_data), intent(in) :: ele
      type(edge_data),    intent(in) :: edge
      type(group_data),   intent(in) :: ele_grp
      type(element_group_table), intent(inout) :: ele_grp_tbl
!
!
      call allocate_imark_ele_grp(edge%numedge)
!
      call alloc_num_other_grp(ele_grp%num_grp, ele_grp_tbl%edge)
!
      call count_nod_4_ele_grp(edge%numedge, ele%numele, nedge_4_ele,   &
     &    edge%iedge_4_ele, ele_grp%num_grp, ele_grp%num_item,          &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    ele_grp_tbl%edge%ntot_e_grp, ele_grp_tbl%edge%nitem_e_grp,    &
     &    ele_grp_tbl%edge%istack_e_grp, imark_ele_grp)
!
!
      call alloc_item_other_grp(ele_grp_tbl%edge)
!
      call set_nod_4_ele_grp(edge%numedge, ele%numele, nedge_4_ele,     &
     &    edge%iedge_4_ele, ele_grp%num_grp, ele_grp%num_item,          &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    ele_grp_tbl%edge%ntot_e_grp, ele_grp_tbl%edge%nitem_e_grp,    &
     &    ele_grp_tbl%edge%istack_e_grp, ele_grp_tbl%edge%item_e_grp,   &
     &    imark_ele_grp)
!
      call deallocate_imark_ele_grp
!
      end subroutine set_edge_4_ele_group
!
!-----------------------------------------------------------------------
!
      subroutine set_node_4_ele_group(ele, nod, ele_grp, ele_grp_tbl)
!
      type(element_data), intent(in) :: ele
      type(node_data),    intent(in) :: nod
      type(group_data),   intent(in) :: ele_grp
      type(element_group_table), intent(inout) :: ele_grp_tbl
!
!
      call allocate_imark_ele_grp(nod%numnod)
!
      call alloc_num_other_grp(ele_grp%num_grp, ele_grp_tbl%node)
!
      call count_nod_4_ele_grp(nod%numnod, ele%numele, ele%nnod_4_ele,  &
     &    ele%ie, ele_grp%num_grp, ele_grp%num_item,                    &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    ele_grp_tbl%node%ntot_e_grp, ele_grp_tbl%node%nitem_e_grp,    &
     &    ele_grp_tbl%node%istack_e_grp, imark_ele_grp)
!
!
      call alloc_item_other_grp(ele_grp_tbl%node)
!
      call set_nod_4_ele_grp(nod%numnod, ele%numele, ele%nnod_4_ele,    &
     &    ele%ie, ele_grp%num_grp, ele_grp%num_item,                    &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    ele_grp_tbl%node%ntot_e_grp, ele_grp_tbl%node%nitem_e_grp,    &
     &    ele_grp_tbl%node%istack_e_grp, ele_grp_tbl%node%item_e_grp,   &
     &    imark_ele_grp)
!
      call deallocate_imark_ele_grp
!
      end subroutine set_node_4_ele_group
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine empty_sf_ed_nod_ele_grp_type(ele_grp, ele_grp_tbl)
!
      type(group_data),   intent(in) :: ele_grp
      type(element_group_table), intent(inout) :: ele_grp_tbl
!
!
      call alloc_num_other_grp(ele_grp%num_grp, ele_grp_tbl%surf)
      call alloc_num_other_grp(ele_grp%num_grp, ele_grp_tbl%edge)
      call alloc_num_other_grp(ele_grp%num_grp, ele_grp_tbl%node)
!
      ele_grp_tbl%surf%ntot_e_grp = 0
      ele_grp_tbl%edge%ntot_e_grp = 0
      ele_grp_tbl%node%ntot_e_grp = 0
      call alloc_item_other_grp(ele_grp_tbl%surf)
      call alloc_item_other_grp(ele_grp_tbl%edge)
      call alloc_item_other_grp(ele_grp_tbl%node)
!
      end subroutine empty_sf_ed_nod_ele_grp_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_imark_ele_grp(num)
!
      integer(kind = kint), intent(in) :: num
!
      allocate( imark_ele_grp(num) )
      imark_ele_grp = 0
!
      end subroutine allocate_imark_ele_grp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_imark_ele_grp
!
      deallocate(imark_ele_grp)
!
      end subroutine deallocate_imark_ele_grp
!
!-----------------------------------------------------------------------
!
      end module set_connects_4_ele_group
