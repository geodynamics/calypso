!>@file  t_element_group_table.f90
!!      module t_element_group_table
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2008
!
!>@brief Structure of connectivity data for element group items
!!
!!@verbatim
!!      subroutine const_element_group_table                            &
!!     &         (node, ele, surf, edge, ele_grp, ele_grp_tbl)
!!      subroutine empty_sf_ed_nod_ele_group(ele_grp, ele_grp_tbl)
!!      subroutine dealloc_element_group_table(ele_grp_tbl)
!!        type(node_data),    intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data),    intent(in) :: edge
!!        type(group_data),   intent(in) :: ele_grp
!!        type (element_group_table), intent(inout) :: ele_grp_tbl
!!@endverbatim
!
      module t_element_group_table
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_group_connects
!
      implicit  none
!
!>   Structure of connectivities for element group
      type element_group_table
!>   local surface connectivity for element group
        type(group_connect_data) :: surf_tbl
!>   local edge connectivity for element group
        type(group_connect_data) :: edge_tbl
!>   local node connectivity for element group
        type(group_connect_data) :: nod_tbl
      end type element_group_table
!
      integer(kind=kint), allocatable, private :: imark_ele_grp(:)
!
      private :: allocate_imark_ele_grp, deallocate_imark_ele_grp
      private :: set_surf_4_ele_group, set_edge_4_ele_group
      private :: set_node_4_ele_group
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_element_group_table                              &
     &         (node, ele, surf, edge, ele_grp, ele_grp_tbl)
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data),    intent(in) :: edge
      type(group_data),   intent(in) :: ele_grp
!
      type (element_group_table), intent(inout) :: ele_grp_tbl
!
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_4_ele_group'
      call set_surf_4_ele_group(ele, surf, ele_grp,                     &
     &                          ele_grp_tbl%surf_tbl)
!
       if (iflag_debug.eq.1) write(*,*) 'set_edge_4_ele_group'
      call set_edge_4_ele_group(ele, edge, ele_grp,                     &
     &                          ele_grp_tbl%edge_tbl)
!
       if (iflag_debug.eq.1) write(*,*) 'set_node_4_ele_group'
      call set_node_4_ele_group(ele, node, ele_grp,                     &
     &                          ele_grp_tbl%nod_tbl)
!
      end subroutine const_element_group_table
!
! ----------------------------------------------------------------------
!
      subroutine empty_sf_ed_nod_ele_group(ele_grp, ele_grp_tbl)
!
      type(group_data),   intent(in) :: ele_grp
      type(element_group_table), intent(inout) :: ele_grp_tbl
!
!
      call alloc_num_other_grp(ele_grp%num_grp, ele_grp_tbl%surf_tbl)
      call alloc_num_other_grp(ele_grp%num_grp, ele_grp_tbl%edge_tbl)
      call alloc_num_other_grp(ele_grp%num_grp, ele_grp_tbl%nod_tbl)
!
      ele_grp_tbl%surf_tbl%ntot_e_grp = 0
      ele_grp_tbl%edge_tbl%ntot_e_grp = 0
      ele_grp_tbl%nod_tbl%ntot_e_grp = 0
      call alloc_item_other_grp(ele_grp_tbl%surf_tbl)
      call alloc_item_other_grp(ele_grp_tbl%edge_tbl)
      call alloc_item_other_grp(ele_grp_tbl%nod_tbl)
!
      end subroutine empty_sf_ed_nod_ele_group
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_element_group_table(ele_grp_tbl)
!
      type(element_group_table), intent(inout) :: ele_grp_tbl
!
      call dealloc_grp_connect(ele_grp_tbl%surf_tbl)
      call dealloc_grp_connect(ele_grp_tbl%edge_tbl)
      call dealloc_grp_connect(ele_grp_tbl%nod_tbl)
!
      end subroutine dealloc_element_group_table
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_surf_4_ele_group(ele, surf, ele_grp, surf_tbl)
!
      use set_node_4_group
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(group_data),   intent(in) :: ele_grp
      type(group_connect_data), intent(inout) :: surf_tbl
!
!
      call allocate_imark_ele_grp(surf%numsurf)
!
      call alloc_num_other_grp(ele_grp%num_grp, surf_tbl)
!
      call count_nod_4_ele_grp(surf%numsurf, ele%numele, nsurf_4_ele,   &
     &    surf%isf_4_ele, ele_grp%num_grp, ele_grp%num_item,            &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    surf_tbl%ntot_e_grp, surf_tbl%nitem_e_grp,                    &
     &    surf_tbl%istack_e_grp, imark_ele_grp)
!
!
      call alloc_item_other_grp(surf_tbl)
!
      call set_nod_4_ele_grp(surf%numsurf, ele%numele, nsurf_4_ele,     &
     &    surf%isf_4_ele,  ele_grp%num_grp, ele_grp%num_item,           &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    surf_tbl%ntot_e_grp, surf_tbl%nitem_e_grp,                    &
     &    surf_tbl%istack_e_grp, surf_tbl%item_e_grp, imark_ele_grp)
!
      call deallocate_imark_ele_grp
!
      end subroutine set_surf_4_ele_group
!
!-----------------------------------------------------------------------
!
      subroutine set_edge_4_ele_group(ele, edge, ele_grp, edge_tbl)
!
      use set_node_4_group
!
      type(element_data), intent(in) :: ele
      type(edge_data),    intent(in) :: edge
      type(group_data),   intent(in) :: ele_grp
      type(group_connect_data), intent(inout) :: edge_tbl
!
!
      call allocate_imark_ele_grp(edge%numedge)
!
      call alloc_num_other_grp(ele_grp%num_grp, edge_tbl)
!
      call count_nod_4_ele_grp(edge%numedge, ele%numele, nedge_4_ele,   &
     &    edge%iedge_4_ele, ele_grp%num_grp, ele_grp%num_item,          &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    edge_tbl%ntot_e_grp, edge_tbl%nitem_e_grp,                    &
     &    edge_tbl%istack_e_grp, imark_ele_grp)
!
!
      call alloc_item_other_grp(edge_tbl)
!
      call set_nod_4_ele_grp(edge%numedge, ele%numele, nedge_4_ele,     &
     &    edge%iedge_4_ele, ele_grp%num_grp, ele_grp%num_item,          &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    edge_tbl%ntot_e_grp, edge_tbl%nitem_e_grp,                    &
     &    edge_tbl%istack_e_grp, edge_tbl%item_e_grp, imark_ele_grp)
!
      call deallocate_imark_ele_grp
!
      end subroutine set_edge_4_ele_group
!
!-----------------------------------------------------------------------
!
      subroutine set_node_4_ele_group(ele, nod, ele_grp, nod_tbl)
!
      use set_node_4_group
!
      type(element_data), intent(in) :: ele
      type(node_data),    intent(in) :: nod
      type(group_data),   intent(in) :: ele_grp
      type(group_connect_data), intent(inout) :: nod_tbl
!
!
      call allocate_imark_ele_grp(nod%numnod)
!
      call alloc_num_other_grp(ele_grp%num_grp, nod_tbl)
!
      call count_nod_4_ele_grp(nod%numnod, ele%numele, ele%nnod_4_ele,  &
     &    ele%ie, ele_grp%num_grp, ele_grp%num_item,                    &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    nod_tbl%ntot_e_grp, nod_tbl%nitem_e_grp,                      &
     &    nod_tbl%istack_e_grp, imark_ele_grp)
!
!
      call alloc_item_other_grp(nod_tbl)
!
      call set_nod_4_ele_grp(nod%numnod, ele%numele, ele%nnod_4_ele,    &
     &    ele%ie, ele_grp%num_grp, ele_grp%num_item,                    &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    nod_tbl%ntot_e_grp, nod_tbl%nitem_e_grp,                      &
     &    nod_tbl%istack_e_grp, nod_tbl%item_e_grp, imark_ele_grp)
!
      call deallocate_imark_ele_grp
!
      end subroutine set_node_4_ele_group
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
      end module t_element_group_table
