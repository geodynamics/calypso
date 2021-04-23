!>@file  t_surface_group_table.f90
!!      module t_surface_group_table
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2008
!
!>@brief Structure of connectivity data for group items
!!
!!@verbatim
!!      subroutine const_surface_group_table                            &
!!     &         (ele, surf, edge, surf_grp, sf_grp_tbl)
!!      subroutine empty_sf_ed_nod_surf_grp_type(surf_grp, sf_grp_tbl)
!!      subroutine dealloc_surf_item_sf_grp(sf_grp_tbl)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data),    intent(in) :: edge
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(surface_group_table), intent(inout) :: sf_grp_tbl
!!@endverbatim
!
      module t_surface_group_table
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
!
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_edge_data
      use t_group_connects
!
      implicit  none
!
!
!>   Structure of connectivities for surface group
      type surface_group_table
!>   local surface ID for surface group
        integer(kind=kint), allocatable :: isurf_grp(:)
!>   local surface ID for opposite side of surface group
        integer(kind=kint), allocatable :: isurf_grp_n(:)
!
!>   local edge connectivity for surface group
        type(group_connect_data) :: edge_tbl
      end type surface_group_table
!
      private :: alloc_surf_item_sf_grp
      private :: set_surf_id_4_surf_group, set_edge_4_surf_group
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_surface_group_table                              &
     &         (ele, surf, edge, surf_grp, sf_grp_tbl)
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data),    intent(in) :: edge
      type(surface_group_data), intent(in) :: surf_grp
!
      type(surface_group_table), intent(inout) :: sf_grp_tbl
!
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_id_4_surf_group'
      call set_surf_id_4_surf_group(ele, surf, surf_grp, sf_grp_tbl)
!
       if (iflag_debug.eq.1) write(*,*) 'set_edge_4_surf_group'
      call set_edge_4_surf_group(surf, edge, surf_grp,                  &
     &    sf_grp_tbl%isurf_grp, sf_grp_tbl%edge_tbl)
!
      end subroutine const_surface_group_table
!
! ----------------------------------------------------------------------
!
      subroutine empty_sf_ed_nod_surf_grp_type(surf_grp, sf_grp_tbl)
!
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_group_table), intent(inout) :: sf_grp_tbl
!
!
      call alloc_num_other_grp(surf_grp%num_grp, sf_grp_tbl%edge_tbl)
!
      sf_grp_tbl%edge_tbl%ntot_e_grp = 0
      call alloc_surf_item_sf_grp(surf_grp%num_item, sf_grp_tbl)
      call alloc_item_other_grp(sf_grp_tbl%edge_tbl)
!
      end subroutine empty_sf_ed_nod_surf_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_item_sf_grp(sf_grp_tbl)
!
      type(surface_group_table), intent(inout) :: sf_grp_tbl
!
!
      call dealloc_grp_connect(sf_grp_tbl%edge_tbl)
!
      deallocate(sf_grp_tbl%isurf_grp  )
      deallocate(sf_grp_tbl%isurf_grp_n)
!
      end subroutine dealloc_surf_item_sf_grp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_item_sf_grp(nitem_grp, sf_grp_tbl)
!
      integer(kind = kint), intent(in) :: nitem_grp
      type(surface_group_table), intent(inout) :: sf_grp_tbl
!
      allocate(sf_grp_tbl%isurf_grp(nitem_grp)  )
      allocate(sf_grp_tbl%isurf_grp_n(nitem_grp))
!
      if(nitem_grp .gt. 0) then
        sf_grp_tbl%isurf_grp =   0
        sf_grp_tbl%isurf_grp_n = 0
      end if
!
      end subroutine alloc_surf_item_sf_grp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_surf_id_4_surf_group(ele, surf,                    &
     &          surf_grp, sf_grp_tbl)
!
      use set_surface_id_4_surf_grp
!
      type(element_data),       intent(in) :: ele
      type(surface_data),       intent(in) :: surf
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_group_table), intent(inout) :: sf_grp_tbl
!
!
      call alloc_surf_item_sf_grp(surf_grp%num_item, sf_grp_tbl)
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
     &          isurf_grp, edge_tbl)
!
      use set_node_4_group
!
      type(surface_data),        intent(in) :: surf
      type(edge_data),           intent(in) :: edge
      type(surface_group_data), intent(in) :: surf_grp
      integer(kind=kint), intent(in) :: isurf_grp(surf_grp%num_item)
!
      type(group_connect_data), intent(inout) :: edge_tbl
!
      integer(kind=kint), allocatable :: imark_surf_grp(:)
!
!
      allocate( imark_surf_grp(edge%numedge) )
      imark_surf_grp = 0
!
      call alloc_num_other_grp(surf_grp%num_grp, edge_tbl)
!
      call count_nod_4_ele_grp(edge%numedge, surf%numsurf,              &
     &    nedge_4_surf, edge%iedge_4_sf,                                &
     &    surf_grp%num_grp, surf_grp%num_item, surf_grp%istack_grp,     &
     &    isurf_grp, edge_tbl%ntot_e_grp, edge_tbl%nitem_e_grp,         &
     &    edge_tbl%istack_e_grp, imark_surf_grp)
!
      call alloc_item_other_grp(edge_tbl)
!
      call set_nod_4_ele_grp(edge%numedge, surf%numsurf,                &
     &    nedge_4_surf, edge%iedge_4_sf,                                &
     &    surf_grp%num_grp, surf_grp%num_item, surf_grp%istack_grp,     &
     &    isurf_grp, edge_tbl%ntot_e_grp, edge_tbl%nitem_e_grp,         &
     &    edge_tbl%istack_e_grp, edge_tbl%item_e_grp, imark_surf_grp)
!
      deallocate(imark_surf_grp)
!
      end subroutine set_edge_4_surf_group
!
!-----------------------------------------------------------------------
!
      end module t_surface_group_table
