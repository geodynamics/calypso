!>@file   t_viewer_group.f90
!!@brief  module t_viewer_group
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief Structure of surface information for pickup surface
!!
!!@verbatim
!!      subroutine alloc_domain_stack_4_surf(num_pe, domain_grps)
!!      subroutine dealloc_viewer_node_grps_item(view_nod_grps)
!!        type(viewer_node_groups), intent(inout) :: view_nod_grps
!!      subroutine alloc_viewer_node_grps_stack                         &
!!     &         (num_group, view_nod_grps)
!!      subroutine alloc_viewer_surf_grps_stack(num_group, view_grps)
!!      subroutine alloc_merged_node_grps_stack(num_pe, view_nod_grps)
!!      subroutine alloc_merged_surf_grps_stack(num_pe, view_grps)
!!      subroutine dealloc_viewer_surf_grps_item(view_grps)
!!        type(viewer_surface_groups), intent(inout) :: view_grps
!!
!!      subroutine alloc_merged_group_stack(num_pe, ngrp, v_grp)
!!      subroutine alloc_merged_group_item(v_grp)
!!      subroutine dealloc_merged_group_stack(v_grp)
!!      subroutine dealloc_merged_group_item(v_grp)
!!        type(viewer_group_data), intent(inout) :: v_grp
!!@endverbatim
!
      module t_viewer_group
!
      use m_precision
      use m_geometry_constants
!
      implicit none
!
!
      type viewer_group_data
        integer(kind = kint) :: num_item
        integer(kind = kint), allocatable :: istack_sf(:)
        integer(kind = kint), allocatable :: item_sf(:)
      end type viewer_group_data
!
      type viewer_node_groups
        integer(kind = kint) :: num_grp
        character(len=kchara), allocatable :: grp_name(:)
        type(viewer_group_data) :: node_grp
      end type viewer_node_groups
!
      type viewer_surface_groups
        integer(kind = kint) :: num_grp
        character(len=kchara), allocatable :: grp_name(:)
        type(viewer_group_data) :: surf_grp
        type(viewer_group_data) :: edge_grp
        type(viewer_group_data) :: node_grp
      end type viewer_surface_groups
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine alloc_domain_stack_4_surf(num_pe, domain_grps)
!
      integer, intent(in) :: num_pe
      type(viewer_surface_groups), intent(inout) :: domain_grps
!
!
      domain_grps%num_grp = 1
!
      call alloc_merged_surf_grps_stack(num_pe, domain_grps)
!
      domain_grps%grp_name = 'subdomains'
!
      end subroutine alloc_domain_stack_4_surf
!
!------------------------------------------------------------------
!
      subroutine alloc_viewer_node_grps_stack                           &
     &         (num_group, view_nod_grps)
!
      integer(kind = kint), intent(in) :: num_group
      type(viewer_node_groups), intent(inout) :: view_nod_grps
!
!
      view_nod_grps%num_grp = num_group
      allocate(view_nod_grps%grp_name(view_nod_grps%num_grp))
!
      call alloc_viewer_group_stack                                     &
     &   (view_nod_grps%num_grp, view_nod_grps%node_grp)
!
      end subroutine alloc_viewer_node_grps_stack
!
!------------------------------------------------------------------
!
      subroutine alloc_viewer_surf_grps_stack(num_group, view_grps)
!
      integer(kind = kint), intent(in) :: num_group
      type(viewer_surface_groups), intent(inout) :: view_grps
!
!
      view_grps%num_grp = num_group
      allocate(view_grps%grp_name(view_grps%num_grp))
!
      call alloc_viewer_group_stack                                     &
     &   (view_grps%num_grp, view_grps%surf_grp)
      call alloc_viewer_group_stack                                     &
     &   (view_grps%num_grp, view_grps%edge_grp)
      call alloc_viewer_group_stack                                     &
     &   (view_grps%num_grp, view_grps%node_grp)
!
      end subroutine alloc_viewer_surf_grps_stack
!
!------------------------------------------------------------------
!
      subroutine alloc_merged_node_grps_stack(num_pe, view_nod_grps)
!
      integer, intent(in) :: num_pe
      type(viewer_node_groups), intent(inout) :: view_nod_grps
!
!
      allocate(view_nod_grps%grp_name(view_nod_grps%num_grp))
!
      call alloc_merged_group_stack                                     &
     &   (num_pe, view_nod_grps%num_grp, view_nod_grps%node_grp)
!
      end subroutine alloc_merged_node_grps_stack
!
!------------------------------------------------------------------
!
      subroutine alloc_merged_surf_grps_stack(num_pe, view_grps)
!
      integer, intent(in) :: num_pe
      type(viewer_surface_groups), intent(inout) :: view_grps
!
!
      allocate(view_grps%grp_name(view_grps%num_grp))
!
      call alloc_merged_group_stack                                     &
     &   (num_pe, view_grps%num_grp, view_grps%surf_grp)
      call alloc_merged_group_stack                                     &
     &   (num_pe, view_grps%num_grp, view_grps%edge_grp)
      call alloc_merged_group_stack                                     &
     &   (num_pe, view_grps%num_grp, view_grps%node_grp)
!
      end subroutine alloc_merged_surf_grps_stack
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_viewer_node_grps_item(view_nod_grps)
!
      type(viewer_node_groups), intent(inout) :: view_nod_grps
!
!
      deallocate(view_nod_grps%grp_name)
      call dealloc_merged_group_stack(view_nod_grps%node_grp)
      call dealloc_merged_group_item(view_nod_grps%node_grp)
!
      end subroutine dealloc_viewer_node_grps_item
!
!------------------------------------------------------------------
!
      subroutine dealloc_viewer_surf_grps_item(view_grps)
!
      type(viewer_surface_groups), intent(inout) :: view_grps
!
!
      call dealloc_merged_group_item(view_grps%surf_grp)
      call dealloc_merged_group_item(view_grps%edge_grp)
      call dealloc_merged_group_item(view_grps%node_grp)
!
      call dealloc_merged_group_stack(view_grps%surf_grp)
      call dealloc_merged_group_stack(view_grps%edge_grp)
      call dealloc_merged_group_stack(view_grps%node_grp)
!
      end subroutine dealloc_viewer_surf_grps_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_viewer_group_stack(ngrp, v_grp)
!
      integer(kind = kint), intent(in) :: ngrp
      type(viewer_group_data), intent(inout) :: v_grp
!
!
      allocate(v_grp%istack_sf(0:ngrp))
      v_grp%istack_sf = 0
!
      end subroutine alloc_viewer_group_stack
!
!------------------------------------------------------------------
!
      subroutine alloc_merged_group_stack(num_pe, ngrp, v_grp)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: ngrp
      type(viewer_group_data), intent(inout) :: v_grp
!
!
      allocate( v_grp%istack_sf(0:num_pe*ngrp)  )
      v_grp%istack_sf = 0
!
      end subroutine alloc_merged_group_stack
!
!------------------------------------------------------------------
!
      subroutine alloc_merged_group_item(v_grp)
!
      type(viewer_group_data), intent(inout) :: v_grp
!
!
      allocate( v_grp%item_sf(v_grp%num_item)  )
      if(v_grp%num_item .gt. 0) v_grp%item_sf = 0
!
      end subroutine alloc_merged_group_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_merged_group_stack(group)
!
      type(viewer_group_data), intent(inout) :: group
!
!
      deallocate(group%istack_sf)
!
      end subroutine dealloc_merged_group_stack
!
!------------------------------------------------------------------
!
      subroutine dealloc_merged_group_item(v_grp)
!
      type(viewer_group_data), intent(inout) :: v_grp
!
!
      deallocate(v_grp%item_sf)
!
      end subroutine dealloc_merged_group_item
!
!------------------------------------------------------------------
!
      end module t_viewer_group
