!
!      module set_surf_connect_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine s_set_surf_connect_4_viewer(group, surf, mgd_sf_grp, &
!!     &          view_mesh, domain_grps, view_ele_grps, view_sf_grps)
!!        type(mesh_groups), intent(in) :: group
!!        type(surface_data), intent(inout) :: surf
!!        type(viewer_ele_grp_surface), intent(inout) :: mgd_sf_grp
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!        type(viewer_surface_groups), intent(inout) :: domain_grps
!!        type(viewer_surface_groups), intent(inout) :: view_ele_grps
!!        type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
      module set_surf_connect_4_viewer
!
      use m_precision
      use m_constants
!
      use t_viewer_mesh
      use t_mesh_data
!
      implicit none
!
      private :: s_set_groups_4_viewer_surface
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_set_surf_connect_4_viewer(group, surf, mgd_sf_grp,   &
     &          view_mesh, domain_grps, view_ele_grps, view_sf_grps)
!
      use t_surface_data
      use t_viewer_ele_grp_surface
      use pickup_surface_4_viewer
!
      type(mesh_groups), intent(in) :: group
!
      type(surface_data), intent(inout) :: surf
      type(viewer_ele_grp_surface), intent(inout) :: mgd_sf_grp
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
      type(viewer_surface_groups), intent(inout) :: domain_grps
      type(viewer_surface_groups), intent(inout) :: view_ele_grps
      type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
!
      call allocate_imark_surf(surf)
      call mark_used_surface_4_viewer(group, surf, mgd_sf_grp)
!
      call count_used_surface_4_viewer                                  &
     &   (surf%numsurf, view_mesh%nsurf_viewer)
!
      call allocate_sf_cvt_table_viewer(surf, view_mesh)
      call set_surf_cvt_table_viewer(surf)
!
      call deallocate_imark_surf
!
      call alloc_surf_connect_viewer(surf%nnod_4_surf, view_mesh)
      call set_surf_connect_viewer(surf, view_mesh)
!
      call s_set_groups_4_viewer_surface(group, surf, mgd_sf_grp,       &
     &    view_mesh%nsurf_viewer, domain_grps,                          &
     &    view_ele_grps, view_sf_grps)
!
      call deallocate_sf_cvt_table_viewer
!
      end subroutine s_set_surf_connect_4_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine s_set_groups_4_viewer_surface(group, surf, mgd_sf_grp, &
     &          nsurf_viewer, domain_grps, view_ele_grps, view_sf_grps)
!
      use t_mesh_data
      use t_surface_data
      use t_viewer_ele_grp_surface
!
      use renumber_surface_4_viewer
      use pickup_surface_4_viewer
!
      integer(kind = kint), intent(in) :: nsurf_viewer
      type(mesh_groups), intent(in) :: group
      type(surface_data), intent(in) :: surf
      type(viewer_ele_grp_surface), intent(in) :: mgd_sf_grp
!
      type(viewer_surface_groups), intent(inout) :: domain_grps
      type(viewer_surface_groups), intent(inout) :: view_ele_grps
      type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
!     renumber domain boundary
!
      domain_grps%surf_grp%num_item = surf%numsurf_iso
!       write(*,*) 'alloc_domain_stack_4_surf'
      call alloc_domain_stack_4_surf(1, domain_grps)
      call alloc_merged_group_item(domain_grps%surf_grp)
!
      call set_surf_domain_item_viewer(surf, domain_grps%surf_grp)
      call set_surf_domain_stack_viewer                                 &
     &   (nsurf_viewer, domain_grps%surf_grp)
!
!     renumber element group boundary
!
      view_ele_grps%num_grp = group%ele_grp%num_grp
      view_ele_grps%surf_grp%num_item                                   &
     &     = mgd_sf_grp%ntot_sf_iso_ele_grp
      call alloc_merged_surf_grps_stack(1, view_ele_grps)
      call alloc_merged_group_item(view_ele_grps%surf_grp)
!
      view_ele_grps%grp_name(1:view_ele_grps%num_grp)                   &
     &     = group%ele_grp%grp_name(1:view_ele_grps%num_grp)
!
      call set_element_group_item_viewer                                &
     &   (mgd_sf_grp, view_ele_grps%surf_grp)
      call set_element_group_stack_viewer(nsurf_viewer,                 &
     &    mgd_sf_grp, view_ele_grps%num_grp, view_ele_grps%surf_grp)
!
!     renumber surface boundary
!
      view_sf_grps%num_grp = group%surf_grp%num_grp
      view_sf_grps%surf_grp%num_item = group%surf_grp%num_item
!
      call alloc_merged_surf_grps_stack(1, view_sf_grps)
      call alloc_merged_group_item(view_sf_grps%surf_grp)
!
      view_sf_grps%grp_name(1:view_sf_grps%num_grp)                     &
     &        = group%surf_grp%grp_name(1:view_sf_grps%num_grp)
!
      call set_surface_group_item_viewer                                &
     &   (mgd_sf_grp, view_sf_grps%surf_grp)
      call set_surface_group_stack_viewer(nsurf_viewer,                 &
     &    group, view_sf_grps%num_grp, view_sf_grps%surf_grp)
!
      end subroutine s_set_groups_4_viewer_surface
!
!------------------------------------------------------------------
!
      end module set_surf_connect_4_viewer
