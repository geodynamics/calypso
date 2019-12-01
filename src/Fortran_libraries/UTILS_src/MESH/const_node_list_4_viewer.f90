!
!      module const_node_list_4_viewer
!
!      Written by Kemorin on Jan., 2007
!
!!      subroutine allocate_imark_node_4_list(numnod)
!!      subroutine deallocate_imark_node_4_list
!!
!!      subroutine mark_node_4_domain_viewer(nnod_4_surf, nnod_4_edge,  &
!!     &          view_mesh, domain_edge_grp, domain_surf_grp)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(viewer_group_data), intent(in) :: domain_edge_grp
!!        type(viewer_group_data), intent(in) :: domain_surf_grp
!!      subroutine mark_node_4_ele_grp_viewer                           &
!!     &         (igrp, nnod_4_surf, nnod_4_edge, view_mesh,            &
!!     &          domain_edge_grp, ele_surf_grp, ele_edge_grp)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(viewer_group_data), intent(in) :: domain_edge_grp
!!        type(viewer_group_data), intent(in) :: ele_surf_grp
!!        type(viewer_group_data), intent(in) :: ele_edge_grp
!!      subroutine mark_node_4_surf_grp_viewer                          &
!!     &         (igrp, nnod_4_surf, nnod_4_edge, view_mesh,            &
!!     &          domain_edge_grp, sf_surf_grp, sf_edge_grp)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(viewer_group_data), intent(in) :: domain_edge_grp
!!        type(viewer_group_data), intent(in)  :: sf_surf_grp
!!        type(viewer_group_data), intent(in)  :: sf_edge_grp
!!
!!      subroutine count_nod_stack_4_ele_gp_viewer                      &
!!     &         (igrp, nnod_viewer, ele_nod_grp)
!!        type(viewer_group_data), intent(inout) :: ele_nod_grp
!!
!!      subroutine const_nod_4_ele_gp_viewer                            &
!!     &         (igrp, nnod_viewerk, ele_nod_grp)
!!        type(viewer_group_data), intent(inout) :: ele_nod_grp
!
      module const_node_list_4_viewer
!
      use m_precision
      use m_geometry_constants
!
      use t_viewer_mesh
      use t_viewer_group
!
      implicit none
!
      integer(kind = kint), allocatable, private :: imark_node(:)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_imark_node_4_list(numnod)
!
      integer(kind = kint) :: numnod
!
      allocate( imark_node(numnod) )
      imark_node = 0
!
      end subroutine allocate_imark_node_4_list
!
!------------------------------------------------------------------
!
      subroutine deallocate_imark_node_4_list
!
      deallocate( imark_node )
!
      end subroutine deallocate_imark_node_4_list
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mark_node_4_domain_viewer(nnod_4_surf, nnod_4_edge,    &
     &          view_mesh, domain_edge_grp, domain_surf_grp)
!
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
      type(viewer_mesh_data), intent(in) :: view_mesh
      type(viewer_group_data), intent(in) :: domain_edge_grp
      type(viewer_group_data), intent(in) :: domain_surf_grp
!
      integer(kind = kint) :: k1, inum, isurf, iedge, inod
!
!
      imark_node = 0
      do k1 = 1, nnod_4_edge
        do inum = 1, domain_edge_grp%num_item
          iedge = abs(domain_edge_grp%item_sf(inum))
          inod = view_mesh%ie_edge_viewer(iedge,k1)
          imark_node(inod) = 1
        end do
      end do
!
      if (nnod_4_surf .eq. num_lag_sf) then
        do inum = 1, domain_edge_grp%num_item
          isurf = domain_surf_grp%item_sf(inum)
          inod = view_mesh%ie_sf_viewer(isurf,num_lag_sf)
          imark_node(inod) = 1
        end do
      end if
!
      end subroutine mark_node_4_domain_viewer
!
!------------------------------------------------------------------
!
      subroutine mark_node_4_ele_grp_viewer                             &
     &         (igrp, nnod_4_surf, nnod_4_edge, view_mesh,              &
     &          domain_edge_grp, ele_surf_grp, ele_edge_grp)
!
      integer(kind = kint), intent(in) :: igrp
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
      type(viewer_mesh_data), intent(in) :: view_mesh
      type(viewer_group_data), intent(in) :: ele_surf_grp
      type(viewer_group_data), intent(in) :: ele_edge_grp
      type(viewer_group_data), intent(in) :: domain_edge_grp
!
      integer(kind = kint) :: k1, ist, ied, inum, isurf, iedge, inod
!
!
      imark_node = 0
!
      ist = ele_edge_grp%istack_sf(igrp-1) + 1
      ied = ele_edge_grp%istack_sf(igrp)
      do k1 = 1, nnod_4_edge
        do inum = ist, ied
          iedge = abs(ele_edge_grp%item_sf(inum))
          inod = view_mesh%ie_edge_viewer(iedge,k1)
          imark_node(inod) = 1
        end do
      end do
!
      if (nnod_4_surf .eq. num_lag_sf) then
        ist = ele_surf_grp%istack_sf(igrp-1) + 1
        ied = ele_surf_grp%istack_sf(igrp)
        do inum = 1, domain_edge_grp%num_item
          isurf = abs(ele_surf_grp%item_sf(inum))
          inod = view_mesh%ie_sf_viewer(isurf,num_lag_sf)
          imark_node(inod) = 1
        end do
      end if
!
      end subroutine mark_node_4_ele_grp_viewer
!
!------------------------------------------------------------------
!
      subroutine mark_node_4_surf_grp_viewer                            &
     &         (igrp, nnod_4_surf, nnod_4_edge, view_mesh,              &
     &          domain_edge_grp, sf_surf_grp, sf_edge_grp)
!
      integer(kind = kint), intent(in) :: igrp
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
      type(viewer_mesh_data), intent(in) :: view_mesh
      type(viewer_group_data), intent(in) :: domain_edge_grp
      type(viewer_group_data), intent(in)  :: sf_surf_grp
      type(viewer_group_data), intent(in)  :: sf_edge_grp
!
      integer(kind = kint) :: k1, ist, ied, inum, isurf, iedge, inod
!
!
      imark_node = 0
!
      ist = sf_edge_grp%istack_sf(igrp-1) + 1
      ied = sf_edge_grp%istack_sf(igrp  )
      do k1 = 1, nnod_4_edge
        do inum = ist, ied
          iedge = abs( sf_edge_grp%item_sf(inum) )
          inod = view_mesh%ie_edge_viewer(iedge,k1)
          imark_node(inod) = 1
        end do
      end do
!
      if (nnod_4_surf .eq. num_lag_sf) then
        ist = sf_surf_grp%istack_sf(igrp-1) + 1
        ied = sf_surf_grp%istack_sf(igrp  )
        do inum = 1, domain_edge_grp%num_item
          isurf = sf_surf_grp%item_sf(inum)
          inod = view_mesh%ie_sf_viewer(isurf,num_lag_sf)
          imark_node(inod) = 1
        end do
      end if
!
      end subroutine mark_node_4_surf_grp_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_nod_stack_4_ele_gp_viewer                        &
     &         (igrp, nnod_viewer, ele_nod_grp)
!
      integer(kind = kint), intent(in) :: igrp
      integer(kind = kint), intent(in) :: nnod_viewer
      type(viewer_group_data), intent(inout) :: ele_nod_grp
!
      integer(kind = kint) :: inod
!
      ele_nod_grp%istack_sf(igrp) = ele_nod_grp%istack_sf(igrp-1)
      do inod = 1, nnod_viewer
        ele_nod_grp%istack_sf(igrp) = ele_nod_grp%istack_sf(igrp)      &
     &                                + imark_node(inod)
      end do
      ele_nod_grp%num_item = ele_nod_grp%istack_sf(igrp)
!
      end subroutine count_nod_stack_4_ele_gp_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine const_nod_4_ele_gp_viewer                              &
     &         (igrp, nnod_viewer, ele_nod_grp)
!
      integer(kind = kint), intent(in) :: igrp
      integer(kind = kint), intent(in) :: nnod_viewer
      type(viewer_group_data), intent(inout) :: ele_nod_grp
!
      integer(kind = kint) :: inod, icou
!
      icou = ele_nod_grp%istack_sf(igrp-1)
      do inod = 1, nnod_viewer
        if (imark_node(inod) .eq. 1) then
          icou = icou + 1
          ele_nod_grp%item_sf(icou) = inod
        end if
      end do
!
      end subroutine const_nod_4_ele_gp_viewer
!
!------------------------------------------------------------------
!
      end module const_node_list_4_viewer
