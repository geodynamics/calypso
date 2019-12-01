!
!      module const_grp_edge_4_viewer
!
!     Written by H. Matsui on Jan., 2007
!
!!      subroutine construct_edge_4_ele_grp(nnod_4_surf, view_mesh,     &
!!     &          ngrp_ele_sf, ele_surf_grp, ele_edge_grp, edge_sf_tbl)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(viewer_group_data), intent(in)  :: ele_surf_grp
!!        type(viewer_group_data), intent(inout)  :: ele_edge_grp
!!      subroutine construct_edge_4_surf_grp                            &
!!     &         (nnod_4_surf, nnod_4_edge, view_mesh,                  &
!!     &          ngrp_surf_sf, sf_surf_grp, sf_edge_grp, edge_sf_tbl)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(viewer_group_data), intent(in)  :: sf_surf_grp
!!        type(viewer_group_data), intent(inout)  :: sf_edge_grp
!!        type(sum_hash_tbl), intent(inout) :: edge_sf_tbl
!
      module const_grp_edge_4_viewer
!
      use m_precision
!
      use t_viewer_mesh
      use t_viewer_group
      use t_sum_hash
!
      implicit    none
!
      integer(kind=kint ), allocatable :: ele_edge_item_tmp(:)
      integer(kind=kint ), allocatable :: surf_edge_item_tmp(:)
      private :: ele_edge_item_tmp, surf_edge_item_tmp
!
      private :: allocate_ele_edge_item_tmp
      private :: allocate_sf_edge_item_tmp
      private :: deallocate_ele_edge_item_tmp
      private :: deallocate_sf_edge_item_tmp
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine construct_edge_4_ele_grp(nnod_4_surf, view_mesh,       &
     &          ngrp_ele_sf, ele_surf_grp, ele_edge_grp, edge_sf_tbl)
!
      use set_edge_hash_by_sf
      use set_edge_data_by_sf
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ngrp_ele_sf
      type(viewer_mesh_data), intent(in) :: view_mesh
      type(viewer_group_data), intent(in)  :: ele_surf_grp
!
      type(viewer_group_data), intent(inout)  :: ele_edge_grp
      type(sum_hash_tbl), intent(inout) :: edge_sf_tbl
!
      integer(kind = kint) :: igrp, ngrp, ist, nedge_grp
!
!
      call alloc_merged_group_item(ele_edge_grp)
!
      do igrp = 1, ngrp_ele_sf
        ngrp = ele_surf_grp%istack_sf(igrp)                             &
     &        - ele_surf_grp%istack_sf(igrp-1)
        ist = ele_surf_grp%istack_sf(igrp-1) + 1
!
!   set hash data for edge elements using sum of local node ID
!
        call clear_sum_hash(edge_sf_tbl)
!
!        write(*,*) 'const_part_edge_hash_4_sf', igrp
        call const_part_edge_hash_4_sf                                  &
     &    (view_mesh%nsurf_viewer, nnod_4_surf, view_mesh%ie_sf_viewer, &
     &     ngrp, ele_surf_grp%item_sf(ist), edge_sf_tbl)
!
!
        ist = ele_edge_grp%istack_sf(igrp-1)
!
        call allocate_ele_edge_item_tmp(ele_edge_grp)
        ele_edge_item_tmp(1:ele_edge_grp%num_item)                      &
     &          = ele_edge_grp%item_sf(1:ele_edge_grp%num_item)
        call dealloc_merged_group_item(ele_edge_grp)
!
        call count_num_edges_by_sf                                      &
     &     (edge_sf_tbl%ntot_id, edge_sf_tbl%ntot_list,                 &
     &      edge_sf_tbl%istack_hash, edge_sf_tbl%iend_hash,             &
     &      edge_sf_tbl%iflag_hash, nedge_grp)
        ele_edge_grp%istack_sf(igrp)                                    &
     &        = ele_edge_grp%istack_sf(igrp-1) + nedge_grp
        ele_edge_grp%num_item = ele_edge_grp%istack_sf(igrp)
!
        call alloc_merged_group_item(ele_edge_grp)
        ele_edge_grp%item_sf(1:ist) = ele_edge_item_tmp(1:ist)
        call deallocate_ele_edge_item_tmp
!
!        write(*,*) 'set_part_edges_4_sf', igrp
        call set_part_edges_4_sf(view_mesh%nsurf_viewer,                &
     &      nedge_grp, view_mesh%iedge_sf_viewer,                       &
     &      edge_sf_tbl%ntot_id, edge_sf_tbl%ntot_list,                 &
     &      edge_sf_tbl%istack_hash, edge_sf_tbl%iend_hash,             &
     &      edge_sf_tbl%id_hash, edge_sf_tbl%iflag_hash,                &
     &      ele_edge_grp%item_sf(ist+1) )
!
      end do
!
!      write(50,*) 'ele_edge_item_sf', ele_edge_grp%item_sf
!
      end subroutine construct_edge_4_ele_grp
!
!------------------------------------------------------------------
!
      subroutine construct_edge_4_surf_grp(nnod_4_surf, view_mesh,      &
     &          ngrp_surf_sf, sf_surf_grp, sf_edge_grp, edge_sf_tbl)
!
      use set_edge_hash_by_sf
      use set_edge_data_by_sf
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ngrp_surf_sf
      type(viewer_mesh_data), intent(in) :: view_mesh
      type(viewer_group_data), intent(in)  :: sf_surf_grp
      type(viewer_group_data), intent(inout)  :: sf_edge_grp
      type(sum_hash_tbl), intent(inout) :: edge_sf_tbl
!
      integer(kind = kint) :: igrp, ngrp, ist, nedge_grp
!
!
      call alloc_merged_group_item(sf_edge_grp)
!
      do igrp = 1, ngrp_surf_sf
        ngrp = sf_surf_grp%istack_sf(igrp)                              &
     &        - sf_surf_grp%istack_sf(igrp-1)
        ist = sf_surf_grp%istack_sf(igrp-1) + 1
!
!   set hash data for edge elements using sum of local node ID
!
        call clear_sum_hash(edge_sf_tbl)
!
!        write(*,*) 'const_part_edge_hash_4_sf', igrp
        call const_part_edge_hash_4_sf                                  &
     &    (view_mesh%nsurf_viewer, nnod_4_surf, view_mesh%ie_sf_viewer, &
     &     ngrp, sf_surf_grp%item_sf(ist), edge_sf_tbl)
!
!
        call allocate_sf_edge_item_tmp(sf_edge_grp)
        surf_edge_item_tmp(1:sf_edge_grp%num_item)                      &
     &          = sf_edge_grp%item_sf(1:sf_edge_grp%num_item)
        call dealloc_merged_group_item(sf_edge_grp)
!
        call count_num_edges_by_sf                                      &
     &     (edge_sf_tbl%ntot_id, edge_sf_tbl%ntot_list,                 &
     &      edge_sf_tbl%istack_hash, edge_sf_tbl%iend_hash,             &
     &      edge_sf_tbl%iflag_hash, nedge_grp)
        sf_edge_grp%istack_sf(igrp)                                     &
     &        = sf_edge_grp%istack_sf(igrp-1) + nedge_grp
        sf_edge_grp%num_item = sf_edge_grp%istack_sf(igrp)
!
        call alloc_merged_group_item(sf_edge_grp)
        ist = sf_edge_grp%istack_sf(igrp-1)
        sf_edge_grp%item_sf(1:ist) = surf_edge_item_tmp(1:ist)
        call deallocate_sf_edge_item_tmp
!
!        write(*,*) 'set_part_edges_4_sf', igrp
        call set_part_edges_4_sf(view_mesh%nsurf_viewer,                &
     &      nedge_grp, view_mesh%iedge_sf_viewer,                       &
     &      edge_sf_tbl%ntot_id, edge_sf_tbl%ntot_list,                 &
     &      edge_sf_tbl%istack_hash, edge_sf_tbl%iend_hash,             &
     &      edge_sf_tbl%id_hash, edge_sf_tbl%iflag_hash,                &
     &      sf_edge_grp%item_sf(ist+1) )
      end do
!
      end subroutine construct_edge_4_surf_grp
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_ele_edge_item_tmp(ele_edge_grp)
!
!
      type(viewer_group_data), intent(in) :: ele_edge_grp
!
      allocate( ele_edge_item_tmp(ele_edge_grp%num_item) )
!
      end subroutine allocate_ele_edge_item_tmp
!
!------------------------------------------------------------------
!
      subroutine allocate_sf_edge_item_tmp(sf_edge_grp)
!
!
      type(viewer_group_data), intent(in)  :: sf_edge_grp
!
      allocate( surf_edge_item_tmp(sf_edge_grp%num_item) )
!
      end subroutine allocate_sf_edge_item_tmp
!
!------------------------------------------------------------------
!
      subroutine deallocate_ele_edge_item_tmp
!
      deallocate( ele_edge_item_tmp )
!
      end subroutine deallocate_ele_edge_item_tmp
!
!------------------------------------------------------------------
!
      subroutine deallocate_sf_edge_item_tmp
!
      deallocate( surf_edge_item_tmp )
!
      end subroutine deallocate_sf_edge_item_tmp
!
!------------------------------------------------------------------
!
      end module const_grp_edge_4_viewer
