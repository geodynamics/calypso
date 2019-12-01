!>@file   renumber_para_viewer_mesh.f90
!!@brief  module renumber_para_viewer_mesh
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief Surface mesh data generator for kemoviewer
!!
!!@verbatim
!!      subroutine s_renumber_para_viewer_mesh                          &
!!     &         (nshift_node, nshift_surf, nshift_edge, mgd_v_mesh)
!!        type(merged_viewer_mesh), intent(inout) :: mgd_v_mesh
!!@endverbatim
!
      module renumber_para_viewer_mesh
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use t_viewer_mesh
      use t_viewer_group
      use t_merged_viewer_mesh
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_renumber_para_viewer_mesh                            &
     &         (nshift_node, nshift_surf, nshift_edge, mgd_v_mesh)
!
      integer(kind = kint), intent(in) :: nshift_node, nshift_surf
      integer(kind = kint), intent(in) :: nshift_edge
      type(merged_viewer_mesh), intent(inout) :: mgd_v_mesh
!
!
      call set_global_node_info_4_viewer                                &
     &   (nshift_node, mgd_v_mesh%view_mesh)
      call set_global_surf_info_4_viewer                                &
     &   (mgd_v_mesh%view_mesh%nnod_v_surf,                             &
     &    nshift_node, nshift_surf, mgd_v_mesh%view_mesh)
      call set_global_edge_info_4_viewer                                &
     &   (mgd_v_mesh%view_mesh%nnod_v_edge,                             &
     &    nshift_node, nshift_edge, mgd_v_mesh%view_mesh)
!
!
      call shift_global_surf_grps_items                                 &
     &   (nshift_node, nshift_surf, nshift_edge,                        &
     &    mgd_v_mesh%domain_grps)
!
      call set_global_group_items                                       &
     &   (nshift_node, mgd_v_mesh%view_nod_grps%node_grp)
!
      call shift_global_surf_grps_items                                 &
     &   (nshift_node, nshift_surf, nshift_edge,                        &
     &    mgd_v_mesh%view_ele_grps)
!
      call shift_global_surf_grps_items                                 &
     &   (nshift_node, nshift_surf, nshift_edge,                        &
     &    mgd_v_mesh%view_sf_grps)
!
      end subroutine s_renumber_para_viewer_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_global_node_info_4_viewer(nshift_node, view_mesh)
!
      integer(kind = kint), intent(in) :: nshift_node
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: i
!
!
!$omp parallel do
      do i = 1, view_mesh%nnod_viewer
        view_mesh%inod_gl_view(i) = i + nshift_node
      end do
!$omp end parallel do
!
      end subroutine set_global_node_info_4_viewer
!
! -----------------------------------------------------------------------
!
      subroutine set_global_surf_info_4_viewer                          &
     &         (nnod_4_surf, nshift_node, nshift_surf, view_mesh)
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: nshift_node, nshift_surf
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: i, k
!
!
!$omp parallel do
      do i = 1, view_mesh%nsurf_viewer
        view_mesh%isurf_gl_view(i) = i + nshift_surf
      end do
!$omp end parallel do
!
!$omp parallel
      do k = 1, nnod_4_surf
!$omp do
        do i = 1, view_mesh%nsurf_viewer
          view_mesh%ie_sf_viewer(i,k)                                   &
     &          = view_mesh%ie_sf_viewer(i,k) + nshift_node
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine set_global_surf_info_4_viewer
!
! -----------------------------------------------------------------------
!
      subroutine set_global_edge_info_4_viewer                          &
     &         (nnod_4_edge, nshift_node, nshift_edge, view_mesh)
!
      integer(kind = kint), intent(in) :: nnod_4_edge
      integer(kind = kint), intent(in) :: nshift_node, nshift_edge
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: i, k
!
!
!$omp parallel do
      do i = 1, view_mesh%nedge_viewer
        view_mesh%iedge_gl_view(i) = i + nshift_edge
      end do
!$omp end parallel do
!$omp parallel
      do k = 1, nnod_4_edge
!$omp do
        do i = 1, view_mesh%nedge_viewer
          view_mesh%ie_edge_viewer(i,k)                                 &
     &          = view_mesh%ie_edge_viewer(i,k) + nshift_node
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!
!$omp parallel
      do k = 1, nedge_4_surf
!$omp do
        do i = 1, view_mesh%nsurf_viewer
          view_mesh%iedge_sf_viewer(i,k)                                &
     &        = view_mesh%iedge_sf_viewer(i,k)                          &
     &         + sign(nshift_edge, view_mesh%iedge_sf_viewer(i,k))
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine set_global_edge_info_4_viewer
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine shift_global_surf_grps_items                           &
     &         (nshift_node, nshift_surf, nshift_edge, view_grps)
!
      integer(kind = kint), intent(in) :: nshift_node, nshift_surf
      integer(kind = kint), intent(in) :: nshift_edge
      type(viewer_surface_groups), intent(inout) :: view_grps
!
!
      call set_global_group_items(nshift_node, view_grps%node_grp)
      call set_global_group_items(nshift_surf, view_grps%surf_grp)
      call set_global_group_items(nshift_edge, view_grps%edge_grp)
!
      end subroutine shift_global_surf_grps_items
!
!------------------------------------------------------------------
!
      subroutine set_global_group_items(nshift, viewer_grp)
!
      integer(kind = kint), intent(in) :: nshift
      type(viewer_group_data), intent(inout) :: viewer_grp
!
      integer(kind = kint) :: i
!
!$omp parallel do
      do i = 1, viewer_grp%num_item
        viewer_grp%item_sf(i) = viewer_grp%item_sf(i)                   &
     &                     + sign(nshift,viewer_grp%item_sf(i))
      end do
!$omp end parallel do
!
      end subroutine set_global_group_items
!
! -----------------------------------------------------------------------
!
      end module renumber_para_viewer_mesh
