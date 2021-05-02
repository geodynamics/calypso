!>@file   const_kemoview_mesh.f90
!!@brief  module const_kemoview_mesh
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief Surface mesh data generator for kemoviewer
!!
!!@verbatim
!!      subroutine const_surf_mesh_4_viewer                             &
!!     &       (group, mesh, view_mesh, domain_grps,                    &
!!     &        view_nod_grps, view_ele_grps, view_sf_grps)
!!        type(mesh_groups), intent(in) :: group
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!        type(viewer_surface_groups), intent(inout) :: domain_grps
!!        type(viewer_node_groups), intent(inout) :: view_nod_grps
!!        type(viewer_surface_groups), intent(inout) :: view_ele_grps
!!        type(viewer_surface_groups), intent(inout) :: view_sf_grps
!!@endverbatim
!
      module const_kemoview_mesh
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_file_format_switch
!
      use t_mesh_data
      use t_file_IO_parameter
      use t_viewer_mesh
      use t_viewer_ele_grp_surface
!
      implicit none
!
      private :: set_surf_domain_id_viewer
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine const_surf_mesh_4_viewer                               &
     &         (group, mesh, view_mesh, domain_grps,                    &
     &          view_nod_grps, view_ele_grps, view_sf_grps)
!
      use const_merged_surf_4_group
      use set_surf_connect_4_viewer
      use set_nodes_4_viewer
      use const_edge_4_viewer
      use set_nodes_4_groups_viewer
      use const_mesh_information
      use const_surface_data
      use set_local_id_table_4_1ele
!
      type(mesh_groups), intent(in) :: group
      type(mesh_geometry), intent(inout) :: mesh
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
      type(viewer_surface_groups), intent(inout) :: domain_grps
!
      type(viewer_node_groups), intent(inout) :: view_nod_grps
      type(viewer_surface_groups), intent(inout) :: view_ele_grps
      type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
      type(viewer_ele_grp_surface) :: mgd_sf_grp
!
!
      call construct_surface_data(mesh%node, mesh%ele, mesh%surf)
!
!       write(*,*) 'const_merged_surface_4_ele_grp'
       call const_merged_surface_4_ele_grp(mesh%node, mesh%ele,         &
     &     group, mesh%surf, mgd_sf_grp)
!
!       write(*,*) 'const_merged_surface_4_sf_grp'
       call const_merged_surface_4_sf_grp(group, mesh%surf, mgd_sf_grp)
!
!  pickup surface and nodes
!
       call s_set_surf_connect_4_viewer(group, mesh%surf, mgd_sf_grp,   &
     &     view_mesh, domain_grps, view_ele_grps, view_sf_grps)
!       write(*,*) 's_set_nodes_4_viewer'
!
!
       call s_set_nodes_4_viewer                                        &
     &   (mesh%surf%nnod_4_surf, mesh, group, view_mesh, view_nod_grps)
!
!       write(*,*) 'set_surf_domain_id_viewer'
       call set_surf_domain_id_viewer(mesh%surf, view_mesh)
!
!       write(*,*)  'construct_edge_4_viewer'
      call alloc_inod_in_edge(mesh%edge)
      call copy_inod_in_edge(mesh%edge%nnod_4_edge,                     &
     &    mesh%edge%node_on_edge, mesh%edge%node_on_edge_sf)
!
       call construct_edge_4_viewer(mesh%surf%nnod_4_surf, mesh%edge,   &
     &     view_mesh, domain_grps, view_ele_grps, view_sf_grps)
!       write(*,*)  's_set_nodes_4_groups_viewer'
       call s_set_nodes_4_groups_viewer                                 &
     &    (mesh%surf%nnod_4_surf, mesh%edge%nnod_4_edge,                &
     &     view_mesh, domain_grps, view_ele_grps, view_sf_grps)
!
       call dealloc_n_iso_surf_4_ele_grp(mgd_sf_grp)
       call dealloc_iso_surf_4_egrp_m(mgd_sf_grp)
       call dealloc_iso_surf_4_sgrp_m(mgd_sf_grp)
!
      end subroutine const_surf_mesh_4_viewer
!
!------------------------------------------------------------------
!
      subroutine set_surf_domain_id_viewer(merged_surf, view_mesh)
!
      type(surface_data), intent(in) :: merged_surf
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
!
      call alloc_surf_type_viewer(view_mesh)
!
      if ( merged_surf%nnod_4_surf .eq. 4) then
        view_mesh%surftyp_viewer(1:view_mesh%nsurf_viewer) = 221
      else if ( merged_surf%nnod_4_surf .eq. 8) then
        view_mesh%surftyp_viewer(1:view_mesh%nsurf_viewer) = 222
      else if ( merged_surf%nnod_4_surf .eq. 9) then
        view_mesh%surftyp_viewer(1:view_mesh%nsurf_viewer) = 223
      end if
!
      end subroutine set_surf_domain_id_viewer
!
!------------------------------------------------------------------
!
      end module const_kemoview_mesh
