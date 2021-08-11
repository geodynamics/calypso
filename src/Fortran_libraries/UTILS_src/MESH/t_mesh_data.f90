!>@file   t_mesh_data.f90
!!@brief  module t_mesh_data
!!
!!@author H. Matsui
!!@date Programmed on Dec., 2008
!
!> @brief Structure for mesh data
!!   including geometry, connectivity, and groups
!
!!@verbatim
!!      subroutine dealloc_mesh_infos_w_normal(mesh, group)
!!      subroutine dealloc_mesh_infomations(mesh, group)
!!      subroutine dealloc_nod_ele_infos(mesh)
!!      subroutine dealloc_mesh_smp_stack(mesh, group)
!!      subroutine dealloc_mesh_data(mesh, group)
!!
!!      subroutine dealloc_mesh_type(mesh)
!!      subroutine dealloc_mesh_geometry_base(mesh)
!!      subroutine dealloc_groups_data(group)
!!      subroutine dealloc_surf_edge(mesh)
!!      subroutine check_mesh_smp_size(id_rank, mesh)
!!      subroutine check_surf_edge_smp_size(mesh)
!!
!!      subroutine compare_mesh_groups(id_rank, group_ref, group)
!!        type(mesh_groups), intent(in) :: group_ref, group
!!
!!      subroutine check_mesh_allocations(my_rank, geofem)
!!        integer, intent(in) :: my_rank
!!        type(mesh_data), intent(in), target :: geofem
!!@endverbatim
!
      module t_mesh_data
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_group_data
      use t_surface_group_connect
      use t_surface_data
      use t_edge_data
      use t_surface_group_normals
      use t_group_connects
      use t_surface_boundary
!
      implicit  none
!
!
!>     Structure for grid data
!>        (position, connectivity, and communication)
      type mesh_geometry
!>     Structure for node communication
        type(communication_table) :: nod_comm
!>     Structure for node position
        type(node_data) ::           node
!>     Structure for element position and connectivity
        type(element_data) ::        ele
!
!>     Structure for surface position and connectivity
        type(surface_data) ::        surf
!>     Structure for edge position and connectivity
        type(edge_data) ::           edge
      end type mesh_geometry
!
!>     Structure for group data (node, element, surface, and infinity)
      type mesh_groups
!>     Structure for node group
        type (group_data) ::             nod_grp
!>     Structure for element group
        type (group_data) ::             ele_grp
!>     Structure for surface group
        type (surface_group_data) ::     surf_grp
!
!>     Structure for node data on surface group
        type (surface_node_grp_data) ::   surf_nod_grp
!>     Structure for grometry data for surface group
        type (surface_group_normals) ::  surf_grp_norm
!
!>     Structure for infinity surface
        type (scalar_surf_BC_list) ::    infty_grp
      end type mesh_groups
!
!
!>     Structure for mesh data
!>        (position, connectivity, group, and communication)
      type mesh_data
!>     Structure for grid data
        type(mesh_geometry) :: mesh
!>     Structure for group data
        type(mesh_groups) ::   group
      end type mesh_data
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine dealloc_mesh_infos_w_normal(mesh, group)
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!
      call dealloc_vect_surf_grp_nod(group%surf_nod_grp)
!
      call dealloc_mesh_infomations(mesh, group)
      call dealloc_mesh_smp_stack(mesh, group)
      call dealloc_mesh_data(mesh, group)
!
      end subroutine dealloc_mesh_infos_w_normal
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_mesh_infomations(mesh, group)
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!
      call dealloc_num_surf_grp_nod_smp(group%surf_nod_grp)
      call dealloc_surf_grp_nod(group%surf_nod_grp)
!
      call dealloc_surface_geometory(mesh%surf)
      call dealloc_edge_geometory(mesh%edge)
!
      call deallocate_iso_surface_type(mesh%surf)
      call deallocate_ext_surface_type(mesh%surf)
!
      call dealloc_edge_param_smp(mesh%edge)
      call dealloc_surf_param_smp(mesh%surf)
!
      call dealloc_surf_edge(mesh)
!
      call dealloc_nod_ele_infos(mesh)
!
      end subroutine dealloc_mesh_infomations
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_nod_ele_infos(mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call dealloc_inod_in_edge(mesh%edge)
      call dealloc_inod_in_surf(mesh%surf)
!
      call dealloc_overlapped_ele(mesh%ele)
      call dealloc_ele_geometry(mesh%ele)
!
      end subroutine dealloc_nod_ele_infos
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_mesh_smp_stack(mesh, group)
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!
      call dealloc_sf_group_smp(group%surf_grp)
      call dealloc_group_smp(group%ele_grp)
      call dealloc_group_smp(group%nod_grp)
!
      call dealloc_ele_param_smp(mesh%ele)
      call dealloc_node_param_smp(mesh%node)
!
      end subroutine dealloc_mesh_smp_stack
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_mesh_data(mesh, group)
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!
      call dealloc_mesh_type(mesh)
      call dealloc_groups_data(group)
!
      end subroutine dealloc_mesh_data
!
! ----------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_groups_data(group)
!
      type(mesh_groups), intent(inout) :: group
!
!
      call dealloc_group(group%nod_grp)
      call dealloc_group(group%ele_grp)
      call dealloc_sf_group(group%surf_grp)
!
      end subroutine dealloc_groups_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_mesh_type(mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call dealloc_ele_connect(mesh%ele)
      call dealloc_node_geometry_w_sph(mesh%node)
      call dealloc_comm_table(mesh%nod_comm)
!
      end subroutine dealloc_mesh_type
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_mesh_geometry_base(mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call dealloc_ele_connect(mesh%ele)
      call dealloc_node_geometry_base(mesh%node)
      call dealloc_comm_table(mesh%nod_comm)
!
      end subroutine dealloc_mesh_geometry_base
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_node_geometry_IO(mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call dealloc_node_geometry_base(mesh%node)
      call dealloc_neib_id(mesh%nod_comm)
!
      end subroutine dealloc_node_geometry_IO
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_surf_edge(mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call dealloc_element_4_surface(mesh%surf)
      call dealloc_surface_connect(mesh%surf)
!
      call dealloc_edge_connect(mesh%edge)
      call dealloc_edge_4_ele(mesh%edge)
!
      end subroutine dealloc_surf_edge
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine check_mesh_smp_size(id_rank, mesh)
!
      type(mesh_geometry), intent(in) :: mesh
!
      integer, intent(in) :: id_rank
!
       write(*,*) 'PE: ', id_rank,                                      &
     &        'mesh%node%istack_nod_smp: ', mesh%node%istack_nod_smp
       write(*,*) 'PE: ', id_rank,                                      &
     &        'mesh%node%istack_nod_smp: ', mesh%node%istack_nod_smp
       write(*,*) 'PE: ', id_rank,                                      &
     &        'mesh%ele%istack_ele_smp: ', mesh%ele%istack_ele_smp
!
      end subroutine check_mesh_smp_size
!
!-----------------------------------------------------------------------
!
      subroutine check_surf_edge_smp_size(mesh)
!
      type(mesh_geometry), intent(in) :: mesh
!
!
      write(*,*) 'mesh%surfistack_surf_smp ',                           &
     &           mesh%surf%istack_surf_smp
      write(*,*) 'mesh%edge%istack_edge_smp ',                          &
     &           mesh%edge%istack_edge_smp
!
      end subroutine check_surf_edge_smp_size
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine compare_mesh_groups(id_rank, group_ref, group)
!
      integer, intent(in) :: id_rank
      type(mesh_groups), intent(in) :: group_ref, group
!
!
      call compare_group_types                                         &
     &   (id_rank, group_ref%nod_grp, group%nod_grp)
      call compare_group_types                                         &
     &   (id_rank, group_ref%ele_grp, group%ele_grp)
      call compare_surface_grp_types                                   &
     &   (id_rank, group_ref%surf_grp, group%surf_grp)
!
      end subroutine compare_mesh_groups
!
!-----------------------------------------------------------------------
!
      subroutine check_mesh_allocations(my_rank, geofem)
!
      integer, intent(in) :: my_rank
      type(mesh_data), intent(in), target :: geofem
!
      write(*,*) my_rank, 'node%istack_numnod',                         &
     &              allocated(geofem%mesh%node%istack_numnod)
      write(*,*) my_rank, 'ele%istack_numele',                          &
     &              allocated(geofem%mesh%ele%istack_numele)
      write(*,*) my_rank, 'node%inod_global',                           &
     &              allocated(geofem%mesh%node%inod_global)
      write(*,*) my_rank, 'node%rr',                                    &
     &              allocated(geofem%mesh%node%rr)
      write(*,*) my_rank, 'ele%iele_global',                            &
     &              allocated(geofem%mesh%ele%iele_global)
      write(*,*) my_rank, 'ele%ie',                                     &
     &              allocated(geofem%mesh%ele%ie)
      write(*,*) my_rank, 'ele%interior_ele',                           &
     &              allocated(geofem%mesh%ele%interior_ele)
      write(*,*) my_rank, 'ele%x_ele',                                  &
     &              allocated(geofem%mesh%ele%x_ele)
      write(*,*) my_rank, 'node%istack_nod_smp',                         &
     &              allocated(geofem%mesh%node%istack_nod_smp)
      write(*,*) my_rank, 'ele%istack_ele_smp',                         &
     &              allocated(geofem%mesh%ele%istack_ele_smp)
!
      write(*,*) my_rank, 'surf%node_on_sf',                            &
     &              allocated(geofem%mesh%surf%node_on_sf)
      write(*,*) my_rank, 'surf%isf_4_ele',                             &
     &              allocated(geofem%mesh%surf%isf_4_ele)
      write(*,*) my_rank, 'surf%isurf_global',                          &
     &              allocated(geofem%mesh%surf%isurf_global)
      write(*,*) my_rank, 'surf%isf_external',                          &
     &              allocated(geofem%mesh%surf%isf_external)
      write(*,*) my_rank, 'surf%isf_isolate',                           &
     &              allocated(geofem%mesh%surf%isf_isolate)
      write(*,*) my_rank, 'surf%x_surf',                                &
     &              allocated(geofem%mesh%surf%x_surf)
      write(*,*) my_rank, 'surf%area_surf',                             &
     &              allocated(geofem%mesh%surf%area_surf)
      write(*,*) my_rank, 'surf%istack_surf_smp',                       &
     &              allocated(geofem%mesh%surf%istack_surf_smp)
      write(*,*) my_rank, 'surf%iele_4_surf',                           &
     &              allocated(geofem%mesh%surf%iele_4_surf)
!
      write(*,*) my_rank, 'edge%node_on_edge',                          &
     &              allocated(geofem%mesh%edge%node_on_edge)
      write(*,*) my_rank, 'edge%ie_edge',                               &
     &              allocated(geofem%mesh%edge%ie_edge)
      write(*,*) my_rank, 'edge%iedge_global',                          &
     &              allocated(geofem%mesh%edge%iedge_global)
      write(*,*) my_rank, 'edge%iedge_4_ele',                           &
     &              allocated(geofem%mesh%edge%iedge_4_ele)
      write(*,*) my_rank, 'edge%iedge_isolate',                         &
     &              allocated(geofem%mesh%edge%iedge_isolate)
      write(*,*) my_rank, 'edge%x_edge',                                &
     &              allocated(geofem%mesh%edge%x_edge)
      write(*,*) my_rank, 'edge%edge_length',                           &
     &              allocated(geofem%mesh%edge%edge_length)
      write(*,*) my_rank, 'edge%istack_edge_smp',                       &
     &              allocated(geofem%mesh%edge%istack_edge_smp)
!
      write(*,*) my_rank, 'nod_comm%id_neib',                           &
     &              associated(geofem%mesh%nod_comm%id_neib)
      write(*,*) my_rank, 'nod_comm%istack_import',                     &
     &              associated(geofem%mesh%nod_comm%istack_import)
      write(*,*) my_rank, 'nod_comm%istack_export',                     &
     &              associated(geofem%mesh%nod_comm%istack_export)
      write(*,*) my_rank, 'nod_comm%item_import',                       &
     &              associated(geofem%mesh%nod_comm%item_import)
      write(*,*) my_rank, 'nod_comm%item_export',                       &
     &              associated(geofem%mesh%nod_comm%item_export)
!
      write(*,*) my_rank, 'nod_grp%grp_name',                           &
     &              allocated(geofem%group%nod_grp%grp_name)
      write(*,*) my_rank, 'ele_grp%grp_name',                           &
     &              allocated(geofem%group%ele_grp%grp_name)
      write(*,*) my_rank, 'surf_grp%grp_name',                          &
     &              allocated(geofem%group%surf_grp%grp_name)
      write(*,*) my_rank, 'nod_grp%istack_grp_smp',                     &
     &              allocated(geofem%group%nod_grp%istack_grp_smp)
      write(*,*) my_rank, 'ele_grp%istack_grp_smp',                     &
     &              allocated(geofem%group%ele_grp%istack_grp_smp)
      write(*,*) my_rank, 'surf_grp%istack_grp_smp',                    &
     &              allocated(geofem%group%surf_grp%istack_grp_smp)
!
      write(*,*) my_rank, 'surf_nod_grp%nnod_sf_grp',                   &
     &              allocated(geofem%group%surf_nod_grp%nnod_sf_grp)
      write(*,*) my_rank, 'surf_nod_grp%istack_surf_nod_smp',           &
     &      allocated(geofem%group%surf_nod_grp%istack_surf_nod_smp)
      write(*,*) my_rank, 'surf_nod_grp%inod_surf_grp',                 &
     &      allocated(geofem%group%surf_nod_grp%inod_surf_grp)
      write(*,*) my_rank, 'surf_nod_grp%surf_norm_nod',                 &
     &      allocated(geofem%group%surf_nod_grp%surf_norm_nod)
!
      write(*,*) my_rank, 'surf_grp_norm%vnorm_sf_grp',                 &
     &              allocated(geofem%group%surf_grp_norm%vnorm_sf_grp)
!
      end subroutine check_mesh_allocations
!
!-----------------------------------------------------------------------
!
      end module t_mesh_data
