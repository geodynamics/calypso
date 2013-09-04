!t_mesh_data.f90
!      module t_mesh_data
!
!> @brief Structure for mesh data
!
!>   including geometry, connectivity, and groups
!
!     Written by H. Matsui on Dec., 2008
!
!      subroutine dealloc_base_mesh_type_info(femmesh)
!      type(mesh_data), intent(inout) :: femmesh
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
      use t_surface_group_geometry
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
        type (surface_group_geometry) ::  surf_grp_geom
!>     Structure for work area  for surface integration
        type (surf_grp_geom_4_fem_int) :: surf_grp_int
!
!>     Structure for element group connectivity
        type (element_group_table) ::    tbls_ele_grp
!>     Structure for surface group connectivity
        type (surface_group_table) ::    tbls_surf_grp
!
!>     Structure for infinity surface
        type (scalar_surf_BC_list) ::    infty_grp
      end type mesh_groups
!
!
!>     Structure for mesh data
!>        (position, connectivity, group, and communication)
      type mesh_data
        type(mesh_geometry) :: mesh
!<     Structure for grid data
        type(mesh_groups) ::   group
!<     Structure for group data
      end type mesh_data
!
!
!
!>     Structure for element data (communication)
      type elemens_comms
        type(communication_table) :: ele_comm
!<     Structure for element communication
      end type elemens_comms
!
!>     Structure for surface data
!>        (position, connectivity, and communication)
      type surface_geometry
        type(communication_table) :: surf_comm
!<     Structure for surface communication
        type(surface_data) ::        surf
!<     Structure for surface position and connectivity
      end type surface_geometry
!
!>     Structure for edge data
!>        (position, connectivity, and communication)
      type edge_geometry
        type(communication_table) :: edge_comm
!<     Structure for edge communication
        type(edge_data) ::           edge
!<     Structure for edge position and connectivity
      end type edge_geometry
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine dealloc_base_mesh_type_info(femmesh)
!
      type(mesh_data), intent(inout) :: femmesh
!
!      call deallocate_grp_type_smp(femmesh%group%nod_grp)
!      call deallocate_grp_type_smp(femmesh%group%ele_grp)
!      call deallocate_sf_grp_type_smp(femmesh%group%surf_grp)
!
      call deallocate_grp_type_num(femmesh%group%nod_grp)
      call deallocate_grp_type_item(femmesh%group%nod_grp)
      call deallocate_grp_type_num(femmesh%group%ele_grp)
      call deallocate_grp_type_item(femmesh%group%ele_grp)
      call deallocate_sf_grp_type_num(femmesh%group%surf_grp)
      call deallocate_sf_grp_type_item(femmesh%group%surf_grp)
!
      call deallocate_ele_geometry_type(femmesh%mesh%ele)
      call deallocate_ele_connect_type(femmesh%mesh%ele)
      call deallocate_ele_param_smp_type(femmesh%mesh%ele)
!
      call deallocate_node_param_smp_type(femmesh%mesh%node)
      call deallocate_node_geometry_type(femmesh%mesh%node)
!
      call deallocate_type_comm_tbl(femmesh%mesh%nod_comm)
!
      end subroutine dealloc_base_mesh_type_info
!
!------------------------------------------------------------------
!
      end module t_mesh_data
