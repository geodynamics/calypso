!t_mesh_data.f90
!      module t_mesh_data
!
!> @brief Structure for mesh data
!
!>   including geometry, connectivity, and groups
!
!     Written by H. Matsui on Dec., 2008
!
!!      subroutine dealloc_mesh_infomations(nod_comm,                   &
!!     &          node, ele, surf, edge, nod_grp, ele_grp, surf_grp,    &
!!     &          tbls_ele_grp, tbls_sf_grp, surf_nod_grp)
!!      subroutine dealloc_nod_ele_infos(node, ele, surf, edge,         &
!!     &          nod_grp, ele_grp, surf_grp, nod_comm)
!!      subroutine dealloc_mesh_infos(mesh, group)
!!
!!      subroutine dealloc_mesh_type(mesh)
!!      subroutine dealloc_groups_data(group)
!!
!!      subroutine dealloc_ele_surf_edge_type(ele_mesh)
!!      subroutine check_smp_size_surf_edge_type(ele_mesh)
!!
!!      subroutine compare_mesh_groups(my_rank, group_ref, group)
!!        type(mesh_groups), intent(in) :: group_ref, group
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
!>     Structure for grid data
        type(mesh_geometry) :: mesh
!>     Structure for group data
        type(mesh_groups) ::   group
      end type mesh_data
!
!
!>     Structure for element, surface, and edge mesh
!!        (position, connectivity, and communication)
      type element_geometry
!>     Structure for element communication
        type(communication_table) :: ele_comm
!
!>     Structure for surface communication
        type(communication_table) :: surf_comm
!>     Structure for surface position and connectivity
        type(surface_data) ::        surf
!
!>     Structure for edge communication
        type(communication_table) :: edge_comm
!>     Structure for edge position and connectivity
        type(edge_data) ::           edge
      end type element_geometry
!
!
      private :: dealloc_surf_mesh_type, dealloc_edge_mesh_type
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine dealloc_mesh_infomations(mesh, group, ele_mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      call dealloc_grp_connect(group%tbls_surf_grp%edge)
      call dealloc_surf_item_sf_grp_type(group%tbls_surf_grp)
      call dealloc_num_surf_grp_nod_smp(group%surf_nod_grp)
      call dealloc_surf_grp_nod(group%surf_nod_grp)
!
      call dealloc_grp_connect(group%tbls_ele_grp%surf)
      call dealloc_grp_connect(group%tbls_ele_grp%edge)
      call dealloc_grp_connect(group%tbls_ele_grp%node)
!
!
      call deallocate_surface_geom_type(ele_mesh%surf)
      call deallocate_edge_geom_type(ele_mesh%edge)
!
!      call deallocate_iso_surface_type(ele_mesh%surf)
!      call deallocate_ext_surface_type(ele_mesh%surf)
!
      call deallocate_edge_param_smp_type(ele_mesh%edge)
      call deallocate_surf_param_smp_type(ele_mesh%surf)
!
      call deallocate_edge_4_ele_type(ele_mesh%edge)
      call deallocate_edge_connect_type(ele_mesh%edge)
      call deallocate_surface_connect_type(ele_mesh%surf)
      call dealloc_ele_4_surf_type(ele_mesh%surf)
!
      call dealloc_nod_ele_infos(mesh, group, ele_mesh)
!
      end subroutine dealloc_mesh_infomations
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_nod_ele_infos(mesh, group, ele_mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      call deallocate_sf_grp_type_smp(group%surf_grp)
      call deallocate_grp_type_smp(group%ele_grp)
      call deallocate_grp_type_smp(group%nod_grp)
!
      call deallocate_inod_in_edge_type(ele_mesh%edge)
      call deallocate_inod_in_surf_type(ele_mesh%surf)
      call deallocate_ele_param_smp_type(mesh%ele)
      call deallocate_node_param_smp_type(mesh%node)
!
      call deallocate_ele_geometry_type(mesh%ele)
!
      call dealloc_mesh_infos(mesh, group)
!
      end subroutine dealloc_nod_ele_infos
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_mesh_infos(mesh, group)
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!
      call deallocate_ele_connect_type(mesh%ele)
      call deallocate_node_geometry_type(mesh%node)
      call deallocate_type_comm_tbl(mesh%nod_comm)
!
      call deallocate_grp_type(group%nod_grp)
      call deallocate_grp_type(group%ele_grp)
      call deallocate_sf_grp_type(group%surf_grp)
!
      end subroutine dealloc_mesh_infos
!
! ----------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_groups_data(group)
!
      type(mesh_groups), intent(inout) :: group
!
!
      call deallocate_grp_type(group%nod_grp)
      call deallocate_grp_type(group%ele_grp)
      call deallocate_sf_grp_type(group%surf_grp)
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
      call deallocate_ele_connect_type(mesh%ele)
      call deallocate_node_geometry_type(mesh%node)
      call deallocate_type_comm_tbl(mesh%nod_comm)
!
      end subroutine dealloc_mesh_type
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_ele_surf_edge_type(ele_mesh)
!
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      call deallocate_type_comm_tbl(ele_mesh%ele_comm)
!
      call dealloc_surf_mesh_type(ele_mesh)
      call dealloc_edge_mesh_type(ele_mesh)
!
      end subroutine dealloc_ele_surf_edge_type
!
!------------------------------------------------------------------
!
      subroutine dealloc_surf_mesh_type(ele_mesh)
!
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      call deallocate_type_comm_tbl(ele_mesh%surf_comm)
      call deallocate_surface_connect_type(ele_mesh%surf)
!
      end subroutine dealloc_surf_mesh_type
!
!------------------------------------------------------------------
!
      subroutine dealloc_edge_mesh_type(ele_mesh)
!
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      call deallocate_type_comm_tbl(ele_mesh%edge_comm)
      call deallocate_edge_connect_type(ele_mesh%edge)
      call deallocate_edge_4_ele_type(ele_mesh%edge)
!
      end subroutine dealloc_edge_mesh_type
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine check_smp_size_type(my_rank, mesh)
!
      type(mesh_geometry) :: mesh
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank,                                      &
     &        'mesh%node%istack_nod_smp: ', mesh%node%istack_nod_smp
       write(*,*) 'PE: ', my_rank,                                      &
     &        'mesh%node%istack_nod_smp: ', mesh%node%istack_nod_smp
       write(*,*) 'PE: ', my_rank,                                      &
     &        'mesh%ele%istack_ele_smp: ', mesh%ele%istack_ele_smp
!
      end subroutine check_smp_size_type
!
!-----------------------------------------------------------------------
!
      subroutine check_smp_size_surf_edge_type(ele_mesh)
!
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      write(*,*) 'ele_mesh%surfistack_surf_smp ',                       &
     &           ele_mesh%surf%istack_surf_smp
      write(*,*) 'ele_mesh%edge%istack_edge_smp ',                      &
     &           ele_mesh%edge%istack_edge_smp
!
      end subroutine check_smp_size_surf_edge_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine compare_mesh_groups(my_rank, group_ref, group)
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_groups), intent(in) :: group_ref, group
!
!
      call compare_group_types                                         &
     &   (my_rank, group_ref%nod_grp, group%nod_grp)
      call compare_group_types                                         &
     &   (my_rank, group_ref%ele_grp, group%ele_grp)
      call compare_surface_grp_types                                   &
     &   (my_rank, group_ref%surf_grp, group%surf_grp)
!
      end subroutine compare_mesh_groups
!
!-----------------------------------------------------------------------
!
      end module t_mesh_data
