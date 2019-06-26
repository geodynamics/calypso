!t_mesh_data.f90
!      module t_mesh_data
!
!> @brief Structure for mesh data
!
!>   including geometry, connectivity, and groups
!
!     Written by H. Matsui on Dec., 2008
!
!!      subroutine dealloc_mesh_infos_w_normal(mesh, group)
!!      subroutine dealloc_mesh_infomations(mesh, group)
!!      subroutine dealloc_nod_ele_infos(mesh, group)
!!      subroutine dealloc_mesh_infos(mesh, group)
!!
!!      subroutine dealloc_mesh_type(mesh)
!!      subroutine dealloc_mesh_geometry_base(mesh)
!!      subroutine dealloc_groups_data(group)
!!      subroutine dealloc_ele_surf_edge_type(mesh)
!!      subroutine check_mesh_smp_size(id_rank, mesh)
!!      subroutine check_surf_edge_smp_size(mesh)
!!
!!      subroutine compare_mesh_groups(id_rank, group_ref, group)
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
      call deallocate_surface_geom_type(mesh%surf)
      call dealloc_edge_geometory(mesh%edge)
!
      call deallocate_iso_surface_type(mesh%surf)
      call deallocate_ext_surface_type(mesh%surf)
!
      call dealloc_edge_param_smp(mesh%edge)
      call deallocate_surf_param_smp_type(mesh%surf)
!
      call dealloc_edge_4_ele(mesh%edge)
      call dealloc_edge_connect(mesh%edge)
      call deallocate_surface_connect_type(mesh%surf)
      call dealloc_ele_4_surf_type(mesh%surf)
!
      call dealloc_nod_ele_infos(mesh, group)
!
      end subroutine dealloc_mesh_infomations
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_nod_ele_infos(mesh, group)
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!
      call dealloc_sf_group_smp(group%surf_grp)
      call dealloc_group_smp(group%ele_grp)
      call dealloc_group_smp(group%nod_grp)
!
      call dealloc_inod_in_edge(mesh%edge)
      call deallocate_inod_in_surf_type(mesh%surf)
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
      call dealloc_mesh_type(mesh)
      call dealloc_groups_data(group)
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
      call deallocate_ele_connect_type(mesh%ele)
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
      call deallocate_ele_connect_type(mesh%ele)
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
      subroutine dealloc_ele_surf_edge_type(mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call deallocate_surface_connect_type(mesh%surf)
!
      call dealloc_edge_connect(mesh%edge)
      call dealloc_edge_4_ele(mesh%edge)
!
      end subroutine dealloc_ele_surf_edge_type
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
      end module t_mesh_data
