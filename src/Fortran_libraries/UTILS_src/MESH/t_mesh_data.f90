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
      end module t_mesh_data
