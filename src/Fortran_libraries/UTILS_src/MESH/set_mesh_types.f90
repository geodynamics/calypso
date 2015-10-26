!set_mesh_types.f90
!      module set_mesh_types
!
!        programmed by H. Matsui on Dec., 2008
!
!!      subroutine set_mesh_data_types(femmesh)
!!      subroutine set_mesh_data_type_to_IO(my_rank, femmesh)
!!       type(mesh_data), intent(mesh_groups) :: femmesh
!!
!!      subroutine set_geometry_types_data(mesh)
!!      subroutine set_mesh_type_to_IO(my_rank, mesh)
!!      subroutine set_mesh_data_to_IO(my_rank, nod_comm, node, ele)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(communication_table), intent(inout) :: nod_comm
!!        type(node_data), intent(inout) ::           node
!!        type(element_data), intent(inout) ::        ele
!!
!!      subroutine set_nnod_surf_edge_for_type(surf_mesh, edge_mesh,    &
!!     &          mesh)
!!        type(mesh_geometry),    intent(in) :: mesh
!!        type(surface_geometry), intent(inout) :: surf_mesh
!!        type(edge_geometry), intent(inout) ::  edge_mesh
!
      module set_mesh_types
!
      use m_precision
!
      use t_mesh_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_data_types(femmesh)
!
      use set_group_types_4_IO
!
      type(mesh_data), intent(inout) :: femmesh
!
!
      call set_geometry_types_data(femmesh%mesh)
      call set_grp_data_type_from_IO(femmesh%group)
!
      end subroutine set_mesh_data_types
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_data_type_to_IO(my_rank, femmesh)
!
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_data), intent(inout) :: femmesh
!
!
      call set_mesh_type_to_IO(my_rank, femmesh%mesh)
      call set_grp_data_type_to_IO(femmesh%group)
!
      end subroutine set_mesh_data_type_to_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_geometry_types_data(mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call set_mesh_geometry_data(mesh%nod_comm, mesh%node, mesh%ele)
!
      end subroutine set_geometry_types_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_type_to_IO(my_rank, mesh)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call set_mesh_data_to_IO                                          &
     &   (my_rank, mesh%nod_comm, mesh%node, mesh%ele)
!
      end subroutine set_mesh_type_to_IO
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_mesh_geometry_data(nod_comm, node, ele)
!
      use set_comm_table_4_IO
      use set_node_data_4_IO
      use set_element_data_4_IO
!
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) ::           node
      type(element_data), intent(inout) ::        ele
!
!
      call copy_comm_tbl_type_from_IO(nod_comm)
!
      call copy_node_geometry_from_IO(node)
      call copy_ele_connect_from_IO(ele)
!
      call allocate_sph_node_geometry(node)
!
      end subroutine set_mesh_geometry_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_data_to_IO(my_rank, nod_comm, node, ele)
!
      use t_comm_table
      use t_geometry_data
      use set_comm_table_4_IO
      use set_element_data_4_IO
      use set_node_data_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) ::           node
      type(element_data), intent(in) ::        ele
!
!
      call copy_comm_tbl_type_to_IO(my_rank, nod_comm)
      call copy_node_geometry_to_IO(node)
      call copy_ele_connect_to_IO(ele)
!
      end subroutine set_mesh_data_to_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_nnod_surf_edge_for_type(surf_mesh, edge_mesh,      &
     &          mesh)
!
      use t_mesh_data
      use t_surface_data
      use t_edge_data
      use set_nnod_4_ele_by_type
!
!
      type(mesh_geometry),    intent(in) :: mesh
      type(surface_geometry), intent(inout) :: surf_mesh
      type(edge_geometry), intent(inout) ::  edge_mesh
!
!
      call set_3D_nnod_4_sfed_by_ele(mesh%ele%nnod_4_ele,               &
     &    surf_mesh%surf%nnod_4_surf, edge_mesh%edge%nnod_4_edge)
!
      end subroutine set_nnod_surf_edge_for_type
!
!  ---------------------------------------------------------------------
!
      end module set_mesh_types
