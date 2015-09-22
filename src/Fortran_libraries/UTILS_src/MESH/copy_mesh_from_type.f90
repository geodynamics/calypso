!
!     module copy_mesh_from_type
!
!      written by H. Matsui on June, 2007
!
!>@file   copy_mesh_from_type.f90
!!@brief  module copy_mesh_from_type
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2010
!
!>@brief Copy FEM mesh data from structure to 1st mesh module
!!
!!@verbatim
!!      subroutine set_mesh_from_type(mesh, group)
!!      subroutine compare_mesh_type_vs_1st(my_rank, mesh, group)
!!      subroutine set_geometry_data_from_type(mesh)
!!      subroutine compare_geometry_type_vs_1st(my_rank, mesh)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) :: group
!!@endverbatim
!
      module copy_mesh_from_type
!
      use m_precision
!
      implicit  none
!
!
      private :: set_geometry_data_from_type
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_mesh_from_type(mesh, group)
!
      use t_mesh_data
      use m_group_data
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
!
      call set_geometry_data_from_type(mesh)
      call group_data_from_type(group)
!
      end subroutine set_mesh_from_type
!
!  ---------------------------------------------------------------------
!
      subroutine compare_mesh_type_vs_1st(my_rank, mesh, group)
!
      use t_mesh_data
      use m_group_data
!
      integer(kind = kint), intent(in)  :: my_rank
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
!
      call compare_geometry_type_vs_1st(my_rank, mesh)
      call compare_group_type_vs_1st(my_rank, group)
!
      end subroutine compare_mesh_type_vs_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_geometry_data_from_type(mesh)
!
      use m_nod_comm_table
      use m_geometry_data
      use t_mesh_data
      use copy_mesh_structures
      use set_nnod_4_ele_by_type
!
      type(mesh_geometry),    intent(inout) :: mesh
!
!
      call copy_comm_tbl_types(mesh%nod_comm, nod_comm)
!
      call copy_node_geometry_types(mesh%node, node1)
      call copy_element_connect_types(mesh%ele, ele1)
!
      call allocate_sph_node_geometry(mesh%node)
      call allocate_ele_geometry_type(ele1)
      call set_3D_nnod_4_sfed_by_ele                                   &
     &   (ele1%nnod_4_ele, surf1%nnod_4_surf, edge1%nnod_4_edge)
!
      call deallocate_ele_connect_type(mesh%ele)
      call deallocate_node_geometry_type(mesh%node)
!
      end subroutine set_geometry_data_from_type
!
!-----------------------------------------------------------------------
!
      subroutine compare_geometry_type_vs_1st(my_rank, mesh)
!
      use m_nod_comm_table
      use m_geometry_data
      use t_mesh_data
      use compare_mesh_structures
!
      integer(kind = kint), intent(in)  :: my_rank
      type(mesh_geometry),    intent(inout) :: mesh
!
!
      call compare_node_types(my_rank, node1, mesh%node)
      call compare_element_types(my_rank, ele1, mesh%ele)
      call compare_node_comm_types(my_rank, nod_comm, mesh%nod_comm)
!
      end subroutine compare_geometry_type_vs_1st
!
!-----------------------------------------------------------------------
!
      end module copy_mesh_from_type
