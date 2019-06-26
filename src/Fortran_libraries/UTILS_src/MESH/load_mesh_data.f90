!>@file   load_mesh_data.f90
!!@brief  module load_mesh_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy FEM mesh data from IO structure
!!
!!@verbatim
!!      subroutine input_mesh(mesh_file, id_rank, mesh, group, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!      subroutine input_mesh_geometry(mesh_file, id_rank, mesh, ierr)
!!      subroutine output_mesh(mesh_file, id_rank, mesh, group)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!
!!      subroutine set_mesh(mesh, group)
!!      subroutine set_mesh_geometry_data(mesh_IO, nod_comm, node, ele)
!!      subroutine set_node_geometry_data(mesh_IO, node)
!!      subroutine set_zero_mesh_data                                   &
!!     &          (nod_comm, node, ele, surf, edge)
!!        type(communication_table), intent(inout) :: nod_comm
!!        type(node_data), intent(inout) :: node
!!        type(element_data), intent(inout) :: ele
!!        type(surface_data), intent(inout) :: surf
!!        type(edge_data), intent(inout) ::    edge
!!      subroutine set_zero_node_data(node)
!!        type(node_data), intent(inout) :: node
!!
!!      subroutine set_grp_data_from_IO(nod_grp, ele_grp, surf_grp)
!!      subroutine set_grp_data_to_IO(nod_grp, ele_grp, surf_grp)
!!        type(mesh_groups), intent(inout) :: mesh_group_IO
!!        type(group_data), intent(inout) :: nod_grp, ele_grp
!!        type(surface_group_data), intent(inout) :: surf_grp
!!@endverbatim
!
      module load_mesh_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine input_mesh(mesh_file, id_rank, mesh, group, ierr)
!
      use mesh_IO_select
      use set_nnod_4_ele_by_type
      use cal_minmax_and_stacks
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      integer(kind = kint), intent(inout) :: ierr
!
!
      call sel_read_mesh(mesh_file, id_rank, mesh, group, ierr)
!
      call s_cal_numbers_from_stack(group%nod_grp%num_grp,              &
     &    group%nod_grp%nitem_grp, group%nod_grp%istack_grp)
      call s_cal_numbers_from_stack(group%ele_grp%num_grp,              &
     &    group%ele_grp%nitem_grp, group%ele_grp%istack_grp)
      call s_cal_numbers_from_stack(group%surf_grp%num_grp,             &
     &    group%surf_grp%nitem_grp, group%surf_grp%istack_grp)
!
      call alloc_sph_node_geometry(mesh%node)
      call set_3D_nnod_4_sfed_by_ele(mesh%ele%nnod_4_ele,               &
     &    mesh%surf%nnod_4_surf, mesh%edge%nnod_4_edge)
!
      end subroutine input_mesh
!
! -----------------------------------------------------------------------
!
      subroutine input_mesh_geometry(mesh_file, id_rank, mesh, ierr)
!
      use mesh_IO_select
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_geometry), intent(inout) :: mesh
      integer(kind = kint), intent(inout) :: ierr
!
      type(mesh_geometry) :: mesh_IO_i
!
!
      call sel_read_mesh_geometry(mesh_file, id_rank, mesh_IO_i, ierr)
      call set_mesh_geometry_data(mesh_IO_i,                            &
     &    mesh%nod_comm, mesh%node, mesh%ele)
!
      end subroutine input_mesh_geometry
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_mesh(mesh_file, id_rank, mesh, group)
!
      use mesh_IO_select
      use set_element_data_4_IO
      use copy_mesh_structures
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
      type(mesh_data) :: fem_IO_i
!
!
      call copy_comm_tbl_type(mesh%nod_comm, fem_IO_i%mesh%nod_comm)
      call copy_node_geometry_types(mesh%node, fem_IO_i%mesh%node)
      call copy_ele_connect_to_IO(mesh%ele, fem_IO_i%mesh%ele)
!
      call set_grp_data_to_IO                                           &
     &   (group%nod_grp, group%ele_grp, group%surf_grp, fem_IO_i%group)
!
!       save mesh information
      call sel_write_mesh_file                                          &
     &   (mesh_file, id_rank, fem_IO_i%mesh, fem_IO_i%group)
!
      call dealloc_mesh_geometry_base(fem_IO_i%mesh)
      call dealloc_groups_data(fem_IO_i%group)
!
      end subroutine output_mesh
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_mesh(fem_IO, mesh, group)
!
      use set_nnod_4_ele_by_type
!
      type(mesh_data), intent(inout) :: fem_IO
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!
      call set_mesh_geometry_data(fem_IO%mesh,                          &
     &    mesh%nod_comm, mesh%node, mesh%ele)
      call set_grp_data_from_IO(fem_IO%group,                           &
     &    group%nod_grp, group%ele_grp, group%surf_grp)
      call dealloc_groups_data(fem_IO%group)
!
      call set_3D_nnod_4_sfed_by_ele(mesh%ele%nnod_4_ele,               &
     &    mesh%surf%nnod_4_surf, mesh%edge%nnod_4_edge)
!
      end subroutine set_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_mesh_geometry_data(mesh_IO, nod_comm, node, ele)
!
      use set_element_data_4_IO
      use copy_mesh_structures
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) ::           node
      type(element_data), intent(inout) ::        ele
!
!
      call copy_comm_tbl_type(mesh_IO%nod_comm, nod_comm)
      call copy_node_geometry_types(mesh_IO%node, node)
      call copy_ele_connect_from_IO(mesh_IO%ele, ele)
!
      call dealloc_mesh_geometry_base(mesh_IO)
!
      call alloc_sph_node_geometry(node)
!
      end subroutine set_mesh_geometry_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_geometry_data(mesh_IO, node)
!
      use set_element_data_4_IO
      use copy_mesh_structures
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      type(node_data), intent(inout) :: node
!
!
      call copy_node_geometry_types(mesh_IO%node, node)
!
      call dealloc_node_geometry_IO(mesh_IO)
!
      end subroutine set_node_geometry_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_zero_mesh_data(nod_comm, node, ele, surf, edge)
!
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use set_nnod_4_ele_by_type
!
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) ::    edge
!
!
      nod_comm%num_neib =    izero
      nod_comm%ntot_import = izero
      nod_comm%ntot_export = izero
      call alloc_comm_table_num(nod_comm)
      call alloc_comm_table_item(nod_comm)
!
      node%numnod =        izero
      node%internal_node = izero
      call alloc_node_geometry_w_sph(node)
!
      ele%numele = izero
      ele%first_ele_type = izero
      call allocate_ele_connect_type(ele)
!
      call set_3D_nnod_4_sfed_by_ele                                    &
     &   (ele%nnod_4_ele, surf%nnod_4_surf, edge%nnod_4_edge)
!
      end subroutine set_zero_mesh_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_zero_node_data(node)
!
      use t_geometry_data
!
      type(node_data), intent(inout) :: node
!
!
      node%numnod =        izero
      node%internal_node = izero
      call alloc_node_geometry_w_sph(node)
!
      end subroutine set_zero_node_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_grp_data_from_IO                                   &
     &        (mesh_group_IO, nod_grp, ele_grp, surf_grp)
!
      use set_group_types_4_IO
!
      type(mesh_groups), intent(inout) :: mesh_group_IO
      type(group_data), intent(inout) :: nod_grp, ele_grp
      type(surface_group_data), intent(inout) :: surf_grp
!
!
      call set_gruop_stracture(mesh_group_IO%nod_grp, nod_grp)
      call set_gruop_stracture(mesh_group_IO%ele_grp, ele_grp)
      call set_surf_grp_stracture(mesh_group_IO%surf_grp, surf_grp)
!
      end subroutine set_grp_data_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine set_grp_data_to_IO(nod_grp, ele_grp, surf_grp,         &
     &          mesh_group_IO)
!
      use set_group_types_4_IO
!
      type(group_data), intent(inout) :: nod_grp, ele_grp
      type(surface_group_data), intent(inout) :: surf_grp
      type(mesh_groups), intent(inout) :: mesh_group_IO
!
!
      call set_gruop_stracture(nod_grp, mesh_group_IO%nod_grp)
      call set_gruop_stracture(ele_grp, mesh_group_IO%ele_grp)
      call set_surf_grp_stracture(surf_grp, mesh_group_IO%surf_grp)
!
      end subroutine set_grp_data_to_IO
!
!-----------------------------------------------------------------------
!
      end module load_mesh_data
