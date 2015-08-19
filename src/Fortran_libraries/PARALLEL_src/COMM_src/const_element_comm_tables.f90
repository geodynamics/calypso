!>@file   const_element_comm_tables.f90
!!@brief  module const_element_comm_tables
!!
!!@author H. Matsui
!!@date Programmed in June, 2015
!
!> @brief Belonged element list for each node
!!
!!@verbatim
!!      subroutine const_ele_comm_tbl_global_id                         &
!!     &         (mesh, ele_mesh, surf_mesh, edge_mesh)
!!      subroutine dealloc_ele_comm_tbls_gl_nele                        &
!!     &         (mesh, ele_mesh, surf_mesh, edge_mesh)
!!
!!      subroutine const_global_surface_id(surf, sf_comm)
!!        type(surface_data), intent(inout) :: surf
!!        type(communication_table), intent(in) :: sf_comm
!!
!!      subroutine const_global_numnod_list(node)
!!@endverbatim
!
      module const_element_comm_tables
!
      use m_precision
      use calypso_mpi
      use t_next_node_ele_4_node
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_comm_table
      use t_belonged_element_4_node
      use t_next_node_ele_4_node
!
      implicit none
!
      private :: const_ele_comm_tbl, const_surf_comm_table
      private :: const_edge_comm_table
      private :: const_ele_comm_table_type
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_ele_comm_tbl_global_id                           &
     &         (mesh, ele_mesh, surf_mesh, edge_mesh)
!
      use set_ele_id_4_node_type
!
      type(mesh_geometry), intent(inout) :: mesh
      type(element_comms), intent(inout) ::    ele_mesh
      type(surface_geometry), intent(inout) :: surf_mesh
      type(edge_geometry),    intent(inout) :: edge_mesh
!
!
      type(belonged_table) :: belongs
!
!
      call const_global_numnod_list(mesh%node)
!
      call const_ele_comm_tbl(mesh, belongs, ele_mesh)
      call const_global_numele_list(mesh%ele)
!
      call const_surf_comm_table(mesh, belongs, surf_mesh)
      call const_global_surface_id(surf_mesh%surf, surf_mesh%surf_comm)
!
      call const_edge_comm_table(mesh, belongs, edge_mesh)
      call const_global_edge_id(edge_mesh%edge, edge_mesh%edge_comm)
!
      end subroutine const_ele_comm_tbl_global_id
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_ele_comm_tbls_gl_nele                          &
     &         (mesh, ele_mesh, surf_mesh, edge_mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
      type(element_comms), intent(inout) ::    ele_mesh
      type(surface_geometry), intent(inout) :: surf_mesh
      type(edge_geometry),    intent(inout) :: edge_mesh
!
!
      call deallocate_type_comm_tbl(ele_mesh%ele_comm)
      call deallocate_type_comm_tbl(surf_mesh%surf_comm)
      call deallocate_type_comm_tbl(edge_mesh%edge_comm)
!
      call dealloc_numnod_stack(mesh%node)
      call dealloc_numele_stack(mesh%ele)
      call dealloc_numsurf_stack(surf_mesh%surf)
      call dealloc_numedge_stack(edge_mesh%edge)
!
      end subroutine dealloc_ele_comm_tbls_gl_nele
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_global_numnod_list(node)
!
      use t_geometry_data
      use const_global_element_ids
!
      type(node_data), intent(inout) :: node
!
!
      call alloc_numnod_stack(nprocs, node)
!
      call count_number_of_node_stack(node%numnod, node%istack_numnod)
      call count_number_of_node_stack                                   &
     &   (node%internal_node, node%istack_internod)
!
      end subroutine const_global_numnod_list
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_numele_list(ele)
!
      use const_global_element_ids
!
      type(element_data), intent(inout) :: ele
!
!
      call alloc_numele_stack(nprocs, ele)
!
      call count_number_of_node_stack(ele%numele, ele%istack_numele)
      call count_number_of_node_stack                                   &
     &   (ele%internal_ele, ele%istack_interele)
!
      end subroutine const_global_numele_list
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_surface_id(surf, sf_comm)
!
      use t_surface_data
      use const_global_element_ids
!
      type(surface_data), intent(inout) :: surf
      type(communication_table), intent(in) :: sf_comm
      character(len=kchara), parameter :: txt = 'surface'
!
!
      call alloc_numsurf_stack(nprocs, surf)
!
      call count_number_of_node_stack                                   &
     &  (surf%numsurf, surf%istack_numsurf)
      call count_number_of_node_stack                                   &
     &  (surf%internal_surf, surf%istack_intersurf)
!
      call set_global_ele_id                                            &
     &   (txt, surf%numsurf, surf%istack_intersurf,                     &
     &    surf%interior_surf, sf_comm, surf%isurf_global)
!
      end subroutine const_global_surface_id
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_edge_id(edge, ed_comm)
!
      use t_edge_data
      use const_global_element_ids
!
      type(edge_data), intent(inout) :: edge
      type(communication_table), intent(in) :: ed_comm
      character(len=kchara), parameter :: txt = 'edge'
!
!
      call alloc_numedge_stack(nprocs, edge)
!
      call count_number_of_node_stack                                   &
     &  (edge%numedge, edge%istack_numedge)
      call count_number_of_node_stack                                   &
     &  (edge%internal_edge, edge%istack_interedge)
!
      call set_global_ele_id                                            &
     &   (txt, edge%numedge, edge%istack_interedge,                     &
     &    edge%interior_edge, ed_comm, edge%iedge_global)
!
      end subroutine const_global_edge_id
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_ele_comm_tbl(mesh, belongs, ele_mesh)
!
      use set_ele_id_4_node_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(element_comms), intent(inout) ::    ele_mesh
      type(belonged_table), intent(inout) :: belongs
!
      character(len=kchara), parameter :: txt = 'element'
!
!
      call s_set_ele_id_4_node_type(mesh, belongs%blng_ele)
      call belonged_ele_id_4_node(mesh, belongs%host_ele)
      call const_ele_comm_table_type                                    &
     &   (txt, mesh%ele%numele, mesh%ele%interior_ele, mesh%ele%x_ele,  &
     &    mesh%node, mesh%nod_comm, belongs%blng_ele, belongs%host_ele, &
     &    ele_mesh%ele_comm)
      call dealloc_iele_belonged(belongs%host_ele)
      call dealloc_iele_belonged(belongs%blng_ele)
!
      end subroutine const_ele_comm_tbl
!
!-----------------------------------------------------------------------
!
      subroutine const_surf_comm_table(mesh, belongs, surf_mesh)
!
      use set_ele_id_4_node_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_geometry), intent(inout) :: surf_mesh
      type(belonged_table), intent(inout) :: belongs
!
      character(len=kchara), parameter :: txt = 'surface'
!
!
      call set_surf_id_4_node_type                                      &
     &   (mesh, surf_mesh%surf, belongs%blng_surf)
      call belonged_surf_id_4_node                                      &
     &   (mesh, surf_mesh%surf, belongs%host_surf)
      call const_ele_comm_table_type                                    &
     &   (txt, surf_mesh%surf%numsurf, surf_mesh%surf%interior_surf,    &
     &    surf_mesh%surf%x_surf, mesh%node, mesh%nod_comm,              &
     &    belongs%blng_surf, belongs%host_surf, surf_mesh%surf_comm)
      call dealloc_iele_belonged(belongs%host_surf)
      call dealloc_iele_belonged(belongs%blng_surf)
!
      end subroutine const_surf_comm_table
!
!-----------------------------------------------------------------------
!
      subroutine const_edge_comm_table(mesh, belongs, edge_mesh)
!
      use set_ele_id_4_node_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(edge_geometry), intent(inout) :: edge_mesh
      type(belonged_table), intent(inout) :: belongs
!
      character(len=kchara), parameter :: txt = 'edge'
!
!
      call set_edge_id_4_node_type                                      &
     &   (mesh, edge_mesh%edge, belongs%blng_edge)
      call belonged_edge_id_4_node                                      &
     &   (mesh, edge_mesh%edge, belongs%host_edge)
      call const_ele_comm_table_type                                    &
     &   (txt, edge_mesh%edge%numedge, edge_mesh%edge%interior_edge,    &
     &    edge_mesh%edge%x_edge, mesh%node, mesh%nod_comm,              &
     &    belongs%blng_edge, belongs%host_edge, edge_mesh%edge_comm)
      call dealloc_iele_belonged(belongs%host_edge)
      call dealloc_iele_belonged(belongs%blng_edge)
!
      end subroutine const_edge_comm_table
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_ele_comm_table_type                              &
     &         (txt, numele, internal_flag, x_ele, node, nod_comm,      &
     &          neib_e, host, e_comm)
!
      use const_element_comm_table
      use const_global_element_ids
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      type(node_data), intent(in) :: node
      type(element_around_node), intent(in) :: host
      type(element_around_node), intent(in) :: neib_e
      type(communication_table), intent(in) :: nod_comm
!
      type(communication_table), intent(inout) :: e_comm
!
!
      e_comm%num_neib = nod_comm%num_neib
      call allocate_type_neib_id(e_comm)
      call allocate_type_import_num(e_comm)
!
      call count_element_import_num(node%numnod, host%istack_4_node,    &
     &    nod_comm%num_neib, nod_comm%id_neib,                          &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    e_comm%num_neib, e_comm%id_neib, e_comm%num_import,           &
     &    e_comm%istack_import, e_comm%ntot_import)
!
      call allocate_element_rev_imports(e_comm%ntot_import)
      call allocate_type_import_item(e_comm)
!
!
      call set_element_import_item(node%numnod, numele,                 &
     &    node%inod_global, x_ele, host%istack_4_node,                  &
     &    host%iele_4_node, nod_comm%num_neib, nod_comm%istack_import,  &
     &    nod_comm%item_import, e_comm%num_neib, e_comm%istack_import,  &
     &    e_comm%item_import)
!
      call allocate_type_export_num(e_comm)
!
      call element_num_reverse_SR(e_comm%num_neib, e_comm%id_neib,      &
     &    e_comm%num_import, e_comm%num_export, e_comm%istack_export,   &
     &    e_comm%ntot_export)
!
      call allocate_element_rev_exports(e_comm%ntot_export)
      call allocate_type_export_item(e_comm)
!
      call element_position_reverse_SR(e_comm%num_neib, e_comm%id_neib, &
     &    e_comm%istack_import, e_comm%istack_export)
!
      call set_element_export_item(txt, node%numnod, numele,            &
     &    node%inod_global, internal_flag, x_ele, neib_e%istack_4_node, &
     &    neib_e%iele_4_node, nod_comm%num_neib,                        &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    e_comm%num_neib, e_comm%istack_export, e_comm%item_export)
!
      call deallocate_element_rev_list
!
      call check_element_position(txt, numele, x_ele, e_comm)
!
      end subroutine const_ele_comm_table_type
!
!-----------------------------------------------------------------------
!
      end module const_element_comm_tables
