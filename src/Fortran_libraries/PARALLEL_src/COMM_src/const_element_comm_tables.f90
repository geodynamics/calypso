!>@file   const_element_comm_tables.f90
!!@brief  module const_element_comm_tables
!!
!!@author H. Matsui
!!@date Programmed in June, 2015
!
!> @brief Belonged element list for each node
!!
!!@verbatim
!!      subroutine empty_element_comm_tbls(ele_mesh)
!!      subroutine const_element_comm_tbls(mesh, ele_mesh)
!!      subroutine dealloc_ele_comm_tbls_gl_nele(mesh, ele_mesh)
!!        type(mesh_geometry), intent(inout) ::    mesh
!!        type(element_geometry), intent(inout) :: ele_mesh
!!
!!      subroutine const_global_element_id(ele)
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
      use m_machine_parameter
!
      implicit none
!
      type(belonged_table), save, private :: blng_tbl
!
!      private :: const_ele_comm_tbl, const_surf_comm_table
!      private :: const_edge_comm_table
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine empty_element_comm_tbls(ele_mesh)
!
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      call empty_comm_table(ele_mesh%ele_comm)
      call empty_comm_table(ele_mesh%surf_comm)
      call empty_comm_table(ele_mesh%edge_comm)
!
      end subroutine empty_element_comm_tbls
!
!-----------------------------------------------------------------------
!
      subroutine const_element_comm_tbls(mesh, ele_mesh)
!
      use set_ele_id_4_node_type
!
      type(mesh_geometry), intent(inout) :: mesh
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      if(iflag_debug.gt.0) write(*,*)' const_global_numnod_list'
      call const_global_numnod_list(mesh%node)
!
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_tbl'
      call const_ele_comm_tbl(mesh%node, mesh%ele, mesh%nod_comm,       &
     &    blng_tbl, ele_mesh%ele_comm)
      call const_global_element_id(mesh%ele, ele_mesh%ele_comm)
!
      if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table'
      call const_surf_comm_table(mesh%node, mesh%nod_comm,              &
     &    ele_mesh%surf, blng_tbl, ele_mesh%surf_comm)
      if(iflag_debug.gt.0) write(*,*)' const_global_surface_id'
      call const_global_surface_id(ele_mesh%surf, ele_mesh%surf_comm)
      call calypso_mpi_barrier
!
      if(iflag_debug.gt.0) write(*,*)' const_edge_comm_table'
      call const_edge_comm_table(mesh%node, mesh%nod_comm,              &
     &    ele_mesh%edge, blng_tbl, ele_mesh%edge_comm)
      if(iflag_debug.gt.0) write(*,*)' const_global_edge_id'
      call const_global_edge_id(ele_mesh%edge, ele_mesh%edge_comm)
      call calypso_mpi_barrier
!
      end subroutine const_element_comm_tbls
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_ele_comm_tbls_gl_nele(mesh, ele_mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      call deallocate_type_comm_tbl(ele_mesh%ele_comm)
      call deallocate_type_comm_tbl(ele_mesh%surf_comm)
      call deallocate_type_comm_tbl(ele_mesh%edge_comm)
!
      call dealloc_numnod_stack(mesh%node)
      call dealloc_numele_stack(mesh%ele)
      call dealloc_numedge_stack(ele_mesh%edge)
!
      end subroutine dealloc_ele_comm_tbls_gl_nele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_global_numnod_list(node)
!
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
      subroutine const_global_element_id(ele, ele_comm)
!
      use const_global_element_ids
!
      type(element_data), intent(inout) :: ele
      type(communication_table), intent(in) :: ele_comm
!
      character(len=kchara), parameter :: txt = 'element'
!
!
      call const_global_numele_list(ele)
      call set_global_ele_id(txt, ele%numele, ele%istack_interele,      &
     &   ele%interior_ele, ele_comm, ele%iele_global)
!
      end subroutine const_global_element_id
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_surface_id(surf, sf_comm)
!
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
      call dealloc_numsurf_stack(surf)
!
      end subroutine const_global_surface_id
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_edge_id(edge, ed_comm)
!
      use const_global_element_ids
!
      type(edge_data), intent(inout) :: edge
      type(communication_table), intent(in) :: ed_comm
      character(len=kchara), parameter :: txt = 'edge'
!
!
      call alloc_numedge_stack(nprocs, edge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' count_number_of_node_stack in edge'
      call count_number_of_node_stack                                   &
     &  (edge%numedge, edge%istack_numedge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' count_number_of_node_stack in edge'
      call count_number_of_node_stack                                   &
     &  (edge%internal_edge, edge%istack_interedge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' set_global_ele_id in edge'
      call set_global_ele_id                                            &
     &   (txt, edge%numedge, edge%istack_interedge,                     &
     &    edge%interior_edge, ed_comm, edge%iedge_global)
!
      end subroutine const_global_edge_id
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_ele_comm_tbl                                     &
     &         (node, ele, nod_comm, belongs, ele_comm)
!
      use set_ele_id_4_node_type
      use const_element_comm_table
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(belonged_table), intent(inout) :: belongs
      type(communication_table), intent(inout) :: ele_comm
!
      character(len=kchara), parameter :: txt = 'element'
!
!
      call set_ele_id_4_node(node, ele, belongs%blng_ele)
      call belonged_ele_id_4_node(node, ele, belongs%host_ele)
      call const_comm_table_by_connenct                                 &
     &   (txt, ele%numele, ele%nnod_4_ele, ele%ie,                      &
     &    ele%interior_ele, ele%x_ele, node, nod_comm,                  &
     &    belongs%blng_ele, belongs%host_ele, ele_comm)
      call dealloc_iele_belonged(belongs%host_ele)
      call dealloc_iele_belonged(belongs%blng_ele)
!
      end subroutine const_ele_comm_tbl
!
!-----------------------------------------------------------------------
!
      subroutine const_surf_comm_table                                  &
     &         (node, nod_comm, surf, belongs, surf_comm)
!
      use set_ele_id_4_node_type
      use const_element_comm_table
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(communication_table), intent(in) :: nod_comm
      type(belonged_table), intent(inout) :: belongs
      type(communication_table), intent(inout) :: surf_comm
!
      character(len=kchara), parameter :: txt = 'surface'
!
!
      call set_surf_id_4_node(node, surf, belongs%blng_surf)
      call belonged_surf_id_4_node(node, surf, belongs%host_surf)
      call const_comm_table_by_connenct                                 &
     &   (txt, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,            &
     &    surf%interior_surf, surf%x_surf, node, nod_comm,              &
     &    belongs%blng_surf, belongs%host_surf, surf_comm)
      call dealloc_iele_belonged(belongs%host_surf)
      call dealloc_iele_belonged(belongs%blng_surf)
!
      end subroutine const_surf_comm_table
!
!-----------------------------------------------------------------------
!
      subroutine const_edge_comm_table                                  &
     &         (node, nod_comm, edge, belongs, edge_comm)
!
      use set_ele_id_4_node_type
      use const_element_comm_table
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(communication_table), intent(in) :: nod_comm
!
      type(belonged_table), intent(inout) :: belongs
      type(communication_table), intent(inout) :: edge_comm
!
      character(len=kchara), parameter :: txt = 'edge'
!
!
      if(iflag_debug.gt.0) write(*,*) ' set_edge_id_4_node in edge'
      call set_edge_id_4_node(node, edge, belongs%blng_edge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' belonged_edge_id_4_node in edge'
      call belonged_edge_id_4_node(node, edge, belongs%host_edge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' const_comm_table_by_connenct in edge'
      call const_comm_table_by_connenct                                 &
     &    (txt, edge%numedge, edge%nnod_4_edge, edge%ie_edge,           &
     &    edge%interior_edge, edge%x_edge, node, nod_comm,              &
     &    belongs%blng_edge, belongs%host_edge, edge_comm)
      call dealloc_iele_belonged(belongs%host_edge)
      call dealloc_iele_belonged(belongs%blng_edge)
!
      end subroutine const_edge_comm_table
!
!-----------------------------------------------------------------------
!
      end module const_element_comm_tables
