!>@file   m_ele_sf_eg_comm_tables.f90
!!@brief  module m_ele_sf_eg_comm_tables
!!
!!@author H. Matsui
!!@date Programmed in June, 2015
!
!> @brief Belonged element list for each node
!!
!!@verbatim
!!      subroutine dealloc_ele_sf_eg_comm_tables
!!      subroutine const_element_comm_tables_1st
!!@endverbatim
!
      module m_ele_sf_eg_comm_tables
!
      use m_precision
      use t_comm_table
!
      use calypso_mpi
!
      implicit none
!
      type(communication_table), save :: ele_comm
      type(communication_table), save :: surf_comm
      type(communication_table), save :: edge_comm
!
      private :: const_element_comm_table_1st
      private :: const_edge_comm_table_1st
      private :: const_surf_comm_table_1st
      private :: const_ele_comm_table_1st
      private :: const_global_element_id_1st
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_ele_sf_eg_comm_tables
!
      use m_geometry_data
!
!
      call dealloc_numnod_stack(node1)
      call dealloc_numele_stack(ele1)
      call dealloc_numsurf_stack(surf1)
      call dealloc_numedge_stack(edge1)
!
      call deallocate_type_comm_tbl(ele_comm)
      call deallocate_type_comm_tbl(surf_comm)
      call deallocate_type_comm_tbl(edge_comm)
!
      end subroutine dealloc_ele_sf_eg_comm_tables
!
!-----------------------------------------------------------------------
!
      subroutine const_element_comm_tables_1st
!
      use m_geometry_data
      use const_element_comm_tables
!
!
      call const_global_numnod_list(node1)

!
      call const_element_comm_table_1st
      call const_global_element_id_1st
!
      call const_surf_comm_table_1st
      call const_global_surface_id(surf1, surf_comm)
!
      call const_edge_comm_table_1st
      call const_global_edge_id(edge1, edge_comm)
!
      end subroutine const_element_comm_tables_1st
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_global_element_id_1st
!
      use m_geometry_data
      use const_element_comm_tables
      use const_global_element_ids
!
      character(len=kchara), parameter :: txt = 'element'
!
!
      call const_global_numele_list(ele1)
!
      call set_global_ele_id(txt, ele1%numele, ele1%istack_interele,    &
     &   ele1%interior_ele, ele_comm, ele1%iele_global)
!
      end subroutine const_global_element_id_1st
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_element_comm_table_1st
!
      use m_nod_comm_table
      use m_geometry_data
      use m_element_id_4_node
      use m_belonged_element_4_node
!
      character(len=kchara), parameter :: txt = 'element'
!
!
      call set_ele_id_4_node_comm
      call belonged_ele_id_4_node_1(blng_tbls%host_ele)
      call const_ele_comm_table_1st                                     &
     &   (txt, node1%numnod, ele1%numele, node1%inod_global,            &
     &    ele1%interior_ele, ele1%x_ele, nod_comm, ele_4_nod_comm,      &
     &    blng_tbls%host_ele, ele_comm)
      call dealloc_iele_belonged(blng_tbls%host_ele)
      call dealloc_iele_belonged(ele_4_nod_comm)
!
      end subroutine const_element_comm_table_1st
!
!-----------------------------------------------------------------------
!
      subroutine const_surf_comm_table_1st
!
      use m_nod_comm_table
      use m_geometry_data
      use m_element_id_4_node
      use m_belonged_element_4_node
!
      character(len=kchara), parameter :: txt = 'surface'
!
!
      call set_surf_id_4_node
      call belonged_surf_id_4_node_1(blng_tbls%host_surf)
      call const_ele_comm_table_1st                                     &
     &   (txt, node1%numnod, surf1%numsurf, node1%inod_global,          &
     &    surf1%interior_surf, surf1%x_surf, nod_comm, surf_4_nod1,     &
     &    blng_tbls%host_surf, surf_comm)
      call dealloc_iele_belonged(blng_tbls%host_surf)
      call dealloc_iele_belonged(surf_4_nod1)
!
      end subroutine const_surf_comm_table_1st
!
!-----------------------------------------------------------------------
!
      subroutine const_edge_comm_table_1st
!
      use m_nod_comm_table
      use m_geometry_data
      use m_element_id_4_node
      use m_belonged_element_4_node
!
      character(len=kchara), parameter :: txt = 'edge'
!
!
      call set_edge_id_4_node
      call belonged_edge_id_4_node_1(blng_tbls%host_edge)
      call const_ele_comm_table_1st                                     &
     &   (txt, node1%numnod, edge1%numedge, node1%inod_global,          &
     &    edge1%interior_edge, edge1%x_edge, nod_comm, edge_4_nod1,     &
     &    blng_tbls%host_edge,  edge_comm)
      call dealloc_iele_belonged(blng_tbls%host_edge)
      call dealloc_iele_belonged(edge_4_nod1)
!
      end subroutine const_edge_comm_table_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_ele_comm_table_1st                               &
     &         (txt, numnod, numele, inod_global,                       &
     &          internal_flag, x_ele, nod_comm, neib_e, host, e_comm)
!
      use t_comm_table
      use t_next_node_ele_4_node
      use const_element_comm_table
      use const_global_element_ids
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
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
      call count_element_import_num(numnod, host%istack_4_node,         &
     &    nod_comm%num_neib, nod_comm%id_neib,                          &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    e_comm%num_neib, e_comm%id_neib, e_comm%num_import,           &
     &    e_comm%istack_import, e_comm%ntot_import)
!
      call allocate_element_rev_imports(e_comm%ntot_import)
      call allocate_type_import_item(e_comm)
!
!
      call set_element_import_item(numnod, numele, inod_global, x_ele,  &
     &    host%istack_4_node, host%iele_4_node, nod_comm%num_neib,      &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    e_comm%num_neib, e_comm%istack_import, e_comm%item_import)
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
      call set_element_export_item(txt, numnod, numele, inod_global,    &
     &    internal_flag, x_ele, neib_e%istack_4_node,                   &
     &    neib_e%iele_4_node, nod_comm%num_neib,                        &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    e_comm%num_neib, e_comm%istack_export, e_comm%item_export)
!
      call deallocate_element_rev_list
!
      call check_element_position(txt, numele, x_ele, e_comm)
!
      end subroutine const_ele_comm_table_1st
!
!-----------------------------------------------------------------------
!
      end module m_ele_sf_eg_comm_tables
