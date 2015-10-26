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
      use m_nod_comm_table
      use m_geometry_data
      use m_element_id_4_node
      use m_belonged_element_4_node
      use set_ele_id_4_node_type
      use const_element_comm_tables
!
!
      call const_global_numnod_list(node1)
!
      call const_ele_comm_tbl                                           &
     &   (node1, ele1, nod_comm, blng_tbls, ele_comm)
      call const_global_element_id(ele1, ele_comm)
!
      call const_surf_comm_table                                       &
     &   (node1, nod_comm, surf1, blng_tbls, surf_comm)
      call const_global_surface_id(surf1, surf_comm)
!
      call const_edge_comm_table                                        &
     &   (node1, nod_comm, edge1, blng_tbls, edge_comm)
      call const_global_edge_id(edge1, edge_comm)
!
      end subroutine const_element_comm_tables_1st
!
!-----------------------------------------------------------------------
!
      end module m_ele_sf_eg_comm_tables
