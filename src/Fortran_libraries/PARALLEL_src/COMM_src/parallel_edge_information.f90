!>@file   parallel_edge_information.f90
!!@brief  module parallel_edge_information
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2020
!
!> @brief Construct mesh strucuture informations
!!
!!@verbatim
!!      subroutine const_para_edge_infos                                &
!!     &         (nod_comm, node, ele, surf, edge)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(inout) :: edge
!!@endverbatim
!
      module parallel_edge_information
!
      use m_precision
      use m_machine_parameter
!
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_para_double_numbering
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_para_edge_infos                                  &
     &         (nod_comm, node, ele, surf, edge)
!
      use set_surf_edge_mesh
      use cal_mesh_position
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(edge_data), intent(inout) :: edge
!
      type(node_ele_double_number) :: inod_dbl
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_node_double_numbering'
      call alloc_double_numbering(node%numnod, inod_dbl)
      call set_node_double_numbering(node, nod_comm,               &
     &                               inod_dbl)
!
      if(iflag_debug .gt. 0) write(*,*) 'const_edge_connectivity'
      call const_edge_connectivity(node, ele, surf,                &
     &    inod_dbl%irank, inod_dbl%index, edge)
      call dealloc_double_numbering(inod_dbl)
!
      if (iflag_debug.gt.0) write(*,*) 'set_center_of_edge'
      call alloc_edge_geometory(edge)
      call set_center_of_edge(node, edge)
!
      end subroutine const_para_edge_infos
!
! ----------------------------------------------------------------------
!
      end module parallel_edge_information
