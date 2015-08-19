!m_next_node_id_4_node.f90
!      module m_next_node_id_4_node
!
!> @brief Neighbouring node list for each node
!
!      Written by H.Matsui on Oct., 2006
!
!      subroutine const_next_nod_id_4_node
!
      module m_next_node_id_4_node
!
      use m_precision
      use m_constants
      use t_next_node_ele_4_node
!
      implicit none
!
!>   Structure of neighbouring node list for each node
      type(next_nod_id_4_nod), save :: neib_nod1
!neib_nod1%istack_next
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_next_nod_id_4_node
!
      use m_machine_parameter
      use m_geometry_data
      use m_element_id_4_node
!
      use set_ele_id_4_node_type
!
!
      call const_next_nod_id_4_node_type(node1, ele1, ele_4_nod1,       &
     &    neib_nod1)
!
      end subroutine const_next_nod_id_4_node
!
!-----------------------------------------------------------------------
!
      end module m_next_node_id_4_node
