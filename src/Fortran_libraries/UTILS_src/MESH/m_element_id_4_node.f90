!>@file   m_element_id_4_node.f90
!!@brief  module m_element_id_4_node
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2006
!
!> @brief included element list for each node
!!
      module m_element_id_4_node
!
      use m_precision
      use m_constants
      use t_next_node_ele_4_node
!
      implicit none
!
!
!>   Structure of included element list for each node
      type(element_around_node), save :: ele_4_nod1
!
!>   Structure of neighbouring node list for each node
      type(next_nod_id_4_nod), save :: neib_nod1
!
      end module m_element_id_4_node
