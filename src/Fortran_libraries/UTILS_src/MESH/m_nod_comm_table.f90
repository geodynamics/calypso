!>@file   m_nod_comm_table.f90
!!@brief  module m_nod_comm_table
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n     Modified in 2006 
!
!> @brief Communication table for node
!!
!!@verbatim
!!      subroutine copy_node_comm_tbl_from_type(org_comm)
!!      subroutine copy_node_comm_tbl_to_type(new_comm)
!!        type(communication_table), intent(in) :: org_comm
!!        type(communication_table), intent(inout) :: new_comm
!!@endverbatim
!
      module m_nod_comm_table
!
      use m_precision
      use t_comm_table
!
      implicit  none
!
!> data structure for node communication table
      type(communication_table), save :: nod_comm
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine copy_node_comm_tbl_from_type(org_comm)
!
      type(communication_table), intent(in) :: org_comm
!
!
      call copy_comm_tbl_types(org_comm, nod_comm)
!
      end subroutine copy_node_comm_tbl_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_node_comm_tbl_to_type(new_comm)
!
      type(communication_table), intent(inout) :: new_comm
!
!
      call copy_comm_tbl_types(nod_comm, new_comm)
!
      end subroutine copy_node_comm_tbl_to_type
!
!-----------------------------------------------------------------------
!
      end module m_nod_comm_table
