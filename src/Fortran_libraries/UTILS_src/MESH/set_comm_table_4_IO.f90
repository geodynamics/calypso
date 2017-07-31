!>@file   set_comm_table_4_IO.f90
!!@brief  module set_comm_table_4_IO
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!> @brief Copy node communication table between IO buffer
!!
!!@verbatim
!!      subroutine copy_comm_tbl_type(comm_org, comm_new)
!!        integer(kind = kint), intent(in) :: my_rank
!!        type(communication_table), intent(in) :: comm_tbls
!!@endverbatim
!
      module set_comm_table_4_IO
!
      use m_precision
!
      use t_comm_table
      use copy_communication_table
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_comm_tbl_type(comm_org, comm_new)
!
      type(communication_table), intent(in) :: comm_org
      type(communication_table), intent(inout) :: comm_new
!
!
      comm_new%num_neib = comm_org%num_neib
!
      call allocate_type_comm_tbl_num(comm_new)
!
      call copy_num_communication                                       &
     &   (comm_new%num_neib, comm_new%id_neib,                          &
     &    comm_new%istack_import, comm_new%istack_export,               &
     &    comm_new%ntot_import, comm_new%ntot_export, comm_org%id_neib, &
     &    comm_org%istack_import, comm_org%istack_export)
!
      call allocate_type_comm_tbl_item(comm_new)
!
      call copy_communication_item                                      &
     &   (comm_new%ntot_import, comm_new%ntot_export,                   &
     &    comm_new%item_import, comm_new%item_export,                   &
     &    comm_org%item_import, comm_org%item_export)
!
      end subroutine copy_comm_tbl_type
!
!-----------------------------------------------------------------------
!
      end module set_comm_table_4_IO
