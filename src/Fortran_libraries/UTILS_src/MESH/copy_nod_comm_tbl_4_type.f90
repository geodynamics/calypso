!copy_nod_comm_tbl_4_type.f90
!      module copy_nod_comm_tbl_4_type
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine copy_node_comm_tbl_from_type(nod_comm)
!      subroutine copy_node_comm_tbl_to_type(nod_comm)
!
!      subroutine copy_node_import_from_type(nod_comm)
!      subroutine copy_node_import_to_type(nod_comm)
!        type(communication_table), intent(inout) :: nod_comm
!
      module copy_nod_comm_tbl_4_type
!
      use m_precision
!
      use m_nod_comm_table
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
      subroutine copy_node_comm_tbl_from_type(nod_comm)
!
      type(communication_table), intent(in) :: nod_comm
!
!
      num_neib = nod_comm%num_neib
!
      call allocate_neib_id
      call allocate_nod_import_num
      call allocate_nod_export_num
!
      call copy_num_communication(num_neib, id_neib,                    &
     &    istack_import, istack_export, ntot_import, ntot_export,       &
     &    nod_comm%id_neib, nod_comm%istack_import,                     &
     &    nod_comm%istack_export)
      call copy_num_import_export(num_neib, num_import, num_export,     &
     &    istack_import, istack_export)
!
      call allocate_nod_import_item
      call allocate_nod_export_item
!
      call copy_communication_item(ntot_import, ntot_export,            &
     &    item_import, item_export, nod_comm%item_import,               &
     &    nod_comm%item_export)
!
      end subroutine copy_node_comm_tbl_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_node_comm_tbl_to_type(nod_comm)
!
      type(communication_table), intent(inout) :: nod_comm
!
!
      nod_comm%num_neib = num_neib
!
      call allocate_type_comm_tbl_num(nod_comm)
!
      call copy_num_communication(nod_comm%num_neib,                   &
     &    nod_comm%id_neib, nod_comm%istack_import,                    &
     &    nod_comm%istack_export, nod_comm%ntot_import,                &
     &    nod_comm%ntot_export, id_neib, istack_import, istack_export)
      call copy_num_import_export(nod_comm%num_neib,                   &
     &    nod_comm%num_import, nod_comm%num_export,                    &
     &    nod_comm%istack_import, nod_comm%istack_export)
!
      call allocate_type_comm_tbl_item(nod_comm)
!
      call copy_communication_item(nod_comm%ntot_import,               &
     &    nod_comm%ntot_export, nod_comm%item_import,                  &
     &    nod_comm%item_export, item_import, item_export)
!
      end subroutine copy_node_comm_tbl_to_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_node_import_from_type(nod_comm)
!
      type(communication_table), intent(in) :: nod_comm
!
!
      num_neib = nod_comm%num_neib
!
      call allocate_neib_id
      call allocate_nod_import_num
      call allocate_nod_export_num
!
      call copy_num_import(num_neib, id_neib,                           &
     &    istack_import, istack_export, ntot_import, ntot_export,       &
     &    nod_comm%id_neib, nod_comm%istack_import)
      call copy_num_import_export(num_neib, num_import, num_export,     &
     &    istack_import, istack_export)
!
      call allocate_nod_import_item
      call allocate_nod_export_item
!
      call copy_communication_item(ntot_import, ntot_export,            &
     &    item_import, item_export, nod_comm%item_import,               &
     &    nod_comm%item_export)
!
      end subroutine copy_node_import_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_node_import_to_type(nod_comm)
!
      type(communication_table), intent(inout) :: nod_comm
!
!
      nod_comm%num_neib = num_neib
!
      call allocate_type_comm_tbl_num(nod_comm)
!
      call copy_num_import(nod_comm%num_neib, nod_comm%id_neib,        &
     &    nod_comm%istack_import, nod_comm%istack_export,              &
     &    nod_comm%ntot_import, nod_comm%ntot_export, id_neib,         &
     &    istack_import)
      call copy_num_import_export(nod_comm%num_neib,                   &
     &    nod_comm%num_import, nod_comm%num_export,                    &
     &    nod_comm%istack_import, nod_comm%istack_export)
!
      call allocate_type_comm_tbl_item(nod_comm)
!
      call copy_communication_item(nod_comm%ntot_import,               &
     &    nod_comm%ntot_export, nod_comm%item_import,                  &
     &    nod_comm%item_export, item_import, item_export)
!
      end subroutine copy_node_import_to_type
!
!-----------------------------------------------------------------------
!
      end module copy_nod_comm_tbl_4_type
