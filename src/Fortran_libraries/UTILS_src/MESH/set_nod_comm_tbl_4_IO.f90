!
!      module set_nod_comm_tbl_4_IO
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine copy_node_comm_tbl_from_IO
!      subroutine copy_node_comm_tbl_to_IO(my_rank)
!
!      subroutine copy_node_import_from_IO
!      subroutine copy_node_import_to_IO
!
      module set_nod_comm_tbl_4_IO
!
      use m_precision
!
      use m_nod_comm_table
      use m_comm_data_IO
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
      subroutine copy_node_comm_tbl_from_IO
!
!
      num_neib = num_neib_domain_IO
!
      call allocate_neib_id
      call allocate_nod_import_num
      call allocate_nod_export_num
!
      call copy_num_communication(num_neib, id_neib,                    &
     &    istack_import, istack_export, ntot_import, ntot_export,       &
     &    id_neib_domain_IO, istack_import_IO, istack_export_IO)
      call copy_num_import_export(num_neib, num_import, num_export,     &
     &    istack_import, istack_export)
!
      call allocate_nod_import_item
      call allocate_nod_export_item
!
      call copy_communication_item(ntot_import, ntot_export,            &
     &    item_import, item_export, item_import_IO, item_export_IO)
!
      call deallocate_comm_item_IO
!
      end subroutine copy_node_comm_tbl_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_node_comm_tbl_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      my_rank_IO = my_rank
      num_neib_domain_IO = num_neib
!
      call allocate_neib_comm_stack_IO
!
      call copy_num_communication(num_neib_domain_IO,                   &
     &    id_neib_domain_IO, istack_import_IO, istack_export_IO,        &
     &    ntot_import_IO, ntot_export_IO, id_neib,                      &
     &    istack_import, istack_export)
!
      call allocate_comm_item_IO
!
      call copy_communication_item(ntot_import_IO, ntot_export_IO,      &
     &    item_import_IO, item_export_IO, item_import, item_export)
!
      end subroutine copy_node_comm_tbl_to_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_node_import_from_IO
!
!
      num_neib = num_neib_domain_IO
!
      call allocate_neib_id
      call allocate_nod_import_num
      call allocate_nod_export_num
!
      call copy_num_import(num_neib, id_neib,                           &
     &    istack_import, istack_export, ntot_import, ntot_export,       &
     &    id_neib_domain_IO, istack_import_IO)
      call copy_num_import_export(num_neib, num_import, num_export,     &
     &    istack_import, istack_export)
!
      call allocate_nod_import_item
      call allocate_nod_export_item
!
      call copy_communication_item(ntot_import, ntot_export,            &
     &    item_import, item_export, item_import_IO, item_export_IO)
!
      call deallocate_comm_item_IO
!
      end subroutine copy_node_import_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_node_import_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      my_rank_IO = my_rank
      num_neib_domain_IO = num_neib
!
      call allocate_neib_comm_stack_IO
!
      call copy_num_import(num_neib_domain_IO, id_neib_domain_IO,       &
     &    istack_import_IO, istack_export_IO,                           &
     &    ntot_import_IO, ntot_export_IO, id_neib, istack_import)
!
      call allocate_comm_item_IO
!
      call copy_communication_item(ntot_import_IO, ntot_export_IO,      &
     &    item_import_IO, item_export_IO, item_import, item_export)
!
      end subroutine copy_node_import_to_IO
!
!-----------------------------------------------------------------------
!
      end module set_nod_comm_tbl_4_IO
