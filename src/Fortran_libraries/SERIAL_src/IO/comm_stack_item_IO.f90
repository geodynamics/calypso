!
!      module comm_stack_item_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_import_data(id_file)
!      subroutine read_export_data(id_file)
!      subroutine write_import_data(id_file)
!      subroutine write_export_data(id_file)
!
!      subroutine read_import_data_b(id_file)
!      subroutine read_export_data_b(id_file)
!      subroutine write_import_data_b(id_file)
!      subroutine write_export_data_b(id_file)
!
      module comm_stack_item_IO
!
      use m_precision
!
      use m_constants
      use m_comm_data_IO
      use stack_array_IO
      use comm_table_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_import_data(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      call allocate_import_stack_IO
!
      if (num_neib_domain_IO .gt. 0) then
!
        call read_arrays_for_stacks(id_file, num_neib_domain_IO,        &
     &      izero, ntot_import_IO, istack_import_IO)
!
        call allocate_import_item_IO
        call read_send_recv_item(id_file, ntot_import_IO,               &
     &      item_import_IO)
      else
        ntot_import_IO = 0
        call allocate_import_item_IO
      end if
!
      end subroutine read_import_data
!
! -----------------------------------------------------------------------
!
      subroutine read_export_data(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      call allocate_export_stack_IO
!
      if (num_neib_domain_IO .gt. 0) then
!
        call read_arrays_for_stacks(id_file, num_neib_domain_IO,        &
     &      izero, ntot_export_IO, istack_export_IO)
        call allocate_export_item_IO
        call read_send_recv_item(id_file, ntot_export_IO,               &
     &      item_export_IO)
      else
        ntot_export_IO = 0
        call allocate_export_item_IO
      end if
!
      end subroutine read_export_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_import_data(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      call write_send_recv_data(id_file, num_neib_domain_IO,            &
     &    ntot_import_IO, istack_import_IO, item_import_IO)
!
      call deallocate_import_item_IO
!
      end subroutine write_import_data
!
! -----------------------------------------------------------------------
!
      subroutine write_export_data(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      call write_send_recv_data(id_file, num_neib_domain_IO,            &
     &    ntot_export_IO, istack_export_IO, item_export_IO)
!
      call deallocate_export_item_IO
!
      end subroutine write_export_data
!
! -----------------------------------------------------------------------! -----------------------------------------------------------------------
!
      subroutine read_import_data_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      call allocate_import_stack_IO
      if (num_neib_domain_IO .gt. 0) then
!
        call read_arrays_for_stacks_b(id_file, num_neib_domain_IO,      &
     &    izero, ntot_import_IO, istack_import_IO)
!
        call allocate_import_item_IO
        call read_send_recv_item_b(id_file, ntot_import_IO,             &
     &      item_import_IO)
      else
        ntot_import_IO = 0
        call allocate_import_item_IO
      end if
!
      end subroutine read_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine read_export_data_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      call allocate_export_stack_IO
!
      if (num_neib_domain_IO .gt. 0) then
!
        call read_arrays_for_stacks_b(id_file, num_neib_domain_IO,      &
     &    izero, ntot_export_IO, istack_export_IO)
!
        call allocate_export_item_IO
        call read_send_recv_item_b(id_file, ntot_export_IO,             &
     &      item_export_IO)
      else
        ntot_export_IO = 0
        call allocate_export_item_IO
      end if
!
      end subroutine read_export_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_import_data_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      call write_send_recv_data_b(id_file, num_neib_domain_IO,          &
     &    ntot_import_IO, istack_import_IO, item_import_IO)
!
      call deallocate_import_item_IO
!
      end subroutine write_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine write_export_data_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      call write_send_recv_data_b(id_file, num_neib_domain_IO,          &
     &    ntot_export_IO, istack_export_IO, item_export_IO)
!
      call deallocate_export_item_IO
!
      end subroutine write_export_data_b
!
! -----------------------------------------------------------------------! -----------------------------------------------------------------------!
      end module comm_stack_item_IO
