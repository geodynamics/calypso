!>@file   domain_data_IO_b.f90
!!@brief  module domain_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for doimain data IO
!!
!!@verbatim
!!      subroutine read_domain_info_b(id_rank, bflag, comm_IO)
!!      subroutine read_import_data_b(bflag, comm_IO)
!!      subroutine read_export_data_b(bflag, comm_IO)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine write_domain_info_b(id_rank, comm_IO, bflag)
!!      subroutine write_import_data_b(comm_IO, bflag)
!!      subroutine write_export_data_b(comm_IO, bflag)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(communicatiole), intent(in) :: comm_IO
!!@endverbatim
!!
!@param id_file file ID
!
      module domain_data_IO_b
!
      use m_precision
!
      use t_comm_table
      use binary_IO
      use transfer_to_long_integers
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine read_domain_info_b(id_rank, bflag, comm_IO)
!
      use m_error_IDs
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: irank_read
!
!
      bflag%ierr_IO = 0
      call read_one_integer_b(bflag, irank_read)
      if(bflag%ierr_IO .ne. 0) return
!
       if(int(irank_read) .ne. id_rank) then
         bflag%ierr_IO = ierr_mesh
         return
       end if
!
      call read_one_integer_b(bflag, comm_IO%num_neib)
      if(bflag%ierr_IO .ne. 0) return
!
      call alloc_neighbouring_id(comm_IO)
!
      call read_mul_integer_b                                           &
     &   (bflag, cast_long(comm_IO%num_neib), comm_IO%id_neib)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine read_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_import_data_b(bflag, comm_IO)
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(communication_table), intent(inout) :: comm_IO
!
!
      call alloc_import_num(comm_IO)
      if (comm_IO%num_neib .gt. 0) then
!
        call read_integer_stack_b(bflag, cast_long(comm_IO%num_neib),   &
     &      comm_IO%istack_import, comm_IO%ntot_import)
        if(bflag%ierr_IO .ne. 0) return
!
        call alloc_import_item(comm_IO)
        call read_mul_integer_b                                         &
     &     (bflag, cast_long(comm_IO%ntot_import), comm_IO%item_import)
        if(bflag%ierr_IO .ne. 0) return
      else
        comm_IO%ntot_import = 0
        call alloc_import_item(comm_IO)
      end if
!
      end subroutine read_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine read_export_data_b(bflag, comm_IO)
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(communication_table), intent(inout) :: comm_IO
!
!
      call alloc_export_num(comm_IO)
      if (comm_IO%num_neib .gt. 0) then
        call read_integer_stack_b(bflag, cast_long(comm_IO%num_neib),   &
     &      comm_IO%istack_export, comm_IO%ntot_export)
        if(bflag%ierr_IO .ne. 0) return
!
        call alloc_export_item(comm_IO)
        call read_mul_integer_b                                         &
     &     (bflag, cast_long(comm_IO%ntot_export), comm_IO%item_export)
        if(bflag%ierr_IO .ne. 0) return
      else
        comm_IO%ntot_export = 0
        call alloc_export_item(comm_IO)
      end if
!
      end subroutine read_export_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_domain_info_b(id_rank, comm_IO, bflag)
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: irank_write
!
!
      irank_write = int(id_rank,KIND(irank_write))
      call write_one_integer_b(irank_write, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_one_integer_b(comm_IO%num_neib, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call write_mul_integer_b                                          &
     &   (cast_long(comm_IO%num_neib), comm_IO%id_neib, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
     subroutine write_import_data_b(comm_IO, bflag)
!
     type(communication_table), intent(in) :: comm_IO
     type(binary_IO_flags), intent(inout) :: bflag
!
!
     call write_integer_stack_b                                         &
    &   (cast_long(comm_IO%num_neib), comm_IO%istack_import, bflag)
     if(bflag%ierr_IO .ne. 0) return
!
     call write_mul_integer_b                                           &
    &   (cast_long(comm_IO%ntot_import), comm_IO%item_import, bflag)
     if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine write_export_data_b(comm_IO, bflag)
!
      type(communication_table), intent(in) :: comm_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call write_integer_stack_b                                        &
     &   (cast_long(comm_IO%num_neib), comm_IO%istack_export, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call write_mul_integer_b                                          &
     &   (cast_long(comm_IO%ntot_export), comm_IO%item_export, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_export_data_b
!
! -----------------------------------------------------------------------!
      end module domain_data_IO_b
