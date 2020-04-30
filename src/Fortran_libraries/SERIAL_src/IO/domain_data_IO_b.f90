!>@file   domain_data_IO_b.f90
!!@brief  module domain_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for doimain data IO
!!
!!@verbatim
!!      subroutine read_domain_info_b(id_rank, bbuf, comm_IO)
!!      subroutine read_import_data_b(bbuf, comm_IO)
!!      subroutine read_export_data_b(bbuf, comm_IO)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine write_domain_info_b(id_rank, comm_IO, bbuf)
!!      subroutine write_import_data_b(comm_IO, bbuf)
!!      subroutine write_export_data_b(comm_IO, bbuf)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
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
      use t_binary_IO_buffer
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
      subroutine read_domain_info_b(id_rank, bbuf, comm_IO)
!
      use m_error_IDs
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: irank_read
!
!
      call read_one_integer_b(bbuf, irank_read)
      if(bbuf%ierr_bin .gt. 0) return
!
     if(int(irank_read) .ne. id_rank) then
       bbuf%ierr_bin = ierr_mesh
       return
     end if
!
      call read_one_integer_b(bbuf, comm_IO%num_neib)
      if(bbuf%ierr_bin .gt. 0) return
!
      call alloc_neighbouring_id(comm_IO)
!
      call read_mul_integer_b                                           &
     &   (bbuf, cast_long(comm_IO%num_neib), comm_IO%id_neib)
      if(bbuf%ierr_bin .gt. 0) return
!
      end subroutine read_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_import_data_b(bbuf, comm_IO)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(communication_table), intent(inout) :: comm_IO
!
!
      call alloc_import_num(comm_IO)
      if (comm_IO%num_neib .gt. 0) then
        call read_integer_stack_b(bbuf, cast_long(comm_IO%num_neib),    &
     &      comm_IO%istack_import, comm_IO%ntot_import)
        if(bbuf%ierr_bin .gt. 0) return
!
        call alloc_import_item(comm_IO)
        call read_mul_integer_b                                         &
     &     (bbuf, cast_long(comm_IO%ntot_import), comm_IO%item_import)
        if(bbuf%ierr_bin .gt. 0) return
      else
        comm_IO%ntot_import = 0
        call alloc_import_item(comm_IO)
      end if
!
      end subroutine read_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine read_export_data_b(bbuf, comm_IO)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(communication_table), intent(inout) :: comm_IO
!
!
      call alloc_export_num(comm_IO)
      if (comm_IO%num_neib .gt. 0) then
        call read_integer_stack_b(bbuf, cast_long(comm_IO%num_neib),    &
     &      comm_IO%istack_export, comm_IO%ntot_export)
        if(bbuf%ierr_bin .gt. 0) return
!
        call alloc_export_item(comm_IO)
        call read_mul_integer_b                                         &
     &     (bbuf, cast_long(comm_IO%ntot_export), comm_IO%item_export)
        if(bbuf%ierr_bin .gt. 0) return
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
      subroutine write_domain_info_b(id_rank, comm_IO, bbuf)
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint) :: irank_write
!
!
      irank_write = int(id_rank,KIND(irank_write))
      call write_one_integer_b(irank_write, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_one_integer_b(comm_IO%num_neib, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      call write_mul_integer_b                                          &
     &   (cast_long(comm_IO%num_neib), comm_IO%id_neib, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      end subroutine write_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
     subroutine write_import_data_b(comm_IO, bbuf)
!
     type(communication_table), intent(in) :: comm_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
     call write_integer_stack_b                                         &
    &   (cast_long(comm_IO%num_neib), comm_IO%istack_import, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
     call write_mul_integer_b                                           &
    &   (cast_long(comm_IO%ntot_import), comm_IO%item_import, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      end subroutine write_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine write_export_data_b(comm_IO, bbuf)
!
      type(communication_table), intent(in) :: comm_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_integer_stack_b                                        &
     &   (cast_long(comm_IO%num_neib), comm_IO%istack_export, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      call write_mul_integer_b                                          &
     &   (cast_long(comm_IO%ntot_export), comm_IO%item_export, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      end subroutine write_export_data_b
!
! -----------------------------------------------------------------------!
      end module domain_data_IO_b
