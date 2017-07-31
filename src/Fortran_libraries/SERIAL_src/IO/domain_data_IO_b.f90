!>@file   domain_data_IO_b.f90
!!@brief  module domain_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for doimain data IO
!!
!!@verbatim
!!      subroutine read_domain_info_b(my_rank_IO, comm_IO, ierr)
!!      subroutine read_import_data_b(comm_IO)
!!      subroutine read_export_data_b(comm_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine write_domain_info_b(my_rank_IO, comm_IO)
!!      subroutine write_import_data_b(comm_IO)
!!      subroutine write_export_data_b(comm_IO)
!!        type(communication_table), intent(inout) :: comm_IO
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
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine read_domain_info_b(my_rank_IO, comm_IO, ierr)
!
      use m_error_IDs
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: irank_read
!
!
      call read_one_integer_b(irank_read)
       ierr = 0
       if(irank_read .ne. my_rank_IO) then
         ierr = ierr_mesh
         return
       end if
!
      call read_one_integer_b(comm_IO%num_neib)
!
      call allocate_type_neib_id(comm_IO)
!
      call read_mul_integer_b(comm_IO%num_neib, comm_IO%id_neib)
!
      end subroutine read_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_import_data_b(comm_IO)
!
      type(communication_table), intent(inout) :: comm_IO
!
!
      call allocate_type_import_num(comm_IO)
      if (comm_IO%num_neib .gt. 0) then
!
        call read_integer_stack_b(comm_IO%num_neib,                     &
     &      comm_IO%istack_import, comm_IO%ntot_import)
!
        call allocate_type_import_item(comm_IO)
        call read_mul_integer_b                                         &
     &     (comm_IO%ntot_import, comm_IO%item_import)
      else
        comm_IO%ntot_import = 0
        call allocate_type_import_item(comm_IO)
      end if
!
      end subroutine read_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine read_export_data_b(comm_IO)
!
      type(communication_table), intent(inout) :: comm_IO
!
!
      call allocate_type_export_num(comm_IO)
      if (comm_IO%num_neib .gt. 0) then
        call read_integer_stack_b(comm_IO%num_neib,                     &
     &      comm_IO%istack_export, comm_IO%ntot_export)
!
        call allocate_type_export_item(comm_IO)
        call read_mul_integer_b                                         &
     &     (comm_IO%ntot_export, comm_IO%item_export)
      else
        comm_IO%ntot_export = 0
        call allocate_type_export_item(comm_IO)
      end if
!
      end subroutine read_export_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_domain_info_b(my_rank_IO, comm_IO)
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
!
!
      call write_one_integer_b(my_rank_IO)
      call write_one_integer_b(comm_IO%num_neib)
!
      call write_mul_integer_b(comm_IO%num_neib, comm_IO%id_neib)
!
      call deallocate_type_neib_id(comm_IO)
!
      end subroutine write_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_import_data_b(comm_IO)
!
      type(communication_table), intent(inout) :: comm_IO
!
!
      call write_integer_stack_b                                        &
    &    (comm_IO%num_neib, comm_IO%istack_import)
      call write_mul_integer_b                                          &
    &    (comm_IO%ntot_import, comm_IO%item_import)
!
      call deallocate_type_import(comm_IO)
!
      end subroutine write_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine write_export_data_b(comm_IO)
!
      type(communication_table), intent(inout) :: comm_IO
!
!
      call write_integer_stack_b                                        &
     &   (comm_IO%num_neib, comm_IO%istack_export)
      call write_mul_integer_b                                          &
     &   (comm_IO%ntot_export, comm_IO%item_export)
!
      call deallocate_type_export(comm_IO)
!
      end subroutine write_export_data_b
!
! -----------------------------------------------------------------------!
      end module domain_data_IO_b
