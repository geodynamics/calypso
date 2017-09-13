!>@file   domain_data_IO.f90
!!@brief  module domain_data_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Routine for doimain data IO
!!
!!@verbatim
!!      subroutine read_domain_info(id_file, my_rank_IO, comm_IO, ierr)
!!      subroutine read_import_data(id_file, comm_IO)
!!      subroutine read_export_data(id_file, comm_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine write_domain_info(id_file, my_rank_IO, comm_IO)
!!      subroutine write_import_data(id_file, comm_IO)
!!      subroutine write_export_data(id_file, comm_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!@endverbatim
!!
!@param id_file file ID
!
      module domain_data_IO
!
      use m_precision
      use m_constants
!
      use t_comm_table
      use field_data_IO
      use comm_table_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine read_domain_info(id_file, my_rank_IO, comm_IO, ierr)
!
      use m_error_IDs
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: irank_read
      character(len=255) :: character_4_read = ''
!
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) irank_read
!
      ierr = 0
      if(irank_read .ne. my_rank_IO) then
        ierr = ierr_mesh
        return
      end if
!
      read(id_file,*) comm_IO%num_neib
!
      call allocate_type_neib_id(comm_IO)
!
      if (comm_IO%num_neib .gt. 0) then
        read(id_file,*) comm_IO%id_neib(1:comm_IO%num_neib)
      end if
!
      end subroutine read_domain_info
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_import_data(id_file, comm_IO)
!
      integer(kind = kint), intent(in) :: id_file
      type(communication_table), intent(inout) :: comm_IO
!
!
      call allocate_type_import_num(comm_IO)
!
      if (comm_IO%num_neib .gt. 0) then
!
        call read_arrays_for_stacks(id_file, comm_IO%num_neib,          &
     &      izero, comm_IO%ntot_import, comm_IO%istack_import)
!
        call allocate_type_import_item(comm_IO)
        call read_send_recv_item(id_file, comm_IO%ntot_import,          &
     &      comm_IO%item_import)
      else
        comm_IO%ntot_import = 0
        call allocate_type_import_item(comm_IO)
      end if
!
      end subroutine read_import_data
!
! -----------------------------------------------------------------------
!
      subroutine read_export_data(id_file, comm_IO)
!
      integer(kind = kint), intent(in) :: id_file
      type(communication_table), intent(inout) :: comm_IO
!
!
      call allocate_type_export_num(comm_IO)
!
      if (comm_IO%num_neib .gt. 0) then
!
        call read_arrays_for_stacks(id_file, comm_IO%num_neib,          &
     &      izero, comm_IO%ntot_export, comm_IO%istack_export)
        call allocate_type_export_item(comm_IO)
        call read_send_recv_item(id_file, comm_IO%ntot_export,          &
     &      comm_IO%item_export)
      else
        comm_IO%ntot_export = 0
        call allocate_type_export_item(comm_IO)
      end if
!
      end subroutine read_export_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_domain_info(id_file, my_rank_IO, comm_IO)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
!
!
!      write(id_file,'(a)') '! '
!      write(id_file,'(a)') '! 1.parallel information'
!      write(id_file,'(a)') '!    domain ID'
!      write(id_file,'(a)') '!    number of domain for transfer'
!      write(id_file,'(a)') '!    domain ID for transfer'
!      write(id_file,'(a)') '! '
!
      write(id_file,'(i16)') my_rank_IO
      write(id_file,'(i16)') comm_IO%num_neib
!
      if (comm_IO%num_neib .gt. 0) then
        write(id_file,'(8i16)') comm_IO%id_neib(1:comm_IO%num_neib)
      else
        write(id_file,'(a)') ''
      end if
!
      call deallocate_type_neib_id(comm_IO)
!
      end subroutine write_domain_info
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_import_data(id_file, comm_IO)
!
      integer(kind = kint), intent(in) :: id_file
      type(communication_table), intent(inout) :: comm_IO
!
!
      call write_send_recv_data                                         &
     &   (id_file, comm_IO%num_neib, comm_IO%ntot_import,               &
     &    comm_IO%istack_import, comm_IO%item_import)
!
      call deallocate_type_import(comm_IO)
!
      end subroutine write_import_data
!
! -----------------------------------------------------------------------
!
      subroutine write_export_data(id_file, comm_IO)
!
      integer(kind = kint), intent(in) :: id_file
      type(communication_table), intent(inout) :: comm_IO
!
!
      call write_send_recv_data                                         &
     &   (id_file, comm_IO%num_neib, comm_IO%ntot_export,               &
     &    comm_IO%istack_export, comm_IO%item_export)
!
      call deallocate_type_export(comm_IO)
!
      end subroutine write_export_data
!
! -----------------------------------------------------------------------!
      end module domain_data_IO
