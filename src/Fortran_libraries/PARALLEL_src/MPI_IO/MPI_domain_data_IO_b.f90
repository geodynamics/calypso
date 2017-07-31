!>@file   MPI_domain_data_IO_b.f90
!!@brief  module MPI_domain_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for gzipped binary doimain data IO
!!
!!@verbatim
!!      subroutine mpi_read_domain_info_b(IO_param, comm_IO)
!!      subroutine mpi_read_import_data_b(IO_param, comm_IO)
!!      subroutine mpi_read_export_data_b(IO_param, comm_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine mpi_write_domain_info_b(IO_param, comm_IO)
!!      subroutine mpi_write_import_data_b(IO_param, comm_IO)
!!      subroutine mpi_write_export_data_b(IO_param, comm_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!@endverbatim
!
      module MPI_domain_data_IO_b
!
      use m_precision
      use m_constants
!
      use t_comm_table
      use t_calypso_mpi_IO_param
      use MPI_binary_head_IO
      use MPI_binary_data_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine mpi_read_domain_info_b(IO_param, comm_IO)
!
      use m_error_IDs
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: nprocs_read
!
!
      call mpi_read_one_inthead_b(IO_param, nprocs_read)
      if(nprocs_read .ne. IO_param%nprocs_in) then
        call calypso_mpi_abort(ierr_file, '#. of subdmain is wrong')
      end if
!
      call mpi_read_one_integer_b(IO_param, comm_IO%num_neib)
!
      call allocate_type_neib_id(comm_IO)
!
      call mpi_read_int_vector_b                                        &
     &   (IO_param, comm_IO%num_neib, comm_IO%id_neib)
!
      end subroutine mpi_read_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_import_data_b(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call allocate_type_import_num(comm_IO)
!
      call mpi_read_integer_stack_b(IO_param, comm_IO%num_neib,         &
     &    comm_IO%istack_import, comm_IO%ntot_import)
!
      call allocate_type_import_item(comm_IO)
!
      call mpi_read_int_vector_b                                        &
     &     (IO_param, comm_IO%ntot_import, comm_IO%item_import)
!
      end subroutine mpi_read_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_export_data_b(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call allocate_type_export_num(comm_IO)
!
      call mpi_read_integer_stack_b(IO_param, comm_IO%num_neib,         &
     &      comm_IO%istack_export, comm_IO%ntot_export)
!
      call allocate_type_export_item(comm_IO)
!
      call mpi_read_int_vector_b                                        &
     &     (IO_param, comm_IO%ntot_export, comm_IO%item_export)
!
      end subroutine mpi_read_export_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_domain_info_b(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call mpi_write_one_inthead_b(IO_param, IO_param%nprocs_in)
      call mpi_write_one_integer_b(IO_param, comm_IO%num_neib)
!
      call set_istack_4_parallell_data(comm_IO%num_neib, IO_param)
!
      call mpi_write_int_vector_b                                       &
     &   (IO_param, comm_IO%num_neib, comm_IO%id_neib)
!
      call deallocate_type_neib_id(comm_IO)
!
      end subroutine mpi_write_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_import_data_b(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call set_istack_4_parallell_data(comm_IO%num_neib, IO_param)
      call mpi_write_integer_stack_b                                    &
     &   (IO_param, comm_IO%num_neib, comm_IO%istack_import)
!
      call set_istack_4_parallell_data(comm_IO%ntot_import, IO_param)
      call mpi_write_int_vector_b                                       &
     &   (IO_param, comm_IO%ntot_import, comm_IO%item_import)
!
      call deallocate_type_import(comm_IO)
!
      end subroutine mpi_write_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_export_data_b(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call set_istack_4_parallell_data(comm_IO%num_neib, IO_param)
      call mpi_write_integer_stack_b                                    &
     &   (IO_param, comm_IO%num_neib, comm_IO%istack_export)
!
      call set_istack_4_parallell_data(comm_IO%ntot_export, IO_param)
      call mpi_write_int_vector_b                                       &
     &   (IO_param, comm_IO%ntot_export, comm_IO%item_export)
!
      call deallocate_type_export(comm_IO)
!
      end subroutine mpi_write_export_data_b
!
! -----------------------------------------------------------------------!
      end module MPI_domain_data_IO_b
