!>@file   gz_MPI_domain_data_IO_b.f90
!!@brief  module gz_MPI_domain_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for gzipped binary doimain data IO
!!
!!@verbatim
!!      subroutine gz_mpi_read_domain_info_b(IO_param, comm_IO)
!!      subroutine gz_mpi_read_import_data_b(IO_param, comm_IO)
!!      subroutine gz_mpi_read_export_data_b(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine gz_mpi_write_domain_info_b(IO_param, comm_IO)
!!      subroutine gz_mpi_write_import_data_b(IO_param, comm_IO)
!!      subroutine gz_mpi_write_export_data_b(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(in) :: comm_IO
!!@endverbatim
!
      module gz_MPI_domain_data_IO_b
!
      use m_precision
      use m_constants
!
      use t_comm_table
      use t_calypso_mpi_IO_param
      use gz_MPI_binary_datum_IO
      use gz_MPI_binary_data_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_domain_info_b(IO_param, comm_IO)
!
      use m_error_IDs
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call gz_mpi_read_process_id_b(IO_param)
      call gz_mpi_read_one_integer_b(IO_param, comm_IO%num_neib)
!
      call alloc_neighbouring_id(comm_IO)
      num64 = comm_IO%num_neib
      call gz_mpi_read_int_vector_b                                     &
     &   (IO_param, num64, comm_IO%id_neib)
!
      end subroutine gz_mpi_read_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_import_data_b(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call alloc_import_num(comm_IO)
      num64 = comm_IO%num_neib
      call gz_mpi_read_integer_stack_b(IO_param, num64,                 &
     &    comm_IO%istack_import, comm_IO%ntot_import)
!
      call alloc_import_item(comm_IO)
      num64 = comm_IO%ntot_import
      call gz_mpi_read_int_vector_b                                     &
     &   (IO_param, num64, comm_IO%item_import)
!
      end subroutine gz_mpi_read_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_export_data_b(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call alloc_export_num(comm_IO)
      num64 = comm_IO%num_neib
      call gz_mpi_read_integer_stack_b(IO_param, num64,                 &
     &    comm_IO%istack_export, comm_IO%ntot_export)
!
      call alloc_export_item(comm_IO)
      num64 = comm_IO%ntot_export
      call gz_mpi_read_int_vector_b                                     &
     &   (IO_param, num64, comm_IO%item_export)
!
      end subroutine gz_mpi_read_export_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_domain_info_b(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: comm_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call gz_mpi_write_process_id_b(IO_param)
      call gz_mpi_write_one_integer_b(IO_param, comm_IO%num_neib)
!
      num64 = comm_IO%num_neib
      call gz_mpi_write_int_vector_b                                    &
     &   (IO_param, num64, comm_IO%id_neib)
!
      end subroutine gz_mpi_write_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_import_data_b(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: comm_IO
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = comm_IO%num_neib
      call gz_mpi_write_integer_stack_b                                 &
     &   (IO_param, num64, comm_IO%istack_import)
!
      num64 = comm_IO%ntot_import
      call gz_mpi_write_int_vector_b                                    &
     &   (IO_param, num64, comm_IO%item_import)
!
      end subroutine gz_mpi_write_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_export_data_b(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: comm_IO
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = comm_IO%num_neib
      call gz_mpi_write_integer_stack_b                                 &
     &   (IO_param, num64, comm_IO%istack_export)
!
      num64 = comm_IO%ntot_export
      call gz_mpi_write_int_vector_b                                    &
     &   (IO_param, num64, comm_IO%item_export)
!
      end subroutine gz_mpi_write_export_data_b
!
! -----------------------------------------------------------------------!
      end module gz_MPI_domain_data_IO_b
