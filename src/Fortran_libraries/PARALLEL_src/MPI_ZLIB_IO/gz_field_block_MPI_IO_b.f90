!>@file  gz_field_block_MPI_IO_b.f90
!!       module gz_field_block_MPI_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_field_time_mpi_b                            &
!!     &         (IO_param_l, i_time_step_IO, time_IO, delta_t_IO)
!!      subroutine gz_write_field_data_mpi_b(IO_param_l,                &
!!     &          nnod, num_field, ntot_comp, field_name, d_nod)
!!
!!      subroutine gz_read_step_data_mpi_b                              &
!!     &         (IO_param_l, i_time_step_IO, time_IO, delta_t_IO)
!!@endverbatim
!
      module gz_field_block_MPI_IO_b
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use m_calypso_mpi_IO
      use t_time_data
      use t_calypso_mpi_IO_param
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_write_field_time_mpi_b                              &
     &         (IO_param_l, i_time_step_IO, time_IO, delta_t_IO)
!
      use m_phys_constants
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
!
      integer(kind=kint), intent(in) :: i_time_step_IO
      real(kind = kreal), intent(in) :: time_IO, delta_t_IO
!
!
      call gz_mpi_write_process_id_b(IO_param_l)
!
      call gz_mpi_write_one_inthead_b(IO_param_l, i_time_step_IO)
      call gz_mpi_write_one_realhead_b(IO_param_l, time_IO)
      call gz_mpi_write_one_realhead_b(IO_param_l, delta_t_IO)
!
      end subroutine gz_write_field_time_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_field_data_mpi_b(IO_param_l,                  &
     &          nnod, num_field, ntot_comp, ncomp_field, field_name,    &
     &          istack_merged, d_nod)
!
      use m_phys_constants
      use field_data_IO
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
!
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len = kchara), intent(in) :: field_name(num_field)
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
!
      call gz_mpi_write_merged_stack_b(IO_param_l,                      &
     &    IO_param_l%nprocs_in, istack_merged)
      call gz_mpi_write_one_inthead_b(IO_param_l, num_field)
      call gz_mpi_write_mul_inthead_b                                   &
     &   (IO_param_l, num_field, ncomp_field)
!
      call gz_mpi_write_mul_charahead_b                                 &
     &   (IO_param_l, num_field, field_name)
      call gz_mpi_write_2d_vector_b                                     &
     &   (IO_param_l, nnod, ntot_comp, d_nod)
!
      end subroutine gz_write_field_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_step_data_mpi_b                                &
     &         (IO_param_l, i_time_step_IO, time_IO, delta_t_IO)
!
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
      use m_error_IDs
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
!
      integer(kind=kint), intent(inout) :: i_time_step_IO
      real(kind = kreal), intent(inout) :: time_IO, delta_t_IO
!
!
      call gz_mpi_read_process_id_b(IO_param_l)
      call gz_mpi_read_one_inthead_b(IO_param_l, i_time_step_IO)
!
      call gz_mpi_read_one_realhead_b(IO_param_l, time_IO)
      call gz_mpi_read_one_realhead_b(IO_param_l, delta_t_IO)
!
      end subroutine gz_read_step_data_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_field_header_mpi_b(IO_param_l, nnod, num_field)
!
      use m_phys_constants
      use field_data_IO
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
      integer(kind = kint_gl), intent(inout) :: nnod
      integer(kind = kint), intent(inout) :: num_field
!
!
      call gz_mpi_read_merged_stack_b(IO_param_l,                       &
     &    IO_param_l%nprocs_in, IO_param_l%istack_merged)
      nnod = IO_param_l%istack_merged(IO_param_l%id_rank+1)             &
     &      - IO_param_l%istack_merged(IO_param_l%id_rank)
!
      call gz_mpi_read_one_inthead_b(IO_param_l, num_field)
!
      end subroutine gz_read_field_header_mpi_b
!
! -----------------------------------------------------------------------
!
      end module gz_field_block_MPI_IO_b
