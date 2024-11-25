!>@file  field_block_MPI_IO_b.f90
!!       module field_block_MPI_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_field_time_mpi_b(IO_param_l,                   &
!!     &          i_time_step_IO, time_IO, delta_t_IO)
!!      subroutine write_field_data_mpi_b(IO_param_l,                   &
!!     &          i_time_step_IO, time_IO, delta_t_IO,                  &
!!     &          nnod, num_field, ntot_comp, ncomp_field,              &
!!     &          field_name, d_nod, istack_merged)
!!      subroutine read_field_time_mpi_b(num_pe, IO_param_l, t_IO)
!!      subroutine read_num_field_mpi_b                                 &
!!     &         (num_pe, IO_param_l, nnod, istack_merged)
!!@endverbatim
!
      module field_block_MPI_IO_b
!
      use m_precision
      use m_constants
!
      use m_phys_constants
      use calypso_mpi
      use m_calypso_mpi_IO
!
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
      subroutine write_field_time_mpi_b(IO_param_l,                     &
     &          i_time_step_IO, time_IO, delta_t_IO)
!
      use MPI_binary_head_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
!
      integer(kind=kint), intent(in) :: i_time_step_IO
      real(kind = kreal), intent(in) :: time_IO, delta_t_IO
!
!
      call mpi_write_process_id_b(IO_param_l)
      call mpi_write_one_inthead_b(IO_param_l, i_time_step_IO)
      call mpi_write_one_realhead_b(IO_param_l, time_IO)
      call mpi_write_one_realhead_b(IO_param_l, delta_t_IO)
!
      end subroutine write_field_time_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine write_field_data_mpi_b(IO_param_l,                     &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod, istack_merged)
!
      use MPI_binary_data_IO
      use MPI_binary_head_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
!
      integer(kind = kint_gl), intent(in)                               &
     &                    :: istack_merged(0:IO_param_l%nprocs_in)
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
!
      call mpi_write_i8stack_head_b(IO_param_l,                         &
     &    cast_long(IO_param_l%nprocs_in), istack_merged)
!
      call mpi_write_one_inthead_b(IO_param_l, num_field)
      call mpi_write_mul_inthead_b(IO_param_l,num_field, ncomp_field)
!
      call mpi_write_mul_charahead_b(IO_param_l, num_field, field_name)
!
      call copy_istack_4_parallell_data(istack_merged, IO_param_l)
      call mpi_write_2d_vector_b(IO_param_l, nnod, ntot_comp, d_nod)
!
      end subroutine write_field_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_time_mpi_b(num_pe, IO_param_l, t_IO)
!
      use m_phys_constants
      use field_data_MPI_IO
      use MPI_binary_head_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: num_pe
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
      type(time_data), intent(inout) :: t_IO
!
!
      call mpi_read_process_id_b(IO_param_l)
!
      call mpi_read_one_inthead_b(IO_param_l, t_IO%i_time_step)
      call mpi_read_one_realhead_b(IO_param_l, t_IO%time)
      call mpi_read_one_realhead_b(IO_param_l, t_IO%dt)
!
      end subroutine read_field_time_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_num_field_mpi_b                                   &
     &         (num_pe, IO_param_l, nnod, istack_merged)
!
      use m_phys_constants
      use field_data_MPI_IO
      use MPI_binary_head_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: num_pe
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
      integer(kind = kint_gl), intent(inout) ::  nnod
      integer(kind = kint_gl), intent(inout) :: istack_merged(0:num_pe)
!
!
      call mpi_read_i8stack_head_b                                      &
     &   (IO_param_l, cast_long(num_pe), istack_merged)
      call sync_field_header_mpi                                        &
     &   (num_pe, IO_param_l%id_rank, istack_merged, nnod)
!
      end subroutine read_num_field_mpi_b
!
! -----------------------------------------------------------------------
!
      end module field_block_MPI_IO_b
