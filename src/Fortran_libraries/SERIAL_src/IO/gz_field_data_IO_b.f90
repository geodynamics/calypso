!>@file  gz_field_data_IO_b.f90
!!       module gz_field_data_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_step_data_b                                 &
!!     &         (id_rank, i_time_step_IO, time_IO, delta_t_IO)
!!      subroutine gz_write_field_data_b(nnod, num_field,               &
!!     &          ntot_comp, ncomp_field, field_name, d_nod)
!!
!!      subroutine gz_read_step_data_b                                  &
!!     &         (my_rank, i_time_step_IO, time_IO, delta_t_IO,         &
!!     &          istack_merged, num_field)
!!      subroutine gz_read_field_data_b(nnod, num_field, ncomp,         &
!!     &          field_name, vect)
!!@endverbatim
!
      module gz_field_data_IO_b
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use gz_binary_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_step_data_b                                   &
     &         (id_rank, i_time_step_IO, time_IO, delta_t_IO)
!
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: i_time_step_IO
      real(kind = kreal), intent(in) :: time_IO, delta_t_IO
!
!
      call gz_write_endian_flag
      call gz_write_one_integer_b(id_rank)
      call gz_write_one_integer_b(i_time_step_IO)
!
      call gz_write_one_real_b(time_IO)
      call gz_write_one_real_b(delta_t_IO)
!
      end subroutine gz_write_step_data_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_field_data_b(nnod, num_field,                 &
     &          ntot_comp, ncomp_field, field_name, d_nod)
!
      use m_phys_constants
!
      integer(kind=kint), intent(in) ::  nnod
!
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      istack_merged(1) = nnod
      call gz_write_mul_int8_b(ione, istack_merged)
      call gz_write_one_integer_b(num_field)
      call gz_write_mul_integer_b(num_field, ncomp_field)
!
      call gz_write_mul_character_b(num_field, field_name)
      call gz_write_2d_vector_b(nnod, ntot_comp, d_nod)
!
      end subroutine gz_write_field_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_step_data_b                                    &
     &         (my_rank, i_time_step_IO, time_IO, delta_t_IO,           &
     &          istack_merged, num_field)
!
      integer(kind=kint), intent(in) :: my_rank
!
      integer(kind=kint), intent(inout) :: i_time_step_IO
      real(kind = kreal), intent(inout) :: time_IO, delta_t_IO
!
      integer(kind=kint_gl), intent(inout) :: istack_merged(1)
      integer(kind=kint), intent(inout) :: num_field
      integer(kind = kint) :: id_rank
!
!
      call gz_read_endian_flag(my_rank)
!
      call gz_read_one_integer_b(id_rank)
      call gz_read_one_integer_b(i_time_step_IO)
      call gz_read_one_real_b(time_IO)
      call gz_read_one_real_b(delta_t_IO)
!
      call gz_read_mul_int8_b(ione, istack_merged(1))
      call gz_read_one_integer_b(num_field)
!
      end subroutine gz_read_step_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_field_data_b(nnod, num_field, ncomp,           &
     &          field_name, vect)
!
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ncomp
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: vect(nnod,ncomp)
!
!
      call gz_read_mul_character_b(num_field, field_name)
      call gz_read_2d_vector_b(nnod, ncomp, vect)
!
      end subroutine gz_read_field_data_b
!
! -----------------------------------------------------------------------
!
      end module gz_field_data_IO_b
