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
!!     &         (id_rank, i_time_step_IO, time_IO, delta_t_IO, bflag)
!!      subroutine gz_write_field_data_b(nnod, num_field,               &
!!     &          ntot_comp, ncomp_field, field_name, d_nod, bflag)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!
!!      subroutine gz_read_step_data_b(bflag, id_rank,                  &
!!     &          i_time_step_IO, time_IO, delta_t_IO,                  &
!!     &          istack_merged, num_field)
!!      subroutine gz_read_field_data_b                                 &
!!     &         (bflag, nnod, num_field, ncomp, field_name, vect)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!@endverbatim
!
      module gz_field_data_IO_b
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use gz_binary_IO
      use transfer_to_long_integers
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
     &         (id_rank, i_time_step_IO, time_IO, delta_t_IO, bflag)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: i_time_step_IO
      real(kind = kreal), intent(in) :: time_IO, delta_t_IO
!
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: irank_write
!
!
      irank_write = int(id_rank,KIND(irank_write))
      call gz_write_one_integer_b(irank_write, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_one_integer_b(i_time_step_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_write_one_real_b(time_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_one_real_b(delta_t_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_step_data_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_field_data_b(nnod, num_field,                 &
     &          ntot_comp, ncomp_field, field_name, d_nod, bflag)
!
      use m_phys_constants
!
      integer(kind=kint_gl), intent(in) ::  nnod
!
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint_gl) :: istack_merged(1)
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      istack_merged(1) = nnod
      call gz_write_mul_int8_b(ione64, istack_merged, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_one_integer_b(num_field, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_mul_integer_b                                       &
     &   (cast_long(num_field), ncomp_field, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_write_mul_character_b                                     &
     &   (cast_long(num_field), field_name, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_2d_vector_b(nnod, ntot_comp, d_nod, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_field_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_step_data_b(bflag, id_rank,                    &
     &          i_time_step_IO, time_IO, delta_t_IO,                    &
     &          istack_merged, num_field)
!
      use m_error_IDs
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_flags), intent(inout) :: bflag
      integer(kind=kint), intent(inout) :: i_time_step_IO
      real(kind = kreal), intent(inout) :: time_IO, delta_t_IO
!
      integer(kind = kint_gl), intent(inout) :: istack_merged(1)
      integer(kind = kint), intent(inout) :: num_field
!
      integer(kind = kint) :: id_read_rank
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      call gz_read_one_integer_b(bflag, id_read_rank)
      if(int(id_read_rank) .ne. id_rank) then
        write(*,*) 'error in peocess ID input'
        bflag%ierr_IO = ierr_file
      end if
      if(bflag%ierr_IO .gt. 0) return
!
      call gz_read_one_integer_b(bflag, i_time_step_IO)
      if(bflag%ierr_IO .gt. 0) return
!
      call gz_read_one_real_b(bflag, time_IO)
      if(bflag%ierr_IO .gt. 0) return
!
      call gz_read_one_real_b(bflag, delta_t_IO)
      if(bflag%ierr_IO .gt. 0) return
!
      call gz_read_mul_int8_b(bflag, ione64, istack_merged(1))
      if(bflag%ierr_IO .gt. 0) return
!
      call gz_read_one_integer_b(bflag, num_field)
      if(bflag%ierr_IO .gt. 0) return
!
      end subroutine gz_read_step_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_field_data_b                                   &
     &         (bflag, nnod, num_field, ncomp, field_name, vect)
!
      type(binary_IO_flags), intent(inout) :: bflag
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: num_field, ncomp
!
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: vect(nnod,ncomp)
!
!
      call gz_read_mul_character_b                                      &
     &   (bflag, cast_long(num_field), field_name)
      if(bflag%ierr_IO .gt. 0) return
!
      call gz_read_2d_vector_b(bflag, nnod, ncomp, vect)
      if(bflag%ierr_IO .gt. 0) return
!
      end subroutine gz_read_field_data_b
!
! -----------------------------------------------------------------------
!
      end module gz_field_data_IO_b
