!>@file  gz_field_data_IO_b.f90
!!       module gz_field_data_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_step_data_b(FPz_f, id_rank,                 &
!!     &          i_time_step_IO, time_IO, delta_t_IO, zbuf)
!!      subroutine gz_write_field_data_b(FPz_f, nnod, num_field,        &
!!     &          ntot_comp, ncomp_field, field_name, d_nod, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_read_step_data_b(FPz_f, zbuf, id_rank,            &
!!     &          i_time_step_IO, time_IO, delta_t_IO)
!!      subroutine gz_read_num_field_b(FPz_f, zbuf,                     &
!!     &                               istack_merged, num_field)
!!      subroutine gz_read_field_data_b                                 &
!!     &         (FPz_f, zbuf, nnod, num_field, ncomp, field_name, vect)
!!        character, pointer, intent(in) :: FPz_f
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module gz_field_data_IO_b
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_buffer_4_gzip
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
      subroutine gz_write_step_data_b(FPz_f, id_rank,                   &
     &          i_time_step_IO, time_IO, delta_t_IO, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: i_time_step_IO
      real(kind = kreal), intent(in) :: time_IO, delta_t_IO
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: irank_write
!
!
      irank_write = int(id_rank,KIND(irank_write))
      call gz_write_one_integer_b(FPz_f, irank_write, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_one_integer_b(FPz_f, i_time_step_IO, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_write_one_real_b(FPz_f, time_IO, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_one_real_b(FPz_f, delta_t_IO, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_write_step_data_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_field_data_b(FPz_f, nnod, num_field,          &
     &          ntot_comp, ncomp_field, field_name, d_nod, zbuf)
!
      use m_phys_constants
!
      character, pointer, intent(in) :: FPz_f
      integer(kind=kint_gl), intent(in) ::  nnod
!
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: istack_merged(1)
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      istack_merged(1) = nnod
      call gz_write_mul_int8_b(FPz_f, ione64, istack_merged, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_one_integer_b(FPz_f, num_field, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_mul_integer_b                                       &
     &   (FPz_f, cast_long(num_field), ncomp_field, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_write_mul_character_b                                     &
     &   (FPz_f, cast_long(num_field), field_name, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_2d_vector_b(FPz_f, nnod, ntot_comp, d_nod, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_write_field_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_step_data_b(FPz_f, zbuf, id_rank,              &
     &          i_time_step_IO, time_IO, delta_t_IO)
!
      use m_error_IDs
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
!
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind=kint), intent(inout) :: i_time_step_IO
      real(kind = kreal), intent(inout) :: time_IO, delta_t_IO
!
      integer(kind = kint) :: id_read_rank
!
!
      call gz_read_one_integer_b(FPz_f, zbuf, id_read_rank)
      if(int(id_read_rank) .ne. id_rank) then
        write(*,*) 'error in peocess ID input'
        zbuf%ierr_zlib = ierr_file
      end if
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_read_one_integer_b(FPz_f, zbuf, i_time_step_IO)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_read_one_real_b(FPz_f, zbuf, time_IO)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_read_one_real_b(FPz_f, zbuf, delta_t_IO)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_read_step_data_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_num_field_b(FPz_f, zbuf,                       &
     &                               istack_merged, num_field)
!
      use m_error_IDs
!
      character, pointer, intent(in) :: FPz_f
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl), intent(inout) :: istack_merged(1)
      integer(kind = kint), intent(inout) :: num_field
!
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      call gz_read_mul_int8_b(FPz_f, zbuf, ione64, istack_merged(1))
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_read_one_integer_b(FPz_f, zbuf, num_field)
!
      end subroutine gz_read_num_field_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_field_data_b                                   &
     &         (FPz_f, zbuf, nnod, num_field, ncomp, field_name, vect)
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: num_field, ncomp
!
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: vect(nnod,ncomp)
!
!
      call gz_read_mul_character_b                                      &
     &   (FPz_f, zbuf, cast_long(num_field), field_name)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_read_2d_vector_b(FPz_f, zbuf, nnod, ncomp, vect)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_read_field_data_b
!
! -----------------------------------------------------------------------
!
      end module gz_field_data_IO_b
