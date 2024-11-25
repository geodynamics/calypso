!>@file  field_data_IO_b.f90
!!       module field_data_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_step_data_b(id_rank, t_IO, bbuf)
!!      subroutine write_field_data_b(num_field, field_name,            &
!!     &          ncomp_field, nnod64, ntot_comp, d_nod, bbuf)
!!        type(time_data), intent(in) :: t_IO
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!
!!      subroutine read_step_data_b(bbuf, t_IO)
!!      subroutine read_field_num_b(bbuf, istack_merged, num_field)
!!      subroutine read_field_data_b                                    &
!!     &         (bbuf, num_field, field_name, nnod64, ntot_comp, vect)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(time_data), intent(inout) :: t_IO
!!@endverbatim
!
      module field_data_IO_b
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_time_data
      use t_binary_IO_buffer
      use binary_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_step_data_b(id_rank, t_IO, bbuf)
!
      integer, intent(in) :: id_rank
      type(time_data), intent(in) :: t_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint) :: irank_write
!
!
      irank_write = int(id_rank,KIND(irank_write))
      call write_one_integer_b(irank_write, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_one_integer_b(t_IO%i_time_step, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      call write_one_real_b(t_IO%time, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_one_real_b(t_IO%dt, bbuf)
!
      end subroutine write_step_data_b
!
! -----------------------------------------------------------------------
!
      subroutine write_field_data_b(num_field, field_name,              &
     &          ncomp_field, nnod64, ntot_comp, d_nod, bbuf)
!
      use m_phys_constants
      use transfer_to_long_integers
!
      integer(kind=kint_gl), intent(in) :: nnod64
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod64,ntot_comp)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl), parameter :: ione64 = 1
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      istack_merged(1) = nnod64
      call write_mul_int8_b(ione64, istack_merged, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_one_integer_b(num_field, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_mul_integer_b                                          &
     &   (cast_long(num_field), ncomp_field, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      call write_mul_character_b(num_field, field_name, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_2d_vector_b(nnod64, ntot_comp, d_nod, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      end subroutine write_field_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_step_data_b(bbuf, t_IO)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(time_data), intent(inout) :: t_IO
!
      integer(kind = kint) :: irank_read
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      call read_one_integer_b(bbuf, irank_read)
      if(bbuf%ierr_bin .gt. 0) return
      call read_one_integer_b(bbuf, t_IO%i_time_step)
      if(bbuf%ierr_bin .gt. 0) return
      call read_one_real_b(bbuf, t_IO%time)
      if(bbuf%ierr_bin .ne. 0) return
      call read_one_real_b(bbuf, t_IO%dt)
!
      end subroutine read_step_data_b
!
! -----------------------------------------------------------------------
!
      subroutine read_field_num_b(bbuf, istack_merged, num_field)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind=kint_gl), intent(inout) :: istack_merged(1)
      integer(kind=kint), intent(inout) :: num_field
!
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      call read_mul_int8_b(bbuf, ione64, istack_merged)
      if(bbuf%ierr_bin .ne. 0) return
!
      call read_one_integer_b(bbuf, num_field)
!
      end subroutine read_field_num_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_data_b                                      &
     &         (bbuf, num_field, field_name, nnod64, ntot_comp, vect)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      integer(kind = kint_gl), intent(in) :: nnod64
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: vect(nnod64,ntot_comp)
!
!
      call read_mul_character_b(bbuf, num_field, field_name)
      if(bbuf%ierr_bin .ne. 0) return
!
      call read_2d_vector_b(bbuf, nnod64, ntot_comp, vect)
      if(bbuf%ierr_bin .ne. 0) return
!
      end subroutine read_field_data_b
!
! -----------------------------------------------------------------------
!
      end module field_data_IO_b
