!>@file  sph_global_1d_idx_IO_b.f90
!!       module sph_global_1d_idx_IO_b
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine read_rtp_gl_1d_table_b(bflag, sph_IO)
!!      subroutine read_rj_gl_1d_table_b(bflag, sph_IO)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine write_rtp_gl_1d_table_b(sph_IO, bflag)
!!      subroutine write_rj_gl_1d_table_b(sph_IO, bflag)
!!        type(sph_IO_data), intent(in) :: sph_IO
!!@endverbatim
!
      module sph_global_1d_idx_IO_b
!
      use m_precision
!
      use t_node_id_spherical_IO
      use binary_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_rtp_gl_1d_table_b(bflag, sph_IO)
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint_gl) :: num64
!
!
      sph_IO%numdir_sph = 3
      sph_IO%ncomp_table_1d(1) = 1
      sph_IO%ncomp_table_1d(2) = 1
      sph_IO%ncomp_table_1d(3) = 2
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      num64 = sph_IO%numdir_sph
      call read_mul_integer_b(bflag, num64, sph_IO%nidx_sph)
      if(bflag%ierr_IO .ne. 0) return
      call read_mul_integer_b(bflag, num64, sph_IO%ist_sph)
      if(bflag%ierr_IO .ne. 0) return
      call read_mul_integer_b(bflag, num64, sph_IO%ied_sph)
      if(bflag%ierr_IO .ne. 0) return
!
      call alloc_idx_sph_1d1_IO(sph_IO)
      call alloc_idx_sph_1d2_IO(sph_IO)
      call alloc_idx_sph_1d3_IO(sph_IO)
!
      num64 = sph_IO%nidx_sph(1)
      call read_mul_integer_b(bflag, num64, sph_IO%idx_gl_1)
      if(bflag%ierr_IO .ne. 0) return
      call read_1d_vector_b(bflag, num64 , sph_IO%r_gl_1)
      if(bflag%ierr_IO .ne. 0) return
!
      num64 = sph_IO%nidx_sph(2) * sph_IO%ncomp_table_1d(2)
      call read_mul_integer_b(bflag, num64, sph_IO%idx_gl_2)
      if(bflag%ierr_IO .ne. 0) return
!
      num64 = sph_IO%nidx_sph(3) * sph_IO%ncomp_table_1d(3)
      call read_mul_integer_b(bflag, num64, sph_IO%idx_gl_3)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine read_rtp_gl_1d_table_b
!
! -----------------------------------------------------------------------
!
      subroutine read_rj_gl_1d_table_b(bflag, sph_IO)
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint_gl) :: num64
!
!
      sph_IO%numdir_sph = 2
      sph_IO%ncomp_table_1d(1) = 1
      sph_IO%ncomp_table_1d(2) = 3
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      num64 = sph_IO%numdir_sph
      call read_mul_integer_b(bflag, num64, sph_IO%nidx_sph)
      if(bflag%ierr_IO .ne. 0) return
      call read_mul_integer_b(bflag, num64, sph_IO%ist_sph)
      if(bflag%ierr_IO .ne. 0) return
      call read_mul_integer_b(bflag, num64, sph_IO%ied_sph)
      if(bflag%ierr_IO .ne. 0) return
!
      call alloc_idx_sph_1d1_IO(sph_IO)
      call alloc_idx_sph_1d2_IO(sph_IO)
!
      num64 = sph_IO%nidx_sph(1)
      call read_mul_integer_b(bflag, num64, sph_IO%idx_gl_1)
      if(bflag%ierr_IO .ne. 0) return
      call read_1d_vector_b(bflag, num64, sph_IO%r_gl_1)
      if(bflag%ierr_IO .ne. 0) return
!
      num64 = sph_IO%nidx_sph(2) * sph_IO%ncomp_table_1d(2)
      call read_mul_integer_b(bflag, num64, sph_IO%idx_gl_2)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine read_rj_gl_1d_table_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_rtp_gl_1d_table_b(sph_IO, bflag)
!
      type(sph_IO_data), intent(in) :: sph_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = sph_IO%numdir_sph
      call write_mul_integer_b(num64, sph_IO%nidx_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_mul_integer_b(num64, sph_IO%ist_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_mul_integer_b(num64, sph_IO%ied_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      num64 = sph_IO%nidx_sph(1)
      call write_mul_integer_b(num64, sph_IO%idx_gl_1, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_1d_vector_b(num64, sph_IO%r_gl_1, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      num64 = sph_IO%nidx_sph(2) * sph_IO%ncomp_table_1d(2)
      call write_mul_integer_b(num64, sph_IO%idx_gl_2, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      num64 = sph_IO%nidx_sph(3) * sph_IO%ncomp_table_1d(3)
      call write_mul_integer_b(num64, sph_IO%idx_gl_3, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_rtp_gl_1d_table_b
!
! ----------------------------------------------------------------------
!
      subroutine write_rj_gl_1d_table_b(sph_IO, bflag)
!
      type(sph_IO_data), intent(in) :: sph_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = sph_IO%numdir_sph
      call write_mul_integer_b(num64, sph_IO%nidx_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_mul_integer_b(num64, sph_IO%ist_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_mul_integer_b(num64, sph_IO%ied_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      num64 = sph_IO%nidx_sph(1)
      call write_mul_integer_b(num64, sph_IO%idx_gl_1, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_1d_vector_b(num64, sph_IO%r_gl_1, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      num64 = sph_IO%nidx_sph(2) * sph_IO%ncomp_table_1d(2)
      call write_mul_integer_b(num64, sph_IO%idx_gl_2, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_rj_gl_1d_table_b
!
! ----------------------------------------------------------------------
!
      end module sph_global_1d_idx_IO_b
