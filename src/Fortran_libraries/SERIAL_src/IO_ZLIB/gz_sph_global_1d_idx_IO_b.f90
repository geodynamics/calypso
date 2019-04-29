!>@file   gz_sph_global_1d_idx_IO_b.f90
!!@brief  module gz_sph_global_1d_idx_IO_b
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine gz_read_rtp_gl_1d_table_b(bflag, sph_IO)
!!      subroutine gz_read_rj_gl_1d_table_b(bflag, sph_IO)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine gz_write_rtp_gl_1d_table_b(sph_IO, bflag)
!!      subroutine gz_write_rj_gl_1d_table_b(sph_IO, bflag)
!!        type(sph_IO_data), intent(in) :: sph_IO
!!        type(binary_IO_flags), intent(inout) :: bflag
!!@endverbatim
!
      module gz_sph_global_1d_idx_IO_b
!
      use m_precision
!
      use t_node_id_spherical_IO
      use binary_IO
      use gz_binary_IO
      use transfer_to_long_integers
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_rtp_gl_1d_table_b(bflag, sph_IO)
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint_gl) :: nvect
!
!
      sph_IO%numdir_sph = 3
      sph_IO%ncomp_table_1d(1) = 1
      sph_IO%ncomp_table_1d(2) = 1
      sph_IO%ncomp_table_1d(3) = 2
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      call gz_read_mul_integer_b                                        &
     &   (bflag, cast_long(sph_IO%numdir_sph), sph_IO%nidx_sph)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_mul_integer_b                                        &
     &   (bflag, cast_long(sph_IO%numdir_sph), sph_IO%ist_sph)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_mul_integer_b                                        &
     &   (bflag, cast_long(sph_IO%numdir_sph), sph_IO%ied_sph)
      if(bflag%ierr_IO .ne. 0) return
!
!
      call alloc_idx_sph_1d1_IO(sph_IO)
      call alloc_idx_sph_1d2_IO(sph_IO)
      call alloc_idx_sph_1d3_IO(sph_IO)
!
      call gz_read_mul_integer_b                                        &
     &   (bflag, cast_long(sph_IO%nidx_sph(1)), sph_IO%idx_gl_1)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_1d_vector_b                                          &
     &   (bflag, cast_long(sph_IO%nidx_sph(1)), sph_IO%r_gl_1)
      if(bflag%ierr_IO .ne. 0) return
!
      nvect = sph_IO%nidx_sph(2) * sph_IO%ncomp_table_1d(2)
      call gz_read_mul_integer_b(bflag, nvect, sph_IO%idx_gl_2)
      if(bflag%ierr_IO .ne. 0) return
!
      nvect = sph_IO%nidx_sph(3) * sph_IO%ncomp_table_1d(3)
      call gz_read_mul_integer_b(bflag, nvect, sph_IO%idx_gl_3)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_read_rtp_gl_1d_table_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_rj_gl_1d_table_b(bflag, sph_IO)
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint_gl) :: nvect
!
!
      sph_IO%numdir_sph = 2
      sph_IO%ncomp_table_1d(1) = 1
      sph_IO%ncomp_table_1d(2) = 3
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      call gz_read_mul_integer_b                                        &
     &   (bflag, cast_long(sph_IO%numdir_sph), sph_IO%nidx_sph)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_mul_integer_b                                        &
     &   (bflag, cast_long(sph_IO%numdir_sph), sph_IO%ist_sph)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_mul_integer_b                                        &
     &   (bflag, cast_long(sph_IO%numdir_sph), sph_IO%ied_sph)
      if(bflag%ierr_IO .ne. 0) return
!
!
      call alloc_idx_sph_1d1_IO(sph_IO)
      call alloc_idx_sph_1d2_IO(sph_IO)
!
      call gz_read_mul_integer_b                                        &
     &   (bflag, cast_long(sph_IO%nidx_sph(1)), sph_IO%idx_gl_1)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_1d_vector_b                                          &
     &   (bflag, cast_long(sph_IO%nidx_sph(1)), sph_IO%r_gl_1)
      if(bflag%ierr_IO .ne. 0) return
!
      nvect = sph_IO%nidx_sph(2) * sph_IO%ncomp_table_1d(2)
      call gz_read_mul_integer_b(bflag, nvect, sph_IO%idx_gl_2)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_read_rj_gl_1d_table_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_rtp_gl_1d_table_b(sph_IO, bflag)
!
      type(sph_IO_data), intent(in) :: sph_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint_gl) :: nvect
!
!
      call gz_write_mul_integer_b                                       &
     &   (cast_long(sph_IO%numdir_sph), sph_IO%nidx_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_mul_integer_b                                       &
     &   (cast_long(sph_IO%numdir_sph), sph_IO%ist_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_mul_integer_b                                       &
     &   (cast_long(sph_IO%numdir_sph), sph_IO%ied_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_write_mul_integer_b                                       &
     &   (cast_long(sph_IO%nidx_sph(1)), sph_IO%idx_gl_1, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_1d_vector_b                                         &
     &   (cast_long(sph_IO%nidx_sph(1)), sph_IO%r_gl_1, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      nvect = sph_IO%nidx_sph(2) * sph_IO%ncomp_table_1d(2)
      call gz_write_mul_integer_b(nvect, sph_IO%idx_gl_2, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      nvect = sph_IO%nidx_sph(3) * sph_IO%ncomp_table_1d(3)
      call gz_write_mul_integer_b(nvect, sph_IO%idx_gl_3, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_rtp_gl_1d_table_b
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_rj_gl_1d_table_b(sph_IO, bflag)
!
      type(sph_IO_data), intent(in) :: sph_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint_gl) :: nvect
!
!
      call gz_write_mul_integer_b                                       &
     &   (cast_long(sph_IO%numdir_sph), sph_IO%nidx_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_mul_integer_b                                       &
     &   (cast_long(sph_IO%numdir_sph), sph_IO%ist_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_mul_integer_b                                       &
     &   (cast_long(sph_IO%numdir_sph), sph_IO%ied_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_write_mul_integer_b                                       &
     &   (cast_long(sph_IO%nidx_sph(1)), sph_IO%idx_gl_1, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_1d_vector_b                                         &
     &   (cast_long(sph_IO%nidx_sph(1)), sph_IO%r_gl_1, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      nvect = sph_IO%nidx_sph(2) * sph_IO%ncomp_table_1d(2)
      call gz_write_mul_integer_b(nvect, sph_IO%idx_gl_2, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_rj_gl_1d_table_b
!
! ----------------------------------------------------------------------
!
      end module gz_sph_global_1d_idx_IO_b
