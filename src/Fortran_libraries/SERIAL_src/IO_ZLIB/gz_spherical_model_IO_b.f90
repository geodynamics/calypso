!>@file  gz_spherical_model_IO_b.f90
!!       module gz_spherical_model_IO_b
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine gz_read_rank_4_sph_b(bflag, sph_IO)
!!      subroutine gz_read_gl_resolution_sph_b(bflag, sph_IO)
!!      subroutine gz_read_gl_nodes_sph_b(bflag, sph_IO)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine gz_write_rank_4_sph_b(sph_IO, bflag)
!!      subroutine gz_write_gl_resolution_sph_b(sph_IO, bflag)
!!      subroutine gz_write_gl_nodes_sph_b(sph_IO, bflag)
!!        type(sph_IO_data), intent(in) :: sph_IO
!!        type(binary_IO_flags), intent(inout) :: bflag
!!@endverbatim
!
      module gz_spherical_model_IO_b
!
      use m_precision
!
      use t_node_id_spherical_IO
      use gz_binary_IO
      use binary_IO
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
      subroutine gz_read_rank_4_sph_b(bflag, sph_IO)
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call gz_read_mul_integer_b                                        &
     &   (bflag, cast_long(sph_IO%numdir_sph), sph_IO%sph_rank)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_read_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_gl_resolution_sph_b(bflag, sph_IO)
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call gz_read_mul_integer_b                                        &
     &   (bflag, cast_long(sph_IO%numdir_sph), sph_IO%nidx_gl_sph)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_one_integer_b(bflag, sph_IO%ltr_gl)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_read_gl_resolution_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_gl_nodes_sph_b(bflag, sph_IO)
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint_gl) :: nvect
!
!
      call gz_read_one_integer_b(bflag, sph_IO%numnod_sph)
      if(bflag%ierr_IO .ne. 0) return
!
      call alloc_nod_id_sph_IO(sph_IO)
!
      call gz_read_mul_int8_b                                           &
     &   (bflag, cast_long(sph_IO%numnod_sph), sph_IO%inod_gl_sph)
      if(bflag%ierr_IO .ne. 0) return
!
      nvect = sph_IO%numnod_sph * sph_IO%numdir_sph
      call gz_read_mul_integer_b(bflag, nvect, sph_IO%idx_gl_sph)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_read_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_rank_4_sph_b(sph_IO, bflag)
!
      type(sph_IO_data), intent(in) :: sph_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call gz_write_mul_integer_b                                       &
     &   (cast_long(sph_IO%numdir_sph), sph_IO%sph_rank, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_gl_resolution_sph_b(sph_IO, bflag)
!
      type(sph_IO_data), intent(in) :: sph_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call gz_write_mul_integer_b                                       &
     &   (cast_long(sph_IO%numdir_sph), sph_IO%nidx_gl_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_one_integer_b(sph_IO%ltr_gl, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_gl_resolution_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_gl_nodes_sph_b(sph_IO, bflag)
!
      type(sph_IO_data), intent(in) :: sph_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint_gl) ::  nvect
!
!
      call gz_write_one_integer_b(sph_IO%numnod_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_mul_int8_b                                          &
     &   (cast_long(sph_IO%numnod_sph), sph_IO%inod_gl_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      nvect = sph_IO%numnod_sph * sph_IO%numdir_sph
      call gz_write_mul_integer_b(nvect, sph_IO%idx_gl_sph, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
!
      end module gz_spherical_model_IO_b
