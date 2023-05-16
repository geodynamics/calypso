!>@file  gz_spherical_model_IO_b.f90
!!       module gz_spherical_model_IO_b
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine gz_read_rank_4_sph_b(FPz_f, zbuf, sph_IO)
!!      subroutine gz_read_gl_resolution_sph_b(FPz_f, zbuf, sph_IO)
!!      subroutine gz_read_gl_nodes_sph_b(FPz_f, zbuf, sph_IO)
!!        character, pointer, intent(in) :: FPz_f
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine gz_write_rank_4_sph_b(FPz_f, sph_IO, zbuf)
!!      subroutine gz_write_gl_resolution_sph_b(FPz_f, sph_IO, zbuf)
!!      subroutine gz_write_gl_nodes_sph_b(FPz_f, sph_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(sph_IO_data), intent(in) :: sph_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module gz_spherical_model_IO_b
!
      use m_precision
!
      use t_node_id_spherical_IO
      use t_buffer_4_gzip
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
      subroutine gz_read_rank_4_sph_b(FPz_f, zbuf, sph_IO)
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call gz_read_mul_integer_b                                        &
     &   (FPz_f, zbuf, cast_long(sph_IO%numdir_sph), sph_IO%sph_rank)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_read_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_gl_resolution_sph_b(FPz_f, zbuf, sph_IO)
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call gz_read_mul_integer_b                                        &
     &   (FPz_f, zbuf, cast_long(sph_IO%numdir_sph), sph_IO%nidx_gl_sph)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_read_one_integer_b(FPz_f, zbuf, sph_IO%ltr_gl)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_read_gl_resolution_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_gl_nodes_sph_b(FPz_f, zbuf, sph_IO)
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint_gl) :: nvect
!
!
      call gz_read_one_integer_b(FPz_f, zbuf, sph_IO%numnod_sph)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call alloc_nod_id_sph_IO(sph_IO)
!
      call gz_read_mul_int8_b(FPz_f, zbuf,                              &
     &    cast_long(sph_IO%numnod_sph), sph_IO%inod_gl_sph)
      if(zbuf%ierr_zlib .ne. 0) return
!
      nvect = sph_IO%numnod_sph * sph_IO%numdir_sph
      call gz_read_mul_integer_b(FPz_f, zbuf, nvect, sph_IO%idx_gl_sph)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_read_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_rank_4_sph_b(FPz_f, sph_IO, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      type(sph_IO_data), intent(in) :: sph_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call gz_write_mul_integer_b                                       &
     &   (FPz_f, cast_long(sph_IO%numdir_sph), sph_IO%sph_rank, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_write_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_gl_resolution_sph_b(FPz_f, sph_IO, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      type(sph_IO_data), intent(in) :: sph_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call gz_write_mul_integer_b                                       &
     &   (FPz_f, cast_long(sph_IO%numdir_sph), sph_IO%nidx_gl_sph, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_one_integer_b(FPz_f, sph_IO%ltr_gl, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_write_gl_resolution_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_gl_nodes_sph_b(FPz_f, sph_IO, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      type(sph_IO_data), intent(in) :: sph_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) ::  nvect
!
!
      call gz_write_one_integer_b(FPz_f, sph_IO%numnod_sph, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_mul_int8_b(FPz_f, cast_long(sph_IO%numnod_sph),     &
     &                         sph_IO%inod_gl_sph, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      nvect = sph_IO%numnod_sph * sph_IO%numdir_sph
      call gz_write_mul_integer_b                                       &
     &   (FPz_f, nvect, sph_IO%idx_gl_sph, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_write_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
!
      end module gz_spherical_model_IO_b
