!>@file  gz_spherical_model_IO.f90
!!       module gz_spherical_model_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine read_rank_4_sph_gz(FPz_f, sph_IO, zbuf)
!!      subroutine read_gl_resolution_sph_gz(FPz_f, sph_IO, zbuf)
!!      subroutine read_gl_nodes_sph_gz(FPz_f, sph_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine write_rank_4_sph_gz(FPz_f, sph_IO, zbuf)
!!      subroutine write_gl_resolution_sph_gz(FPz_f, sph_IO, zbuf)
!!      subroutine write_gl_nodes_sph_gz(FPz_f, sph_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(sph_IO_data), intent(in) :: sph_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module gz_spherical_model_IO
!
      use m_precision
!
      use t_node_id_spherical_IO
      use t_buffer_4_gzip
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_rank_4_sph_gz(FPz_f, sph_IO, zbuf)
!
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      type(sph_IO_data), intent(inout) :: sph_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call skip_gz_comment_int(FPz_f, sph_IO%sph_rank(1), zbuf)
      read(zbuf%fixbuf(1),*) sph_IO%sph_rank(1:sph_IO%numdir_sph)
!
      end subroutine read_rank_4_sph_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_resolution_sph_gz(FPz_f, sph_IO, zbuf)
!
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      type(sph_IO_data), intent(inout) :: sph_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call skip_gz_comment_int(FPz_f, sph_IO%nidx_gl_sph(1), zbuf)
      read(zbuf%fixbuf(1),*) sph_IO%nidx_gl_sph(1:sph_IO%numdir_sph)
!
      call skip_gz_comment_int(FPz_f, sph_IO%ltr_gl, zbuf)
!
      end subroutine read_gl_resolution_sph_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_nodes_sph_gz(FPz_f, sph_IO, zbuf)
!
      use gzip_file_access
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      type(sph_IO_data), intent(inout) :: sph_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i
!
!
      call skip_gz_comment_int(FPz_f, sph_IO%numnod_sph, zbuf)
!
      call alloc_nod_id_sph_IO(sph_IO)
!
      do i = 1, sph_IO%numnod_sph
        call get_one_line_text_from_gz(FPz_f, zbuf)
        read(zbuf%fixbuf(1),*) sph_IO%inod_gl_sph(i),                   &
     &                        sph_IO%idx_gl_sph(i,1:sph_IO%numdir_sph)
      end do
!
      end subroutine read_gl_nodes_sph_gz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_rank_4_sph_gz(FPz_f, sph_IO, zbuf)
!
      use m_sph_modes_grid_labels
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      type(sph_IO_data), intent(in) :: sph_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      character(len=kchara) :: fmt_txt
!
!
      zbuf%fixbuf(1) = hd_segment() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      write(fmt_txt,'(a1,i2,a10)')                                      &
     &         '(', sph_IO%numdir_sph, '(i16),2a1)'
      write(zbuf%fixbuf(1),fmt_txt)                                     &
     &         sph_IO%sph_rank(1:sph_IO%numdir_sph), char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      end subroutine write_rank_4_sph_gz
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_resolution_sph_gz(FPz_f, sph_IO, zbuf)
!
      use m_sph_modes_grid_labels
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      type(sph_IO_data), intent(in) :: sph_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      character(len=kchara) :: fmt_txt
!
!
      zbuf%fixbuf(1) = hd_trunc() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      write(fmt_txt,'(a1,i2,a10)')                                      &
     &       '(', sph_IO%numdir_sph, '(i16),2a1)'
      write(zbuf%fixbuf(1),fmt_txt)                                     &
     &       sph_IO%nidx_gl_sph(1:sph_IO%numdir_sph), char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      write(zbuf%fixbuf(1),'(i16,2a1)')                                 &
     &       sph_IO%ltr_gl, char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      end subroutine write_gl_resolution_sph_gz
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_nodes_sph_gz(FPz_f, sph_IO, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      type(sph_IO_data), intent(in) :: sph_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i
      character(len=kchara) :: fmt_txt
!
!
      write(zbuf%fixbuf(1),'(i16,2a1)')                                 &
     &        sph_IO%numnod_sph, char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      write(fmt_txt,'(a5,i2,a10)')                                      &
     &                '(i16,', sph_IO%numdir_sph, '(i16),2a1)'
      do i = 1, sph_IO%numnod_sph
        write(zbuf%fixbuf(1),fmt_txt) sph_IO%inod_gl_sph(i),            &
     &      sph_IO%idx_gl_sph(i,1:sph_IO%numdir_sph), char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end do
!
      end subroutine write_gl_nodes_sph_gz
!
! -----------------------------------------------------------------------
!
      end module gz_spherical_model_IO
