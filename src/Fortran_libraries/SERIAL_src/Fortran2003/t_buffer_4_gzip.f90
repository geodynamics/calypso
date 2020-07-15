!>@file  t_buffer_4_gzip.f90
!!       module t_buffer_4_gzip
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!!        Modified in March, 2020
!!
!> @brief Buffer and pointers for zlib data IO
!!
!!@verbatim
!!      subroutine alloc_zip_buffer(zbuf)
!!      subroutine alloc_textbuffer_for_zlib(len_buf, zbuf)
!!      subroutine alloc_fixbuffer_for_zlib(zbuf)
!!      subroutine dealloc_zip_buffer(zbuf)
!!      subroutine dealloc_textbuffer_for_zlib(zbuf)
!!      subroutine dealloc_fixbuffer_for_zlib(zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine link_pointer_for_zlib_buffer                         &
!!     &         (len_gzipbuf, gzipbuf, len_buf, textbuf, zbuf)
!!      subroutine unlink_pointer_for_zlib_buffer(zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
!!      subroutine link_real_buffer_for_zlib(num, data, zbuf)
!!      subroutine link_int8_buffer_for_zlib(num, int8_dat, zbuf)
!!      subroutine link_int4_buffer_for_zlib(num, int4_dat, zbuf)
!!      subroutine link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine unlink_compressed_buffer(zbuf)
!!      subroutine unlink_real_buffer_for_zlib(zbuf)
!!      subroutine unlink_int8_buffer_for_zlib(zbuf)
!!      subroutine unlink_int4_buffer_for_zlib(zbuf)
!!      subroutine unlink_text_buffer_for_zlib(zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module t_buffer_4_gzip
!
      use ISO_C_BINDING
      use m_precision
!
      implicit none
!
!>      lengh of fixed text buffer
      integer(kind = 4), parameter, private :: nbuf = 65535
!
!>      Structure of buffer for zlib
      type buffer_4_gzip
!>        Actual size of compressed data buffer
        integer(kind = kint_gl) :: ilen_gzipped
!>        Reserved size of compressed data buffer
        integer(kind = kint_gl) :: ilen_gz
!>        Compressed data buffer
        character(len=1), allocatable :: gzip_buf(:)
!
!>        Error flag from zlib
        integer(C_int) :: ierr_zlib
!>        Byte swapping flag to zlib
        integer(C_int) :: iflag_swap
!
!>        Size of compressed buffer to zlib
        integer(C_int) :: len_gzipbuf
!>        Size of decompressed buffer to zlib
        integer(C_int) :: len_buf
!>        Actual size of compressed buffer to zlib
        integer(C_int) :: len_used
!>        Number of words from zlib read
        integer(C_int) :: num_word
!
!>        Pointer of compressed data buffer
        character(C_char), pointer :: gzipbuf_p(:)
!>        Pointer of decompressed text buffer
        character(C_char), pointer :: buf_p(:)
!>        Pointer of decompressed real data
        real(C_double), pointer :: dat_p(:)
!>        Pointer of decompressed 8-byte integer data
        integer(C_long), pointer :: idat8_p(:)
!>        Pointer of decompressed 84byte integer data
        integer(C_int), pointer :: idat4_p(:)
!
!>        decompressed text buffer
        character(len=1), allocatable :: textbuf(:)
!>        text buffer with fixed length
        character(len=nbuf), allocatable :: fixbuf(:)
      end type buffer_4_gzip
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_zip_buffer(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      allocate(zbuf%gzip_buf(zbuf%ilen_gz))
!
      end subroutine alloc_zip_buffer
!
! -----------------------------------------------------------------------
!
      subroutine alloc_textbuffer_for_zlib(len_buf, zbuf)
!
      integer, intent(in) :: len_buf
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      allocate(zbuf%textbuf(len_buf))
!
      end subroutine alloc_textbuffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_fixbuffer_for_zlib(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      allocate(zbuf%fixbuf(1))
!
      end subroutine alloc_fixbuffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_zip_buffer(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      deallocate(zbuf%gzip_buf)
!
      end subroutine dealloc_zip_buffer
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_textbuffer_for_zlib(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      deallocate(zbuf%textbuf)
!
      end subroutine dealloc_textbuffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_fixbuffer_for_zlib(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      deallocate(zbuf%fixbuf)
!
      end subroutine dealloc_fixbuffer_for_zlib
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine link_pointer_for_zlib_buffer                           &
     &         (len_gzipbuf, gzipbuf, len_buf, textbuf, zbuf)
!
      integer, intent(in) :: len_gzipbuf
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
!
      end subroutine link_pointer_for_zlib_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_pointer_for_zlib_buffer(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      call unlink_compressed_buffer(zbuf)
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine unlink_pointer_for_zlib_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
!
      integer, intent(in) :: len_gzipbuf
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%len_gzipbuf = int(len_gzipbuf,KIND(zbuf%len_gzipbuf))
      zbuf%gzipbuf_p => gzipbuf
!
      end subroutine link_compressed_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine link_real_buffer_for_zlib(num, data, zbuf)
!
      integer, intent(in) :: num
      real(kind = kreal), target, intent(in) :: data(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%len_buf = int((num*kreal),KIND(zbuf%len_buf))
      zbuf%dat_p => data
!
      end subroutine link_real_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine link_int8_buffer_for_zlib(num, int8_dat, zbuf)
!
      integer, intent(in) :: num
      integer(kind = kint_gl), target, intent(in) :: int8_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%len_buf = int((num*kint_gl),KIND(zbuf%len_buf))
      zbuf%idat8_p => int8_dat
!
      end subroutine link_int8_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine link_int4_buffer_for_zlib(num, int4_dat, zbuf)
!
      integer, intent(in) :: num
      integer(kind = 4), target, intent(in) :: int4_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%len_buf = int((num*4),KIND(zbuf%len_buf))
      zbuf%idat4_p => int4_dat
!
      end subroutine link_int4_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
!
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%len_buf = int(len_buf,KIND(zbuf%len_buf))
      zbuf%buf_p => textbuf
!
      end subroutine link_text_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine unlink_compressed_buffer(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      nullify(zbuf%gzipbuf_p)
!
      end subroutine unlink_compressed_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_real_buffer_for_zlib(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      nullify(zbuf%dat_p)
!
      end subroutine unlink_real_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_int8_buffer_for_zlib(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      nullify(zbuf%idat8_p)
!
      end subroutine unlink_int8_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_int4_buffer_for_zlib(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      nullify(zbuf%idat4_p)
!
      end subroutine unlink_int4_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_text_buffer_for_zlib(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      nullify(zbuf%buf_p)
!
      end subroutine unlink_text_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      end module t_buffer_4_gzip
