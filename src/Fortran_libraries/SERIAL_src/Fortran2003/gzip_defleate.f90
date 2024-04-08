!>@file   gzip_defleate.f90
!!        module gzip_defleate
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Wrapper for compression routines by zlib
!!
!!@verbatim
!!      subroutine gzip_defleat_real_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, data, zbuf)
!!      subroutine gzip_defleat_int8_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, int8_dat, zbuf)
!!      subroutine gzip_defleat_int4_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, int4_dat, zbuf)
!!
!!      subroutine zlib_defleat_char_once                               &
!!     &         (len_buf, textbuf, len_gzipbuf, zbuf, gzipbuf)
!!      subroutine gzip_defleat_char_once                               &
!!     &         (len_gzipbuf, gzipbuf, len_buf, buf, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gzip_defleat_char_begin(len_buf, textbuf,            &
!!     &                                   len_gzipbuf, zbuf, gzipbuf)
!!      subroutine gzip_defleat_char_cont(len_buf, textbuf, zbuf)
!!      subroutine gzip_defleat_char_last(len_buf, textbuf, zbuf)
!!        character, pointer, intent(inout) :: stream_ptr
!!        integer, intent(in) :: len_buf
!!        character(len=1), target, intent(in) :: textbuf(len_buf)
!!        integer, intent(in) :: len_gzipbuf
!!        character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module gzip_defleate
!
      use ISO_C_BINDING
      use m_precision
      use t_buffer_4_gzip
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      interface
!
!  ---------------------------------------------------------------------
      integer(C_int) function  calypso_gzip_defleat_once                &
     &           (len_buf, buf, len_gzipbuf, gzipbuf)                   &
     &            BIND(C, name = 'calypso_gzip_defleat_once')
!
        use ISO_C_BINDING
!
        integer(C_int), value :: len_buf
        type(C_ptr), value, intent(in) :: buf
        integer(C_int), value :: len_gzipbuf
        character(C_char), intent(inout) :: gzipbuf(*)
!
        end function calypso_gzip_defleat_once
!  -----------------
        integer(C_int) function calypso_gzip_defleat_begin              &
     &           (len_buf, buf, len_gzipbuf, gzipbuf)                   &
     &            BIND(C, name = 'calypso_gzip_defleat_begin')
!
        use ISO_C_BINDING
!
        integer(C_int), value :: len_buf
        type(C_ptr), value, intent(in) :: buf
        integer(C_int), value :: len_gzipbuf
        character(C_char), intent(inout) :: gzipbuf(*)
!
        end function calypso_gzip_defleat_begin
!  -----------------
        integer(C_int) function  calypso_gzip_defleat_cont              &
     &           (len_buf, buf, len_gzipbuf)                            &
     &            BIND(C, name = 'calypso_gzip_defleat_cont')
!
        use ISO_C_BINDING
!
        integer(C_int), value :: len_buf
        type(C_ptr), value, intent(in) :: buf
        integer(C_int), value :: len_gzipbuf
!
        end function calypso_gzip_defleat_cont
!  -----------------
        integer(C_int) function calypso_gzip_defleat_last               &
     &           (len_buf, buf, len_gzipbuf)                            &
     &            BIND(C, name = 'calypso_gzip_defleat_last')
!
        use ISO_C_BINDING
!
        integer(C_int), value :: len_buf
        type(C_ptr), value, intent(in) :: buf
        integer(C_int), value :: len_gzipbuf
!
        end function calypso_gzip_defleat_last
!  -----------------
        integer(C_int) function calypso_zlib_defleat_once               &
     &           (len_buf, buf, len_gzipbuf, gzipbuf)                   &
     &            BIND(C, name = 'calypso_zlib_defleat_once')
!
        use ISO_C_BINDING
!
        integer(C_int), value :: len_buf
        type(C_ptr), value, intent(in) :: buf
        integer(C_int), value :: len_gzipbuf
        character(C_char), intent(inout) :: gzipbuf(*)
!
        end function calypso_zlib_defleat_once
!  -----------------
        integer(C_int) function calypso_zlib_defleat_begin              &
     &           (len_buf, buf, len_gzipbuf, gzipbuf)                   &
     &            BIND(C, name = 'calypso_zlib_defleat_begin')
!
        use ISO_C_BINDING
!
        integer(C_int), value :: len_buf
        type(C_ptr), value, intent(in) :: buf
        integer(C_int), value :: len_gzipbuf
        character(C_char), intent(inout) :: gzipbuf(*)
!
        end function calypso_zlib_defleat_begin
!  -----------------
!
      end interface
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_real_once                                 &
     &         (num, data, len_gzipbuf, zbuf, gzipbuf)
!
      integer, intent(in) :: num
      real(kind = kreal), target, intent(in) :: data(num)
      integer, intent(in) :: len_gzipbuf
!
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_real_buffer_for_zlib(num, data, zbuf)
      zbuf%len_used = calypso_gzip_defleat_once(zbuf%len_buf,           &
     &                                          C_LOC(zbuf%dat_p(1)),   &
     &                                          zbuf%len_gzipbuf,       &
     &                                          zbuf%gzipbuf_p)
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine gzip_defleat_real_once
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_int8_once                                 &
     &         (num, int8_dat, len_gzipbuf, zbuf, gzipbuf)
!
      integer, intent(in) :: num
      integer(kind = kint_gl), target, intent(in) :: int8_dat(num)
      integer, intent(in) :: len_gzipbuf
!
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_int8_buffer_for_zlib(num, int8_dat, zbuf)
      zbuf%len_used = calypso_gzip_defleat_once(zbuf%len_buf,           &
     &                                          C_LOC(zbuf%idat8_p(1)), &
     &                                          zbuf%len_gzipbuf,       &
     &                                          zbuf%gzipbuf_p)
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine gzip_defleat_int8_once
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_int4_once                                 &
     &         (num, int4_dat, len_gzipbuf, zbuf, gzipbuf)
!
      integer, intent(in) :: num
      integer(kind = 4), target, intent(in) :: int4_dat(num)
      integer, intent(in) :: len_gzipbuf
!
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_int4_buffer_for_zlib(num, int4_dat, zbuf)
      zbuf%len_used = calypso_gzip_defleat_once(zbuf%len_buf,           &
     &                                          C_LOC(zbuf%idat4_p(1)), &
     &                                          zbuf%len_gzipbuf,       &
     &                                          zbuf%gzipbuf_p)
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine gzip_defleat_int4_once
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine zlib_defleat_char_once                                 &
     &         (len_buf, textbuf, len_gzipbuf, zbuf, gzipbuf)
!
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
      integer, intent(in) :: len_gzipbuf
!
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      zbuf%len_used = calypso_zlib_defleat_once(zbuf%len_buf,           &
     &                                          C_LOC(zbuf%buf_p(1)),   &
     &                                          zbuf%len_gzipbuf,       &
     &                                          zbuf%gzipbuf_p)
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine zlib_defleat_char_once
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_char_once                                 &
     &         (len_buf, textbuf, len_gzipbuf, zbuf, gzipbuf)
!
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
      integer, intent(in) :: len_gzipbuf
!
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      zbuf%len_used = calypso_gzip_defleat_once(zbuf%len_buf,           &
     &                                          C_LOC(zbuf%buf_p(1)),   &
     &                                          zbuf%len_gzipbuf,       &
     &                                          zbuf%gzipbuf_p)
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine gzip_defleat_char_once
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_char_begin(len_buf, textbuf,              &
     &                                   len_gzipbuf, zbuf, gzipbuf)
!
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
      integer, intent(in) :: len_gzipbuf
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      zbuf%len_used = calypso_gzip_defleat_begin(zbuf%len_buf,          &
     &                                           C_LOC(zbuf%buf_p(1)),  &
     &                                           zbuf%len_gzipbuf,      &
     &                                           zbuf%gzipbuf_p)
!
      end subroutine gzip_defleat_char_begin
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_char_cont(len_buf, textbuf, zbuf)
!
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      zbuf%len_used = calypso_gzip_defleat_cont(zbuf%len_buf,           &
     &                                          C_LOC(zbuf%buf_p(1)),   &
     &                                          zbuf%len_gzipbuf)
!
      end subroutine gzip_defleat_char_cont
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_char_last(len_buf, textbuf, zbuf)
!
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      zbuf%len_used = calypso_gzip_defleat_last(zbuf%len_buf,           &
     &                                          C_LOC(zbuf%buf_p(1)),   &
     &                                          zbuf%len_gzipbuf)
      call unlink_pointer_for_zlib_buffer(zbuf)
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine gzip_defleat_char_last
!
!  ---------------------------------------------------------------------
!
      end module gzip_defleate
