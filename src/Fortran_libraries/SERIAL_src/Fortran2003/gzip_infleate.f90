!>@file   gzip_infleate.f90
!!        module gzip_infleate
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Wrapper for decompression routines by zlib
!!
!!@verbatim
!!      subroutine gzip_infleat_real_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, data, zbuf)
!!      subroutine gzip_infleat_int8_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, int8_dat, zbuf)
!!      subroutine gzip_infleat_int4_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, int4_dat, zbuf)
!!      subroutine gzip_infleat_char_once                               &
!!     &         (len_gzipbuf, gzipbuf, len_buf, buf, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gzip_infleat_char_begin(zbuf)
!!      subroutine gzip_infleat_char_cont(zbuf)
!!      subroutine gzip_infleat_char_last(zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module gzip_infleate
!
      use ISO_C_BINDING
      use m_precision
      use t_buffer_4_gzip
!
      implicit none
!
!  -----------------
      interface
!  -----------------
      integer(C_int) function calypso_gzip_infleat_once                 &
     &           (len_gzipbuf, gzipbuf, len_buf, buf)                   &
     &            BIND(C, name = 'calypso_gzip_infleat_once')
!
        use ISO_C_BINDING
!
        integer(C_int), value :: len_gzipbuf
        character(C_char), intent(in) :: gzipbuf(*)
        integer(C_int), value :: len_buf
!
        type(C_ptr), value :: buf
!
        end function calypso_gzip_infleat_once
!  -----------------
        integer(C_int) function calypso_gzip_infleat_begin              &
     &           (len_gzipbuf, gzipbuf, len_buf, buf)                   &
     &            BIND(C, name = 'calypso_gzip_infleat_begin')
!
        use ISO_C_BINDING
!
        integer(C_int), value :: len_gzipbuf
        character(C_char), intent(in) :: gzipbuf(*)
        integer(C_int), value :: len_buf
!
        type(C_ptr), value :: buf
!
        end function calypso_gzip_infleat_begin
!  -----------------
        integer(C_int) function calypso_gzip_infleat_cont               &
     &           (len_gzipbuf, len_buf, buf)                            &
     &            BIND(C, name = 'calypso_gzip_infleat_cont')
!
        use ISO_C_BINDING
!
        integer(C_int), value :: len_gzipbuf, len_buf
        type(C_ptr), value :: buf
!
        end function calypso_gzip_infleat_cont
!  -----------------
        integer(C_int) function  calypso_gzip_infleat_last              &
     &           (len_gzipbuf, len_buf, buf)                            &
     &            BIND(C, name = 'calypso_gzip_infleat_last')
!
        use ISO_C_BINDING
!
        integer(C_int), value :: len_gzipbuf, len_buf
        type(C_ptr), value :: buf
!
        end function calypso_gzip_infleat_last
!  -----------------
        integer(C_int) function calypso_zlib_infleat_once               &
     &           (len_gzipbuf, gzipbuf, len_buf, buf)                   &
     &            BIND(C, name = 'calypso_zlib_infleat_once')
!
        use ISO_C_BINDING
!
        integer(C_int), value :: len_gzipbuf
        character(C_char), intent(in) :: gzipbuf(*)
        integer(C_int), value :: len_buf
!
        type(C_ptr), value, intent(in) :: buf
!
        end function calypso_zlib_infleat_once
!  -----------------
        integer(C_int) function calypso_zlib_infleat_begin              &
     &           (len_gzipbuf, gzipbuf, len_buf, buf)                   &
     &            BIND(C, name = 'calypso_zlib_infleat_begin')
!
        use ISO_C_BINDING
!
        integer(C_int), value :: len_gzipbuf
        character(C_char), intent(in) :: gzipbuf(*)
        integer(C_int), value :: len_buf
!
        type(C_ptr), value, intent(in) :: buf
!
        end function calypso_zlib_infleat_begin
!  ---------------------------------------------------------------------
      end interface
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_real_once                                 &
     &         (len_gzipbuf, gzipbuf, num, data, zbuf)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: num
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
!
      real(kind = kreal), target, intent(inout) :: data(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_real_buffer_for_zlib(num, data, zbuf)
      zbuf%len_used = calypso_gzip_infleat_once(zbuf%len_gzipbuf,       &
     &                                          zbuf%gzipbuf_p,         &
     &                                          zbuf%len_buf,           &
     &                                          C_LOC(zbuf%dat_p(1)))
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine gzip_infleat_real_once
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_int8_once                                 &
     &         (len_gzipbuf, gzipbuf, num, int8_dat, zbuf)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: num
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
!
      integer(kind = kint_gl), target, intent(inout) :: int8_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_int8_buffer_for_zlib(num, int8_dat, zbuf)
      zbuf%len_used = calypso_gzip_infleat_once(zbuf%len_gzipbuf,       &
     &                                          zbuf%gzipbuf_p,         &
     &                                          zbuf%len_buf,           &
     &                                          C_LOC(zbuf%idat8_p(1)))
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine gzip_infleat_int8_once
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_int4_once                                 &
     &         (len_gzipbuf, gzipbuf, num, int4_dat, zbuf)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: num
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
!
      integer(kind = 4), target, intent(inout) :: int4_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_int4_buffer_for_zlib(num, int4_dat, zbuf)
      zbuf%len_used = calypso_gzip_infleat_once(zbuf%len_gzipbuf,       &
     &                                          zbuf%gzipbuf_p,         &
     &                                          zbuf%len_buf,           &
     &                                          C_LOC(zbuf%idat4_p(1)))
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine gzip_infleat_int4_once
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_char_once                                 &
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
      call link_pointer_for_zlib_buffer                                 &
     &   (len_gzipbuf, gzipbuf, len_buf, textbuf, zbuf)
      zbuf%len_used = calypso_gzip_infleat_once(zbuf%len_gzipbuf,       &
     &                                          zbuf%gzipbuf_p,         &
     &                                          zbuf%len_buf,           &
     &                                          C_LOC(zbuf%buf_p(1)))
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine gzip_infleat_char_once
!
!  ---------------------------------------------------------------------
!
      subroutine zlib_infleat_char_once                                 &
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
      call link_pointer_for_zlib_buffer                                 &
     &   (len_gzipbuf, gzipbuf, len_buf, textbuf, zbuf)
      zbuf%len_used = calypso_zlib_infleat_once(zbuf%len_gzipbuf,       &
     &                                          zbuf%gzipbuf_p,         &
     &                                          zbuf%len_buf,           &
     &                                          C_LOC(zbuf%buf_p(1)))
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine zlib_infleat_char_once
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_char_begin(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%len_used = calypso_gzip_infleat_begin(zbuf%len_gzipbuf,      &
     &                                           zbuf%gzipbuf_p,        &
     &                                           zbuf%len_buf,          &
     &                                           C_LOC(zbuf%buf_p(1)))
!
      end subroutine gzip_infleat_char_begin
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_char_cont(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      zbuf%len_used = calypso_gzip_infleat_cont(zbuf%len_gzipbuf,       &
     &                                          zbuf%len_buf,           &
     &                                          C_LOC(zbuf%buf_p(1)))
!
      end subroutine gzip_infleat_char_cont
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_char_last(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%len_used = calypso_gzip_infleat_last(zbuf%len_gzipbuf,       &
     &                                          zbuf%len_buf,           &
     &                                          C_LOC(zbuf%buf_p(1)))
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine gzip_infleat_char_last
!
!  ---------------------------------------------------------------------
!
      end module gzip_infleate
