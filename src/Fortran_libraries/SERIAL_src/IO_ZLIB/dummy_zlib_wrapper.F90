!
#ifndef ZLIB_IO
! -----------------------------------------------------------------------
!
!void gzip_defleat_once(int *len_buf, const char *buf, int *len_gzipbuf,
!                       int *len_gzipped, char *gzipbuf);
      subroutine gzip_defleat_once                                      &
     &         (len_buf, buf, len_gzipbuf, len_gzipped, gzipbuf)
!
      use m_precision
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: len_buf
      character(len = *), intent(in) :: gzipbuf
      character(len = *), intent(in) :: buf
      integer, intent(in) :: len_gzipped
!
      return
      end subroutine gzip_defleat_once
!
! -----------------------------------------------------------------------
!
!void gzip_defleat_begin(int *len_buf, const char *buf, int *len_gzipbuf,
!                        int *len_gzipped, char *gzipbuf);
      subroutine gzip_defleat_begin                                     &
     &         (len_buf, buf, len_gzipbuf, len_gzipped, gzipbuf)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: len_buf
      character(len = *), intent(in) :: gzipbuf
      character(len = *), intent(in) :: buf
      integer, intent(in) :: len_gzipped
!
      return
      end subroutine gzip_defleat_begin
!
! -----------------------------------------------------------------------
!
!void gzip_defleat_cont(int *len_buf, const char *buf, int *len_gzipbuf, int *len_gzipped);
      subroutine gzip_defleat_cont                                      &
     &         ( len_buf, buf, len_gzipbuf, len_gzipped)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: len_buf
      character(len = *), intent(in) :: buf
      integer, intent(in) :: len_gzipped
!
      return
      end subroutine gzip_defleat_cont
!
! -----------------------------------------------------------------------
!
!void gzip_defleat_last(int *len_buf, const char *buf, int *len_gzipbuf, int *len_gzipped);
      subroutine gzip_defleat_last                                      &
     &         ( len_buf, buf, len_gzipbuf, len_gzipped)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: len_buf
      character(len = *), intent(in) :: buf
      integer, intent(in) :: len_gzipped
!
      return
      end subroutine gzip_defleat_last
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
!void gzip_infleat_once(int *len_gzipbuf, const char *gzipbuf, int *len_buf, 
!                       char *buf, int *len_gzipped);
      subroutine gzip_infleat_once                                      &
     &         (len_gzipbuf, gzipbuf, len_buf, buf, len_gzipped)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: len_buf
      character(len = len_buf), intent(in) :: gzipbuf
      character(len = *), intent(in) :: buf
      integer, intent(in) :: len_gzipped
!
      return
      end subroutine gzip_infleat_once
!
! -----------------------------------------------------------------------
!
!void gzip_infleat_begin(int *len_gzipbuf, const char *gzipbuf, int *len_buf, 
!                        char *buf, int *len_gzipped);
      subroutine gzip_infleat_begin                                     &
     &         (len_gzipbuf, gzipbuf, len_buf, buf, len_gzipped)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: len_buf
      character(len = *), intent(in) :: gzipbuf
      character(len = *), intent(in) :: buf
      integer, intent(in) :: len_gzipped
!
      return
      end subroutine gzip_infleat_begin
!
! -----------------------------------------------------------------------
!
!void gzip_infleat_cont(int *len_gzipbuf, int *len_buf, const char *buf, int *len_gzipped);
      subroutine gzip_infleat_cont                                      &
     &         (len_gzipbuf, len_buf, buf, len_gzipped)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: len_buf
      character(len = *), intent(in) :: buf
      integer, intent(in) :: len_gzipped
!
      return
      end subroutine gzip_infleat_cont
!
! -----------------------------------------------------------------------
!
!void gzip_infleat_last(int *len_gzipbuf, int *len_buf, const char *buf, int *len_gzipped);
      subroutine gzip_infleat_last                                      &
     &         (len_gzipbuf, len_buf, buf, len_gzipped)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: len_buf
      character(len = *), intent(in) :: buf
      integer, intent(in) :: len_gzipped
!
      return
      end subroutine gzip_infleat_last
!
! -----------------------------------------------------------------------
!
#endif
