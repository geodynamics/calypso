!>@file   select_gz_stream_file_IO.F90
!!@brief  module select_gz_stream_file_IO
!!
!!@authorH.Matsui and H.Okuda
!!@date Programmed  H. Matsui in  Sep. 2022
!
!>@brief subroutines to find comment lines in data
!!
!!@verbatim
!!      subroutine sel_open_read_gz_stream_file                         &
!!     &         (FPz_f, id_file, file_name, flag_gzip, zbuf)
!!      subroutine sel_close_read_gz_stream_file                        &
!!     &         (FPz_f, id_file, flag_gzip, zbuf)
!!        character, pointer, intent(inout) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: file_name
!!        logical, intent(inout) :: flag_gzip
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine sel_read_line_gz_stream                              &
!!     &         (FPz_f, id_file, flag_gzip, zbuf)
!!      subroutine sel_skip_comment_gz_stream(FPz_f, id_file,           &
!!     &                                      flag_gzip, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_file
!!        logical, intent(in) :: flag_gzip
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine sel_gz_write_text_stream(flag_gzip, id_file,         &
!!     &                                    textbuf, zbuf)
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len = *), intent(in) :: textbuf
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module select_gz_stream_file_IO
!
      use m_precision
      use t_buffer_4_gzip
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_open_read_gz_stream_file                           &
     &         (FPz_f, id_file, file_name, flag_gzip, zbuf)
!
      use set_parallel_file_name
      use skip_comment_f
      use gzip_file_access
!
      character, pointer, intent(inout) :: FPz_f
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: file_name
      logical, intent(inout) :: flag_gzip
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      character(len=kchara) :: prefix, extension
!
!
      call alloc_fixbuffer_for_zlib(zbuf)
      call split_extrension(file_name, prefix, extension)
!
      flag_gzip = .FALSE.
!#ifdef ZLIB_IO
      if(cmp_no_case(extension, gz_ext)) flag_gzip = .TRUE.
      if(flag_gzip) then
        write(*,*) 'read gzipped monitor file: ', trim(file_name)
        call open_rd_gzfile_f(FPz_f, file_name, zbuf)
        return
      end if
!#endif
!
      write(*,*) 'read ASCII file as stream: ', trim(file_name)
      open(id_file, file = file_name,  status='old',                    &
     &     FORM='UNFORMATTED', ACCESS='STREAM')
!
      end subroutine sel_open_read_gz_stream_file
!
!   --------------------------------------------------------------------
!
      subroutine sel_close_read_gz_stream_file(FPz_f, id_file,          &
     &                                         flag_gzip, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(inout) :: FPz_f
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_gzip
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!#ifdef ZLIB_IO
      if(flag_gzip) then
        call close_gzfile_b(FPz_f)
        go to 10
      end if
!#endif
!
      close(id_file)
  10  continue
      call dealloc_fixbuffer_for_zlib(zbuf)
!
      end subroutine sel_close_read_gz_stream_file
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine sel_read_line_gz_stream                                &
     &         (FPz_f, id_file, flag_gzip, zbuf)
!
      use skip_comment_f
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_gzip
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
!#ifdef ZLIB_IO
      if(flag_gzip) then
        call get_one_line_text_from_gz(FPz_f, zbuf)
        if(check_gzfile_eof(FPz_f) .ne. 0) zbuf%len_used = -1
        return
      end if
!#endif
!
      call read_one_line_from_stream(id_file, len(zbuf%fixbuf(1)),      &
     &    zbuf%num_word, zbuf%len_used, zbuf%fixbuf(1))
      zbuf%fixbuf(1)(zbuf%len_used:zbuf%len_used) = char(32)
!
      end subroutine sel_read_line_gz_stream
!
!   --------------------------------------------------------------------
!
      subroutine sel_skip_comment_gz_stream(FPz_f, id_file,             &
     &                                      flag_gzip, zbuf)
!
      use skip_comment_f
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_gzip
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
!#ifdef ZLIB_IO
      if(flag_gzip) then
        call skip_gz_comment_get_nword(FPz_f, zbuf)
        return
      end if
!#endif
!
      call skip_comment_from_stream(id_file, len(zbuf%fixbuf(1)),       &
     &    zbuf%num_word, zbuf%len_used, zbuf%fixbuf(1))
      zbuf%fixbuf(1)(zbuf%len_used:zbuf%len_used) = char(32)
!
      end subroutine sel_skip_comment_gz_stream
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_gz_write_text_stream(flag_gzip, id_file,           &
     &                                    textbuf, zbuf)
!
      use data_convert_by_zlib
      use transfer_to_long_integers
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file
      character(len = *), intent(in) :: textbuf
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
!#ifdef ZLIB_IO
      if(flag_gzip) then
        call gzip_defleate_characters_b(cast_long(len(textbuf)),        &
     &                                  textbuf, zbuf)
        write(id_file) zbuf%gzip_buf(1:zbuf%ilen_gzipped)
        call dealloc_zip_buffer(zbuf)
        return
      end if
!#endif
!
      write(id_file) textbuf
!
      end subroutine sel_gz_write_text_stream
!
! -----------------------------------------------------------------------
!
      end module select_gz_stream_file_IO
