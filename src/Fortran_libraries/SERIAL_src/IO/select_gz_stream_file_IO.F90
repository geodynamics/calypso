!>@file   select_gz_stream_file_IO.F90
!!@brief  module select_gz_stream_file_IO
!!
!!@authorH.Matsui and H.Okuda
!!@date Programmed  H. Matsui in  Sep. 2022
!
!>@brief subroutines to find comment lines in data
!!
!!@verbatim
!!      subroutine check_gzip_or_ascii_file                             &
!!     &         (base_name, file_name, flag_gzip_lc, flag_miss)
!!        character(len = kchara), intent(in) :: base_name
!!        character(len = kchara), intent(inout) :: file_name
!!        logical, intent(inout) :: flag_gzip_lc, flag_miss
!!
!!      subroutine sel_open_read_gz_stream_file                         &
!!     &         (FPz_f, id_file, base_name, flag_gzip, zbuf)
!!      subroutine sel_open_check_gz_stream_file(FPz_f, id_file,        &
!!     &          base_name, flag_gzip, flag_miss, file_name, zbuf)
!!      subroutine sel_close_read_gz_stream_file                        &
!!     &         (FPz_f, id_file, flag_gzip, zbuf)
!!      subroutine sel_redwind_gz_stream_file(FPz_f, id_file, flag_gzip)
!!        character, pointer, intent(inout) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: base_name
!!        logical, intent(inout) :: flag_gzip, flag_miss
!!        character(len=kchara), intent(inout) :: file_name
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
!!      subroutine sel_gz_write_text_stream_w_len(flag_gzip, id_file,   &
!!     &                                          len64, textbuf, zbuf)
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint_gl), intent(in) :: len64
!!        character(len = len64), intent(in) :: textbuf
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
      subroutine check_gzip_or_ascii_file                               &
     &         (base_name, file_name, flag_gzip_lc, flag_miss)
!
      use set_parallel_file_name
      use delete_data_files
!
      character(len = kchara), intent(in) :: base_name
      character(len = kchara), intent(inout) :: file_name
      logical, intent(inout) :: flag_gzip_lc, flag_miss
!
      character(len = kchara) :: gzip_name
!
      gzip_name = add_gzip_extension(base_name)
      flag_miss = .TRUE.
      if(check_file_exist(base_name)) then
        flag_miss =    .FALSE.
        flag_gzip_lc = .FALSE.
      else if(check_file_exist(gzip_name)) then
        flag_miss =    .FALSE.
        flag_gzip_lc = .TRUE.
      end if
!
      if(flag_gzip_lc) then
        file_name = gzip_name
      else
        file_name = base_name
      end if
!
      end subroutine check_gzip_or_ascii_file
!
! -----------------------------------------------------------------------
!
      subroutine sel_open_read_gz_stream_file                           &
     &         (FPz_f, id_file, base_name, flag_gzip, zbuf)
!
      use set_parallel_file_name
      use skip_comment_f
      use gzip_file_access
!
      character, pointer, intent(inout) :: FPz_f
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: base_name
!
      logical, intent(inout) :: flag_gzip
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      character(len=kchara) :: file_name
      logical :: flag_miss
!
!
      call sel_open_check_gz_stream_file(FPz_f, id_file, base_name,     &
     &    flag_gzip, flag_miss, file_name, zbuf)
      if(flag_miss) then
        write(*,*) trim(file_name), ' is not found. Stop.'
        stop
      end if
!
      end subroutine sel_open_read_gz_stream_file
!
!   --------------------------------------------------------------------
!
      subroutine sel_open_check_gz_stream_file(FPz_f, id_file,          &
     &          base_name, flag_gzip, flag_miss, file_name, zbuf)
!
      use set_parallel_file_name
      use skip_comment_f
      use gzip_file_access
!
      character, pointer, intent(inout) :: FPz_f
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: base_name
!
      logical, intent(inout) :: flag_gzip, flag_miss
      character(len=kchara), intent(inout) :: file_name
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call check_gzip_or_ascii_file                                     &
     &   (base_name, file_name, flag_gzip, flag_miss)
!      write(*,*) 'check_gzip_or_ascii_file', flag_gzip
      if(flag_miss) return
!
      call alloc_fixbuffer_for_zlib(zbuf)
!
!#ifdef ZLIB_IO
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
      end subroutine sel_open_check_gz_stream_file
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
!
      subroutine sel_redwind_gz_stream_file(FPz_f, id_file, flag_gzip)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_gzip
!
      integer(kind = kint) :: ierr
!
!#ifdef ZLIB_IO
      if(flag_gzip) then
        ierr = rewind_gzfile(FPz_f)
        return
      end if
!#endif
!
      rewind(id_file)
!
      end subroutine sel_redwind_gz_stream_file
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
      if(zbuf%len_used .le. 0) return
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
        call skip_gz_comment_get_nword(FPz_f, zbuf)
        if(check_gzfile_eof(FPz_f) .ne. 0) zbuf%len_used = -1
        return
      end if
!#endif
!
      call skip_comment_from_stream(id_file, len(zbuf%fixbuf(1)),       &
     &    zbuf%num_word, zbuf%len_used, zbuf%fixbuf(1))
      if(zbuf%len_used .le. 0) return
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
      subroutine sel_gz_write_text_stream_w_len(flag_gzip, id_file,     &
     &                                          len64, textbuf, zbuf)
!
      use data_convert_by_zlib
      use transfer_to_long_integers
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint_gl), intent(in) :: len64
      character(len = len64), intent(in) :: textbuf
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
!#ifdef ZLIB_IO
      if(flag_gzip) then
        call gzip_defleate_characters_b(len64, textbuf, zbuf)
        write(id_file) zbuf%gzip_buf(1:zbuf%ilen_gzipped)
        call dealloc_zip_buffer(zbuf)
        return
      end if
!#endif
!
      write(id_file) textbuf(1:len64)
!
      end subroutine sel_gz_write_text_stream_w_len
!
! -----------------------------------------------------------------------
!
      end module select_gz_stream_file_IO
