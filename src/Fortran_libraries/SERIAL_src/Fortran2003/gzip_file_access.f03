!>@file   gzip_file_access.f90
!!        module gzip_file_access
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Wrapper for decompression routines by zlib
!!
!!@verbatim
!!      subroutine open_wt_gzfile_f(gzip_name, zbuf)
!!      subroutine open_ad_gzfile_f(gzip_name, zbuf)
!!      subroutine open_rd_gzfile_f(gzip_name, zbuf)
!!      subroutine close_gzfile_b()
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine get_one_line_text_from_gz(len_buf, textbuf, zbuf)
!!      subroutine gz_write_textbuf_no_lf(len_buf, textbuf, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gzread_real_f(num, data, zbuf)
!!      subroutine gzread_int8_f(num, int8_dat, zbuf)
!!      subroutine gzread_int4_f(num, int4_dat, zbuf)
!!      subroutine gzread_chara_f(len_buf, textbuf, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gzwrite_real_f(num, data, zbuf)
!!      subroutine gzwrite_int8_f(num, int8_dat, zbuf)
!!      subroutine gzwrite_int4_f(num, int4_dat, zbuf)
!!      subroutine gzwrite_chara_f(len_buf, textbuf, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module gzip_file_access
!
      use ISO_C_BINDING
      use m_precision
      use t_buffer_4_gzip
!
      implicit none
!
!  -----------------
!
      interface
!
!  -----------------
        subroutine open_wt_gzfile(gz_file_name)                         &
     &           BIND(C, name = 'open_wt_gzfile')
          use ISO_C_BINDING
!
          character(C_char), intent(in) :: gz_file_name(*)
        end subroutine open_wt_gzfile
!  -----------------
        subroutine open_ad_gzfile(gz_file_name)                         &
     &           BIND(C, name = 'open_ad_gzfile')
        use ISO_C_BINDING
!
          character(C_char), intent(in) :: gz_file_name(*)
        end subroutine open_ad_gzfile
!  -----------------
        subroutine open_rd_gzfile(gz_file_name)                         &
     &           BIND(C, name = 'open_rd_gzfile')
          use ISO_C_BINDING
!
          character(C_char), intent(in) :: gz_file_name(*)
        end subroutine open_rd_gzfile
!  -----------------
        subroutine close_gzfile() BIND(C, name = 'close_gzfile')
          use ISO_C_BINDING
        end subroutine close_gzfile
!  -----------------
        subroutine get_one_line_from_gz                                 &
     &           (num_buffer, num_word, nchara, line_buf)               &
     &            BIND(C, name = 'get_one_line_from_gz')
          use ISO_C_BINDING
!
          integer(C_int), intent(in) :: num_buffer
          integer(C_int), intent(inout) :: num_word
          integer(C_int), intent(inout) :: nchara
          type(C_ptr), value :: line_buf
        end subroutine get_one_line_from_gz
!  -----------------
        subroutine write_compress_txt_nolf(nchara, line_buf)            &
     &            BIND(C, name = 'write_compress_txt_nolf')
          use ISO_C_BINDING
!
          integer(C_int), intent(in) :: nchara
          type(C_ptr), value :: line_buf
        end subroutine write_compress_txt_nolf
!
!  -----------------
!
        subroutine gzread_32bit_f(iflag_swap, ilength, buf, ierr)       &
     &            BIND(C, name = 'gzread_32bit_f')
          use ISO_C_BINDING
!
          integer(C_int), intent(in) :: iflag_swap, ilength
          type(C_ptr), value, intent(in) :: buf
          integer(C_int), intent(inout) :: ierr
        end subroutine gzread_32bit_f
!  -----------------
        subroutine gzread_64bit_f(iflag_swap, ilength, buf, ierr)       &
     &            BIND(C, name = 'gzread_64bit_f')
          use ISO_C_BINDING
!
          integer(C_int), intent(in) :: iflag_swap, ilength
          type(C_ptr), value, intent(in) :: buf
          integer(C_int), intent(inout) :: ierr
        end subroutine gzread_64bit_f
!  -----------------
        subroutine gzwrite_f(ilength, buf, ierr)                        &
     &            BIND(C, name = 'gzwrite_f')
          use ISO_C_BINDING
!
          integer(C_int), intent(in) :: ilength
          type(C_ptr), value, intent(in) :: buf
          integer(C_int), intent(inout) :: ierr
        end subroutine gzwrite_f
!  -----------------
      end interface
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine open_wt_gzfile_f(gzip_name, zbuf)
!
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: gzip_name
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib                                    &
     &   (kchara, add_null_character(gzip_name), zbuf)
      call open_wt_gzfile(zbuf%buf_p)
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine open_wt_gzfile_f
!
!------------------------------------------------------------------
!
      subroutine open_ad_gzfile_f(gzip_name, zbuf)
!
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: gzip_name
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib                                    &
     &   (kchara, add_null_character(gzip_name), zbuf)
      call open_ad_gzfile(zbuf%buf_p)
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine open_ad_gzfile_f
!
!------------------------------------------------------------------
!
      subroutine open_rd_gzfile_f(gzip_name, zbuf)
!
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: gzip_name
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib                                    &
     &   (kchara, add_null_character(gzip_name), zbuf)
      call open_rd_gzfile(zbuf%buf_p)
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine open_rd_gzfile_f
!
!------------------------------------------------------------------
!
      subroutine close_gzfile_b()
!
!
      call close_gzfile()
!
      end subroutine close_gzfile_b
!
!------------------------------------------------------------------
!
      subroutine get_one_line_text_from_gz(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib                                    &
     &   (len(zbuf%fixbuf(1)), zbuf%fixbuf(1), zbuf)
      call get_one_line_from_gz(zbuf%len_buf, zbuf%num_word,            &
     &    zbuf%len_used, C_LOC(zbuf%buf_p))
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine get_one_line_text_from_gz
!
!  ---------------------------------------------------------------------
!
      subroutine gz_write_textbuf_no_lf(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib                                    &
     &   (len(zbuf%fixbuf(1)), zbuf%fixbuf(1), zbuf)
      call write_compress_txt_nolf(zbuf%len_buf, C_LOC(zbuf%buf_p))
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine gz_write_textbuf_no_lf
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gzread_real_f(num, data, zbuf)
!
      integer, intent(in) :: num
      real(kind = kreal), target, intent(inout) :: data(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_real_buffer_for_zlib(num, data, zbuf)
      call gzread_64bit_f(zbuf%iflag_swap,                              &
     &    zbuf%len_buf, C_LOC(zbuf%dat_p), zbuf%ierr_zlib)
      call unlink_real_buffer_for_zlib(zbuf)
!
      end subroutine gzread_real_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzread_int8_f(num, int8_dat, zbuf)
!
      integer, intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_int8_buffer_for_zlib(num, int8_dat, zbuf)
      call gzread_64bit_f(zbuf%iflag_swap,                              &
     &    zbuf%len_buf , C_LOC(zbuf%idat8_p), zbuf%ierr_zlib)
      call unlink_int8_buffer_for_zlib(zbuf)
!
      end subroutine gzread_int8_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzread_int4_f(num, int4_dat, zbuf)
!
      integer, intent(in) :: num
      integer, target, intent(inout) :: int4_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_int4_buffer_for_zlib(num, int4_dat, zbuf)
      call gzread_32bit_f(zbuf%iflag_swap,                              &
     &    zbuf%len_buf, C_LOC(zbuf%idat4_p), zbuf%ierr_zlib)
      call unlink_int4_buffer_for_zlib(zbuf)
!
      end subroutine gzread_int4_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzread_chara_f(len_buf, textbuf, zbuf)
!
      use m_machine_parameter
!
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(C_int), parameter :: iflag_noswap = iendian_KEEP
!
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call gzread_32bit_f(iflag_noswap,                                 &
     &    zbuf%len_buf, C_LOC(zbuf%buf_p), zbuf%ierr_zlib)
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine gzread_chara_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gzwrite_real_f(num, data, zbuf)
!
      integer, intent(in) :: num
      real(kind = kreal), target, intent(in) :: data(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_real_buffer_for_zlib(num, data, zbuf)
      call gzwrite_f(zbuf%len_buf , C_LOC(zbuf%dat_p), zbuf%ierr_zlib)
      call unlink_real_buffer_for_zlib(zbuf)
!
      end subroutine gzwrite_real_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzwrite_int8_f(num, int8_dat, zbuf)
!
      integer, intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_int8_buffer_for_zlib(num, int8_dat, zbuf)
      call gzwrite_f(zbuf%len_buf, C_LOC(zbuf%idat8_p), zbuf%ierr_zlib)
      call unlink_int8_buffer_for_zlib(zbuf)
!
      end subroutine gzwrite_int8_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzwrite_int4_f(num, int4_dat, zbuf)
!
      integer, intent(in) :: num
      integer(kind = 4), target, intent(in) :: int4_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_int4_buffer_for_zlib(num, int4_dat, zbuf)
      call gzwrite_f(zbuf%len_buf, C_LOC(zbuf%idat4_p), zbuf%ierr_zlib)
      call unlink_int4_buffer_for_zlib(zbuf)
!
      end subroutine gzwrite_int4_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzwrite_chara_f(len_buf, textbuf, zbuf)
!
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call gzwrite_f(zbuf%len_buf , C_LOC(zbuf%buf_p), zbuf%ierr_zlib)
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine gzwrite_chara_f
!
!  ---------------------------------------------------------------------
!
      end module gzip_file_access
