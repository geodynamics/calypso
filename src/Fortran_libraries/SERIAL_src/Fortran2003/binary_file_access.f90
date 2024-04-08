!>@file   binary_file_access.f90
!!        module binary_file_access
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Wrapper for binary file access routine
!!
!!@verbatim
!!      subroutine open_wt_rawfile_f(file_name)
!!      subroutine open_ad_rawfile_f(file_name)
!!      subroutine open_rd_rawfile_f(file_name)
!!      subroutine close_rawfile_f()
!!
!!      subroutine rawread_real_f(num, data, bbuf)
!!      subroutine rawread_int8_f(num, int8_dat, bbuf)
!!      subroutine rawread_int4_f(num, int4_dat, bbuf)
!!      subroutine rawread_chara_f(len_buf, textbuf, bbuf)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!
!!      subroutine rawwrite_real_f(num, data, bbuf)
!!      subroutine rawwrite_int8_f(num, int8_dat, bbuf)
!!      subroutine rawwrite_int4_f(num, int4_dat, bbuf)
!!      subroutine rawwrite_chara_f(len_buf, textbuf, bbuf)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!@endverbatim
!
      module binary_file_access
!
      use ISO_C_BINDING
      use m_precision
      use t_binary_IO_buffer
!
      implicit none
!
!  -----------------
!
      interface
!
!  -----------------
        subroutine open_wt_rawfile(gz_file_name, ierr)                  &
     &           BIND(C, name = 'open_wt_rawfile')
          use ISO_C_BINDING
!
          character(C_char), intent(in) :: gz_file_name(*)
          integer(C_int), intent(inout) :: ierr
        end subroutine open_wt_rawfile
!  -----------------
        subroutine open_ad_rawfile(gz_file_name, ierr)                  &
     &           BIND(C, name = 'open_ad_rawfile')
        use ISO_C_BINDING
!
          character(C_char), intent(in) :: gz_file_name(*)
          integer(C_int), intent(inout) :: ierr
        end subroutine open_ad_rawfile
!  -----------------
        subroutine open_rd_rawfile(gz_file_name, ierr)                  &
     &           BIND(C, name = 'open_rd_rawfile')
          use ISO_C_BINDING
!
          character(C_char), intent(in) :: gz_file_name(*)
          integer(C_int), intent(inout) :: ierr
        end subroutine open_rd_rawfile
!  -----------------
        subroutine close_rawfile() BIND(C, name = 'close_rawfile')
          use ISO_C_BINDING
        end subroutine close_rawfile
!
!  -----------------
!
        integer(C_long) function  rawread_32bit                         &
     &                (iflag_swap, ilength, buf)                        &
     &                 BIND(C, name = 'rawread_32bit')
          use ISO_C_BINDING
!
          integer(C_int), value :: iflag_swap
          integer(C_long), value :: ilength
          type(C_ptr), value, intent(in) :: buf
        end function rawread_32bit
!  -----------------
        integer(C_long) function rawread_64bit                          &
     &                (iflag_swap, ilength, buf)                        &
     &                 BIND(C, name = 'rawread_64bit')
          use ISO_C_BINDING
!
          integer(C_int), value :: iflag_swap
          integer(C_long), value :: ilength
          type(C_ptr), value, intent(in) :: buf
        end function rawread_64bit
!  -----------------
        integer(C_long) function rawwrite(ilength, buf)                 &
     &            BIND(C, name = 'rawwrite')
          use ISO_C_BINDING
!
          integer(C_long), value :: ilength
          type(C_ptr), value, intent(in) :: buf
        end function rawwrite
!  -----------------
        integer(C_int) function  rawseek_go_fwd(ioffset)                &
     &            BIND(C, name = 'rawseek_go_fwd')
          use ISO_C_BINDING
!
          integer(C_long), value :: ioffset
        end function rawseek_go_fwd
!  -----------------
      end interface
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine open_wt_rawfile_f(file_name, bbuf)
!
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: file_name
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call link_text_buffer_for_bin                                     &
     &   (kchara, add_null_character(file_name), bbuf)
      call open_wt_rawfile(bbuf%buf_p, bbuf%ierr_bin)
      call unlink_text_buffer_for_bin(bbuf)
!
      end subroutine open_wt_rawfile_f
!
!------------------------------------------------------------------
!
      subroutine open_ad_rawfile_f(file_name, bbuf)
!
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: file_name
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call link_text_buffer_for_bin                                     &
     &   (kchara, add_null_character(file_name), bbuf)
      call open_ad_rawfile(bbuf%buf_p, bbuf%ierr_bin)
      call unlink_text_buffer_for_bin(bbuf)
!
      end subroutine open_ad_rawfile_f
!
!------------------------------------------------------------------
!
      subroutine open_rd_rawfile_f(file_name, bbuf)
!
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: file_name
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call link_text_buffer_for_bin                                     &
     &   (kchara, add_null_character(file_name), bbuf)
      call open_rd_rawfile(bbuf%buf_p, bbuf%ierr_bin)
      call unlink_text_buffer_for_bin(bbuf)
!
      end subroutine open_rd_rawfile_f
!
!------------------------------------------------------------------
!
      subroutine close_rawfile_f()
!
!
      call close_rawfile()
!
      end subroutine close_rawfile_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine rawread_real_f(num, data, bbuf)
!
      integer, intent(in) :: num
      real(kind = kreal), target, intent(inout) :: data(num)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call link_real_buffer_for_bin(num, data, bbuf)
      bbuf%len_used = rawread_64bit(bbuf%iflag_swap, bbuf%len_buf,      &
     &                              C_LOC(bbuf%dat_p(1)))
      call unlink_real_buffer_for_bin(bbuf)
!
      end subroutine rawread_real_f
!
!  ---------------------------------------------------------------------
!
      subroutine rawread_int8_f(num, int8_dat, bbuf)
!
      integer, intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call link_int8_buffer_for_bin(num, int8_dat, bbuf)
      bbuf%len_used = rawread_64bit(bbuf%iflag_swap, bbuf%len_buf,      &
     &                              C_LOC(bbuf%idat8_p(1)))
      call unlink_int8_buffer_for_bin(bbuf)
!
      end subroutine rawread_int8_f
!
!  ---------------------------------------------------------------------
!
      subroutine rawread_int4_f(num, int4_dat, bbuf)
!
      integer, intent(in) :: num
      integer, target, intent(inout) :: int4_dat(num)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call link_int4_buffer_for_bin(num, int4_dat, bbuf)
      bbuf%len_used = rawread_32bit(bbuf%iflag_swap, bbuf%len_buf,      &
     &                              C_LOC(bbuf%idat4_p(1)))
      call unlink_int4_buffer_for_bin(bbuf)
      bbuf%ierr_bin = int(bbuf%len_buf - bbuf%len_used)
!
      end subroutine rawread_int4_f
!
!  ---------------------------------------------------------------------
!
      subroutine rawread_chara_f(len_buf, textbuf, bbuf)
!
      use m_machine_parameter
!
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(C_int), parameter :: iflag_noswap = iendian_KEEP
!
      call link_text_buffer_for_bin(len_buf, textbuf, bbuf)
      bbuf%len_used = rawread_32bit(iflag_noswap, bbuf%len_buf,         &
     &                              C_LOC(bbuf%buf_p(1)))
      call unlink_text_buffer_for_bin(bbuf)
      bbuf%ierr_bin = int(bbuf%len_buf - bbuf%len_used)
!
      end subroutine rawread_chara_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine rawwrite_real_f(num, data, bbuf)
!
      integer, intent(in) :: num
      real(kind = kreal), target, intent(in) :: data(num)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call link_real_buffer_for_bin(num, data, bbuf)
      bbuf%len_used = rawwrite(bbuf%len_buf, C_LOC(bbuf%dat_p(1)))
      call unlink_real_buffer_for_bin(bbuf)
      bbuf%ierr_bin = int(bbuf%len_buf - bbuf%len_used)
!
      end subroutine rawwrite_real_f
!
!  ---------------------------------------------------------------------
!
      subroutine rawwrite_int8_f(num, int8_dat, bbuf)
!
      integer, intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call link_int8_buffer_for_bin(num, int8_dat, bbuf)
      bbuf%len_used =  rawwrite(bbuf%len_buf, C_LOC(bbuf%idat8_p(1)))
      call unlink_int8_buffer_for_bin(bbuf)
      bbuf%ierr_bin = int(bbuf%len_buf - bbuf%len_used)
!
      end subroutine rawwrite_int8_f
!
!  ---------------------------------------------------------------------
!
      subroutine rawwrite_int4_f(num, int4_dat, bbuf)
!
      integer, intent(in) :: num
      integer(kind = 4), target, intent(in) :: int4_dat(num)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call link_int4_buffer_for_bin(num, int4_dat, bbuf)
      bbuf%len_used = rawwrite(bbuf%len_buf, C_LOC(bbuf%idat4_p(1)))
      call unlink_int4_buffer_for_bin(bbuf)
      bbuf%ierr_bin = int(bbuf%len_buf - bbuf%len_used)
!
      end subroutine rawwrite_int4_f
!
!  ---------------------------------------------------------------------
!
      subroutine rawwrite_chara_f(len_buf, textbuf, bbuf)
!
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call link_text_buffer_for_bin(len_buf, textbuf, bbuf)
      bbuf%len_used = rawwrite(bbuf%len_buf, C_LOC(bbuf%buf_p(1)))
      call unlink_text_buffer_for_bin(bbuf)
      bbuf%ierr_bin = int(bbuf%len_buf - bbuf%len_used)
!
      end subroutine rawwrite_chara_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine rawseek_go_fwd_f(ioffset, bbuf)
!
      integer, intent(in) :: ioffset
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      bbuf%len_buf = int(ioffset,KIND(bbuf%len_buf))
      bbuf%ierr_bin = rawseek_go_fwd(bbuf%len_buf)
!
      end subroutine rawseek_go_fwd_f
!
!  ---------------------------------------------------------------------
!
      end module binary_file_access
