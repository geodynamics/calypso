!>@file  t_binary_IO_buffer.f90
!!       module t_binary_IO_buffer
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!!        Modified in March, 2020
!!
!> @brief Buffer and pointers for binary IO data IO
!!
!!@verbatim
!!      subroutine link_real_buffer_for_bin(num, data, bbuf)
!!      subroutine link_int8_buffer_for_bin(num, int8_dat, bbuf)
!!      subroutine link_int4_buffer_for_bin(num, int4_dat, bbuf)
!!      subroutine link_text_buffer_for_bin(len_buf, textbuf, bbuf)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!
!!      subroutine unlink_real_buffer_for_bin(bbuf)
!!      subroutine unlink_int8_buffer_for_bin(bbuf)
!!      subroutine unlink_int4_buffer_for_bin(bbuf)
!!      subroutine unlink_text_buffer_for_bin(bbuf)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!@endverbatim
!
      module t_binary_IO_buffer
!
      use ISO_C_BINDING
      use m_precision
!
      implicit none
!
        integer(kind = kint), parameter, private :: id_bin = 19
!
!>      Structure of buffer for binary IO
      type binary_IO_buffer
!>        Binary file ID
        integer(kind = kint) :: id_binary = id_bin
!
!>        Error flag from binary IO
        integer(C_int) :: ierr_bin
!>        Byte swapping flag to binary IO
        integer(C_int) :: iflag_swap
!
!>        Size of decompressed buffer to binary IO
        integer(C_long) :: len_buf
!>        Actual size of compressed buffer to binary IO
        integer(C_long) :: len_used
!
!>        Pointer of decompressed text buffer
        character(C_char), pointer :: buf_p(:)
!>        Pointer of decompressed real data
        real(C_double), pointer :: dat_p(:)
!>        Pointer of decompressed 8-byte integer data
        integer(C_long), pointer :: idat8_p(:)
!>        Pointer of decompressed 84byte integer data
        integer(C_int), pointer :: idat4_p(:)
      end type binary_IO_buffer
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine link_real_buffer_for_bin(num, data, bbuf)
!
      integer, intent(in) :: num
      real(kind = kreal), target, intent(in) :: data(num)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      bbuf%len_buf = int((num*kreal),KIND(bbuf%len_buf))
      bbuf%dat_p => data
!
      end subroutine link_real_buffer_for_bin
!
!  ---------------------------------------------------------------------
!
      subroutine link_int8_buffer_for_bin(num, int8_dat, bbuf)
!
      integer, intent(in) :: num
      integer(kind = kint_gl), target, intent(in) :: int8_dat(num)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      bbuf%len_buf = int((num*kint_gl),KIND(bbuf%len_buf))
      bbuf%idat8_p => int8_dat
!
      end subroutine link_int8_buffer_for_bin
!
!  ---------------------------------------------------------------------
!
      subroutine link_int4_buffer_for_bin(num, int4_dat, bbuf)
!
      integer, intent(in) :: num
      integer(kind = 4), target, intent(in) :: int4_dat(num)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      bbuf%len_buf = int((num*4),KIND(bbuf%len_buf))
      bbuf%idat4_p => int4_dat
!
      end subroutine link_int4_buffer_for_bin
!
!  ---------------------------------------------------------------------
!
      subroutine link_text_buffer_for_bin(len_buf, textbuf, bbuf)
!
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      bbuf%len_buf = int(len_buf,KIND(bbuf%len_buf))
      bbuf%buf_p => textbuf
!
      end subroutine link_text_buffer_for_bin
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine unlink_real_buffer_for_bin(bbuf)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      nullify(bbuf%dat_p)
!
      end subroutine unlink_real_buffer_for_bin
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_int8_buffer_for_bin(bbuf)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      nullify(bbuf%idat8_p)
!
      end subroutine unlink_int8_buffer_for_bin
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_int4_buffer_for_bin(bbuf)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      nullify(bbuf%idat4_p)
!
      end subroutine unlink_int4_buffer_for_bin
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_text_buffer_for_bin(bbuf)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      nullify(bbuf%buf_p)
!
      end subroutine unlink_text_buffer_for_bin
!
!  ---------------------------------------------------------------------
!
      end module t_binary_IO_buffer
