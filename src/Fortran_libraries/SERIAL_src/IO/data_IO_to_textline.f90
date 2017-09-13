!>@file  data_IO_to_textline.f90
!!       module data_IO_to_textline
!!
!!@author H. Matsui
!!@date   Programmed 2007
!!@date   modified in Sep., 2017
!
!> @brief Data IO to one line text buffer
!!
!!@verbatim
!!      integer(kind = kint) function len_one_word_textline(word)
!!      integer(kind = kint) function len_multi_int_textline(num)
!!      integer(kind = kint) function len_multi_6digit_line(num)
!!      integer(kind = kint) function len_vector_textline(num)
!!      integer(kind = kint) function len_int8_and_mul_int_textline(num)
!!      integer(kind = kint) function len_int8_and_vector_textline(num)
!!
!!      character(len=len_trim(word)+1) function one_word_textline(word)
!!      function integer_nolfline(int_dat)
!!        character(len=16) :: integer_nolfline
!!      function integer_textline(int_dat)
!!        character(len=16+1) :: integer_textline
!!      function int_stack_textline(num, istack)
!!        character(len=num*16+1) :: int_stack_textline
!!      function multi_int_textline(num, int_dat)
!!        character(len=num*16+1) :: multi_int_textline
!!      function int_stack8_textline(num, istack)
!!        character(len=num*16+1) :: int_stack8_textline
!!      function multi_int8_textline(num, int8_dat)
!!        character(len=num*16+1) :: multi_int8_textline
!!      function mul_6digit_int_line(num, int_dat)
!!        character(len=num*6+1) :: mul_6digit_int_line
!!      function vector_textline(num, real_dat)
!!        character(len=num*25+1) :: vector_textline
!!      function int8_and_mul_int_textline(int8_gl, num, int_dat)
!!        character(len=num*16+17) :: int8_and_mul_int_textline
!!      function int8_and_vector_textline(int8_gl, num, real_dat)
!!        character(len=num*25+17) :: int8_and_vector_textline
!!
!!      integer(kind = kint) function read_one_word_textline            &
!!     &                            (textbuf, field_name)
!!      subroutine read_integer_nolfline(textbuf, int_dat)
!!      subroutine read_integer_textline(textbuf, int_dat)
!!      subroutine read_int_stack_textline(textbuf, num, istack, ntot)
!!      subroutine read_multi_int_textline(textbuf, num, int_dat)
!!      subroutine read_int8_stack_textline(textbuf, num, istack)
!!      subroutine read_multi_int8_textline(textbuf, num, int8_dat)
!!      subroutine read_mul_6digit_int_line(textbuf, num, int_dat)
!!      subroutine read_vector_textline(textbuf, num, real_dat)
!!      subroutine read_int8_and_mul_int_textline                       &
!!     &         (textbuf, int8_gl, num, int_dat)
!!      subroutine read_int8_and_vector_textline                        &
!!     &         (textbuf, int8_gl, num, real_dat)
!!@endverbatim
!
      module data_IO_to_textline
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint), parameter :: len_real_text = 25 + 1
      integer(kind = kint), parameter :: len_real_nolf = 25
      integer(kind = kint), parameter :: len_int_txt = 16 + 1
      integer(kind = kint), parameter :: len_integer_nolf = 16
      integer(kind = kint), parameter :: len_6digit_txt = 6 + 1
      integer(kind = kint), parameter :: len_6digit_nolf = 6
!
! -------------------------------------------------------------------
!
       contains
!
! -------------------------------------------------------------------
!
      integer(kind = kint) function len_one_word_textline(word)
!
      character(len=kchara), intent(in) :: word
!
!
      len_one_word_textline = len_trim(word) + 1
!
      end function len_one_word_textline
!
! -------------------------------------------------------------------
!
      integer(kind = kint) function len_multi_int_textline(num)
!
      integer(kind = kint), intent(in) ::    num
!
!
      len_multi_int_textline = num*16+1
!
      end function len_multi_int_textline
!
! -------------------------------------------------------------------
!
      integer(kind = kint) function len_multi_6digit_line(num)
!
      integer(kind = kint), intent(in) ::    num
!
!
      len_multi_6digit_line = num*6+1
!
      end function len_multi_6digit_line
!
! -------------------------------------------------------------------
!
      integer(kind = kint) function len_vector_textline(num)
!
      integer(kind = kint), intent(in) ::    num
!
      len_vector_textline = num*25+1
!
      end function len_vector_textline
!
! -------------------------------------------------------------------
!
      integer(kind = kint) function len_int8_and_mul_int_textline(num)
!
      integer(kind = kint), intent(in) ::    num
!
      len_int8_and_mul_int_textline = num*16+17
!
      end function len_int8_and_mul_int_textline
!
! -------------------------------------------------------------------
!
      integer(kind = kint) function len_int8_and_vector_textline(num)
!
      integer(kind = kint), intent(in) ::    num
!
      len_int8_and_vector_textline = num*25+17
!
      end function len_int8_and_vector_textline
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      function one_word_textline(word)
!
      character(len=kchara), intent(in) :: word
      character(len=len_trim(word)+1) :: one_word_textline
!
!
      one_word_textline = trim(word) // char(10)
!
      end function one_word_textline
!
! -------------------------------------------------------------------
!
      function integer_nolfline(int_dat)
!
      integer(kind = kint), intent(in) ::    int_dat
      integer(kind = kint) :: int_tmp(1)
!
      character(len=16) :: integer_nolfline
!
!
      int_tmp(1) = int_dat
      integer_nolfline = multi_int_textline(ione, int_tmp)
!
      end function integer_nolfline
!
! -------------------------------------------------------------------
!
      function integer_textline(int_dat)
!
      integer(kind = kint), intent(in) ::    int_dat
      integer(kind = kint) :: int_tmp(1)
!
      character(len=16+1) :: integer_textline
!
!
      int_tmp(1) = int_dat
      integer_textline = multi_int_textline(ione, int_tmp)
!
      end function integer_textline
!
! -------------------------------------------------------------------
!
      function int_stack_textline(num, istack)
!
      integer(kind = kint), intent(in) ::    num
      integer(kind = kint), intent(in) ::    istack(0:num)
!
      character(len=num*16+1) :: int_stack_textline
!
!
      int_stack_textline = multi_int_textline(num, istack(1))
!
      end function int_stack_textline
!
! -------------------------------------------------------------------
!
      function multi_int_textline(num, int_dat)
!
      integer(kind = kint), intent(in) ::    num
      integer(kind = kint), intent(in) ::    int_dat(num)
!
      character(len=num*16+1) :: multi_int_textline
!
      character(len=kchara) :: fmt_txt
!
!
      if(num .gt. 0) then
        write(fmt_txt,'(a1,i7,a9)') '(', num, '(i16),a1)'
        write(multi_int_textline,fmt_txt)  int_dat(1:num), char(10)
      else
        multi_int_textline(1:1) = char(10)
      end if
!
      end function multi_int_textline
!
! -------------------------------------------------------------------
!
      function int_stack8_textline(num, istack)
!
      integer(kind = kint), intent(in) ::    num
      integer(kind = kint_gl), intent(in) :: istack(0:num)
!
      character(len=num*16+1) :: int_stack8_textline
!
!
      int_stack8_textline = multi_int8_textline(num, istack(1))
!
      end function int_stack8_textline
!
! -------------------------------------------------------------------
!
      function multi_int8_textline(num, int8_dat)
!
      integer(kind = kint), intent(in) ::    num
      integer(kind = kint_gl), intent(in) ::    int8_dat(num)
!
      character(len=num*16+1) :: multi_int8_textline
!
      character(len=kchara) :: fmt_txt
!
!
      if(num .gt. 0) then
        write(fmt_txt,'(a1,i7,a9)') '(', num, '(i16),a1)'
        write(multi_int8_textline,fmt_txt) int8_dat(1:num), char(10)
      else
        multi_int8_textline(1:1) = char(10)
      end if
!
      end function multi_int8_textline
!
! -------------------------------------------------------------------
!
      function mul_6digit_int_line(num, int_dat)
!
      integer(kind = kint), intent(in) ::    num
      integer(kind = kint), intent(in) ::    int_dat(num)
!
      character(len=num*6+1) :: mul_6digit_int_line
!
      character(len=kchara) :: fmt_txt
!
!
      if(num .gt. 0) then
        write(fmt_txt,'(a1,i7,a8)') '(', num, '(i6),a1)'
        write(mul_6digit_int_line,fmt_txt)  int_dat(1:num), char(10)
      else
        mul_6digit_int_line(1:1) = char(10)
      end if
!
      end function mul_6digit_int_line
!
! -------------------------------------------------------------------
!
      function vector_textline(num, real_dat)
!
      integer(kind = kint), intent(in) ::    num
      real(kind = kreal), intent(in) ::      real_dat(num)
!
      character(len=num*25+1) :: vector_textline
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a1,i1,a16)') '(', num, '(1pE25.15e3),a1)'
      write(vector_textline,fmt_txt)  real_dat(1:num), char(10)
!
      end function vector_textline
!
! -------------------------------------------------------------------
!
      function int8_and_mul_int_textline(int8_gl, num, int_dat)
!
      integer(kind = kint), intent(in) ::    num
      integer(kind = kint_gl), intent(in) :: int8_gl
      integer(kind = kint), intent(in) ::    int_dat(num)
!
      character(len=num*16+17) :: int8_and_mul_int_textline
!
      character(len=kchara) :: fmt_txt
!
!
      if(num .gt. 0) then
        write(fmt_txt,'(a1,i7,a9)') '(', (num+1), '(i16),a1)'
        write(int8_and_mul_int_textline,fmt_txt)                        &
     &                        int8_gl, int_dat(1:num), char(10)
      else
        write(int8_and_mul_int_textline,'(i16,a1)') int8_gl, char(10)
      end if
!
      end function int8_and_mul_int_textline
!
! -------------------------------------------------------------------
!
      function int8_and_vector_textline(int8_gl, num, real_dat)
!
      integer(kind = kint), intent(in) ::    num
      integer(kind = kint_gl), intent(in) :: int8_gl
      real(kind = kreal), intent(in) ::    real_dat(num)
!
      character(len=num*25+17) :: int8_and_vector_textline
!
      character(len=kchara) :: fmt_txt
!
!
      if(num .gt. 0) then
        write(fmt_txt,'(a5,i1,a16)') '(i16,', num, '(1pE25.15e3),a1)'
        write(int8_and_vector_textline,fmt_txt)                         &
     &                        int8_gl, real_dat(1:num), char(10)
      else
        write(int8_and_vector_textline,'(i16,a1)') int8_gl, char(10)
      end if
!
      end function int8_and_vector_textline
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      integer(kind = kint) function read_one_word_textline              &
     &                            (textbuf, field_name)
!
      character(len=kchara), intent(in) :: textbuf
      character(len=kchara), intent(inout) :: field_name
!
      integer(kind = kint) :: i
!
      field_name = ''
      do i = 1, kchara
        if(iachar(textbuf(i:i)) .eq. 10                                 &
     &    .or. iachar(textbuf(i:i)) .eq. 0) exit 
        field_name(i:i) = textbuf(i:i)
      end do
      read_one_word_textline = i - 1
!
      end function read_one_word_textline
!
! -------------------------------------------------------------------
!
      subroutine read_integer_nolfline(textbuf, int_dat)
!
      character(len=16), intent(in) :: textbuf
      integer(kind = kint), intent(inout) :: int_dat
!
!
      read(textbuf,*) int_dat
!
      end subroutine read_integer_nolfline
!
! -------------------------------------------------------------------
!
      subroutine read_integer_textline(textbuf, int_dat)
!
      character(len=16+1), intent(in) :: textbuf
      integer(kind = kint), intent(inout) :: int_dat
!
      integer(kind = kint) :: int_tmp(1)
!
      call read_multi_int_textline(textbuf, ione, int_tmp(1))
      int_dat = int_tmp(1)
!
      end subroutine read_integer_textline
!
! -------------------------------------------------------------------
!
      subroutine read_int_stack_textline(textbuf, num, istack, ntot)
!
      integer(kind = kint), intent(in) :: num
      character(len=num*16+1), intent(in) :: textbuf
      integer(kind = kint), intent(inout) :: istack(0:num)
      integer(kind = kint), intent(inout) :: ntot
!
!
      istack(0) = 0
      call read_multi_int_textline(textbuf, num, istack(1))
      ntot = istack(num)
!
      end subroutine read_int_stack_textline
!
! -------------------------------------------------------------------
!
      subroutine read_multi_int_textline(textbuf, num, int_dat)
!
      integer(kind = kint), intent(in) :: num
      character(len=num*16+1), intent(in) :: textbuf
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      character(len=num*16) ::    tmp1
!
      if(num .le. 0) return
      tmp1 = textbuf(1:num*16)
      read(tmp1,*) int_dat(1:num)
!
      end subroutine read_multi_int_textline
!
! -------------------------------------------------------------------
!
      subroutine read_int8_stack_textline(textbuf, num, istack)
!
      integer(kind = kint), intent(in) :: num
      character(len=num*16+1), intent(in) :: textbuf
      integer(kind = kint_gl), intent(inout) :: istack(0:num)
!
      istack(0) = 0
      call read_multi_int8_textline(textbuf, num, istack(1))
!
      end subroutine read_int8_stack_textline
!
! -------------------------------------------------------------------
!
      subroutine read_multi_int8_textline(textbuf, num, int8_dat)
!
      integer(kind = kint), intent(in) :: num
      character(len=num*16+1), intent(in) :: textbuf
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      character(len=num*16) ::    tmp1
!
      if(num .le. 0) return
      tmp1 = textbuf(1:num*16)
      read(tmp1,*) int8_dat(1:num)
!
      end subroutine read_multi_int8_textline
!
! -------------------------------------------------------------------
!
      subroutine read_mul_6digit_int_line(textbuf, num, int_dat)
!
      integer(kind = kint), intent(in) :: num
      character(len=num*6+1), intent(in) :: textbuf
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      character(len=num*16) ::    tmp1
!
      if(num .le. 0) return
      tmp1 = textbuf(1:num*6)
      read(tmp1,*) int_dat(1:num)
!
      end subroutine read_mul_6digit_int_line
!
! -------------------------------------------------------------------
!
      subroutine read_vector_textline(textbuf, num, real_dat)
!
      integer(kind = kint), intent(in) :: num
      character(len=num*25+1), intent(in) :: textbuf
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      character(len=num*25) ::    tmp1
!
      if(num .le. 0) return
      tmp1 = textbuf(1:num*25)
      read(tmp1,*) real_dat(1:num)
!
      end subroutine read_vector_textline
!
! -------------------------------------------------------------------
!
      subroutine read_int8_and_mul_int_textline                         &
     &         (textbuf, int8_gl, num, int_dat)
!
      integer(kind = kint), intent(in) :: num
      character(len=num*16+17), intent(in) :: textbuf
      integer(kind = kint_gl), intent(inout) :: int8_gl
      integer(kind = kint), intent(inout) ::    int_dat(num)
!
      character(len=num*16+16) ::    tmp1
!
      tmp1 = textbuf(1:num*16+16)
      if(num .gt. 0) then
        read(tmp1,*) int8_gl, int_dat(1:num)
      else
        read(tmp1,*) int8_gl
      end if
!
      end subroutine read_int8_and_mul_int_textline
!
! -------------------------------------------------------------------
!
      subroutine read_int8_and_vector_textline                          &
     &         (textbuf, int8_gl, num, real_dat)
!
      integer(kind = kint), intent(in) :: num
      character(len=num*25+17), intent(in) :: textbuf
      integer(kind = kint_gl), intent(inout) :: int8_gl
      real(kind = kreal), intent(inout) ::    real_dat(num)
!
      character(len=num*25+16) ::    tmp1
!
      tmp1 = textbuf(1:num*25+16)
      if(num .gt. 0) then
        read(tmp1,*) int8_gl, real_dat(1:num)
      else
        read(tmp1,*) int8_gl
      end if
!
      end subroutine read_int8_and_vector_textline
!
! -------------------------------------------------------------------
!
      end module data_IO_to_textline
