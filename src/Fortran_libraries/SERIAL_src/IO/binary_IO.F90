!>@file  binary_IO.f90
!!       module binary_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Core routines for binary IO
!!
!!@verbatim
!!      subroutine open_write_binary_file(file_name, bbuf)
!!      subroutine open_append_binary_file(file_name, bbuf)
!!      subroutine open_read_binary_file(file_name, id_rank, bbuf)
!!      subroutine close_binary_file 
!!      subroutine seek_forward_binary_file(len_byte, bbuf)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!
!!      subroutine write_endian_flag(bbuf)
!!      subroutine write_one_integer_to_32bit(int_dat, bbuf)
!!      subroutine write_one_integer_b(int_dat, bbuf)
!!      subroutine write_one_real_b(real_dat, bbuf)
!!      subroutine write_mul_int_to_32bit(num, int4_dat, bbuf)
!!      subroutine write_mul_int8_b(num, int_gl_dat, bbuf)
!!      subroutine write_mul_integer_b(num, int_dat, bbuf)
!!      subroutine write_integer_stack_b(num, istack, bbuf)
!!      subroutine write_mul_character_b(num, chara_dat, bbuf)
!!      subroutine write_mul_one_character_b(num, chara_dat, bbuf)
!!      subroutine write_1d_vector_b(num, real_dat, bbuf)
!!      subroutine write_2d_vector_b(n1, n2, real_dat, bbuf)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!
!!      integer function endian_check(id_rank, int_dat)
!!      subroutine read_one_integer_from_32bit(bbuf, int_dat)
!!      subroutine read_one_integer_b(bbuf, int_dat)
!!      subroutine read_one_real_b(bbuf, real_dat)
!!      subroutine read_mul_int_from_32bit(bbuf, num, int_dat)
!!      subroutine read_mul_int8_b(bbuf, num, int_gl_dat)
!!      subroutine read_mul_integer_b(bbuf, num, int_dat)
!!      subroutine read_integer_stack_b(bbuf, num, istack, ntot)
!!      subroutine read_mul_character_b(bbuf, num, chara_dat)
!!      subroutine read_mul_one_character_b(bbuf, num, chara_dat)
!!      subroutine read_1d_vector_b(bbuf, num, real_dat)
!!      subroutine read_2d_vector_b(bbuf, n1, n2, real_dat)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!@endverbatim
!
      module binary_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_error_IDs
      use t_binary_IO_buffer
!
      implicit none
!
      private :: write_endian_flag, read_endian_flag
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_write_binary_file(file_name, bbuf)
!
      use set_parallel_file_name
      use binary_file_access
!
      character(len=kchara), intent(in) :: file_name
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      character(len=kchara) :: file_name_w_null
!
!
#ifdef ZLIB_IO
      file_name_w_null = add_null_character(file_name)
      call open_wt_rawfile_f(file_name_w_null, bbuf)
      if(bbuf%ierr_bin .gt. 0) return
#else
      open(bbuf%id_binary, file = file_name, form='unformatted')
#endif
!
      call write_endian_flag(bbuf)
!
      end subroutine open_write_binary_file
!
! -----------------------------------------------------------------------
!
      subroutine open_append_binary_file(file_name, bbuf)
!
      use set_parallel_file_name
      use binary_file_access
!
      character(len=kchara), intent(in) :: file_name
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      character(len=kchara) :: file_name_w_null
!
!
#ifdef ZLIB_IO
      file_name_w_null = add_null_character(file_name)
      call open_ad_rawfile_f(file_name_w_null, bbuf)
#else
      open(bbuf%id_binary, file = file_name, form='unformatted',        &
     &      position='append')
#endif
!
      end subroutine open_append_binary_file
!
! -----------------------------------------------------------------------
!
      subroutine open_read_binary_file(file_name, id_rank, bbuf)
!
      use set_parallel_file_name
      use binary_file_access
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      character(len=kchara) :: file_name_w_null
!
!
      bbuf%ierr_bin = 0
#ifdef ZLIB_IO
      file_name_w_null = add_null_character(file_name)
      call open_rd_rawfile_f(file_name_w_null, bbuf)
      if(bbuf%ierr_bin .gt. 0) return
#else
      open(bbuf%id_binary, file = file_name, form='unformatted')
#endif
!
      call read_endian_flag(bbuf, id_rank)
!
      end subroutine open_read_binary_file
!
! -----------------------------------------------------------------------
!
      subroutine close_binary_file
!
      use binary_file_access
!
#ifdef ZLIB_IO
      call close_rawfile_f
#else
      close(bbuf%id_binary)
#endif
!
      end subroutine close_binary_file
!
! -----------------------------------------------------------------------
!
      subroutine seek_forward_binary_file(len_byte, bbuf)
!
      use binary_file_access
!
      integer(kind = kint_gl), intent(in) :: len_byte
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer :: ilength
      character(len=1) :: tmpchara(len_byte)
      integer(kind = kint_gl) :: ist
!
!
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((len_byte - ist), huge_20))
        call rawseek_go_fwd_f(ilength, bbuf)
        ist = ist + ilength
        if(ist .ge. len_byte) exit
      end do
#else
      read(bbuf%id_binary) tmpchara(1:len_byte)
#endif
!
      end subroutine seek_forward_binary_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_endian_flag(bbuf)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl), parameter :: ione64 = 1
      integer :: itmp4(1)
!
      itmp4(1) = i_UNIX
      call write_mul_int_to_32bit(ione64, itmp4, bbuf)
!
      end subroutine write_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine write_one_integer_to_32bit(int_dat, bbuf)
!
      use transfer_to_long_integers
!
      integer(kind = kint_4b), intent(in) :: int_dat
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl), parameter :: ione64 = 1
      integer(kind = kint_gl) :: itmp64(1)
!
!
      itmp64(1) = cast_long(int_dat)
      call write_mul_int8_b(ione64, itmp64, bbuf)
!
      end subroutine write_one_integer_to_32bit
!
! -----------------------------------------------------------------------
!
      subroutine write_one_integer_b(int_dat, bbuf)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: int_dat
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl), parameter :: ione64 = 1
      integer(kind = kint_gl) :: itmp64(1)
!
!
      itmp64(1) = cast_long(int_dat)
      call write_mul_int8_b(ione64, itmp64, bbuf)
!
      end subroutine write_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine write_one_real_b(real_dat, bbuf)
!
      real(kind = kreal), intent(in) :: real_dat
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl), parameter :: ione64 = 1
      real(kind = kreal) :: rtmp(1)
!
      rtmp(1) = real_dat
      call write_1d_vector_b(ione64, rtmp, bbuf)
!
      end subroutine write_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_mul_int_to_32bit(num, int4_dat, bbuf)
!
      use binary_file_access
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_4b), intent(in) :: int4_dat(num)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint) :: ist
      integer:: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
!
        call rawwrite_int4_f(ilength, int4_dat(ist+1), bbuf)
        ist = ist + ilength
        if(bbuf%ierr_bin .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      write(bbuf%id_binary)  int4_dat(1:num)
#endif
!
      end subroutine write_mul_int_to_32bit
!
! -----------------------------------------------------------------------
!
      subroutine write_mul_int8_b(num, int_gl_dat, bbuf)
!
      use binary_file_access
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int_gl_dat(num)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint) :: ist
      integer:: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
!
        call rawwrite_int8_f(ilength, int_gl_dat(ist+1), bbuf)
        ist = ist + ilength
        if(bbuf%ierr_bin .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      write(bbuf%id_binary)  int_gl_dat(1:num)
#endif
!
      end subroutine write_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine write_mul_integer_b(num, int_dat, bbuf)
!
      use transfer_to_long_integers
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      type(tmp_i8_array)  :: tmp64
!
      if(num .le. 0) return
      call dup_from_short_array(num, int_dat, tmp64)
      call write_mul_int8_b(tmp64%n1, tmp64%id_a, bbuf)
      call dealloc_1d_i8array(tmp64)
      if(bbuf%ierr_bin .ne. 0) return
!
      end subroutine write_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine write_integer_stack_b(num, istack, bbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: istack(0:num)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_mul_integer_b(num, istack(1), bbuf)
!
      end subroutine write_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine write_mul_character_b(num, chara_dat, bbuf)
!
      use binary_file_access
!
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: ist
      integer:: lbyte, ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength *  kchara
!
        call rawwrite_chara_f(lbyte, chara_dat(ist+1), bbuf)
        ist = ist + ilength
        if(bbuf%ierr_bin .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      write(bbuf%id_binary)  chara_dat(1:num)
#endif
      return
!
      end subroutine write_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine write_mul_one_character_b(num, chara_dat, bbuf)
!
      use binary_file_access
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=1), intent(in) :: chara_dat(num)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: ist
      integer:: lbyte, ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength
!
        call rawwrite_chara_f(lbyte, chara_dat(ist+1), bbuf)
        ist = ist + ilength
        if(bbuf%ierr_bin .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      write(bbuf%id_binary)  chara_dat(1:num)
#endif
      return
!
      end subroutine write_mul_one_character_b
!
! -----------------------------------------------------------------------
!
      subroutine write_1d_vector_b(num, real_dat, bbuf)
!
      use binary_file_access
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: ist
      integer:: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
!
        call rawwrite_real_f(ilength, real_dat(ist+1), bbuf)
        ist = ist + ilength
        if(bbuf%ierr_bin .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      write(bbuf%id_binary)  real_dat(1:num)
#endif
      return
!
      end subroutine write_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine write_2d_vector_b(n1, n2, real_dat, bbuf)
!
      use transfer_to_long_integers
!
      integer(kind = kint_gl), intent(in) :: n1
      integer(kind = kint), intent(in) :: n2
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: num
!
!
      num = n1 * cast_long(n2)
      call write_1d_vector_b(num, real_dat(1,1), bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      end subroutine write_2d_vector_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer function endian_check(id_rank, int_dat)
!
      integer, intent(in) :: id_rank
      integer, intent(in) :: int_dat
!
!
      if(int_dat .eq. i_UNIX) then
        if(id_rank.eq.0) write(*,*) 'binary data have correct endian!'
        endian_check = iendian_KEEP
      else if(int_dat .eq. i_XINU) then
        if(id_rank.eq.0) write(*,*) 'binary data have opposite endian!'
        endian_check = iendian_FLIP
      else
        endian_check = -1
        if(id_rank.eq.0) write(*,*) 'Binary Data is someting wrong!',   &
     &                               int_dat
      end if
!
      end function endian_check
!
! -----------------------------------------------------------------------
!
      subroutine read_endian_flag(bbuf, id_rank)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      integer, intent(in) :: id_rank
!
      integer(kind = kint_gl), parameter :: ione64 = 1
      integer :: int_dat(1)
!
      bbuf%iflag_swap = iendian_KEEP
      call read_mul_int_from_32bit(bbuf, ione64, int_dat)
!
#ifdef ZLIB_IO
      bbuf%iflag_swap = endian_check(id_rank, int_dat(1))
#else
      bbuf%iflag_swap = iendian_KEEP
#endif
!
      end subroutine read_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine read_one_integer_from_32bit(bbuf, int_dat)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      integer(kind = kint_4b), intent(inout) :: int_dat
!
      integer(kind = kint_gl), parameter :: ione64 = 1
      integer(kind = kint_gl) :: itmp64(1)
!
!
      call read_mul_int8_b(bbuf, ione64, itmp64)
      int_dat = int(itmp64(1),KIND(int_dat))
!
      end subroutine read_one_integer_from_32bit
!
! -----------------------------------------------------------------------
!
      subroutine read_one_integer_b(bbuf, int_dat)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      integer(kind = kint), intent(inout) :: int_dat
!
      integer(kind = kint_gl), parameter :: ione64 = 1
      integer(kind = kint_gl) :: itmp64(1)
!
!
      call read_mul_int8_b(bbuf, ione64, itmp64)
      int_dat = int(itmp64(1),KIND(int_dat))
!
      end subroutine read_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine read_one_real_b(bbuf, real_dat)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      real(kind = kreal), intent(inout) :: real_dat
!
      integer(kind = kint_gl), parameter :: ione64 = 1
      real(kind = kreal) :: rtmp(1)
!
      call read_1d_vector_b(bbuf, ione64, rtmp)
      real_dat = rtmp(1)
!
      end subroutine read_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_mul_int_from_32bit(bbuf, num, int_dat)
!
      use binary_file_access
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_4b), intent(inout) :: int_dat(num)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: ist
      integer:: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
!
        call rawread_int4_f(ilength, int_dat(ist+1), bbuf)
        ist = ist + ilength
        if(bbuf%ierr_bin .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      read(bbuf%id_binary, err=99, end=99)  int_dat(1:num)
#endif
      return
!
      end subroutine read_mul_int_from_32bit
!
! -----------------------------------------------------------------------
!
      subroutine read_mul_int8_b(bbuf, num, int_gl_dat)
!
      use binary_file_access
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int_gl_dat(num)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: ist
      integer:: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
!
        call rawread_int8_f(ilength, int_gl_dat(ist+1), bbuf)
        ist = ist + ilength
        if(bbuf%ierr_bin .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      read(bbuf%id_binary, err=99, end=99)  int_gl_dat(1:num)
#endif
      return
!
      end subroutine read_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine read_mul_integer_b(bbuf, num, int_dat)
!
      use transfer_to_long_integers
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      type(tmp_i8_array)  :: tmp64
!
      if(num .le. 0) return
!
      call alloc_1d_i8array(num, tmp64)
      call read_mul_int8_b(bbuf, tmp64%n1, tmp64%id_a)
      if(bbuf%ierr_bin .gt. 0) return
!
      call dup_to_short_array(tmp64, int_dat)
!
      end subroutine read_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine read_integer_stack_b(bbuf, num, istack, ntot)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: ntot
      integer(kind = kint), intent(inout) :: istack(0:num)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      istack(0) = 0
      call read_mul_integer_b(bbuf, num, istack(1))
      ntot = istack(num)
      if(bbuf%ierr_bin .gt. 0) return
!
      end subroutine read_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine read_mul_character_b(bbuf, num, chara_dat)
!
      use binary_file_access
!
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: ist
      integer:: lbyte, ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength *  kchara
!
        call rawread_chara_f(lbyte, chara_dat(ist+1), bbuf)
        ist = ist + ilength
        if(bbuf%ierr_bin .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      read(bbuf%id_binary, err=99, end=99)  chara_dat(1:num)
#endif
      return
!
      end subroutine read_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine read_mul_one_character_b(bbuf, num, chara_dat)
!
      use binary_file_access
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=1), intent(inout) :: chara_dat(num)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: ist
      integer:: lbyte, ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength
!
        call rawread_chara_f(lbyte, chara_dat(ist+1), bbuf)
        ist = ist + ilength
        if(bbuf%ierr_bin .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      read(bbuf%id_binary, err=99, end=99)  chara_dat(1:num)
#endif
      return
!
      end subroutine read_mul_one_character_b
!
! -----------------------------------------------------------------------
!
      subroutine read_1d_vector_b(bbuf, num, real_dat)
!
      use binary_file_access
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: ist
      integer:: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
!
        call rawread_real_f(ilength, real_dat(ist+1), bbuf)
        ist = ist + ilength
        if(bbuf%ierr_bin .ne. 0) go to 99
        if(ist .ge. num) exit
      end do
#else
      read(bbuf%id_binary, err=99, end=99)  real_dat(1:num)
#endif
      return
!
  99  continue
      bbuf%ierr_bin = ierr_file
!
      end subroutine read_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine read_2d_vector_b(bbuf, n1, n2, real_dat)
!
      use transfer_to_long_integers
!
      integer(kind = kint_gl), intent(in) :: n1
      integer(kind = kint), intent(in) :: n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: num
!
!
      num = n1 * cast_long(n2)
      call read_1d_vector_b(bbuf, num, real_dat(1,1))
!
      end subroutine read_2d_vector_b
!
! -----------------------------------------------------------------------
!
      end module binary_IO
