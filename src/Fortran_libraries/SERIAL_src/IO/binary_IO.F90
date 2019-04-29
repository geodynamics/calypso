!>@file  binary_IO.f90
!!       module binary_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Core routines for binary IO
!!
!!@verbatim
!!      subroutine open_write_binary_file(file_name, bflag)
!!      subroutine open_append_binary_file(file_name, bflag)
!!      subroutine open_read_binary_file(file_name, id_rank, bflag)
!!      subroutine close_binary_file 
!!      subroutine seek_forward_binary_file(len_byte)
!!
!!      subroutine write_endian_flag(bflag)
!!      subroutine write_one_integer_to_32bit(int_dat, bflag)
!!      subroutine write_one_integer_b(int_dat, bflag)
!!      subroutine write_one_real_b(real_dat, bflag)
!!      subroutine write_mul_int_to_32bit(num, int4_dat, bflag)
!!      subroutine write_mul_int8_b(num, int_gl_dat, bflag)
!!      subroutine write_mul_integer_b(num, int_dat, bflag)
!!      subroutine write_integer_stack_b(num, istack, bflag)
!!      subroutine write_mul_character_b(num, chara_dat, bflag)
!!      subroutine write_mul_one_character_b(num, chara_dat, bflag)
!!      subroutine write_1d_vector_b(num, real_dat, bflag)
!!      subroutine write_2d_vector_b(n1, n2, real_dat, bflag)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!
!!      integer function endian_check(id_rank, int_dat)
!!      integer(kind = kint) function read_endian_flag(id_rank)
!!      subroutine read_one_integer_from_32bit(bflag, int_dat)
!!      subroutine read_one_integer_b(bflag, int_dat)
!!      subroutine read_one_real_b(bflag, real_dat)
!!      subroutine read_mul_int_from_32bit(bflag, num, int_dat)
!!      subroutine read_mul_int8_b(bflag, num, int_gl_dat)
!!      subroutine read_mul_integer_b(bflag, num, int_dat)
!!      subroutine read_integer_stack_b(bflag, num, istack, ntot)
!!      subroutine read_mul_character_b(bflag, num, chara_dat)
!!      subroutine read_mul_one_character_b(bflag, num, chara_dat)
!!      subroutine read_1d_vector_b(bflag, num, real_dat)
!!      subroutine read_2d_vector_b(bflag, n1, n2, real_dat)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!@endverbatim
!
      module binary_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_error_IDs
!
      implicit none
!
      type binary_IO_flags
!>        integer flag for byte swapping
        integer :: iflag_swap = -1
!>        Error flag for data IO
        integer(kind = kint) :: ierr_IO = 0
      end type binary_IO_flags
!
      integer(kind = kint), parameter, private :: len_4byte = 4
      integer(kind = kint), parameter, private :: id_binary = 19
!
      private :: write_endian_flag, read_endian_flag
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_write_binary_file(file_name, bflag)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_name
      type(binary_IO_flags), intent(inout) :: bflag
!
      character(len=kchara) :: file_name_w_null
!
!
      bflag%ierr_IO = 0
#ifdef ZLIB_IO
      file_name_w_null = add_null_character(file_name)
      call open_wt_rawfile(file_name_w_null, bflag%ierr_IO)
      if(bflag%ierr_IO .gt. 0) return
#else
      open(id_binary, file = file_name, form='unformatted')
#endif
!
      call write_endian_flag(bflag)
!
      end subroutine open_write_binary_file
!
! -----------------------------------------------------------------------
!
      subroutine open_append_binary_file(file_name, bflag)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_name
      type(binary_IO_flags), intent(inout) :: bflag
!
      character(len=kchara) :: file_name_w_null
!
!
      bflag%ierr_IO = 0
#ifdef ZLIB_IO
      file_name_w_null = add_null_character(file_name)
      call open_ad_rawfile(file_name_w_null, bflag%ierr_IO)
      if(bflag%ierr_IO .gt. 0) return
#else
      open(id_binary, file = file_name, form='unformatted',             &
     &      position='append')
#endif
!
      end subroutine open_append_binary_file
!
! -----------------------------------------------------------------------
!
      subroutine open_read_binary_file(file_name, id_rank, bflag)
!
      use set_parallel_file_name
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(binary_IO_flags), intent(inout) :: bflag
!
      character(len=kchara) :: file_name_w_null
!
!
      bflag%ierr_IO = 0
#ifdef ZLIB_IO
      file_name_w_null = add_null_character(file_name)
      call open_rd_rawfile(file_name_w_null, bflag%ierr_IO)
      if(bflag%ierr_IO .gt. 0) return
#else
      open(id_binary, file = file_name, form='unformatted')
#endif
!
      call read_endian_flag(bflag, id_rank)
!
      end subroutine open_read_binary_file
!
! -----------------------------------------------------------------------
!
      subroutine close_binary_file
!
#ifdef ZLIB_IO
      call close_rawfile
#else
      close(id_binary)
#endif
!
      end subroutine close_binary_file
!
! -----------------------------------------------------------------------
!
      subroutine seek_forward_binary_file(len_byte)
!
      integer(kind = kint_gl), intent(in) :: len_byte
!
      integer :: len_result, ilength
      character(len=1) :: tmpchara(len_byte)
      integer(kind = kint_gl) :: ist
!
!
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((len_byte - ist), huge_20))
        call rawseek_go_fwd_f(ilength, len_result)
        ist = ist + ilength
        if(ist .ge. len_byte) exit
      end do
#else
      read(id_binary) tmpchara(1:len_byte)
#endif
!
      end subroutine seek_forward_binary_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_endian_flag(bflag)
!
      type(binary_IO_flags), intent(inout) :: bflag
!
!
#ifdef ZLIB_IO
      call rawwrite_f(kint, i_UNIX, bflag%ierr_IO)
      bflag%ierr_IO = bflag%ierr_IO - kint
#else
      write(id_binary)  i_UNIX
#endif
!
      end subroutine write_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine write_one_integer_to_32bit(int_dat, bflag)
!
      use transfer_to_long_integers
!
      integer(kind = len_4byte), intent(in) :: int_dat
      type(binary_IO_flags), intent(inout) :: bflag
!
!
#ifdef ZLIB_IO
      call rawwrite_f(kint_gl, int_dat, bflag%ierr_IO)
      bflag%ierr_IO = bflag%ierr_IO - len_4byte
#else
      write(id_binary)  int_dat
#endif
!
      end subroutine write_one_integer_to_32bit
!
! -----------------------------------------------------------------------
!
      subroutine write_one_integer_b(int_dat, bflag)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: int_dat
      type(binary_IO_flags), intent(inout) :: bflag
!
!
#ifdef ZLIB_IO
      call rawwrite_f(kint_gl, cast_long(int_dat), bflag%ierr_IO)
      bflag%ierr_IO = bflag%ierr_IO - kint_gl
#else
      write(id_binary)  cast_long(int_dat)
#endif
!
      end subroutine write_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine write_one_real_b(real_dat, bflag)
!
      real(kind = kreal), intent(in) :: real_dat
      type(binary_IO_flags), intent(inout) :: bflag
!
!
#ifdef ZLIB_IO
      call rawwrite_f(kreal, real_dat, bflag%ierr_IO)
      bflag%ierr_IO = bflag%ierr_IO - kreal
#else
      write(id_binary)  real_dat
#endif
!
      end subroutine write_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_mul_int_to_32bit(num, int4_dat, bflag)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = len_4byte), intent(in) :: int4_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: ist
      integer:: lbyte, ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength *  len_4byte
!
        call rawwrite_f(lbyte, int4_dat(ist+1), bflag%ierr_IO)
        ist = ist + ilength
        bflag%ierr_IO = bflag%ierr_IO - lbyte
        if(bflag%ierr_IO .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      write(id_binary)  int4_dat(1:num)
#endif
!
      end subroutine write_mul_int_to_32bit
!
! -----------------------------------------------------------------------
!
      subroutine write_mul_int8_b(num, int_gl_dat, bflag)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int_gl_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: ist
      integer:: lbyte, ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength *  kint_gl
!
        call rawwrite_f(lbyte, int_gl_dat(ist+1), bflag%ierr_IO)
        ist = ist + ilength
        bflag%ierr_IO = bflag%ierr_IO - lbyte
        if(bflag%ierr_IO .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      write(id_binary)  int_gl_dat(1:num)
#endif
!
      end subroutine write_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine write_mul_integer_b(num, int_dat, bflag)
!
      use transfer_to_long_integers
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      type(tmp_i8_array)  :: tmp64
!
      if(num .le. 0) return
      call dup_from_short_array(num, int_dat, tmp64)
      call write_mul_int8_b(tmp64%n1, tmp64%id_a, bflag)
      call dealloc_1d_i8array(tmp64)
!
      end subroutine write_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine write_integer_stack_b(num, istack, bflag)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: istack(0:num)
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call write_mul_integer_b(num, istack(1), bflag)
!
      end subroutine write_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine write_mul_character_b(num, chara_dat, bflag)
!
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: ist
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
        call rawwrite_f(lbyte, chara_dat(ist+1), bflag%ierr_IO)
        ist = ist + ilength
        bflag%ierr_IO = bflag%ierr_IO - lbyte
        if(bflag%ierr_IO .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      write(id_binary)  chara_dat(1:num)
#endif
      return
!
      end subroutine write_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine write_mul_one_character_b(num, chara_dat, bflag)
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=1), intent(in) :: chara_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: ist
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
        call rawwrite_f(lbyte, chara_dat(ist+1), bflag%ierr_IO)
        ist = ist + ilength
        bflag%ierr_IO = bflag%ierr_IO - lbyte
        if(bflag%ierr_IO .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      write(id_binary)  chara_dat(1:num)
#endif
      return
!
      end subroutine write_mul_one_character_b
!
! -----------------------------------------------------------------------
!
      subroutine write_1d_vector_b(num, real_dat, bflag)
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: ist
      integer:: lbyte, ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte =  ilength * kreal
!
        call rawwrite_f(lbyte, real_dat(ist+1), bflag%ierr_IO)
        ist = ist + ilength
        bflag%ierr_IO = bflag%ierr_IO - lbyte
        if(bflag%ierr_IO .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      write(id_binary)  real_dat(1:num)
#endif
      return
!
      end subroutine write_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine write_2d_vector_b(n1, n2, real_dat, bflag)
!
      integer(kind = kint_gl), intent(in) :: n1
      integer(kind = kint), intent(in) :: n2
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: i2
!
!
#ifdef ZLIB_IO
      do i2 = 1, n2
        call write_1d_vector_b(n1, real_dat(1,i2), bflag)
        if(bflag%ierr_IO .ne. 0) return
      end do
#else
      write(id_binary)  real_dat(1:n1,1:n2)
#endif
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
      subroutine read_endian_flag(bflag, id_rank)
!
      type(binary_IO_flags), intent(inout) :: bflag
      integer, intent(in) :: id_rank
      integer :: int_dat
!
!
#ifdef ZLIB_IO
      call rawread_32bit_f(iendian_KEEP, kint, int_dat, bflag%ierr_IO)
      if(bflag%ierr_IO .ne. kint) goto 99
      bflag%iflag_swap = endian_check(id_rank, int_dat)
#else
      read(id_binary)  int_dat
      bflag%iflag_swap = iendian_KEEP
#endif
      bflag%ierr_IO = 0
      return
!
  99  continue
      bflag%ierr_IO = ierr_file
      return
!
      end subroutine read_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine read_one_integer_from_32bit(bflag, int_dat)
!
      type(binary_IO_flags), intent(inout) :: bflag
      integer(kind = len_4byte), intent(inout) :: int_dat
!
!
#ifdef ZLIB_IO
      call rawread_32bit_f                                              &
     &    (bflag%iflag_swap, len_4byte, int_dat, bflag%ierr_IO)
      if(bflag%ierr_IO .ne. len_4byte) goto 99
#else
      read(id_binary, err=99, end=99)  int_dat
#endif
!
      bflag%ierr_IO = 0
      return
!
  99  continue
      bflag%ierr_IO = ierr_file
      return
!
      end subroutine read_one_integer_from_32bit
!
! -----------------------------------------------------------------------
!
      subroutine read_one_integer_b(bflag, int_dat)
!
      type(binary_IO_flags), intent(inout) :: bflag
      integer(kind = kint), intent(inout) :: int_dat
!
      integer(kind = kint_gl) :: int64
!
!
#ifdef ZLIB_IO
      call rawread_64bit_f                                              &
     &    (bflag%iflag_swap, kint_gl, int64, bflag%ierr_IO)
      if(bflag%ierr_IO .ne. kint_gl) goto 99
#else
      read(id_binary, err=99, end=99)  int64
#endif
!
      int_dat = int(int64,KIND(int_dat))
      bflag%ierr_IO = 0
      return
!
  99  continue
      bflag%ierr_IO = ierr_file
      return
!
      end subroutine read_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine read_one_real_b(bflag, real_dat)
!
      type(binary_IO_flags), intent(inout) :: bflag
      real(kind = kreal), intent(inout) :: real_dat
!
!
#ifdef ZLIB_IO
      call rawread_64bit_f                                              &
     &   (bflag%iflag_swap, kreal, real_dat, bflag%ierr_IO)
      if(bflag%ierr_IO .ne. kreal) goto 99
#else
      read(id_binary, err=99, end=99)  real_dat
#endif
      bflag%ierr_IO = 0
      return
!
  99  continue
      bflag%ierr_IO = ierr_file
      return
!
      end subroutine read_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_mul_int_from_32bit(bflag, num, int_dat)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = len_4byte), intent(inout) :: int_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: ist
      integer:: lbyte, ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength * len_4byte
!
        call rawread_32bit_f                                            &
     &     (bflag%iflag_swap, lbyte, int_dat(ist+1), bflag%ierr_IO)
        ist = ist + ilength
        bflag%ierr_IO = bflag%ierr_IO - lbyte
        if(bflag%ierr_IO .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      read(id_binary, err=99, end=99)  int_dat(1:num)
#endif
      return
!
      end subroutine read_mul_int_from_32bit
!
! -----------------------------------------------------------------------
!
      subroutine read_mul_int8_b(bflag, num, int_gl_dat)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int_gl_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: ist
      integer:: lbyte, ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength * kint_gl
!
        call rawread_64bit_f                                            &
     &     (bflag%iflag_swap, lbyte, int_gl_dat(ist+1), bflag%ierr_IO)
        ist = ist + ilength
        bflag%ierr_IO = bflag%ierr_IO - lbyte
        if(bflag%ierr_IO .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      read(id_binary, err=99, end=99)  int_gl_dat(1:num)
#endif
      return
!
      end subroutine read_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine read_mul_integer_b(bflag, num, int_dat)
!
      use transfer_to_long_integers
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      type(tmp_i8_array)  :: tmp64
!
      if(num .le. 0) return
!
      call alloc_1d_i8array(num, tmp64)
      call read_mul_int8_b(bflag, tmp64%n1, tmp64%id_a)
      if(bflag%ierr_IO .gt. 0) return
!
      call dup_to_short_array(tmp64, int_dat)
!
      end subroutine read_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine read_integer_stack_b(bflag, num, istack, ntot)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: ntot
      integer(kind = kint), intent(inout) :: istack(0:num)
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      istack(0) = 0
      call read_mul_integer_b(bflag, num, istack(1))
      if(bflag%ierr_IO .gt. 0) return
      ntot = istack(num)
!
      end subroutine read_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine read_mul_character_b(bflag, num, chara_dat)
!
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: ist
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
        call rawread_32bit_f                                            &
     &     (iendian_KEEP, lbyte, chara_dat(ist+1), bflag%ierr_IO)
        ist = ist + ilength
        bflag%ierr_IO = bflag%ierr_IO - lbyte
        if(bflag%ierr_IO .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      read(id_binary, err=99, end=99)  chara_dat(1:num)
#endif
      return
!
      end subroutine read_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine read_mul_one_character_b(bflag, num, chara_dat)
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=1), intent(inout) :: chara_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: ist
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
        call rawread_32bit_f                                            &
     &      (iendian_KEEP, lbyte, chara_dat(ist+1), bflag%ierr_IO)
        ist = ist + ilength
        bflag%ierr_IO = bflag%ierr_IO - lbyte
        if(bflag%ierr_IO .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      read(id_binary, err=99, end=99)  chara_dat(1:num)
#endif
      return
!
      end subroutine read_mul_one_character_b
!
! -----------------------------------------------------------------------
!
      subroutine read_1d_vector_b(bflag, num, real_dat)
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: ist
      integer:: lbyte, ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte =  ilength * kreal
!
        call rawread_64bit_f                                            &
     &     (bflag%iflag_swap, lbyte, real_dat(ist+1), bflag%ierr_IO)
        ist = ist + ilength
        bflag%ierr_IO = bflag%ierr_IO - lbyte
        if(bflag%ierr_IO .ne. 0) return
        if(ist .ge. num) exit
      end do
#else
      read(id_binary, err=99, end=99)  real_dat(1:num)
#endif
      return
!
      end subroutine read_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine read_2d_vector_b(bflag, n1, n2, real_dat)
!
      integer(kind = kint_gl), intent(in) :: n1
      integer(kind = kint), intent(in) :: n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: i2
!
!
#ifdef ZLIB_IO
      do i2 = 1, n2
        call read_1d_vector_b(bflag, n1, real_dat(1,i2))
        if(bflag%ierr_IO .ne. 0) return
      end do
#else
      read(id_binary, err=99, end=99)  real_dat(1:n1,1:n2)
#endif
!
      end subroutine read_2d_vector_b
!
! -----------------------------------------------------------------------
!
      end module binary_IO
