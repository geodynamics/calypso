!>@file  gz_binary_IO.f90
!!       module gz_binary_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine open_wt_gzfile_b(gzip_name, zbuf)
!!      subroutine open_rd_gzfile_b(gzip_name, id_rank, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_write_one_integer_b(int_dat, zbuf)
!!      subroutine gz_write_one_real_b(real_dat, zbuf)
!!      subroutine gz_write_mul_int8_b(num, int8_dat, zbuf)
!!      subroutine gz_write_mul_integer_b(num, int_dat, zbuf)
!!      subroutine gz_write_integer_stack_b(num, istack, zbuf)
!!      subroutine gz_write_mul_character_b(num, chara_dat, zbuf)
!!      subroutine gz_write_1d_vector_b(num, real_dat, zbuf)
!!      subroutine gz_write_2d_vector_b(n1, n2, real_dat, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_read_endian_flag(id_rank, zbuf)
!!      subroutine gz_read_one_integer_b(zbuf, int_dat)
!!      subroutine gz_read_one_real_b(zbuf, real_dat)
!!      subroutine gz_read_mul_int8_b(zbuf, num, int8_dat)
!!      subroutine gz_read_mul_integer_b(zbuf, num, int_dat)
!!      subroutine gz_read_integer_stack_b(zbuf, num, istack, ntot)
!!      subroutine gz_read_mul_character_b(zbuf, num, chara_dat)
!!      subroutine gz_read_1d_vector_b(zbuf, num, real_dat)
!!      subroutine gz_read_2d_vector_b(zbuf, n1, n2, real_dat)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module gz_binary_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_error_IDs
      use t_buffer_4_gzip
      use binary_IO
!
      implicit none
!
      private :: gz_write_endian_flag, gz_read_endian_flag
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_wt_gzfile_b(gzip_name, zbuf)
!
      use set_parallel_file_name
      use skip_gz_comment
      use gzip_file_access
!
      character(len=kchara), intent(in) :: gzip_name
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call open_wt_gzfile_f(gzip_name, zbuf)
      call gz_write_endian_flag(zbuf)
!
      end subroutine open_wt_gzfile_b
!
!------------------------------------------------------------------
!
      subroutine open_rd_gzfile_b(gzip_name, id_rank, zbuf)
!
      use set_parallel_file_name
      use skip_gz_comment
      use gzip_file_access
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: gzip_name
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call open_rd_gzfile_f(gzip_name, zbuf)
      call gz_read_endian_flag(id_rank, zbuf)
!
      if(zbuf%iflag_swap .eq. -1) zbuf%ierr_zlib = ierr_file
!
      end subroutine open_rd_gzfile_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_endian_flag(zbuf)
!
      use gzip_file_access
!
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer, parameter :: i_UNIX4(1) = (/i_UNIX/)
!
!
      call gzwrite_int4_f(1, i_UNIX4, zbuf)
!
      end subroutine gz_write_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_one_integer_b(int_dat, zbuf)
!
      use transfer_to_long_integers
      use gzip_file_access
!
      integer(kind = kint), intent(in) :: int_dat
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl), parameter :: ione64 = 1
      integer(kind = kint_gl) :: i8tmp(1)
!
!
      i8tmp(1) = cast_long(int_dat)
      call gz_write_mul_int8_b(ione64, i8tmp, zbuf)
!
      end subroutine gz_write_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_one_real_b(real_dat, zbuf)
!
      use gzip_file_access
!
      real(kind = kreal), intent(in) :: real_dat
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl), parameter :: ione64 = 1
      real(kind = kreal) :: rtmp(1)
!
!
      rtmp(1) = real_dat
      call gz_write_1d_vector_b(ione64, rtmp, zbuf)
!
      end subroutine gz_write_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_mul_int8_b(num, int8_dat, zbuf)
!
      use gzip_file_access
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist
      integer :: ilength
!
!
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
!
        call gzwrite_int8_f(ilength, int8_dat(ist+1), zbuf)
        ist = ist + ilength
        if(zbuf%ierr_zlib .ne. 0) return
        if(ist .ge. num) exit
      end do
      return
!
      end subroutine gz_write_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_mul_integer_b(num, int_dat, zbuf)
!
      use transfer_to_long_integers
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      type(tmp_i8_array)  :: tmp64
!
      if(num .le. 0) return
      call dup_from_short_array(num, int_dat, tmp64)
      call gz_write_mul_int8_b(num, tmp64%id_a, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call dealloc_1d_i8array(tmp64)
!
      end subroutine gz_write_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_integer_stack_b(num, istack, zbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: istack(0:num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      call gz_write_mul_integer_b(num, istack(1), zbuf)
!
      end subroutine gz_write_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_mul_character_b(num, chara_dat, zbuf)
!
      use gzip_file_access
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) ::  ist
      integer :: lbyte, ilength
!
!
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength * kchara
!
        call gzwrite_chara_f(lbyte, chara_dat(ist+1), zbuf)
        ist = ist + ilength
        if(zbuf%ierr_zlib .ne. 0) return
        if(ist .ge. num) exit
      end do
      return
!
      end subroutine gz_write_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_1d_vector_b(num, real_dat, zbuf)
!
      use gzip_file_access
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist
      integer :: ilength
!
!
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
!
        call gzwrite_real_f(ilength, real_dat(ist+1), zbuf)
        if(zbuf%ierr_zlib .ne. 0) return
        ist = ist + ilength
        if(ist .ge. num) exit
      end do
      return
!
      end subroutine gz_write_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_2d_vector_b(n1, n2, real_dat, zbuf)
!
      integer(kind = kint_gl), intent(in) :: n1
      integer(kind = kint), intent(in) :: n2
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i2
!
!
      do i2 = 1, n2
        call gz_write_1d_vector_b(n1, real_dat(1,i2), zbuf)
        if(zbuf%ierr_zlib .ne. 0) return
      end do
!
      end subroutine gz_write_2d_vector_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_endian_flag(id_rank, zbuf)
!
      use binary_IO
      use gzip_file_access
!
      integer, intent(in) :: id_rank
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer :: int_dat(1)
!
!
      zbuf%iflag_swap = iendian_KEEP
      call gzread_int4_f(1, int_dat, zbuf)
      zbuf%iflag_swap = endian_check(id_rank, int_dat(1))
!
      end subroutine gz_read_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_one_integer_b(zbuf, int_dat)
!
      use gzip_file_access
!
      integer(kind = kint), intent(inout) :: int_dat
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl), parameter :: ione64 = 1
      integer(kind = kint_gl) :: int64(1)
!
!
      call gz_read_mul_int8_b(zbuf, ione64, int64)
      int_dat = int(int64(1),KIND(int_dat))
!
      end subroutine gz_read_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_one_real_b(zbuf, real_dat)
!
      use gzip_file_access
!
      real(kind = kreal), intent(inout) :: real_dat
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl), parameter :: ione64 = 1
      real(kind = kreal) :: rtmp(1)
!
!
      call gz_read_1d_vector_b(zbuf, ione64, rtmp)
      real_dat = rtmp(1)
!
      end subroutine gz_read_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_mul_int8_b(zbuf, num, int8_dat)
!
      use gzip_file_access
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: lbyte, ilength
      integer(kind = kint_gl) :: ist
!
!
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength * kint_gl
!
        call gzread_int8_f(ilength, int8_dat(ist+1), zbuf)
        if(zbuf%ierr_zlib .ne. 0) return
        ist = ist + ilength
        if(ist .ge. num) exit
      end do
!
      end subroutine gz_read_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_mul_integer_b(zbuf, num, int_dat)
!
      use transfer_to_long_integers
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      type(tmp_i8_array)  :: tmp64
!
      if(num .le. 0) return
      call alloc_1d_i8array(num, tmp64)
      call gz_read_mul_int8_b(zbuf, num, tmp64%id_a)
      if(zbuf%ierr_zlib .ne. 0) return
      call dup_to_short_array(tmp64, int_dat)
!
      end subroutine gz_read_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_integer_stack_b(zbuf, num, istack, ntot)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: ntot
      integer(kind = kint), intent(inout) :: istack(0:num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      istack(0) = 0
      call gz_read_mul_integer_b(zbuf, num, istack(1))
      if(zbuf%ierr_zlib .ne. 0) return
      ntot = istack(num)
!
      end subroutine gz_read_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_mul_character_b(zbuf, num, chara_dat)
!
      use gzip_file_access
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: lbyte, ilength
      integer(kind = kint_gl) :: ist
!
!
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength * kchara
!
        call gzread_chara_f(lbyte, chara_dat(ist+1), zbuf)
        if(zbuf%ierr_zlib .ne. 0) return
        ist = ist + ilength
        if(ist .ge. num) exit
      end do
!
      end subroutine gz_read_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_1d_vector_b(zbuf, num, real_dat)
!
      use gzip_file_access
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: ilength
      integer(kind = kint_gl) :: ist
!
!
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
!
        call gzread_real_f(ilength, real_dat(ist+1), zbuf)
        if(zbuf%ierr_zlib .ne. 0) return
        ist = ist + ilength
        if(ist .ge. num) exit
      end do
!
      end subroutine gz_read_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_2d_vector_b(zbuf, n1, n2, real_dat)
!
      integer(kind = kint_gl), intent(in) :: n1
      integer(kind = kint), intent(in) :: n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i2
!
!
      do i2 = 1, n2
        call gz_read_1d_vector_b(zbuf, n1, real_dat(1,i2))
        if(zbuf%ierr_zlib .ne. 0) return
      end do
!
      end subroutine gz_read_2d_vector_b
!
! -----------------------------------------------------------------------
!
      end module gz_binary_IO
