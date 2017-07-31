!>@file  gz_binary_IO.f90
!!       module gz_binary_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine open_wt_gzfile_b(gzip_name)
!!      subroutine open_rd_gzfile_b(gzip_name, my_rank)
!!
!!      subroutine gz_write_endian_flag
!!      subroutine gz_write_one_integer_b(int_dat)
!!      subroutine gz_write_one_real_b(real_dat)
!!      subroutine gz_write_mul_int8_b(num, int8_dat)
!!      subroutine gz_write_mul_integer_b(num, int_dat)
!!      subroutine gz_write_integer_stack_b(num, istack)
!!      subroutine gz_write_mul_character_b(num, chara_dat)
!!      subroutine gz_write_1d_vector_b(num, real_dat)
!!      subroutine gz_write_2d_vector_b(n1, n2, real_dat)
!!
!!      subroutine gz_read_endian_flag(my_rank)
!!      subroutine gz_read_one_integer_b(int_dat)
!!      subroutine gz_read_one_real_b(real_dat)
!!      subroutine gz_read_mul_int8_b(num, int8_dat)
!!      subroutine gz_read_mul_integer_b(num, int_dat)
!!      subroutine gz_read_integer_stack_b(num, istack, ntot)
!!      subroutine gz_read_mul_character_b(num, chara_dat)
!!      subroutine gz_read_1d_vector_b(num, real_dat)
!!      subroutine gz_read_2d_vector_b(n1, n2, real_dat)
!!@endverbatim
!
      module gz_binary_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_time_data
      use t_field_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_wt_gzfile_b(gzip_name)
!
      use set_parallel_file_name
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: gzip_name
!
!
      call open_wt_gzfile_f(gzip_name)
      call gz_write_endian_flag
!
      end subroutine open_wt_gzfile_b
!
!------------------------------------------------------------------
!
      subroutine open_rd_gzfile_b(gzip_name, my_rank)
!
      use set_parallel_file_name
      use skip_gz_comment
!
      integer(kind=kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: gzip_name
!
!
      call open_rd_gzfile_f(gzip_name)
      call gz_read_endian_flag(my_rank)
!
      end subroutine open_rd_gzfile_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_endian_flag
!
      integer(kind = kint) :: ierr
!
!
      call gzwrite_f(kint, i_UNIX, ierr)
!
      end subroutine gz_write_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_one_integer_b(int_dat)
!
      integer(kind = kint), intent(in) :: int_dat
!
      integer(kind = kint) :: ierr
!
!
      call gzwrite_f(kint, int_dat, ierr)
!
      end subroutine gz_write_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_one_real_b(real_dat)
!
      real(kind = kreal), intent(in) :: real_dat
!
      integer(kind = kint) :: ierr
!
!
      call gzwrite_f(kreal, real_dat, ierr)
!
      end subroutine gz_write_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_mul_int8_b(num, int8_dat)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      integer(kind = kint) :: ierr, ilength
!
!
      ilength = num *  kint_gl
      call gzwrite_f(ilength, int8_dat, ierr)
!
      end subroutine gz_write_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_mul_integer_b(num, int_dat)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: ierr, ilength
!
!
      ilength = num *  kint
      call gzwrite_f(ilength, int_dat(1), ierr)
!
      end subroutine gz_write_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_integer_stack_b(num, istack)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: istack(0:num)
!
!
      call gz_write_mul_integer_b(num, istack(1))
!
      end subroutine gz_write_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_mul_character_b(num, chara_dat)
!
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
!
      integer(kind = kint) :: ierr, ilength
!
!
      ilength = num *  kchara
      call gzwrite_f(ilength, chara_dat(1), ierr)
!
      end subroutine gz_write_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_1d_vector_b(num, real_dat)
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength =  num * kreal
      call gzwrite_f(ilength, real_dat(1), ierr)
!
      end subroutine gz_write_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_2d_vector_b(n1, n2, real_dat)
!
      integer(kind = kint), intent(in) :: n1, n2
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
!
      integer(kind = kint) :: ierr, ilength
!
!
      ilength = n1 * n2 * kreal
      call gzwrite_f(ilength, real_dat(1,1), ierr)
!
      end subroutine gz_write_2d_vector_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_endian_flag(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: ierr, int_dat
!
!
      call gzread_f(iflag_endian, kint, int_dat, ierr)
!
      if(int_dat .eq. i_UNIX) then
        if(my_rank.eq.0) write(*,*) 'binary data have correct endian!'
        iflag_endian = iendian_KEEP
      else if(int_dat .eq. i_XINU) then
        if(my_rank.eq.0) write(*,*) 'binary data have opposite endian!'
        iflag_endian = iendian_FLIP
      else
        iflag_endian = -1
        if(my_rank.eq.0) write(*,*) 'Binary Data is someting wrong!',   &
     &                   int_dat
      end if
!
      end subroutine gz_read_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_one_integer_b(int_dat)
!
      integer(kind = kint), intent(inout) :: int_dat
!
      integer(kind = kint) :: ierr
!
!
      call gzread_f(iflag_endian, kint, int_dat, ierr)
!
      end subroutine gz_read_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_one_real_b(real_dat)
!
      real(kind = kreal), intent(inout) :: real_dat
!
      integer(kind = kint) :: ierr
!
!
      call gzread_f(iflag_endian, kreal, real_dat, ierr)
!
      end subroutine gz_read_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_mul_int8_b(num, int8_dat)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength = num * kint_gl
      call gzread_f(iflag_endian, ilength, int8_dat(1), ierr)
!
      end subroutine gz_read_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_mul_integer_b(num, int_dat)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength = num * kint
      call gzread_f(iflag_endian, ilength, int_dat(1), ierr)
!
      end subroutine gz_read_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_integer_stack_b(num, istack, ntot)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: ntot
      integer(kind = kint), intent(inout) :: istack(0:num)
!
!
      istack(0) = 0
      call gz_read_mul_integer_b(num, istack(1))
      ntot = istack(num)
!
      end subroutine gz_read_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_mul_character_b(num, chara_dat)
!
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength = num * kchara
      call gzread_f(iflag_endian, ilength, chara_dat(1), ierr)
!
      end subroutine gz_read_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_1d_vector_b(num, real_dat)
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength =  num * kreal
      call gzread_f(iflag_endian, ilength, real_dat(1), ierr)
!
      end subroutine gz_read_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_2d_vector_b(n1, n2, real_dat)
!
      integer(kind = kint), intent(in) :: n1, n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength =  n1 * n2 * kreal
      call gzread_f(iflag_endian, ilength, real_dat(1,1), ierr)
!
      end subroutine gz_read_2d_vector_b
!
! -----------------------------------------------------------------------
!
      end module gz_binary_IO
