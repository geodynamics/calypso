!>@file  t_buffer_4_gzip.f90
!!       module t_buffer_4_gzip
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!
!> @brief Output gzipped merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine alloc_zip_buffer(zbuf)
!!      subroutine dealloc_zip_buffer(zbuf)
!!
!!      subroutine defleate_endian_flag(zbuf)
!!      subroutine defleate_int8_vector_b(num, int8_dat, zbuf)
!!      subroutine defleate_1d_vector_b(num, real_dat, zbuf)
!!      subroutine defleate_1d_character_b(num, chara_dat, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine infleate_endian_flag(id_rank, iflag_swap, zbuf)
!!      subroutine infleate_int8_vector_b(num, int8_dat, zbuf)
!!      subroutine infleate_1d_vector_b(num, real_dat, zbuf)
!!      subroutine infleate_1d_character_b(num, chara_dat, zbuf)
!!          type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module t_buffer_4_gzip
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use transfer_to_long_integers
!
      implicit none
!
!>      Structure of 
      type buffer_4_gzip
!>        Actual size of compressed data buffer
        integer(kind = kint_gl) :: ilen_gzipped
!>        Reserved size of compressed data buffer
        integer(kind = kint_gl) :: ilen_gz
!>        Compressed data buffer
        character(len=1), allocatable :: gzip_buf(:)
      end type buffer_4_gzip
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_zip_buffer(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      allocate(zbuf%gzip_buf(zbuf%ilen_gz))
!
      end subroutine alloc_zip_buffer
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_zip_buffer(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      deallocate(zbuf%gzip_buf)
!
      end subroutine dealloc_zip_buffer
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine defleate_endian_flag(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer, parameter :: int_dat = i_UNIX
      integer :: ilen_in, ilen_used
!
!
      ilen_in = int(dble(kint)*1.01 + 24)
      zbuf%ilen_gz = ilen_in
      call alloc_zip_buffer(zbuf)
!
      call gzip_defleat_once(kint, int_dat, ilen_in,                    &
     &    ilen_used, zbuf%gzip_buf(1))
        zbuf%ilen_gzipped =  ilen_used
!
      end subroutine defleate_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine defleate_int8_vector_b(num, int8_dat, zbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist, ilen_tmp
      integer :: nline
      integer :: ilen_in, ilen_used, ilen_line
!
!
      zbuf%ilen_gz = int(dble(num*kint_gl)*1.01+24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = int(dble(huge_30) * 1.01 + 24,KIND(ilen_tmp))
      do
        nline = int(min((num - ist), huge_30/cast_long(kint_gl)))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
        ilen_line = nline * kint_gl
!
        call gzip_defleat_once(ilen_line, int8_dat(ist+1), ilen_in,     &
     &      ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
        zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      end subroutine defleate_int8_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine defleate_1d_vector_b(num, real_dat, zbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist, ilen_tmp
      integer :: nline
      integer :: ilen_in, ilen_used, ilen_line
!
!
      zbuf%ilen_gz = int(dble(num*kreal)*1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = int(dble(huge_30) * 1.01 + 24,KIND(ilen_tmp))
      do
        nline = int(min((num - ist), huge_30/cast_long(kreal)))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
        ilen_line = nline * kreal
!
        call gzip_defleat_once(ilen_line, real_dat(ist+1), ilen_in,     &
     &      ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
        zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      end subroutine defleate_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine defleate_1d_character_b(num, chara_dat, zbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist, ilen_tmp
      integer :: nline
      integer :: ilen_in, ilen_used, ilen_line
!
!
      zbuf%ilen_gz = int(dble(num*kchara)*1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = int(dble(huge_30) * 1.01 + 24,KIND(ilen_tmp))
      do
        nline = int(min((num - ist), huge_30/cast_long(kchara)))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
        ilen_line = nline * kchara
!
        call gzip_defleat_once(ilen_line, chara_dat(ist+1), ilen_in,    &
     &      ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
        zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      end subroutine defleate_1d_character_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine infleate_endian_flag(id_rank, iflag_swap, zbuf)
!
      use binary_IO
!
      integer, intent(in) :: id_rank
      integer, intent(inout) :: iflag_swap
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer :: int_dat
      integer :: ilen_in, ilen_used
!
!
      ilen_in = int(zbuf%ilen_gz)
!
      call gzip_infleat_once                                          &
     &   (ilen_in, zbuf%gzip_buf(1), kint, int_dat, ilen_used)
      iflag_swap = endian_check(id_rank, int_dat)
!
      zbuf%ilen_gzipped = ilen_used
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine infleate_int8_vector_b(num, int8_dat, zbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist, ilen_tmp
      integer :: nline
      integer :: ilen_in, ilen_used, ilen_line
!
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = int(dble(huge_30) * 1.01 + 24,KIND(ilen_tmp))
      do
        nline = int(min((num - ist), huge_30/cast_long(kint_gl)))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
        ilen_line = nline * kint_gl
!
        call gzip_infleat_once                                          &
     &     (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),                &
     &      ilen_line, int8_dat(ist+1), ilen_used)
!
        zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_int8_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine infleate_1d_vector_b(num, real_dat, zbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist, ilen_tmp
      integer :: nline
      integer :: ilen_in, ilen_used, ilen_line
!
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = int(dble(huge_30) * 1.01 + 24,KIND(ilen_tmp))
      do
        nline = int(min((num - ist), huge_30/cast_long(kreal)))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
        ilen_line = nline * kreal
!
        call gzip_infleat_once                                          &
     &     (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),                &
     &     ilen_line, real_dat(ist+1), ilen_used)
!
        zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine infleate_1d_character_b(num, chara_dat, zbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist, ilen_tmp
      integer :: nline
      integer :: ilen_in, ilen_used, ilen_line
!
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = int(dble(huge_30) * 1.01 + 24,KIND(ilen_tmp))
      do
        nline = int(min((num - ist), huge_30/cast_long(kchara)))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
        ilen_line = nline * kchara
!
        call gzip_infleat_once                                          &
     &     (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),                &
     &     ilen_line, chara_dat(ist+1), ilen_used)
!
        zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_1d_character_b
!
! -----------------------------------------------------------------------
!
      end module t_buffer_4_gzip
