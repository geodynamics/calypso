!>@file  data_convert_by_zlib.f90
!!       module data_convert_by_zlib
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!
!> @brief BAse routines to compress and decompress data
!!
!!@verbatim
!!      subroutine defleate_endian_flag(zbuf)
!!      subroutine defleate_int8_vector_b(num, int8_dat, zbuf)
!!      subroutine defleate_1d_vector_b(num, real_dat, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine defleate_1d_character_b(num, chara_dat, zbuf)
!!        integer(kind = kint_gl), intent(in) :: num
!!        character(len=kchara), intent(in) :: chara_dat(num)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine defleate_characters_b(num, chara_dat, zbuf)
!!        integer(kind = kint_gl), intent(in) :: num
!!        character(len=1), intent(in) :: chara_dat(num)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine infleate_endian_flag(id_rank, iflag_swap, zbuf)
!!      subroutine infleate_int8_vector_b(num, int8_dat, zbuf)
!!      subroutine infleate_1d_vector_b(num, real_dat, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine infleate_1d_character_b(num, chara_dat, zbuf)
!!        integer(kind = kint_gl), intent(in) :: num
!!        character(len=kchara), intent(inout) :: chara_dat(num)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine infleate_characters_b(num, chara_dat, zbuf)
!!        integer(kind = kint_gl), intent(in) :: num
!!        character(len=1), intent(inout) :: chara_dat(num)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module data_convert_by_zlib
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_buffer_4_gzip
      use transfer_to_long_integers
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine defleate_endian_flag(zbuf)
!
      use gzip_defleate
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer, parameter :: int_dat(1) = (/i_UNIX/)
!
!
      zbuf%ilen_gzipped = 0
!
      call gzip_defleat_int4_once(1, int_dat, int(zbuf%ilen_gz),        &
     &    zbuf, zbuf%gzip_buf(1))
!
      end subroutine defleate_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine defleate_int8_vector_b(num, int8_dat, zbuf)
!
      use gzip_defleate
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist, ilen_tmp
      integer :: nline
      integer :: ilen_in
!
!
      zbuf%ilen_gz = int(dble(num*kint_gl)*1.01+24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
      zbuf%ilen_gzipped = 0
!
      ist = 0
      ilen_tmp = int(dble(huge_30) * 1.01 + 24,KIND(ilen_tmp))
      do
        nline = int(min((num - ist), huge_30/cast_long(kint_gl)))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
        call gzip_defleat_int8_once(nline, int8_dat(ist+1), ilen_in,    &
     &      zbuf, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
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
      use gzip_defleate
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist, ilen_tmp
      integer :: nline
      integer :: ilen_in
!
!
      zbuf%ilen_gz = int(dble(num*kreal)*1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
      zbuf%ilen_gzipped = 0
!
      ist = 0
      ilen_tmp = int(dble(huge_30) * 1.01 + 24,KIND(ilen_tmp))
      do
        nline = int(min((num - ist), huge_30/cast_long(kreal)))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
        call gzip_defleat_real_once(nline, real_dat(ist+1), ilen_in,    &
     &      zbuf, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
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
      use gzip_defleate
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist, ilen_tmp
      integer :: nline
      integer :: ilen_in, ilen_line
!
!
      zbuf%ilen_gz = int(dble(num*kchara)*1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
      zbuf%ilen_gzipped = 0
!
      ist = 0
      ilen_tmp = int(dble(huge_30) * 1.01 + 24,KIND(ilen_tmp))
      do
        nline = int(min((num - ist), huge_30/cast_long(kchara)))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
        ilen_line = nline * kchara
!
        call gzip_defleat_char_once(ilen_line, chara_dat(ist+1),        &
     &      ilen_in, zbuf, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      end subroutine defleate_1d_character_b
!
! -----------------------------------------------------------------------
!
      subroutine defleate_characters_b(num, chara_dat, zbuf)
!
      use gzip_defleate
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=1), intent(in) :: chara_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist, ilen_tmp
      integer :: ilen_in, ilen_line
!
!
      zbuf%ilen_gz = int(dble(num)*1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
      zbuf%ilen_gzipped = 0
!
      ist = 0
      ilen_tmp = int(dble(huge_30) * 1.01 + 24,KIND(ilen_tmp))
      do
        ilen_line = int(min((num - ist), huge_30))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
        call gzip_defleat_char_once(ilen_line, chara_dat(ist+1),        &
     &      ilen_in, zbuf, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
        ist = ist + ilen_line
        if(ist .ge. num) exit
      end do
!
      end subroutine defleate_characters_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine infleate_endian_flag(id_rank, iflag_swap, zbuf)
!
      use binary_IO
      use gzip_infleate
!
      integer, intent(in) :: id_rank
      integer, intent(inout) :: iflag_swap
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer :: int_dat(1)
      integer :: ilen_in
!
!
      zbuf%ilen_gzipped = 0
      ilen_in = int(zbuf%ilen_gz)
!
      call gzip_infleat_int4_once                                       &
     &   (ilen_in, zbuf%gzip_buf(1), ione, int_dat, zbuf)
      iflag_swap = endian_check(id_rank, int_dat(1))
!
      end subroutine infleate_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine infleate_int8_vector_b(num, int8_dat, zbuf)
!
      use gzip_infleate
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist, ilen_tmp
      integer :: nline
      integer :: ilen_in
!
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = int(dble(huge_30) * 1.01 + 24,KIND(ilen_tmp))
      do
        nline = int(min((num - ist), huge_30/cast_long(kint_gl)))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
        call gzip_infleat_int8_once                                     &
     &     (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),                &
     &      nline, int8_dat(ist+1), zbuf)
!
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      end subroutine infleate_int8_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine infleate_1d_vector_b(num, real_dat, zbuf)
!
      use gzip_infleate
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist, ilen_tmp
      integer :: nline
      integer :: ilen_in
!
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = int(dble(huge_30) * 1.01 + 24,KIND(ilen_tmp))
      do
        nline = int(min((num - ist), huge_30/cast_long(kreal)))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
        call gzip_infleat_real_once                                     &
     &     (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),                &
     &     nline, real_dat(ist+1), zbuf)
!
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      end subroutine infleate_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine infleate_1d_character_b(num, chara_dat, zbuf)
!
      use gzip_infleate
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist, ilen_tmp
      integer :: nline
      integer :: ilen_in, ilen_line
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
        call gzip_infleat_char_once                                     &
     &     (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),                &
     &      ilen_line, chara_dat(ist+1), zbuf)
!
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      end subroutine infleate_1d_character_b
!
! -----------------------------------------------------------------------
!
      subroutine infleate_characters_b(num, chara_dat, zbuf)
!
      use gzip_infleate
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=1), intent(inout) :: chara_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist, ilen_tmp
      integer :: ilen_in, ilen_line
!
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = int(dble(huge_30) * 1.01 + 24,KIND(ilen_tmp))
      do
        ilen_line = int(min((num - ist), huge_30))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
        call gzip_infleat_char_once                                     &
     &     (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),                &
     &      ilen_line, chara_dat(ist+1), zbuf)
!
        ist = ist + ilen_line
        if(ist .ge. num) exit
      end do
!
      end subroutine infleate_characters_b
!
! -----------------------------------------------------------------------
!
      end module data_convert_by_zlib
