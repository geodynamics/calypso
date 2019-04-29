!>@file  zlib_convert_text.f90
!!       module zlib_convert_text
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine defleate_characters(ilength, chara_dat, zbuf)
!!
!!      subroutine infleate_characters(ilength, text, zbuf)
!!      subroutine infleate_1word(ilength, word, zbuf)
!!      subroutine infleate_skip_header(ilength, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module zlib_convert_text
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use data_IO_to_textline
      use t_buffer_4_gzip
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine defleate_characters(ilength, chara_dat, zbuf)
!
      use data_IO_to_textline
!
      integer, intent(in) :: ilength
      character(len=ilength), intent(in) :: chara_dat
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer :: ilen_in, ilen_used
!
!
      zbuf%ilen_gz = int(dble(ilength) *1.01+24, KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
!
      ilen_in = int(zbuf%ilen_gz)
      call gzip_defleat_once(ilength, chara_dat, ilen_in,               &
     &    ilen_used, zbuf%gzip_buf(1))
      zbuf%ilen_gzipped = ilen_used
!
      end subroutine defleate_characters
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine infleate_characters(ilength, text, zbuf)
!
      use data_IO_to_textline
!
      integer, intent(in) :: ilength
      character(len=ilength), intent(inout) :: text
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer :: ilen_in, ilen_used
!
!
      ilen_in = int(zbuf%ilen_gz)
      call gzip_infleat_once(ilen_in, zbuf%gzip_buf(1),                 &
     &    ilength, text, ilen_used)
      zbuf%ilen_gzipped = ilen_used
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_characters
!
! -----------------------------------------------------------------------
!
      subroutine infleate_1word(ilength, word, zbuf)
!
      use field_data_IO
      use field_data_MPI_IO
!
      integer, intent(inout) :: ilength
      character(len=kchara), intent(inout) :: word
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer :: ilen_in, ilen_used
      integer(kind = kint) :: i
!
      character(len=1), allocatable :: textbuf(:)
      character(len=kchara) :: textbuf_c
!
!
      ilen_in = int(zbuf%ilen_gz)
      call gzip_infleat_once                                            &
     &   (ilen_in, zbuf%gzip_buf(1), kchara, textbuf_c, ilen_used)
      zbuf%ilen_gzipped = ilen_used
!
      call read_each_field_name_buffer(textbuf_c, word, ilength)
      ilength = ilength + 1
!      do i = 1, kchara
!        write(*,*) ilength, i, word(i:i),                              &
!     &       iachar(textbuf_c(i:i)), iachar(word(i:i))
!      end do
!
!      write(*,*) 'word', ilength, trim(word)
!
      allocate(textbuf(ilength))
      call gzip_infleat_once                                            &
     &   (ilen_in, zbuf%gzip_buf(1), ilength, textbuf(1), ilen_used)
      zbuf%ilen_gzipped = ilen_used
      deallocate(textbuf)
!
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_1word
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine infleate_skip_header(ilength, zbuf)
!
      integer, intent(in) :: ilength
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer :: ilen_in, ilen_used
      character(len=1), allocatable :: chara_dat(:)
!
!
      allocate(chara_dat(ilength))
!
      ilen_in = int(zbuf%ilen_gz)
      call gzip_infleat_once(ilen_in, zbuf%gzip_buf(1),                 &
     &    ilength, chara_dat(1), ilen_used)
      zbuf%ilen_gzipped = ilen_used
!
      deallocate(chara_dat)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_skip_header
!
! -----------------------------------------------------------------------
!
      end module zlib_convert_text
