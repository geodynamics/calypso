!>@file   gz_data_IO.f90
!!        module gz_data_IO
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!> @brief Gzipped data IO subroutines for various types
!!
!!@verbatim
!!      subroutine read_gz_multi_real(num, real_input, zbuf)
!!      subroutine read_gz_integer_stack(num, istack, ntot, zbuf)
!!      subroutine read_gz_multi_int(num, int_input, zbuf)
!!      subroutine read_gz_surf_group(is1, ntot, istack, item_sf, zbuf)
!!      subroutine read_gz_multi_int8(num, int8_input, zbuf)
!!        type(buffer_4_gzip), intent(inout):: zbuf
!!
!!      subroutine write_gz_surf_group(is1, ntot, istack, item_sf, zbuf)
!!      subroutine write_gz_multi_int_8i16(num, int_data, zbuf)
!!      subroutine write_gz_multi_int_10i8(num, int_data, zbuf)
!!      subroutine write_gz_multi_int_10i12(num, int_data, zbuf)
!!        type(buffer_4_gzip), intent(inout):: zbuf
!!
!!      subroutine write_gz_comment_string(comment, zbuf)
!!      subroutine gz_write_chara_nolf(chara_output, zbuf)
!!        type(buffer_4_gzip), intent(inout):: zbuf
!!@endverbatim
!
      module gz_data_IO
!
      use m_precision
      use m_constants
      use m_file_format_switch
      use t_buffer_4_gzip
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine read_gz_multi_real(num, real_input, zbuf)
!
      use gzip_file_access
      use skip_gz_comment
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_input(num)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, ist2, ied2
!
!
      if(num .le. 0) return
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) real_input(1:zbuf%num_word)
!
      if(num .gt. zbuf%num_word) then
        ist = zbuf%num_word
        do
          call get_one_line_text_from_gz(zbuf)
          ist2 = ist + 1
          ied2 = ist + zbuf%num_word
          ist = ied2
          read(zbuf%fixbuf(1),*) real_input(:ied2)
          if(ist .ge. num) exit
        end do
      end if
!
      end subroutine read_gz_multi_real
!
!------------------------------------------------------------------
!
      subroutine read_gz_integer_stack(num, istack, ntot, zbuf)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: istack(0:num)
      integer(kind = kint), intent(inout) :: ntot
      type(buffer_4_gzip), intent(inout):: zbuf
!
!
      istack(0) = 0
      call read_gz_multi_int(num, istack(1), zbuf)
      ntot = istack(num)
!
      end subroutine read_gz_integer_stack
!
!------------------------------------------------------------------
!
      subroutine read_gz_multi_int(num, int_input, zbuf)
!
      use gzip_file_access
      use skip_gz_comment
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_input(num)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, ist2, ied2
!
!
      if(num .le. 0) return
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) int_input(1:zbuf%num_word)
!
      if(num .gt. zbuf%num_word) then
        ist = zbuf%num_word
        do
          call get_one_line_text_from_gz(zbuf)
          ist2 = ist + 1
          ied2 = ist + zbuf%num_word
          ist = ied2
          read(zbuf%fixbuf(1),*) int_input(ist2:ied2)
          if(ist .ge. num) exit
        end do
      end if
!
      end subroutine read_gz_multi_int
!
!------------------------------------------------------------------
!
      subroutine read_gz_surf_group(is1, ntot, istack, item_sf, zbuf)
!
      use gzip_file_access
      use skip_gz_comment
!
      integer(kind = kint), intent(in) :: is1, ntot
      integer(kind = kint), intent(in) :: istack(0:1)
      integer(kind = kint), intent(inout) :: item_sf(2,ntot)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, ist2, ied2
!
!
      if((istack(1) - istack(0)) .le. 0) return
!
      call skip_gz_comment_get_nword(zbuf)
      ist2 = istack(0) + 1
      ied2 = istack(0) + zbuf%num_word
      read(zbuf%fixbuf(1),*) item_sf(is1,ist2:ied2)
!
      if((istack(1) - istack(0)) .gt. zbuf%num_word) then
        ist = istack(0) + zbuf%num_word
        do
          call get_one_line_text_from_gz(zbuf)
          ist2 = ist + 1
          ied2 = ist + zbuf%num_word
          ist = ied2
          read(zbuf%fixbuf(1),*) item_sf(is1,ist2:ied2)
          if(ist .ge. istack(1)) exit
        end do
      end if
!
      end subroutine read_gz_surf_group
!
!------------------------------------------------------------------
!
      subroutine read_gz_multi_int8(num, int8_input, zbuf)
!
      use gzip_file_access
      use skip_gz_comment
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_input(num)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, ist2, ied2
!
!
      if(num .le. 0) return
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) int8_input(1:zbuf%num_word)
!
      if(num .gt. zbuf%num_word) then
        ist = zbuf%num_word
        do
          call get_one_line_text_from_gz(zbuf)
          ist2 = ist + 1
          ied2 = ist + zbuf%num_word
          ist = ied2
          read(zbuf%fixbuf(1),*) int8_input(ist2:ied2)
          if(ist .ge. num) exit
        end do
      end if
!
      end subroutine read_gz_multi_int8
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_gz_surf_group(is1, ntot, istack, item_sf, zbuf)
!
      use gzip_file_access
!
      integer(kind = kint), intent(in) :: is1, ntot
      integer(kind = kint), intent(in) :: istack(0:1)
      integer(kind = kint), intent(in) :: item_sf(2,ntot)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, n
      character(len=kchara) :: fmt_txt
!
!
      ist = istack(0)
      do
        n = min(istack(1)-ist-ione,iseven) + 1
        write(fmt_txt,'(a1,i2,a8)') '(', n, 'i16,2a1)'
        write(zbuf%fixbuf(1),fmt_txt) item_sf(is1,ist+1:ist+n),         &
     &                               char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
        ist = ist + n
        if(ist .ge. istack(1)) exit
      end do
!
      end subroutine write_gz_surf_group
!
!------------------------------------------------------------------
!
      subroutine write_gz_multi_int_8i16(num, int_data, zbuf)
!
      use gzip_file_access
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_data(num)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, n
      character(len=kchara) :: fmt_txt
!
!
      ist = 0
      do
        n = min(num-ist-ione,iseven) + 1
        write(fmt_txt,'(a1,i2,a8)') '(', n, 'i16,2a1)'
        write(zbuf%fixbuf(1),fmt_txt) int_data(ist+1:ist+n),            &
     &                               char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
        ist = ist + n
        if(ist .ge. num) exit
      end do
!
      end subroutine write_gz_multi_int_8i16
!
!------------------------------------------------------------------
!
      subroutine write_gz_multi_int_10i8(num, int_data, zbuf)
!
      use gzip_file_access
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_data(num)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, n
      character(len=kchara) :: fmt_txt
!
!
      ist = 0
      do
        n = min(num-ist-ione,inine) + 1
        write(fmt_txt,'(a1,i3,a7)') '(', n, 'i8,2a1)'
        write(zbuf%fixbuf(1),fmt_txt) int_data(ist+1:ist+n),            &
     &                               char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
        ist = ist + n
        if(ist .ge. num) exit
      end do
!
      end subroutine write_gz_multi_int_10i8
!
!------------------------------------------------------------------
!
      subroutine write_gz_multi_int_10i12(num, int_data, zbuf)
!
      use gzip_file_access
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_data(num)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, n
      character(len=kchara) :: fmt_txt
!
!
      ist = 0
      do
        n = min(num-ist-ione,inine) + 1
        write(fmt_txt,'(a1,i3,a8)') '(', n, 'i12,2a1)'
        write(zbuf%fixbuf(1),fmt_txt) int_data(ist+1:ist+n),            &
     &                               char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
        ist = ist + n
        if(ist .ge. num) exit
      end do
!
      end subroutine write_gz_multi_int_10i12
!
!------------------------------------------------------------------
!
      subroutine write_gz_comment_string(comment, zbuf)
!
      use gzip_file_access
!
      character(len=*), intent(in)  ::  comment
      type(buffer_4_gzip), intent(inout):: zbuf
!
!
      write(zbuf%fixbuf(1),'(a,2a1)') comment, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      end subroutine write_gz_comment_string
!
!------------------------------------------------------------------
!
      subroutine gz_write_chara_nolf(chara_output, zbuf)
!
      use gzip_file_access
!
      character(len=kchara), intent(in) :: chara_output
      type(buffer_4_gzip), intent(inout):: zbuf
!
!
      write(zbuf%fixbuf(1),'(2a,a1)') trim(chara_output), '    ',       &
     &                               CHAR(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      end subroutine gz_write_chara_nolf
!
! ----------------------------------------------------------------------
!
      end module gz_data_IO
