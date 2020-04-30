!>@file   skip_gz_comment.f90
!!        module skip_gz_comment
!!
!! @author H. Matsui
!! @date   Programmed in July, 2007
!!
!!
!> @brief Skip comment line in gzipped text file
!!
!!@verbatim
!!      integer function length_of_c_text(text)
!!
!!      subroutine open_wt_gzfile_a(gzip_name, zbuf)
!!      subroutine open_ad_gzfile_a(gzip_name, zbuf)
!!      subroutine open_rd_gzfile_a(gzip_name, zbuf)
!!      subroutine close_gzfile_a(zbuf)
!!        type(buffer_4_gzip), intent(inout):: zbuf
!!
!!      subroutine skip_gz_comment_int(int_input, zbuf)
!!      subroutine skip_gz_comment_int2(int_input, int_input2, zbuf)
!!      subroutine skip_gz_comment_int8_int(i8_input, int_input2, zbuf)
!!      subroutine skip_gz_comment_real(real_input, zbuf)
!!      subroutine skip_gz_comment_real2(real_input, real_input2, zbuf)
!!      subroutine skip_gz_comment_chara(chara_input, zbuf)
!!      subroutine skip_gz_comment_chara_int                            &
!!     &         (chara_input, int_input, zbuf)
!!      subroutine skip_gz_comment_chara_lint                           &
!!     &         (chara_input, int8_input, zbuf)
!!        type(buffer_4_gzip), intent(inout):: zbuf
!!
!!      subroutine skip_gz_comment_get_nword(zbuf)
!!        type(buffer_4_gzip), intent(inout):: zbuf
!!@endverbatim
!
      module skip_gz_comment
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
      integer function length_of_c_text(text)
      character(len=*), intent(in) ::  text
      integer :: i
!
      length_of_c_text = -1
      do i = 1, len(text)
        if(text(i:i) .eq. char(0)) then
          length_of_c_text = i
          exit
        end if
      end do
      end function length_of_c_text
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine open_wt_gzfile_a(gzip_name, zbuf)
!
      use gzip_file_access
!
      character(len = kchara), intent(in) :: gzip_name
      type(buffer_4_gzip) , intent(inout):: zbuf
!
!
      call alloc_fixbuffer_for_zlib(zbuf)
      call open_wt_gzfile_f(gzip_name, zbuf)
!
      end subroutine open_wt_gzfile_a
!
! ----------------------------------------------------------------------
!
      subroutine open_ad_gzfile_a(gzip_name, zbuf)
!
      use gzip_file_access
!
      character(len = kchara), intent(in) :: gzip_name
      type(buffer_4_gzip) , intent(inout):: zbuf
!
!
      call alloc_fixbuffer_for_zlib(zbuf)
      call open_ad_gzfile_f(gzip_name, zbuf)
!
      end subroutine open_ad_gzfile_a
!
! ----------------------------------------------------------------------
!
      subroutine open_rd_gzfile_a(gzip_name, zbuf)
!
      use gzip_file_access
!
      character(len = kchara), intent(in) :: gzip_name
      type(buffer_4_gzip) , intent(inout):: zbuf
!
!
      call alloc_fixbuffer_for_zlib(zbuf)
      call open_rd_gzfile_f(gzip_name, zbuf)
!
      end subroutine open_rd_gzfile_a
!
! ----------------------------------------------------------------------
!
      subroutine close_gzfile_a(zbuf)
!
      use gzip_file_access
!
      type(buffer_4_gzip) , intent(inout):: zbuf
!
      call close_gzfile_b()
      call dealloc_fixbuffer_for_zlib(zbuf)
!
      end subroutine close_gzfile_a
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_int(int_input, zbuf)
!
      integer(kind = kint), intent(inout) :: int_input
      type(buffer_4_gzip) , intent(inout):: zbuf
!
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) int_input
!
      end subroutine skip_gz_comment_int
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_int2(int_input, int_input2, zbuf)
!
      integer(kind = kint), intent(inout) :: int_input, int_input2
      type(buffer_4_gzip), intent(inout):: zbuf
!
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) int_input, int_input2
!
      end subroutine skip_gz_comment_int2
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_int8_int(i8_input, int_input2, zbuf)
!
      integer(kind = kint_gl), intent(inout) :: i8_input
      integer(kind = kint), intent(inout) :: int_input2
      type(buffer_4_gzip), intent(inout):: zbuf
!
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) i8_input, int_input2
!
      end subroutine skip_gz_comment_int8_int
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_real(real_input, zbuf)
!
      real(kind = kreal), intent(inout) :: real_input
      type(buffer_4_gzip), intent(inout):: zbuf
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) real_input
!
      end subroutine skip_gz_comment_real
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_real2(real_input, real_input2, zbuf)
!
      real(kind = kreal), intent(inout) :: real_input, real_input2
      type(buffer_4_gzip), intent(inout):: zbuf
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) real_input, real_input2
!
      end subroutine skip_gz_comment_real2
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_chara(chara_input, zbuf)
!
      character(len = kchara), intent(inout) :: chara_input
      type(buffer_4_gzip), intent(inout):: zbuf
!
      character(len=kchara) :: charaint, fmtchara, tmpchara
!
!
      call skip_gz_comment_get_nword(zbuf)
!
      write(charaint,'(i8)') min(int(zbuf%len_used - 1), int(kchara))
      write(fmtchara,'(a2,a,a1)')                                       &
     &          '(a', trim(ADJUSTL(charaint)),')'

      write(tmpchara,fmtchara) zbuf%fixbuf(1)
      write(chara_input,'(a)') ADJUSTL(tmpchara)
!
      end subroutine skip_gz_comment_chara
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_chara_int                              &
     &         (chara_input, int_input, zbuf)
!
      character(len = kchara), intent(inout) :: chara_input
      integer(kind = kint), intent(inout) :: int_input
      type(buffer_4_gzip), intent(inout):: zbuf
!
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) chara_input, int_input
!
      end subroutine skip_gz_comment_chara_int
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_chara_lint                             &
     &         (chara_input, int8_input, zbuf)
!
      character(len = kchara), intent(inout) :: chara_input
      integer(kind = kint_gl), intent(inout) :: int8_input
      type(buffer_4_gzip), intent(inout):: zbuf
!
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) chara_input, int8_input
!
      end subroutine skip_gz_comment_chara_lint
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_get_nword(zbuf)
!
      use gzip_file_access
!
      type(buffer_4_gzip), intent(inout):: zbuf
      character(len=1) :: chara_flag
!      character(len=65535) :: tbuf2
!
      do
        call get_one_line_text_from_gz(zbuf)
        if(zbuf%len_used .le. 1) cycle
!
        write(chara_flag,'(a1)',err=1) adjustl(zbuf%fixbuf(1))
        if(chara_flag.eq.char(10) .or. chara_flag.eq.char(13)) cycle
        if(chara_flag.ne.'#' .and. chara_flag.ne.'!') exit
   1    continue
      end do
!
!      write(charaint,'(i8)') zbuf%len_used - 1
!      write(fmtchara,'(a2,a,a4)')  '(a', trim(ADJUSTL(charaint)),',a1)'
!      write(tbuf2,fmtchara) zbuf%fixbuf(1), char(32)
!      do i = 1, zbuf%len_used + 1
!        write(*,*) i, ichar(zbuf%fixbuf(1)(i:i)), ichar(tbuf2(i:i)),   &
!     &              zbuf%fixbuf(1)(i:i), tbuf2(i:i)
!      end do
!
      end subroutine skip_gz_comment_get_nword
!
!------------------------------------------------------------------
!
      end module skip_gz_comment
