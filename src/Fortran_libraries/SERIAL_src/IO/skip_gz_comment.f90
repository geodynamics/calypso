!
!      module skip_gz_comment
!
!     Written by H. Matsui on July, 2007
!
!      subroutine skip_gz_comment_int(int_input)
!      subroutine skip_gz_comment_int2(int_input, int_input2)
!      subroutine skip_gz_comment_real(real_input)
!      subroutine skip_gz_comment_real2(real_input, real_input2)
!      subroutine skip_gz_comment_chara(chara_input)
!
!      subroutine read_gz_multi_real(num, real_input)
!      subroutine read_gz_multi_int(num, int_input)
!      subroutine write_gz_multi_int_8i10(num, int_output)
!      subroutine write_gz_multi_int_10i8(num, int_output)
!      subroutine write_gz_multi_int_10i12(num, int_output)
!
!      subroutine write_gz_comment_string(comment)
!
      module skip_gz_comment
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), parameter :: nbuf = 65535
      integer (kind =kint) :: num_word, nchara
      character(len=nbuf) :: textbuf, tbuf2
      character(len=1), private :: chara_flag
!
      private :: nchara, tbuf2
      private :: skip_gz_comment_get_nword
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_int(int_input)
!
      integer(kind = kint), intent(inout) :: int_input
!
!
      call skip_gz_comment_get_nword
      read(textbuf,*) int_input
!
      end subroutine skip_gz_comment_int
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_int2(int_input, int_input2)
!
      integer(kind = kint), intent(inout) :: int_input, int_input2
!
!
      call skip_gz_comment_get_nword
      read(textbuf,*) int_input, int_input2
!
      end subroutine skip_gz_comment_int2
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_real(real_input)
!
      real(kind = kreal), intent(inout) :: real_input
!
      call skip_gz_comment_get_nword
      read(textbuf,*) real_input
!
      end subroutine skip_gz_comment_real
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_real2(real_input, real_input2)
!
      real(kind = kreal), intent(inout) :: real_input, real_input2
!
      call skip_gz_comment_get_nword
      read(textbuf,*) real_input, real_input2
!
      end subroutine skip_gz_comment_real2
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_chara(chara_input)
!
      character(len = kchara), intent(inout) :: chara_input
      character(len=kchara) :: charaint, fmtchara, tmpchara
!
!
      call skip_gz_comment_get_nword
!
      write(charaint,'(i8)') min(nchara-1,kchara)
      write(fmtchara,'(a2,a,a1)')                                       &
     &          '(a', trim(ADJUSTL(charaint)),')'

      write(tmpchara,fmtchara) textbuf
      write(chara_input,'(a)') ADJUSTL(tmpchara)
!
      end subroutine skip_gz_comment_chara
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_get_nword
!
!
      do
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        if(nchara .le. 1) cycle
!
        write(chara_flag,'(a1)',err=1) adjustl(textbuf)
        if(chara_flag.eq.char(10) .or. chara_flag.eq.char(13)) cycle
        if(chara_flag.ne.'#' .and. chara_flag.ne.'!') exit
   1    continue
      end do
!
!      write(charaint,'(i8)') nchara-1
!      write(fmtchara,'(a2,a,a4)')  '(a', trim(ADJUSTL(charaint)),',a1)'
!      write(tbuf2,fmtchara) textbuf, char(32)
!      do i = 1, nchara+1
!        write(*,*) i, ichar(textbuf(i:i)), ichar(tbuf2(i:i)),       &
!     &              textbuf(i:i), tbuf2(i:i)
!      end do
!
      end subroutine skip_gz_comment_get_nword
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_gz_multi_real(num, real_input)
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_input(num)
!
      integer(kind = kint) :: ist
!
!
      if(num .le. 0) return
!
      call skip_gz_comment_get_nword
      read(textbuf,*) real_input(1:num_word)
!
      if(num .gt. num_word) then
        ist = num_word
        do
          call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
          read(textbuf,*) real_input(ist+1:ist+num_word)
          ist = ist + num_word
          if(ist .ge. num) exit
        end do
      end if
!
      end subroutine read_gz_multi_real
!
!------------------------------------------------------------------
!
      subroutine read_gz_multi_int(num, int_input)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_input(num)
!
      integer(kind = kint) :: ist
!
!
      if(num .le. 0) return
!
      call skip_gz_comment_get_nword
      read(textbuf,*) int_input(1:num_word)
!
      if(num .gt. num_word) then
        ist = num_word
        do
          call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
          read(textbuf,*) int_input(ist+1:ist+num_word)
          ist = ist + num_word
          if(ist .ge. num) exit
        end do
      end if
!
      end subroutine read_gz_multi_int
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_gz_multi_int_8i10(num, int_output)
!
      integer(kind = kint) :: num
      integer(kind = kint) :: int_output(num)
!
      integer(kind = kint) :: ist, n
      character(len=kchara) :: fmt_txt
!
!
      ist = 0
      do
        n = min((num-ist-1),7) + 1
        write(fmt_txt,'(a1,i2,a7)') '(', n, 'i10,a1)'
        write(textbuf,fmt_txt) int_output(ist+1:ist+n), char(0)
        call write_compress_txt(nbuf, textbuf)
        ist = ist + n
        if(ist .ge. num) exit
      end do
!
      end subroutine write_gz_multi_int_8i10
!
!------------------------------------------------------------------
!
      subroutine write_gz_multi_int_10i8(num, int_output)
!
      integer(kind = kint) :: num
      integer(kind = kint) :: int_output(num)
!
      integer(kind = kint) :: ist, n
      character(len=kchara) :: fmt_txt
!
!
      ist = 0
      do
        n = min((num-ist-1),9) + 1
        write(fmt_txt,'(a1,i3,a6)') '(', n, 'i8,a1)'
        write(textbuf,fmt_txt) int_output(ist+1:ist+n), char(0)
        call write_compress_txt(nbuf, textbuf)
        ist = ist + n
        if(ist .ge. num) exit
      end do
!
      end subroutine write_gz_multi_int_10i8
!
!------------------------------------------------------------------
!
      subroutine write_gz_multi_int_10i12(num, int_output)
!
      integer(kind = kint) :: num
      integer(kind = kint) :: int_output(num)
!
      integer(kind = kint) :: ist, n
      character(len=kchara) :: fmt_txt
!
!
      ist = 0
      do
        n = min((num-ist-1),9) + 1
        write(fmt_txt,'(a1,i3,a7)') '(', n, 'i12,a1)'
        write(textbuf,fmt_txt) int_output(ist+1:ist+n), char(0)
        call write_compress_txt(nbuf, textbuf)
        ist = ist + n
        if(ist .ge. num) exit
      end do
!
      end subroutine write_gz_multi_int_10i12
!
!------------------------------------------------------------------
!
      subroutine write_gz_comment_string(comment)
!
      character*(*), intent(in)  ::  comment
!
!
      write(textbuf,'(a,a1)') comment, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      end subroutine write_gz_comment_string
!
!------------------------------------------------------------------
!
      end module skip_gz_comment
