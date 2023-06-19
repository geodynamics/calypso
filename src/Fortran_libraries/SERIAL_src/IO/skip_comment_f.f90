!>@file   skip_comment_f.f90
!!@brief  module skip_comment_f
!!
!!@authorH.Matsui and H.Okuda
!!@date Programmed  H. Matsui in  Feb. 2001 
!!@date Modified   H. Matsui in  Oct. 2013 
!
!> @brief subroutines to find comment lines in data
!!
!!@verbatim
!!      subroutine skip_comment(id_file, character_4_read, iend)
!!        integer (kind=kint), intent(in) :: id_file
!!        character(len=255), intent(inout) :: character_4_read
!!        integer (kind=kint), intent(inout) :: iend
!!
!!      subroutine read_one_line_from_stream(id_file,                   &
!!     &          lenghbuf, num_word, nchara_read, tbuf)
!!      subroutine skip_comment_from_stream(id_file,                    &
!!     &          lenghbuf, num_word, nchara_read, tbuf)
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: lenghbuf
!!        integer(kind = kint), intent(inout) :: num_word, nchara_read
!!        character(len=lenghbuf), intent(inout) :: tbuf
!!
!!      subroutine count_field_by_comma(id_file, charabuf,              &
!!     &          ncomp, field_name)
!!      subroutine read_stack_array(character_4_read, id_file, num,     &
!!     &                            istack_array, iend)
!!        integer (kind=kint), intent(in) :: id_file, num
!!        integer (kind=kint), intent(inout) :: istack_array(0:num)
!!        integer (kind=kint), intent(inout) :: iend
!!
!!      subroutine change_2_upper_case(string)
!!      subroutine change_2_lower_case(string)
!!      character(len=kchara) function fill_from_null(input)
!!
!!      real(kind = kreal) function compare_data(data, ref)
!!        real(kind = kreal), intent(in) :: data, ref
!!           Compare two reals
!!              if ref = 0.0 then compare_data = data
!!              else compare_data = abs((data - ref) / ref)
!!      logical function cmp_no_case(cmp_chara, ref_chara)
!!          if ref_chara and cmp_chara are same ignoreing case,
!!          returns 1, othewwise returns 0
!!      logical function yes_flag(control)
!!      logical function no_flag(control)
!!@endverbatim
!
      module skip_comment_f
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine skip_comment(id_file, character_4_read, iend)
!
      integer (kind=kint), intent(in) :: id_file
      character(len=255), intent(inout) :: character_4_read
      integer (kind=kint), intent(inout) :: iend
      character(len=1) :: detect_comment
!
      iend = 0
  10  continue
        read(id_file,'(a)',end=99) character_4_read
        read(character_4_read,*,end=10)  detect_comment
      if(detect_comment.eq.'!' .or. detect_comment.eq.'#') go to 10
!
      return
!
  99  continue
      write(*,*) 'File is probably incorrect.'
      iend = 99
      return
!
      end subroutine skip_comment
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_one_line_from_stream(id_file,                     &
     &          lenghbuf, num_word, nchara_read, tbuf)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: lenghbuf
!
      integer(kind = kint), intent(inout) :: num_word, nchara_read
      character(len=lenghbuf), intent(inout) :: tbuf
!
      integer(kind = kint) :: i
!
!
      do i = 1, lenghbuf
        read(id_file, end=99,err=99) tbuf(i:i)
        if(tbuf(i:i) .eq. char(10)) exit
      end do
      nchara_read = i

      num_word = 0
      if(tbuf(1:1) .ne. char(32)) num_word = num_word + 1
      do i = 2, nchara_read
        if(tbuf(i-1:i-1) .eq. char(32)                                  &
     &         .and. tbuf(i:i) .ne. char(32)) num_word = num_word + 1
      end do
      return
!
  99  continue
      nchara_read = -1
      num_word = -1
!
      end subroutine read_one_line_from_stream
!
! -----------------------------------------------------------------------
!
      subroutine skip_comment_from_stream(id_file,                      &
     &          lenghbuf, num_word, nchara_read, tbuf)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: lenghbuf
!
      integer(kind = kint), intent(inout) :: num_word, nchara_read
      character(len=lenghbuf), intent(inout) :: tbuf
!
      character(len=1) :: detect_comment
!
  10  continue
        call read_one_line_from_stream(id_file,                         &
     &      lenghbuf, num_word, nchara_read, tbuf)

        if(nchara_read .lt. 0) return
        if(nchara_read .eq. 0) go to 10
!
        read(tbuf,*,end=10)  detect_comment
      if(detect_comment.eq.'!' .or. detect_comment.eq.'#') go to 10
!
      end subroutine skip_comment_from_stream
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_field_by_comma(id_file, charabuf,                &
     &          ncomp, field_name)
!
      integer(kind = kint), parameter :: lengthbuf = 8192
      integer (kind=kint), intent(in) ::   id_file
      integer (kind=kint), intent(inout) :: ncomp
      character(len=kchara), intent(inout) ::    field_name(255)
      character(len=lengthbuf), intent(inout) :: charabuf
!
      character(len=1) :: each_chara(1)
      integer (kind=kint) :: icou, i
!
!
      ncomp = 0
      do
        icou = 0
        read(id_file,'(a)') charabuf
!
        read(charabuf,*)  each_chara(1)
        if( each_chara(1).eq.'0' .or. each_chara(1).eq.'1'              &
     &      .or. each_chara(1).eq.'2' .or. each_chara(1).eq.'3'         &
     &      .or. each_chara(1).eq.'4' .or. each_chara(1).eq.'5'         &
     &      .or. each_chara(1).eq.'6' .or. each_chara(1).eq.'7'         &
     &      .or. each_chara(1).eq.'8' .or. each_chara(1).eq.'9'         &
     &      .or. each_chara(1).eq.'+' .or. each_chara(1).eq.'-') exit
        if( each_chara(1).eq.'!' .or. each_chara(1).eq.'#') cycle
!
        do i = 1, lengthbuf
          if(charabuf(i:i) .eq. ',') icou = icou + 1
        end do
        ncomp = ncomp + icou
!
        read(charabuf,*) field_name(ncomp-icou+1:ncomp)
      end do
!
      end subroutine count_field_by_comma
!
!-----------------------------------------------------------------------
!
      subroutine read_stack_array(character_4_read, id_file, num,       &
     &                            istack_array, iend)
!
      integer (kind=kint), intent(in) :: id_file, num
      integer (kind=kint), intent(inout) :: istack_array(0:num)
      integer (kind=kint), intent(inout) :: iend
!
      character(len=255) :: character_4_read
!
      integer (kind=kint) :: i, ii
!
!
      istack_array(0:num) = -1
!
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*,end=51) istack_array
   51 continue
!
!    Check number of read data
!
      ii = num + 1
      do i = num, 1, -1
        if ( istack_array(i-1) .eq. -1 ) ii = i
      end do
!
!   shift stack array
!
      do i = ii-1, 1,-1
        istack_array(i) = istack_array(i-1)
      end do
      istack_array(0) = 0
!
!    read reast of array
!
      if ( ii .le. num ) then
       read(id_file,*)  (istack_array(i),i=ii, num)
      end if
!
      end subroutine read_stack_array
!
!-----------------------------------------------------------------------
!
      subroutine change_2_upper_case(string)
!
      character(len=kchara), intent(inout) :: string
      integer(kind = kint) :: i, len
!
!
      len = len_trim(string)
      do i = 1, len
        if (string(i:i) .ge. 'a' .and. string(i:i) .le. 'z') then 
          string(i:i) = char(ichar(string(i:i)) - 32)
        end if
      end do
!
      end subroutine change_2_upper_case
!
!-----------------------------------------------------------------------
!
      subroutine change_2_lower_case(string)
!
      character(len=kchara), intent(inout) :: string
      integer(kind = kint) :: i, len
!
!
      len = len_trim(string)
      do i = 1, len
        if (string(i:i) .ge. 'A' .and. string(i:i) .le. 'Z') then 
          string(i:i) = char(ichar(string(i:i)) + 32)
        end if
      end do
!
      end subroutine change_2_lower_case
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function fill_from_null(input)
!
      character(len=*), intent(in) :: input
!
      integer(kind = kint) :: icou, inull
!
      inull = kchara + 1
      do icou = 1, kchara
        fill_from_null(icou:icou) = input(icou:icou)
        if(input(icou:icou) .eq. char(0)) then
          inull = icou
          exit
        end if
      end do
      do icou = inull, kchara
        fill_from_null(icou:icou) = char(32)
      end do
!
      end function fill_from_null
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      logical function cmp_no_case(cmp_chara, ref_chara)
!
      character(len=*), intent(in) :: ref_chara
      character(len=kchara), intent(in) :: cmp_chara
      character(len=kchara)  :: ref_tmp, cmp_tmp
      integer(kind = kint) :: len
!
!
      len = len_trim(ref_chara)
      if(len_trim(cmp_chara) .ne. len) then
        cmp_no_case = .false.
        return
      end if
!
      write(ref_tmp,'(a)')  trim(ref_chara)
      write(cmp_tmp,'(a)')  trim(cmp_chara)
      call change_2_lower_case(ref_tmp)
      call change_2_lower_case(cmp_tmp)
!
      cmp_no_case = .false.
      if(ref_tmp .eq. cmp_tmp) cmp_no_case = .true.
      return
!
      end function cmp_no_case
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function compare_data(data, ref)
!
      real(kind = kreal), intent(in) :: data, ref
!
      if(ref .eq. 0.0d0) then
        compare_data = abs(data)
      else
        compare_data = abs((data - ref) / ref)
      end if
!
      end function compare_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      logical function yes_flag(control)
!
      character(len=kchara), intent(in) :: control
!
!
      if(cmp_no_case(control, 'yes') .or. cmp_no_case(control, '1')     &
     &     .or. cmp_no_case(control, 'on')                              &
     &     .or. cmp_no_case(control, '.true.')) then
        yes_flag = .true.
      else
        yes_flag = .false.
      end if
!
      end function yes_flag
!
!-----------------------------------------------------------------------
!
      logical function no_flag(control)
!
      character(len=kchara), intent(in) :: control
!
!
      if(cmp_no_case(control, 'no') .or. cmp_no_case(control, '0')      &
     &    .or. cmp_no_case(control, 'off')                              &
     &    .or. cmp_no_case(control, '.false.') ) then
        no_flag = .true.
      else
        no_flag = .false.
      end if
!
      end function no_flag
!
!-----------------------------------------------------------------------
!
      end module skip_comment_f
