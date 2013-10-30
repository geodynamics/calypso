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
!!      subroutine skip_comment(character_4_read,id_file)
!!      subroutine count_field_by_comma(id_file, charabuf,              &
!!     &          ncomp, field_name)
!!      subroutine read_stack_array(character_4_read, id_file, num,     &
!!     &          istack_array)
!!
!!      subroutine change_2_upper_case(string)
!!      subroutine change_2_lower_case(string)
!!      integer function compare_ignore_cases(ref_chara, cmp_chara)
!!          if ref_chara and cmp_chara are same ignoreing case,
!!          returns 1, othewwise returns 0
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
      subroutine skip_comment(character_4_read,id_file)
!
       integer (kind=kint), intent(in) :: id_file
       character(len=255), intent(inout) :: character_4_read
       character(len=1) :: detect_comment
!
   10 continue
       read(id_file,'(a)') character_4_read
       read(character_4_read,*,end=10)  detect_comment
      if ( detect_comment.eq.'!' .or. detect_comment.eq.'#') go to 10
!
      return
      end subroutine skip_comment
!
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
     &          istack_array)
!
       integer (kind=kint), intent(in) :: id_file, num
!
       integer (kind=kint), intent(inout) :: istack_array(0:num)
       character(len=255) :: character_4_read
!
       integer (kind=kint) :: i, ii
!
!
        istack_array(0:num) = -1
!
        call skip_comment(character_4_read,id_file)
        read(character_4_read,*,end=51) istack_array
   51   continue
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
      integer function compare_ignore_cases(ref_chara, cmp_chara)
!
      character*(*), intent(in) :: ref_chara
      character(len=kchara), intent(in) :: cmp_chara
      character(len=kchara)  :: ref_tmp, cmp_tmp
      integer(kind = kint) :: len
!
!
      len = len_trim(ref_chara)
      if(len_trim(cmp_chara) .ne. len) then
        compare_ignore_cases = 0
        return
      end if
!
      write(ref_tmp,'(a)')  ref_chara
      write(cmp_tmp,'(a)')  cmp_chara
      call change_2_lower_case(ref_tmp)
      call change_2_lower_case(cmp_tmp)
!
      compare_ignore_cases = 0
      if(ref_tmp .eq. cmp_tmp) compare_ignore_cases = 1
      return
!
      end function compare_ignore_cases
!
!-----------------------------------------------------------------------
!
      end module skip_comment_f
