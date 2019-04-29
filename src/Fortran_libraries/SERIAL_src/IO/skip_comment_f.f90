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
!!
!!      logical function cmp_no_case(cmp_chara, ref_chara)
!!          if ref_chara and cmp_chara are same ignoreing case,
!!          returns 1, othewwise returns 0
!!      logical function yes_flag(control)
!!      logical function no_flag(control)
!!
!!      integer(kind = kint) function iflag_divide(charaname)
!!      integer(kind = kint) function max_len_of_charaarray(num, carray)
!!      subroutine write_ctl_chara_cont(id_file, charaname)
!!      subroutine write_ctl_chara_lf(id_file, charaname)
!!      subroutine write_spaces(id_file, nspace)
!!      subroutine write_space_4_parse(id_file, level)
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
!-----------------------------------------------------------------------
!
      integer(kind = kint) function iflag_divide(charaname)
!
      character(len=kchara), intent(in) :: charaname
!
      integer(kind = kint) :: i
!
      iflag_divide = 0
      do i = 1, len_trim(charaname)
        if(charaname(i:i).eq.'/' .or. charaname(i:i).eq.','             &
     &     .or. charaname(i:i).eq.';') then
          iflag_divide = 1
          exit
        end if
      end do
!
      end function iflag_divide
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function max_len_of_charaarray(num, carray)
!
      integer(kind = kint), intent(in) :: num
      character(len = kchara), intent(in) :: carray(num)
!
      integer(kind = kint) :: i, ilen, maxlen
!
      maxlen = 0
      do i = 1, num
        ilen = len_trim(carray(i)) + 2*iflag_divide(carray(i))
        maxlen = max(maxlen,ilen)
      end do
      max_len_of_charaarray = maxlen
!
      end function max_len_of_charaarray
!
! ----------------------------------------------------------------------
!
      subroutine write_ctl_chara_cont(id_file, charaname)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: charaname
!
!
      if(iflag_divide(charaname) .gt. 0) then
        write(id_file,'(a1,a,a1,a2)',advance='no')                      &
     &                char(39), trim(charaname), char(39), '  '
      else
        write(id_file,'(a,a2)',advance='no') trim(charaname), '  '
      end if
!
      end subroutine write_ctl_chara_cont
!
! ----------------------------------------------------------------------
!
      subroutine write_ctl_chara_lf(id_file, charaname)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: charaname
!
!
      if(iflag_divide(charaname) .gt. 0) then
        write(id_file,'(a1,a,a1)') char(39), trim(charaname), char(39)
      else
        write(id_file,'(a)') trim(charaname)
      end if
!
      end subroutine write_ctl_chara_lf
!
! ----------------------------------------------------------------------
!
      subroutine write_spaces(id_file, nspace)
!
      integer(kind = kint), intent(in) :: id_file, nspace
      integer(kind = kint) :: i
!
!
      do i = 1, nspace
        write(id_file,'(a1)',advance='no') char(32)
      end do
!
      end subroutine write_spaces
!
! ----------------------------------------------------------------------
!
      subroutine write_space_4_parse(id_file, level)
!
      integer(kind = kint), intent(in) :: id_file, level
!
      call write_spaces(id_file, (2*level))
!
      end subroutine write_space_4_parse
!
! ----------------------------------------------------------------------
!
      end module skip_comment_f
