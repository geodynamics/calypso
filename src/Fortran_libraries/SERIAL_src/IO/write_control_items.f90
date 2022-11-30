!>@file   write_control_items.f90
!!@brief  module write_control_items
!!
!!@authorH.Matsui and H.Okuda
!!@date Programmed  H. Matsui in  Feb. 2001 
!!@date Modified   H. Matsui in  Oct. 2013 
!
!> @brief subroutines to find comment lines in data
!!
!!@verbatim
!!      integer(kind = kint) function iflag_divide(charaname)
!!      integer(kind = kint) function max_len_of_charaarray(num, carray)
!!
!!      subroutine write_ctl_real_cont(id_file, real_item)
!!      subroutine write_ctl_integer_cont(id_file, int_item)
!!      subroutine write_ctl_fixlen_chara(id_file, maxlen, charaname)
!!      subroutine write_ctl_chara_cont(id_file, charaname)
!!      subroutine write_ctl_chara_lf(id_file, charaname)
!!      subroutine write_spaces(id_file, nspace)
!!      subroutine write_space_4_parse(id_file, level)
!!@endverbatim
!
      module write_control_items
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
! ----------------------------------------------------------------------
!
      subroutine write_ctl_real_cont(id_file, real_item)
!
      integer(kind = kint), intent(in) :: id_file
      real(kind = kreal), intent(in) :: real_item
!
!
      write(id_file,'(1pE25.15e3,a2)',advance='no') real_item,  '  '
!
      end subroutine write_ctl_real_cont
!
! ----------------------------------------------------------------------
!
      subroutine write_ctl_integer_cont(id_file, int_item)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: int_item
!
!
      write(id_file,'(i16,a2)',advance='no') int_item,  '  '
!
      end subroutine write_ctl_integer_cont
!
! ----------------------------------------------------------------------
!
      subroutine write_ctl_fixlen_chara(id_file, maxlen, charaname)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: charaname
!
      integer(kind = kint) :: nspace0
!
!
      nspace0 = maxlen - len_trim(charaname)
      call write_ctl_chara_cont(id_file, charaname)
      if(nspace0 .gt. 0) call write_spaces(id_file, nspace0)
!
      end subroutine write_ctl_fixlen_chara
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
      end module write_control_items
