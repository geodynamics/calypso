!>@file   number_to_bit.f90
!!@brief  module number_to_bit
!
!>@brief  convert number to 8-bit characters for both endians
!!@date Programmed by H. Matsui on Sep., 2021
!!
!!@verbatim
!!      character(len=4) function num2bit4_little(inum)
!!      character(len=2) function num2bit2_little(inum)
!!        integer, intent(in) :: inum
!!      integer(kind = kint) function bit4_to_int_little(a)
!!        character(len = 1), intent(in) :: a(4)
!!      integer(kind = kint) function bit2_to_int_little(a)
!!        character(len = 1), intent(in) :: a(2)
!!
!!      character(len=4) function num2bit4_big(inum)
!!      character(len=2) function num2bit2_big(inum)
!!        integer, intent(in) :: inum
!!      integer(kind = kint) function bit4_to_int_big(a)
!!        character(len = 1), intent(in) :: a(4)
!!      integer(kind = kint) function bit2_to_int_big(a)
!!        character(len = 1), intent(in) :: a(2)
!!
!!      subroutine crc32_4_png(ilength, cbuf, i_crc)
!!        integer(kind = 4), intent(in) :: ilength
!!        character(len = 1), intent(in) :: cbuf(ilength)
!!        integer(kind = 4), intent(inout) :: i_crc
!!@endverbatim
!
      module number_to_bit
!
      use m_precision
!
      implicit none
!
!------------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------------
!
       character(len=4) function num2bit4_little(inum)
!
       integer, intent(in) :: inum
!
       integer :: itmp1, itmp2
!
       if(inum .lt. 0) then
         itmp1 = inum + 2147483647 + 1
         itmp2 = itmp1 / 256**3
         num2bit4_little(4:4) = char(itmp2+128)
       else
         itmp1 = inum
         itmp2 = itmp1 / 256**3
         num2bit4_little(4:4) = char(itmp2)
       end if
!
       itmp1 =-itmp2 * 256**3 +itmp1
       itmp2 = itmp1 / 256**2
       num2bit4_little(3:3) = char(itmp2)
       itmp1 =-itmp2 * 256**2 +itmp1
       itmp2 = itmp1 / 256
       num2bit4_little(2:2) = char(itmp2)
       itmp1 =-itmp2 * 256    +itmp1
       num2bit4_little(1:1) = char(itmp1)
!
       end function num2bit4_little
!
!------------------------------------------------------------------------
!
       character(len=2) function num2bit2_little(inum)
!
       integer, intent(in) :: inum
!
       integer itmp1, itmp2
!
       if(inum .lt. 0) then
         itmp1 = inum + 32767 + 1
         itmp2 = itmp1 / 256
         num2bit2_little(2:2) = char(itmp2+128)
       else
         itmp1 = inum
         itmp2 = itmp1 / 256
         num2bit2_little(2:2) = char(itmp2)
       end if
!
       itmp1 =-itmp2 * 256 + itmp1
       num2bit2_little(1:1) = char(itmp1)
!
       end function num2bit2_little
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!
      integer(kind = kint) function bit4_to_int_little(a)
!
      character(len = 1), intent(in) :: a(4)
      integer(kind = kint) :: i
!
      i = iachar(a(1)) + 256*iachar(a(2)) + 65536 * iachar(a(3))
      if(iachar(a(4)) .ge. 128) then
        i = i + 16777216 * (iachar(a(4)) - 128) - 2147483647 - 1
      else
        i = i + 16777216 * iachar(a(4))
      end if
      bit4_to_int_little = i
!
      end function bit4_to_int_little
!
!------------------------------------------------------------------------
!
      integer(kind = kint) function bit2_to_int_little(a)
!
      character(len = 1), intent(in) :: a(2)
      integer(kind = kint) :: i
!
      if(iachar(a(2)) .ge. 128) then
        i = iachar(a(1)) + 256 * (iachar(a(2)) - 128) - 65535 - 1
      else
        i = iachar(a(1)) + 256 * iachar(a(2))
      end if
      bit2_to_int_little = i
!
      end function bit2_to_int_little
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!
      character(len=4) function num2bit4_big(inum)
!
      integer, intent(in) :: inum
!
      integer :: itmp1, itmp2
!
!
      if(inum .lt. 0) then
        itmp1 = inum + 2147483647 + 1
        itmp2 = itmp1 / 256**3
        num2bit4_big(1:1) = char(itmp2+128)
      else
        itmp1 = inum
        itmp2 = itmp1 / 256**3
        num2bit4_big(1:1) = char(itmp2)
      end if
!
      itmp1 =-itmp2 * 256**3 +itmp1
      itmp2 = itmp1 / 256**2
      num2bit4_big(2:2) = char(itmp2)
      itmp1 =-itmp2 * 256**2 +itmp1
      itmp2 = itmp1 / 256
      num2bit4_big(3:3) = char(itmp2)
      itmp1 =-itmp2 * 256    +itmp1
      num2bit4_big(4:4) = char(itmp1)
!
      end function num2bit4_big
!
!------------------------------------------------------------------------
!
      character(len=2) function num2bit2_big(inum)
!
      integer, intent(in) :: inum
!
      integer :: itmp1, itmp2
!
!
      if(inum .lt. 0) then
        itmp1 = inum + 32767 + 1
        itmp2 = itmp1 / 256
        num2bit2_big(1:1) = char(itmp2+128)
      else
        itmp1 = inum
        itmp2 = itmp1 / 256
        num2bit2_big(1:1) = char(itmp2)
      end if
!
      itmp1 =-itmp2 * 256 +itmp1
      num2bit2_big(2:2) = char(itmp1)
!
      end function num2bit2_big
!
!------------------------------------------------------------------------
!
      integer(kind = kint) function bit4_to_int_big(a)
!
      character(len = 1), intent(in) :: a(4)
      integer(kind = kint) :: i
!
      i = iachar(a(4)) + 256*iachar(a(3)) + 65536 * iachar(a(2))
      if(iachar(a(1)) .ge. 128) then
        i = i + 16777216 * (iachar(a(1)) - 128) - 2147483647 - 1
      else
        i = i + 16777216 * iachar(a(1))
      end if
      bit4_to_int_big = i
!
      end function bit4_to_int_big
!
!------------------------------------------------------------------------
!
      integer(kind = kint) function bit2_to_int_big(a)
!
      character(len = 1), intent(in) :: a(2)
      integer(kind = kint) :: i
!
      if(iachar(a(1)) .ge. 128) then
        i = iachar(a(2)) + 256 * (iachar(a(2)) - 128) - 65535 - 1
      else
        i = iachar(a(2)) + 256 * iachar(a(2))
      end if
      bit2_to_int_big = i
!
      end function bit2_to_int_big
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!
      subroutine crc32_4_png(ilength, cbuf, i_crc)
!
      integer(kind = 4), intent(in) :: ilength
      character(len = 1), intent(in) :: cbuf(ilength)
      integer(kind = 4), intent(inout) :: i_crc
!
      integer(kind = 4), parameter  :: crc_init =  -1
      integer(kind = 4), parameter  :: crc_magic = -306674912
!
! Failed definetion
!      integer(kind = 4), parameter  :: crc_init =  Z'FFFFFFFF'
!      integer(kind = 8), parameter  :: crc_init =  4294967295
!      integer(kind = 8), parameter  :: crc_init =  3 * 1431655765
!      integer(kind = 4), parameter  :: crc_magic = Z'EDB88320'
!      integer(kind = 8), parameter  :: crc_magic = 3988292384
!      integer(kind = 8), parameter  :: crc_magic = 2 * 1994146192
!
      integer(kind = 4) :: i_table(0:255)
!
      integer(kind = 4) :: i_tmp, i_b
      integer(kind = 4) :: i1, i3
      integer(kind = 4) :: i, j
!
!
      do i = 0, 255
        i_tmp = i
        do j = 0, 7
          i_b = iand(i_tmp,1)
!          write(*,*) i,j,i_b, i_tmp
          i_tmp = ishft(i_tmp,-1)
          if(i_b .gt. 0) i_tmp = ieor(i_tmp,crc_magic)
        end do
        i_table(i) = i_tmp
      end do
!
      i_crc = not(i_crc)
      do i = 1, ilength
        i1 = iachar(cbuf(i))
        i3 = iand(ieor(i_crc,i1),255)
        i_crc = ieor(i_table(i3), ishft(i_crc,-8))
!        write(*,*) i, i_crc
      end do
      i_crc = not(i_crc)
!      write(*,*) crc32_4_png
!
      end subroutine crc32_4_png
!
!------------------------------------------------------------------------
!
      end module number_to_bit
