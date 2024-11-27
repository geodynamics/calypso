!>@file   write_bmp_image.f90
!!@brief  module write_bmp_image
!
!> @brief FORTRAN 77 program to make PPM / BMP
!>                                by  K. Hayashi
!!  Modified by H. Matsui on June, 2009
!!
!!@verbatim
!!      subroutine pixout_ppm_p3(fhead, npixel_x, npixel_y, rgb)
!!      subroutine pixout_ppm_p6(fhead, npixel_x, npixel_y, rgb)
!!      subroutine pixout_BMP(fhead, npixel_x, npixel_y, rgb)
!!
!!      character(len=kchara) function add_bmp_suffix(file_header)
!!        character(len=kchara), intent(in) :: file_header
!!      character(len=kchara), intent(in) :: fhead
!!      integer, intent(in) :: ihpixf, jvpixf
!!
!!       character(len=54) function BMP_header(ihpixf, jvpixf)
!!         integer, intent(in) :: ihpixf, jvpixf
!! RGB data array
!!      character(len=1), intent(in) :: rgb(3,ihpixf,jvpixf)
!!
!!      subroutine cvt_8bit_cl_int_2_chara(ihpixf, jvpixf, icl_tbl, rgb)
!!      integer, intent(in) :: icl_tbl(3,ihpixf,jvpixf)
!!
!!* --------------------------------------------
!!*
!!* Notes
!!* o With a parameter ipixout set at 1, 2 or others,
!!*   this subroutine will generate PPM-P6(binary), PPM-P3(text),
!!*   or BMP(24bit depth without color table).
!!*
!!* o Some parts follow DEC-FORTRAN that had been defacto-standard long ago.
!!*   Some compilers today may not accept if "ipixout" is not 2.
!!*
!!* o g77 (ver. 3.3.3) works for all three choices.
!!* o Recent intel compiler (ver. 9 or so) works for all three choices.
!!*
!!* --------------------------------------------
!!@endverbatim
!
      module write_bmp_image
!
      use m_precision
!
      implicit none
!
      private :: add_ppm_suffix
!
!------------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------------
!
      subroutine pixout_ppm_p3(fhead, npixel_x, npixel_y, rgb)
!
!* interface arg.
      character(len=kchara), intent(in) :: fhead
      integer(kind = kint), intent(in) :: npixel_x, npixel_y
! RGB data array
      character(len=1), intent(in) :: rgb(3,npixel_x,npixel_y)
!* local
      integer, parameter :: id_img = 16
      character(len=kchara) :: fname
      integer i, j, k, ihpixf, jvpixf
      integer itmp, icnt
!
!

      ihpixf = int(npixel_x)
      jvpixf = int(npixel_y)
!
!* PPM P3 ! rather "safer" choice for many Fortran compiler(s).

      fname = add_ppm_suffix(fhead)
      open(unit=id_img,file=fname,status='unknown')
      write(*,*) 'Now writing PPM (P3) file : ', fname
!* header
      write(id_img,'(A)') 'P3'
      write(id_img,'(2(1x,i4),'' 255 '')')  ihpixf, jvpixf
      icnt = 0
! here, j (vertical address) runs from top to bottom.
!* image data
      do j = jvpixf, 1, -1
        do i = 1, ihpixf, 1
          do k = 1, 3
            itmp = ichar(rgb(k,i,j))
            icnt = icnt + 4
            if (icnt .LT. 60) then
! mind "$" is not standard.
              write(id_img,fmt='(1x,i3,$)') itmp
            else
              write(id_img,fmt='(1x,i3)') itmp
              icnt = 0
            endif
          end do
        end do
      end do
      write(id_img,'(A)') ' '
      close(id_img)
!
      end subroutine pixout_ppm_p3
!
!------------------------------------------------------------------------
!
      subroutine pixout_ppm_p6(fhead, npixel_x, npixel_y, rgb)
!
!* interface arg.
      character(len=kchara), intent(in) :: fhead
      integer(kind = kint), intent(in) :: npixel_x, npixel_y
! RGB data array
      character(len=1), intent(in) :: rgb(3,npixel_x,npixel_y)
!
!* local
      integer, parameter :: id_img = 16
      character(len=kchara) :: fname
      integer i, j, ihpixf, jvpixf
      integer itmp
      character(len=14) :: frmtstr
!
!
      ihpixf = int(npixel_x)
      jvpixf = int(npixel_y)
!
!* PPM P6
      fname = add_ppm_suffix(fhead)
      open(unit=id_img,file=fname,status='unknown')
      write(*,*) 'Now writing PPM (P6) file : ', fname
!* header
      write(id_img,'(''P6'', 2(1x,i4),'' 255 '',$)') ihpixf, jvpixf
!* image data
      itmp = ihpixf * jvpixf * 3
! make output "format"
      write(frmtstr,'(''('',i8.8,''A,$)'')') itmp
      write(id_img,fmt=frmtstr)                                         &
     &                ((rgb(1:3,i,j),i=1,ihpixf),j=jvpixf,1,-1)
! some compiler may not accept this line.
! here, j (vertical address) runs from top to bottom.
     close(id_img)
!
      end subroutine pixout_ppm_p6
!
!------------------------------------------------------------------------
!
       subroutine pixout_BMP(fhead, npixel_x, npixel_y, rgb)
!* interface arg.
       character(len=kchara), intent(in) :: fhead
       integer(kind = kint), intent(in) :: npixel_x, npixel_y
! RGB data array
       character(len=1), intent(in) :: rgb(3,npixel_x,npixel_y)
!* local
       integer, parameter :: id_img = 16
       character(len=kchara) :: fname
       integer :: i, j, ihpixf, jvpixf
       integer :: itmp
       character(len=14) :: frmtstr
!
       ihpixf = int(npixel_x)
       jvpixf = int(npixel_y)
!
!* BMP (24bit depth)... this part works only when width is multiple of 4.

      if (mod(ihpixf, 4) .NE. 0) then
        write(*,*) 'width must be multiple of 4'
        stop
      endif
!
      fname = add_bmp_suffix(fhead)
!      open(unit=id_img,file=fname,status='unknown')
!      write(id_img,'(a)')
!      close(id_img)
!
      open(unit=id_img,file=fname,status='unknown')
      write(*,*) 'Now writing BMP(24bit) file : ', trim(fname)
!* writing header part
      write(id_img,'(a54)',ADVANCE='NO') BMP_header(ihpixf, jvpixf)
!* image data
      itmp = ihpixf * jvpixf * 3
      write(frmtstr,'(''('',i8.8,''A,$)'')') itmp
      write(id_img,fmt=frmtstr)                                         &
     &                        ((rgb(3:1:-1,i,j),i=1,ihpixf),j=1,jvpixf)
! writing in BGR order, not RGB.
      close(id_img)
!
      end subroutine pixout_BMP
!
!------------------------------------------------------------------------
!
      character(len=54) function BMP_header(ihpixf, jvpixf)
!
      use number_to_bit
!
!* interface arg.
      integer, intent(in) :: ihpixf, jvpixf
      character(len=54) :: headmsw
!* local
      integer :: itmp
      character(len=4) ::  byt4
      character(len=2) ::  byt2
!
!
!* header 1 (file header ; 1--14 byte)
      headmsw( 1: 2) = 'BM'             ! declaring this is BMP file
      itmp = 54 + ihpixf * jvpixf * 3 ! total file size = header + data
      headmsw( 3: 6) = num2bit4_little(itmp)
      itmp = 0                        ! may be 0
      headmsw( 7: 8) = num2bit2_little(itmp)
      itmp = 0                        ! may be 0
      headmsw( 9:10) = num2bit2_little(itmp)
      itmp = 54                       ! must be 54 : total length of header
      headmsw(11:14) = num2bit4_little(itmp)
!* header 2 (bit-map header ; 13--54 byte)
      itmp = 40                       ! must be 40 : length of bit-map header
      headmsw(15:18) = num2bit4_little(itmp)
      itmp = ihpixf                   ! width
      headmsw(19:22) = num2bit4_little(itmp)
      itmp = jvpixf                   ! height
      headmsw(23:26) = num2bit4_little(itmp)
      itmp = 1                        ! must be 1
      headmsw(27:28) = num2bit2_little(itmp)
      itmp = 24                       ! must be 24 : color depth in bit.
      headmsw(29:30) = num2bit2_little(itmp)
      itmp = 0                        ! may be 0 : compression method index
      headmsw(31:34) = num2bit4_little(itmp)
      itmp = 0                        ! may be 0 : file size if compressed
      headmsw(35:38) = num2bit4_little(itmp)
      itmp = 0                        ! arbit. : pixel per meter, horizontal
      headmsw(39:42) = num2bit4_little(itmp)
      itmp = 0                        ! arbit. : pixel per meter, vertical
      headmsw(43:46) = num2bit4_little(itmp)
      itmp = 0                        ! may be 0 here : num. of color used
      headmsw(47:50) = num2bit4_little(itmp)
      itmp = 0                        ! may be 0 here : num. of important color
      headmsw(51:54) = num2bit4_little(itmp)
!
      BMP_header(1:54) = headmsw(1:54)
!
      end function BMP_header
!
!------------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_ppm_suffix(file_header)
!
      character(len=kchara), intent(in) :: file_header
!
       write(add_ppm_suffix,1011) trim(file_header)
 1011 format (a,".ppm")
!
      end function add_ppm_suffix
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_bmp_suffix(file_header)
!
      character(len=kchara), intent(in) :: file_header
!
       write(add_bmp_suffix,1011) trim(file_header)
 1011 format (a,".bmp")
!
      end function add_bmp_suffix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cvt_8bit_cl_int_2_chara(ihpixf, jvpixf, icl_tbl, rgb)
!
      integer, intent(in) :: ihpixf, jvpixf
      integer, intent(in) :: icl_tbl(3,ihpixf,jvpixf)
! RGB data array
      character(len=1), intent(inout) :: rgb(3,ihpixf,jvpixf)
!
      integer :: j
!
!
      do j = 1, jvpixf
        rgb(1,1:ihpixf,j) = char( icl_tbl(1,1:ihpixf,j) )
        rgb(2,1:ihpixf,j) = char( icl_tbl(2,1:ihpixf,j) )
        rgb(3,1:ihpixf,j) = char( icl_tbl(3,1:ihpixf,j) )
      end do
!
      end subroutine cvt_8bit_cl_int_2_chara
!
!-----------------------------------------------------------------------
!
      end module write_bmp_image
