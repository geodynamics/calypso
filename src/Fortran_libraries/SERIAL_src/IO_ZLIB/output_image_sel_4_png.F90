!> @file  output_image_sel_4_png.f90
!!      module output_image_sel_4_png
!!
!! @author  H. Matsui
!! @date Written in June, 2009
!
!> @brief selector for image data output
!!
!!@verbatim
!!      subroutine sel_output_image_file(id_file_type, img_head,        &
!!     &          npix_x, npix_y, cimage)
!!      subroutine sel_rgba_image_file(id_file_type, img_head,          &
!!     &          npix_x, npix_y, cimage)
!!@endverbatim
!
      module output_image_sel_4_png
!
      use m_precision
      use m_constants
!
#ifdef PNG_OUTPUT
      use t_png_file_access
#endif
!
      use write_bmp_image
!
      implicit none
!
      character(len = kchara), parameter :: hd_BMP =       'BMP'
      character(len = kchara), parameter :: hd_PNG =       'PNG'
      character(len = kchara), parameter :: hd_QUILT_BMP = 'QUILT'
      character(len = kchara), parameter :: hd_QUILT_BMP_GZ = 'QUILT_GZ'
!
      character(len = kchara), parameter                                &
     &              :: hd_QUILT_BMP_GZ2 = 'QUILT_GZIP'
      character(len = kchara), parameter                                &
     &              :: hd_QUILT_BMP_GZ3 = 'GZ_QUILT'
      character(len = kchara), parameter                                &
     &              :: hd_QUILT_BMP_GZ4 = 'GZIP_QUILT'
!
      integer(kind = kint), parameter :: iflag_UNDEFINED =  -1
      integer(kind = kint), parameter :: iflag_BMP = 11
      integer(kind = kint), parameter :: iflag_PNG = 12
      integer(kind = kint), parameter :: iflag_QUILT_BMP =    111
      integer(kind = kint), parameter :: iflag_QUILT_BMP_GZ = 113

#ifdef PNG_OUTPUT
      type(buffer_4_png), private :: pbuf
#endif
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_output_image_file(id_file_type, img_head,          &
     &          npix_x, npix_y, cimage)
!
      use calypso_png_file_IO
!
      integer(kind = kint), intent(in) :: id_file_type
      integer(kind = kint), intent(in) :: npix_x, npix_y
      character(len=kchara), intent(in) :: img_head
      character(len = 1), intent(in) :: cimage(3,npix_x*npix_y)
!
!
      if(id_file_type .eq. iflag_PNG) then
#ifdef PNG_OUTPUT
        call write_png_rgb_f                                            &
     &     (img_head, npix_x, npix_y, cimage(1,1), pbuf)
#else
        call calypso_write_png                                          &
     &     (img_head, ithree, npix_x, npix_y, cimage(1,1))
#endif
        return
      end if
!
      call pixout_BMP(img_head, npix_x, npix_y, cimage(1,1))
!
      end subroutine sel_output_image_file
!
!------------------------------------------------------------------
!
      subroutine sel_rgba_image_file(id_file_type, img_head,            &
     &          npix_x, npix_y, cimage)
!
      use calypso_png_file_IO
!
      integer(kind = kint), intent(in) :: id_file_type
      integer(kind = kint), intent(in) :: npix_x, npix_y
      character(len=kchara), intent(in) :: img_head
      character(len = 1), intent(in) :: cimage(4,npix_x*npix_y)
!
!
      if(id_file_type .eq. iflag_PNG) then
#ifdef PNG_OUTPUT
        call write_png_rgba_f                                           &
     &     (img_head, npix_x, npix_y, cimage(1,1), pbuf)
#else
        call calypso_write_png                                          &
     &     (img_head, ifour, npix_x, npix_y, cimage(1,1))
#endif
        return
      end if
!
      write(*,*) 'BitMap does not support transparent image'
!
      end subroutine sel_rgba_image_file
!
!------------------------------------------------------------------
!
      end module output_image_sel_4_png
