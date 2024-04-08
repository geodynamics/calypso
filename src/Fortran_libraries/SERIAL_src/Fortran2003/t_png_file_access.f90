!> @file  t_png_file_access.f90
!!      module t_png_file_access
!!
!! @author  H. Matsui
!! @date Written in June, 2009
!!
!> @brief routines for libpng access
!!
!!@verbatim
!!      subroutine write_png_rgb_f                                      &
!!     &         (img_head, npix_x, npix_y, cimage, png_buf)
!!      subroutine write_png_rgba_f                                     &
!!     &         (img_head, npix_x, npix_y, cimage, png_buf)
!!        type(buffer_4_png), intent(inout) :: png_buf
!!
!!      subroutine read_png_file_f(img_head, npix_x, npix_y, png_buf)
!!      subroutine copy_rgb_from_png_f(npix_x, npix_y, cimage, png_buf)
!!      subroutine copy_rgba_from_png_f(npix_x, npix_y, cimage, png_buf)
!!      subroutine copy_grayscale_from_png_f                            &
!!     &         (npix_x, npix_y, cimage, png_buf)
!!      subroutine copy_grayalpha_from_png_f                            &
!!     &         (npix_x, npix_y, cimage, png_buf)
!!        type(buffer_4_png), intent(inout) :: png_buf
!!@endverbatim
!
      module t_png_file_access
!
      use ISO_C_BINDING
      use m_precision
      use m_constants
!
      implicit none
!
!>      Integer flag for rgb image
      integer(C_int), parameter :: iflag_rgb =   0
!>      Integer flag for rgba image
      integer(C_int), parameter :: iflag_rgba =  1
!>      Integer flag for grayscale with alpha image
      integer(C_int), parameter :: iflag_ba =   11
!>      Integer flag for grayscale image
      integer(C_int), parameter :: iflag_bw =   10
!
      type buffer_4_png
!>        pointer for file prefix
        character(kchara,C_char) :: fhead_p
!
!>        integer flag to detect rgba
        integer(C_int) :: iflag_cmode
!>        horizontal pixel size
        integer(C_int) :: npix4_x
!>        vertical pixel size
        integer(C_int) :: npix4_y
!>        pointer for image data
        character(C_char), pointer :: cimage_p(:,:)
      end type buffer_4_png
!
      private :: link_to_image_data_4_png, get_image_size_from_png
!
!  -----------------
!
      interface
!
!  -----------------
        subroutine write_png_rgba_c(file_head, num_x, num_y, cimage)    &
     &            BIND(C, name = 'write_png_rgba_c')
          use ISO_C_BINDING
!
          character(C_char), intent(in) :: file_head(*)
          integer(C_int), intent(in) :: num_x, num_y
          type(C_ptr), value, intent(in) :: cimage
        end subroutine write_png_rgba_c
!  -----------------
        subroutine write_png_rgb_c(file_head, num_x, num_y, cimage)     &
     &            BIND(C, name = 'write_png_rgb_c')
          use ISO_C_BINDING
!
          character(C_char), intent(in) :: file_head(*)
          integer(C_int), intent(in) :: num_x, num_y
          type(C_ptr), value, intent(in) :: cimage
        end subroutine write_png_rgb_c
!  -----------------
!
        subroutine read_png_file_c(file_head, num_x, num_y, iflag_rgba) &
     &            BIND(C, name = 'read_png_file_c')
          use ISO_C_BINDING
!
          character(C_char), intent(in) :: file_head(*)
          integer(C_int), intent(inout) :: num_x, num_y
          integer(C_int), intent(inout) :: iflag_rgba
        end subroutine read_png_file_c
!
!  -----------------
        subroutine copy_rgb_from_png_c                                  &
     &           (num_x, num_y, iflag_rgba, cimage)                     &
     &            BIND(C, name = 'copy_rgb_from_png_c')
          use ISO_C_BINDING
!
          integer(C_int), value :: num_x, num_y
          integer(C_int), value :: iflag_rgba
          type(C_ptr), value, intent(in) :: cimage
        end subroutine copy_rgb_from_png_c
!  -----------------
        subroutine copy_rgba_from_png_c                                 &
     &           (num_x, num_y, iflag_rgba, cimage)                     &
     &            BIND(C, name = 'copy_rgba_from_png_c')
          use ISO_C_BINDING
!
          integer(C_int), value :: num_x, num_y
          integer(C_int), value :: iflag_rgba
          type(C_ptr), value, intent(in) :: cimage
        end subroutine copy_rgba_from_png_c
!  -----------------
        subroutine copy_grayscale_from_png_c                            &
     &           (num_x, num_y, iflag_rgba, cimage)                     &
     &            BIND(C, name = 'copy_grayscale_from_png_c')
          use ISO_C_BINDING
!
          integer(C_int), value :: num_x, num_y
          integer(C_int), value :: iflag_rgba
          type(C_ptr), value, intent(in) :: cimage
        end subroutine copy_grayscale_from_png_c
!  -----------------
        subroutine copy_grayalpha_from_png_c                            &
     &           (num_x, num_y, iflag_rgba, cimage)                     &
     &            BIND(C, name = 'copy_grayalpha_from_png_c')
          use ISO_C_BINDING
!
          integer(C_int), value :: num_x, num_y
          integer(C_int), value :: iflag_rgba
          type(C_ptr), value, intent(in) :: cimage
        end subroutine copy_grayalpha_from_png_c
!  -----------------
      end interface
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_png_rgb_f                                        &
     &         (img_head, npix_x, npix_y, cimage, png_buf)
!
      integer(kind = kint), intent(in) :: npix_x, npix_y
      character(len=kchara), intent(in) :: img_head
      character(len = 1), intent(in) :: cimage(3,npix_x*npix_y)
!
      type(buffer_4_png), intent(inout) :: png_buf
!
!
      write(png_buf%fhead_p, '(a,a1)') trim(img_head), CHAR(0)
      call link_to_image_data_4_png                                     &
     &   (ifour, npix_x, npix_y, cimage, png_buf)
      call write_png_rgb_c(png_buf%fhead_p,                             &
     &                     png_buf%npix4_x, png_buf%npix4_y,            &
     &                     C_LOC(png_buf%cimage_p(1,1)))
      nullify(png_buf%cimage_p)
!
      end subroutine write_png_rgb_f
!
!------------------------------------------------------------------
!
      subroutine write_png_rgba_f                                       &
     &         (img_head, npix_x, npix_y, cimage, png_buf)
!
      integer(kind = kint), intent(in) :: npix_x, npix_y
      character(len=kchara), intent(in) :: img_head
      character(len = 1), intent(in) :: cimage(4,npix_x*npix_y)
!
      type(buffer_4_png), intent(inout) :: png_buf
!
!
      write(png_buf%fhead_p, '(a,a1)') trim(img_head), CHAR(0)
      call link_to_image_data_4_png                                     &
     &   (ifour, npix_x, npix_y, cimage, png_buf)
      call write_png_rgba_c(png_buf%fhead_p,                            &
     &                      png_buf%npix4_x, png_buf%npix4_y,           &
     &                      C_LOC(png_buf%cimage_p(1,1)))
      nullify(png_buf%cimage_p)
!
      end subroutine write_png_rgba_f
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_png_file_f(img_head, npix_x, npix_y, png_buf)
!
      character(len=kchara), intent(in) :: img_head
      integer(kind = kint), intent(inout) :: npix_x, npix_y
      type(buffer_4_png), intent(inout) :: png_buf
!
!
      write(png_buf%fhead_p, '(a,a1)') trim(img_head), CHAR(0)
      call read_png_file_c(png_buf%fhead_p,                             &
     &    png_buf%npix4_x, png_buf%npix4_y, png_buf%iflag_cmode)
      call get_image_size_from_png(png_buf, npix_x, npix_y)
!
      end subroutine read_png_file_f
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_rgb_from_png_f(npix_x, npix_y, cimage, png_buf)
!
      integer(kind = kint), intent(in) :: npix_x, npix_y
      character(len = 1), intent(inout) :: cimage(3,npix_x*npix_y)
      type(buffer_4_png), intent(inout) :: png_buf
!
!
      call link_to_image_data_4_png                                     &
     &   (ithree, npix_x, npix_y, cimage, png_buf)
      call copy_rgb_from_png_c(png_buf%npix4_x, png_buf%npix4_y,        &
     &    png_buf%iflag_cmode, C_LOC(png_buf%cimage_p(1,1)))
      nullify(png_buf%cimage_p)
!
      end subroutine copy_rgb_from_png_f
!
!------------------------------------------------------------------
!
      subroutine copy_rgba_from_png_f(npix_x, npix_y, cimage, png_buf)
!
      integer(kind = kint), intent(in) :: npix_x, npix_y
      character(len = 1), intent(inout) :: cimage(4,npix_x*npix_y)
      type(buffer_4_png), intent(inout) :: png_buf
!
!
      call link_to_image_data_4_png                                     &
     &   (ifour, npix_x, npix_y, cimage, png_buf)
      call copy_rgba_from_png_c(png_buf%npix4_x, png_buf%npix4_y,       &
     &    png_buf%iflag_cmode, C_LOC(png_buf%cimage_p(1,1)))
      nullify(png_buf%cimage_p)
!
      end subroutine copy_rgba_from_png_f
!
!------------------------------------------------------------------
!
      subroutine copy_grayscale_from_png_f                              &
     &         (npix_x, npix_y, cimage, png_buf)
!
      integer(kind = kint), intent(in) :: npix_x, npix_y
      character(len = 1), intent(inout) :: cimage(1,npix_x*npix_y)
      type(buffer_4_png), intent(inout) :: png_buf
!
!
      call link_to_image_data_4_png                                     &
     &   (ione, npix_x, npix_y, cimage, png_buf)
      call copy_grayscale_from_png_c(png_buf%npix4_x, png_buf%npix4_y,  &
     &    png_buf%iflag_cmode, C_LOC(png_buf%cimage_p(1,1)))
      nullify(png_buf%cimage_p)
!
      end subroutine copy_grayscale_from_png_f
!
!------------------------------------------------------------------
!
      subroutine copy_grayalpha_from_png_f                              &
     &         (npix_x, npix_y, cimage, png_buf)
!
      integer(kind = kint), intent(in) :: npix_x, npix_y
      character(len = 1), intent(inout) :: cimage(2,npix_x*npix_y)
      type(buffer_4_png), intent(inout) :: png_buf
!
!
      call link_to_image_data_4_png                                     &
     &   (itwo, npix_x, npix_y, cimage, png_buf)
      call copy_grayalpha_from_png_c(png_buf%npix4_x, png_buf%npix4_y,  &
     &    png_buf%iflag_cmode, C_LOC(png_buf%cimage_p(1,1)))
      nullify(png_buf%cimage_p)
!
      end subroutine copy_grayalpha_from_png_f
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine link_to_image_data_4_png                               &
     &         (ncolor, npix_x, npix_y, cimage, png_buf)
!
      integer(kind = kint), intent(in) :: ncolor, npix_x, npix_y
      character(len = 1), target, intent(in)                            &
     &                                 :: cimage(ncolor,npix_x*npix_y)
!
      type(buffer_4_png), intent(inout) :: png_buf
!
      png_buf%npix4_x = int(npix_x,KIND(png_buf%npix4_x))
      png_buf%npix4_y = int(npix_y,KIND(png_buf%npix4_y))
      png_buf%cimage_p => cimage
!
      end subroutine link_to_image_data_4_png
!
!------------------------------------------------------------------
!
      subroutine get_image_size_from_png(png_buf, npix_x, npix_y)
!
      type(buffer_4_png), intent(in) :: png_buf
      integer(kind = kint), intent(inout) :: npix_x, npix_y
!
      npix_x = int(png_buf%npix4_x,KIND(npix_x))
      npix_y = int(png_buf%npix4_y,KIND(npix_y))
!
      end subroutine get_image_size_from_png
!
!------------------------------------------------------------------
!
      end module t_png_file_access
