!>@file   t_ctl_data_4_screen_pixel.f90
!!@brief  module t_ctl_data_4_screen_pixel
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR projection and streo parameter
!!
!!@verbatim
!!      subroutine read_image_size_ctl                                  &
!!     &         (id_control, hd_block, pixel, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(screen_pixel_ctl), intent(inout) :: pixel
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_image_size_ctl                                 &
!!     &         (id_control, hd_block, pixel, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(screen_pixel_ctl), intent(in) :: pixel
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine reset_image_size_ctl(pixel)
!!      subroutine copy_image_size_ctl(org_pixel, new_pixel)
!!        type(screen_pixel_ctl), intent(in) :: org_pixel
!!        type(screen_pixel_ctl), intent(inout) :: new_pixel
!!
!!      integer(kind = kint) function num_label_pvr_pixels()
!!      subroutine set_label_pvr_pixels(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Input example
!!
!!     begin image_size_ctl
!!       x_pixel_ctl  640
!!       y_pixel_ctl  480
!!     end image_size_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
!
      module t_ctl_data_4_screen_pixel
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_integer
      use skip_comment_f
!
      implicit  none
!
!
!
!>      Structure of screen resolution
      type screen_pixel_ctl
!>        Structure of number of horizontal pixels
        type(read_integer_item) :: num_xpixel_ctl
!>        Structure of number of vertical pixels
        type(read_integer_item) :: num_ypixel_ctl
!
!     3rd level for view_transform_define
        integer (kind=kint) :: i_image_size =  0
      end type screen_pixel_ctl
!
!     4th level for image size
      integer(kind = kint), parameter, private                          &
     &             :: n_label_pvr_pixels = 2
      character(len=kchara), parameter, private                         &
     &             :: hd_x_pixel = 'x_pixel_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_y_pixel = 'y_pixel_ctl'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_image_size_ctl                                    &
     &         (id_control, hd_block, pixel, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(screen_pixel_ctl), intent(inout) :: pixel
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if (pixel%i_image_size.gt.0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_x_pixel, pixel%num_xpixel_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_y_pixel, pixel%num_ypixel_ctl)
      end do
      pixel%i_image_size = 1
!
      end subroutine read_image_size_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_image_size_ctl                                   &
     &         (id_control, hd_block, pixel, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(screen_pixel_ctl), intent(in) :: pixel
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(pixel%i_image_size .le. 0) return
!
      maxlen = len_trim(hd_x_pixel)
      maxlen = max(maxlen, len_trim(hd_y_pixel))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &   hd_x_pixel, pixel%num_xpixel_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &   hd_y_pixel, pixel%num_ypixel_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_image_size_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_image_size_ctl(pixel)
!
      type(screen_pixel_ctl), intent(inout) :: pixel
!
!
      pixel%num_xpixel_ctl%iflag = 0
      pixel%num_ypixel_ctl%iflag = 0
!
      pixel%i_image_size =  0
!
      end subroutine reset_image_size_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_image_size_ctl(org_pixel, new_pixel)
!
      type(screen_pixel_ctl), intent(in) :: org_pixel
      type(screen_pixel_ctl), intent(inout) :: new_pixel
!
!
      new_pixel%i_image_size = org_pixel%i_image_size
!
      call copy_integer_ctl(org_pixel%num_xpixel_ctl,                   &
     &                      new_pixel%num_xpixel_ctl)
      call copy_integer_ctl(org_pixel%num_ypixel_ctl,                   &
     &                      new_pixel%num_ypixel_ctl)
!
      end subroutine copy_image_size_ctl
!
!  ---------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_label_pvr_pixels()
      num_label_pvr_pixels = n_label_pvr_pixels
      return
      end function num_label_pvr_pixels
!
! ----------------------------------------------------------------------
!
      subroutine set_label_pvr_pixels(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_pvr_pixels)
!
!
      call set_control_labels(hd_x_pixel, names( 1))
      call set_control_labels(hd_y_pixel, names( 2))
!
      end subroutine set_label_pvr_pixels
!
! ----------------------------------------------------------------------
!
      end module t_ctl_data_4_screen_pixel
