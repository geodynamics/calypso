!>@file   t_ctl_data_pvr_tracer.f90
!!@brief  module t_ctl_data_pvr_tracer
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief tracer control data for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine init_pvr_tracer_ctl_label(hd_block, pvr_tracer_c)
!!      subroutine read_pvr_tracer_ctl(id_control, hd_block,            &
!!     &                               pvr_tracer_c, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_tracer_ctl), intent(inout) :: pvr_tracer_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_pvr_tracer_ctl(id_control, hd_block,           &
!!     &                                pvr_tracer_c, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_tracer_ctl), intent(in) :: pvr_tracer_c
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dup_pvr_tracer_ctl(org_pvr_iso_c, new_pvr_iso_c)
!!        type(pvr_tracer_ctl), intent(in) :: org_pvr_iso_c
!!        type(pvr_tracer_ctl), intent(inout) :: new_pvr_iso_c
!!      subroutine reset_pvr_tracer_ctl(pvr_tracer_c)
!!        type(pvr_tracer_ctl), intent(inout) :: pvr_tracer_c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin isosurface_ctl
!!      tracer_file_prefix       tracer_out
!!
!!      tracer_increment          10
!!      rendering_radius          3.0e-3
!!
!!      color_mode_ctl      'single_color' or 'colormap'
!!      RGB_color_ctl       0.7   0.2   0.3
!!      opacity_ctl         0.9
!!    end isosurface_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_pvr_tracer
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_integer
      use t_control_array_real
      use t_control_array_real3
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_tracer_ctl
!>        Control block name
        character(len = kchara) :: block_name = 'tracer_ctl'
!
!>        File prefix for output tracer data
        type(read_character_item) :: tracer_file_prefix
!
!>        Tracer increment of perticle rendering
        type(read_integer_item) ::   render_increment_ctl
!>        Radius of renderingperticle
        type(read_real_item) ::      render_radius_ctl
!
!>        Color mode for tracer rendering
        type(read_character_item) :: color_mode_ctl
!>        RGB value of tracer color
        type(read_real3_item) :: rgb_color_ctl
!>        Tracer opacity
        type(read_real_item) :: opacity_ctl
!
        integer(kind = kint) :: i_pvr_tracer_ctl = 0
      end type pvr_tracer_ctl
!
!     3rd level for isosurface
!
      character(len=kchara), private                                    &
     &              :: hd_tracer_prefix =    'tracer_file_prefix'
      character(len=kchara), private                                    &
     &              :: hd_tracer_increment = 'tracer_increment'
      character(len=kchara), private                                    &
     &              :: hd_render_radius =    'rendering_radius'
      character(len=kchara), private                                    &
     &              :: hd_color_mode =       'color_mode_ctl'
      character(len=kchara), private                                    &
     &              :: hd_tracer_RGB =       'RGB_color_ctl'
      character(len=kchara), private                                    &
     &              :: hd_tracer_opacity =   'opacity_ctl'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_tracer_ctl(id_control, hd_block,              &
     &                               pvr_tracer_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_tracer_ctl), intent(inout) :: pvr_tracer_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_tracer_prefix,               &
     &                           pvr_tracer_c%tracer_file_prefix)
!
        call read_integer_ctl_type(c_buf, hd_tracer_increment,          &
     &                             pvr_tracer_c%render_increment_ctl)
        call read_real_ctl_type(c_buf, hd_render_radius,                &
     &                          pvr_tracer_c%render_radius_ctl)
!
        call read_chara_ctl_type(c_buf, hd_color_mode,                  &
     &                           pvr_tracer_c%color_mode_ctl)
        call read_real3_ctl_type(c_buf, hd_tracer_RGB,                  &
     &                          pvr_tracer_c%rgb_color_ctl)
        call read_real_ctl_type(c_buf, hd_tracer_opacity,               &
     &                          pvr_tracer_c%opacity_ctl)
      end do
      pvr_tracer_c%i_pvr_tracer_ctl = 1
!
      end subroutine read_pvr_tracer_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_pvr_tracer_ctl(id_control, hd_block,             &
     &                                pvr_tracer_c, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_tracer_ctl), intent(in) :: pvr_tracer_c
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(pvr_tracer_c%i_pvr_tracer_ctl .le. 0) return
!
      maxlen = len_trim(hd_tracer_prefix)
      maxlen = max(maxlen, len_trim(hd_render_radius))
      maxlen = max(maxlen, len_trim(hd_tracer_increment))
      maxlen = max(maxlen, len_trim(hd_color_mode))
      maxlen = max(maxlen, len_trim(hd_tracer_RGB))
      maxlen = max(maxlen, len_trim(hd_tracer_opacity))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          pvr_tracer_c%tracer_file_prefix)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &                            pvr_tracer_c%render_increment_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &                         pvr_tracer_c%render_radius_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          pvr_tracer_c%color_mode_ctl)
      call write_real3_ctl_type(id_control, level, maxlen,              &
     &                          pvr_tracer_c%rgb_color_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &                         pvr_tracer_c%opacity_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_pvr_tracer_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine init_pvr_tracer_ctl_label(hd_block, pvr_tracer_c)
!
      character(len=kchara), intent(in) :: hd_block
      type(pvr_tracer_ctl), intent(inout) :: pvr_tracer_c
!
!
      pvr_tracer_c%block_name = hd_block
        call init_chara_ctl_item_label(hd_tracer_prefix,                &
     &                                 pvr_tracer_c%tracer_file_prefix)
!
        call init_int_ctl_item_label(hd_tracer_increment,               &
     &                               pvr_tracer_c%render_increment_ctl)
        call init_real_ctl_item_label(hd_render_radius,                 &
     &                                pvr_tracer_c%render_radius_ctl)
!
        call init_chara_ctl_item_label(hd_color_mode,                   &
     &                                 pvr_tracer_c%color_mode_ctl)
        call init_real3_ctl_item_label(hd_tracer_RGB,                   &
     &                                pvr_tracer_c%rgb_color_ctl)
        call init_real_ctl_item_label(hd_tracer_opacity,                &
     &                                pvr_tracer_c%opacity_ctl)
!
      end subroutine init_pvr_tracer_ctl_label
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dup_pvr_tracer_ctl(org_pvr_iso_c, new_pvr_iso_c)
!
      type(pvr_tracer_ctl), intent(in) :: org_pvr_iso_c
      type(pvr_tracer_ctl), intent(inout) :: new_pvr_iso_c
!
!
      call copy_chara_ctl(org_pvr_iso_c%tracer_file_prefix,             &
     &                    new_pvr_iso_c%tracer_file_prefix)
!
      call copy_integer_ctl(org_pvr_iso_c%render_increment_ctl,         &
     &                      new_pvr_iso_c%render_increment_ctl)
      call copy_real_ctl(org_pvr_iso_c%render_radius_ctl,               &
     &                   new_pvr_iso_c%render_radius_ctl)
!
      call copy_chara_ctl(org_pvr_iso_c%color_mode_ctl,                 &
     &                    new_pvr_iso_c%color_mode_ctl)
      call copy_real3_ctl(org_pvr_iso_c%rgb_color_ctl,                  &
     &                    new_pvr_iso_c%rgb_color_ctl)
      call copy_real_ctl(org_pvr_iso_c%opacity_ctl,                     &
     &                   new_pvr_iso_c%opacity_ctl)
!
      end subroutine dup_pvr_tracer_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_tracer_ctl(pvr_tracer_c)
!
      type(pvr_tracer_ctl), intent(inout) :: pvr_tracer_c
!
!
      pvr_tracer_c%tracer_file_prefix%iflag =  0
!
      pvr_tracer_c%render_increment_ctl%iflag =  0
      pvr_tracer_c%render_radius_ctl%iflag =     0
!
      pvr_tracer_c%color_mode_ctl%iflag =   0
      pvr_tracer_c%rgb_color_ctl%iflag =    0
      pvr_tracer_c%opacity_ctl%iflag =      0
!
      pvr_tracer_c%i_pvr_tracer_ctl =       0
!
      end subroutine reset_pvr_tracer_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_pvr_tracer
