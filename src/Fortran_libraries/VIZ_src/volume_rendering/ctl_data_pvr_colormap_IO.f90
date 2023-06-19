!>@file   ctl_data_pvr_colormap_IO.f90
!!@brief  module ctl_data_pvr_colormap_IO
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief colormap control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine read_pvr_colordef_ctl                                &
!!     &         (id_control, hd_block, color, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_colormap_ctl), intent(inout) :: color
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_pvr_colordef_ctl                               &
!!     &         (id_control, hd_block, color, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_colormap_ctl), intent(in) :: color
!!        integer(kind = kint), intent(inout) :: level
!!
!!      integer(kind = kint) function num_label_pvr_colormap()
!!      integer(kind = kint) function num_label_LIC_colormap()
!!      subroutine set_label_pvr_colormap(names)
!!      subroutine set_label_LIC_colormap(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of color control for Kemo's volume rendering
!!
!!  begin colormap_ctl
!!    colormap_mode_ctl       rainbow
!!    background_color_ctl    0.0   0.0   0.0
!!!
!!    LIC_color_field             magnetic_field
!!    LIC_color_componenet        magnitude
!!
!!    LIC_transparent_field         magnetic_field
!!    LIC_transparent_componenet    magnitude
!!!
!!    data_mapping_ctl   Colormap_list
!!    array color_table_ctl    3
!!      color_table_ctl    0.0   0.0
!!      color_table_ctl    0.5   0.5
!!      color_table_ctl    1.0   1.0
!!    end array color_table_ctl
!!!
!!    opacity_style_ctl              point_linear
!!    array  linear_opacity_ctl         7
!!      linear_opacity_ctl   0.0     0.01
!!      linear_opacity_ctl   0.01    0.015
!!      linear_opacity_ctl   0.2     0.02
!!      linear_opacity_ctl   0.6     0.04
!!      linear_opacity_ctl   0.7     0.03
!!      linear_opacity_ctl   0.85    0.01
!!      linear_opacity_ctl   0.95    0.001
!!    end array linear_opacity_ctl
!!
!!    array  step_opacity_ctl         7
!!      step_opacity_ctl   0.0     0.01    0.01
!!      step_opacity_ctl   0.01    0.2     0.015
!!      step_opacity_ctl   0.2     0.35    0.02
!!      step_opacity_ctl   0.6     0.7     0.04
!!      step_opacity_ctl   0.7     0.85    0.03
!!      step_opacity_ctl   0.85    0.95    0.01
!!      step_opacity_ctl   0.95    1.0     0.001
!!    end array step_opacity_ctl
!!    constant_opacity_ctl           0.003
!!!
!!    range_min_ctl   0.0
!!    range_max_ctl   1.0
!!  end colormap_ctl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_data_pvr_colormap_IO
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_view_transfer
      use t_control_array_character
      use t_control_array_real
      use t_control_array_real2
      use t_control_array_real3
      use t_ctl_data_pvr_colormap
      use skip_comment_f
!
      implicit  none
!
!     3rd level for colormap
!
      character(len=kchara), parameter, private                         &
     &        :: hd_colormap_mode =     'colormap_mode_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_background_color =  'background_color_ctl'
!
      character(len=kchara), parameter, private                         &
     &        :: hd_lic_color_fld =  'LIC_color_field'
      character(len=kchara), parameter, private                         &
     &        :: hd_lic_color_comp =  'LIC_color_componenet'
      character(len=kchara), parameter, private                         &
     &        :: hd_lic_opacity_fld =  'LIC_transparent_field'
      character(len=kchara), parameter, private                         &
     &        :: hd_lic_opacity_comp =  'LIC_transparent_componenet'
!
      character(len=kchara), parameter, private                         &
     &        :: hd_data_mapping = 'data_mapping_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_pvr_range_min = 'range_min_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_pvr_range_max = 'range_max_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_colortable = 'color_table_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_opacity_style = 'opacity_style_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_constant_opacity = 'constant_opacity_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_linear_opacity = 'linear_opacity_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_opacity_def =    'step_opacity_ctl'
!
      integer(kind = kint), parameter :: n_label_pvr_colormap = 10
      integer(kind = kint), parameter :: n_label_lic_colormap = 14
!
      private :: n_label_pvr_colormap, n_label_lic_colormap
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_colordef_ctl                                  &
     &         (id_control, hd_block, color, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pvr_colormap_ctl), intent(inout) :: color
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(color%i_pvr_colordef.gt.0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_array_r2(id_control, hd_colortable,           &
     &      color%colortbl_ctl, c_buf)
        call read_control_array_r2(id_control, hd_linear_opacity,       &
     &      color%linear_opacity_ctl, c_buf)
!
        call read_control_array_r3(id_control, hd_opacity_def,          &
     &      color%step_opacity_ctl, c_buf)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_lic_color_fld, color%lic_color_fld_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_lic_color_comp, color%lic_color_comp_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_lic_opacity_fld, color%lic_opacity_fld_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_lic_opacity_comp, color%lic_opacity_comp_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_colormap_mode, color%colormap_mode_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_data_mapping, color%data_mapping_ctl)
        call read_chara_ctl_type(c_buf, hd_opacity_style,               &
     &      color%opacity_style_ctl)
!
        call read_real_ctl_type(c_buf, hd_pvr_range_min,                &
     &      color%range_min_ctl)
        call read_real_ctl_type(c_buf, hd_pvr_range_max,                &
     &      color%range_max_ctl)
        call read_real_ctl_type(c_buf, hd_constant_opacity,             &
     &      color%fix_opacity_ctl)
        call read_real3_ctl_type                                        &
     &     (c_buf, hd_background_color, color%background_color_ctl)
      end do
      color%i_pvr_colordef = 1
!
      end subroutine read_pvr_colordef_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_pvr_colordef_ctl                                 &
     &         (id_control, hd_block, color, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_colormap_ctl), intent(in) :: color
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(color%i_pvr_colordef .le. 0) return
!
      maxlen = len_trim(hd_lic_color_fld)
      maxlen = max(maxlen, len_trim(hd_lic_color_comp))
      maxlen = max(maxlen, len_trim(hd_lic_opacity_fld))
      maxlen = max(maxlen, len_trim(hd_lic_opacity_comp))
      maxlen = max(maxlen, len_trim(hd_data_mapping))
      maxlen = max(maxlen, len_trim(hd_opacity_style))
      maxlen = max(maxlen, len_trim(hd_constant_opacity))
      maxlen = max(maxlen, len_trim(hd_pvr_range_min))
      maxlen = max(maxlen, len_trim(hd_pvr_range_max))
      maxlen = max(maxlen, len_trim(hd_colormap_mode))
      maxlen = max(maxlen, len_trim(hd_background_color))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_colormap_mode, color%colormap_mode_ctl)
      call write_real3_ctl_type(id_control, level, maxlen,              &
     &    hd_background_color, color%background_color_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_lic_color_fld, color%lic_color_fld_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_lic_color_comp, color%lic_color_comp_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_lic_opacity_fld, color%lic_opacity_fld_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_lic_opacity_comp, color%lic_opacity_comp_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_data_mapping, color%data_mapping_ctl)
      call write_control_array_r2(id_control, level,                    &
     &    hd_colortable, color%colortbl_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_opacity_style, color%opacity_style_ctl)
      call write_control_array_r2(id_control, level,                    &
     &    hd_linear_opacity, color%linear_opacity_ctl)
      call write_control_array_r3(id_control, level,                    &
     &    hd_opacity_def, color%step_opacity_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_constant_opacity, color%fix_opacity_ctl)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_pvr_range_min, color%range_min_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_pvr_range_max, color%range_max_ctl)
!
      level = write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_pvr_colordef_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_pvr_colormap()
      num_label_pvr_colormap = n_label_pvr_colormap
      return
      end function num_label_pvr_colormap
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_LIC_colormap()
      num_label_LIC_colormap = n_label_lic_colormap
      return
      end function num_label_LIC_colormap
!
! ----------------------------------------------------------------------
!
      subroutine set_label_pvr_colormap(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_pvr_colormap)
!
!
      call set_control_labels(hd_colormap_mode,  names( 1))
!
      call set_control_labels(hd_data_mapping,     names( 2))
      call set_control_labels(hd_pvr_range_min,    names( 3))
      call set_control_labels(hd_pvr_range_max,    names( 4))
      call set_control_labels(hd_colortable,       names( 5))
!
      call set_control_labels(hd_opacity_style,    names( 6))
      call set_control_labels(hd_constant_opacity, names( 7))
      call set_control_labels(hd_linear_opacity,   names( 8))
      call set_control_labels(hd_opacity_def,      names( 9))
!
      call set_control_labels(hd_background_color,    names(10))
!
      end subroutine set_label_pvr_colormap
!
! ----------------------------------------------------------------------
!
      subroutine set_label_LIC_colormap(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_lic_colormap)
!
!
      call set_control_labels(hd_colormap_mode,  names( 1))
!
      call set_control_labels(hd_lic_color_fld,    names( 2))
      call set_control_labels(hd_lic_color_comp,   names( 3))
      call set_control_labels(hd_lic_opacity_fld,  names( 4))
      call set_control_labels(hd_lic_opacity_comp, names( 5))
!
!
      call set_control_labels(hd_data_mapping,     names( 6))
      call set_control_labels(hd_pvr_range_min,    names( 7))
      call set_control_labels(hd_pvr_range_max,    names( 8))
      call set_control_labels(hd_colortable,       names( 9))
!
      call set_control_labels(hd_opacity_style,    names(10))
      call set_control_labels(hd_constant_opacity, names(11))
      call set_control_labels(hd_linear_opacity,   names(12))
      call set_control_labels(hd_opacity_def,      names(13))
!
      call set_control_labels(hd_background_color,    names(14))
!
      end subroutine set_label_LIC_colormap
!
!  ---------------------------------------------------------------------
!
      end module ctl_data_pvr_colormap_IO
