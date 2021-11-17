!>@file   t_ctl_data_pvr_colormap.f90
!!@brief  module t_ctl_data_pvr_colormap
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief colormap control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine read_pvr_colordef_ctl                                &
!!     &         (id_control, hd_block, color, c_buf)
!!      subroutine reset_pvr_colormap_flags(color)
!!      subroutine dealloc_pvr_color_crl(color)
!!        type(pvr_colormap_ctl), intent(inout) :: color
!!      subroutine dup_pvr_colordef_ctl(org_color, new_color)
!!        type(pvr_colormap_ctl), intent(in) :: org_color
!!        type(pvr_colormap_ctl), intent(inout) :: new_color
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
      module t_ctl_data_pvr_colormap
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
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_colormap_ctl
        type(read_character_item) :: lic_color_fld_ctl
        type(read_character_item) :: lic_color_comp_ctl
        type(read_character_item) :: lic_opacity_fld_ctl
        type(read_character_item) :: lic_opacity_comp_ctl
!
        type(read_character_item) :: colormap_mode_ctl
        type(read_character_item) :: data_mapping_ctl
        type(read_character_item) :: opacity_style_ctl
!
        type(read_real_item) :: range_min_ctl
        type(read_real_item) :: range_max_ctl
        type(read_real_item) :: fix_opacity_ctl
!
!>      Structure for color map controls
!!@n      colortbl_ctl%vec1:  field data value
!!@n      colortbl_ctl%vec2:  color map value
        type(ctl_array_r2) :: colortbl_ctl
!
!>        Structure for opacity controls
!!@n        linear_opacity_ctl%vec1:  field value to define opacity
!!@n        linear_opacity_ctl%vec3:  Opacity at this point
        type(ctl_array_r2) :: linear_opacity_ctl
!>        Structure for opacity controls
!!@n        step_opacity_ctl%vec1:  Minimum value for one opacity
!!@n        step_opacity_ctl%vec2:  Maximum value for one opacity
!!@n        step_opacity_ctl%vec3:  Opacity for each level
        type(ctl_array_r3) :: step_opacity_ctl
!
!     Top level
!     2nd level for color definition
        integer (kind=kint) :: i_pvr_colordef =        0
      end type pvr_colormap_ctl
!
!     3rd level for colormap
!
      character(len=kchara) :: hd_colormap_mode =  'colormap_mode_ctl'
!
      character(len=kchara)                                             &
     &        :: hd_lic_color_fld =  'LIC_color_field'
      character(len=kchara)                                             &
     &        :: hd_lic_color_comp =  'LIC_color_componenet'
      character(len=kchara)                                             &
     &        :: hd_lic_opacity_fld =  'LIC_transparent_field'
      character(len=kchara)                                             &
     &        :: hd_lic_opacity_comp =  'LIC_transparent_componenet'
!
      character(len=kchara) :: hd_data_mapping = 'data_mapping_ctl'
      character(len=kchara) :: hd_pvr_range_min = 'range_min_ctl'
      character(len=kchara) :: hd_pvr_range_max = 'range_max_ctl'
      character(len=kchara) :: hd_colortable = 'color_table_ctl'
      character(len=kchara) :: hd_opacity_style = 'opacity_style_ctl'
      character(len=kchara) :: hd_constant_opacity                      &
     &                        = 'constant_opacity_ctl'
      character(len=kchara) :: hd_linear_opacity = 'linear_opacity_ctl'
      character(len=kchara) :: hd_opacity_def =    'step_opacity_ctl'
!
      integer(kind = kint), parameter :: n_label_pvr_colormap =  9
      integer(kind = kint), parameter :: n_label_lic_colormap = 13
!
      private :: hd_colormap_mode, hd_data_mapping
      private :: hd_pvr_range_min, hd_pvr_range_max
      private :: hd_colortable, hd_opacity_style
      private :: hd_constant_opacity, hd_opacity_def, hd_linear_opacity
      private :: hd_lic_color_comp
      private :: hd_lic_opacity_fld, hd_lic_opacity_comp
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
        call load_one_line_from_control(id_control, c_buf)
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
      end do
      color%i_pvr_colordef = 1
!
      end subroutine read_pvr_colordef_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_colordef_ctl(color)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(pvr_colormap_ctl), intent(inout) :: color
!
!
      call calypso_mpi_bcast_one_int(color%i_pvr_colordef, 0)
!
      call bcast_ctl_array_r2(color%colortbl_ctl)
      call bcast_ctl_array_r2(color%linear_opacity_ctl)
!
      call bcast_ctl_array_r3(color%step_opacity_ctl)
!
      call bcast_ctl_type_c1(color%lic_color_fld_ctl)
      call bcast_ctl_type_c1(color%lic_color_comp_ctl)
      call bcast_ctl_type_c1(color%lic_opacity_fld_ctl)
      call bcast_ctl_type_c1(color%lic_opacity_comp_ctl)
!
      call bcast_ctl_type_c1(color%colormap_mode_ctl)
      call bcast_ctl_type_c1(color%data_mapping_ctl)
      call bcast_ctl_type_c1(color%opacity_style_ctl)
!
      call bcast_ctl_type_r1(color%range_min_ctl)
      call bcast_ctl_type_r1(color%range_max_ctl)
      call bcast_ctl_type_r1(color%fix_opacity_ctl)
!
      end subroutine bcast_pvr_colordef_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_colormap_flags(color)
!
      type(pvr_colormap_ctl), intent(inout) :: color
!
!
      color%lic_color_fld_ctl%iflag =    0
      color%lic_color_comp_ctl%iflag =   0
      color%lic_opacity_fld_ctl%iflag =  0
      color%lic_opacity_comp_ctl%iflag = 0
!
      color%colormap_mode_ctl%iflag =   0
      color%data_mapping_ctl%iflag =    0
      color%range_min_ctl%iflag =       0
      color%range_max_ctl%iflag =       0
      color%opacity_style_ctl%iflag =   0
      color%fix_opacity_ctl%iflag =     0
!
      color%i_pvr_colordef = 0
!
      end subroutine reset_pvr_colormap_flags
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_color_crl(color)
!
      type(pvr_colormap_ctl), intent(inout) :: color
!
!
      call reset_pvr_colormap_flags(color)
      call dealloc_control_array_r3(color%step_opacity_ctl)
      call dealloc_control_array_r2(color%linear_opacity_ctl)
      call dealloc_control_array_r2(color%colortbl_ctl)
!
      color%colortbl_ctl%num =       0
      color%colortbl_ctl%icou = 0
      color%step_opacity_ctl%num =   0
      color%step_opacity_ctl%icou =    0
      color%linear_opacity_ctl%num = 0
      color%linear_opacity_ctl%icou =  0
!
      end subroutine dealloc_pvr_color_crl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_pvr_colordef_ctl(org_color, new_color)
!
      type(pvr_colormap_ctl), intent(in) :: org_color
      type(pvr_colormap_ctl), intent(inout) :: new_color
!
!
      new_color%i_pvr_colordef = org_color%i_pvr_colordef
!
      call dup_control_array_r2(org_color%colortbl_ctl,                 &
     &                          new_color%colortbl_ctl)
      call dup_control_array_r2(org_color%linear_opacity_ctl,           &
     &                          new_color%linear_opacity_ctl)
!
      call dup_control_array_r3(org_color%step_opacity_ctl,             &
     &                          new_color%step_opacity_ctl)
!
      call copy_chara_ctl(org_color%lic_color_fld_ctl,                  &
     &                    new_color%lic_color_fld_ctl)
      call copy_chara_ctl(org_color%lic_color_comp_ctl,                 &
     &                    new_color%lic_color_comp_ctl)
      call copy_chara_ctl(org_color%lic_opacity_fld_ctl,                &
     &                    new_color%lic_opacity_fld_ctl)
      call copy_chara_ctl(org_color%lic_opacity_comp_ctl,               &
     &                    new_color%lic_opacity_comp_ctl)
!
      call copy_chara_ctl(org_color%colormap_mode_ctl,                  &
     &                    new_color%colormap_mode_ctl)
      call copy_chara_ctl(org_color%data_mapping_ctl,                   &
     &                    new_color%data_mapping_ctl)
      call copy_chara_ctl(org_color%opacity_style_ctl,                  &
     &                    new_color%opacity_style_ctl)
!
      call copy_real_ctl(org_color%range_min_ctl,                       &
     &                   new_color%range_min_ctl)
      call copy_real_ctl(org_color%range_max_ctl,                       &
     &                   new_color%range_max_ctl)
      call copy_real_ctl(org_color%fix_opacity_ctl,                     &
     &                   new_color%fix_opacity_ctl)
!
      end subroutine dup_pvr_colordef_ctl
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
      end subroutine set_label_LIC_colormap
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_pvr_colormap
