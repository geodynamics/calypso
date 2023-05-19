!>@file   t_ctl_data_pvr_colormap.f90
!!@brief  module t_ctl_data_pvr_colormap
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief colormap control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine reset_pvr_colormap_flags(color)
!!      subroutine dealloc_pvr_color_crl(color)
!!        type(pvr_colormap_ctl), intent(inout) :: color
!!      subroutine dup_pvr_colordef_ctl(org_color, new_color)
!!        type(pvr_colormap_ctl), intent(in) :: org_color
!!        type(pvr_colormap_ctl), intent(inout) :: new_color
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
!  ---------------------------------------------------------------------
!
      contains
!
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
!
      end module t_ctl_data_pvr_colormap