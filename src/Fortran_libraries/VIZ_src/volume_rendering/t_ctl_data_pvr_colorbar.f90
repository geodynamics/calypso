!>@file   t_ctl_data_pvr_colorbar.f90
!!@brief  module t_ctl_data_pvr_colorbar
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief colormap control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine reset_pvr_colorbar_ctl_flags(cbar_ctl)
!!        type(pvr_colorbar_ctl), intent(inout) :: cbar_ctl
!!      subroutine copy_pvr_colorbar_ctl(org_cbar_c, new_cbar_c)
!!        type(pvr_colorbar_ctl), intent(in) :: org_cbar_c
!!        type(pvr_colorbar_ctl), intent(inout) :: new_cbar_c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of color control for Kemo's volume rendering
!!
!!begin volume_rendering   (BMP or PNG)
!!  begin colorbar_ctl
!!    colorbar_switch_ctl    ON
!!    colorbar_scale_ctl     ON
!!    colorbar_position_ctl  'side' or 'bottom'
!!    iflag_zeromarker       ON
!!    colorbar_range     0.0   1.0
!!    font_size_ctl         3
!!    num_grid_ctl     4
!!!
!!    axis_label_switch      ON
!!    time_label_switch      ON
!!  end colorbar_ctl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_pvr_colorbar
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_view_transfer
      use t_control_array_character
      use t_control_array_integer
      use t_control_array_real2
      use skip_comment_f
!
      implicit  none
!
      type pvr_colorbar_ctl
        type(read_character_item) :: colorbar_switch_ctl
        type(read_character_item) :: colorbar_scale_ctl
        type(read_character_item) :: colorbar_position_ctl
        type(read_character_item) :: zeromarker_flag_ctl
        type(read_integer_item) ::   font_size_ctl
        type(read_integer_item) ::   ngrid_cbar_ctl
        type(read_real2_item) ::     cbar_range_ctl
!
        type(read_character_item) :: axis_switch_ctl
        type(read_character_item) :: time_switch_ctl
!
!     2nd level for volume rendering
        integer(kind = kint) :: i_pvr_colorbar = 0
      end type pvr_colorbar_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_colorbar_ctl_flags(cbar_ctl)
!
      type(pvr_colorbar_ctl), intent(inout) :: cbar_ctl
!
!
      cbar_ctl%colorbar_switch_ctl%iflag = 0
      cbar_ctl%colorbar_scale_ctl%iflag =  0
      cbar_ctl%colorbar_position_ctl%iflag =  0
      cbar_ctl%font_size_ctl%iflag =       0
      cbar_ctl%ngrid_cbar_ctl%iflag =      0
      cbar_ctl%zeromarker_flag_ctl%iflag = 0
      cbar_ctl%cbar_range_ctl%iflag =      0
!
      cbar_ctl%i_pvr_colorbar = 0
!
      end subroutine reset_pvr_colorbar_ctl_flags
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_pvr_colorbar_ctl(org_cbar_c, new_cbar_c)
!
      type(pvr_colorbar_ctl), intent(in) :: org_cbar_c
      type(pvr_colorbar_ctl), intent(inout) :: new_cbar_c
!
!
      new_cbar_c%i_pvr_colorbar = org_cbar_c%i_pvr_colorbar
!
      call copy_integer_ctl(org_cbar_c%font_size_ctl,                   &
     &                      new_cbar_c%font_size_ctl)
      call copy_integer_ctl(org_cbar_c%ngrid_cbar_ctl,                  &
     &                      new_cbar_c%ngrid_cbar_ctl)
!
      call copy_chara_ctl(org_cbar_c%colorbar_switch_ctl,               &
     &                    new_cbar_c%colorbar_switch_ctl)
      call copy_chara_ctl(org_cbar_c%colorbar_scale_ctl,                &
     &                    new_cbar_c%colorbar_scale_ctl)
      call copy_chara_ctl(org_cbar_c%colorbar_position_ctl,             &
     &                    new_cbar_c%colorbar_position_ctl)
      call copy_chara_ctl(org_cbar_c%zeromarker_flag_ctl,               &
     &                    new_cbar_c%zeromarker_flag_ctl)
!
      call copy_chara_ctl(org_cbar_c%axis_switch_ctl,                   &
     &                    new_cbar_c%axis_switch_ctl)
      call copy_chara_ctl(org_cbar_c%time_switch_ctl,                   &
     &                    new_cbar_c%time_switch_ctl)
!
      call copy_real2_ctl(org_cbar_c%cbar_range_ctl,                    &
     &                    new_cbar_c%cbar_range_ctl)
!
      end subroutine copy_pvr_colorbar_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_pvr_colorbar
