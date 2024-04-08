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
!!      logical function cmp_pvr_colorbar_ctl(cbar_ctl1, cbar_ctl2)
!!        type(read_character_item), intent(in) :: cbar_ctl1, cbar_ctl2
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of color control for Kemo's volume rendering
!!
!!begin volume_rendering   (BMP or PNG)
!!  begin colorbar_ctl
!!    colorbar_switch_ctl    ON
!!    colorbar_scale_ctl     ON
!!    colorbar_position_ctl  'side' or 'bottom'
!!    zeromarker_switch      ON
!!    colorbar_range     0.0   1.0
!!    font_size_ctl         3
!!    num_grid_ctl     4
!!!
!!    axis_label_switch      ON
!!    time_label_switch      ON
!!    map_grid_switch        ON
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
!
      implicit  none
!
      type pvr_colorbar_ctl
!>        Control block name
        character(len = kchara) :: block_name = 'colorbar_ctl'
!
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
        type(read_character_item) :: mapgrid_switch_ctl
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
      call copy_chara_ctl(org_cbar_c%mapgrid_switch_ctl,                &
     &                    new_cbar_c%mapgrid_switch_ctl)
!
      call copy_real2_ctl(org_cbar_c%cbar_range_ctl,                    &
     &                    new_cbar_c%cbar_range_ctl)
!
      end subroutine copy_pvr_colorbar_ctl
!
!  ---------------------------------------------------------------------
!
      logical function cmp_pvr_colorbar_ctl(cbar_ctl1, cbar_ctl2)
!
      use skip_comment_f
!
      type(pvr_colorbar_ctl), intent(in) :: cbar_ctl1, cbar_ctl2
!
      cmp_pvr_colorbar_ctl = .FALSE.
      if(cbar_ctl1%i_pvr_colorbar .ne. cbar_ctl2%i_pvr_colorbar) return
      if(cmp_no_case(trim(cbar_ctl1%block_name),                        &
     &               trim(cbar_ctl2%block_name)) .eqv. .FALSE.) return
      if(cmp_read_chara_item(cbar_ctl1%colorbar_switch_ctl,             &
     &                       cbar_ctl2%colorbar_switch_ctl)             &
     &                                          .eqv. .FALSE.) return
      if(cmp_read_chara_item(cbar_ctl1%colorbar_scale_ctl,              &
     &                       cbar_ctl2%colorbar_scale_ctl)              &
     &                                          .eqv. .FALSE.) return
      if(cmp_read_chara_item(cbar_ctl1%colorbar_position_ctl,           &
     &                       cbar_ctl2%colorbar_position_ctl)           &
     &                                          .eqv. .FALSE.) return
      if(cmp_read_chara_item(cbar_ctl1%zeromarker_flag_ctl,             &
     &                       cbar_ctl2%zeromarker_flag_ctl)             &
     &                                          .eqv. .FALSE.) return
!
!
      if(cmp_read_integer_item(cbar_ctl1%font_size_ctl,                 &
     &                         cbar_ctl2%font_size_ctl)                 &
     &                                          .eqv. .FALSE.) return
      if(cmp_read_integer_item(cbar_ctl1%ngrid_cbar_ctl,                &
     &                         cbar_ctl2%ngrid_cbar_ctl)                &
     &                                          .eqv. .FALSE.) return
      if(cmp_read_real2_item(cbar_ctl1%cbar_range_ctl,                  &
     &                       cbar_ctl2%cbar_range_ctl)                  &
     &                                          .eqv. .FALSE.) return
!
      if(cmp_read_chara_item(cbar_ctl1%axis_switch_ctl,                 &
     &                       cbar_ctl2%axis_switch_ctl)                 &
     &                                          .eqv. .FALSE.) return
      if(cmp_read_chara_item(cbar_ctl1%time_switch_ctl,                 &
     &                       cbar_ctl2%time_switch_ctl)                 &
     &                                          .eqv. .FALSE.) return
      if(cmp_read_chara_item(cbar_ctl1%mapgrid_switch_ctl,              &
     &                       cbar_ctl2%mapgrid_switch_ctl)              &
     &                                          .eqv. .FALSE.) return
!
      cmp_pvr_colorbar_ctl = .TRUE.
!
      end function cmp_pvr_colorbar_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_pvr_colorbar
