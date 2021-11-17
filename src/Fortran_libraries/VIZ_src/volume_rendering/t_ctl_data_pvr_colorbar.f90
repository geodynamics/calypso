!>@file   t_ctl_data_pvr_colorbar.f90
!!@brief  module t_ctl_data_pvr_colorbar
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief colormap control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine read_pvr_colorbar_ctl                                &
!!     &          (id_control, hd_block, cbar_ctl, c_buf)
!!      subroutine bcast_pvr_colorbar_ctl(cbar_ctl)
!!      subroutine reset_pvr_colorbar_ctl_flags(cbar_ctl)
!!        type(pvr_colorbar_ctl), intent(inout) :: cbar_ctl
!!      subroutine copy_pvr_colorbar_ctl(org_cbar_c, new_cbar_c)
!!        type(pvr_colorbar_ctl), intent(in) :: org_cbar_c
!!        type(pvr_colorbar_ctl), intent(inout) :: new_cbar_c
!!
!!      integer(kind = kint) function num_label_pvr_colorbar()
!!      subroutine set_label_pvr_colorbar(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of color control for Kemo's volume rendering
!!
!!begin volume_rendering   (BMP or PNG)
!!  begin colorbar_ctl
!!    colorbar_switch_ctl    ON
!!    colorbar_scale_ctl     ON
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
      use calypso_mpi
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
!
      type pvr_colorbar_ctl
        type(read_character_item) :: colorbar_switch_ctl
        type(read_character_item) :: colorbar_scale_ctl
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
!     3rd level for colorbar
!
      character(len=kchara)                                             &
     &                    :: hd_colorbar_switch = 'colorbar_switch_ctl'
      character(len=kchara) :: hd_colorbar_scale = 'colorbar_scale_ctl'
      character(len=kchara) :: hd_pvr_font_size = 'font_size_ctl'
      character(len=kchara) :: hd_pvr_numgrid_cbar = 'num_grid_ctl'
      character(len=kchara) :: hd_zeromarker_flag = 'iflag_zeromarker'
      character(len=kchara) :: hd_cbar_range = 'colorbar_range'
!
      character(len=kchara) :: hd_axis_switch = 'axis_label_switch'
      character(len=kchara) :: hd_time_switch = 'time_label_switch'
!
      integer(kind = kint), parameter :: n_label_pvr_colorbar = 8
!
      private :: hd_colorbar_switch, hd_colorbar_scale
      private :: hd_pvr_font_size, hd_cbar_range
      private :: hd_pvr_numgrid_cbar, hd_zeromarker_flag
      private :: hd_axis_switch, hd_time_switch, n_label_pvr_colorbar
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_colorbar_ctl                                  &
     &          (id_control, hd_block, cbar_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pvr_colorbar_ctl), intent(inout) :: cbar_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(cbar_ctl%i_pvr_colorbar .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_pvr_font_size, cbar_ctl%font_size_ctl)
        call read_integer_ctl_type(c_buf, hd_pvr_numgrid_cbar,          &
     &      cbar_ctl%ngrid_cbar_ctl)
!
!
        call read_chara_ctl_type(c_buf, hd_colorbar_switch,             &
     &      cbar_ctl%colorbar_switch_ctl)
        call read_chara_ctl_type(c_buf, hd_colorbar_scale,              &
     &      cbar_ctl%colorbar_scale_ctl)
        call read_chara_ctl_type(c_buf, hd_zeromarker_flag,             &
     &      cbar_ctl%zeromarker_flag_ctl)
!
        call read_chara_ctl_type(c_buf, hd_axis_switch,                 &
     &      cbar_ctl%axis_switch_ctl)
        call read_chara_ctl_type(c_buf, hd_time_switch,                 &
     &      cbar_ctl%time_switch_ctl)
!
        call read_real2_ctl_type(c_buf,                                 &
     &      hd_cbar_range, cbar_ctl%cbar_range_ctl)
      end do
      cbar_ctl%i_pvr_colorbar = 1
!
      end subroutine read_pvr_colorbar_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_colorbar_ctl(cbar_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(pvr_colorbar_ctl), intent(inout) :: cbar_ctl
!
!
      call calypso_mpi_bcast_one_int(cbar_ctl%i_pvr_colorbar, 0)
!
      call bcast_ctl_type_i1(cbar_ctl%font_size_ctl)
      call bcast_ctl_type_i1(cbar_ctl%ngrid_cbar_ctl)
!
      call bcast_ctl_type_c1(cbar_ctl%colorbar_switch_ctl)
      call bcast_ctl_type_c1(cbar_ctl%colorbar_scale_ctl)
      call bcast_ctl_type_c1(cbar_ctl%zeromarker_flag_ctl)
!
      call bcast_ctl_type_c1(cbar_ctl%axis_switch_ctl)
      call bcast_ctl_type_c1(cbar_ctl%time_switch_ctl)
!
      call bcast_ctl_type_r2(cbar_ctl%cbar_range_ctl)
!
      end subroutine bcast_pvr_colorbar_ctl
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
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_pvr_colorbar()
      num_label_pvr_colorbar = n_label_pvr_colorbar
      return
      end function num_label_pvr_colorbar
!
! ----------------------------------------------------------------------
!
      subroutine set_label_pvr_colorbar(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_pvr_colorbar)
!
!
      call set_control_labels(hd_colorbar_switch,  names( 1))
      call set_control_labels(hd_colorbar_scale,   names( 2))
      call set_control_labels(hd_pvr_font_size,    names( 3))
      call set_control_labels(hd_pvr_numgrid_cbar, names( 4))
      call set_control_labels(hd_zeromarker_flag,  names( 5))
      call set_control_labels(hd_cbar_range,       names( 6))
!
      call set_control_labels(hd_axis_switch,      names( 7))
      call set_control_labels(hd_time_switch,      names( 8))
!
      end subroutine set_label_pvr_colorbar
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_pvr_colorbar
