!>@file   ctl_data_pvr_colorbar_IO.f90
!!@brief  module ctl_data_pvr_colorbar_IO
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief colormap control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine read_pvr_colorbar_ctl                                &
!!     &          (id_control, hd_block, cbar_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_colorbar_ctl), intent(inout) :: cbar_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_pvr_colorbar_ctl                               &
!!     &          (id_control, hd_block, cbar_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_colorbar_ctl), intent(in) :: cbar_ctl
!!        integer(kind = kint), intent(inout) :: level
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
!!    colorbar_position_ctl  'side' or 'bottom'
!!    iflag_zeromarker       ON
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
      module ctl_data_pvr_colorbar_IO
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
      use t_ctl_data_pvr_colorbar
      use skip_comment_f
!
      implicit  none
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_colorbar_switch = 'colorbar_switch_ctl'
      character(len=kchara), parameter, private                         &
     &                  :: hd_colorbar_scale = 'colorbar_scale_ctl'
      character(len=kchara), parameter, private                         &
     &                  :: hd_cbar_position = 'colorbar_position_ctl'
      character(len=kchara), parameter, private                         &
     &                  :: hd_pvr_font_size = 'font_size_ctl'
      character(len=kchara), parameter, private                         &
     &                  :: hd_pvr_numgrid_cbar = 'num_grid_ctl'
      character(len=kchara), parameter, private                         &
     &                  :: hd_zeromarker_flag = 'iflag_zeromarker'
      character(len=kchara), parameter, private                         &
     &                  :: hd_cbar_range = 'colorbar_range'
!
      character(len=kchara), parameter, private                         &
     &                  :: hd_axis_switch = 'axis_label_switch'
      character(len=kchara), parameter, private                         &
     &                  :: hd_time_switch = 'time_label_switch'
      character(len=kchara), parameter, private                         &
     &                  :: hd_mapgrid_switch = 'map_grid_switch'
!
      integer(kind = kint), parameter :: n_label_pvr_colorbar = 10
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
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
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
        call read_chara_ctl_type(c_buf, hd_cbar_position,               &
     &      cbar_ctl%colorbar_position_ctl)
        call read_chara_ctl_type(c_buf, hd_zeromarker_flag,             &
     &      cbar_ctl%zeromarker_flag_ctl)
!
        call read_chara_ctl_type(c_buf, hd_axis_switch,                 &
     &      cbar_ctl%axis_switch_ctl)
        call read_chara_ctl_type(c_buf, hd_time_switch,                 &
     &      cbar_ctl%time_switch_ctl)
        call read_chara_ctl_type(c_buf, hd_mapgrid_switch,              &
     &      cbar_ctl%mapgrid_switch_ctl)
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
      subroutine write_pvr_colorbar_ctl                                 &
     &          (id_control, hd_block, cbar_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_colorbar_ctl), intent(in) :: cbar_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(cbar_ctl%i_pvr_colorbar .le. 0) return
!
      maxlen = len_trim(hd_colorbar_switch)
      maxlen = max(maxlen, len_trim(hd_pvr_font_size))
      maxlen = max(maxlen, len_trim(hd_pvr_numgrid_cbar))
      maxlen = max(maxlen, len_trim(hd_colorbar_scale))
      maxlen = max(maxlen, len_trim(hd_cbar_position))
      maxlen = max(maxlen, len_trim(hd_zeromarker_flag))
      maxlen = max(maxlen, len_trim(hd_axis_switch))
      maxlen = max(maxlen, len_trim(hd_time_switch))
      maxlen = max(maxlen, len_trim(hd_mapgrid_switch))
      maxlen = max(maxlen, len_trim(hd_cbar_range))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_colorbar_switch, cbar_ctl%colorbar_switch_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_colorbar_scale, cbar_ctl%colorbar_scale_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_cbar_position, cbar_ctl%colorbar_position_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_zeromarker_flag, cbar_ctl%zeromarker_flag_ctl)
      call write_real2_ctl_type(id_control, level, maxlen,              &
     &    hd_cbar_range, cbar_ctl%cbar_range_ctl)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_pvr_font_size, cbar_ctl%font_size_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_pvr_numgrid_cbar, cbar_ctl%ngrid_cbar_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_axis_switch, cbar_ctl%axis_switch_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_time_switch, cbar_ctl%time_switch_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_mapgrid_switch, cbar_ctl%mapgrid_switch_ctl)
!
      level = write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_pvr_colorbar_ctl
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
      call set_control_labels(hd_cbar_position,    names( 3))
      call set_control_labels(hd_pvr_font_size,    names( 4))
      call set_control_labels(hd_pvr_numgrid_cbar, names( 5))
      call set_control_labels(hd_zeromarker_flag,  names( 6))
      call set_control_labels(hd_cbar_range,       names( 7))
!
      call set_control_labels(hd_axis_switch,      names( 8))
      call set_control_labels(hd_time_switch,      names( 9))
      call set_control_labels(hd_mapgrid_switch,   names(10))
!
      end subroutine set_label_pvr_colorbar
!
!  ---------------------------------------------------------------------
!
      end module ctl_data_pvr_colorbar_IO
