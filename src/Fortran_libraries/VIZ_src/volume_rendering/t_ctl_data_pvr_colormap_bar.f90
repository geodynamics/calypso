!>@file   t_ctl_data_pvr_colormap_bar.f90
!!@brief  module t_ctl_data_pvr_colormap_bar
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief colormap control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine sel_read_ctl_pvr_colormap_file                       &
!!     &         (id_control, hd_block, file_name, cmap_cbar_c, c_buf)
!!      subroutine read_pvr_cmap_cbar                                   &
!!     &         (id_control, hd_block, cmap_cbar_c, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        character(len = kchara), intent(inout) :: file_name
!!        type(pvr_colormap_bar_ctl), intent(inout) :: cmap_cbar_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!        type(pvr_colormap_bar_ctl), intent(inout) :: cmap_cbar_c
!!
!!      subroutine sel_write_ctl_pvr_colormap_file                      &
!!     &         (id_control, hd_block, file_name, cmap_cbar_c, level)
!!      subroutine write_control_pvr_colormap_file                      &
!!     &         (id_control, file_name, hd_block, cmap_cbar_c)
!!      subroutine write_pvr_cmap_cbar(id_control, hd_block,            &
!!     &                               cmap_cbar_c, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: file_name
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_colormap_bar_ctl), intent(in) :: cmap_cbar_c
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine deallocate_pvr_cmap_cbar(cmap_cbar_c)
!!        type(pvr_colormap_bar_ctl), intent(inout) :: cmap_cbar_c
!!
!!      subroutine dup_pvr_cmap_cbar(org_cmap_cbar_c, new_cmap_cbar_c)
!!        type(pvr_colormap_bar_ctl), intent(in) :: org_cmap_cbar_c
!!        type(pvr_colormap_bar_ctl), intent(inout) :: new_cmap_cbar_c
!!
!!      integer(kind = kint) function num_label_pvr_cmap_bar()
!!      subroutine set_label_pvr_cmap_bar(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of color control for Kemo's volume rendering
!!
!!  begin pvr_color_ctl   (BMP or PNG)
!!    begin colormap_ctl
!!      colormap_mode_ctl       rainbow
!!      background_color_ctl    0.0   0.0   0.0
!!!
!!      LIC_color_field             magnetic_field
!!      LIC_color_componenet        magnitude
!!
!!      LIC_transparent_field         magnetic_field
!!      LIC_transparent_componenet    magnitude
!!!
!!      data_mapping_ctl   Colormap_list
!!      array color_table_ctl
!!        color_table_ctl    0.0   0.0
!!        color_table_ctl    0.5   0.5
!!        color_table_ctl    1.0   1.0
!!      end array color_table_ctl
!!!
!!      opacity_style_ctl              point_linear
!!      array  linear_opacity_ctl
!!        linear_opacity_ctl   0.0     0.01
!!        linear_opacity_ctl   0.01    0.015
!!        linear_opacity_ctl   0.2     0.02
!!        linear_opacity_ctl   0.6     0.04
!!        linear_opacity_ctl   0.7     0.03
!!        linear_opacity_ctl   0.85    0.01
!!        linear_opacity_ctl   0.95    0.001
!!      end array linear_opacity_ctl
!!
!!      array  step_opacity_ctl
!!        step_opacity_ctl   0.0     0.01    0.01
!!        step_opacity_ctl   0.01    0.2     0.015
!!        step_opacity_ctl   0.2     0.35    0.02
!!        step_opacity_ctl   0.6     0.7     0.04
!!        step_opacity_ctl   0.7     0.85    0.03
!!        step_opacity_ctl   0.85    0.95    0.01
!!        step_opacity_ctl   0.95    1.0     0.001
!!      end array step_opacity_ctl
!!      constant_opacity_ctl           0.003
!!!
!!      range_min_ctl   0.0
!!      range_max_ctl   1.0
!!    end   colormap_ctl
!!
!!    begin colorbar_ctl
!!      colorbar_switch_ctl    ON
!!      colorbar_position_ctl  'left' or 'bottom'
!!      colorbar_scale_ctl     ON
!!      iflag_zeromarker       ON
!!      colorbar_range     0.0   1.0
!!      font_size_ctl         3
!!      num_grid_ctl     4
!!!
!!      axis_label_switch      ON
!!    end colorbar_ctl
!!  end pvr_color_ctl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_pvr_colormap_bar
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_pvr_colormap
      use t_ctl_data_pvr_colorbar
      use skip_comment_f
!
      implicit  none
!
!
!>  Structure of control data for PVR colormap and colorbar
      type pvr_colormap_bar_ctl
!>    Structure for colormap
        type(pvr_colormap_ctl) :: color
!>    Structure for colorbar
        type(pvr_colorbar_ctl) :: cbar_ctl
!
        integer (kind=kint) :: i_cmap_cbar = 0
      end type pvr_colormap_bar_ctl
!
!     2nd level for colormap and colorbar
!
      character(len=kchara) :: hd_colormap_file =  "pvr_color_ctl"
!
      character(len=kchara) :: hd_colormap =      'colormap_ctl'
      character(len=kchara) :: hd_pvr_colorbar =  'colorbar_ctl'
      integer(kind = kint), parameter :: n_label_pvr_cmap_bar = 2
!
      private :: hd_colormap, hd_pvr_colorbar, n_label_pvr_cmap_bar
      private :: hd_colormap_file
!
      private ::  read_control_pvr_colormap_file
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_ctl_pvr_colormap_file                         &
     &         (id_control, hd_block, file_name, cmap_cbar_c, c_buf)
!
      use write_control_elements
      use ctl_data_pvr_colorbar_IO
      use ctl_data_pvr_colormap_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      character(len = kchara), intent(inout) :: file_name
      type(pvr_colormap_bar_ctl), intent(inout) :: cmap_cbar_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
!
      if(check_file_flag(c_buf, hd_block)) then
        file_name = third_word(c_buf)
!
        call write_one_ctl_file_message                                 &
     &     (hd_block, c_buf%level, file_name)
        call read_control_pvr_colormap_file                             &
     &     (id_control+1, file_name, hd_block, cmap_cbar_c, c_buf)
      else if(cmap_cbar_c%i_cmap_cbar .eq. 0) then
        file_name = 'NO_FILE'
!
        call read_pvr_colordef_ctl(id_control, hd_colormap_file,        &
     &      cmap_cbar_c%color, c_buf)
        call read_pvr_colordef_ctl(id_control, hd_colormap,             &
     &      cmap_cbar_c%color, c_buf)
!
        call read_pvr_colorbar_ctl(id_control, hd_pvr_colorbar,         &
     &      cmap_cbar_c%cbar_ctl, c_buf)
      end if
!
      end subroutine sel_read_ctl_pvr_colormap_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_pvr_colormap_file                         &
     &         (id_control, file_name, hd_block, cmap_cbar_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(pvr_colormap_bar_ctl), intent(inout) :: cmap_cbar_c
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(file_name .eq. 'NO_FILE') then
        write(*,*)  'Colormap control is included'
        return
      end if
!
!
      c_buf%level = c_buf%level + 1
      open(id_control, file = file_name, status='old')
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_pvr_cmap_cbar(id_control, hd_block,                   &
     &      cmap_cbar_c, c_buf)
        call read_pvr_cmap_cbar(id_control, hd_colormap_file,           &
     &      cmap_cbar_c, c_buf)
        if(cmap_cbar_c%i_cmap_cbar .gt. 0) exit
      end do
      close(id_control)
!
      c_buf%level = c_buf%level - 1
!
      end subroutine read_control_pvr_colormap_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_cmap_cbar                                     &
     &         (id_control, hd_block, cmap_cbar_c, c_buf)
!
      use ctl_data_pvr_colorbar_IO
      use ctl_data_pvr_colormap_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pvr_colormap_bar_ctl), intent(inout) :: cmap_cbar_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(cmap_cbar_c%i_cmap_cbar .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_pvr_colordef_ctl(id_control, hd_colormap,             &
     &      cmap_cbar_c%color, c_buf)
        call read_pvr_colorbar_ctl(id_control, hd_pvr_colorbar,         &
     &      cmap_cbar_c%cbar_ctl, c_buf)
      end do
      cmap_cbar_c%i_cmap_cbar = 1
!
      end subroutine read_pvr_cmap_cbar
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_write_ctl_pvr_colormap_file                        &
     &         (id_control, hd_block, file_name, cmap_cbar_c, level)
!
      use ctl_data_pvr_colorbar_IO
      use ctl_data_pvr_colormap_IO
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(pvr_colormap_bar_ctl), intent(in) :: cmap_cbar_c
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(cmp_no_case(file_name, 'NO_FILE')) then
        call write_pvr_colordef_ctl(id_control, hd_colormap,            &
     &      cmap_cbar_c%color, level)
        call write_pvr_colorbar_ctl(id_control, hd_pvr_colorbar,        &
     &      cmap_cbar_c%cbar_ctl, level)
      else if(id_control .eq. id_monitor) then
        write(*,'(4a)') '!  ', trim(hd_block),                           &
     &        ' should be written to file ... ', trim(file_name)
        call write_pvr_colorbar_ctl(id_control, hd_pvr_colorbar,        &
     &      cmap_cbar_c%cbar_ctl, level)
      else
        write(*,'(3a)') trim(hd_block),                                 &
     &        ' is written to file ... ', trim(file_name)
        call write_file_name_for_ctl_line(id_control, level,            &
     &                                    hd_block, file_name)
        call write_control_pvr_colormap_file                            &
     &     (id_control+1, file_name, hd_block, cmap_cbar_c)
      end if
!
      end subroutine sel_write_ctl_pvr_colormap_file
!
!  ---------------------------------------------------------------------
!
      subroutine write_control_pvr_colormap_file                        &
     &         (id_control, file_name, hd_block, cmap_cbar_c)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(pvr_colormap_bar_ctl), intent(in) :: cmap_cbar_c
!
      integer(kind = kint) :: level
!
!
      if(file_name .eq. 'NO_FILE') return
!
      level = 0
      open(id_control, file = file_name)
      call write_pvr_cmap_cbar(id_control, hd_block,                    &
     &                         cmap_cbar_c, level)
      close(id_control)
!
      end subroutine write_control_pvr_colormap_file
!
!  ---------------------------------------------------------------------
!
      subroutine write_pvr_cmap_cbar(id_control, hd_block,              &
     &                               cmap_cbar_c, level)
!
      use ctl_data_pvr_colorbar_IO
      use ctl_data_pvr_colormap_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_colormap_bar_ctl), intent(in) :: cmap_cbar_c
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(cmap_cbar_c%i_cmap_cbar .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_pvr_colordef_ctl(id_control, hd_colormap,              &
     &    cmap_cbar_c%color, level)
      call write_pvr_colorbar_ctl(id_control, hd_pvr_colorbar,          &
     &    cmap_cbar_c%cbar_ctl, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_pvr_cmap_cbar
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_pvr_cmap_cbar(cmap_cbar_c)
!
      type(pvr_colormap_bar_ctl), intent(inout) :: cmap_cbar_c
!
      call reset_pvr_colorbar_ctl_flags(cmap_cbar_c%cbar_ctl)
      call dealloc_pvr_color_crl(cmap_cbar_c%color)
!
      cmap_cbar_c%i_cmap_cbar = 0
!
      end subroutine deallocate_pvr_cmap_cbar
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_pvr_cmap_cbar(org_cmap_cbar_c, new_cmap_cbar_c)
!
      type(pvr_colormap_bar_ctl), intent(in) :: org_cmap_cbar_c
      type(pvr_colormap_bar_ctl), intent(inout) :: new_cmap_cbar_c
!
!
      new_cmap_cbar_c%i_cmap_cbar = org_cmap_cbar_c%i_cmap_cbar
!
      call dup_pvr_colordef_ctl(org_cmap_cbar_c%color,                  &
     &                          new_cmap_cbar_c%color)
      call copy_pvr_colorbar_ctl(org_cmap_cbar_c%cbar_ctl,              &
     &                           new_cmap_cbar_c%cbar_ctl)
!
      end subroutine dup_pvr_cmap_cbar
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_pvr_cmap_bar()
      num_label_pvr_cmap_bar = n_label_pvr_cmap_bar
      return
      end function num_label_pvr_cmap_bar
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_cmap_bar(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_pvr_cmap_bar)
!
!
      call set_control_labels(hd_colormap,     names( 1))
      call set_control_labels(hd_pvr_colorbar, names( 2))
!
      end subroutine set_label_pvr_cmap_bar
!
! ----------------------------------------------------------------------
!
      end module t_ctl_data_pvr_colormap_bar
