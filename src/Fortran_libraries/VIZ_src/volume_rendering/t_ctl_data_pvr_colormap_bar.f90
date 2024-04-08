!>@file   t_ctl_data_pvr_colormap_bar.f90
!!@brief  module t_ctl_data_pvr_colormap_bar
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief colormap control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine init_pvr_cmap_cbar_label(hd_block, cmap_cbar_c)
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
!!      logical function cmp_pvr_colormap_bar_ctl(cmap_cbar1,           &
!!     &                                          cmap_cbar2)
!!        type(pvr_colormap_bar_ctl), intent(in) :: cmap_cbar1
!!        type(pvr_colormap_bar_ctl), intent(in) :: cmap_cbar2
!!
!!      subroutine deallocate_pvr_cmap_cbar(cmap_cbar_c)
!!        type(pvr_colormap_bar_ctl), intent(inout) :: cmap_cbar_c
!!
!!      subroutine dup_pvr_cmap_cbar(org_cmap_cbar_c, new_cmap_cbar_c)
!!        type(pvr_colormap_bar_ctl), intent(in) :: org_cmap_cbar_c
!!        type(pvr_colormap_bar_ctl), intent(inout) :: new_cmap_cbar_c
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
!!      zeromarker_switch      ON
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
!>        Control block name
        character(len = kchara) :: block_name = 'pvr_color_ctl'
!>        Structure for colormap
        type(pvr_colormap_ctl) :: color
!>        Structure for colorbar
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
!
      private :: hd_colormap, hd_pvr_colorbar
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
     &     (id_control+2, file_name, hd_block, cmap_cbar_c, c_buf)
!
      else if(check_begin_flag(c_buf, hd_block)) then
        file_name = 'NO_FILE'
        call write_included_message(hd_block, c_buf%level)
        call read_pvr_cmap_cbar(id_control, hd_block,                   &
     &                          cmap_cbar_c, c_buf)
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
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(pvr_colormap_bar_ctl), intent(inout) :: cmap_cbar_c
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(no_file_flag(file_name)) then
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
      if(cmap_cbar_c%i_cmap_cbar .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
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
!
      subroutine init_pvr_cmap_cbar_label(hd_block, cmap_cbar_c)
!
      use ctl_data_pvr_colorbar_IO
      use ctl_data_pvr_colormap_IO
!
      character(len=kchara), intent(in) :: hd_block
      type(pvr_colormap_bar_ctl), intent(inout) :: cmap_cbar_c
!
!
      cmap_cbar_c%block_name = hd_block
      call init_pvr_colordef_ctl_labels(hd_colormap,                    &
     &                                  cmap_cbar_c%color)
      call init_pvr_colorbar_ctl_label(hd_pvr_colorbar,                 &
     &                                 cmap_cbar_c%cbar_ctl)
!
      end subroutine init_pvr_cmap_cbar_label
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
        write(*,'(4a)') '!  ', trim(hd_block),                          &
     &        ' should be written to file ... ', trim(file_name)
        call write_pvr_colorbar_ctl(id_control, hd_pvr_colorbar,        &
     &      cmap_cbar_c%cbar_ctl, level)
      else
        write(*,'(3a)') trim(hd_block),                                 &
     &        ' is written to file ... ', trim(file_name)
        call write_file_name_for_ctl_line(id_control, level,            &
     &                                    hd_block, file_name)
        call write_control_pvr_colormap_file                            &
     &     (id_control+2, file_name, hd_block, cmap_cbar_c)
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
      if(no_file_flag(file_name)) return
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
!
      logical function cmp_pvr_colormap_bar_ctl(cmap_cbar1,             &
     &                                          cmap_cbar2)
!
      type(pvr_colormap_bar_ctl), intent(in) :: cmap_cbar1
      type(pvr_colormap_bar_ctl), intent(in) :: cmap_cbar2
!
      cmp_pvr_colormap_bar_ctl = .FALSE.
      if(cmap_cbar1%i_cmap_cbar .ne. cmap_cbar2%i_cmap_cbar) return
      if(cmp_no_case(trim(cmap_cbar1%block_name),                       &
     &               trim(cmap_cbar2%block_name)) .eqv. .FALSE.) return
      if(cmp_pvr_colormap_ctl(cmap_cbar1%color, cmap_cbar2%color)       &
     &                                            .eqv. .FALSE.) return
      if(cmp_pvr_colorbar_ctl(cmap_cbar1%cbar_ctl, cmap_cbar2%cbar_ctl) &
     &                                            .eqv. .FALSE.) return
!
      cmp_pvr_colormap_bar_ctl = .TRUE.
!
      end function cmp_pvr_colormap_bar_ctl
!
!   --------------------------------------------------------------------
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
      new_cmap_cbar_c%block_name = org_cmap_cbar_c%block_name
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
!
      end module t_ctl_data_pvr_colormap_bar
