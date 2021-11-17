!>@file   t_ctl_data_pvr_colormap_bar.f90
!!@brief  module t_ctl_data_pvr_colormap_bar
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief colormap control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine read_control_pvr_colormap_file                       &
!!     &         (id_control, color_file_name, hd_block, cmap_cbar_c)
!!        type(pvr_colormap_bar_ctl), intent(inout) :: cmap_cbar_c
!!
!!      subroutine read_pvr_cmap_cbar                                   &
!!     &         (id_control, hd_block, cmap_cbar_c, c_buf)
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
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_pvr_colormap
      use t_ctl_data_pvr_colorbar
      use skip_comment_f
      use bcast_control_arrays
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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_pvr_colormap_file                         &
     &         (id_control, color_file_name, hd_block, cmap_cbar_c)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: color_file_name
      character(len=kchara), intent(in) :: hd_block
      type(pvr_colormap_bar_ctl), intent(inout) :: cmap_cbar_c
!
      type(buffer_for_control) :: c_buf1
!
!
      if(color_file_name .eq. 'NO_FILE') then
        write(*,*)  'Colormap control is included'
        return
      end if
!
      write(*,*) 'Colormap control file:', trim(color_file_name)
      open(id_control, file = color_file_name, status='old')
!
      do
        call load_one_line_from_control(id_control, c_buf1)
        call read_pvr_cmap_cbar(id_control, hd_block,                   &
     &      cmap_cbar_c, c_buf1)
        call read_pvr_cmap_cbar(id_control, hd_colormap_file,           &
     &      cmap_cbar_c, c_buf1)
        if(cmap_cbar_c%i_cmap_cbar .gt. 0) exit
      end do
      close(id_control)
!
      end subroutine read_control_pvr_colormap_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_cmap_cbar                                     &
     &         (id_control, hd_block, cmap_cbar_c, c_buf)
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
        call load_one_line_from_control(id_control, c_buf)
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
