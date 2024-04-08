!>@file   bcast_pvr_color_ctl.f90
!!@brief  module bcast_pvr_color_ctl
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief colormap control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine bcast_pvr_colordef_ctl(color)
!!        type(pvr_colormap_ctl), intent(inout) :: color
!!      subroutine bcast_lighting_ctl(light)
!!        type(pvr_light_ctl), intent(in) :: light
!!      subroutine bcast_pvr_colorbar_ctl(cbar_ctl)
!!        type(pvr_colorbar_ctl), intent(inout) :: cbar_ctl
!!
!!      subroutine bcast_pvr_render_area_ctl(render_area_c)
!!        type(pvr_render_area_ctl), intent(inout) :: render_area_c
!!@endverbatim
!
      module bcast_pvr_color_ctl
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_colordef_ctl(color)
!
      use t_ctl_data_pvr_colormap
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(pvr_colormap_ctl), intent(inout) :: color
!
!
      call calypso_mpi_bcast_character(color%block_name,                &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(color%i_pvr_colordef, 0)
!
      call bcast_ctl_array_r2(color%colortbl_ctl)
      call bcast_ctl_array_r2(color%linear_opacity_ctl)
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
      call bcast_ctl_type_r3(color%background_color_ctl)
!
      end subroutine bcast_pvr_colordef_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_lighting_ctl(light)
!
      use t_ctl_data_pvr_light
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(pvr_light_ctl), intent(inout) :: light
!
!
      call calypso_mpi_bcast_character(light%block_name,                &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(light%i_pvr_lighting, 0)
!
      call bcast_ctl_array_r3(light%light_position_ctl)
      call bcast_ctl_array_r3(light%light_sph_posi_ctl)
!
      call bcast_ctl_type_r1(light%ambient_coef_ctl )
      call bcast_ctl_type_r1(light%diffuse_coef_ctl )
      call bcast_ctl_type_r1(light%specular_coef_ctl)
!
      end subroutine bcast_lighting_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_colorbar_ctl(cbar_ctl)
!
      use t_ctl_data_pvr_colorbar
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(pvr_colorbar_ctl), intent(inout) :: cbar_ctl
!
!
      call calypso_mpi_bcast_character(cbar_ctl%block_name,             &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(cbar_ctl%i_pvr_colorbar, 0)
!
      call bcast_ctl_type_i1(cbar_ctl%font_size_ctl)
      call bcast_ctl_type_i1(cbar_ctl%ngrid_cbar_ctl)
!
      call bcast_ctl_type_c1(cbar_ctl%colorbar_switch_ctl)
      call bcast_ctl_type_c1(cbar_ctl%colorbar_scale_ctl)
      call bcast_ctl_type_c1(cbar_ctl%colorbar_position_ctl)
      call bcast_ctl_type_c1(cbar_ctl%zeromarker_flag_ctl)
!
      call bcast_ctl_type_c1(cbar_ctl%axis_switch_ctl)
      call bcast_ctl_type_c1(cbar_ctl%time_switch_ctl)
      call bcast_ctl_type_c1(cbar_ctl%mapgrid_switch_ctl)
!
      call bcast_ctl_type_r2(cbar_ctl%cbar_range_ctl)
!
      end subroutine bcast_pvr_colorbar_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_render_area_ctl(render_area_c)
!
      use t_ctl_data_pvr_area
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(pvr_render_area_ctl), intent(inout) :: render_area_c
!
!
      call calypso_mpi_bcast_character(render_area_c%block_name,        &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(render_area_c%i_plot_area, 0)
!
      call bcast_ctl_array_c1(render_area_c%pvr_area_ctl)
      call bcast_ctl_array_c2r(render_area_c%surf_enhanse_ctl)
!
      end subroutine bcast_pvr_render_area_ctl
!
!  ---------------------------------------------------------------------
!
      end module bcast_pvr_color_ctl
