!>@file   bcast_control_data_4_pvr.f90
!!@brief  module bcast_control_data_4_pvr
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine bcast_vr_psf_ctl(pvr)
!!        type(pvr_parameter_ctl), intent(inout) :: pvr
!!@end verbatim
!
!
      module bcast_control_data_4_pvr
!
      use m_precision
      use calypso_mpi
!
      use t_control_data_4_pvr
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_vr_psf_ctl(pvr)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_control_arrays
      use bcast_pvr_color_ctl
      use bcast_ctl_data_view_trans
      use bcast_ctl_data_pvr_surfaces
      use transfer_to_long_integers
!
      type(pvr_parameter_ctl), intent(inout) :: pvr
!
!
      call calypso_mpi_bcast_one_int(pvr%i_pvr_ctl, 0)
      call calypso_mpi_bcast_character(pvr%block_name,                  &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_character(pvr%fname_mat_ctl,               &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_character(pvr%fname_cmap_cbar_c,           &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_character(pvr%fname_pvr_light_c,           &
     &                                 cast_long(kchara), 0)
!
!
      call bcast_view_transfer_ctl(pvr%mat)
!
      call bcast_pvr_isosurfs_ctl(pvr%pvr_isos_c)
      call bcast_pvr_sections_ctl(pvr%pvr_scts_c)
!
      call bcast_lighting_ctl(pvr%light)
      
      call calypso_mpi_bcast_character(pvr%cmap_cbar_c%block_name,      &
     &                                 cast_long(kchara), 0)
      call bcast_pvr_colorbar_ctl(pvr%cmap_cbar_c%cbar_ctl)
      call bcast_pvr_colordef_ctl(pvr%cmap_cbar_c%color)
!
      call bcast_quilt_image_ctl(pvr%quilt_c)
      call bcast_pvr_moving_view_ctl(pvr%movie)
      call bcast_pvr_render_area_ctl(pvr%render_area_c)
!
      call bcast_ctl_type_c1(pvr%updated_ctl)
      call bcast_ctl_type_c1(pvr%file_head_ctl)
      call bcast_ctl_type_c1(pvr%file_fmt_ctl )
      call bcast_ctl_type_c1(pvr%monitoring_ctl)
!
      call bcast_ctl_type_c1(pvr%streo_ctl)
      call bcast_ctl_type_c1(pvr%anaglyph_ctl)
      call bcast_ctl_type_c1(pvr%quilt_ctl)
!
      call bcast_ctl_type_c1(pvr%pvr_field_ctl)
      call bcast_ctl_type_c1(pvr%pvr_comp_ctl)
!
      end subroutine bcast_vr_psf_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_update_flag(pvr)
!
      use bcast_control_arrays
!
      type(pvr_parameter_ctl), intent(inout) :: pvr
!
!
      call bcast_ctl_type_c1(pvr%updated_ctl)
!
      end subroutine bcast_pvr_update_flag
!
!  ---------------------------------------------------------------------
!
      end module bcast_control_data_4_pvr
