!>@file   t_control_data_4_pvr.f90
!!@brief  module t_control_data_4_pvr
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine deallocate_cont_dat_pvr(pvr_ctl)
!!      subroutine reset_pvr_update_flags(pvr_ctl)
!!        type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
!!
!!      subroutine add_field_4_pvr_to_fld_ctl(pvr_ctl, field_ctl)
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!
!!      subroutine dup_pvr_ctl(org_pvr, new_pvr)
!!      subroutine copy_pvr_update_flag(org_pvr, new_pvr)
!!        type(pvr_parameter_ctl), intent(in) :: org_pvr
!!        type(pvr_parameter_ctl), intent(inout) :: new_pvr
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of control for Kemo's volume rendering
!!
!!begin volume_rendering   (BMP or PNG)
!!  updated_sign         go
!!  pvr_file_prefix      pvr_temp
!!  pvr_output_format    PNG
!!  monitoring_mode      YES
!!
!!  streo_imaging        YES
!!  anaglyph_switch      NO
!!  quilt_3d_imaging     YES
!!!
!!  output_field    temperature
!!  output_component     scalar
!!!
!!  begin plot_area_ctl
!!   ...
!!  end  plot_area_ctl
!!!
!!  begin view_transform_ctl
!!   ...
!!  end view_transform_ctl
!!
!!  begin lighting_ctl
!!   ...
!!  end lighting_ctl
!!
!!  begin pvr_color_ctl
!!   ...
!!  end   pvr_color_ctl
!!!
!!  begin colorbar_ctl
!!   ...
!!  end colorbar_ctl
!!!
!!  array section_ctl
!!    file surface_define     ctl_psf_eq
!!    begin surface_define
!!      ...
!!    end surface_define
!!  end array section_ctl
!!!
!!  array isosurface_ctl  2
!!    begin isosurface_ctl
!!      isosurf_value       0.3
!!      opacity_ctl         0.9
!!      surface_direction   normal
!!    end isosurface_ctl
!!     ...
!!  end array isosurface_ctl
!!!
!!  begin snapshot_movie_ctl
!!   ...
!!  end snapshot_movie_ctl
!!!
!!end volume_rendering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_4_pvr
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_view_transfer
      use t_control_array_integer
      use t_control_array_character
      use t_control_array_chara2real
      use t_ctl_data_pvr_colormap_bar
      use t_ctl_data_pvr_light
      use t_control_data_pvr_sections
      use t_ctl_data_pvr_movie
      use t_ctl_data_quilt_image
      use t_control_data_pvr_isosurfs
      use t_ctl_data_pvr_area
      use skip_comment_f
!
      implicit  none
!
!
!>  Structure of control data for PVR rendering
      type pvr_parameter_ctl
!>        Control block name
        character(len = kchara) :: block_name = 'volume_rendering'
!
!>     file name for modelview matrix
        character(len=kchara) :: fname_mat_ctl = 'NO_FILE'
!>     Structure for modelview marices
        type(modeview_ctl) :: mat
!
!>     file name for lighting parameter
        character(len=kchara) :: fname_pvr_light_c = 'NO_FILE'
!>     Structure for lighting
        type(pvr_light_ctl) :: light
!
!>     file name for colormap and colorbar
        character(len=kchara) :: fname_cmap_cbar_c = 'NO_FILE'
!>     Structure for colormap and colorbar
        type(pvr_colormap_bar_ctl) :: cmap_cbar_c
!
!>     Structure for image rotation
        type(pvr_movie_ctl) :: movie
!>     Structure of quilt image controls
        type(quilt_image_ctl) :: quilt_c
!
        type(read_character_item) :: updated_ctl
!
!>        File prefix of output image file
        type(read_character_item) :: file_head_ctl
!>        File format of output image file
        type(read_character_item) :: file_fmt_ctl
        type(read_character_item) :: monitoring_ctl
!
        type(read_character_item) :: streo_ctl
        type(read_character_item) :: anaglyph_ctl
        type(read_character_item) :: quilt_ctl
!
!>      Structure for element group list for PVR
!!@n      group_4_monitor_ctl%c_tbl: Name of element group for PVR
        type(pvr_render_area_ctl) :: render_area_c
!
!>        Structure of field name for rendering
        type(read_character_item) :: pvr_field_ctl
!>        Structure of component name for rendering
        type(read_character_item) :: pvr_comp_ctl
!
        type(pvr_sections_ctl) :: pvr_scts_c
!
!>       constrol structure for isosurfaces in PVR
        type(pvr_isosurfs_ctl) :: pvr_isos_c
!
!     Top level flag
        integer(kind = kint) :: i_pvr_ctl = 0
      end type pvr_parameter_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_cont_dat_pvr(pvr_ctl)
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
!
!
      call reset_pvr_light_flags(pvr_ctl%light)
      call reset_quilt_image_ctl(pvr_ctl%quilt_c)
      call dealloc_pvr_movie_control_flags(pvr_ctl%movie)
!
      call dealloc_view_transfer_ctl(pvr_ctl%mat)
      call dealloc_pvr_light_crl(pvr_ctl%light)
      call deallocate_pvr_cmap_cbar(pvr_ctl%cmap_cbar_c)
!
      call dealloc_pvr_render_area_ctl(pvr_ctl%render_area_c)
      call dealloc_pvr_isosurfs_ctl(pvr_ctl%pvr_isos_c)
      call dealloc_pvr_sections_ctl(pvr_ctl%pvr_scts_c)
!
      pvr_ctl%updated_ctl%iflag =     0
      pvr_ctl%file_head_ctl%iflag =   0
      pvr_ctl%file_fmt_ctl%iflag =    0
      pvr_ctl%anaglyph_ctl%iflag =    0
      pvr_ctl%pvr_field_ctl%iflag =   0
      pvr_ctl%pvr_comp_ctl%iflag =    0
!
      pvr_ctl%fname_mat_ctl = 'NO_FILE'
      pvr_ctl%fname_pvr_light_c = 'NO_FILE'
      pvr_ctl%fname_cmap_cbar_c = 'NO_FILE'
!
      pvr_ctl%i_pvr_ctl = 0
!
      end subroutine deallocate_cont_dat_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_update_flags(pvr_ctl)
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
!
      pvr_ctl%i_pvr_ctl = 0
      pvr_ctl%updated_ctl%iflag =     0
!
      end subroutine reset_pvr_update_flags
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_field_4_pvr_to_fld_ctl(pvr_ctl, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(pvr_ctl%pvr_field_ctl%iflag .gt. 0) then
        call add_viz_name_ctl                                           &
     &     (pvr_ctl%pvr_field_ctl%charavalue, field_ctl)
      end if
!
      end subroutine add_field_4_pvr_to_fld_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_pvr_ctl(org_pvr, new_pvr)
!
      use t_ctl_data_4_view_transfer
      use bcast_control_arrays
!
      type(pvr_parameter_ctl), intent(in) :: org_pvr
      type(pvr_parameter_ctl), intent(inout) :: new_pvr
!
!
      new_pvr%block_name =        org_pvr%block_name
      new_pvr%i_pvr_ctl =         org_pvr%i_pvr_ctl
      new_pvr%fname_mat_ctl =     org_pvr%fname_mat_ctl
      new_pvr%fname_cmap_cbar_c = org_pvr%fname_cmap_cbar_c
      new_pvr%fname_pvr_light_c = org_pvr%fname_pvr_light_c
!
      call dup_view_transfer_ctl(org_pvr%mat, new_pvr%mat)
!
      call dup_pvr_isosurfs_ctl(org_pvr%pvr_isos_c, new_pvr%pvr_isos_c)
      call dup_pvr_sections_ctl(org_pvr%pvr_scts_c, new_pvr%pvr_scts_c)
!
      call dup_lighting_ctl(org_pvr%light, new_pvr%light)
      call dup_pvr_cmap_cbar(org_pvr%cmap_cbar_c, new_pvr%cmap_cbar_c)
!
      call dup_quilt_image_ctl(org_pvr%quilt_c, new_pvr%quilt_c)
      call dup_pvr_movie_control_flags(org_pvr%movie, new_pvr%movie)
      call dup_pvr_render_area_ctl(org_pvr%render_area_c,               &
     &                             new_pvr%render_area_c)
!
      call copy_chara_ctl(org_pvr%updated_ctl, new_pvr%updated_ctl)
      call copy_chara_ctl(org_pvr%file_head_ctl, new_pvr%file_head_ctl)
      call copy_chara_ctl(org_pvr%file_fmt_ctl, new_pvr%file_fmt_ctl)
      call copy_chara_ctl(org_pvr%monitoring_ctl,                       &
     &                    new_pvr%monitoring_ctl)
!
      call copy_chara_ctl(org_pvr%streo_ctl, new_pvr%streo_ctl)
      call copy_chara_ctl(org_pvr%anaglyph_ctl, new_pvr%anaglyph_ctl)
      call copy_chara_ctl(org_pvr%quilt_ctl, new_pvr%quilt_ctl)
!
      call copy_chara_ctl(org_pvr%pvr_field_ctl, new_pvr%pvr_field_ctl)
      call copy_chara_ctl(org_pvr%pvr_comp_ctl, new_pvr%pvr_comp_ctl)
!
      end subroutine dup_pvr_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_pvr_update_flag(org_pvr, new_pvr)
!
      type(pvr_parameter_ctl), intent(in) :: org_pvr
      type(pvr_parameter_ctl), intent(inout) :: new_pvr
!
!
      call copy_chara_ctl(org_pvr%updated_ctl, new_pvr%updated_ctl)
!
      end subroutine copy_pvr_update_flag
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_4_pvr
