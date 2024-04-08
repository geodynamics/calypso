!>@file   ctl_data_each_pvr_IO.f90
!!@brief  module ctl_data_each_pvr_IO
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data IO for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine init_pvr_ctl_label(hd_block, pvr_ctl)
!!      subroutine read_pvr_ctl(id_control, hd_block, pvr_ctl, c_buf)
!!      subroutine read_pvr_update_flag                                 &
!!     &         (id_control, hd_block, pvr_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_pvr_ctl                                        &
!!     &         (id_control, hd_block, pvr_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!!        integer(kind = kint), intent(inout) :: level
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of control for Kemo's volume rendering
!!
!!begin volume_rendering   (BMP or PNG)
!!  updated_sign         go
!!  pvr_file_prefix      pvr_temp
!!  pvr_output_format    PNG
!!  monitoring_mode      YES
!!
!!  stereo_imaging       YES
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
!!  begin pvr_color_ctl
!!   ...
!!  end   pvr_color_ctl
!!!
!!  begin lighting_ctl
!!   ...
!!  end lighting_ctl
!!
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
!!  array isosurface_ctl
!!    begin isosurface_ctl
!!      isosurf_value       0.3
!!      opacity_ctl         0.9
!!      surface_direction   normal
!!    end isosurface_ctl
!!     ...
!!  end array isosurface_ctl
!!!
!!  begin quilt_image_ctl
!!   ...
!!  end quilt_image_ctl
!!!
!!  begin snapshot_movie_ctl
!!   ...
!!  end snapshot_movie_ctl
!!end volume_rendering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_data_each_pvr_IO
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
      use t_ctl_data_quilt_image
      use t_ctl_data_pvr_movie
      use t_control_data_pvr_isosurfs
      use t_ctl_data_pvr_area
      use t_control_data_4_pvr
      use skip_comment_f
!
      implicit  none
!
!
!     2nd level for volume_rendering
!
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_updated =     'updated_sign'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_file_prefix = 'pvr_file_prefix'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_out_format =  'pvr_output_format'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_monitor =     'monitoring_mode'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_stereo =    'stereo_imaging'
      character(len=kchara), parameter, private                         &
     &             :: hd_anaglyph_switch = 'anaglyph_switch'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_quilt_3d = 'quilt_3d_imaging'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_output_field_def = 'output_field'
      character(len=kchara), parameter, private                         &
     &             :: hd_output_comp_def =  'output_component'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_view_transform = 'view_transform_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_plot_area =   'plot_area_ctl'
!
      character(len=kchara), parameter, private                         &
     &              :: hd_colormap_file =  'pvr_color_ctl'
      character(len=kchara), parameter, private                         &
     &              :: hd_colormap =      'colormap_ctl'
      character(len=kchara), parameter, private                         &
     &              :: hd_pvr_lighting =  'lighting_ctl'
      character(len=kchara), parameter, private                         &
     &              :: hd_pvr_colorbar =  'colorbar_ctl'
!
!     3rd level for surface_define
!
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_sections = 'section_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_isosurf =  'isosurface_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_quilt_image =  'quilt_image_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_snapshot_movie = 'snapshot_movie_ctl'
!
!       Deprecated label
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_file_head =   'pvr_file_head'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_out_type =    'pvr_output_type'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_rotation =    'image_rotation_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_streo =    'streo_imaging'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_ctl(id_control, hd_block, pvr_ctl, c_buf)
!
      use ctl_file_pvr_modelview_IO
      use ctl_file_pvr_light_IO
      use ctl_data_pvr_movie_IO
      use ctl_data_view_transfer_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(pvr_ctl%i_pvr_ctl .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call sel_read_ctl_modelview_file(id_control, hd_view_transform, &
     &      izero, pvr_ctl%fname_mat_ctl, pvr_ctl%mat, c_buf)
        call sel_read_ctl_pvr_colormap_file                             &
     &     (id_control, hd_colormap_file, pvr_ctl%fname_cmap_cbar_c,    &
     &      pvr_ctl%cmap_cbar_c, c_buf)
        call sel_read_ctl_pvr_light_file(id_control, hd_pvr_lighting,   &
     &      pvr_ctl%fname_pvr_light_c, pvr_ctl%light, c_buf)
!
        call read_pvr_sections_ctl(id_control, hd_pvr_sections,         &
     &                             pvr_ctl%pvr_scts_c, c_buf)
        call read_pvr_isosurfs_ctl(id_control, hd_pvr_isosurf,          &
     &                             pvr_ctl%pvr_isos_c, c_buf)
!
        call read_pvr_render_area_ctl(id_control, hd_plot_area,         &
     &                                pvr_ctl%render_area_c, c_buf)
        call read_quilt_image_ctl(id_control, hd_quilt_image,           &
     &                            pvr_ctl%quilt_c, c_buf)
        call read_pvr_rotation_ctl(id_control, hd_snapshot_movie,       &
     &                              pvr_ctl%movie, c_buf)
        call read_pvr_rotation_ctl(id_control, hd_pvr_rotation,         &
     &                             pvr_ctl%movie, c_buf)
!
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_updated, pvr_ctl%updated_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_file_prefix, pvr_ctl%file_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_file_head, pvr_ctl%file_head_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_out_format, pvr_ctl%file_fmt_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_out_type, pvr_ctl%file_fmt_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_monitor, pvr_ctl%monitoring_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_stereo, pvr_ctl%streo_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_streo, pvr_ctl%streo_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_anaglyph_switch, pvr_ctl%anaglyph_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_quilt_3d, pvr_ctl%quilt_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_output_field_def, pvr_ctl%pvr_field_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_output_comp_def, pvr_ctl%pvr_comp_ctl)
      end do
      pvr_ctl%i_pvr_ctl = 1
!
      end subroutine read_pvr_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_update_flag                                   &
     &         (id_control, hd_block, pvr_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pvr_ctl%i_pvr_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_updated, pvr_ctl%updated_ctl)
      end do
      pvr_ctl%i_pvr_ctl = 1
!
      end subroutine read_pvr_update_flag
!
!  ---------------------------------------------------------------------
!
      subroutine write_pvr_ctl                                          &
     &         (id_control, hd_block, pvr_ctl, level)
!
      use ctl_file_pvr_modelview_IO
      use ctl_file_pvr_light_IO
      use ctl_data_pvr_movie_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(pvr_ctl%i_pvr_ctl .le. 0) return
!
      maxlen = len_trim(hd_pvr_updated)
      maxlen = max(maxlen, len_trim(hd_pvr_file_prefix))
      maxlen = max(maxlen, len_trim(hd_pvr_out_format))
      maxlen = max(maxlen, len_trim(hd_pvr_monitor))
      maxlen = max(maxlen, len_trim(hd_anaglyph_switch))
      maxlen = max(maxlen, len_trim(hd_pvr_stereo))
      maxlen = max(maxlen, len_trim(hd_pvr_quilt_3d))
      maxlen = max(maxlen, len_trim(hd_output_field_def))
      maxlen = max(maxlen, len_trim(hd_output_comp_def))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    pvr_ctl%updated_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    pvr_ctl%file_head_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    pvr_ctl%file_fmt_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    pvr_ctl%monitoring_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    pvr_ctl%streo_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    pvr_ctl%anaglyph_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    pvr_ctl%quilt_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    pvr_ctl%pvr_field_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    pvr_ctl%pvr_comp_ctl)
!
      call sel_write_ctl_modelview_file(id_control, hd_view_transform,  &
     &    pvr_ctl%fname_mat_ctl, pvr_ctl%mat, level)
      call write_pvr_render_area_ctl(id_control, hd_plot_area,          &
     &    pvr_ctl%render_area_c, level)
!
      call sel_write_ctl_pvr_colormap_file                              &
     &   (id_control, hd_colormap_file, pvr_ctl%fname_cmap_cbar_c,      &
     &    pvr_ctl%cmap_cbar_c, level)
      call sel_write_ctl_pvr_light_file                                 &
     &   (id_control, hd_pvr_lighting, pvr_ctl%fname_pvr_light_c,       &
     &    pvr_ctl%light, level)
!
      call write_pvr_sections_ctl(id_control, hd_pvr_sections,          &
     &    pvr_ctl%pvr_scts_c, level)
      call write_pvr_isosurfs_ctl(id_control, hd_pvr_isosurf,           &
     &    pvr_ctl%pvr_isos_c, level)
      call write_quilt_image_ctl(id_control, hd_quilt_image,            &
     &    pvr_ctl%quilt_c, level)
      call write_pvr_rotation_ctl(id_control, hd_snapshot_movie,        &
     &    pvr_ctl%movie, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_pvr_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine init_pvr_ctl_label(hd_block, pvr_ctl)
!
      use ctl_file_pvr_modelview_IO
      use ctl_file_pvr_light_IO
      use ctl_data_pvr_movie_IO
      use ctl_data_view_transfer_IO
!
      character(len=kchara), intent(in) :: hd_block
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
!
!
      pvr_ctl%block_name = hd_block
      call int_pvr_render_area_ctl(hd_plot_area, pvr_ctl%render_area_c)
      call init_pvr_cmap_cbar_label(hd_colormap_file,                   &
     &                              pvr_ctl%cmap_cbar_c)
      call init_view_transfer_ctl_label(hd_view_transform, pvr_ctl%mat)
      call init_lighting_ctl_label(hd_pvr_lighting, pvr_ctl%light)
      call init_pvr_sections_ctl(hd_pvr_sections, pvr_ctl%pvr_scts_c)
      call init_pvr_isosurfs_ctl(hd_pvr_isosurf, pvr_ctl%pvr_isos_c)
      call init_quilt_image_ctl_label(hd_quilt_image, pvr_ctl%quilt_c)
      call init_pvr_rotation_ctl_label(hd_snapshot_movie,               &
     &                                 pvr_ctl%movie)
!
        call init_chara_ctl_item_label                                  &
     &     (hd_pvr_updated, pvr_ctl%updated_ctl)
!
        call init_chara_ctl_item_label                                  &
     &     (hd_pvr_file_prefix, pvr_ctl%file_head_ctl)
        call init_chara_ctl_item_label                                  &
     &     (hd_pvr_file_head, pvr_ctl%file_head_ctl)
!
        call init_chara_ctl_item_label                                  &
     &     (hd_pvr_out_format, pvr_ctl%file_fmt_ctl)
        call init_chara_ctl_item_label                                  &
     &     (hd_pvr_out_type, pvr_ctl%file_fmt_ctl)
!
        call init_chara_ctl_item_label                                  &
     &     (hd_pvr_monitor, pvr_ctl%monitoring_ctl)
!
        call init_chara_ctl_item_label                                  &
     &     (hd_pvr_stereo, pvr_ctl%streo_ctl)
        call init_chara_ctl_item_label                                  &
     &     (hd_anaglyph_switch, pvr_ctl%anaglyph_ctl)
        call init_chara_ctl_item_label                                  &
     &     (hd_pvr_quilt_3d, pvr_ctl%quilt_ctl)
!
        call init_chara_ctl_item_label                                  &
     &     (hd_output_field_def, pvr_ctl%pvr_field_ctl)
        call init_chara_ctl_item_label                                  &
     &     (hd_output_comp_def, pvr_ctl%pvr_comp_ctl)
!
      end subroutine init_pvr_ctl_label
!
!  ---------------------------------------------------------------------
!
      end module ctl_data_each_pvr_IO
