!>@file   t_control_data_4_map.f90
!!@brief  module t_control_data_4_map
!!
!!@author H. Matsui
!!@date Programmed in May. 2006
!
!>@brief  control ID data for surfacing module
!!
!!@verbatim
!!      subroutine init_map_ctl_stract(map_c)
!!      subroutine dealloc_cont_dat_4_map(map_c)
!!        type(map_ctl), intent(inout) :: map_c
!!      subroutine dup_control_4_map(org_map_c, new_map_c)
!!        type(map_ctl), intent(in) :: org_map_c
!!        type(map_ctl), intent(inout) :: new_map_c
!!
!!      subroutine add_fields_4_map_to_fld_ctl(map_c, field_ctl)
!!        type(map_ctl), intent(in) :: map_c
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! example of control for Kemo's surface rendering
!!
!!  begin cross_section_ctl
!!    map_image_prefix       'map'
!!    map_image_format        PNG
!!
!!    output_field       magnetic_field
!!    output_component   r
!!
!!    isoline_field       magnetic_field
!!    isoline_component   r
!!
!!    begin section_ctl
!!      file surface_define     ctl_psf_eq
!!      begin surface_define
!!        section_method    equation
!!  
!!        array coefs_ctl  10
!!          coefs_ctl  x2     1.0
!!          coefs_ctl  y2     1.0
!!          coefs_ctl  z2     0.0
!!          coefs_ctl  xy     0.0
!!          coefs_ctl  yz     0.0
!!          coefs_ctl  zx     0.0
!!          coefs_ctl  x      0.0
!!          coefs_ctl  y      0.0
!!          coefs_ctl  z      0.0
!!          coefs_ctl  const  1.0
!!        end array coefs_ctl
!!  
!!        array section_area_ctl 1
!!          section_area_ctl   outer_core   end
!!        end array section_area_ctl
!!      end surface_define
!!
!!      zeroline_switch_ctl           On
!!      isoline_color_mode      color, white, or black
!!      isoline_number_ctl            20
!!      isoline_range_ctl          -0.5   0.5
!!      isoline_width_ctl             1.5
!!      grid_width_ctl                1.0
!!
!!      tangent_cylinder_switch_ctl   On
!!      inner_radius_ctl              0.53846
!!      outer_radius_ctl              1.53846
!!    end section_ctl
!!
!!    begin map_projection_ctl
!!      begin image_size_ctl
!!        x_pixel_ctl  640
!!        y_pixel_ctl  480
!!      end image_size_ctl
!!
!!      projection_type_ctl      Aitoff, xy_plane, xz_plane, yz_plane
!!      begin projection_matrix_ctl
!!        perspective_xy_ratio_ctl   1.0
!!        horizontal_range_ctl       -2.4   2.4
!!        vertical_range_ctl         -1.2   1.2
!!      end projection_matrix_ctl
!!    end map_projection_ctl
!!
!!    begin colormap_ctl
!!      colormap_mode_ctl       rainbow
!!      background_color_ctl    0.0   0.0   0.0
!!
!!      data_mapping_ctl   Colormap_list
!!      array color_table_ctl
!!        color_table_ctl    0.0   0.0
!!        color_table_ctl    0.5   0.5
!!        color_table_ctl    1.0   1.0
!!      end array color_table_ctl
!!    end   colormap_ctl
!!
!!    begin colorbar_ctl
!!      colorbar_switch_ctl    ON
!!      colorbar_position_ctl  'left' or 'bottom'
!!      colorbar_scale_ctl     ON
!!      iflag_zeromarker       ON
!!      colorbar_range     0.0   1.0
!!      font_size_ctl         3
!!      num_grid_ctl          4
!!!
!!      axis_label_switch      ON
!!    end colorbar_ctl
!!  end  cross_section_ctl
!!  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  
!!      map_image_format:
!!           ucd, VTK
!!
!!    num_result_comp: number of fields
!!    output_field: (Original name: color_comp and color_subcomp)
!!         field and componenet name for output
!!           x, y, z, radial, elevation, azimuth, cylinder_r, norm
!!           vector, sym_tensor, asym_tensor
!!           spherical_vector, cylindrical_vector
!!    output_value: (Original name: specified_color)
!!
!!    section_method: (original: method)
!!           sphere, ellipsoid, equation, group
!!    center_position: position of center (for sphere and plane)
!!        array center_position    3
!!          center_position  x   0.0
!!          center_position  y   0.0
!!          center_position  z   0.0
!!        end array center_position
!!    radius:  radius of sphere
!!    axial_length: length of axis
!!          (for ellipsoid, hyperboloid, paraboloid)
!!        array axial_length   3
!!          axial_length  x   1.0
!!          axial_length  y   0.5
!!          axial_length  z   0.0
!!        end array axial_length
!!    coefficients:  coefficients for equation
!!        array coefs_ctl  10
!!          coefs_ctl  x2     1.0
!!          coefs_ctl  y2     0.5
!!          coefs_ctl  z2     0.0
!!          coefs_ctl  xy     1.0
!!          coefs_ctl  yz     0.5
!!          coefs_ctl  zx     0.0
!!          coefs_ctl  x      1.0
!!          coefs_ctl  y      0.5
!!          coefs_ctl  z      0.0
!!          coefs_ctl  const  1.0
!!        end array coefs_ctl
!!    group_type:  (Original: defined_style)
!!           node_group or surface_group
!!    group_name:  name of group to plot
!!
!!   field type:
!!     scalar, vector,     sym_tensor, asym_tensor
!!     spherical_vector,   spherical_sym_tensor
!!     cylindrical_vector, cylindrical_sym_tensor
!!     norm
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_4_map
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use skip_comment_f
      use t_read_control_elements
      use t_control_array_character
      use t_ctl_data_pvr_section
      use t_ctl_data_4_view_transfer
      use t_ctl_data_pvr_colormap_bar
!
      implicit  none
!
!
      type map_ctl
!>        Structure of cross section definition
        type(pvr_section_ctl) :: map_define_ctl
!
!>        Structure for file prefix
        type(read_character_item) :: map_image_prefix_ctl
!>        Structure for data field format
        type(read_character_item) :: map_image_fmt_ctl
!
!>        Structure of field name for rendering
        type(read_character_item) :: map_field_ctl
!>        Structure of component name for rendering
        type(read_character_item) :: map_comp_ctl
!
!>        Structure of isoline field name for rendering
        type(read_character_item) :: isoline_field_ctl
!>        Structure of isoline component name for rendering
        type(read_character_item) :: isoline_comp_ctl
!
!>     file name for modelview matrix
        character(len=kchara) :: fname_mat_ctl = 'NO_FILE'
!>     Structure for modelview marices
        type(modeview_ctl) :: mat
!
!>     file name for colormap and colorbar
        character(len=kchara) :: fname_cmap_cbar_c = 'NO_FILE'
!>     Structure for colormap and colorbar
        type(pvr_colormap_bar_ctl) :: cmap_cbar_c
!
!     Top level
        integer (kind=kint) :: i_map_ctl = 0
!     2nd level for cross_section_ctl
        integer (kind=kint) :: i_output_field =   0
      end type map_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_map_ctl_stract(map_c)
!
      type(map_ctl), intent(inout) :: map_c
!
      call init_psf_def_ctl_stract(map_c%map_define_ctl%psf_def_c)
!
      end subroutine init_map_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_cont_dat_4_map(map_c)
!
      type(map_ctl), intent(inout) :: map_c
!
!
      call dealloc_pvr_section_ctl(map_c%map_define_ctl)
      call dealloc_view_transfer_ctl(map_c%mat)
      call deallocate_pvr_cmap_cbar(map_c%cmap_cbar_c)
!
      map_c%map_image_prefix_ctl%iflag = 0
      map_c%map_image_fmt_ctl%iflag =    0
      map_c%map_field_ctl%iflag =        0
      map_c%map_comp_ctl%iflag =         0
      map_c%isoline_field_ctl%iflag =    0
      map_c%isoline_comp_ctl%iflag =     0
!
      map_c%fname_mat_ctl = 'NO_FILE'
      map_c%fname_cmap_cbar_c = 'NO_FILE'
!
      map_c%i_map_ctl =        0
      map_c%i_output_field =   0
!
      end subroutine dealloc_cont_dat_4_map
!
!  ---------------------------------------------------------------------
!
      subroutine dup_control_4_map(org_map_c, new_map_c)
!
      type(map_ctl), intent(in) :: org_map_c
      type(map_ctl), intent(inout) :: new_map_c
!
!
      call dup_pvr_section_ctl(org_map_c%map_define_ctl,                &
     &                         new_map_c%map_define_ctl)
      call dup_view_transfer_ctl(org_map_c%mat, new_map_c%mat)
      call dup_pvr_cmap_cbar(org_map_c%cmap_cbar_c,                     &
     &                       new_map_c%cmap_cbar_c)
!
      call copy_chara_ctl(org_map_c%map_image_prefix_ctl,               &
     &                    new_map_c%map_image_prefix_ctl)
      call copy_chara_ctl(org_map_c%map_image_fmt_ctl,                  &
     &                    new_map_c%map_image_fmt_ctl)
      call copy_chara_ctl(org_map_c%map_field_ctl,                      &
     &                    new_map_c%map_field_ctl)
      call copy_chara_ctl(org_map_c%map_comp_ctl,                       &
     &                    new_map_c%map_comp_ctl)
      call copy_chara_ctl(org_map_c%isoline_field_ctl,                  &
     &                    new_map_c%isoline_field_ctl)
      call copy_chara_ctl(org_map_c%isoline_comp_ctl,                   &
     &                    new_map_c%isoline_comp_ctl)
!
      new_map_c%fname_mat_ctl =     org_map_c%fname_mat_ctl
      new_map_c%fname_cmap_cbar_c = org_map_c%fname_cmap_cbar_c
!
      new_map_c%i_map_ctl =        org_map_c%i_map_ctl
      new_map_c%i_output_field =   org_map_c%i_output_field
!
      end subroutine dup_control_4_map
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine add_fields_4_map_to_fld_ctl(map_c, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      type(map_ctl), intent(in) :: map_c
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(map_c%map_field_ctl%iflag .gt. 0) then
        call add_viz_name_ctl(map_c%map_field_ctl%charavalue,           &
     &                        field_ctl)
      end if
      if(map_c%isoline_field_ctl%iflag .gt. 0) then
        call add_viz_name_ctl(map_c%isoline_field_ctl%charavalue,       &
     &                        field_ctl)
      end if
!
      end subroutine add_fields_4_map_to_fld_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_4_map
