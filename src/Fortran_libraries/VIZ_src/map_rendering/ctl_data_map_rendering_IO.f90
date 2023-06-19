!>@file   ctl_data_map_rendering_IO.f90
!!@brief  module ctl_data_map_rendering_IO
!!
!!@author H. Matsui
!!@date Programmed in May. 2006
!
!>@brief  control ID data for surfacing module
!!
!!@verbatim
!!      subroutine s_read_map_control_data                              &
!!     &         (id_control, hd_block, map_c, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(map_ctl), intent(inout) :: map_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_map_control_data                               &
!!     &         (id_control, hd_block, map_c, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(map_ctl), intent(inout) :: map_c
!!        integer(kind = kint), intent(inout) :: level
!!
!!      integer(kind = kint) function num_label_map_ctl()
!!      integer(kind = kint) function num_label_map_ctl_w_dpl()
!!      subroutine set_label_map_ctl(names)
!!      subroutine set_label_map_ctl_w_dpl(names)
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
!!        ...
!!      end surface_define
!!
!!      zeroline_switch_ctl           On
!!      isoline_color_mode      color, white, or black
!!      isoline_number_ctl            20
!!      isoline_range_ctl          -0.5   0.5
!!    isoline_width_ctl             1.5
!!    grid_width_ctl                1.0
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
!!      begin projection_matrix_ctl
!!        perspective_xy_ratio_ctl   1.0
!!        horizontal_range_ctl       -2.4   2.4
!!        vertical_range_ctl         -1.2   1.2
!!      end projection_matrix_ctl
!!    end map_projection_ctl
!!
!!    file map_color_ctl    'ctl_color_Br'
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
!!      num_grid_ctl     4
!!!
!!      axis_label_switch      ON
!!    end colorbar_ctl
!!  end  cross_section_ctl
!!  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  
!!      map_image_format:
!!           BMP, png
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
!!           plane, sphere, ellipsoid, hyperboloid, paraboloid
!!           equation, group
!!    normal_vector: normal vector (for plane)
!!        array normal_vector    3
!!          normal_vector  x   0.0
!!          normal_vector  y   0.0
!!          normal_vector  z   1.0
!!        end array normal_vector
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
      module ctl_data_map_rendering_IO
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use skip_comment_f
      use t_read_control_elements
      use t_control_array_real
      use t_control_array_character
      use t_control_array_charareal
      use t_control_data_4_map
      use t_ctl_data_pvr_section
      use calypso_mpi
!
      implicit  none
!
!     2nd level for cross_section_ctl
      character(len=kchara), parameter, private                         &
     &                  :: hd_map_image_prefix = 'map_image_prefix'
      character(len=kchara), parameter, private                         &
     &                  :: hd_map_image_format = 'map_image_format'
      character(len=kchara), parameter, private                         &
     &                  :: hd_section_ctl =      'section_ctl'
!
      character(len=kchara), parameter, private                         &
     &                  :: hd_map_output_field = 'output_field'
      character(len=kchara), parameter, private                         &
     &                  :: hd_map_output_comp =  'output_component'
      character(len=kchara), parameter, private                         &
     &                  :: hd_map_isoline_field = 'isoline_field'
      character(len=kchara), parameter, private                         &
     &                  :: hd_map_isoline_comp =  'isoline_component'
!
      character(len=kchara), parameter, private                         &
     &                  :: hd_map_projection = 'map_projection_ctl'
      character(len=kchara), parameter, private                         &
     &                  :: hd_map_colormap_file =  'map_color_ctl'
!
      integer(kind = kint), parameter :: n_label_map_ctl = 9
      private :: n_label_map_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_read_map_control_data                                &
     &         (id_control, hd_block, map_c, c_buf)
!
      use t_ctl_data_pvr_colormap_bar
      use ctl_file_pvr_modelview_IO
      use ctl_data_pvr_section_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(map_ctl), intent(inout) :: map_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(map_c%i_map_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call sel_read_ctl_modelview_file(id_control, hd_map_projection, &
     &      map_c%fname_mat_ctl, map_c%mat, c_buf)
        call sel_read_ctl_pvr_colormap_file                             &
     &     (id_control, hd_map_colormap_file, map_c%fname_cmap_cbar_c,  &
     &      map_c%cmap_cbar_c, c_buf)
!
        call read_pvr_section_ctl(id_control, hd_section_ctl,           &
     &                            map_c%map_define_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_map_image_prefix,            &
     &      map_c%map_image_prefix_ctl)
        call read_chara_ctl_type(c_buf, hd_map_image_format,            &
     &      map_c%map_image_fmt_ctl)
!
        call read_chara_ctl_type(c_buf, hd_map_output_field,            &
     &      map_c%map_field_ctl)
        call read_chara_ctl_type(c_buf, hd_map_output_comp,             &
     &      map_c%map_comp_ctl)
!
        call read_chara_ctl_type(c_buf, hd_map_isoline_field,           &
     &      map_c%isoline_field_ctl)
        call read_chara_ctl_type(c_buf, hd_map_isoline_comp,            &
     &      map_c%isoline_comp_ctl)
      end do
      map_c%i_map_ctl = 1
!
      end subroutine s_read_map_control_data
!
!   --------------------------------------------------------------------
!
      subroutine write_map_control_data                                 &
     &         (id_control, hd_block, map_c, level)
!
      use t_ctl_data_pvr_colormap_bar
      use ctl_file_pvr_modelview_IO
      use ctl_data_pvr_section_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(map_ctl), intent(in) :: map_c
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(map_c%i_map_ctl .le. 0) return
!
      maxlen = len_trim(hd_map_image_prefix)
      maxlen = max(maxlen, len_trim(hd_map_image_format))
      maxlen = max(maxlen, len_trim(hd_map_output_field))
      maxlen = max(maxlen, len_trim(hd_map_output_comp))
      maxlen = max(maxlen, len_trim(hd_map_isoline_field))
      maxlen = max(maxlen, len_trim(hd_map_isoline_comp))
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_map_image_prefix, map_c%map_image_prefix_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_map_image_format, map_c%map_image_fmt_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_map_output_field, map_c%map_field_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_map_output_comp, map_c%map_comp_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_map_isoline_field, map_c%isoline_field_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_map_isoline_comp, map_c%isoline_comp_ctl)
!
      call write_pvr_section_ctl(id_control, hd_section_ctl,            &
     &                           map_c%map_define_ctl, level)
!
      call sel_write_ctl_modelview_file(id_control, hd_map_projection,  &
     &    map_c%fname_mat_ctl, map_c%mat, level)
      call sel_write_ctl_pvr_colormap_file                              &
     &   (id_control, hd_map_colormap_file, map_c%fname_cmap_cbar_c,    &
     &    map_c%cmap_cbar_c, level)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_map_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      integer(kind = kint) function num_label_map_ctl()
      num_label_map_ctl = n_label_map_ctl
      return
      end function num_label_map_ctl
!
! ----------------------------------------------------------------------
!
      subroutine set_label_map_ctl(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_map_ctl)
!
!
      call set_control_labels(hd_map_image_prefix, names( 1))
      call set_control_labels(hd_map_image_format, names( 2))
      call set_control_labels(hd_section_ctl,      names( 3))
!
      call set_control_labels(hd_map_output_field,   names( 4))
      call set_control_labels(hd_map_output_comp,    names( 5))
      call set_control_labels(hd_map_isoline_field,  names( 6))
      call set_control_labels(hd_map_isoline_comp,   names( 7))
!
      call set_control_labels(hd_map_projection,    names( 8))
      call set_control_labels(hd_map_colormap_file, names( 9))
!
      end subroutine set_label_map_ctl
!
!  ---------------------------------------------------------------------
!
      end module ctl_data_map_rendering_IO
