!>@file   ctl_data_field_line_IO.f90
!!@brief  module ctl_data_field_line_IO
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for each field line
!!
!!@verbatim
!!      subroutine init_field_line_ctl_label(hd_block, fln)
!!      subroutine s_read_field_line_ctl(id_control, hd_block,          &
!!     &                                 fln, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(fline_ctl), intent(inout) :: fln
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_field_line_ctl(id_control, hd_block,           &
!!     &                                 fln, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(fline_ctl), intent(in) :: fln
!!        integer(kind = kint), intent(inout) :: level
!!  ---------------------------------------------------------------------
!!     example of control for Kemo's field line
!!
!!  begin fieldline
!!    fline_file_prefix    'fline'
!!    fline_output_format   ucd
!!
!!    field_line_field_ctl      magnetic_field   end
!!    coloring_field_ctl        magnetic_field   end
!!    coloring_comp_ctl          radial   end
!!
!!    array chosen_ele_grp_ctl
!!      chosen_ele_grp_ctl   outer_core   end
!!    end array chosen_ele_grp_ctl
!!
!!  starting_type:    position_list, surface_list,  or surface_group
!!    line_direction_ctl        forward
!!    max_line_stepping_ctl     1000
!!    starting_type_ctl     position_list
!!
!!    start_surf_grp_ctl      icb_surf
!!    num_fieldline_ctl       10
!!
!!    selection_type_ctl:    amplitude, area_size
!!
!!    array starting_point_ctl  10
!!      starting_point_ctl  0.0  0.0  0.0
!!    end array starting_point_ctl
!!
!!    array starting_gl_surface_id  10
!!      starting_gl_surface_id  12  3
!!    end array
!!
!!     field type:
!!     scalar, vector, sym_tensor, asym_tensor
!!       spherical_vector,   spherical_sym_tensor
!!       cylindrical_vector, cylindrical_sym_tensor
!!       norm, 
!!
!!  end fieldline
!!  ---------------------------------------------------------------------
!!@endverbatim
!
      module ctl_data_field_line_IO
!
      use m_precision
!
      use m_machine_parameter
      use t_ctl_data_field_line
      use t_read_control_elements
      use t_control_array_integer
      use t_control_array_character
      use t_control_array_integer2
      use t_control_array_real3
      use calypso_mpi
!
      implicit  none
!
      character(len=kchara), parameter, private                         &
     &      :: hd_fline_file_prefix =   'fline_file_prefix'
      character(len=kchara), parameter, private                         &
     &      :: hd_fline_output_format = 'fline_output_format'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_field_line_field =  'field_line_field_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_coloring_field = 'coloring_field_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_coloring_comp =  'coloring_comp_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_fline_grp = 'chosen_ele_grp_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_line_direction = 'line_direction_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_max_line_stepping = 'max_line_stepping_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_starting_type =  'starting_type_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_start_surf_grp = 'start_surf_grp_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_num_fieldline = 'num_fieldline_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_selection_type = 'selection_type_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_xx_start_point = 'starting_point_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_start_global_surf = 'starting_gl_surface_id'
!
!   Deprecated labels
      character(len=kchara), parameter, private                         &
     &      :: hd_fline_file_head = 'fline_file_head'
      character(len=kchara), parameter, private                         &
     &      :: hd_fline_output_type = 'fline_output_type'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_read_field_line_ctl(id_control, hd_block,            &
     &                                 fln, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(fline_ctl), intent(inout) :: fln
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
!
      if (fln%i_vr_fline_ctl.gt.0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_array_c1(id_control,                          &
     &      hd_fline_grp, fln%fline_area_grp_ctl, c_buf)
!
        call read_control_array_r3(id_control,                          &
     &      hd_xx_start_point, fln%seed_point_ctl, c_buf)
        call read_control_array_i2(id_control,                          &
     &      hd_start_global_surf, fln%seed_surface_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_fline_file_prefix,           &
     &      fln%fline_file_head_ctl)
        call read_chara_ctl_type(c_buf, hd_fline_file_head,             &
     &      fln%fline_file_head_ctl)
!
        call read_chara_ctl_type(c_buf, hd_fline_output_format,         &
     &      fln%fline_output_type_ctl)
        call read_chara_ctl_type(c_buf, hd_fline_output_type,           &
     &      fln%fline_output_type_ctl)
!
        call read_chara_ctl_type(c_buf, hd_field_line_field,            &
     &      fln%fline_field_ctl )
        call read_chara_ctl_type(c_buf, hd_coloring_field,              &
     &      fln%fline_color_field_ctl )
        call read_chara_ctl_type(c_buf, hd_coloring_comp,               &
     &      fln%fline_color_comp_ctl )
        call read_chara_ctl_type(c_buf, hd_starting_type,               &
     &      fln%starting_type_ctl )
        call read_chara_ctl_type(c_buf, hd_start_surf_grp,              &
     &      fln%start_surf_grp_ctl )
        call read_chara_ctl_type(c_buf, hd_selection_type,              &
     &      fln%selection_type_ctl )
        call read_chara_ctl_type(c_buf, hd_line_direction,              &
     &      fln%line_direction_ctl )
!
        call read_integer_ctl_type(c_buf, hd_num_fieldline,             &
     &      fln%num_fieldline_ctl )
        call read_integer_ctl_type(c_buf, hd_max_line_stepping,         &
     &      fln%max_line_stepping_ctl)
      end do
      fln%i_vr_fline_ctl = 1 
!
      end subroutine s_read_field_line_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_field_line_ctl(id_control, hd_block,             &
     &                                 fln, level)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(fline_ctl), intent(in) :: fln
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(fln%i_vr_fline_ctl .le. 0) return
!
      maxlen = len_trim(hd_fline_file_prefix)
      maxlen = max(maxlen, len_trim(hd_fline_output_format))
      maxlen = max(maxlen, len_trim(hd_field_line_field))
      maxlen = max(maxlen, len_trim(hd_coloring_field))
      maxlen = max(maxlen, len_trim(hd_coloring_comp))
      maxlen = max(maxlen, len_trim(hd_line_direction))
      maxlen = max(maxlen, len_trim(hd_max_line_stepping))
      maxlen = max(maxlen, len_trim(hd_starting_type))
      maxlen = max(maxlen, len_trim(hd_start_surf_grp))
      maxlen = max(maxlen, len_trim(hd_num_fieldline))
      maxlen = max(maxlen, len_trim(hd_selection_type))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    fln%fline_file_head_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    fln%fline_output_type_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    fln%fline_field_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    fln%fline_color_field_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    fln%fline_color_comp_ctl)
!
      call write_control_array_c1(id_control, level,                    &
     &    fln%fline_area_grp_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    fln%line_direction_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    fln%max_line_stepping_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    fln%starting_type_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    fln%start_surf_grp_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    fln%num_fieldline_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    fln%selection_type_ctl)
!
      call write_control_array_r3(id_control, level,                    &
     &    fln%seed_point_ctl)
      call write_control_array_i2 (id_control, level,                   &
     &    fln%seed_surface_ctl)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_field_line_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine init_field_line_ctl_label(hd_block, fln)
!
      character(len=kchara), intent(in) :: hd_block
      type(fline_ctl), intent(inout) :: fln
!
!
      fln%block_name = hd_block
!
        call init_chara_ctl_array_label                                 &
     &     (hd_fline_grp, fln%fline_area_grp_ctl)
!
        call init_r3_ctl_array_label                                    &
     &     (hd_xx_start_point, fln%seed_point_ctl)
        call init_int2_ctl_array_label                                  &
     &     (hd_start_global_surf, fln%seed_surface_ctl)
!
        call init_chara_ctl_item_label(hd_fline_file_prefix,            &
     &      fln%fline_file_head_ctl)
        call init_chara_ctl_item_label(hd_fline_file_head,              &
     &      fln%fline_file_head_ctl)
!
        call init_chara_ctl_item_label(hd_fline_output_format,          &
     &      fln%fline_output_type_ctl)
        call init_chara_ctl_item_label(hd_fline_output_type,            &
     &      fln%fline_output_type_ctl)
!
        call init_chara_ctl_item_label(hd_field_line_field,             &
     &      fln%fline_field_ctl )
        call init_chara_ctl_item_label(hd_coloring_field,               &
     &      fln%fline_color_field_ctl )
        call init_chara_ctl_item_label(hd_coloring_comp,                &
     &      fln%fline_color_comp_ctl )
        call init_chara_ctl_item_label(hd_starting_type,                &
     &      fln%starting_type_ctl )
        call init_chara_ctl_item_label(hd_start_surf_grp,               &
     &      fln%start_surf_grp_ctl )
        call init_chara_ctl_item_label(hd_selection_type,               &
     &      fln%selection_type_ctl )
        call init_chara_ctl_item_label(hd_line_direction,               &
     &      fln%line_direction_ctl )
!
        call init_int_ctl_item_label(hd_num_fieldline,                  &
     &      fln%num_fieldline_ctl )
        call init_int_ctl_item_label(hd_max_line_stepping,              &
     &      fln%max_line_stepping_ctl)
!
      end subroutine init_field_line_ctl_label
!
!  ---------------------------------------------------------------------
!
      end module ctl_data_field_line_IO
