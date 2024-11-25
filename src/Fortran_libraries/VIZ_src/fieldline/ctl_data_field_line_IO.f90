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
!!    field_line_field_ctl      magnetic_field
!!    coloring_field_ctl        magnetic_field   end
!!    coloring_comp_ctl          radial   end
!!    array output_field
!!      output_field    velocity         vector
!!      output_field    magnetic_field   radial
!!    end array output_field
!!
!!    array chosen_ele_grp_ctl
!!      chosen_ele_grp_ctl   outer_core   end
!!    end array chosen_ele_grp_ctl
!!
!!    starting_type:    position_list, surface_list,
!!                      spray_in_domain, or surface_group
!!    line_direction_ctl        forward
!!    max_line_stepping_ctl     1000
!!    max_trace_length_ctl      20.0
!!
!!    communication_mode_ctl     send_recv
!!    starting_type_ctl     position_list
!!
!!    seed_surface_grp_ctl      icb_surf
!!    seed_element_grp_ctl      outer_core
!!    num_fieldline_ctl       10
!!
!!    seed_reference_field_ctl           magnetic_field
!!    seed_reference_component_ctl       radial
!!
!!    selection_type_ctl:    amplitude, area_size
!!
!!    begin seed_lists_ctl
!!      array seed_point_ctl
!!        seed_point_ctl  0.0  0.0  0.0
!!      end array seed_point_ctl
!!
!!      array seed_geological_ctl
!!        seed_geological_ctl  1.03    36.5    140.0
!!      end array seed_geological_ctl
!!
!!      array seed_spherical_ctl
!!        seed_geological_ctl 0.75    -1.047    3.141592
!!      end array seed_spherical_ctl
!!
!!      array starting_gl_surface_id  10
!!        starting_gl_surface_id  12  3
!!      end array
!!    end seed_lists_ctl
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
      use t_control_array_real
      use t_control_array_real3
      use t_fline_seeds_list_ctl
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
     &      :: hd_fline_rst_prefix =   'tracer_restart_prefix'
      character(len=kchara), parameter, private                         &
     &      :: hd_fline_rst_format =   'tracer_restart_format'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_field_line_field =  'field_line_field_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_coloring_field = 'coloring_field_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_coloring_comp =  'coloring_comp_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_fline_result_field = 'output_field'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_fline_grp = 'chosen_ele_grp_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_line_direction = 'line_direction_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_max_line_stepping = 'max_line_stepping_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_max_trace_length = 'max_trace_length_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_starting_type =  'starting_type_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_fline_comm_type =  'communication_mode_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_seed_surf_grp = 'seed_surface_grp_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_seed_ele_grp =  'seed_element_grp_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_seed_ref_field =  'seed_reference_field_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_seed_ref_comp =   'seed_reference_component_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_seed_file_prefix =   'seed_file_prefix_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_num_fieldline = 'num_fieldline_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_selection_type = 'selection_type_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_seed_lists = 'seed_lists_ctl'
!
!   Deprecated labels
      character(len=kchara), parameter, private                         &
     &      :: hd_fline_file_head = 'fline_file_head'
      character(len=kchara), parameter, private                         &
     &      :: hd_fline_output_type = 'fline_output_type'
      character(len=kchara), parameter, private                         &
     &      :: hd_start_surf_grp = 'start_surf_grp_ctl'
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
        call read_fline_seeds_list_ctl(id_control, hd_seed_lists,       &
     &                                 fln%seeds_ctl, c_buf)
!
        call read_control_array_c1(id_control,                          &
     &      hd_fline_grp, fln%fline_area_grp_ctl, c_buf)
!
        call read_control_array_c2(id_control,                          &
     &      hd_fline_result_field, fln%fline_field_output_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_fline_file_prefix,           &
     &                           fln%fline_file_head_ctl)
        call read_chara_ctl_type(c_buf, hd_fline_output_format,         &
     &                           fln%fline_output_type_ctl)
!
        call read_chara_ctl_type(c_buf, hd_fline_rst_prefix,            &
     &                           fln%fline_rst_prefix_ctl)
        call read_chara_ctl_type(c_buf, hd_fline_rst_format,            &
     &                           fln%fline_rst_format_ctl)
!
        call read_chara_ctl_type(c_buf, hd_field_line_field,            &
     &                           fln%fline_field_ctl)
        call read_chara_ctl_type(c_buf, hd_coloring_field,              &
     &                           fln%fline_color_field_ctl)
        call read_chara_ctl_type(c_buf, hd_coloring_comp,               &
     &                           fln%fline_color_comp_ctl)
        call read_chara_ctl_type(c_buf, hd_starting_type,               &
     &                           fln%starting_type_ctl)
        call read_chara_ctl_type(c_buf, hd_fline_comm_type,             &
     &                           fln%fline_comm_mode_ctl)
!
        call read_chara_ctl_type(c_buf, hd_seed_surf_grp,               &
     &                           fln%seed_surf_grp_ctl)
        call read_chara_ctl_type(c_buf, hd_seed_ele_grp,                &
     &                           fln%seed_ele_grp_ctl)
!
        call read_chara_ctl_type(c_buf, hd_seed_ref_field,              &
     &                           fln%seed_ref_field_ctl)
        call read_chara_ctl_type(c_buf, hd_seed_ref_comp,               &
     &                           fln%seed_ref_comp_ctl)
!
        call read_chara_ctl_type(c_buf, hd_seed_file_prefix,            &
     &                           fln%seed_file_prefix_ctl)
!
        call read_chara_ctl_type(c_buf, hd_selection_type,              &
     &      fln%selection_type_ctl )
        call read_chara_ctl_type(c_buf, hd_line_direction,              &
     &      fln%line_direction_ctl )
!
        call read_integer_ctl_type(c_buf, hd_num_fieldline,             &
     &      fln%num_fieldline_ctl )
        call read_integer_ctl_type(c_buf, hd_max_line_stepping,         &
     &      fln%max_line_stepping_ctl)

        call read_real_ctl_type(c_buf, hd_max_trace_length,             &
     &      fln%max_trace_length_ctl)
!
! ---------------Deprecated items
        call read_chara_ctl_type(c_buf, hd_start_surf_grp,              &
     &                           fln%seed_surf_grp_ctl)
        call read_chara_ctl_type(c_buf, hd_fline_file_head,             &
     &      fln%fline_file_head_ctl)
        call read_chara_ctl_type(c_buf, hd_fline_output_type,           &
     &      fln%fline_output_type_ctl)
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
      maxlen = max(maxlen, len_trim(hd_fline_rst_prefix))
      maxlen = max(maxlen, len_trim(hd_fline_rst_format))
      maxlen = max(maxlen, len_trim(hd_field_line_field))
      maxlen = max(maxlen, len_trim(hd_coloring_field))
      maxlen = max(maxlen, len_trim(hd_coloring_comp))
      maxlen = max(maxlen, len_trim(hd_line_direction))
      maxlen = max(maxlen, len_trim(hd_max_line_stepping))
      maxlen = max(maxlen, len_trim(hd_starting_type))
      maxlen = max(maxlen, len_trim(hd_fline_comm_type))
      maxlen = max(maxlen, len_trim(hd_seed_surf_grp))
      maxlen = max(maxlen, len_trim(hd_seed_ele_grp))
      maxlen = max(maxlen, len_trim(hd_seed_ref_field))
      maxlen = max(maxlen, len_trim(hd_seed_ref_comp))
      maxlen = max(maxlen, len_trim(hd_seed_file_prefix))
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
      call write_control_array_c2(id_control, level,                    &
     &    fln%fline_field_output_ctl)
!
      call write_control_array_c1(id_control, level,                    &
     &                            fln%fline_area_grp_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          fln%line_direction_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &                          fln%max_line_stepping_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &                          fln%max_trace_length_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          fln%starting_type_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          fln%fline_comm_mode_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          fln%seed_surf_grp_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          fln%seed_ele_grp_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          fln%seed_file_prefix_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          fln%seed_ref_field_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          fln%seed_ref_comp_ctl)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &                            fln%num_fieldline_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                            fln%selection_type_ctl)
!
      call write_fline_seeds_list_ctl(id_control, fln%seeds_ctl, level)
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
      call init_fline_seeds_list_ctl(hd_seed_lists, fln%seeds_ctl)
!
        call init_chara_ctl_array_label                                 &
     &     (hd_fline_grp, fln%fline_area_grp_ctl)
!
        call init_chara2_ctl_array_label                                &
     &     (hd_fline_result_field, fln%fline_field_output_ctl)
!
        call init_chara_ctl_item_label(hd_fline_file_prefix,            &
     &                                 fln%fline_file_head_ctl)
        call init_chara_ctl_item_label(hd_fline_output_format,          &
     &                                 fln%fline_output_type_ctl)
!
        call init_chara_ctl_item_label(hd_fline_rst_prefix,             &
     &                                 fln%fline_rst_prefix_ctl)
        call init_chara_ctl_item_label(hd_fline_rst_format,             &
     &                                 fln%fline_rst_format_ctl)
!
        call init_chara_ctl_item_label(hd_field_line_field,             &
     &                                 fln%fline_field_ctl)
        call init_chara_ctl_item_label(hd_coloring_field,               &
     &                                 fln%fline_color_field_ctl)
        call init_chara_ctl_item_label(hd_coloring_comp,                &
     &                                 fln%fline_color_comp_ctl)
        call init_chara_ctl_item_label(hd_starting_type,                &
     &                                 fln%starting_type_ctl)
!
        call init_chara_ctl_item_label(hd_fline_comm_type,              &
     &                                 fln%fline_comm_mode_ctl)
!
        call init_chara_ctl_item_label(hd_seed_surf_grp,                &
     &                                fln%seed_surf_grp_ctl)
        call init_chara_ctl_item_label(hd_seed_ele_grp,                 &
     &                                fln%seed_ele_grp_ctl)
!
        call init_chara_ctl_item_label(hd_seed_ref_field,               &
     &                                fln%seed_ref_field_ctl)
        call init_chara_ctl_item_label(hd_seed_ref_comp,                &
     &                                fln%seed_ref_comp_ctl)
!
        call init_chara_ctl_item_label(hd_seed_file_prefix,             &
     &                                fln%seed_file_prefix_ctl)
!
        call init_chara_ctl_item_label(hd_selection_type,               &
     &                                 fln%selection_type_ctl)
        call init_chara_ctl_item_label(hd_line_direction,               &
     &                                 fln%line_direction_ctl)
!
        call init_int_ctl_item_label(hd_num_fieldline,                  &
     &                               fln%num_fieldline_ctl)
        call init_int_ctl_item_label(hd_max_line_stepping,              &
     &                               fln%max_line_stepping_ctl)
        call init_real_ctl_item_label(hd_max_trace_length,              &
     &                                fln%max_trace_length_ctl)
!
! ---------------Deprecated items
        call init_chara_ctl_item_label(hd_fline_file_head,              &
     &      fln%fline_file_head_ctl)
        call init_chara_ctl_item_label(hd_fline_output_type,            &
     &      fln%fline_output_type_ctl)
!
      end subroutine init_field_line_ctl_label
!
!  ---------------------------------------------------------------------
!
      end module ctl_data_field_line_IO
