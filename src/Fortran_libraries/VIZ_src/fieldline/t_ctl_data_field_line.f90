!>@file   t_ctl_data_field_line.f90
!!@brief  module t_ctl_data_field_line
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for each field line
!!
!!@verbatim
!!      subroutine deallocate_cont_dat_fline(fln)
!!      subroutine reset_fline_control_flags(fln)
!!        type(fline_ctl), intent(inout) :: fln
!!
!!      subroutine dup_control_4_fline(org_fln, new_fln)
!!        type(fline_ctl), intent(in) :: org_fln
!!        type(fline_ctl), intent(inout) :: new_fln
!!
!!      subroutine add_field_4_fline_to_fld_ctl                         &
!!     &         (fline_ctl_struct, field_ctl)
!!        type(fline_ctl), intent(in) :: fline_ctl_struct
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!  ---------------------------------------------------------------------
!!     example of control for Kemo's field line
!!
!!  begin fieldline
!!    fline_file_prefix    'fline'
!!    fline_output_format   ucd
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
!!    selection_type_ctl     amplitude
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
!!    field_line_field_ctl      magnetic_field   end
!!    coloring_field_ctl        magnetic_field   end
!!    coloring_comp_ctl        radial   end
!!
!!  end fieldline
!!  ---------------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_field_line
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_integer
      use t_control_array_character
      use t_control_array_integer2
      use t_control_array_real3
      use skip_comment_f
!
      implicit  none
!
!
      type fline_ctl
!>        Control block name
        character(len = kchara) :: block_name = 'fieldline'
!
        type(read_character_item) :: fline_file_head_ctl
        type(read_character_item) :: fline_output_type_ctl
!
        type(read_character_item) :: fline_field_ctl
        type(read_character_item) :: fline_color_field_ctl
        type(read_character_item) :: fline_color_comp_ctl
!
!>      Structure for element group to draw field line
!!@n      fline_area_grp_ctl%c_tbl:  element group to draw field line
        type(ctl_array_chara) :: fline_area_grp_ctl
!
!
        type(read_character_item) :: starting_type_ctl
        type(read_character_item) :: selection_type_ctl
        type(read_character_item) :: line_direction_ctl
!
        type(read_character_item) :: start_surf_grp_ctl
!
        type(read_integer_item) :: num_fieldline_ctl
        type(read_integer_item) :: max_line_stepping_ctl
!
!>      Structure for seed points
!!@n      seed_point_ctl%vec1:  X-component of seed points
!!@n      seed_point_ctl%vec2:  Y-component of seed points
!!@n      seed_point_ctl%vec3:  Z-component of seed points
        type(ctl_array_r3) :: seed_point_ctl
!
!>      Structure for seed points on center of the surfaces
!!@n      seed_surface_ctl%int1:  element ID for seed points
!!@n      seed_surface_ctl%int2:  Surface ID for seed points
        type(ctl_array_i2) :: seed_surface_ctl
!
        integer (kind=kint) :: i_vr_fline_ctl = 0
      end type fline_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_cont_dat_fline(fln)
!
      type(fline_ctl), intent(inout) :: fln
!
!
      call dealloc_control_array_i2(fln%seed_surface_ctl)
      call dealloc_control_array_r3(fln%seed_point_ctl)
      call dealloc_control_array_chara(fln%fline_area_grp_ctl)
!
      call reset_fline_control_flags(fln)
!
      end subroutine deallocate_cont_dat_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine reset_fline_control_flags(fln)
!
      type(fline_ctl), intent(inout) :: fln
!
!
      fln%fline_area_grp_ctl%num = 0
      fln%seed_point_ctl%num =     0
      fln%seed_surface_ctl%num =   0
!
      fln%fline_file_head_ctl%iflag = 0
      fln%fline_output_type_ctl%iflag = 0
!
      fln%num_fieldline_ctl%iflag  =     0
      fln%max_line_stepping_ctl%iflag  = 0
      fln%starting_type_ctl%iflag =  0
      fln%selection_type_ctl%iflag = 0
      fln%start_surf_grp_ctl%iflag = 0
!
      fln%i_vr_fline_ctl = 0
!
      fln%fline_area_grp_ctl%icou = 0
      fln%seed_point_ctl%icou =     0
      fln%seed_surface_ctl%icou =   0
!
      fln%fline_color_field_ctl%iflag =   0
      fln%fline_color_comp_ctl%iflag =    0
      fln%fline_field_ctl%iflag = 0
      fln%line_direction_ctl%iflag = 0
!
      end subroutine reset_fline_control_flags
!
!  ---------------------------------------------------------------------
!
      subroutine dup_control_4_fline(org_fln, new_fln)
!
      type(fline_ctl), intent(in) :: org_fln
      type(fline_ctl), intent(inout) :: new_fln
!
!
      call copy_chara_ctl(org_fln%fline_file_head_ctl,                  &
     &                    new_fln%fline_file_head_ctl)
      call copy_chara_ctl(org_fln%fline_output_type_ctl,                &
     &                    new_fln%fline_output_type_ctl)
!
      call copy_chara_ctl(org_fln%fline_field_ctl,                      &
     &                    new_fln%fline_field_ctl)
      call copy_chara_ctl(org_fln%fline_color_field_ctl,                &
     &                    new_fln%fline_color_field_ctl)
      call copy_chara_ctl(org_fln%fline_color_comp_ctl,                 &
     &                    new_fln%fline_color_comp_ctl)
!
      call copy_chara_ctl(org_fln%starting_type_ctl,                    &
     &                    new_fln%starting_type_ctl)
      call copy_chara_ctl(org_fln%selection_type_ctl,                   &
     &                    new_fln%selection_type_ctl)
      call copy_chara_ctl(org_fln%line_direction_ctl,                   &
     &                    new_fln%line_direction_ctl)
!
      call copy_chara_ctl(org_fln%start_surf_grp_ctl,                   &
     &                    new_fln%start_surf_grp_ctl)
!
      call copy_integer_ctl(org_fln%num_fieldline_ctl,                  &
     &                      new_fln%num_fieldline_ctl)
      call copy_integer_ctl(org_fln%max_line_stepping_ctl,              &
     &                      new_fln%max_line_stepping_ctl)
!
      call dup_control_array_c1(org_fln%fline_area_grp_ctl,             &
     &                          new_fln%fline_area_grp_ctl)
      call dup_control_array_r3(org_fln%seed_point_ctl,                 &
     &                          new_fln%seed_point_ctl)
      call dup_control_array_i2(org_fln%seed_surface_ctl,               &
     &                          new_fln%seed_surface_ctl)
!
      new_fln%block_name =      org_fln%block_name
      new_fln%i_vr_fline_ctl =  org_fln%i_vr_fline_ctl
!
      end subroutine dup_control_4_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_field_4_fline_to_fld_ctl                           &
     &         (fline_ctl_struct, field_ctl)
!
      use t_control_array_character3
!
      use add_nodal_fields_ctl
!
      type(fline_ctl), intent(in) :: fline_ctl_struct
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(fline_ctl_struct%fline_field_ctl%iflag .gt. 0) then
        call add_viz_name_ctl                                           &
     &     (fline_ctl_struct%fline_field_ctl%charavalue, field_ctl)
      end if
!
      if(fline_ctl_struct%fline_color_field_ctl%iflag .gt. 0) then
        call add_viz_name_ctl                                           &
     &     (fline_ctl_struct%fline_color_field_ctl%charavalue,          &
     &      field_ctl)
      end if
!
      end subroutine add_field_4_fline_to_fld_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_field_line
