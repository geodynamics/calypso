!
!      module m_control_data_4_psf
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine deallocate_cont_dat_4_psf(psf)
!        type(psf_ctl), intent(inout) :: psf
!      subroutine deallocate_psf_coefs_ctl(psf)
!        type(psf_ctl), intent(inout) :: psf
!      subroutine deallocate_psf_center_ctl(psf)
!        type(psf_ctl), intent(inout) :: psf
!      subroutine deallocate_psf_axis_ctl(psf)
!        type(psf_ctl), intent(inout) :: psf
!
!      subroutine read_control_data_4_psf(psf)
!        type(psf_ctl), intent(inout) :: psf
!      subroutine read_psf_control_data(psf)
!        type(psf_ctl), intent(inout) :: psf
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! example of control for Kemo's surface rendering
!
!  begin cross_sectioning
!    psf_file_head    'psf'
!    psf_output_type   ucd
!  
!    begin surface_define
!      section_method    equation
!  
!      array coefs_ctl  10
!        coefs_ctl  x2     1.0
!        coefs_ctl  y2     1.0
!        coefs_ctl  z2     0.0
!        coefs_ctl  xy     0.0
!        coefs_ctl  yz     0.0
!        coefs_ctl  zx     0.0
!        coefs_ctl  x      0.0
!        coefs_ctl  y      0.0
!        coefs_ctl  z      0.0
!        coefs_ctl  const  1.0
!      end array coefs_ctl
!  
!      begin plot_area_ctl
!        array chosen_ele_grp_ctl 1
!          chosen_ele_grp_ctl   outer_core   end
!        end array chosen_ele_grp_ctl
!      end plot_area_ctl
!    end surface_define
!  
!    begin output_field_define
!      array  output_field   2
!        output_field    velocity         vector   end
!        output_field    magnetic_field   radial   end
!      end  array output_field
!    end output_field_define
!  end  cross_sectioning
!  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  
!      psf_output_type:
!           ucd, OpenDX
!
!    num_result_comp: number of fields
!    output_field: (Original name: color_comp and color_subcomp)
!         field and componenet name for output
!           x, y, z, radial, elevation, azimuth, cylinder_r, norm
!           vector, sym_tensor, asym_tensor
!           spherical_vector, cylindrical_vector
!    output_value: (Original name: specified_color)
!
!    section_method: (original: method)
!           sphere, ellipsoid, hyperboloid, paraboloid, equation
!           group
!    center_position: position of center (for sphere)
!        array center_position    3
!          center_position  x   0.0
!          center_position  y   0.0
!          center_position  z   0.0
!        end array center_position
!    radius:  radius of sphere
!    axial_length: length of axis
!          (for ellipsoid, hyperboloid, paraboloid)
!        array axial_length   3
!          axial_length  x   1.0
!          axial_length  y   0.5
!          axial_length  z   0.0
!        end array axial_length
!    coefficients:  coefficients for equation
!        array coefs_ctl  10
!          coefs_ctl  x2     1.0
!          coefs_ctl  y2     0.5
!          coefs_ctl  z2     0.0
!          coefs_ctl  xy     1.0
!          coefs_ctl  yz     0.5
!          coefs_ctl  zx     0.0
!          coefs_ctl  x      1.0
!          coefs_ctl  y      0.5
!          coefs_ctl  z      0.0
!          coefs_ctl  const  1.0
!        end array coefs_ctl
!    group_type:  (Original: defined_style)
!           node_group or surface_group
!    group_name:  name of group to plot
!
!   field type:
!     scalar, vector,     sym_tensor, asym_tensor
!     spherical_vector,   spherical_sym_tensor
!     cylindrical_vector, cylindrical_sym_tensor
!     norm
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module m_control_data_4_psf
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
      use t_read_control_arrays
!
      implicit  none
!
!
      type psf_ctl
        character(len=kchara) :: psf_file_head_ctl
        character(len=kchara) :: psf_output_type_ctl
!
        character(len=kchara) :: section_method_ctl
!
!
!>      Structure for coefficients for sueface equation
!!@n      psf_coefs_ctl%c_tbl: 
!!@n      psf_coefs_ctl%vect:  coefficients
        type(ctl_array_cr) :: psf_coefs_ctl
!
!>      Structure for definition of center
!!@n      psf_axis_ctl%c_tbl: direction of axis
!!@n      psf_axis_ctl%vect:  position
        type(ctl_array_cr) :: psf_center_ctl
!
!>      Structure for definition of elipsoid
!!@n      psf_axis_ctl%c_tbl: direction of axis
!!@n      psf_axis_ctl%vect:  vector component
        type(ctl_array_cr) :: psf_axis_ctl
!
        real(kind = kreal) :: radius_psf_ctl
!
        character(len=kchara) :: psf_group_name_ctl
!
!>      Structure for list of output field
!!@n      psf_out_field_ctl%c1_tbl: Name of field
!!@n      psf_out_field_ctl%c2_tbl: Name of component
        type(ctl_array_c2) :: psf_out_field_ctl
!
!>      Structure for element group list for Parallel Sectioning
!!@n      psf_area_ctl%c_tbl: Name of element group
        type(ctl_array_chara) :: psf_area_ctl
!
!     Top level
        integer (kind=kint) :: i_psf_ctl = 0
!     2nd level for surface_rendering
        integer (kind=kint) :: i_psf_file_head =  0
        integer (kind=kint) :: i_psf_out_type =   0
        integer (kind=kint) :: i_surface_define = 0
        integer (kind=kint) :: i_output_field =   0
!     3rd level for surface_define
        integer (kind=kint) :: i_section_method =  0
        integer (kind=kint) :: i_radius =          0
        integer (kind=kint) :: i_plot_area =       0
        integer (kind=kint) :: i_group_name =      0
      end type psf_ctl
!
!
!     Top level
      character(len=kchara) :: hd_psf_ctl = 'surface_rendering'
      character(len=kchara) :: hd_section_ctl = 'cross_sectioning'
!
!     2nd level for surface_rendering
      character(len=kchara) :: hd_psf_file_head = 'psf_file_head'
      character(len=kchara) :: hd_psf_out_type =  'psf_output_type'
      character(len=kchara) :: hd_surface_define = 'surface_define'
      character(len=kchara) :: hd_output_field = 'output_field_define'
!
!     3rd level for surface_define
      character(len=kchara) :: hd_section_method =  'section_method'
      character(len=kchara) :: hd_radius =          'radius'
      character(len=kchara) :: hd_plot_area =       'plot_area_ctl'
      character(len=kchara) :: hd_group_name =      'group_name'
!
!     4th level for center_position
      character(len=kchara) :: hd_center_ctl = 'center_position'
!
!     4th level for axial_length
      character(len=kchara) :: hd_axis_ctl = 'axial_length'
!
!     4th level for coefficients
      character(len=kchara) :: hd_coefs_ctl = 'coefs_ctl'
!
!     4th level for plot_area
      character(len=kchara) :: hd_plot_grp = 'chosen_ele_grp_ctl'
!
!     3rd level for output_field_define
      character(len=kchara) :: hd_psf_result_field = 'output_field'
!
!
      private :: hd_psf_file_head
      private :: hd_psf_out_type, hd_surface_define, hd_output_field
      private :: hd_section_method
      private :: hd_radius, hd_plot_area
      private :: hd_group_name, hd_center_ctl, hd_axis_ctl
      private :: hd_coefs_ctl, hd_plot_grp, hd_psf_result_field
!
      private :: read_psf_output_ctl
      private :: read_psf_plot_area_ctl, read_section_def_control
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_cont_dat_4_psf(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      call dealloc_control_array_c2(psf%psf_out_field_ctl)
      psf%psf_out_field_ctl%num =  0
      psf%psf_out_field_ctl%icou = 0
!
      call dealloc_control_array_chara(psf%psf_area_ctl)
      psf%psf_area_ctl%num =  0
      psf%psf_area_ctl%icou = 0
!
      end subroutine deallocate_cont_dat_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_psf_coefs_ctl(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      call dealloc_control_array_c_r(psf%psf_coefs_ctl)
!
      end subroutine deallocate_psf_coefs_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_psf_center_ctl(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      call dealloc_control_array_c_r(psf%psf_center_ctl)
!
      end subroutine deallocate_psf_center_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_psf_axis_ctl(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
      call dealloc_control_array_c_r(psf%psf_axis_ctl)
!
      end subroutine deallocate_psf_axis_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_control_data_4_psf(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      call load_ctl_label_and_line
      call read_psf_control_data(psf)
!
      end subroutine read_control_data_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine read_psf_control_data(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      if(right_begin_flag(hd_psf_ctl) .eq. 0                            &
     &   .and. right_begin_flag(hd_section_ctl) .eq. 0) return
      if (psf%i_psf_ctl.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_psf_ctl, psf%i_psf_ctl)
        if(psf%i_psf_ctl .gt. 0) exit
        call find_control_end_flag(hd_section_ctl, psf%i_psf_ctl)
        if(psf%i_psf_ctl .gt. 0) exit
!
        call  read_section_def_control(psf)
        call  read_psf_output_ctl(psf)
!
!
        call read_character_ctl_item(hd_psf_file_head,                  &
     &        psf%i_psf_file_head, psf%psf_file_head_ctl)
        call read_character_ctl_item(hd_psf_out_type,                   &
     &        psf%i_psf_out_type, psf%psf_output_type_ctl)
      end do
!
      end subroutine read_psf_control_data
!
!   --------------------------------------------------------------------
!
      subroutine read_section_def_control(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      if(right_begin_flag(hd_surface_define) .eq. 0) return
      if (psf%i_surface_define.gt.0) return
!
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_surface_define,                   &
     &         psf%i_surface_define)
        if(psf%i_surface_define .gt. 0) exit
!
!
        call read_control_array_c_r(hd_coefs_ctl, psf%psf_coefs_ctl)
        call read_control_array_c_r(hd_center_ctl, psf%psf_center_ctl)
        call read_control_array_c_r(hd_axis_ctl, psf%psf_axis_ctl)
!
        call read_psf_plot_area_ctl(psf)
!
!
        call read_real_ctl_item(hd_radius,                              &
     &        psf%i_radius, psf%radius_psf_ctl)
!
        call read_character_ctl_item(hd_section_method,                 &
     &        psf%i_section_method, psf%section_method_ctl)
        call read_character_ctl_item(hd_group_name,                     &
     &        psf%i_group_name, psf%psf_group_name_ctl)
      end do
!
      end subroutine read_section_def_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_psf_output_ctl(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      if(right_begin_flag(hd_output_field) .eq. 0) return
      if (psf%i_output_field .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_output_field, psf%i_output_field)
        if(psf%i_output_field .gt. 0) exit
!
        call read_control_array_c2                                      &
     &     (hd_psf_result_field, psf%psf_out_field_ctl)
      end do
!
      end subroutine read_psf_output_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_psf_plot_area_ctl(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      if(right_begin_flag(hd_plot_area) .eq. 0) return
      if (psf%i_plot_area.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_plot_area, psf%i_plot_area)
        if(psf%i_plot_area .gt. 0) exit
!
        call read_control_array_c1(hd_plot_grp, psf%psf_area_ctl)
      end do
!
      end subroutine read_psf_plot_area_ctl
!
!   --------------------------------------------------------------------
!
      end module m_control_data_4_psf
