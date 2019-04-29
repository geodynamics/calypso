!>@file   t_control_data_4_psf.f90
!!@brief  module t_control_data_4_psf
!!
!!@author H. Matsui
!!@date Programmed in May. 2006
!
!>@brief  control ID data for surfacing module
!!
!!@verbatim
!!      subroutine init_psf_ctl_stract(psf_c)
!!      subroutine deallocate_cont_dat_4_psf(psf_c)
!!        type(psf_ctl), intent(inout) :: psf_c
!!
!!      subroutine read_psf_control_data(psf_c)
!!      subroutine bcast_psf_control_data(psf_c)
!!        type(psf_ctl), intent(inout) :: psf_c
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! example of control for Kemo's surface rendering
!!
!!  begin cross_section_ctl
!!    section_file_prefix    'psf'
!!    psf_output_type         ucd
!!  
!!    begin surface_define
!!      section_method    equation
!!  
!!      array coefs_ctl  10
!!        coefs_ctl  x2     1.0
!!        coefs_ctl  y2     1.0
!!        coefs_ctl  z2     0.0
!!        coefs_ctl  xy     0.0
!!        coefs_ctl  yz     0.0
!!        coefs_ctl  zx     0.0
!!        coefs_ctl  x      0.0
!!        coefs_ctl  y      0.0
!!        coefs_ctl  z      0.0
!!        coefs_ctl  const  1.0
!!      end array coefs_ctl
!!  
!!      array section_area_ctl 1
!!        section_area_ctl   outer_core   end
!!      end array section_area_ctl
!!    end surface_define
!!  
!!    begin output_field_define
!!      array  output_field   2
!!        output_field    velocity         vector   end
!!        output_field    magnetic_field   radial   end
!!      end  array output_field
!!    end output_field_define
!!  end  cross_section_ctl
!!  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  
!!      psf_output_type:
!!           ucd, OpenDX
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
      module t_control_data_4_psf
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
      use t_control_elements
      use t_read_control_arrays
!
      implicit  none
!
!
      type psf_ctl
!>        Structure for file prefix
        type(read_character_item) :: psf_file_head_ctl
!>        Structure for data field format
        type(read_character_item) :: psf_output_type_ctl
!
!>        Structure for cross section type
        type(read_character_item) :: section_method_ctl
!
!
!>      Structure for coefficients for sueface equation
!!@n      psf_coefs_ctl%c_tbl: 
!!@n      psf_coefs_ctl%vect:  coefficients
        type(ctl_array_cr) :: psf_coefs_ctl
!
!>      Structure for definition of normal vector of plnae sruface
!!@n      psf_normal_ctl%c_tbl: direction of axis
!!@n      psf_normal_ctl%vect:  position
        type(ctl_array_cr) :: psf_normal_ctl
!
!>      Structure for definition of center
!!@n      psf_center_ctl%c_tbl: direction of center
!!@n      psf_center_ctl%vect:  position
        type(ctl_array_cr) :: psf_center_ctl
!
!>      Structure for definition of elipsoid
!!@n      psf_axis_ctl%c_tbl: direction of axis
!!@n      psf_axis_ctl%vect:  vector component
        type(ctl_array_cr) :: psf_axis_ctl
!
!>        Structure for radius
        type(read_real_item) :: radius_psf_ctl
!
!>        Surface group name for section
        type(read_character_item) :: psf_group_name_ctl
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
!     2nd level for cross_section_ctl
        integer (kind=kint) :: i_surface_define = 0
        integer (kind=kint) :: i_output_field =   0
!     3rd level for surface_define
        integer (kind=kint) :: i_plot_area =      0
      end type psf_ctl
!
!
!     2nd level for cross_section_ctl
      character(len=kchara), parameter                                  &
     &                  :: hd_psf_file_prefix = 'section_file_prefix'
      character(len=kchara), parameter                                  &
     &                  :: hd_psf_out_type =    'psf_output_type'
      character(len=kchara), parameter                                  &
     &                  :: hd_surface_define =  'surface_define'
      character(len=kchara), parameter                                  &
     &                  :: hd_output_field = 'output_field_define'
!
!     3rd level for surface_define
      character(len=kchara), parameter                                  &
     &                  :: hd_section_method =  'section_method'
      character(len=kchara), parameter                                  &
     &                  :: hd_radius =          'radius'
      character(len=kchara), parameter                                  &
     &                  :: hd_group_name =      'group_name'
!
      character(len=kchara), parameter                                  &
     &                  :: hd_normal_ctl = 'normal_vector'
      character(len=kchara), parameter                                  &
     &                  :: hd_center_ctl = 'center_position'
      character(len=kchara), parameter                                  &
     &                  :: hd_axis_ctl = 'axial_length'
      character(len=kchara), parameter                                  &
     &                  :: hd_coefs_ctl = 'coefs_ctl'
!
      character(len=kchara), parameter                                  &
     &                  :: hd_psf_area = 'section_area_ctl'
!
!     3rd level for output_field_define
      character(len=kchara), parameter                                  &
     &                  :: hd_psf_result_field = 'output_field'
!
!
      character(len=kchara), parameter                                  &
     &                  :: hd_psf_file_head =   'psf_file_head'
!
      character(len=kchara), parameter                                  &
     &                  :: hd_plot_area =       'plot_area_ctl'
      character(len=kchara), parameter                                  &
     &                  :: hd_plot_grp = 'chosen_ele_grp_ctl'
!
      private :: hd_psf_file_head, hd_psf_file_prefix
      private :: hd_psf_out_type, hd_output_field
      private :: hd_section_method, hd_psf_area
      private :: hd_radius, hd_plot_area
      private :: hd_group_name, hd_center_ctl, hd_axis_ctl
      private :: hd_coefs_ctl, hd_plot_grp, hd_psf_result_field
!
      private :: read_psf_output_ctl, read_psf_plot_area_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_psf_ctl_stract(psf_c)
!
      type(psf_ctl), intent(inout) :: psf_c
!
      psf_c%radius_psf_ctl%realvalue = 0.0d0
      psf_c%psf_out_field_ctl%num = 0
      psf_c%psf_area_ctl%num =      0
!
      end subroutine init_psf_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_cont_dat_4_psf(psf_c)
!
      type(psf_ctl), intent(inout) :: psf_c
!
!
      call dealloc_control_array_c2(psf_c%psf_out_field_ctl)
      psf_c%psf_out_field_ctl%num =  0
      psf_c%psf_out_field_ctl%icou = 0
!
      call dealloc_control_array_chara(psf_c%psf_area_ctl)
      psf_c%psf_area_ctl%num =  0
      psf_c%psf_area_ctl%icou = 0
!
      call dealloc_control_array_c_r(psf_c%psf_coefs_ctl)
      psf_c%psf_coefs_ctl%num =  0
      psf_c%psf_coefs_ctl%icou = 0
!
      call dealloc_control_array_c_r(psf_c%psf_normal_ctl)
      psf_c%psf_normal_ctl%num =  0
      psf_c%psf_normal_ctl%icou = 0
!
      call dealloc_control_array_c_r(psf_c%psf_center_ctl)
      psf_c%psf_center_ctl%num =  0
      psf_c%psf_center_ctl%icou = 0
!
      call dealloc_control_array_c_r(psf_c%psf_axis_ctl)
      psf_c%psf_axis_ctl%num =  0
      psf_c%psf_axis_ctl%icou = 0
!
      end subroutine deallocate_cont_dat_4_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_psf_control_data(hd_block, psf_c)
!
      character(len=kchara), intent(in) :: hd_block
!
      type(psf_ctl), intent(inout) :: psf_c
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (psf_c%i_psf_ctl.gt.0) return
      do
        call load_ctl_label_and_line
!
        psf_c%i_psf_ctl = find_control_end_flag(hd_block)
        if(psf_c%i_psf_ctl .gt. 0) exit
!
        if(right_begin_flag(hd_surface_define) .gt. 0) then
          call  read_section_def_control(psf_c)
        end if
!
        if(right_begin_flag(hd_output_field) .gt. 0) then
          call  read_psf_output_ctl(psf_c)
        end if
!
!
        call read_chara_ctl_type(hd_psf_file_prefix,                    &
     &      psf_c%psf_file_head_ctl)
        call read_chara_ctl_type(hd_psf_file_head,                      &
     &      psf_c%psf_file_head_ctl)
        call read_chara_ctl_type(hd_psf_out_type,                       &
     &      psf_c%psf_output_type_ctl)
      end do
!
      end subroutine read_psf_control_data
!
!   --------------------------------------------------------------------
!
      subroutine read_section_def_control(psf_c)
!
      type(psf_ctl), intent(inout) :: psf_c
!
!
      if(psf_c%i_surface_define.gt.0) return
!
      do
        call load_ctl_label_and_line
!
        psf_c%i_surface_define                                          &
     &       = find_control_end_flag(hd_surface_define)
        if(psf_c%i_surface_define .gt. 0) exit
!
!
        call read_control_array_c_r(hd_coefs_ctl, psf_c%psf_coefs_ctl)
        call read_control_array_c_r                                     &
     &     (hd_center_ctl, psf_c%psf_center_ctl)
        call read_control_array_c_r                                     &
     &     (hd_normal_ctl, psf_c%psf_normal_ctl)
        call read_control_array_c_r(hd_axis_ctl, psf_c%psf_axis_ctl)
!
        call read_control_array_c1(hd_psf_area, psf_c%psf_area_ctl)
!
        call read_psf_plot_area_ctl(psf_c)
!
!
        call read_real_ctl_type(hd_radius, psf_c%radius_psf_ctl)
!
        call read_chara_ctl_type(hd_section_method,                     &
     &      psf_c%section_method_ctl)
        call read_chara_ctl_type                                        &
     &      (hd_group_name, psf_c%psf_group_name_ctl)
      end do
!
      end subroutine read_section_def_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_psf_output_ctl(psf_c)
!
      type(psf_ctl), intent(inout) :: psf_c
!
!
      if (psf_c%i_output_field .gt. 0) return
      do
        call load_ctl_label_and_line
!
        psf_c%i_output_field = find_control_end_flag(hd_output_field)
        if(psf_c%i_output_field .gt. 0) exit
!
        call read_control_array_c2                                      &
     &     (hd_psf_result_field, psf_c%psf_out_field_ctl)
      end do
!
      end subroutine read_psf_output_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_psf_plot_area_ctl(psf_c)
!
      type(psf_ctl), intent(inout) :: psf_c
!
!
      if(right_begin_flag(hd_plot_area) .eq. 0) return
      if (psf_c%i_plot_area.gt.0) return
      do
        call load_ctl_label_and_line
!
        psf_c%i_plot_area = find_control_end_flag(hd_plot_area)
        if(psf_c%i_plot_area .gt. 0) exit
!
        call read_control_array_c1(hd_plot_grp, psf_c%psf_area_ctl)
      end do
!
      end subroutine read_psf_plot_area_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_psf_control_data(psf_c)
!
      use calypso_mpi
      use bcast_control_arrays
!
      type(psf_ctl), intent(inout) :: psf_c
!
!
      call MPI_BCAST(psf_c%i_psf_ctl,  1,                               &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(psf_c%i_output_field,  1,                          &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_array_c2(psf_c%psf_out_field_ctl)
!
      call bcast_ctl_type_c1(psf_c%psf_file_head_ctl)
      call bcast_ctl_type_c1(psf_c%psf_output_type_ctl)
!
      call bcast_section_def_control(psf_c)
!
      end subroutine bcast_psf_control_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_section_def_control(psf_c)
!
      use calypso_mpi
      use bcast_control_arrays
!
      type(psf_ctl), intent(inout) :: psf_c
!
!
      call MPI_BCAST(psf_c%i_surface_define,  1,                        &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(psf_c%i_plot_area,  1,                             &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_array_cr(psf_c%psf_coefs_ctl)
      call bcast_ctl_array_cr(psf_c%psf_center_ctl)
      call bcast_ctl_array_cr(psf_c%psf_normal_ctl)
      call bcast_ctl_array_cr(psf_c%psf_axis_ctl)
!
      call bcast_ctl_array_c1(psf_c%psf_area_ctl)
!
      call bcast_ctl_type_r1(psf_c%radius_psf_ctl)
!
      call bcast_ctl_type_c1(psf_c%section_method_ctl)
      call bcast_ctl_type_c1(psf_c%psf_group_name_ctl)
!
      end subroutine bcast_section_def_control
!
!   --------------------------------------------------------------------
!
      end module t_control_data_4_psf
