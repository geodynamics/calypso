!>@file   t_control_data_4_psf_def.f90
!!@brief  module t_control_data_4_psf_def
!!
!!@author H. Matsui
!!@date Programmed in May. 2006
!
!>@brief  control ID data for surfacing module
!!
!!@verbatim
!!      subroutine init_psf_def_ctl_stract(psf_def_c)
!!      subroutine dealloc_cont_dat_4_psf_def(psf_def_c)
!!        type(psf_define_ctl), intent(inout) :: psf_def_c
!!      subroutine dup_control_4_psf_def(org_psf_def_c, new_psf_def_c)
!!        type(psf_define_ctl), intent(in) :: org_psf_def_c
!!        type(psf_define_ctl), intent(inout) :: new_psf_def_c
!!
!!      subroutine read_section_def_control                             &
!!     &         (id_control, hd_block, psf_def_c, psf_def_c)
!!      subroutine bcast_section_def_control(psf_def_c)
!!        type(psf_define_ctl), intent(inout) :: psf_def_c
!!
!!      integer(kind = kint) function num_label_psf_define_control()
!!      subroutine set_label_psf_define_control(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! example of control for Kemo's surface rendering
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
      module t_control_data_4_psf_def
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
!
      implicit  none
!
!
!>        Structure of cross section definition
      type psf_define_ctl
!>        Structure for cross section type
        type(read_character_item) :: section_method_ctl
!
!>      Structure for coefficients for sueface equation
!!@n      psf_coefs_ctl%c_tbl: 
!!@n      psf_coefs_ctl%vect:  coefficients
        type(ctl_array_cr) :: psf_coefs_ctl
!
!>      Structure for definition of center
!!@n      psf_center_ctl%c_tbl: direction of center
!!@n      psf_center_ctl%vect:  position
        type(ctl_array_cr) :: psf_center_ctl
!>      Structure for definition of normal vector of plnae sruface
!!@n      psf_normal_ctl%c_tbl: direction of axis
!!@n      psf_normal_ctl%vect:  position
        type(ctl_array_cr) :: psf_normal_ctl
!>      Structure for definition of elipsoid
!!@n      psf_axis_ctl%c_tbl: direction of axis
!!@n      psf_axis_ctl%vect:  vector component
        type(ctl_array_cr) :: psf_axis_ctl
!>        Structure for radius
        type(read_real_item) :: radius_psf_ctl
!
!>        Surface group name for section
        type(read_character_item) :: psf_group_name_ctl
!
!>      Structure for element group list for Parallel Sectioning
!!@n      psf_area_ctl%c_tbl: Name of element group
        type(ctl_array_chara) :: psf_area_ctl
!
        integer (kind=kint) :: i_surface_define = 0
      end type psf_define_ctl
!
      character(len=kchara), parameter                                  &
     &                  :: hd_section_method =  'section_method'
      character(len=kchara), parameter                                  &
     &                  :: hd_coefs_ctl = 'coefs_ctl'
      character(len=kchara), parameter                                  &
     &                  :: hd_normal_ctl = 'normal_vector'
      character(len=kchara), parameter                                  &
     &                  :: hd_axis_ctl = 'axial_length'
      character(len=kchara), parameter                                  &
     &                  :: hd_center_ctl = 'center_position'
      character(len=kchara), parameter                                  &
     &                  :: hd_radius =          'radius'
      character(len=kchara), parameter                                  &
     &                  :: hd_group_name =      'group_name'
      character(len=kchara), parameter                                  &
     &                  :: hd_psf_area = 'section_area_ctl'
!
      integer(kind = kint), parameter :: n_label_psf_define_ctl = 8
!
      private :: hd_section_method, hd_psf_area
      private :: hd_normal_ctl, hd_center_ctl, hd_axis_ctl
      private :: hd_coefs_ctl, hd_radius, hd_group_name
      private :: n_label_psf_define_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_psf_def_ctl_stract(psf_def_c)
!
      type(psf_define_ctl), intent(inout) :: psf_def_c
!
      psf_def_c%radius_psf_ctl%realvalue = 0.0d0
      psf_def_c%psf_area_ctl%num =      0
!
      end subroutine init_psf_def_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_cont_dat_4_psf_def(psf_def_c)
!
      type(psf_define_ctl), intent(inout) :: psf_def_c
!
!
      psf_def_c%i_surface_define = 0
!
      psf_def_c%section_method_ctl%iflag =  0
!
      call dealloc_control_array_c_r(psf_def_c%psf_coefs_ctl)
      psf_def_c%psf_coefs_ctl%num =  0
      psf_def_c%psf_coefs_ctl%icou = 0
!
      psf_def_c%radius_psf_ctl%iflag =      0
      call dealloc_control_array_c_r(psf_def_c%psf_center_ctl)
      psf_def_c%psf_center_ctl%num =  0
      psf_def_c%psf_center_ctl%icou = 0
      call dealloc_control_array_c_r(psf_def_c%psf_normal_ctl)
      psf_def_c%psf_normal_ctl%num =  0
      psf_def_c%psf_normal_ctl%icou = 0
      call dealloc_control_array_c_r(psf_def_c%psf_axis_ctl)
      psf_def_c%psf_axis_ctl%num =  0
      psf_def_c%psf_axis_ctl%icou = 0
!
      psf_def_c%psf_group_name_ctl%iflag =  0
!
      call dealloc_control_array_chara(psf_def_c%psf_area_ctl)
      psf_def_c%psf_area_ctl%num =  0
      psf_def_c%psf_area_ctl%icou = 0
!
      end subroutine dealloc_cont_dat_4_psf_def
!
!  ---------------------------------------------------------------------
!
      subroutine dup_control_4_psf_def(org_psf_def_c, new_psf_def_c)
!
      type(psf_define_ctl), intent(in) :: org_psf_def_c
      type(psf_define_ctl), intent(inout) :: new_psf_def_c
!
!
      new_psf_def_c%i_surface_define = org_psf_def_c%i_surface_define
!
      call copy_chara_ctl(org_psf_def_c%section_method_ctl,             &
     &                    new_psf_def_c%section_method_ctl)
!
      call dup_control_array_c_r(org_psf_def_c%psf_coefs_ctl,           &
     &                           new_psf_def_c%psf_coefs_ctl)
!
      call copy_real_ctl(org_psf_def_c%radius_psf_ctl,                  &
     &                    new_psf_def_c%radius_psf_ctl)
      call dup_control_array_c_r(org_psf_def_c%psf_center_ctl,          &
     &                           new_psf_def_c%psf_center_ctl)
      call dup_control_array_c_r(org_psf_def_c%psf_normal_ctl,          &
     &                           new_psf_def_c%psf_normal_ctl)
      call dup_control_array_c_r(org_psf_def_c%psf_axis_ctl,            &
     &                           new_psf_def_c%psf_axis_ctl)
!
      call copy_chara_ctl(org_psf_def_c%psf_group_name_ctl,             &
     &                    new_psf_def_c%psf_group_name_ctl)
!
      call dup_control_array_c1(org_psf_def_c%psf_area_ctl,             &
     &                          new_psf_def_c%psf_area_ctl)
!
      end subroutine dup_control_4_psf_def
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_section_def_control                               &
     &         (id_control, hd_block, psf_def_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(psf_define_ctl), intent(inout) :: psf_def_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(psf_def_c%i_surface_define .gt. 0) return
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c_r(id_control,                         &
     &      hd_coefs_ctl, psf_def_c%psf_coefs_ctl, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_center_ctl, psf_def_c%psf_center_ctl, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_normal_ctl, psf_def_c%psf_normal_ctl, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_axis_ctl, psf_def_c%psf_axis_ctl, c_buf)
!
        call read_control_array_c1(id_control,                          &
     &      hd_psf_area, psf_def_c%psf_area_ctl, c_buf)
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_radius, psf_def_c%radius_psf_ctl)
!
        call read_chara_ctl_type(c_buf,                                 &
     &      hd_section_method, psf_def_c%section_method_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_group_name, psf_def_c%psf_group_name_ctl)
      end do
      psf_def_c%i_surface_define = 1
!
      end subroutine read_section_def_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_section_def_control(psf_def_c)
!
      use calypso_mpi
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(psf_define_ctl), intent(inout) :: psf_def_c
!
!
      call calypso_mpi_bcast_one_int(psf_def_c%i_surface_define, 0)
!
      call bcast_ctl_array_cr(psf_def_c%psf_coefs_ctl)
      call bcast_ctl_array_cr(psf_def_c%psf_center_ctl)
      call bcast_ctl_array_cr(psf_def_c%psf_normal_ctl)
      call bcast_ctl_array_cr(psf_def_c%psf_axis_ctl)
!
      call bcast_ctl_array_c1(psf_def_c%psf_area_ctl)
!
      call bcast_ctl_type_r1(psf_def_c%radius_psf_ctl)
!
      call bcast_ctl_type_c1(psf_def_c%section_method_ctl)
      call bcast_ctl_type_c1(psf_def_c%psf_group_name_ctl)
!
      end subroutine bcast_section_def_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      integer(kind = kint) function num_label_psf_define_control()
      num_label_psf_define_control = n_label_psf_define_ctl
      return
      end function num_label_psf_define_control
!
! ----------------------------------------------------------------------
!
      subroutine set_label_psf_define_control(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_psf_define_ctl)
!
!
      call set_control_labels(hd_section_method, names( 1))
      call set_control_labels(hd_coefs_ctl,      names( 2))
      call set_control_labels(hd_normal_ctl,     names( 3))
      call set_control_labels(hd_axis_ctl,       names( 4))
      call set_control_labels(hd_center_ctl,     names( 5))
      call set_control_labels(hd_radius,         names( 6))
      call set_control_labels(hd_group_name,     names( 7))
      call set_control_labels(hd_psf_area,       names( 8))
!
      end subroutine set_label_psf_define_control
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_4_psf_def
