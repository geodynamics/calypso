!>@file   ctl_data_section_def_IO.f90
!!@brief  module ctl_data_section_def_IO
!!
!!@author H. Matsui
!!@date Programmed in May. 2006
!
!>@brief  control ID data for surfacing module
!!
!!@verbatim
!!      subroutine init_psf_def_ctl_stract(hd_block, psf_def_c)
!!      subroutine read_section_def_control                             &
!!     &         (id_control, hd_block, psf_def_c, psf_def_c)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(psf_define_ctl), intent(inout) :: psf_def_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_section_def_control                            &
!!     &         (id_control, hd_block, psf_def_c, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(psf_define_ctl), intent(in) :: psf_def_c
!!        integer(kind = kint), intent(inout) :: level
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
      module ctl_data_section_def_IO
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
      use t_control_data_4_psf_def
!
      implicit  none
!
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
      private :: hd_section_method, hd_psf_area
      private :: hd_normal_ctl, hd_center_ctl, hd_axis_ctl
      private :: hd_coefs_ctl, hd_radius, hd_group_name
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
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
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(psf_def_c%i_surface_define .gt. 0) return
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
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
!
      subroutine write_section_def_control                              &
     &         (id_control, hd_block, psf_def_c, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(psf_define_ctl), intent(in) :: psf_def_c
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(psf_def_c%i_surface_define .le. 0) return
!
      maxlen = len_trim(hd_section_method)
      maxlen = max(maxlen, len_trim(hd_radius))
      maxlen = max(maxlen, len_trim(hd_group_name))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_array_c1(id_control, level,                    &
     &    psf_def_c%psf_area_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    psf_def_c%section_method_ctl)
!
      call write_control_array_c_r(id_control, level,                   &
     &    psf_def_c%psf_coefs_ctl)
!
      call write_control_array_c_r(id_control, level,                   &
     &    psf_def_c%psf_normal_ctl)
      call write_control_array_c_r(id_control, level,                   &
     &    psf_def_c%psf_axis_ctl)
!
      call write_control_array_c_r(id_control, level,                   &
     &    psf_def_c%psf_center_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    psf_def_c%radius_psf_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    psf_def_c%psf_group_name_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_section_def_control
!
!   --------------------------------------------------------------------
!
      subroutine init_psf_def_ctl_stract(hd_block, psf_def_c)
!
      character(len=kchara), intent(in) :: hd_block
      type(psf_define_ctl), intent(inout) :: psf_def_c
!
!
      psf_def_c%radius_psf_ctl%realvalue = 0.0d0
      psf_def_c%psf_area_ctl%num =      0
      psf_def_c%block_name = hd_block
!
        call init_c_r_ctl_array_label                                   &
     &     (hd_coefs_ctl, psf_def_c%psf_coefs_ctl)
        call init_c_r_ctl_array_label                                   &
     &     (hd_center_ctl, psf_def_c%psf_center_ctl)
        call init_c_r_ctl_array_label                                   &
     &     (hd_normal_ctl, psf_def_c%psf_normal_ctl)
        call init_c_r_ctl_array_label                                   &
     &     (hd_axis_ctl, psf_def_c%psf_axis_ctl)
!
        call init_chara_ctl_array_label                                 &
     &     (hd_psf_area, psf_def_c%psf_area_ctl)
!
        call init_real_ctl_item_label                                   &
     &     (hd_radius, psf_def_c%radius_psf_ctl)
!
        call init_chara_ctl_item_label                                  &
     &     (hd_section_method, psf_def_c%section_method_ctl)
        call init_chara_ctl_item_label                                  &
     &     (hd_group_name, psf_def_c%psf_group_name_ctl)
!
      end subroutine init_psf_def_ctl_stract
!
!   --------------------------------------------------------------------
!
      end module ctl_data_section_def_IO
