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
!!      subroutine dealloc_cont_dat_4_psf(psf_c)
!!        type(psf_ctl), intent(inout) :: psf_c
!!      subroutine dup_control_4_psf(org_psf_c, new_psf_c)
!!        type(psf_ctl), intent(in) :: org_psf_c
!!        type(psf_ctl), intent(inout) :: new_psf_c
!!
!!      subroutine read_psf_control_data                                &
!!     &         (id_control, hd_block, psf_c, c_buf)
!!      subroutine bcast_psf_control_data(psf_c)
!!        type(psf_ctl), intent(inout) :: psf_c
!!      subroutine add_fields_4_psf_to_fld_ctl(psf_c, field_ctl)
!!        type(psf_ctl), intent(in) :: psf_c
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!
!!      integer(kind = kint) function num_label_psf_ctl()
!!      integer(kind = kint) function num_label_psf_ctl_w_dpl()
!!      subroutine set_label_psf_ctl(names)
!!      subroutine set_label_psf_ctl_w_dpl(names)
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
      use skip_comment_f
      use t_read_control_elements
      use t_control_array_real
      use t_control_array_character
      use t_control_array_charareal
      use t_control_array_character2
      use t_control_data_4_psf_def
      use t_control_data_4_fld_on_psf
      use calypso_mpi
!
      implicit  none
!
!
      type psf_ctl
!>        Structure of cross section definition
        type(psf_define_ctl) :: psf_def_c
!>        Structure of fields on isosurface control
        type(field_on_psf_ctl) :: fld_on_psf_c
!
!>        Structure for file prefix
        type(read_character_item) :: psf_file_head_ctl
!>        Structure for data field format
        type(read_character_item) :: psf_output_type_ctl
!
!     Top level
        integer (kind=kint) :: i_psf_ctl = 0
!     2nd level for cross_section_ctl
        integer (kind=kint) :: i_output_field =   0
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
!       Deprecated flag
      character(len=kchara), parameter                                  &
     &                  :: hd_psf_file_head =   'psf_file_head'
!
      integer(kind = kint), parameter :: n_label_psf_ctl = 4
      integer(kind = kint), parameter :: n_label_psf_ctl_w_dpl = 5
!
      private :: hd_psf_file_head, hd_psf_file_prefix
      private :: hd_psf_out_type, hd_output_field
      private :: hd_surface_define
      private :: n_label_psf_ctl, n_label_psf_ctl_w_dpl
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
      call init_psf_def_ctl_stract(psf_c%psf_def_c)
      call init_fld_on_psf_control(psf_c%fld_on_psf_c)
!
      end subroutine init_psf_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_cont_dat_4_psf(psf_c)
!
      type(psf_ctl), intent(inout) :: psf_c
!
!
      call dealloc_cont_dat_4_psf_def(psf_c%psf_def_c)
      call dealloc_fld_on_psf_control(psf_c%fld_on_psf_c)
!
      psf_c%psf_file_head_ctl%iflag =   0
      psf_c%psf_output_type_ctl%iflag = 0
!
      psf_c%i_psf_ctl =        0
      psf_c%i_output_field =   0
!
      end subroutine dealloc_cont_dat_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dup_control_4_psf(org_psf_c, new_psf_c)
!
      type(psf_ctl), intent(in) :: org_psf_c
      type(psf_ctl), intent(inout) :: new_psf_c
!
!
      call dup_control_4_psf_def(org_psf_c%psf_def_c,                   &
     &                           new_psf_c%psf_def_c)
      call dup_fld_on_psf_control(org_psf_c%fld_on_psf_c,               &
     &                            new_psf_c%fld_on_psf_c)
!
      call copy_chara_ctl(org_psf_c%psf_file_head_ctl,                  &
     &                    new_psf_c%psf_file_head_ctl)
      call copy_chara_ctl(org_psf_c%psf_output_type_ctl,                &
     &                    new_psf_c%psf_output_type_ctl)
!
      new_psf_c%i_psf_ctl =        org_psf_c%i_psf_ctl
      new_psf_c%i_output_field =   org_psf_c%i_output_field
!
      end subroutine dup_control_4_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_psf_control_data                                  &
     &         (id_control, hd_block, psf_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(psf_ctl), intent(inout) :: psf_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(psf_c%i_psf_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_surface_define)) then
          call read_section_def_control(id_control, hd_surface_define,  &
     &                                  psf_c%psf_def_c, c_buf)
        end if
!
        if(check_begin_flag(c_buf, hd_output_field)) then
          call read_fld_on_psf_control(id_control, hd_output_field,     &
     &                                 psf_c%fld_on_psf_c, c_buf)
        end if
!
        call read_chara_ctl_type(c_buf, hd_psf_file_prefix,             &
     &      psf_c%psf_file_head_ctl)
        call read_chara_ctl_type(c_buf, hd_psf_file_head,               &
     &      psf_c%psf_file_head_ctl)
        call read_chara_ctl_type(c_buf, hd_psf_out_type,                &
     &      psf_c%psf_output_type_ctl)
      end do
      psf_c%i_psf_ctl = 1
!
      end subroutine read_psf_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_psf_control_data(psf_c)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(psf_ctl), intent(inout) :: psf_c
!
!
      call calypso_mpi_bcast_one_int(psf_c%i_psf_ctl, 0)
      call calypso_mpi_bcast_one_int(psf_c%i_output_field, 0)
!
      call bcast_ctl_type_c1(psf_c%psf_file_head_ctl)
      call bcast_ctl_type_c1(psf_c%psf_output_type_ctl)
!
      call bcast_section_def_control(psf_c%psf_def_c)
      call bcast_fld_on_psf_control(psf_c%fld_on_psf_c)
!
      end subroutine bcast_psf_control_data
!
!   --------------------------------------------------------------------
!
      subroutine add_fields_4_psf_to_fld_ctl(psf_c, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      type(psf_ctl), intent(in) :: psf_c
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      call add_fields_on_psf_to_fld_ctl(my_rank, psf_c%fld_on_psf_c,    &
     &                                  field_ctl)
!
      end subroutine add_fields_4_psf_to_fld_ctl
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      integer(kind = kint) function num_label_psf_ctl()
      num_label_psf_ctl = n_label_psf_ctl
      return
      end function num_label_psf_ctl
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_label_psf_ctl_w_dpl()
      num_label_psf_ctl_w_dpl = n_label_psf_ctl_w_dpl
      return
      end function num_label_psf_ctl_w_dpl
!
! ----------------------------------------------------------------------
!
      subroutine set_label_psf_ctl(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_psf_ctl)
!
!
      call set_control_labels(hd_psf_file_prefix, names( 1))
      call set_control_labels(hd_psf_out_type,    names( 2))
      call set_control_labels(hd_surface_define,  names( 3))
      call set_control_labels(hd_output_field,    names( 4))
!
      end subroutine set_label_psf_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_psf_ctl_w_dpl(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_psf_ctl_w_dpl)
!
!
      call set_label_psf_ctl(names(1))
      call set_control_labels(hd_psf_file_head,  names( 5))
!
      end subroutine set_label_psf_ctl_w_dpl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_4_psf
