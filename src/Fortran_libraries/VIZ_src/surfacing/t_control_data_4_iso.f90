!>@file   t_control_data_4_iso.f90
!!@brief  module t_control_data_4_iso
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for each isosurface
!!
!!@verbatim
!!      subroutine init_iso_ctl_stract(iso_c)
!!      subroutine dealloc_cont_dat_4_iso(iso_c)
!!        type(iso_ctl), intent(inout) :: iso_c
!!      subroutine dup_control_4_iso(org_iso_c, new_iso_c)
!!        type(iso_ctl), intent(in) :: org_iso_c
!!        type(iso_ctl), intent(inout) :: new_iso_c
!!
!!      subroutine add_fields_4_iso_to_fld_ctl(iso_c, field_ctl)
!!        type(iso_ctl), intent(in) :: iso_c
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of control for Kemo's surface rendering
!!
!!  begin isosurface_ctl
!!    isosurface_file_prefix    'psf'
!!    iso_output_type            ucd
!!
!!    begin isosurf_define
!!      isosurf_field        pressure
!!      isosurf_component      scalar
!!      isosurf_value            4000.0
!!
!!      array isosurf_area_ctl   2
!!        isosurf_area_ctl   inner_core   end
!!        isosurf_area_ctl   outer_core   end
!!      end array isosurf_area_ctl
!!    end isosurf_define
!!
!!    begin field_on_isosurf
!!      result_type      constant
!!      result_value     0.7
!!      array output_field   2
!!        output_field    velocity         vector   end
!!        output_field    magnetic_field   radial   end
!!      end array output_field
!!    end field_on_isosurf
!!  end isosurface_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    iso_output_type:
!!           ucd, ucd_gz, iso
!!
!!    result_type:  (Original name: display_method)
!!                   specified_fields
!!                   constant
!!    num_result_comp: number of fields
!!    output_field: (Original name: color_comp and color_subcomp)
!!         field and componenet name for output
!!           x, y, z, radial, elevation, azimuth, cylinder_r
!!           norm, vector, tensor, spherical_vector, cylindrical_vector
!!    result_value: (Original name: specified_color)
!!
!!    
!!
!!    isosurf_data: field for isosurface
!!    isosurf_comp: component for isosurface
!!           x, y, z, radial, elevation, azimuth, cylinder_r, norm
!!    isosurf_value:  value for isosurface
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_4_iso
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use skip_comment_f
      use t_read_control_elements
      use t_control_array_character
      use t_control_data_4_iso_def
      use t_control_data_4_fld_on_psf
!
      implicit  none
!
      type iso_ctl
!>        Structure of isosurface define control
        type(iso_define_ctl) :: iso_def_c
!>        file name for fields on isosurface control
        character(len=kchara) :: fname_fld_on_iso
!>        Structure of fields on isosurface control
        type(field_on_psf_ctl) :: fld_on_iso_c
!
!>        Structure for file prefix
        type(read_character_item) :: iso_file_head_ctl
!>        Structure for data field format
        type(read_character_item) :: iso_output_type_ctl
!
!     Top level
        integer (kind=kint) :: i_iso_ctl = 0
      end type iso_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_iso_ctl_stract(iso_c)
!
      type(iso_ctl), intent(inout) :: iso_c
!
!
      call init_iso_define_control(iso_c%iso_def_c)
      call init_fld_on_psf_control(iso_c%fld_on_iso_c)
!
      end subroutine init_iso_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_cont_dat_4_iso(iso_c)
!
      type(iso_ctl), intent(inout) :: iso_c
!
!
      iso_c%iso_file_head_ctl%iflag =    0
      iso_c%iso_output_type_ctl%iflag =  0
!
      iso_c%i_iso_ctl =         0
!
      call dealloc_iso_define_control(iso_c%iso_def_c)
      call dealloc_fld_on_psf_control(iso_c%fld_on_iso_c)
!
      end subroutine dealloc_cont_dat_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine dup_control_4_iso(org_iso_c, new_iso_c)
!
      type(iso_ctl), intent(in) :: org_iso_c
      type(iso_ctl), intent(inout) :: new_iso_c
!
!
      call copy_chara_ctl(org_iso_c%iso_file_head_ctl,                  &
     &                    new_iso_c%iso_file_head_ctl)
      call copy_chara_ctl(org_iso_c%iso_output_type_ctl,                &
     &                    new_iso_c%iso_output_type_ctl)
!
      new_iso_c%i_iso_ctl =        org_iso_c%i_iso_ctl
      new_iso_c%fname_fld_on_iso = org_iso_c%fname_fld_on_iso
!
      call dup_iso_define_control                                       &
     &   (org_iso_c%iso_def_c, new_iso_c%iso_def_c)
      call dup_fld_on_psf_control                                       &
     &   (org_iso_c%fld_on_iso_c, new_iso_c%fld_on_iso_c)
!
      end subroutine dup_control_4_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_fields_4_iso_to_fld_ctl(iso_c, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      type(iso_ctl), intent(in) :: iso_c
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(iso_c%iso_def_c%isosurf_data_ctl%iflag .gt. 0) then
        call add_viz_name_ctl                                           &
     &     (iso_c%iso_def_c%isosurf_data_ctl%charavalue, field_ctl)
      end if
!
      call add_fields_on_psf_to_fld_ctl(iso_c%fld_on_iso_c, field_ctl)
!
      end subroutine add_fields_4_iso_to_fld_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_4_iso
