!>@file   set_sections_file_ctl.f90
!!@brief  module set_sections_file_ctl
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief  Parameters for for cross sections
!!
!!@verbatim
!!      subroutine count_control_4_field_on_psf                         &
!!     &         (fld_on_psf_c, num_nod_phys, phys_nod_name, psf_fld)
!!      subroutine set_control_4_field_on_psf                           &
!!     &         (fld_on_psf_c, num_nod_phys, phys_nod_name,            &
!!     &          psf_fld, psf_param)
!!        integer(kind = kint), intent(in) :: num_nod_phys
!!        character(len=kchara), intent(in)                             &
!!     &                        :: phys_nod_name(num_nod_phys)
!!        type(field_on_psf_ctl), intent(in) :: fld_on_psf_c
!!        type(phys_data), intent(inout) :: psf_fld
!!        type(psf_parameters), intent(inout) :: psf_param
!!
!!      subroutine set_read_psf_file_ctl(default_prefix,                &
!!     &          file_prefix_ctl, file_format_ctl, ucd_param)
!!      subroutine s_set_sections_file_ctl(default_prefix,              &
!!     &          file_prefix_ctl, file_format_ctl, ucd_param)
!!        character(len = kchara), intent(in) :: default_prefix
!!        type(read_character_item), intent(in) :: file_prefix_ctl
!!        type(read_character_item), intent(in) :: file_format_ctl
!!        type(field_IO_params), intent(inout) :: ucd_param
!!
!!      integer(kind = kint) function sel_psf_file_format(file_fmt_ctl)
!!@endverbatim
!
      module set_sections_file_ctl
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_control_4_field_on_psf                           &
     &         (fld_on_psf_c, num_nod_phys, phys_nod_name, psf_fld)
!
      use t_control_data_4_fld_on_psf
      use t_phys_data
      use t_psf_patch_data
      use set_field_comp_for_viz
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in)                                 &
     &                        :: phys_nod_name(num_nod_phys)
      type(field_on_psf_ctl), intent(in) :: fld_on_psf_c
!
      type(phys_data), intent(inout) :: psf_fld
!
!
      call check_field_4_viz(num_nod_phys, phys_nod_name,               &
     &    fld_on_psf_c%field_output_ctl%num,                            &
     &    fld_on_psf_c%field_output_ctl%c1_tbl,                         &
     &    psf_fld%num_phys, psf_fld%num_phys_viz)
!
      end subroutine count_control_4_field_on_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_field_on_psf                             &
     &         (fld_on_psf_c, num_nod_phys, phys_nod_name,              &
     &          psf_fld, psf_param)
!
      use t_control_data_4_fld_on_psf
      use t_phys_data
      use t_psf_patch_data
      use set_field_comp_for_viz
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in)                                 &
     &                        :: phys_nod_name(num_nod_phys)
      type(field_on_psf_ctl), intent(in) :: fld_on_psf_c
!
      type(phys_data), intent(inout) :: psf_fld
      type(psf_parameters), intent(inout) :: psf_param
!
!
      if ( psf_fld%num_phys .gt. 0 ) then
        call set_components_4_viz(num_nod_phys, phys_nod_name,          &
     &      fld_on_psf_c%field_output_ctl%num,                          &
     &      fld_on_psf_c%field_output_ctl%c1_tbl,                       &
     &      fld_on_psf_c%field_output_ctl%c2_tbl, psf_fld%num_phys,     &
     &      psf_param%id_output, psf_param%icomp_output,                &
     &      psf_fld%num_component, psf_param%ncomp_org,                 &
     &      psf_fld%phys_name)
      end if
!
      end subroutine set_control_4_field_on_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_read_psf_file_ctl(default_prefix,                  &
     &          file_prefix_ctl, file_format_ctl, ucd_param)
!
      use t_control_array_character
      use t_file_IO_parameter
      use m_file_format_switch
      use stop_by_missing_zlib
!
      character(len = kchara), intent(in) :: default_prefix
      type(read_character_item), intent(in) :: file_prefix_ctl
      type(read_character_item), intent(in) :: file_format_ctl
      type(field_IO_params), intent(inout) :: ucd_param
!
!
      call s_set_sections_file_ctl(default_prefix,                      &
     &    file_prefix_ctl, file_format_ctl, ucd_param)
      ucd_param%iflag_format                                            &
     &      = mod(ucd_param%iflag_format, iflag_single)
      call stop_by_no_zlib_in_ucd(ucd_param%file_prefix,                &
     &                            ucd_param%iflag_format)
!
      end subroutine set_read_psf_file_ctl
!
! -----------------------------------------------------------------------
!
      subroutine s_set_sections_file_ctl(default_prefix,                &
     &          file_prefix_ctl, file_format_ctl, ucd_param)
!
      use t_control_array_character
      use t_file_IO_parameter
      use m_field_file_format
!
      character(len = kchara), intent(in) :: default_prefix
      type(read_character_item), intent(in) :: file_prefix_ctl
      type(read_character_item), intent(in) :: file_format_ctl
      type(field_IO_params), intent(inout) :: ucd_param
!
!
      ucd_param%iflag_IO = file_prefix_ctl%iflag
      if(ucd_param%iflag_IO .eq. 0) then
        ucd_param%iflag_format = -1
        ucd_param%file_prefix = default_prefix
        return
      else
        ucd_param%file_prefix = file_prefix_ctl%charavalue
      end if
!
      if(file_format_ctl%iflag .eq. 0) then
        ucd_param%iflag_format = iflag_sgl_vtk
      else
        ucd_param%iflag_format                                          &
     &        = sel_psf_file_format(file_format_ctl%charavalue)
      end if
!
      end subroutine s_set_sections_file_ctl
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function sel_psf_file_format(file_fmt_ctl)
!
      use m_merged_field_fmt_labels
      use m_field_file_format
      use t_multi_flag_labels
!
      character(len=kchara), intent(in) :: file_fmt_ctl
!
!
      call init_mgd_field_type_flags
      if     (check_mul_flags(file_fmt_ctl, mgd_udt_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, udt_flags)) then
        sel_psf_file_format = iflag_sgl_udt
      else if(check_mul_flags(file_fmt_ctl, mgd_udt_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, udt_gz_flags)) then
        sel_psf_file_format = iflag_sgl_udt_gz
!
      else if(check_mul_flags(file_fmt_ctl, mgd_ucd_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, ucd_flags)) then
        sel_psf_file_format = iflag_sgl_ucd
      else if(check_mul_flags(file_fmt_ctl, mgd_ucd_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, ucd_gz_flags)) then
        sel_psf_file_format = iflag_sgl_ucd_gz
!
      else if(check_mul_flags(file_fmt_ctl, mgd_vtd_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, vtd_flags)) then
        sel_psf_file_format = iflag_sgl_vtd
      else if(check_mul_flags(file_fmt_ctl, mgd_vtd_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, vtd_gz_flags)) then
        sel_psf_file_format = iflag_sgl_vtd_gz
!
      else if(check_mul_flags(file_fmt_ctl, mgd_vtk_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, vtk_flags)                  &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_fld_ascii_labels)       &
     &   .or. check_mul_flags(file_fmt_ctl, field_ascii_labels)) then
        sel_psf_file_format = iflag_sgl_vtk
      else if(check_mul_flags(file_fmt_ctl, mgd_vtk_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, vtk_gz_flags)               &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_fld_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, field_gz_labels)) then
        sel_psf_file_format = iflag_sgl_vtk_gz
!
      else if(check_mul_flags(file_fmt_ctl, mgd_iso_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, iso_flags)) then
        sel_psf_file_format = iflag_sgl_ucd_bin
      else if(check_mul_flags(file_fmt_ctl, mgd_iso_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, iso_gz_flags)) then
        sel_psf_file_format = iflag_sgl_ucd_bin_gz
!
      else if(check_mul_flags(file_fmt_ctl, mgd_psf_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, psf_flags)                  &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_fld_bin_labels)         &
     &   .or. check_mul_flags(file_fmt_ctl, field_bin_labels)) then
        sel_psf_file_format = iflag_sgl_udt_bin
      else if(check_mul_flags(file_fmt_ctl, mgd_psf_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, psf_gz_flags)               &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_fbin_gz_labels)         &
     &   .or. check_mul_flags(file_fmt_ctl, fbin_gz_labels)) then
        sel_psf_file_format = iflag_sgl_udt_bin_gz
!
      else
        sel_psf_file_format = iflag_sgl_vtk
      end if
      call dealloc_mgd_field_type_flags
!
      end function sel_psf_file_format
!
! -----------------------------------------------------------------------
!
      end module set_sections_file_ctl
