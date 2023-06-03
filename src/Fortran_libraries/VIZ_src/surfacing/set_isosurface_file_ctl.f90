!>@file   set_isosurface_file_ctl.f90
!!@brief  module set_isosurface_file_ctl
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control parameter for isuosurfaces
!!
!!@verbatim
!!      subroutine count_control_iso_def(iso_def_c, ele_grp, iso_param)
!!        type(group_data), intent(in) :: ele_grp
!!        type(iso_define_ctl), intent(in) :: iso_def_c
!!        type(psf_parameters), intent(inout) :: iso_param
!!
!!      subroutine s_set_isosurface_file_ctl(default_prefix,            &
!!     &          file_prefix_ctl, file_format_ctl, ucd_param)
!!        character(len = kchara), intent(in) :: default_prefix
!!        type(read_character_item), intent(in) :: file_prefix_ctl
!!        type(read_character_item), intent(in) :: file_format_ctl
!!        type(field_IO_params), intent(inout) :: ucd_param
!!
!!      integer(kind = kint) function sel_iso_file_format(file_fmt_ctl)
!!        character(len=kchara), intent(in) :: file_fmt_ctl
!!@endverbatim
!
      module set_isosurface_file_ctl
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
      subroutine count_control_iso_def(iso_def_c, ele_grp, iso_param)
!
      use t_group_data
      use t_control_data_4_iso_def
      use t_psf_patch_data
!
      use set_area_4_viz
!
      type(group_data), intent(in) :: ele_grp
      type(iso_define_ctl), intent(in) :: iso_def_c
!
      type(psf_parameters), intent(inout) :: iso_param
!
!
      call count_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    iso_def_c%iso_area_ctl%num, iso_def_c%iso_area_ctl%c_tbl,     &
     &    iso_param%nele_grp_area)
!
      end subroutine count_control_iso_def
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_set_isosurface_file_ctl(default_prefix,              &
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
     &      = sel_iso_file_format(file_format_ctl%charavalue)
      end if
!
      end subroutine s_set_isosurface_file_ctl
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function sel_iso_file_format(file_fmt_ctl)
!
      use m_merged_field_fmt_labels
      use m_field_file_format
      use t_multi_flag_labels
!
      character(len=kchara), intent(in) :: file_fmt_ctl
!
!
      call init_mgd_field_type_flags
!
      if     (check_mul_flags(file_fmt_ctl, mgd_udt_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_ucd_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, udt_flags)                  &
     &   .or. check_mul_flags(file_fmt_ctl, ucd_flags)) then
        sel_iso_file_format = iflag_sgl_ucd
!
      else if(check_mul_flags(file_fmt_ctl, mgd_udt_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_ucd_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, udt_gz_flags)               &
     &   .or. check_mul_flags(file_fmt_ctl, ucd_gz_flags)) then
        sel_iso_file_format = iflag_sgl_ucd_gz
!
      else if(check_mul_flags(file_fmt_ctl, mgd_vtd_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_vtk_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, vtd_flags)                  &
     &   .or. check_mul_flags(file_fmt_ctl, vtk_flags)                  &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_fld_ascii_labels)       &
     &   .or. check_mul_flags(file_fmt_ctl, field_ascii_labels)) then
        sel_iso_file_format = iflag_sgl_vtk
!
      else if(check_mul_flags(file_fmt_ctl, mgd_vtd_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_vtk_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, vtd_gz_flags)               &
     &   .or. check_mul_flags(file_fmt_ctl, vtk_gz_flags)               &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_fld_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, field_gz_labels)) then
        sel_iso_file_format = iflag_sgl_vtk_gz
!
!
      else if(check_mul_flags(file_fmt_ctl, mgd_iso_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_psf_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, iso_flags)                  &
     &   .or. check_mul_flags(file_fmt_ctl, psf_flags)                  &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_fld_bin_labels)         &
     &   .or. check_mul_flags(file_fmt_ctl, field_bin_labels)) then
        sel_iso_file_format = iflag_sgl_ucd_bin
      else if(check_mul_flags(file_fmt_ctl, mgd_iso_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_psf_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, iso_gz_flags)               &
     &   .or. check_mul_flags(file_fmt_ctl, psf_gz_flags)               &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_fbin_gz_labels)         &
     &   .or. check_mul_flags(file_fmt_ctl, fbin_gz_labels)) then
        sel_iso_file_format = iflag_sgl_ucd_bin_gz
!
      else
        sel_iso_file_format = iflag_sgl_vtk
      end if
      call dealloc_mgd_field_type_flags
!
      end function sel_iso_file_format
!
! -----------------------------------------------------------------------
!
      end module set_isosurface_file_ctl
