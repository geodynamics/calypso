!>@file   t_control_params_4_psf.f90
!!@brief  module t_control_params_4_psf
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief  Parameters for for cross sections
!!
!!@verbatim
!!      subroutine alloc_coefficients_4_psf(psf_def)
!!      subroutine dealloc_coefficients_4_psf(psf_def)
!!      subroutine count_control_4_psf                                  &
!!     &         (psf_c, ele_grp, num_nod_phys, phys_nod_name,          &
!!     &          psf_fld, psf_param, psf_file_IO, ierr)
!!        type(psf_ctl), intent(in) :: psf_c
!!        type(group_data), intent(in) :: ele_grp
!!        type(phys_data), intent(inout) :: psf_fld
!!        type(psf_parameters), intent(inout) :: psf_param
!!        type(field_IO_params), intent(inout) :: psf_file_IO
!!      subroutine set_control_4_psf                                    &
!!     &         (psf_c, ele_grp, sf_grp, num_nod_phys, phys_nod_name,  &
!!     &          psf_fld, psf_param, psf_def, ierr)
!!        type(psf_ctl), intent(in) :: psf_c
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_data), intent(inout) :: psf_fld
!!        type(psf_parameters), intent(inout) :: psf_param
!!        type(section_define), intent(inout) :: psf_def
!!
!!      subroutine set_read_psf_file_ctl(default_prefix,                &
!!     &          file_prefix_ctl, file_format_ctl, ucd_param)
!!      subroutine set_merged_psf_file_ctl(default_prefix,              &
!!     &          file_prefix_ctl, file_format_ctl, ucd_param)
!!        character(len = kchara), intent(in) :: default_prefix
!!        type(read_character_item), intent(in) :: file_prefix_ctl
!!        type(read_character_item), intent(in) :: file_format_ctl
!!        type(field_IO_params), intent(inout) :: ucd_param
!!      integer(kind = kint) function sel_psf_file_format(file_fmt_ctl)
!!@endverbatim
!
      module t_control_params_4_psf
!
      use m_precision
!
      implicit  none
!
!
      character(len=kchara), parameter :: default_psf_prefix = 'psf'
!
      type section_define
!>        Sectioning flag
        integer(kind = kint) :: id_section_method
!>        Coefficients of cross section
        real(kind = kreal), allocatable :: const_psf(:)
!>        Surface group name of cross section
        integer(kind = kint) :: id_psf_group
      end type section_define
!
      private :: default_psf_prefix
      private :: count_control_4_psf_define, set_control_psf_define
      private :: count_control_4_field_on_psf
      private :: set_control_4_field_on_psf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_coefficients_4_psf(psf_def)
!
      type(section_define), intent(inout) :: psf_def
!
!
      allocate(psf_def%const_psf(10))
      psf_def%const_psf = 0.0d0
!
      end subroutine alloc_coefficients_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_coefficients_4_psf(psf_def)
!
      type(section_define), intent(inout) :: psf_def
!
!
      deallocate(psf_def%const_psf)
!
      end subroutine dealloc_coefficients_4_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_control_4_psf                                    &
     &         (psf_c, ele_grp, num_nod_phys, phys_nod_name,            &
     &          psf_fld, psf_param, psf_file_IO, ierr)
!
      use m_error_IDs
      use m_file_format_switch
      use t_control_data_4_psf
      use t_group_data
      use t_phys_data
      use t_psf_patch_data
      use t_file_IO_parameter
      use parallel_ucd_IO_select
      use set_area_4_viz
      use set_field_comp_for_viz
!
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(psf_ctl), intent(in) :: psf_c
      type(phys_data), intent(inout) :: psf_fld
      type(psf_parameters), intent(inout) :: psf_param
      type(field_IO_params), intent(inout) :: psf_file_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      call set_merged_psf_file_ctl(default_psf_prefix,                  &
     &    psf_c%psf_file_head_ctl, psf_c%psf_output_type_ctl,           &
     &    psf_file_IO)
      if((psf_file_IO%iflag_format/iflag_single) .eq. 0) then
        psf_file_IO%iflag_format = psf_file_IO%iflag_format             &
     &                            + iflag_single
      end if
!
      call count_control_4_field_on_psf                                 &
     &   (psf_c%fld_on_psf_c, num_nod_phys, phys_nod_name, psf_fld)
      call count_control_4_psf_define                                   &
     &   (psf_c%psf_def_c, ele_grp, psf_param, ierr)
!
      end subroutine count_control_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_psf                                      &
     &         (psf_c, ele_grp, sf_grp, num_nod_phys, phys_nod_name,    &
     &          psf_fld, psf_param, psf_def, ierr)
!
      use m_error_IDs
      use t_control_data_4_psf
      use t_group_data
      use t_phys_data
      use t_psf_patch_data
!
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
      type(psf_ctl), intent(in) :: psf_c
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(phys_data), intent(inout) :: psf_fld
      type(psf_parameters), intent(inout) :: psf_param
      type(section_define), intent(inout) :: psf_def
      integer(kind = kint), intent(inout) :: ierr
!
!
      call alloc_area_group_psf(psf_param)
      call set_control_psf_define                                       &
     &   (psf_c%psf_def_c, ele_grp, sf_grp, psf_param, psf_def, ierr)
!
      call alloc_output_comps_psf(psf_fld%num_phys, psf_param)
      call set_control_4_field_on_psf(psf_c%fld_on_psf_c,               &
     &    num_nod_phys, phys_nod_name,  psf_fld, psf_param)
!
      end subroutine set_control_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine count_control_4_psf_define                             &
     &         (psf_def_c, ele_grp, psf_param, ierr)
!
      use m_error_IDs
      use m_file_format_switch
      use t_control_data_4_psf_def
      use t_group_data
      use t_psf_patch_data
      use set_area_4_viz
!
      type(group_data), intent(in) :: ele_grp
      type(psf_define_ctl), intent(in) :: psf_def_c
!
      type(psf_parameters), intent(inout) :: psf_param
      integer(kind = kint), intent(inout) :: ierr
!
!
      call count_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    psf_def_c%psf_area_ctl%num, psf_def_c%psf_area_ctl%c_tbl,     &
     &    psf_param%nele_grp_area)
!
      if(psf_param%nele_grp_area .eq. 0) then
        ierr = ierr_VIZ
        write(e_message,'(a)') 'set correct element group'
        return
      end if
!
      end subroutine count_control_4_psf_define
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_psf_define                                 &
     &         (psf_def_c, ele_grp, sf_grp, psf_param, psf_def, ierr)
!
      use m_error_IDs
      use t_control_data_4_psf_def
      use t_group_data
      use t_psf_patch_data
      use set_cross_section_coefs
      use set_area_4_viz
      use set_coefs_of_sections
!
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
      type(psf_define_ctl), intent(in) :: psf_def_c
!
      type(psf_parameters), intent(inout) :: psf_param
      type(section_define), intent(inout) :: psf_def
      integer(kind = kint), intent(inout) :: ierr
!
      character(len = kchara) :: tmpchara
!
!
      call s_set_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    psf_def_c%psf_area_ctl%num, psf_def_c%psf_area_ctl%c_tbl,     &
     &    psf_param%nele_grp_area, psf_param%id_ele_grp_area)
!
      call s_set_coefs_of_sections(psf_def_c,                           &
     &    psf_def%id_section_method, psf_def%const_psf, ierr)
!
      tmpchara = psf_def_c%section_method_ctl%charavalue
      if(ierr .gt. 0 .and. cmp_no_case(tmpchara, cflag_grp)) then
        psf_def%id_section_method = 0
        call set_surf_grp_id_4_viz(sf_grp%num_grp, sf_grp%grp_name,     &
     &      psf_def_c%psf_group_name_ctl%charavalue,                    &
     &      psf_def%id_psf_group)
      else if(ierr .gt. 0) then
        write(e_message,'(a)') 'Set cross section mode'
        return
      end if
!
      end subroutine set_control_psf_define
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
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
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
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
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
     &     psf_param%id_output, psf_param%icomp_output,                 &
     &     psf_fld%num_component, psf_param%ncomp_org,                  &
     &     psf_fld%phys_name)
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
!
      character(len = kchara), intent(in) :: default_prefix
      type(read_character_item), intent(in) :: file_prefix_ctl
      type(read_character_item), intent(in) :: file_format_ctl
      type(field_IO_params), intent(inout) :: ucd_param
!
!
      call set_merged_psf_file_ctl(default_prefix,                      &
     &    file_prefix_ctl, file_format_ctl, ucd_param)
      ucd_param%iflag_format                                            &
     &      = mod(ucd_param%iflag_format, iflag_single)
!
      end subroutine set_read_psf_file_ctl
!
! -----------------------------------------------------------------------
!
      subroutine set_merged_psf_file_ctl(default_prefix,                &
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
      end subroutine set_merged_psf_file_ctl
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
      end module t_control_params_4_psf
