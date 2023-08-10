!>@file   t_control_params_4_iso.f90
!!@brief  module t_control_params_4_iso
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control parameter for isuosurfaces
!!
!!@verbatim
!!      subroutine set_control_iso_def(iso_def_c, ele_grp,              &
!!     &          num_nod_phys, phys_nod_name, iso_param, iso_def)
!!        type(group_data), intent(in) :: ele_grp
!!        integer(kind = kint), intent(in) :: num_nod_phys
!!        character(len=kchara), intent(in)                             &
!!     &                      :: phys_nod_name(num_nod_phys)
!!        type(iso_define_ctl), intent(in) :: iso_def_c
!!        type(psf_parameters), intent(inout) :: iso_param
!!        type(isosurface_define), intent(inout) :: iso_def
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine count_control_4_field_on_iso                         &
!!     &         (fld_on_iso_c, num_nod_phys, phys_nod_name,            &
!!     &          iso_fld, iso_def)
!!      subroutine set_control_4_field_on_iso                           &
!!     &         (fld_on_iso_c, num_nod_phys, phys_nod_name,            &
!!     &          iso_fld, iso_param, iso_def)
!!        type(field_on_psf_ctl), intent(in) :: fld_on_iso_c
!!        integer(kind = kint), intent(in) :: num_nod_phys
!!        character(len=kchara), intent(in)                             &
!!     &                      :: phys_nod_name(num_nod_phys)
!!        type(phys_data), intent(inout) :: iso_fld
!!        type(psf_parameters), intent(inout) :: iso_param
!!        type(isosurface_define), intent(inout) :: iso_def
!!@endverbatim
!
      module t_control_params_4_iso
!
      use m_precision
!
      implicit  none
!
!
      integer(kind = kint), parameter :: iflag_constant_iso = -1
      integer(kind = kint), parameter :: iflag_field_iso =     1
!
      type isosurface_define
        integer(kind = kint) :: id_isosurf_data
        integer(kind = kint) :: id_isosurf_comp
        real(kind=kreal) :: isosurf_value
        real(kind=kreal) :: result_value_iso
!
        integer(kind = kint) :: id_iso_result_type
      end type isosurface_define
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_iso_def(iso_def_c, ele_grp,                &
     &          num_nod_phys, phys_nod_name, iso_param, iso_def, ierr)
!
      use set_area_4_viz
      use set_field_comp_for_viz
      use t_control_data_4_iso_def
      use t_group_data
      use t_phys_data
      use t_psf_patch_data
!
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in)                                 &
     &                      :: phys_nod_name(num_nod_phys)
      type(iso_define_ctl), intent(in) :: iso_def_c
!
      type(psf_parameters), intent(inout) :: iso_param
      type(isosurface_define), intent(inout) :: iso_def
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: ncomp, ncomp_org
      character(len=kchara) :: tmpchara
!
!
      ierr = 0
      call s_set_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &     iso_def_c%iso_area_ctl%num, iso_def_c%iso_area_ctl%c_tbl,    &
     &     iso_param%nele_grp_area, iso_param%id_ele_grp_area)
!
      call set_one_component_4_viz(num_nod_phys, phys_nod_name,         &
     &    iso_def_c%isosurf_data_ctl%charavalue,                        &
     &    iso_def_c%isosurf_comp_ctl%charavalue,                        &
     &    iso_def%id_isosurf_data, iso_def%id_isosurf_comp,             &
     &    ncomp, ncomp_org, tmpchara)
      if (ncomp .gt. 1) ierr = 1
!
      iso_def%isosurf_value = iso_def_c%isosurf_value_ctl%realvalue
!
      end subroutine set_control_iso_def
!
!  ---------------------------------------------------------------------
!
      subroutine count_control_4_field_on_iso                           &
     &         (fld_on_iso_c, num_nod_phys, phys_nod_name,              &
     &          iso_fld, iso_def)
!
      use m_file_format_switch
      use m_section_coef_flags
      use set_field_comp_for_viz
      use t_group_data
      use t_file_IO_parameter
      use t_control_data_4_fld_on_psf
      use t_phys_data
      use t_psf_patch_data
!
      use set_area_4_viz
      use skip_comment_f
!
      type(field_on_psf_ctl), intent(in) :: fld_on_iso_c
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in)                                 &
     &                      :: phys_nod_name(num_nod_phys)
!
      type(phys_data), intent(inout) :: iso_fld
      type(isosurface_define), intent(inout) :: iso_def
!
      character(len=kchara) :: tmpchara
!
!
      if(       fld_on_iso_c%field_output_ctl%num .gt. 0                &
     &    .and. fld_on_iso_c%output_value_ctl%iflag .gt. 0) then
        tmpchara = fld_on_iso_c%output_type_ctl%charavalue
        if(cmp_no_case(tmpchara, cflag_field_iso)) then
          iso_def%id_iso_result_type = iflag_field_iso
        else if(cmp_no_case(tmpchara, cflag_const_iso)) then
          iso_def%id_iso_result_type = iflag_constant_iso
        end if
!
      else if(fld_on_iso_c%field_output_ctl%num .eq. 0) then
        iso_def%id_iso_result_type = iflag_constant_iso
      else
        iso_def%id_iso_result_type = iflag_field_iso
      end if
!
      iso_fld%num_phys =     izero
      iso_fld%num_phys_viz = izero
      if      (iso_def%id_iso_result_type .eq. iflag_constant_iso) then
        iso_fld%num_phys =     ione
        iso_fld%num_phys_viz = ione
      else if (iso_def%id_iso_result_type .eq. iflag_field_iso) then
        call check_field_4_viz(num_nod_phys, phys_nod_name,             &
     &      fld_on_iso_c%field_output_ctl%num,                          &
     &      fld_on_iso_c%field_output_ctl%c1_tbl,                       &
     &      iso_fld%num_phys, iso_fld%num_phys_viz)
      end if
!
      end subroutine count_control_4_field_on_iso
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_field_on_iso                             &
     &         (fld_on_iso_c, num_nod_phys, phys_nod_name,              &
     &          iso_fld, iso_param, iso_def)
!
      use set_area_4_viz
      use set_field_comp_for_viz
      use t_group_data
      use t_control_data_4_fld_on_psf
      use t_phys_data
      use t_psf_patch_data
!
      type(field_on_psf_ctl), intent(in) :: fld_on_iso_c
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(phys_data), intent(inout) :: iso_fld
      type(psf_parameters), intent(inout) :: iso_param
      type(isosurface_define), intent(inout) :: iso_def
!
!
      call alloc_output_comps_psf(iso_fld%num_phys, iso_param)
      if (iso_def%id_iso_result_type .eq. iflag_constant_iso) then
        iso_def%result_value_iso                                        &
     &     = fld_on_iso_c%output_value_ctl%realvalue
        iso_param%id_output(1) = iflag_constant_iso
        iso_param%icomp_output(1) = 0
        iso_fld%num_component(1) = 1
        iso_fld%phys_name(1) =     'color'
!
      else if(iso_def%id_iso_result_type .eq. iflag_field_iso) then
        call set_components_4_viz(num_nod_phys, phys_nod_name,          &
     &      fld_on_iso_c%field_output_ctl%num,                          &
     &      fld_on_iso_c%field_output_ctl%c1_tbl,                       &
     &      fld_on_iso_c%field_output_ctl%c2_tbl, iso_fld%num_phys,     &
     &      iso_param%id_output, iso_param%icomp_output,                &
     &      iso_fld%num_component, iso_param%ncomp_org,                 &
     &      iso_fld%phys_name)
      end if
!
      end subroutine set_control_4_field_on_iso
!
!  ---------------------------------------------------------------------
!
      end module t_control_params_4_iso
