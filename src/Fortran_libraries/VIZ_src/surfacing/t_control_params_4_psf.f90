!
!      module t_control_params_4_psf
!
!        programmed by H.Matsui on May. 2006
!
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
      call set_merged_ucd_file_ctl(default_psf_prefix,                  &
     &    psf_c%psf_file_head_ctl, psf_c%psf_output_type_ctl,           &
     &    psf_file_IO)
      if((psf_file_IO%iflag_format/iflag_single) .eq. 0) then
        psf_file_IO%iflag_format = psf_file_IO%iflag_format             &
     &                            + iflag_single
      end if
!
      call check_field_4_viz(num_nod_phys, phys_nod_name,               &
     &   psf_c%psf_out_field_ctl%num, psf_c%psf_out_field_ctl%c1_tbl,   &
     &   psf_fld%num_phys, psf_fld%num_phys_viz)
!
      call count_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    psf_c%psf_area_ctl%num, psf_c%psf_area_ctl%c_tbl,             &
     &    psf_param%nele_grp_area)
!
      if(psf_param%nele_grp_area .eq. 0) then
        ierr = ierr_VIZ
        write(e_message,'(a)') 'set correct element group'
        return
      end if
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
      use set_cross_section_coefs
      use set_area_4_viz
      use set_coefs_of_sections
      use set_field_comp_for_viz
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
      character(len = kchara) :: tmpchara
!
!
      call s_set_coefs_of_sections(psf_c,                               &
     &    psf_def%id_section_method, psf_def%const_psf, ierr)
!
      tmpchara = psf_c%section_method_ctl%charavalue
      if(ierr .gt. 0 .and. cmp_no_case(tmpchara, cflag_grp)) then
        psf_def%id_section_method = 0
        call set_surf_grp_id_4_viz(sf_grp%num_grp, sf_grp%grp_name,     &
     &      psf_c%psf_group_name_ctl%charavalue,                        &
     &      psf_def%id_psf_group)
      else if(ierr .gt. 0) then
        write(e_message,'(a)') 'Set cross section mode'
        return
      end if
!
!
      call alloc_output_comps_psf(psf_fld%num_phys, psf_param)
      if ( psf_fld%num_phys .gt. 0 ) then
        call set_components_4_viz(num_nod_phys, phys_nod_name,          &
     &     psf_c%psf_out_field_ctl%num, psf_c%psf_out_field_ctl%c1_tbl, &
     &     psf_c%psf_out_field_ctl%c2_tbl, psf_fld%num_phys,            &
     &     psf_param%id_output, psf_param%icomp_output,                 &
     &     psf_fld%num_component, psf_param%ncomp_org,                  &
     &     psf_fld%phys_name)
      end if
!
      call alloc_area_group_psf(psf_param)
      call s_set_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    psf_c%psf_area_ctl%num, psf_c%psf_area_ctl%c_tbl,             &
     &    psf_param%nele_grp_area, psf_param%id_ele_grp_area)
!
      end subroutine set_control_4_psf
!
!  ---------------------------------------------------------------------
!
      end module t_control_params_4_psf
