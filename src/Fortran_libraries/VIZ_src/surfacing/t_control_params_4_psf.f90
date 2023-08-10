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
!!
!!      subroutine count_control_4_psf_define                           &
!!     &         (psf_def_c, ele_grp, psf_param, ierr)
!!      subroutine set_control_psf_define                               &
!!     &         (psf_def_c, ele_grp, sf_grp, psf_param, psf_def, ierr)
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(psf_define_ctl), intent(in) :: psf_def_c
!!        type(psf_parameters), intent(inout) :: psf_param
!!        type(section_define), intent(inout) :: psf_def
!!        integer(kind = kint), intent(inout) :: ierr
!!@endverbatim
!
      module t_control_params_4_psf
!
      use m_precision
!
      implicit  none
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
      use m_section_coef_flags
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
      end module t_control_params_4_psf
