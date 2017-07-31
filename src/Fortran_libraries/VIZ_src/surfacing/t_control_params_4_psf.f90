!
!      module t_control_params_4_psf
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine allocate_control_params_4_psf(num_psf)
!!      subroutine count_control_4_psf(i_psf, psf, num_mat, mat_name,   &
!!     &          num_nod_phys, phys_nod_name, psf_fld, psf_param, ierr)
!!      subroutine set_control_4_psf(i_psf, psf, num_mat, mat_name,     &
!!     &          num_surf, surf_name, num_nod_phys, phys_nod_name,     &
!!     &          psf_fld, psf_param, ierr)
!
      module t_control_params_4_psf
!
      use m_precision
!
      implicit  none
!
!
        character(len = kchara), allocatable :: psf_header(:)
        integer(kind = kint), allocatable :: itype_psf_file(:)
!
        integer(kind = kint), allocatable :: id_section_method(:)
!
!
        real(kind = kreal), allocatable :: const_psf(:,:)
!
      integer(kind = kint), allocatable :: id_psf_group(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_control_params_4_psf(num_psf)
!
      use m_field_file_format
!
      integer(kind= kint), intent(in) :: num_psf
!
!
      allocate(psf_header(num_psf))
      allocate(itype_psf_file(num_psf))
!
      allocate(id_section_method(num_psf))
!
      allocate(const_psf(10,num_psf))
!
      allocate(id_psf_group(num_psf))
!
      itype_psf_file =   iflag_sgl_udt
      id_section_method =  0
      id_psf_group =       0
!
      const_psf = 0.0d0
!
      end subroutine allocate_control_params_4_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_control_4_psf(i_psf, psf, num_mat, mat_name,     &
     &          num_nod_phys, phys_nod_name, psf_fld, psf_param, ierr)
!
      use m_error_IDs
      use t_control_data_4_psf
      use m_file_format_switch
      use t_phys_data
      use t_psf_patch_data
      use parallel_ucd_IO_select
      use set_area_4_viz
      use set_field_comp_for_viz
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint), intent(in) :: i_psf
      type(psf_ctl), intent(in) :: psf
      type(phys_data), intent(inout) :: psf_fld
      type(psf_parameters), intent(inout) :: psf_param
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      if(psf%psf_file_head_ctl%iflag .gt. 0) then
        psf_header(i_psf) =  psf%psf_file_head_ctl%charavalue
      else
        psf_header(i_psf) =  'psf'
      end if
!
      call choose_para_fld_file_format                                  &
     &   (psf%psf_output_type_ctl%charavalue,                           &
     &    psf%psf_output_type_ctl%iflag, itype_psf_file(i_psf) )
!
      call check_field_4_viz(num_nod_phys, phys_nod_name,               &
     &   psf%psf_out_field_ctl%num, psf%psf_out_field_ctl%c1_tbl,       &
     &   psf_fld%num_phys, psf_fld%num_phys_viz)
!
      call count_area_4_viz(num_mat, mat_name,                          &
     &    psf%psf_area_ctl%num, psf%psf_area_ctl%c_tbl,                 &
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
      subroutine set_control_4_psf(i_psf, psf, num_mat, mat_name,       &
     &          num_surf, surf_name, num_nod_phys, phys_nod_name,       &
     &          psf_fld, psf_param, ierr)
!
      use m_error_IDs
      use t_control_data_4_psf
      use t_phys_data
      use t_psf_patch_data
      use set_cross_section_coefs
      use set_area_4_viz
      use set_coefs_of_sections
      use set_field_comp_for_viz
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_surf
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint), intent(in) :: i_psf
      type(psf_ctl), intent(inout) :: psf
      type(phys_data), intent(inout) :: psf_fld
      type(psf_parameters), intent(inout) :: psf_param
      integer(kind = kint), intent(inout) :: ierr
!
      character(len = kchara) :: tmpchara
!
!
      call s_set_coefs_of_sections                                      &
     &   (psf, id_section_method(i_psf), const_psf(1,i_psf), ierr)
!
      if(ierr .gt. 0 .and. cmp_no_case(tmpchara, cflag_grp)) then
        id_section_method(i_psf) = 0
        call set_surf_grp_id_4_viz(num_surf, surf_name,                 &
     &      psf%psf_group_name_ctl%charavalue, id_psf_group(i_psf) )
      else if(ierr .gt. 0) then
        write(e_message,'(a)') 'Set cross section mode'
        return
      end if
!
!
      call alloc_output_comps_psf(psf_fld%num_phys, psf_param)
      if ( psf_fld%num_phys .gt. 0 ) then
        call set_components_4_viz(num_nod_phys, phys_nod_name,          &
     &      psf%psf_out_field_ctl%num, psf%psf_out_field_ctl%c1_tbl,    &
     &      psf%psf_out_field_ctl%c2_tbl, psf_fld%num_phys,             &
     &      psf_param%id_output, psf_param%icomp_output,                &
     &      psf_fld%num_component, psf_param%ncomp_org,                 &
     &      psf_fld%phys_name)
      end if
!
      call alloc_area_group_psf(psf_param)
      call s_set_area_4_viz(num_mat, mat_name,                          &
     &    psf%psf_area_ctl%num, psf%psf_area_ctl%c_tbl,                 &
     &    psf_param%nele_grp_area, psf_param%id_ele_grp_area)
!
      end subroutine set_control_4_psf
!
!  ---------------------------------------------------------------------
!
      end module t_control_params_4_psf
