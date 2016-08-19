!
!      module m_control_params_4_iso
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine allocate_control_params_4_iso(num_iso)
!!      subroutine count_control_4_iso(i_iso, iso, num_mat, mat_name,   &
!!     &          num_nod_phys, phys_nod_name, iso_fld, iso_param)
!!      subroutine set_control_4_iso(i_iso, iso, num_mat, mat_name,     &
!!     &          num_nod_phys, phys_nod_name, iso_fld, iso_param)
!
      module m_control_params_4_iso
!
      use m_precision
!
      implicit  none
!
!
      character(len=kchara), parameter :: cflag_const_iso = 'const'
      character(len=kchara), parameter :: cflag_field_iso = 'field'
!
      integer(kind = kint), parameter :: iflag_constant_iso = -1
      integer(kind = kint), parameter :: iflag_field_iso =     1
!
      character(len = kchara), target, allocatable :: iso_header(:)
      integer(kind = kint), target, allocatable :: itype_iso_file(:)
!
!
      integer(kind = kint), allocatable :: id_isosurf_data(:)
      integer(kind = kint), allocatable :: id_isosurf_comp(:)
      real(kind=kreal), allocatable :: isosurf_value(:)
      real(kind=kreal), allocatable :: result_value_iso(:)
!
      integer(kind = kint), allocatable :: id_iso_result_type(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_control_params_4_iso(num_iso)
!
      use m_field_file_format
!
      integer(kind= kint), intent(in) :: num_iso
!
!
      allocate(iso_header(num_iso))
      allocate(itype_iso_file(num_iso))
!
      allocate(id_isosurf_data(num_iso))
      allocate(id_isosurf_comp(num_iso))
      allocate(isosurf_value(num_iso))
      allocate(result_value_iso(num_iso))
      allocate(id_iso_result_type(num_iso))
!
!
      itype_iso_file = iflag_ucd
      id_iso_result_type = 0
!
      id_isosurf_data =  0
      id_isosurf_comp =  0
      isosurf_value =    0.0d0
      result_value_iso = 0.0d0
!
      end subroutine allocate_control_params_4_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_control_4_iso(i_iso, iso, num_mat, mat_name,     &
     &          num_nod_phys, phys_nod_name, iso_fld, iso_param)
!
      use m_file_format_switch
      use m_control_data_4_iso
      use parallel_ucd_IO_select
      use set_field_comp_for_viz
      use t_phys_data
      use t_psf_patch_data
!
      use set_area_4_viz
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint), intent(in) :: i_iso
      type(iso_ctl), intent(in) :: iso
      type(phys_data), intent(inout) :: iso_fld
      type(psf_parameters), intent(inout) :: iso_param
!
      character(len=kchara) :: tmpchara
!
!
      if(iso%iso_file_head_ctl%iflag .gt. 0) then
        iso_header(i_iso) = iso%iso_file_head_ctl%charavalue
      else
        iso_header(i_iso) =  'iso'
      end if
!
      call choose_para_fld_file_format                                  &
     &   (iso%iso_output_type_ctl%charavalue,                           &
     &    iso%iso_output_type_ctl%iflag, itype_iso_file(i_iso) )
!
      if     (iso%iso_out_field_ctl%num .gt. 0                          &
     &  .and. iso%result_value_iso_ctl%iflag .gt. 0) then
        tmpchara = iso%iso_result_type_ctl%charavalue
        if(cmp_no_case(tmpchara, cflag_field_iso)) then
          id_iso_result_type(i_iso) = iflag_field_iso
        else if(cmp_no_case(tmpchara, cflag_const_iso)) then
          id_iso_result_type(i_iso) = iflag_constant_iso
        end if
!
      else if(iso%iso_out_field_ctl%num .eq. 0) then
        id_iso_result_type(i_iso) = iflag_constant_iso
      else
        id_iso_result_type(i_iso) = iflag_field_iso
      end if
!
      if      (id_iso_result_type(i_iso) .eq. iflag_constant_iso) then
        iso_fld%num_phys = ione
      else if ( id_iso_result_type(i_iso) .eq. iflag_field_iso) then
        call check_field_4_viz(num_nod_phys, phys_nod_name,             &
     &      iso%iso_out_field_ctl%num, iso%iso_out_field_ctl%c1_tbl,    &
     &      iso_fld%num_phys, iso_fld%num_phys_viz)
      end if
!
      call count_area_4_viz(num_mat, mat_name,                          &
     &    iso%iso_area_ctl%num, iso%iso_area_ctl%c_tbl,                 &
     &    iso_param%nele_grp_area)
!
      end subroutine count_control_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_iso(i_iso, iso, num_mat, mat_name,       &
     &          num_nod_phys, phys_nod_name, iso_fld, iso_param)
!
      use m_control_data_4_iso
      use set_area_4_viz
      use set_field_comp_for_viz
      use t_phys_data
      use t_psf_patch_data
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint), intent(in) :: i_iso
      type(iso_ctl), intent(in) :: iso
      type(phys_data), intent(inout) :: iso_fld
      type(psf_parameters), intent(inout) :: iso_param
!
      integer(kind = kint) :: ncomp, ncomp_org
      character(len=kchara) :: tmpchara
!
!
!
      call set_one_component_4_viz(num_nod_phys, phys_nod_name,         &
     &    iso%isosurf_data_ctl%charavalue,                              &
     &    iso%isosurf_comp_ctl%charavalue,                              &
     &    id_isosurf_data(i_iso), id_isosurf_comp(i_iso),               &
     &    ncomp, ncomp_org, tmpchara)
!
      isosurf_value(i_iso) = iso%isosurf_value_ctl%realvalue
!
      call alloc_output_comps_psf(iso_fld%num_phys, iso_param)
      if (id_iso_result_type(i_iso) .eq. iflag_constant_iso) then
        result_value_iso(i_iso) = iso%result_value_iso_ctl%realvalue
        iso_param%id_output(1) = iflag_constant_iso
        iso_param%icomp_output(1) = 0
        iso_fld%num_component(1) = 1
        iso_fld%phys_name(1) =     'color'
!
      else if (id_iso_result_type(i_iso) .eq. iflag_field_iso) then
        call set_components_4_viz(num_nod_phys, phys_nod_name,          &
     &      iso%iso_out_field_ctl%num, iso%iso_out_field_ctl%c1_tbl,    &
     &      iso%iso_out_field_ctl%c2_tbl, iso_fld%num_phys,             &
     &      iso_param%id_output, iso_param%icomp_output,                &
     &      iso_fld%num_component, iso_param%ncomp_org,                 &
     &      iso_fld%phys_name)
      end if
!
      call alloc_area_group_psf(iso_param)
      call s_set_area_4_viz(num_mat, mat_name,                          &
     &     iso%iso_area_ctl%num, iso%iso_area_ctl%c_tbl,                &
     &     iso_param%nele_grp_area, iso_param%id_ele_grp_area)
!
      end subroutine set_control_4_iso
!
!  ---------------------------------------------------------------------
!
      end module m_control_params_4_iso
