!>@file   set_fields_for_psf.f90
!!@brief  module set_fields_for_psf
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!> @brief Construct surface patch
!!
!!@verbatim
!!      subroutine alloc_psf_field_data(num_psf, psf_mesh)
!!      subroutine dealloc_psf_field_data(num_psf, psf_mesh)
!!
!!      subroutine set_field_4_psf(num_psf, edge, nod_fld,              &
!!     &          psf_def, psf_param, psf_list, psf_grp_list, psf_mesh)
!!        type(edge_data), intent(in) :: edge
!!        type(phys_data), intent(in) :: nod_fld
!!        type(section_define), intent(in) :: psf_def(num_psf)
!!        type(psf_parameters), intent(in) :: psf_param(num_psf)
!!        type(sectioning_list), intent(in):: psf_list(num_psf)
!!        type(grp_section_list), intent(in) :: psf_grp_list(num_psf)
!!        type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!!      subroutine set_field_4_iso(num_iso, edge, nod_fld,              &
!!     &          iso_param, iso_def, iso_list, iso_mesh)
!!        type(edge_data), intent(in) :: edge
!!        type(phys_data), intent(in) :: nod_fld
!!        type(psf_parameters), intent(in) :: iso_param(num_iso)
!!        type(isosurface_define), intent(in) :: iso_def(num_iso)
!!        type(sectioning_list), intent(in):: iso_list(num_iso)
!!        type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!!@endverbatim
!
      module set_fields_for_psf
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
      private :: set_field_on_psf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_psf_field_data(num_psf, psf_mesh)
!
      use t_psf_patch_data
!
      integer(kind = kint), intent(in) :: num_psf
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i
!
      do i = 1, num_psf
        call alloc_dat_on_patch_psf(psf_mesh(i))
        call alloc_phys_data_type                                       &
     &     (psf_mesh(i)%node%numnod, psf_mesh(i)%field)
      end do
!
      end subroutine alloc_psf_field_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_psf_field_data(num_psf, psf_mesh)
!
      use t_psf_patch_data
!
      integer(kind = kint), intent(in) :: num_psf
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i
!
      do i = 1, num_psf
        call dealloc_dat_on_patch_psf(psf_mesh(i))
        call dealloc_phys_data_type(psf_mesh(i)%field)
      end do
!
      end subroutine dealloc_psf_field_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_field_4_psf(num_psf, edge, nod_fld,                &
     &          psf_def, psf_param, psf_list, psf_grp_list, psf_mesh)
!
      use t_control_params_4_psf
      use t_edge_data
      use t_phys_data
      use t_psf_geometry_list
      use t_psf_patch_data
!
      integer(kind = kint), intent(in) :: num_psf
      type(edge_data), intent(in) :: edge
      type(phys_data), intent(in) :: nod_fld
!
      type(section_define), intent(in) :: psf_def(num_psf)
      type(psf_parameters), intent(in) :: psf_param(num_psf)
      type(sectioning_list), intent(in):: psf_list(num_psf)
      type(grp_section_list), intent(in) :: psf_grp_list(num_psf)
!
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i
!
      do i = 1, num_psf
        if(psf_def(i)%id_section_method .gt. 0) then
          call set_field_on_psf(nod_fld%n_point,                        &
     &      edge%numedge,  edge%nnod_4_edge, edge%ie_edge,              &
     &      psf_mesh(i)%node%numnod, psf_mesh(i)%node%istack_nod_smp,   &
     &      psf_mesh(i)%node%xx,  psf_mesh(i)%node%rr,                  &
     &      psf_mesh(i)%node%a_r, psf_mesh(i)%node%ss,                  &
     &      psf_mesh(i)%node%a_s, psf_mesh(i)%field%num_phys,           &
     &      psf_mesh(i)%ntot_comp, psf_param(i)%id_output,              &
     &      psf_mesh(i)%field%num_component, psf_param(i)%ncomp_org,    &
     &      psf_param(i)%icomp_output, nod_fld%num_phys,                &
     &      nod_fld%ntot_phys, nod_fld%istack_component, nod_fld%d_fld, &
     &      psf_mesh(i)%field%d_fld, psf_mesh(i)%tmp_psf, psf_list(i))
!
        else if(psf_def(i)%id_section_method .eq. 0) then
          call set_field_on_psf_grp(nod_fld%n_point,                    &
     &      psf_mesh(i)%node%numnod, psf_mesh(i)%node%istack_nod_smp,   &
     &      psf_mesh(i)%node%xx,  psf_mesh(i)%node%rr,                  &
     &      psf_mesh(i)%node%a_r, psf_mesh(i)%node%ss,                  &
     &      psf_mesh(i)%node%a_s, psf_mesh(i)%field%num_phys,           &
     &      psf_mesh(i)%ntot_comp, psf_param(i)%id_output,              &
     &      psf_mesh(i)%field%num_component, psf_param(i)%ncomp_org,    &
     &      psf_param(i)%icomp_output, nod_fld%num_phys,                &
     &      nod_fld%ntot_phys, nod_fld%istack_component, nod_fld%d_fld, &
     &      psf_mesh(i)%field%d_fld, psf_mesh(i)%tmp_psf,               &
     &      psf_grp_list(i))
        end if
      end do
!
      end subroutine set_field_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_field_4_iso(num_iso, edge, nod_fld,                &
     &          iso_param, iso_def, iso_list, iso_mesh)
!
      use t_control_params_4_iso
      use t_edge_data
      use t_phys_data
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use set_nodal_field_for_psf
!
      integer(kind = kint), intent(in) :: num_iso
      type(edge_data), intent(in) :: edge
      type(phys_data), intent(in) :: nod_fld
!
      type(psf_parameters), intent(in) :: iso_param(num_iso)
      type(isosurface_define), intent(in) :: iso_def(num_iso)
      type(sectioning_list), intent(in):: iso_list(num_iso)
!
      type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!
      integer(kind = kint) :: i
!
      do i = 1, num_iso
        if(iso_def(i)%id_iso_result_type .eq. iflag_constant_iso) then
          call set_const_on_psf(iso_mesh(i)%node%numnod,                &
     &        iso_def(i)%result_value_iso, iso_mesh(i)%field%d_fld)
        else
          call set_field_on_psf(nod_fld%n_point,                        &
     &      edge%numedge, edge%nnod_4_edge, edge%ie_edge,               &
     &      iso_mesh(i)%node%numnod, iso_mesh(i)%node%istack_nod_smp,   &
     &      iso_mesh(i)%node%xx,  iso_mesh(i)%node%rr,                  &
     &      iso_mesh(i)%node%a_r, iso_mesh(i)%node%ss,                  &
     &      iso_mesh(i)%node%a_s, iso_mesh(i)%field%num_phys,           &
     &      iso_mesh(i)%ntot_comp, iso_param(i)%id_output,              &
     &      iso_mesh(i)%field%num_component, iso_param(i)%ncomp_org,    &
     &      iso_param(i)%icomp_output, nod_fld%num_phys,                &
     &      nod_fld%ntot_phys, nod_fld%istack_component, nod_fld%d_fld, &
     &      iso_mesh(i)%field%d_fld, iso_mesh(i)%tmp_psf, iso_list(i))
        end if
      end do
!
      end subroutine set_field_4_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_field_on_psf(numnod, numedge, nnod_4_edge,         &
     &      ie_edge, nnod_patch, istack_n_smp, xyz_psf, rr_psf, ar_psf, &
     &      ss_psf, as_psf, nfield_psf, max_ncomp_psf, ifield_psf,      &
     &      ncomp_psf, ncomp_org, icomp_psf, num_phys, ntot_phys,       &
     &      istack_ncomp, d_nod, dat_psf, dat_tmp, psf_list)
!
      use t_psf_geometry_list
      use m_geometry_constants
!
      use set_components_flags
      use set_nodal_field_for_psf
      use convert_components_4_viz
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
!
      integer(kind = kint), intent(in) :: nnod_patch
      integer(kind = kint), intent(in) :: istack_n_smp(0:np_smp)
      real(kind = kreal), intent(in) :: xyz_psf(nnod_patch,3)
      real(kind = kreal), intent(in) :: rr_psf(nnod_patch)
      real(kind = kreal), intent(in) :: ar_psf(nnod_patch)
      real(kind = kreal), intent(in) :: ss_psf(nnod_patch)
      real(kind = kreal), intent(in) :: as_psf(nnod_patch)
      integer(kind = kint), intent(in) :: nfield_psf, max_ncomp_psf
      integer(kind = kint), intent(in) :: ifield_psf(nfield_psf)
      integer(kind = kint), intent(in) :: ncomp_psf(nfield_psf)
      integer(kind = kint), intent(in) :: ncomp_org(nfield_psf)
      integer(kind = kint), intent(in) :: icomp_psf(nfield_psf)
!
      type(sectioning_list), intent(in) :: psf_list
!
      integer(kind = kint), intent(in) :: num_phys, ntot_phys
      integer(kind = kint), intent(in) :: istack_ncomp(0:num_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,ntot_phys)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: dat_psf(nnod_patch,max_ncomp_psf)
!
      real(kind = kreal), intent(inout) :: dat_tmp(nnod_patch,6)
!
      integer(kind = kint) :: i, icou
!
!
      icou = 0
      do i = 1, nfield_psf
          call set_field_on_psf_xyz(numnod, numedge, nnod_4_edge,       &
     &          ie_edge, nnod_patch, num_phys, ntot_phys, istack_ncomp, &
     &          d_nod, ifield_psf(i), ncomp_org(i), dat_tmp, psf_list)
!
        call convert_comps_4_viz(nnod_patch, istack_n_smp,              &
     &      xyz_psf, rr_psf, ar_psf, ss_psf, as_psf,                    &
     &      ncomp_psf(i), ncomp_org(i), icomp_psf(i),                   &
     &      dat_tmp(1,1), dat_psf(1,icou+1))
        icou = icou + ncomp_psf(i)
      end do
!
!
      end subroutine set_field_on_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_field_on_psf_grp(numnod,                           &
     &      nnod_patch, istack_n_smp, xyz_psf, rr_psf, ar_psf,          &
     &      ss_psf, as_psf, nfield_psf, max_ncomp_psf, ifield_psf,      &
     &      ncomp_psf, ncomp_org, icomp_psf, num_phys, ntot_phys,       &
     &      istack_ncomp, d_nod, dat_psf, dat_tmp, psf_grp_list)
!
      use t_psf_geometry_list
      use m_geometry_constants
!
      use set_components_flags
      use set_psf_nodes_4_by_surf_grp
      use convert_components_4_viz
!
      integer(kind = kint), intent(in) :: numnod
!
      integer(kind = kint), intent(in) :: nnod_patch
      integer(kind = kint), intent(in) :: istack_n_smp(0:np_smp)
      real(kind = kreal), intent(in) :: xyz_psf(nnod_patch,3)
      real(kind = kreal), intent(in) :: rr_psf(nnod_patch)
      real(kind = kreal), intent(in) :: ar_psf(nnod_patch)
      real(kind = kreal), intent(in) :: ss_psf(nnod_patch)
      real(kind = kreal), intent(in) :: as_psf(nnod_patch)
      integer(kind = kint), intent(in) :: nfield_psf, max_ncomp_psf
      integer(kind = kint), intent(in) :: ifield_psf(nfield_psf)
      integer(kind = kint), intent(in) :: ncomp_psf(nfield_psf)
      integer(kind = kint), intent(in) :: ncomp_org(nfield_psf)
      integer(kind = kint), intent(in) :: icomp_psf(nfield_psf)
!
      type(grp_section_list), intent(in) :: psf_grp_list
!
      integer(kind = kint), intent(in) :: num_phys, ntot_phys
      integer(kind = kint), intent(in) :: istack_ncomp(0:num_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,ntot_phys)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: dat_psf(nnod_patch,max_ncomp_psf)
!
      real(kind = kreal), intent(inout) :: dat_tmp(nnod_patch,6)
!
      integer(kind = kint) :: i, icou
!
!
      icou = 0
      do i = 1, nfield_psf
        call set_field_on_psf_grp_xyz                                   &
     &     (numnod, nnod_patch, num_phys, ntot_phys, istack_ncomp,      &
     &      d_nod, ifield_psf(i), ncomp_org(i), dat_tmp, psf_grp_list)
!
        call convert_comps_4_viz(nnod_patch, istack_n_smp,              &
     &      xyz_psf, rr_psf, ar_psf, ss_psf, as_psf,                    &
     &      ncomp_psf(i), ncomp_org(i), icomp_psf(i),                   &
     &      dat_tmp(1,1), dat_psf(1,icou+1))
        icou = icou + ncomp_psf(i)
      end do
!
      end subroutine set_field_on_psf_grp
!
!  ---------------------------------------------------------------------
!
      end module set_fields_for_psf
