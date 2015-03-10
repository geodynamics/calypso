!set_fields_for_psf.f90
!      module set_fields_for_psf
!
!      Written by H. Matsui on June, 2006
!
!!      subroutine set_field_4_psf                                      &
!!     &         (num_psf, numnod, numedge, nnod_4_edge, ie_edge,       &
!!     &          istack_nod_psf_smp, num_phys, ntot_phys, istack_ncomp,&
!!     &          d_nod, psf_param, psf_fld, psf_list, psf_pat)
!!      subroutine set_field_4_iso                                      &
!!     &         (num_iso, numnod, numedge, nnod_4_edge, ie_edge,       &
!!     &          istack_nod_psf_smp, num_phys, ntot_phys, istack_ncomp,&
!!     &          d_nod, iso_param, iso_fld, iso_list, iso_pat)
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
      subroutine set_field_4_psf                                        &
     &         (num_psf, numnod, numedge, nnod_4_edge, ie_edge,         &
     &          istack_nod_psf_smp, num_phys, ntot_phys, istack_ncomp,  &
     &          d_nod, psf_param, psf_fld, psf_list, psf_pat)
!
      use t_phys_data
      use t_psf_geometry_list
      use t_psf_patch_data
!
      integer(kind = kint), intent(in) :: num_psf
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in)                                  &
     &              :: istack_nod_psf_smp(0:num_psf*np_smp)
!
      integer(kind = kint), intent(in) :: num_phys, ntot_phys
      integer(kind = kint), intent(in) :: istack_ncomp(0:num_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,ntot_phys)
!
      type(psf_parameters), intent(in) :: psf_param(num_psf)
      type(phys_data), intent(in) :: psf_fld(num_psf)
      type(sectiong_list), intent(in):: psf_list(num_psf)
!
      type(psf_patch_data), intent(inout) :: psf_pat
!
      integer(kind = kint) :: i, ist_smp
!
      do i = 1, num_psf
!
        ist_smp = (i-1)*np_smp
        call set_field_on_psf(numnod, numedge, nnod_4_edge, ie_edge,    &
     &      psf_pat%nnod_psf_tot, istack_nod_psf_smp(ist_smp),          &
     &      psf_pat%xyz_psf, psf_pat%rr, psf_pat%ar, psf_pat%ss,        &
     &      psf_pat%as, psf_fld(i)%num_phys, psf_pat%max_ncomp_psf,     &
     &      psf_param(i)%id_output, psf_fld(i)%num_component,           &
     &      psf_param(i)%ncomp_org, psf_param(i)%icomp_output,          &
     &      num_phys, ntot_phys, istack_ncomp,                          &
     &      d_nod, psf_pat%dat_psf, psf_pat%tmp_psf, psf_list(i))
!
      end do
!
      end subroutine set_field_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_field_4_iso                                        &
     &         (num_iso, numnod, numedge, nnod_4_edge, ie_edge,         &
     &          istack_nod_psf_smp, num_phys, ntot_phys, istack_ncomp,  &
     &          d_nod, iso_param, iso_fld, iso_list, iso_pat)
!
      use m_control_params_4_iso
      use t_phys_data
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use set_nodal_field_for_psf
!
      integer(kind = kint), intent(in) :: num_iso
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in)                                  &
     &              :: istack_nod_psf_smp(0:num_iso*np_smp)
!
      integer(kind = kint), intent(in) :: num_phys, ntot_phys
      integer(kind = kint), intent(in) :: istack_ncomp(0:num_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,ntot_phys)
!
      type(psf_parameters), intent(in) :: iso_param(num_iso)
      type(phys_data), intent(in) :: iso_fld(num_iso)
      type(sectiong_list), intent(in):: iso_list(num_iso)
!
      type(psf_patch_data), intent(inout) :: iso_pat
!
      integer(kind = kint) :: i, ist_smp
!
      do i = 1, num_iso
        ist_smp = (i-1)*np_smp
!
        if(id_iso_result_type(i) .eq. iflag_constant_iso) then
          call set_const_on_psf(iso_pat%nnod_psf_tot,                   &
     &        istack_nod_psf_smp(ist_smp), result_value_iso(i),         &
     &        iso_pat%dat_psf, iso_list(i))
        else
          call set_field_on_psf(numnod, numedge, nnod_4_edge, ie_edge,  &
     &      iso_pat%nnod_psf_tot, istack_nod_psf_smp(ist_smp),          &
     &      iso_pat%xyz_psf, iso_pat%rr, iso_pat%ar, iso_pat%ss,        &
     &      iso_pat%as, iso_fld(i)%num_phys, iso_pat%max_ncomp_psf,     &
     &      iso_param(i)%id_output, iso_fld(i)%num_component,           &
     &      iso_param(i)%ncomp_org, iso_param(i)%icomp_output,          &
     &      num_phys, ntot_phys, istack_ncomp,                          &
     &      d_nod, iso_pat%dat_psf, iso_pat%tmp_psf, iso_list(i))
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
      type(sectiong_list), intent(in) :: psf_list
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
!
!        write(*,*) 'i', i, ifield_psf(i), ncomp_org(i), icomp_psf(i)
!
        call set_field_on_psf_xyz(numnod, numedge, nnod_4_edge,         &
     &          ie_edge, nnod_patch, istack_n_smp,                      &
     &          num_phys, ntot_phys, istack_ncomp, d_nod,               &
     &          ifield_psf(i), ncomp_org(i), dat_tmp, psf_list)
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
      end module set_fields_for_psf
