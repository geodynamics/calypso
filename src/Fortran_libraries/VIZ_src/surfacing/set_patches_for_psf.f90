!set_patches_for_psf.f90
!      module set_patches_for_psf
!
!      Written by H. Matsui on June, 2006
!
!!      subroutine count_psf_patches(num_psf, numnod, numele, numedge,  &
!!     &          nnod_4_ele, ie, iedge_4_ele, num_surf_grp,            &
!!     &          istack_surf_grp, istack_patch_psf_smp,                &
!!     &          psf_search, psf_list)
!!      subroutine count_iso_patches(num_iso, numnod, numele, numedge,  &
!!     &          nnod_4_ele, ie, iedge_4_ele, istack_patch_iso_smp,    &
!!     &          iso_search, iso_list)
!!
!!      subroutine set_psf_patches                                      &
!!     &         (num_psf, numele, numedge, nnod_4_ele, ie,             &
!!     &          iedge_4_ele, num_surf_grp, ntot_surf_grp,             &
!!     &          istack_surf_grp, item_surf_grp, istack_nod_psf_smp,   &
!!     &          istack_patch_psf_smp, psf_search, psf_list, psf_pat)
!!      subroutine set_iso_patches(num_iso, numele, numedge,            &
!!     &          iedge_4_ele, istack_nod_iso_smp, istack_patch_iso_smp,&
!!     &          iso_search, iso_list, iso_pat)
!
      module set_patches_for_psf
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_psf_patches(num_psf, numnod, numele, numedge,    &
     &          nnod_4_ele, ie, iedge_4_ele, num_surf_grp,              &
     &          istack_surf_grp, istack_patch_psf_smp,                  &
     &          psf_search, psf_list)
!
      use m_geometry_constants
      use m_control_params_4_psf
      use t_psf_geometry_list
!
      use set_psf_patch_4_by_surf_grp
      use patch_4_psf
!
      integer(kind = kint), intent(in) :: num_psf
      integer(kind = kint), intent(in) :: numnod, numele, numedge
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in)                                  &
     &                     :: iedge_4_ele(numele,nedge_4_ele)
!
      integer(kind = kint), intent(in) :: num_surf_grp
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_surf_grp(0:num_surf_grp)
!
      type(psf_search_lists), intent(inout) :: psf_search(num_psf)
      type(sectiong_list), intent(inout) :: psf_list(num_psf)
      integer(kind = kint), intent(inout)                               &
     &      :: istack_patch_psf_smp(0:np_smp*num_psf)
!
      integer(kind = kint) :: i, ist_smp
!
!
      istack_patch_psf_smp(0) = 0
      do i = 1, num_psf
        call alloc_mark_ele_psf(psf_search(i))
!
        ist_smp = (i-1)*np_smp
        if( id_section_method(i) .gt. 0) then
          call set_psf_type_id(numnod, numele, nnod_4_ele, ie,          &
     &        psf_search(i)%elem_list, psf_search(i)%mark_e,            &
     &        psf_list(i)%ref_fld)
!
          call count_num_patch_4_psf(numele, numedge, iedge_4_ele,      &
     &        psf_search(i)%elem_list, psf_search(i)%mark_e,            &
     &        psf_list(i)%id_n_on_e, istack_patch_psf_smp(ist_smp) )
!
        else if( id_section_method(i) .eq. 0) then
          call count_num_patch_4_grp(num_surf_grp, istack_surf_grp,     &
     &        id_psf_group(i), istack_patch_psf_smp(ist_smp) )
!
        end if
      end do
!
      end subroutine count_psf_patches
!
!  ---------------------------------------------------------------------
!
      subroutine count_iso_patches(num_iso, numnod, numele, numedge,    &
     &          nnod_4_ele, ie, iedge_4_ele, istack_patch_iso_smp,      &
     &          iso_search, iso_list)
!
      use m_geometry_constants
      use m_control_params_4_iso
      use t_psf_geometry_list
!
      use patch_4_psf
!
      integer(kind = kint), intent(in) :: num_iso
      integer(kind = kint), intent(in) :: numnod, numele, numedge
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: iedge_4_ele(numele,nedge_4_ele)
!
      type(psf_search_lists), intent(inout) :: iso_search(num_iso)
      type(sectiong_list), intent(inout) :: iso_list(num_iso)
      integer(kind = kint), intent(inout)                               &
     &      :: istack_patch_iso_smp(0:np_smp*num_iso)
!
      integer(kind = kint) :: i, ist_smp
!
!
      istack_patch_iso_smp(0) = 0
      do i = 1, num_iso
        call alloc_mark_ele_psf(iso_search(i))
!
        ist_smp = (i-1)*np_smp
        call set_psf_type_id(numnod, numele, nnod_4_ele, ie,            &
     &      iso_search(i)%elem_list, iso_search(i)%mark_e,              &
     &      iso_list(i)%ref_fld)
!
        call count_num_patch_4_psf(numele, numedge, iedge_4_ele,        &
     &    iso_search(i)%elem_list, iso_search(i)%mark_e,                &
     &    iso_list(i)%id_n_on_e, istack_patch_iso_smp(ist_smp))
      end do
!
      end subroutine count_iso_patches
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_psf_patches                                        &
     &         (num_psf, numele, numedge, nnod_4_ele, ie,               &
     &          iedge_4_ele, num_surf_grp, ntot_surf_grp,               &
     &          istack_surf_grp, item_surf_grp, istack_nod_psf_smp,     &
     &          istack_patch_psf_smp, psf_search, psf_list, psf_pat)
!
      use m_geometry_constants
      use m_control_params_4_psf
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use set_psf_patch_4_by_surf_grp
      use patch_4_psf
!
      integer(kind = kint), intent(in) :: num_psf
      integer(kind = kint), intent(in) :: numele, numedge
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: iedge_4_ele(numele,nedge_4_ele)
!
      integer(kind = kint), intent(in) :: num_surf_grp, ntot_surf_grp
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_surf_grp(0:num_surf_grp)
      integer(kind = kint), intent(in)                                  &
     &                      :: item_surf_grp(2,ntot_surf_grp)
!
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_psf_smp(0:np_smp*num_psf)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_patch_psf_smp(0:np_smp*num_psf)
!
      type(psf_search_lists), intent(in) :: psf_search(num_psf)
      type(sectiong_list), intent(in) :: psf_list(num_psf)
      type(psf_patch_data), intent(inout) :: psf_pat
!
      integer(kind = kint) :: i, ist_smp
!
      do i = 1, num_psf
        ist_smp = (i-1)*np_smp
        if( id_section_method(i) .gt. 0) then
!
          call set_patch_4_psf(numele, numedge, iedge_4_ele,            &
     &        psf_search(i)%elem_list, psf_search(i)%mark_e,            &
     &        psf_list(i)%id_n_on_e, psf_pat%npatch_tot,                &
     &        istack_patch_psf_smp(ist_smp), psf_pat%ie_tri)
!
        else if( id_section_method(i) .eq. 0) then
          call set_patch_4_grp(numele, numele, nnod_4_ele, ie,          &
     &        num_surf_grp, ntot_surf_grp, istack_surf_grp,             &
     &        item_surf_grp, id_psf_group(i), psf_list(i)%id_n_on_n,    &
     &        psf_pat%npatch_tot,  istack_patch_psf_smp(ist_smp),       &
     &        psf_pat%ie_tri)
!
        end if
!
        call renumber_patch_id_psf(psf_pat%npatch_tot,                  &
     &      istack_nod_psf_smp(ist_smp), istack_patch_psf_smp(ist_smp), &
     &      psf_pat%ie_tri)
      end do
!
      end subroutine set_psf_patches
!
!  ---------------------------------------------------------------------
!
      subroutine set_iso_patches(num_iso, numele, numedge, iedge_4_ele, &
     &          istack_nod_iso_smp, istack_patch_iso_smp,               &
     &          iso_search, iso_list, iso_pat)
!
      use m_geometry_constants
      use m_control_params_4_iso
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use patch_4_psf
!
      integer(kind = kint), intent(in) :: num_iso
      integer(kind = kint), intent(in) :: numele, numedge
      integer(kind = kint), intent(in)                                  &
     &     :: iedge_4_ele(numele,nedge_4_ele)
!
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_iso_smp(0:np_smp*num_iso)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_patch_iso_smp(0:np_smp*num_iso)
!
      type(psf_search_lists), intent(in) :: iso_search(num_iso)
      type(sectiong_list), intent(in) :: iso_list(num_iso)
      type(psf_patch_data), intent(inout) :: iso_pat
!
      integer(kind = kint) :: i, ist_smp
!
      do i = 1, num_iso
        ist_smp = (i-1)*np_smp
        call set_patch_4_psf(numele, numedge, iedge_4_ele,              &
     &      iso_search(i)%elem_list, iso_search(i)%mark_e,              &
     &      iso_list(i)%id_n_on_e, iso_pat%npatch_tot,                  &
     &      istack_patch_iso_smp(ist_smp), iso_pat%ie_tri)
!
        call renumber_patch_id_psf(iso_pat%npatch_tot,                  &
     &      istack_nod_iso_smp(ist_smp), istack_patch_iso_smp(ist_smp), &
     &      iso_pat%ie_tri)
      end do
!
      end subroutine set_iso_patches
!
!  ---------------------------------------------------------------------
!
      end module set_patches_for_psf
