!find_node_and_patch_psf.f90
!      module find_node_and_patch_psf
!
!      Written by H. Matsui on June, 2006
!
!!      subroutine set_node_and_patch_psf(num_psf, numnod, numele,      &
!!     &          numedge, nnod_4_ele, nnod_4_edge, inod_global, xx,    &
!!     &          ie, ie_edge, iedge_4_ele, num_surf, num_surf_bc,      &
!!     &          surf_istack, surf_item, ntot_node_sf_grp,             &
!!     &          inod_stack_sf_grp, inod_surf_grp, istack_nod_psf_smp, &
!!     &          istack_patch_psf_smp, psf_search, psf_list, psf_pat)
!!      subroutine set_node_and_patch_iso(numnod, numele,               &
!!     &          numedge, nnod_4_ele, nnod_4_edge, inod_global,        &
!!     &          xx, ie, ie_edge, iedge_4_ele,                         &
!!     &          istack_nod_iso_smp, istack_patch_iso_smp,             &
!!     &          iso_search, iso_list, iso_pat)
!
      module find_node_and_patch_psf
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_and_patch_psf(num_psf, numnod, numele,        &
     &          numedge, nnod_4_ele, nnod_4_edge, inod_global, xx,      &
     &          ie, ie_edge, iedge_4_ele, num_surf, num_surf_bc,        &
     &          surf_istack, surf_item, ntot_node_sf_grp,               &
     &          inod_stack_sf_grp, inod_surf_grp, istack_nod_psf_smp,   &
     &          istack_patch_psf_smp, psf_search, psf_list, psf_pat)
!
      use m_geometry_constants
      use m_machine_parameter
      use m_control_params_4_psf
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use set_nodes_for_psf
      use set_patches_for_psf
!
      integer(kind = kint), intent(in) :: num_psf
      integer(kind = kint), intent(in) :: numnod, numele, numedge
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in)                                  &
     &                     :: iedge_4_ele(numele,nedge_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in)                                  &
     &                     :: surf_istack(0:num_surf)
      integer(kind = kint), intent(in)                                  &
     &                      :: surf_item(2,num_surf_bc)
!
      integer(kind = kint), intent(in) :: ntot_node_sf_grp
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_surf_grp(ntot_node_sf_grp)
!
      integer(kind = kint), intent(inout)                               &
     &      :: istack_nod_psf_smp(0:np_smp*num_psf)
      integer(kind = kint), intent(inout)                               &
     &      :: istack_patch_psf_smp(0:np_smp*num_psf)
!
      type(psf_search_lists), intent(inout) :: psf_search(num_psf)
      type(sectiong_list), intent(inout) :: psf_list(num_psf)
      type(psf_patch_data), intent(inout) :: psf_pat
!
      integer(kind = kint) :: i_psf
!
!
      if (iflag_debug.eq.1)  write(*,*) 'count_nodes_4_psf'
      call count_nodes_4_psf(num_psf, numedge, nnod_4_edge, ie_edge,    &
     &    num_surf, inod_stack_sf_grp, istack_nod_psf_smp,              &
     &    psf_search, psf_list)
!
      psf_pat%nnod_psf_tot = istack_nod_psf_smp(num_psf*np_smp)
      do i_psf = 1, num_psf
        call alloc_inod_psf(psf_list(i_psf))
      end do
      call alloc_position_psf(psf_pat)
!
      if (iflag_debug.eq.1)  write(*,*) 'set_nodes_4_psf'
      call set_nodes_4_psf                                              &
     &   (num_psf, numnod, numedge, nnod_4_edge, inod_global,           &
     &    xx, ie_edge, num_surf, ntot_node_sf_grp, inod_stack_sf_grp,   &
     &    inod_surf_grp, istack_nod_psf_smp, psf_search,                &
     &    psf_list, psf_pat)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_psf_patches'
      call count_psf_patches                                            &
     &   (num_psf, numnod, numele, numedge, nnod_4_ele, ie,             &
     &    iedge_4_ele, num_surf, surf_istack, istack_patch_psf_smp,     &
     &    psf_search, psf_list)
!
      psf_pat%npatch_tot = istack_patch_psf_smp(num_psf*np_smp)
      call alloc_patch_data_psf(psf_pat)
!
      if (iflag_debug.eq.1)  write(*,*) 'set_psf_patches'
      call set_psf_patches(num_psf, numele, numedge, nnod_4_ele, ie,    &
     &    iedge_4_ele, num_surf, num_surf_bc, surf_istack, surf_item,   &
     &    istack_nod_psf_smp, istack_patch_psf_smp, psf_search,         &
     &    psf_list, psf_pat)
!
!
      end subroutine set_node_and_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_and_patch_iso(num_iso, numnod, numele,        &
     &          numedge, nnod_4_ele, nnod_4_edge, inod_global,          &
     &          xx, ie, ie_edge, iedge_4_ele,                           &
     &          istack_nod_iso_smp, istack_patch_iso_smp,               &
     &          iso_search, iso_list, iso_pat)
!
      use m_geometry_constants
      use m_control_params_4_iso
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use set_nodes_for_psf
      use set_patches_for_psf
      use set_fields_for_psf
!
      integer(kind = kint), intent(in) :: num_iso
      integer(kind = kint), intent(in) :: numnod, numele, numedge
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in)                                  &
     &                      :: iedge_4_ele(numele,nedge_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(inout)                               &
     &      :: istack_nod_iso_smp(0:np_smp*num_iso)
      integer(kind = kint), intent(inout)                               &
     &      :: istack_patch_iso_smp(0:np_smp*num_iso)
!
      type(psf_search_lists), intent(inout) :: iso_search(num_iso)
      type(sectiong_list), intent(inout):: iso_list(num_iso)
      type(psf_patch_data), intent(inout) :: iso_pat
!
!
      integer(kind = kint) :: i_iso
!
!
      call count_nodes_4_iso(num_iso, numedge, nnod_4_edge, ie_edge,    &
     &    istack_nod_iso_smp, iso_search, iso_list)
!
      do i_iso = 1, num_iso
        call alloc_inod_psf(iso_list(i_iso))
      end do
      iso_pat%nnod_psf_tot = istack_nod_iso_smp(num_iso*np_smp)
      call alloc_position_psf(iso_pat)
!
      call set_nodes_4_iso(num_iso, numnod, numedge, nnod_4_edge,       &
     &    inod_global, xx, ie_edge, istack_nod_iso_smp,                 &
     &    iso_search, iso_list, iso_pat)
!
!
      call count_iso_patches                                            &
     &   (num_iso, numnod, numele, numedge, nnod_4_ele,                 &
     &    ie, iedge_4_ele, istack_patch_iso_smp, iso_search, iso_list)
!
      iso_pat%npatch_tot = istack_patch_iso_smp(num_iso*np_smp)
      call alloc_patch_data_psf(iso_pat)
!
      call set_iso_patches(num_iso, numele, numedge, iedge_4_ele,       &
     &    istack_nod_iso_smp, istack_patch_iso_smp,                     &
     &    iso_search, iso_list, iso_pat)
!
      end subroutine set_node_and_patch_iso
!
!  ---------------------------------------------------------------------
!
      end module find_node_and_patch_psf
