!>@file   set_nodes_for_psf.f90
!!@brief  module set_nodes_for_psf
!!
!!@date  Programmed by H.Matsui in June, 2006
!
!>@brief Check node positions to generate sections
!!
!!@verbatim
!!      subroutine count_nodes_4_psf(num_psf, numedge, nnod_4_edge,     &
!!     &          ie_edge, num_surf, inod_stack_sf_grp,                 &
!!     &          istack_nod_psf_smp, psf_search, psf_list)
!!      subroutine count_nodes_4_iso(num_iso, numedge, nnod_4_edge,     &
!!     &          ie_edge, istack_nod_iso_smp, iso_search, iso_list)
!!
!!      subroutine set_nodes_4_psf                                      &
!!     &         (num_psf, numnod, numedge, nnod_4_edge,                &
!!     &         inod_global, xx, ie_edge, num_surf, ntot_node_sf_grp,  &
!!     &         inod_stack_sf_grp, inod_surf_grp, istack_nod_psf_smp,  &
!!     &         psf_search, psf_list, psf_pat)
!!      subroutine set_nodes_4_iso                                      &
!!     &        (num_iso, numnod, numedge, nnod_4_edge,                 &
!!     &         inod_global, xx, ie_edge, istack_nod_iso_smp,          &
!!     &         iso_search, iso_list, iso_pat)
!!@endverbatim
!
      module set_nodes_for_psf
!
      use m_precision
!
      use m_machine_parameter
      use set_node_for_sections
      use set_nodal_field_for_psf
      use set_psf_nodes_4_by_surf_grp
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_nodes_4_psf(num_psf, numedge, nnod_4_edge,       &
     &          ie_edge, num_surf, inod_stack_sf_grp,                   &
     &          istack_nod_psf_smp, psf_search, psf_list)
!
      use m_control_params_4_psf
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: num_psf
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
!
      integer(kind = kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
!
      integer(kind = kint), intent(inout)                               &
     &      :: istack_nod_psf_smp(0:np_smp*num_psf)
      type(psf_search_lists), intent(inout) :: psf_search(num_psf)
      type(sectiong_list), intent(inout) :: psf_list(num_psf)
!
      integer(kind = kint) :: i, ist_smp, igrp, num
!
!
      istack_nod_psf_smp(0) = 0
      do i = 1, num_psf
        psf_list(i)%istack_n_on_n_smp(0) = 0
        psf_list(i)%istack_n_on_e_smp(0) = 0
!
!
        ist_smp = (i-1)*np_smp
!
        if( id_section_method(i) .gt. 0) then
!
!          write(*,*) 'count_node_at_node_psf'
          call count_node_at_node_psf                                   &
     &       (psf_search(i)%node_list, psf_list(i))
!
!          write(*,*) 'count_node_on_edge_4_psf'
          call count_node_on_edge_4_psf(numedge, nnod_4_edge, ie_edge,  &
     &        psf_search(i)%edge_list, psf_list(i))
!
        else if ( id_section_method(i) .eq. 0) then
!
          igrp = id_psf_group(i)
          num = inod_stack_sf_grp(igrp  ) - inod_stack_sf_grp(igrp-1)
          call count_node_at_node_on_grp(num,                           &
     &        psf_list(i)%istack_n_on_n_smp)
!
          call count_node_on_edge_on_grp(psf_list(i)%istack_n_on_e_smp)
        end if
!
!          write(*,*) 'count_position_4_psf'
        call count_position_4_psf(istack_nod_psf_smp(ist_smp),          &
     &        psf_list(i)%istack_n_on_n_smp,                            &
     &        psf_list(i)%istack_n_on_e_smp)
!
!
        psf_list(i)%nnod_on_nod = psf_list(i)%istack_n_on_n_smp(np_smp)
        psf_list(i)%nnod_on_edge= psf_list(i)%istack_n_on_e_smp(np_smp)
!
!        write(*,*) 'istack_n_on_n_smp',i,psf_list(i)%istack_n_on_n_smp
!        write(*,*) 'istack_n_on_e_smp',i,psf_list(i)%istack_n_on_e_smp
      end do
!
      end subroutine count_nodes_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine count_nodes_4_iso(num_iso, numedge, nnod_4_edge,       &
     &          ie_edge, istack_nod_iso_smp, iso_search, iso_list)
!
      use m_control_params_4_iso
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: num_iso
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
!
      integer(kind = kint), intent(inout)                               &
     &      :: istack_nod_iso_smp(0:np_smp*num_iso)
      type(psf_search_lists), intent(inout) :: iso_search(num_iso)
      type(sectiong_list), intent(inout) :: iso_list(num_iso)
!
      integer(kind = kint) :: i, ist_smp
!
!
      istack_nod_iso_smp(0) = 0
      do i = 1, num_iso
        iso_list(i)%istack_n_on_n_smp(0) = 0
        iso_list(i)%istack_n_on_e_smp(0) = 0
!
        ist_smp = (i-1)*np_smp
        call count_node_at_node_psf                                     &
     &     (iso_search(i)%node_list, iso_list(i))
!
        call count_node_on_edge_4_psf(numedge, nnod_4_edge, ie_edge,    &
     &      iso_search(i)%edge_list, iso_list(i))
!
        call count_position_4_psf(istack_nod_iso_smp(ist_smp),          &
     &      iso_list(i)%istack_n_on_n_smp,                              &
     &      iso_list(i)%istack_n_on_e_smp)
!
        iso_list(i)%nnod_on_nod = iso_list(i)%istack_n_on_n_smp(np_smp)
        iso_list(i)%nnod_on_edge= iso_list(i)%istack_n_on_e_smp(np_smp)
      end do
!
      end subroutine count_nodes_4_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_nodes_4_psf                                        &
      &        (num_psf, numnod, numedge, nnod_4_edge,                  &
      &         inod_global, xx, ie_edge, num_surf, ntot_node_sf_grp,   &
      &         inod_stack_sf_grp, inod_surf_grp, istack_nod_psf_smp,   &
      &         psf_search, psf_list, psf_pat)
!
      use m_control_params_4_psf
      use t_psf_geometry_list
      use t_psf_patch_data
      use coordinate_converter
      use set_node_on_edge_quad_psf
!
      integer(kind = kint), intent(in) :: num_psf
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: num_surf, ntot_node_sf_grp
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_surf_grp(ntot_node_sf_grp)
!
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_psf_smp(0:np_smp*num_psf)
!
      type(psf_search_lists), intent(in) :: psf_search(num_psf)
      type(sectiong_list), intent(inout) :: psf_list(num_psf)
      type(psf_patch_data), intent(inout) :: psf_pat
!
      integer(kind = kint) :: i, ist_smp, ist, num, igrp
!
      do i = 1, num_psf
        ist_smp = (i-1)*np_smp
!
        if( id_section_method(i) .gt. 0) then
!
          call set_node_at_node_psf                                     &
     &       (numnod, psf_search(i)%node_list, psf_list(i))
!
          call set_node_on_edge_4_psf(numedge, nnod_4_edge, ie_edge,    &
     &        istack_nod_psf_smp(ist_smp), psf_search(i)%edge_list,     &
     &        psf_list(i))
!
          call set_node_on_edge_4_quad_psf(numnod, numedge,             &
     &        nnod_4_edge, ie_edge, xx, const_psf(1,i),                 &
     &        psf_list(i)%nnod_on_edge, np_smp,                         &
     &        psf_list(i)%istack_n_on_e_smp, psf_list(i)%iedge_4_nod,   &
     &        psf_list(i)%coef_on_edge)
!
          call set_nod_on_nod_4_edge_psf(numedge, nnod_4_edge, ie_edge, &
     &        istack_nod_psf_smp(ist_smp), psf_search(i)%edge_list,     &
     &        psf_list(i))
!
        else if( id_section_method(i) .eq. 0) then
          igrp = id_psf_group(i)
          ist = inod_stack_sf_grp(igrp-1)
          num = inod_stack_sf_grp(igrp  ) - ist
          call set_node_at_node_on_grp                                  &
     &       (num, inod_surf_grp(ist+1), psf_list(i))
!
        end if
!
!
        call set_position_4_psf(numnod, numedge, nnod_4_edge,           &
     &     ie_edge, inod_global, xx, psf_pat%nnod_psf_tot,              &
     &     istack_nod_psf_smp(ist_smp), psf_pat%inod_hash_psf,          &
     &     psf_pat%xyz_psf, psf_list(i))
      end do
!
      call position_2_sph(psf_pat%nnod_psf_tot, psf_pat%xyz_psf,        &
     &    psf_pat%rr, psf_pat%theta, psf_pat%phi, psf_pat%ar,           &
     &    psf_pat%ss, psf_pat%as)
!
      end subroutine set_nodes_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_nodes_4_iso                                        &
     &         (num_iso, numnod, numedge, nnod_4_edge,                  &
     &          inod_global, xx, ie_edge, istack_nod_iso_smp,           &
     &          iso_search, iso_list, iso_pat)
!
      use m_control_params_4_iso
      use t_psf_geometry_list
      use t_psf_patch_data
      use coordinate_converter
!
      integer(kind = kint), intent(in) :: num_iso
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_iso_smp(0:np_smp*num_iso)
!
      type(psf_search_lists), intent(in) :: iso_search(num_iso)
      type(sectiong_list), intent(inout) :: iso_list(num_iso)
      type(psf_patch_data), intent(inout) :: iso_pat
!
      integer(kind = kint) :: i, ist_smp
!
!
      do i = 1, num_iso
        ist_smp = (i-1)*np_smp
        call set_node_at_node_psf                                       &
     &     (numnod, iso_search(i)%node_list, iso_list(i))
!
        call set_node_on_edge_4_psf(numedge,  nnod_4_edge, ie_edge,     &
     &      istack_nod_iso_smp(ist_smp), iso_search(i)%edge_list,       &
     &      iso_list(i))
!
        call set_nod_on_nod_4_edge_psf(numedge, nnod_4_edge, ie_edge,   &
     &      istack_nod_iso_smp(ist_smp), iso_search(i)%edge_list,       &
     &      iso_list(i))
!
!
        call set_position_4_psf(numnod, numedge, nnod_4_edge,           &
     &     ie_edge, inod_global, xx, iso_pat%nnod_psf_tot,              &
     &     istack_nod_iso_smp(ist_smp), iso_pat%inod_hash_psf,          &
     &     iso_pat%xyz_psf, iso_list(i))
      end do
!
!
      call position_2_sph(iso_pat%nnod_psf_tot, iso_pat%xyz_psf,        &
     &    iso_pat%rr, iso_pat%theta, iso_pat%phi, iso_pat%ar,           &
     &    iso_pat%ss, iso_pat%as)
!
      end subroutine set_nodes_4_iso
!
!  ---------------------------------------------------------------------
!
      end module set_nodes_for_psf
