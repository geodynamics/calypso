!>@file   find_node_and_patch_psf.f90
!!@brief  module find_node_and_patch_psf
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!> @brief Construct surface patch
!!
!!@verbatim
!!      subroutine dealloc_psf_node_and_patch                           &
!!     &         (num_psf, psf_list, psf_mesh)
!!
!!      subroutine set_node_and_patch_psf                               &
!!     &         (num_psf, numnod, internal_node, numele,               &
!!     &          numedge, nnod_4_ele, nnod_4_edge, xx, ie, ie_edge,    &
!!     &          interior_edge, iedge_4_ele, nod_comm, edge_comm,      &
!!     &          num_surf, num_surf_bc, surf_istack, surf_item,        &
!!     &          ntot_node_sf_grp, inod_stack_sf_grp, inod_surf_grp,   &
!!     &          psf_search, psf_list, psf_mesh)
!!      subroutine set_node_and_patch_iso(num_iso, numnod, numele,      &
!!     &          numedge, nnod_4_ele, nnod_4_edge, xx, ie, ie_edge,    &
!!     &          interior_edge, iedge_4_ele, edge_comm,                &
!!     &          iso_search, iso_list, iso_mesh)
!!@endverbatim
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
      subroutine dealloc_psf_node_and_patch                             &
     &         (num_psf, psf_list, psf_mesh)
!
      use t_psf_geometry_list
      use t_psf_patch_data
!
      integer(kind = kint), intent(in) :: num_psf
      type(sectioning_list), intent(inout) :: psf_list(num_psf)
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i_psf
!
!
      do i_psf = 1, num_psf
        call dealloc_nnod_psf(psf_list(i_psf))
        call dealloc_inod_psf(psf_list(i_psf))
        call dealloc_numnod_stack(psf_mesh(i_psf)%node)
        call dealloc_numele_stack(psf_mesh(i_psf)%patch)
        call deallocate_node_geometry_type(psf_mesh(i_psf)%node)
        call deallocate_ele_connect_type(psf_mesh(i_psf)%patch)
      end do
!
      end subroutine dealloc_psf_node_and_patch
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_node_and_patch_psf                                 &
     &         (num_psf, numnod, internal_node, numele,                 &
     &          numedge, nnod_4_ele, nnod_4_edge, xx, ie, ie_edge,      &
     &          interior_edge, iedge_4_ele, nod_comm, edge_comm,        &
     &          num_surf, num_surf_bc, surf_istack, surf_item,          &
     &          ntot_node_sf_grp, inod_stack_sf_grp, inod_surf_grp,     &
     &          psf_search, psf_list, psf_grp_list, psf_mesh)
!
      use m_geometry_constants
      use m_machine_parameter
      use m_control_params_4_psf
      use calypso_mpi
      use t_comm_table
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use set_nodes_for_psf
      use set_patches_for_psf
      use const_element_comm_tables
!
      integer(kind = kint), intent(in) :: num_psf
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: numele, numedge
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: interior_edge(numedge)
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
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: edge_comm
!
      type(psf_search_lists), intent(inout) :: psf_search(num_psf)
      type(sectioning_list), intent(inout) :: psf_list(num_psf)
      type(grp_section_list), intent(inout) :: psf_grp_list(num_psf)
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i_psf
!
!
      do i_psf = 1, num_psf
        if (iflag_debug.eq.1) write(*,*) 'alloc_nnod_psf'
        call alloc_nnod_psf(np_smp, numedge, psf_list(i_psf))
        call alloc_nnod_grp_psf(np_smp, numnod, psf_grp_list(i_psf))
      end do
!
      if (iflag_debug.eq.1)  write(*,*) 'count_nodes_4_psf'
      call count_nodes_4_psf(num_psf, internal_node,                    &
     &    numedge, nnod_4_edge, ie_edge, interior_edge,                 &
     &    num_surf, ntot_node_sf_grp, inod_stack_sf_grp, inod_surf_grp, &
     &    psf_search, psf_list, psf_grp_list, psf_mesh)
!
!
      do i_psf = 1, num_psf
        call alloc_inod_psf(psf_list(i_psf))
        call alloc_inod_grp_psf(psf_grp_list(i_psf))
        call allocate_node_geometry_type(psf_mesh(i_psf)%node)
        call const_global_numnod_list(psf_mesh(i_psf)%node)
      end do
!
      if (iflag_debug.eq.1)  write(*,*) 'set_nodes_4_psf'
      call set_nodes_4_psf                                              &
     &   (num_psf, numnod, internal_node, numedge, nnod_4_edge,         &
     &    xx, ie_edge, interior_edge, nod_comm, edge_comm,              &
     &    num_surf, ntot_node_sf_grp, inod_stack_sf_grp,                &
     &    inod_surf_grp, psf_search, psf_list, psf_grp_list, psf_mesh)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_psf_patches'
      call count_psf_patches                                            &
     &   (num_psf, numnod, numele, numedge, nnod_4_ele, ie,             &
     &    iedge_4_ele, num_surf, surf_istack,                           &
     &    psf_search, psf_list, psf_mesh)
!
      do i_psf = 1, num_psf
        call allocate_ele_connect_type(psf_mesh(i_psf)%patch)
        call const_global_numele_list(psf_mesh(i_psf)%patch)
      end do
!
      if (iflag_debug.eq.1)  write(*,*) 'set_psf_patches'
      call set_psf_patches(num_psf, numele, numedge, nnod_4_ele, ie,    &
     &    iedge_4_ele, num_surf, num_surf_bc, surf_istack, surf_item,   &
     &    psf_search, psf_list, psf_grp_list, psf_mesh)
!
      end subroutine set_node_and_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_and_patch_iso(num_iso, numnod, numele,        &
     &          numedge, nnod_4_ele, nnod_4_edge, xx, ie, ie_edge,      &
     &          interior_edge, iedge_4_ele, edge_comm,                  &
     &          iso_search, iso_list, iso_mesh)
!
      use m_geometry_constants
      use m_control_params_4_iso
      use calypso_mpi
      use t_comm_table
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use set_nodes_for_psf
      use set_patches_for_psf
!
      integer(kind = kint), intent(in) :: num_iso
      integer(kind = kint), intent(in) :: numnod, numele, numedge
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: interior_edge(numedge)
      integer(kind = kint), intent(in)                                  &
     &                      :: iedge_4_ele(numele,nedge_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      type(communication_table), intent(in) :: edge_comm
!
      type(psf_search_lists), intent(inout) :: iso_search(num_iso)
      type(sectioning_list), intent(inout):: iso_list(num_iso)
      type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!
!
      integer(kind = kint) :: i_iso
!
!
      do i_iso = 1, num_iso
        if (iflag_debug.eq.1) write(*,*) 'alloc_nnod_psf'
        call alloc_nnod_psf(np_smp, numedge, iso_list(i_iso))
      end do
!
      call count_nodes_4_iso(num_iso, numedge,                          &
     &    nnod_4_edge, ie_edge, interior_edge,                          &
     &    iso_search, iso_list, iso_mesh)
!
      call set_nodes_4_iso(num_iso, numnod, numedge, nnod_4_edge,       &
     &    xx, ie_edge, interior_edge, edge_comm,                        &
     &    iso_search, iso_list, iso_mesh)
!
!
      call count_iso_patches                                            &
     &   (num_iso, numnod, numele, numedge, nnod_4_ele,                 &
     &    ie, iedge_4_ele, iso_search, iso_list, iso_mesh)
!
      do i_iso = 1, num_iso
        call allocate_ele_connect_type(iso_mesh(i_iso)%patch)
      end do
!
      call set_iso_patches(num_iso, numele, numedge, iedge_4_ele,       &
     &    iso_search, iso_list, iso_mesh)
!
      end subroutine set_node_and_patch_iso
!
!  ---------------------------------------------------------------------
!
      end module find_node_and_patch_psf
