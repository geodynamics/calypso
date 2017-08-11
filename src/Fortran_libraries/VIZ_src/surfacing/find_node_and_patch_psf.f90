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
!!        type(sectioning_list), intent(inout) :: psf_list(num_psf)
!!        type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!!      subroutine set_node_and_patch_psf                               &
!!     &         (num_psf, node, ele, edge, nod_comm, edge_comm,        &
!!     &          sf_grp, sf_grp_nod, psf_search, psf_list,             &
!!     &          psf_grp_list, psf_mesh)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(in) :: edge_comm
!!        type(psf_search_lists), intent(inout) :: psf_search(num_psf)
!!        type(sectioning_list), intent(inout) :: psf_list(num_psf)
!!        type(grp_section_list), intent(inout) :: psf_grp_list(num_psf)
!!        type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!!      subroutine set_node_and_patch_iso                               &
!!     &         (num_iso, node, ele, edge, edge_comm,                  &
!!     &          iso_search, iso_list, iso_mesh)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(communication_table), intent(in) :: edge_comm
!!        type(psf_search_lists), intent(inout) :: iso_search(num_iso)
!!        type(sectioning_list), intent(inout):: iso_list(num_iso)
!!        type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
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
     &         (num_psf, node, ele, edge, nod_comm, edge_comm,          &
     &          sf_grp, sf_grp_nod, psf_search, psf_list,               &
     &          psf_grp_list, psf_mesh)
!
      use m_geometry_constants
      use m_machine_parameter
      use m_control_params_4_psf
      use calypso_mpi
      use t_comm_table
      use t_geometry_data
      use t_edge_data
      use t_surface_group_connect
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use set_nodes_for_psf
      use set_patches_for_psf
      use const_element_comm_tables
!
      integer(kind = kint), intent(in) :: num_psf
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
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
        call alloc_nnod_psf(np_smp, edge%numedge, psf_list(i_psf))
        call alloc_nnod_grp_psf                                         &
     &     (np_smp, node%numnod, psf_grp_list(i_psf))
      end do
!
      if (iflag_debug.eq.1)  write(*,*) 'count_nodes_4_psf'
      call count_nodes_4_psf(num_psf, node%internal_node, edge%numedge, &
     &    edge%nnod_4_edge, edge%ie_edge, edge%interior_edge,           &
     &    sf_grp%num_grp, sf_grp_nod%ntot_node_sf_grp,                  &
     &    sf_grp_nod%inod_stack_sf_grp, sf_grp_nod%inod_surf_grp,       &
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
      call set_nodes_4_psf(num_psf, node%numnod, node%internal_node,    &
     &    edge%numedge, edge%nnod_4_edge, node%xx,                      &
     &    edge%ie_edge, edge%interior_edge, nod_comm, edge_comm,        &
     &    sf_grp%num_grp, sf_grp_nod%ntot_node_sf_grp,                  &
     &    sf_grp_nod%inod_stack_sf_grp, sf_grp_nod%inod_surf_grp,       &
     &    psf_search, psf_list, psf_grp_list, psf_mesh)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_psf_patches'
      call count_psf_patches(num_psf, node%numnod,                      &
     &    ele%numele, edge%numedge, ele%nnod_4_ele, ele%ie,             &
     &    edge%iedge_4_ele, sf_grp%num_grp, sf_grp%istack_grp,          &
     &    psf_search, psf_list, psf_mesh)
!
      if (iflag_debug.eq.1)  write(*,*) 'set_psf_patches'
      call set_psf_patches(num_psf, ele%numele, edge%numedge,          &
     &    ele%nnod_4_ele, ele%ie, edge%iedge_4_ele,                    &
     &    sf_grp%num_grp, sf_grp%num_item, sf_grp%istack_grp,          &
     &    sf_grp%item_sf_grp, psf_search, psf_list, psf_grp_list,      &
     &    psf_mesh)
!
      do i_psf = 1, num_psf
        call dealloc_mark_ele_psf(psf_search(i_psf))
      end do
!
      end subroutine set_node_and_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_and_patch_iso                                 &
     &         (num_iso, node, ele, edge, edge_comm,                    &
     &          iso_search, iso_list, iso_mesh)
!
      use m_geometry_constants
      use m_control_params_4_iso
      use calypso_mpi
      use t_comm_table
      use t_geometry_data
      use t_edge_data
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use set_nodes_for_psf
      use set_patches_for_psf
!
      integer(kind = kint), intent(in) :: num_iso
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(communication_table), intent(in) :: edge_comm
!
      type(psf_search_lists), intent(inout) :: iso_search(num_iso)
      type(sectioning_list), intent(inout):: iso_list(num_iso)
      type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!
      integer(kind= kint) :: i_iso
!
!
      call count_nodes_4_iso(num_iso, edge%numedge,                     &
     &    edge%nnod_4_edge, edge%ie_edge, edge%interior_edge,           &
     &    iso_search, iso_list, iso_mesh)
!
      call set_nodes_4_iso                                              &
     &   (num_iso, node%numnod, edge%numedge, edge%nnod_4_edge,         &
     &    node%xx, edge%ie_edge, edge%interior_edge, edge_comm,         &
     &    iso_search, iso_list, iso_mesh)
!
!
      call count_iso_patches(num_iso, node%numnod,                      &
     &    ele%numele, edge%numedge, ele%nnod_4_ele, ele%ie,             &
     &    edge%iedge_4_ele, iso_search, iso_list, iso_mesh)
!
      call set_iso_patches                                              &
     &   (num_iso, ele%numele, edge%numedge, edge%iedge_4_ele,          &
     &    iso_search, iso_list, iso_mesh)
!
      do i_iso = 1, num_iso
        call dealloc_mark_ele_psf(iso_search(i_iso))
      end do
!
      end subroutine set_node_and_patch_iso
!
!  ---------------------------------------------------------------------
!
      end module find_node_and_patch_psf
