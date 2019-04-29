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
!!     &         (num_psf, mesh, ele_mesh, group, psf_case_tbls,        &
!!     &          psf_def, psf_search, psf_list, psf_grp_list, psf_mesh)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(in) :: edge_comm
!!        type(psf_cases), intent(in) :: psf_case_tbls
!!        type(psf_search_lists), intent(inout) :: psf_search(num_psf)
!!        type(sectioning_list), intent(inout) :: psf_list(num_psf)
!!        type(grp_section_list), intent(inout) :: psf_grp_list(num_psf)
!!        type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!!      subroutine set_node_and_patch_iso                               &
!!     &         (num_iso, mesh, ele_mesh, psf_case_tbls,               &
!!     &          iso_search, iso_list, iso_mesh)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(psf_cases), intent(in) :: psf_case_tbls
!!        type(psf_search_lists), intent(inout) :: iso_search(num_iso)
!!        type(sectioning_list), intent(inout):: iso_list(num_iso)
!!        type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!!@endverbatim
!
      module find_node_and_patch_psf
!
      use m_precision
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
        call dealloc_node_geometry_w_sph(psf_mesh(i_psf)%node)
        call deallocate_ele_connect_type(psf_mesh(i_psf)%patch)
      end do
!
      end subroutine dealloc_psf_node_and_patch
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_node_and_patch_psf                                 &
     &         (num_psf, mesh, ele_mesh, group, psf_case_tbls,          &
     &          psf_def, psf_search, psf_list, psf_grp_list, psf_mesh)
!
      use m_geometry_constants
      use calypso_mpi
      use t_control_params_4_psf
      use t_mesh_data
      use t_surface_group_connect
      use t_psf_geometry_list
      use t_psf_patch_data
      use t_psf_case_table
!
      use set_nodes_for_psf
      use set_patches_for_psf
      use const_element_comm_tables
!
      integer(kind = kint), intent(in) :: num_psf
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_groups), intent(in) ::   group
!
      type(psf_cases), intent(in) :: psf_case_tbls
      type(section_define), intent(in) :: psf_def(num_psf)
!
      type(psf_search_lists), intent(inout) :: psf_search(num_psf)
      type(sectioning_list), intent(inout) :: psf_list(num_psf)
      type(grp_section_list), intent(inout) :: psf_grp_list(num_psf)
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i_psf, nele_psf
      integer(kind = kint) :: ntot_failed(num_psf), ntot_failed_gl
!
!
      do i_psf = 1, num_psf
        if (iflag_debug.eq.1) write(*,*) 'alloc_nnod_psf'
        call alloc_nnod_psf                                             &
     &     (np_smp, ele_mesh%edge, psf_list(i_psf))
        call alloc_nnod_grp_psf                                         &
     &     (np_smp, mesh%node, psf_grp_list(i_psf))
      end do
!
      if (iflag_debug.eq.1)  write(*,*) 'count_nodes_4_psf'
      call count_nodes_4_psf(num_psf, mesh%node, ele_mesh%edge,         &
     &    group%surf_grp, group%surf_nod_grp,                           &
     &    psf_def, psf_search, psf_list, psf_grp_list, psf_mesh)
!
!
      do i_psf = 1, num_psf
        call alloc_inod_psf(psf_list(i_psf))
        call alloc_inod_grp_psf(psf_grp_list(i_psf))
        call alloc_node_geometry_w_sph(psf_mesh(i_psf)%node)
        call const_global_numnod_list(psf_mesh(i_psf)%node)
      end do
!
      if (iflag_debug.eq.1)  write(*,*) 'set_nodes_4_psf'
      call set_nodes_4_psf(num_psf, mesh%node, ele_mesh%edge,           &
     &    mesh%nod_comm, ele_mesh%edge_comm,                            &
     &    group%surf_grp, group%surf_nod_grp, psf_def, psf_search,      &
     &    psf_list, psf_grp_list, psf_mesh)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_psf_patches'
      do i_psf = 1, num_psf
        call count_psf_patches(mesh%node, mesh%ele,                     &
     &      ele_mesh%edge, group%surf_grp, psf_case_tbls,               &
     &      psf_def(i_psf), psf_search(i_psf), psf_list(i_psf),         &
     &      psf_mesh(i_psf)%patch, ntot_failed(i_psf))
!
        if (iflag_debug.eq.1)  write(*,*) 'set_psf_patches'
        call set_psf_patches(mesh%ele, ele_mesh%edge, group%surf_grp,   &
     &     psf_case_tbls, psf_def(i_psf), psf_search(i_psf),            &
     &     psf_list(i_psf), psf_grp_list(i_psf), psf_mesh(i_psf)%patch)
!
        call dealloc_mark_ele_psf(psf_search(i_psf))
      end do
!
      if(i_debug .eq. 0) return
      do i_psf = 1, num_psf
        call mpi_allreduce(psf_mesh(i_psf)%patch%numele, nele_psf,     &
     &      1, CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
        call mpi_allreduce(ntot_failed(i_psf), ntot_failed_gl,         &
     &      1, CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
        if(my_rank .eq. 0) write(*,*) 'nele_psf', i_psf, nele_psf
        if(my_rank .eq. 0) write(*,*) 'ntot_failed_gl',                &
     &                               i_psf, ntot_failed_gl
      end do
!
      end subroutine set_node_and_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_and_patch_iso                                 &
     &         (num_iso, mesh, ele_mesh, psf_case_tbls,                 &
     &          iso_search, iso_list, iso_mesh)
!
      use m_geometry_constants
      use calypso_mpi
      use t_mesh_data
      use t_psf_geometry_list
      use t_psf_patch_data
      use t_psf_case_table
!
      use set_nodes_for_psf
      use set_patches_for_psf
!
      integer(kind = kint), intent(in) :: num_iso
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(psf_cases), intent(in) :: psf_case_tbls
!
      type(psf_search_lists), intent(inout) :: iso_search(num_iso)
      type(sectioning_list), intent(inout):: iso_list(num_iso)
      type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!
      integer(kind= kint) :: i_iso
!
!
      call count_nodes_4_iso                                            &
     &   (num_iso, ele_mesh%edge, iso_search, iso_list, iso_mesh)
!
      call set_nodes_4_iso                                              &
     &   (num_iso, mesh%node, ele_mesh%edge, ele_mesh%edge_comm,        &
     &    iso_search, iso_list, iso_mesh)
!
!
      do i_iso = 1, num_iso
        call count_iso_patches                                          &
     &     (mesh%node, mesh%ele, ele_mesh%edge, psf_case_tbls,          &
     &      iso_search(i_iso), iso_list(i_iso), iso_mesh(i_iso)%patch)
!
        call set_iso_patches(ele_mesh%edge, psf_case_tbls,              &
     &      iso_search(i_iso), iso_list(i_iso), iso_mesh(i_iso)%patch)
!
        call dealloc_mark_ele_psf(iso_search(i_iso))
      end do
!
      end subroutine set_node_and_patch_iso
!
!  ---------------------------------------------------------------------
!
      end module find_node_and_patch_psf
