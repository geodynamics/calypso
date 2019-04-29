!>@file   set_patches_for_psf.f90
!!@brief  module set_patches_for_psf
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!> @brief count and set surface patch
!!
!!@verbatim
!!      subroutine count_psf_patches                                    &
!!     &         (node, ele, edge, sf_grp, psf_case_tbls,               &
!!     &          psf_def, psf_search, psf_list, psf_mesh, ntot_failed)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(psf_cases), intent(in) :: psf_case_tbls
!!        type(section_define), intent(in) :: psf_def
!!        type(psf_search_lists), intent(inout) :: psf_search
!!        type(sectioning_list), intent(inout) :: psf_list
!!        type(psf_local_data), intent(inout) :: psf_mesh
!!      subroutine count_iso_patches(node, ele, edge,  psf_case_tbls,   &
!!     &         iso_search, iso_list, iso_patch)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(psf_cases), intent(in) :: psf_case_tbls
!!        type(psf_search_lists), intent(inout) :: iso_search
!!        type(sectioning_list), intent(inout) :: iso_list
!!        type(psf_local_data), intent(inout) :: iso_mesh
!!
!!      subroutine set_psf_patches                                      &
!!     &         (ele, edge, sf_grp, psf_case_tbls, psf_def,            &
!!     &          psf_search, psf_list, psf_grp_list, psf_patch)
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(psf_cases), intent(in) :: psf_case_tbls
!!        type(section_define), intent(in) :: psf_def
!!        type(psf_search_lists), intent(in) :: psf_search
!!        type(sectioning_list), intent(in) :: psf_list
!!        type(grp_section_list), intent(inout) :: psf_grp_list
!!        type(psf_local_data), intent(inout) :: psf_mesh
!!      subroutine set_iso_patches                                      &
!!     &         (edge, psf_case_tbls, iso_search, iso_list, iso_patch)
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(psf_cases), intent(in) :: psf_case_tbls
!!        type(psf_search_lists), intent(in) :: iso_search
!!        type(sectioning_list), intent(in) :: iso_list
!!        type(psf_local_data), intent(inout) :: iso_mesh
!!@endverbatim
!
      module set_patches_for_psf
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use t_psf_case_table
      use t_geometry_data
      use t_edge_data
      use t_psf_geometry_list
      use t_psf_patch_data
!
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_psf_patches                                      &
     &         (node, ele, edge, sf_grp, psf_case_tbls,                 &
     &          psf_def, psf_search, psf_list, psf_patch, ntot_failed)
!
      use t_group_data
      use t_control_params_4_psf
!
      use set_psf_patch_4_by_surf_grp
      use patch_4_psf
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(surface_group_data), intent(in) :: sf_grp
!
      type(psf_cases), intent(in) :: psf_case_tbls
      type(section_define), intent(in) :: psf_def
!
      type(psf_search_lists), intent(inout) :: psf_search
      type(sectioning_list), intent(inout) :: psf_list
      type(element_data), intent(inout) :: psf_patch
      integer(kind = kint), intent(inout) :: ntot_failed
!
!
      psf_patch%istack_ele_smp(0) = 0
      call alloc_mark_ele_psf(psf_search)
!
      if(psf_def%id_section_method .gt. 0) then
        call set_psf_type_id(ele, node%numnod,                          &
     &      psf_search%elem_list, psf_search%mark_e, psf_list%ref_fld)
!
        call count_num_patch_4_psf                                      &
     &     (edge, psf_list, psf_search%elem_list, psf_search%mark_e,    &
     &      psf_case_tbls%num_case_tbl, psf_case_tbls%psf_case_tbl,     &
     &      psf_patch%istack_ele_smp, ntot_failed)
!
      else if(psf_def%id_section_method .eq. 0) then
        call count_num_patch_4_grp(sf_grp, psf_def%id_psf_group,        &
     &      psf_patch%istack_ele_smp)
!
      end if
      psf_patch%numele = psf_patch%istack_ele_smp(np_smp)
      psf_patch%internal_ele = psf_patch%numele
      psf_patch%nnod_4_ele = num_triangle
!
      end subroutine count_psf_patches
!
!  ---------------------------------------------------------------------
!
      subroutine count_iso_patches(node, ele, edge,  psf_case_tbls,     &
     &         iso_search, iso_list, iso_patch)
!
      use patch_4_psf
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(psf_cases), intent(in) :: psf_case_tbls
!
      type(psf_search_lists), intent(inout) :: iso_search
      type(sectioning_list), intent(inout) :: iso_list
      type(element_data), intent(inout) :: iso_patch
!
      integer(kind = kint) :: ntot_failed
!
!
        iso_patch%istack_ele_smp(0) = 0
        call alloc_mark_ele_psf(iso_search)
!
        call set_psf_type_id(ele, node%numnod,                          &
     &      iso_search%elem_list, iso_search%mark_e, iso_list%ref_fld)
!
        call count_num_patch_4_psf(edge, iso_list,                      &
     &      iso_search%elem_list, iso_search%mark_e,                    &
     &      psf_case_tbls%num_case_tbl, psf_case_tbls%psf_case_tbl,     &
     &      iso_patch%istack_ele_smp, ntot_failed)
        iso_patch%numele  = iso_patch%istack_ele_smp(np_smp)
        iso_patch%internal_ele = iso_patch%numele
        iso_patch%nnod_4_ele = num_triangle
!
      end subroutine count_iso_patches
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_psf_patches                                        &
     &         (ele, edge, sf_grp, psf_case_tbls, psf_def,              &
     &          psf_search, psf_list, psf_grp_list, psf_patch)
!
      use calypso_mpi
      use t_group_data
      use t_control_params_4_psf
!
      use const_element_comm_tables
      use set_psf_patch_4_by_surf_grp
      use patch_4_psf
      use transfer_to_long_integers
!
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(surface_group_data), intent(in) :: sf_grp
!
      type(psf_cases), intent(in) :: psf_case_tbls
      type(section_define), intent(in) :: psf_def
      type(psf_search_lists), intent(in) :: psf_search
      type(sectioning_list), intent(in) :: psf_list
      type(grp_section_list), intent(in) :: psf_grp_list
!
      type(element_data), intent(inout) :: psf_patch
!
      type(tmp_i8_2darray) :: eletmp
!
      call allocate_ele_connect_type(psf_patch)
      call const_global_numele_list(psf_patch)
!
      call alloc_2d_i8array(cast_long(psf_patch%numele),                &
     &    cast_long(num_triangle), eletmp)
      if(psf_def%id_section_method .gt. 0) then
        call set_patch_4_psf                                            &
     &     (edge, psf_list, psf_search%elem_list, psf_search%mark_e,    &
     &      psf_case_tbls%num_case_tbl, psf_case_tbls%psf_case_tbl,     &
     &      psf_patch%istack_numele(my_rank), psf_patch%numele,         &
     &      psf_patch%istack_ele_smp, psf_patch%iele_global,            &
     &     eletmp%id_da)
!
      else if(psf_def%id_section_method .eq. 0) then
!
        call set_patch_4_grp                                            &
     &     (ele, sf_grp, psf_grp_list, psf_def%id_psf_group,            &
     &      psf_patch%istack_numele(my_rank), psf_patch%numele,         &
     &      psf_patch%istack_ele_smp, psf_patch%iele_global,            &
     &      eletmp%id_da)
      end if
      call dup_to_short_darray(eletmp, psf_patch%ie)
!
      end subroutine set_psf_patches
!
!  ---------------------------------------------------------------------
!
      subroutine set_iso_patches                                        &
     &         (edge, psf_case_tbls, iso_search, iso_list, iso_patch)
!
      use calypso_mpi
!
      use const_element_comm_tables
      use patch_4_psf
      use transfer_to_long_integers
!
      type(edge_data), intent(in) :: edge
!
      type(psf_cases), intent(in) :: psf_case_tbls
      type(psf_search_lists), intent(in) :: iso_search
      type(sectioning_list), intent(in) :: iso_list
      type(element_data), intent(inout) :: iso_patch
!
      type(tmp_i8_2darray) :: eletmp
!
!
      call allocate_ele_connect_type(iso_patch)
      call const_global_numele_list(iso_patch)
!
      call alloc_2d_i8array(cast_long(iso_patch%numele),                &
     &    cast_long(num_triangle), eletmp)
      call set_patch_4_psf                                              &
     &   (edge, iso_list, iso_search%elem_list, iso_search%mark_e,      &
     &    psf_case_tbls%num_case_tbl, psf_case_tbls%psf_case_tbl,       &
     &    iso_patch%istack_numele(my_rank),                             &
     &    iso_patch%numele, iso_patch%istack_ele_smp,                   &
     &    iso_patch%iele_global, eletmp%id_da)
      call dup_to_short_darray(eletmp, iso_patch%ie)
!
      end subroutine set_iso_patches
!
!  ---------------------------------------------------------------------
!
      end module set_patches_for_psf
