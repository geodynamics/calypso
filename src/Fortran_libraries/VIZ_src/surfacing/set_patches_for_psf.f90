!set_patches_for_psf.f90
!      module set_patches_for_psf
!
!      Written by H. Matsui on June, 2006
!
!!      subroutine count_psf_patches(num_psf, numnod, numele, numedge,  &
!!     &          nnod_4_ele, ie, iedge_4_ele, num_surf_grp,            &
!!     &          istack_surf_grp, psf_search, psf_list, psf_mesh)
!!      subroutine count_iso_patches(num_iso, numnod, numele, numedge,  &
!!     &          nnod_4_ele, ie, iedge_4_ele,                          &
!!     &          iso_search, iso_list, iso_mesh)
!!
!!      subroutine set_psf_patches(num_psf, numele, numedge, nnod_4_ele,&
!!     &          ie, iedge_4_ele, num_surf_grp, ntot_surf_grp,         &
!!     &          istack_surf_grp, item_surf_grp,                       &
!!     &          psf_search, psf_list, psf_grp_list, psf_mesh)
!!      subroutine set_iso_patches(num_iso, numele, numedge,            &
!!     &          iedge_4_ele, iso_search, iso_list, iso_mesh)
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
     &          istack_surf_grp, psf_search, psf_list, psf_mesh)
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
      type(sectioning_list), intent(inout) :: psf_list(num_psf)
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_psf
        psf_mesh(i)%patch%istack_ele_smp(0) = 0
        call alloc_mark_ele_psf(psf_search(i))
!
        if( id_section_method(i) .gt. 0) then
          call set_psf_type_id(numnod, numele, nnod_4_ele, ie,          &
     &        psf_search(i)%elem_list, psf_search(i)%mark_e,            &
     &        psf_list(i)%ref_fld)
!
          call count_num_patch_4_psf(numele, numedge, iedge_4_ele,      &
     &        psf_search(i)%elem_list, psf_search(i)%mark_e,            &
     &        psf_list(i)%id_n_on_e, psf_mesh(i)%patch%istack_ele_smp)
!
        else if( id_section_method(i) .eq. 0) then
          call count_num_patch_4_grp(num_surf_grp, istack_surf_grp,     &
     &        id_psf_group(i), psf_mesh(i)%patch%istack_ele_smp)
!
        end if
        psf_mesh(i)%patch%numele                                        &
      &       = psf_mesh(i)%patch%istack_ele_smp(np_smp)
        psf_mesh(i)%patch%internal_ele = psf_mesh(i)%patch%numele
        psf_mesh(i)%patch%nnod_4_ele = num_triangle
      end do
!
      end subroutine count_psf_patches
!
!  ---------------------------------------------------------------------
!
      subroutine count_iso_patches(num_iso, numnod, numele, numedge,    &
     &          nnod_4_ele, ie, iedge_4_ele,                            &
     &          iso_search, iso_list, iso_mesh)
!
      use m_geometry_constants
      use m_control_params_4_iso
      use t_psf_geometry_list
      use t_psf_patch_data
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
      type(sectioning_list), intent(inout) :: iso_list(num_iso)
      type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_iso
        iso_mesh(i)%patch%istack_ele_smp(0) = 0
        call alloc_mark_ele_psf(iso_search(i))
!
        call set_psf_type_id(numnod, numele, nnod_4_ele, ie,            &
     &      iso_search(i)%elem_list, iso_search(i)%mark_e,              &
     &      iso_list(i)%ref_fld)
!
        call count_num_patch_4_psf(numele, numedge, iedge_4_ele,        &
     &    iso_search(i)%elem_list, iso_search(i)%mark_e,                &
     &    iso_list(i)%id_n_on_e, iso_mesh(i)%patch%istack_ele_smp)
        iso_mesh(i)%patch%numele                                        &
      &       = iso_mesh(i)%patch%istack_ele_smp(np_smp)
        iso_mesh(i)%patch%internal_ele = iso_mesh(i)%patch%numele
        iso_mesh(i)%patch%nnod_4_ele = num_triangle
      end do
!
      end subroutine count_iso_patches
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_psf_patches(num_psf, numele, numedge, nnod_4_ele,  &
     &          ie, iedge_4_ele, num_surf_grp, ntot_surf_grp,           &
     &          istack_surf_grp, item_surf_grp,                         &
     &          psf_search, psf_list, psf_grp_list, psf_mesh)
!
      use calypso_mpi
      use m_geometry_constants
      use m_control_params_4_psf
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use const_element_comm_tables
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
      type(psf_search_lists), intent(in) :: psf_search(num_psf)
      type(sectioning_list), intent(in) :: psf_list(num_psf)
      type(grp_section_list), intent(inout) :: psf_grp_list(num_psf)
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i
!
      do i = 1, num_psf
        call allocate_ele_connect_type(psf_mesh(i)%patch)
        call const_global_numele_list(psf_mesh(i)%patch)
!
        if( id_section_method(i) .gt. 0) then
!
          call set_patch_4_psf                                          &
     &       (numele, numedge, iedge_4_ele, psf_search(i)%elem_list,    &
     &        psf_search(i)%mark_e, psf_list(i)%id_n_on_e,              &
     &        psf_mesh(i)%patch%istack_numele(my_rank),                 &
     &        psf_mesh(i)%patch%numele,                                 &
     &        psf_mesh(i)%patch%istack_ele_smp,                         &
     &        psf_mesh(i)%patch%iele_global, psf_mesh(i)%patch%ie)
!
        else if( id_section_method(i) .eq. 0) then
!
          call set_patch_4_grp(numele, numele, nnod_4_ele, ie,          &
     &        num_surf_grp, ntot_surf_grp, istack_surf_grp,             &
     &        item_surf_grp, id_psf_group(i),                           &
     &        psf_grp_list(i)%id_n_on_n,                                &
     &        psf_mesh(i)%patch%istack_numele(my_rank),                 &
     &        psf_mesh(i)%patch%numele,                                 &
     &        psf_mesh(i)%patch%istack_ele_smp,                         &
     &        psf_mesh(i)%patch%iele_global, psf_mesh(i)%patch%ie)
!
        end if
!
        call dealloc_mark_ele_psf(psf_search(i))
      end do
!
      end subroutine set_psf_patches
!
!  ---------------------------------------------------------------------
!
      subroutine set_iso_patches(num_iso, numele, numedge,              &
     &          iedge_4_ele, iso_search, iso_list, iso_mesh)
!
      use calypso_mpi
      use m_geometry_constants
      use m_control_params_4_iso
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use const_element_comm_tables
      use patch_4_psf
!
      integer(kind = kint), intent(in) :: num_iso
      integer(kind = kint), intent(in) :: numele, numedge
      integer(kind = kint), intent(in)                                  &
     &     :: iedge_4_ele(numele,nedge_4_ele)
!
      type(psf_search_lists), intent(in) :: iso_search(num_iso)
      type(sectioning_list), intent(in) :: iso_list(num_iso)
      type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!
      integer(kind = kint) :: i
!
      do i = 1, num_iso
        call allocate_ele_connect_type(iso_mesh(i)%patch)
        call const_global_numele_list(iso_mesh(i)%patch)
!
        call set_patch_4_psf(numele, numedge, iedge_4_ele,              &
     &      iso_search(i)%elem_list, iso_search(i)%mark_e,              &
     &      iso_list(i)%id_n_on_e,                                      &
     &      iso_mesh(i)%patch%istack_numele(my_rank),                   &
     &      iso_mesh(i)%patch%numele, iso_mesh(i)%patch%istack_ele_smp, &
     &      iso_mesh(i)%patch%iele_global, iso_mesh(i)%patch%ie)
!
        call dealloc_mark_ele_psf(iso_search(i))
      end do
!
      end subroutine set_iso_patches
!
!  ---------------------------------------------------------------------
!
      end module set_patches_for_psf
