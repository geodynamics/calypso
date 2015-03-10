!>@file   m_isosurface.f90
!!@brief  module m_isosurface
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for isosurfacing
!!
!!@verbatim
!!      subroutine isosurface_init                                      &
!!     &         (numnod, numele, numsurf, numedge, nnod_4_edge,        &
!!     &          ie_edge, isf_4_ele, iedge_4_sf, interior_ele,         &
!!     &          inod_smp_stack, iele_smp_stack,                       &
!!     &          isurf_smp_stack, iedge_smp_stack,                     &
!!     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,  &
!!     &          num_nod_phys, phys_nod_name)
!!
!!      subroutine isosurface_main(istep_iso,                           &
!!     &          numnod, numele, numedge, nnod_4_ele, nnod_4_edge,     &
!!     &          ie, ie_edge, iedge_4_ele, inod_global,                &
!!     &          xx, radius, a_radius, s_cylinder, a_s_cylinder,       &
!!     &          inod_smp_stack, num_nod_phys, num_tot_nod_phys,       &
!!     &          istack_nod_component, d_nod)
!!
!!      subroutine dealloc_iso_field_type
!!      subroutine deallocate_num_patch_iso
!!@endverbatim
!
      module m_isosurface
!
      use m_precision
      use t_mesh_data
      use t_phys_data
      use t_psf_geometry_list
      use t_psf_patch_data
      use t_psf_outputs
      use t_ucd_data
!
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
!>      Number of isosurfaces
      integer(kind = kint) :: num_iso
!
!>      Structure for isosurface mesh
      type(mesh_geometry), allocatable, save :: iso_mesh(:)
!
!>      Structure for isosurface field
      type(phys_data), allocatable, save :: iso_fld(:)
!
!>      Structure for table for sections
      type(sectiong_list), allocatable, save :: iso_list(:)
!
!>      Structure for search table for sections
      type(psf_search_lists), allocatable, save :: iso_search(:)
!
      type(psf_parameters), allocatable, save :: iso_param(:)
!
!
      type(psf_patch_data), save :: iso_pat
      type(psf_collect_type), save :: iso_col
!
!>      Structure for isosurface output (used by master process)
      type(ucd_data), allocatable, save :: iso_out(:)
!
!
!>      End point of node list for each isosurfaces
      integer(kind = kint), allocatable :: istack_nod_iso(:)
      integer(kind = kint), allocatable :: istack_nod_iso_smp(:)
!
      integer(kind = kint), allocatable :: istack_patch_iso(:)
      integer(kind = kint), allocatable :: istack_patch_iso_smp(:)
!
      private :: alloc_iso_field_type, allocate_num_patch_iso
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine isosurface_init                                        &
     &         (numnod, numele, numsurf, numedge, nnod_4_edge,          &
     &          ie_edge, isf_4_ele, iedge_4_sf, interior_ele,           &
     &          inod_smp_stack, iele_smp_stack,                         &
     &          isurf_smp_stack, iedge_smp_stack,                       &
     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,    &
     &          num_nod_phys, phys_nod_name)
!
      use m_geometry_constants
      use m_control_params_4_iso
!
      use set_psf_iso_control
      use search_ele_list_for_psf
!
      integer(kind=kint), intent(in) :: numnod, numele
      integer(kind=kint), intent(in) :: numsurf, numedge
      integer(kind=kint), intent(in) :: nnod_4_edge
      integer(kind=kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind=kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind=kint), intent(in) :: iedge_4_sf(numsurf,nedge_4_surf)
      integer(kind=kint), intent(in) :: interior_ele(numele)
!
      integer(kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: isurf_smp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: iedge_smp_stack(0:np_smp)
!
      integer(kind=kint), intent(in) :: num_mat, num_mat_bc
      integer(kind=kint), intent(in) :: mat_istack(0:num_mat)
      integer(kind=kint), intent(in) :: mat_item(num_mat_bc)
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint) :: i_iso
!
!
      call alloc_iso_field_type(my_rank)
!
      if (iflag_debug.eq.1) write(*,*) 'set_iso_control'
      call set_iso_control(num_iso, num_mat, mat_name,                  &
     &    num_nod_phys, phys_nod_name, iso_param, iso_fld, iso_pat)
!
      if (iflag_debug.eq.1) write(*,*) 'set_search_mesh_list_4_psf'
      call set_search_mesh_list_4_psf(num_iso,                          &
     &        numnod, numele, numsurf, numedge, nnod_4_edge, ie_edge,   &
     &        isf_4_ele, iedge_4_sf, interior_ele, inod_smp_stack,      &
     &        iele_smp_stack, isurf_smp_stack, iedge_smp_stack,         &
     &        num_mat, num_mat_bc, mat_istack, mat_item,                &
     &        iso_param, iso_search)
!
      do i_iso = 1, num_iso
        call alloc_ref_field_4_psf(numnod, iso_list(i_iso))
        call alloc_nnod_psf(np_smp, numnod, numedge, iso_list(i_iso))
      end do
      if (iflag_debug.eq.1) write(*,*) 'allocate_num_patch_iso'
      call allocate_num_patch_iso(np_smp)
!
      call alloc_psf_outputs_num(nprocs, num_iso, iso_col)
!
      end subroutine isosurface_init
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine isosurface_main(istep_iso,                             &
     &          numnod, numele, numedge, nnod_4_ele, nnod_4_edge,       &
     &          ie, ie_edge, iedge_4_ele, inod_global,                  &
     &          xx, radius, a_radius, s_cylinder, a_s_cylinder,         &
     &          inod_smp_stack, num_nod_phys, num_tot_nod_phys,         &
     &          istack_nod_component, d_nod)
!
!
      use m_geometry_constants
      use m_control_params_4_iso
!
      use set_const_4_sections
      use find_node_and_patch_psf
      use set_fields_for_psf
      use collect_psf_data
!
      integer(kind = kint), intent(in) :: istep_iso
!
      integer(kind=kint), intent(in) :: numnod, numele, numedge
      integer(kind=kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind=kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind=kint), intent(in) :: iedge_4_ele(numele,nedge_4_ele)
!
      integer(kind=kint_gl), intent(in) :: inod_global(numnod)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer(kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_component(0:num_nod_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,num_tot_nod_phys)
!
      integer(kind = kint) :: i_iso
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_const_4_isosurfaces'
      call set_const_4_isosurfaces(num_iso, numnod, inod_smp_stack,     &
     &    xx, radius, a_radius, s_cylinder, a_s_cylinder,               &
     &    num_nod_phys, num_tot_nod_phys, istack_nod_component,         &
     &    d_nod, iso_list)
!
      if (iflag_debug.eq.1) write(*,*) 'set_node_and_patch_iso'
      call set_node_and_patch_iso                                       &
     &   (num_iso, numnod, numele, numedge, nnod_4_ele,                 &
     &    nnod_4_edge, inod_global, xx, ie, ie_edge, iedge_4_ele,       &
     &    istack_nod_iso_smp, istack_patch_iso_smp,                     &
     &    iso_search, iso_list, iso_pat)
!
      call alloc_dat_on_patch_psf(iso_pat)
!
      if (iflag_debug.eq.1) write(*,*) 'set_field_4_iso'
      call set_field_4_iso                                              &
     &   (num_iso, numnod, numedge, nnod_4_edge, ie_edge,               &
     &    istack_nod_iso_smp, num_nod_phys, num_tot_nod_phys,           &
     &    istack_nod_component, d_nod, iso_param, iso_fld, iso_list,    &
     &    iso_pat)
!
      do i_iso = 1, num_iso
        call dealloc_inod_psf(iso_list(i_iso))
      end do
!
      if (iflag_debug.eq.1) write(*,*) 'collect_numbers_4_psf'
      call collect_numbers_4_psf(num_iso, iso_header, itype_iso_file,   &
     &    istack_nod_iso_smp, istack_patch_iso_smp,                     &
     &    iso_fld, iso_col, iso_out)
!
!
      call alloc_psf_outputs_data(iso_col)
      call alloc_SR_array_psf(my_rank, nprocs,                          &
     &    iso_pat%nnod_psf_tot, iso_pat%npatch_tot, iso_col)
!
!
      if (iflag_debug.eq.1) write(*,*) 'collect_data_4_iso'
      call collect_mesh_4_psf(num_iso, iso_pat, iso_col, iso_out)
      call collect_field_4_psf(num_iso, iso_pat, iso_col, iso_out)
!
      if (iflag_debug.eq.1) write(*,*) 'output_iso_ucds'
      call output_iso_ucds(num_iso, istep_iso, iso_out)
!
!
      call dealloc_SR_array_psf(my_rank,iso_col)
      call dealloc_psf_outputs_data(iso_col)
      call deallocate_psf_outputs_data(my_rank, num_iso, iso_out)
      call dealloc_dat_on_patch_psf(iso_pat)
      call dealloc_position_psf(iso_pat)
      call dealloc_patch_data_psf(iso_pat)
!
      end subroutine isosurface_main
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_iso_field_type
!
!
      deallocate(iso_mesh, iso_fld, iso_list)
      deallocate( iso_search, iso_out, iso_param)
!
      end subroutine dealloc_iso_field_type
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_num_patch_iso
!
      deallocate(istack_nod_iso,   istack_nod_iso_smp)
      deallocate(istack_patch_iso, istack_patch_iso_smp)
!
      end subroutine deallocate_num_patch_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_iso_field_type(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      allocate(iso_mesh(num_iso))
      allocate(iso_fld(num_iso))
      allocate(iso_list(num_iso))
      allocate(iso_search(num_iso))
      allocate(iso_param(num_iso))
!
      if(my_rank .eq. 0) then
        allocate( iso_out(num_iso) )
      else
        allocate( iso_out(0) )
      end if
!
      end subroutine alloc_iso_field_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_num_patch_iso(np_smp)
!
      integer(kind= kint), intent(in) :: np_smp
!
      allocate(istack_nod_iso(0:num_iso))
      allocate(istack_patch_iso(0:num_iso))
      allocate(istack_nod_iso_smp(0:np_smp*num_iso))
      allocate(istack_patch_iso_smp(0:np_smp*num_iso))
!
      istack_nod_iso = 0
      istack_patch_iso = 0
      istack_nod_iso_smp = 0
      istack_patch_iso_smp = 0
!
      end subroutine allocate_num_patch_iso
!
!  ---------------------------------------------------------------------
!
      end module m_isosurface
