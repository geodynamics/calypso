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
!!     &          ele_grp, num_nod_phys, phys_nod_name)
!!        type(group_data), intent(in) :: ele_grp
!!
!!      subroutine isosurface_main(istep_iso,                           &
!!     &          numnod, numele, numedge, nnod_4_ele,                  &
!!     &          nnod_4_edge, ie, ie_edge, interior_edge, iedge_4_ele, &
!!     &          xx, radius, a_radius, s_cylinder, a_s_cylinder,       &
!!     &          inod_smp_stack, edge_comm, num_nod_phys,              &
!!     &          num_tot_nod_phys, istack_nod_component, d_nod)
!!
!!      subroutine dealloc_iso_field_type
!!@endverbatim
!
      module m_isosurface
!
      use m_precision
      use t_mesh_data
      use t_phys_data
      use t_psf_geometry_list
      use t_psf_patch_data
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
!>      Structure for table for sections
      type(sectioning_list), allocatable, save :: iso_list(:)
!
!>      Structure for search table for sections
      type(psf_search_lists), allocatable, save :: iso_search(:)
!
      type(psf_parameters), allocatable, save :: iso_param(:)
!
!>      Structure for psf patch data on local domain
      type(psf_local_data), allocatable, save :: iso_mesh(:)
!
!>      Structure for isosurface output (used by master process)
      type(ucd_data), allocatable, save :: iso_out(:)
      type(merged_ucd_data), allocatable, save :: iso_out_m(:)
!
      private :: alloc_iso_field_type
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
     &          ele_grp, num_nod_phys, phys_nod_name)
!
      use m_geometry_constants
      use m_control_params_4_iso
!
      use t_group_data
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
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint) :: i_iso
!
!
      call alloc_iso_field_type
!
      if (iflag_debug.eq.1) write(*,*) 'set_iso_control'
      call set_iso_control(num_iso, ele_grp%num_grp, ele_grp%grp_name,  &
     &    num_nod_phys, phys_nod_name, iso_param, iso_mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'set_search_mesh_list_4_psf'
      call set_search_mesh_list_4_psf(num_iso,                          &
     &        numnod, numele, numsurf, numedge, nnod_4_edge, ie_edge,   &
     &        isf_4_ele, iedge_4_sf, interior_ele, inod_smp_stack,      &
     &        iele_smp_stack, isurf_smp_stack, iedge_smp_stack,         &
     &        ele_grp%num_grp, ele_grp%num_item, ele_grp%istack_grp,    &
     &        ele_grp%item_grp, iso_param, iso_search)
!
      do i_iso = 1, num_iso
        call allocate_node_param_smp_type(iso_mesh(i_iso)%node)
        call allocate_ele_param_smp_type(iso_mesh(i_iso)%patch)
!
        call alloc_ref_field_4_psf(numnod, iso_list(i_iso))
      end do
!
      end subroutine isosurface_init
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine isosurface_main(istep_iso,                             &
     &          numnod, numele, numedge, nnod_4_ele,                    &
     &          nnod_4_edge, ie, ie_edge, interior_edge, iedge_4_ele,   &
     &          xx, radius, a_radius, s_cylinder, a_s_cylinder,         &
     &          inod_smp_stack, edge_comm, num_nod_phys,                &
     &          num_tot_nod_phys, istack_nod_component, d_nod)
!
!
      use m_geometry_constants
      use m_control_params_4_iso
      use t_comm_table
!
      use set_const_4_sections
      use find_node_and_patch_psf
      use set_fields_for_psf
      use output_4_psf
!
      integer(kind = kint), intent(in) :: istep_iso
!
      integer(kind=kint), intent(in) :: numnod, numele, numedge
      integer(kind=kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind=kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind=kint), intent(in) :: interior_edge(numedge)
      integer(kind=kint), intent(in) :: iedge_4_ele(numele,nedge_4_ele)
!
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
      type(communication_table), intent(in) :: edge_comm
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
     &    nnod_4_edge, xx, ie, ie_edge, interior_edge, iedge_4_ele,     &
     &    edge_comm, iso_search, iso_list, iso_mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'set_field_4_iso'
      call alloc_psf_field_data(num_iso, iso_mesh)
      call set_field_4_iso                                              &
     &   (num_iso, numnod, numedge, nnod_4_edge, ie_edge,               &
     &    num_nod_phys, num_tot_nod_phys, istack_nod_component, d_nod,  &
     &    iso_param, iso_list, iso_mesh)
!
      call output_isosurface(num_iso, iso_header, itype_iso_file,       &
     &    istep_iso, iso_mesh, iso_out, iso_out_m)
!
      call dealloc_psf_field_data(num_iso, iso_mesh)
      call dealloc_psf_node_and_patch(num_iso, iso_list, iso_mesh)
!
      do i_iso = 1, num_iso
        call dealloc_inod_psf(iso_list(i_iso))
      end do
!
      end subroutine isosurface_main
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_iso_field_type
!
      use set_psf_iso_control
!
      call dealloc_psf_field_name(num_iso, iso_mesh)
!
      deallocate(iso_mesh, iso_list)
      deallocate(iso_search, iso_param)
      deallocate(iso_out, iso_out_m)
!
      end subroutine dealloc_iso_field_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_iso_field_type
!
!
      allocate(iso_mesh(num_iso))
      allocate(iso_list(num_iso))
      allocate(iso_search(num_iso))
      allocate(iso_param(num_iso))
!
      allocate(iso_out(num_iso))
      allocate(iso_out_m(num_iso))
!
      end subroutine alloc_iso_field_type
!
!  ---------------------------------------------------------------------
!
      end module m_isosurface
