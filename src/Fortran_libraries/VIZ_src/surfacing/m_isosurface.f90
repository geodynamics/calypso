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
!!      subroutine ISOSURF_initialize                                   &
!!     &         (node, ele, surf, edge, ele_grp, nod_fld)
!!      subroutine ISOSURF_visualize                                    &
!!     &         (istep_iso, time_d, node, ele, edge, edge_comm, nod_fld)
!!        type(time_data), intent(in) :: time_d
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(group_data), intent(in) :: ele_grp
!!        type(communication_table), intent(in) :: edge_comm
!!        type(phys_data), intent(in) :: nod_fld
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
      use t_time_data
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
      type(time_data), save :: iso_time_IO
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
      subroutine ISOSURF_initialize                                     &
     &         (node, ele, surf, edge, ele_grp, nod_fld)
!
      use m_geometry_constants
      use m_control_data_sections
      use m_control_params_4_iso
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_phys_data
!
      use set_psf_iso_control
      use search_ele_list_for_psf
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      type(group_data), intent(in) :: ele_grp
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint) :: i_iso
!
!
      num_iso = num_iso_ctl
      if (num_iso .le. 0) return
!
      call alloc_iso_field_type
!
      if (iflag_debug.eq.1) write(*,*) 'set_iso_control'
      call set_iso_control(num_iso, ele_grp%num_grp, ele_grp%grp_name,  &
     &    nod_fld%num_phys, nod_fld%phys_name, iso_param, iso_mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'set_search_mesh_list_4_psf'
      call set_search_mesh_list_4_psf                                   &
     &   (num_iso, node%numnod, ele%numele, surf%numsurf,               &
     &    edge%numedge, edge%nnod_4_edge, edge%ie_edge, surf%isf_4_ele, &
     &    edge%iedge_4_sf, ele%interior_ele,                            &
     &    node%istack_nod_smp, ele%istack_ele_smp,                      &
     &    surf%istack_surf_smp, edge%istack_edge_smp,                   &
     &    ele_grp%num_grp, ele_grp%num_item, ele_grp%istack_grp,        &
     &    ele_grp%item_grp, iso_param, iso_search)
!
      do i_iso = 1, num_iso
        call allocate_node_param_smp_type(iso_mesh(i_iso)%node)
        call allocate_ele_param_smp_type(iso_mesh(i_iso)%patch)
!
        call alloc_ref_field_4_psf(node%numnod, iso_list(i_iso))
      end do
!
      end subroutine ISOSURF_initialize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine ISOSURF_visualize                                      &
     &         (istep_iso, time_d, node, ele, edge, edge_comm, nod_fld)
!
!
      use m_geometry_constants
      use m_control_params_4_iso
      use t_time_data
      use t_comm_table
      use t_edge_data
      use t_phys_data
      use t_ucd_data
!
      use set_const_4_sections
      use find_node_and_patch_psf
      use set_fields_for_psf
      use output_4_psf
!
      integer(kind = kint), intent(in) :: istep_iso
!
      type(time_data), intent(in) :: time_d
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(communication_table), intent(in) :: edge_comm
      type(phys_data), intent(in) :: nod_fld
!
!
      if (num_iso.le.0 .or. istep_iso.le.0) return
!
      if (iflag_debug.eq.1) write(*,*) 'set_const_4_isosurfaces'
      call set_const_4_isosurfaces                                      &
     &   (num_iso, node%numnod, node%istack_nod_smp,                    &
     &    node%xx, node%rr, node%a_r, node%ss, node%a_s,                &
     &    nod_fld%num_phys, nod_fld%ntot_phys,                          &
     &    nod_fld%istack_component, nod_fld%d_fld, iso_list)
!
      if (iflag_debug.eq.1) write(*,*) 'set_node_and_patch_iso'
      call set_node_and_patch_iso                                       &
     &   (num_iso, node%numnod, ele%numele, edge%numedge,               &
     &    ele%nnod_4_ele, edge%nnod_4_edge, node%xx, ele%ie,            &
     &    edge%ie_edge, edge%interior_edge, edge%iedge_4_ele,           &
     &    edge_comm, iso_search, iso_list, iso_mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'set_field_4_iso'
      call alloc_psf_field_data(num_iso, iso_mesh)
      call set_field_4_iso(num_iso, node%numnod,                        &
     &   edge%numedge, edge%nnod_4_edge, edge%ie_edge,                  &
     &    nod_fld%num_phys, nod_fld%ntot_phys,                          &
     &    nod_fld%istack_component, nod_fld%d_fld,                      &
     &    iso_param, iso_list, iso_mesh)
!
      call output_isosurface(num_iso, iso_file_IO, istep_iso,           &
     &    time_d, iso_mesh, iso_time_IO, iso_out, iso_out_m)
!
      call dealloc_psf_field_data(num_iso, iso_mesh)
      call dealloc_psf_node_and_patch(num_iso, iso_list, iso_mesh)
!
      end subroutine ISOSURF_visualize
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
