!>@file   m_cross_section.f90
!!@brief  module m_cross_section
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!      subroutine cross_section_init                                   &
!!     &         (numnod, internal_node, numele, numsurf, numedge,      &
!!     &          nnod_4_ele, nnod_4_edge, ie, ie_edge, isf_4_ele,      &
!!     &          iedge_4_sf, iedge_4_ele, nod_comm, edge_comm,         &
!!     &          iedge_4_ele, nod_comm, edge_comm,                     &
!!     &          interior_ele, xx, inod_smp_stack, iele_smp_stack,     &
!!     &          isurf_smp_stack, iedge_smp_stack,                     &
!!     &          ele_grp, sf_grp, sf_grp_nod,                          &
!!     &          num_nod_phys, phys_nod_name)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(in) :: edge_comm
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!
!!
!!      subroutine cross_section_main(istep_psf, numnod, numedge,       &
!!     &          nnod_4_edge, ie_edge, num_nod_phys, num_tot_nod_phys, &
!!     &          istack_nod_component, d_nod)
!!
!!      subroutine dealloc_psf_field_type
!!@endverbatim
!
!
      module m_cross_section
!
      use calypso_mpi
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_phys_data
      use t_psf_geometry_list
      use t_psf_patch_data
      use t_ucd_data
!
      implicit  none
!
!>      Number of sections
      integer(kind = kint) :: num_psf
!
!>      Structure for table for sections
      type(sectioning_list), allocatable, save :: psf_list(:)
!
!>      Structure for search table for sections
      type(psf_search_lists), allocatable, save :: psf_search(:)
!
      type(psf_parameters), allocatable, save :: psf_param(:)
!
!>      Structure for psf patch data on local domain
      type(psf_local_data), allocatable, save :: psf_mesh(:)
!
!>      Structure for cross sectioning output (used by master process)
      type(ucd_data), allocatable, save :: psf_out(:)
      type(merged_ucd_data), allocatable, save :: psf_out_m(:)
!
      private :: alloc_psf_field_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cross_section_init                                     &
     &         (numnod, internal_node, numele, numsurf, numedge,        &
     &          nnod_4_ele, nnod_4_edge, ie, ie_edge, isf_4_ele,        &
     &          iedge_4_sf, iedge_4_ele, nod_comm, edge_comm,           &
     &          interior_ele, xx, inod_smp_stack, iele_smp_stack,       &
     &          isurf_smp_stack, iedge_smp_stack,                       &
     &          ele_grp, sf_grp, sf_grp_nod,                            &
     &          num_nod_phys, phys_nod_name)
!
!
      use m_geometry_constants
      use m_control_params_4_psf
!
      use calypso_mpi
      use t_comm_table
      use t_group_data
      use t_surface_group_connect
      use set_psf_iso_control
      use search_ele_list_for_psf
      use set_const_4_sections
      use find_node_and_patch_psf
      use set_fields_for_psf
      use output_4_psf
!
      integer(kind=kint), intent(in) :: numnod, internal_node, numele
      integer(kind=kint), intent(in) :: numsurf, numedge
      integer(kind=kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind=kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind=kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in)                                  &
     &              :: iedge_4_sf(numsurf,nedge_4_surf)
      integer(kind=kint), intent(in) :: iedge_4_ele(numele,nedge_4_ele)
      integer(kind=kint), intent(in) :: interior_ele(numele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: isurf_smp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: iedge_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: edge_comm
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
      integer(kind = kint) :: i_psf
!
!
      call alloc_psf_field_type
!
      call set_psf_control(num_psf, ele_grp%num_grp, ele_grp%grp_name,  &
     &    sf_grp%num_grp, sf_grp%grp_name,                              &
     &    num_nod_phys, phys_nod_name, psf_param, psf_mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'set_search_mesh_list_4_psf'
      call set_search_mesh_list_4_psf(num_psf,                          &
     &        numnod, numele, numsurf, numedge, nnod_4_edge, ie_edge,   &
     &        isf_4_ele, iedge_4_sf, interior_ele, inod_smp_stack,      &
     &        iele_smp_stack, isurf_smp_stack, iedge_smp_stack,         &
     &        ele_grp%num_grp, ele_grp%num_item, ele_grp%istack_grp,    &
     &        ele_grp%item_grp, psf_param, psf_search)
!
!
      do i_psf = 1, num_psf
        call allocate_node_param_smp_type(psf_mesh(i_psf)%node)
        call allocate_ele_param_smp_type(psf_mesh(i_psf)%patch)
!
        call alloc_ref_field_4_psf(numnod, psf_list(i_psf))
      end do
!
      if (iflag_debug.eq.1) write(*,*) 'set_const_4_crossections'
      call set_const_4_crossections                                     &
     &   (num_psf, numnod, inod_smp_stack, xx, psf_list)
!
      if (iflag_debug.eq.1) write(*,*) 'set_node_and_patch_psf'
      call set_node_and_patch_psf                                       &
     &   (num_psf, numnod, internal_node, numele, numedge, nnod_4_ele,  &
     &    nnod_4_edge, xx, ie, ie_edge, iedge_4_ele, nod_comm,          &
     &    edge_comm, sf_grp%num_grp, sf_grp%num_item,                   &
     &    sf_grp%istack_grp, sf_grp%item_sf_grp,                        &
     &    sf_grp_nod%ntot_node_sf_grp, sf_grp_nod%inod_stack_sf_grp,    &
     &    sf_grp_nod%inod_surf_grp, psf_search, psf_list, psf_mesh)
!
      call alloc_psf_field_data(num_psf, psf_mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'output_section_mesh'
      call output_section_mesh(num_psf, psf_header, itype_psf_file,     &
     &   psf_mesh, psf_out, psf_out_m)
!
!      call calypso_mpi_barrier
!
      end subroutine cross_section_init
!
!  ---------------------------------------------------------------------
!
      subroutine cross_section_main(istep_psf, numnod, numedge,         &
     &          nnod_4_edge, ie_edge, num_nod_phys, num_tot_nod_phys,   &
     &          istack_nod_component, d_nod)
!
      use m_control_params_4_psf
      use set_fields_for_psf
      use set_ucd_data_to_type
      use output_4_psf
!
      integer(kind = kint), intent(in) :: istep_psf
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind=kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_component(0:num_nod_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,num_tot_nod_phys)
!
!
!      call start_eleps_time(20)
      call set_field_4_psf(num_psf, numnod, numedge, nnod_4_edge,       &
     &    ie_edge, num_nod_phys, num_tot_nod_phys,                      &
     &    istack_nod_component, d_nod, psf_param, psf_list, psf_mesh)
!      call end_eleps_time(20)
!
!      call start_eleps_time(21)
      if (iflag_debug.eq.1) write(*,*) 'output_section_mesh'
      call output_section_data                                          &
     &   (num_psf, istep_psf, psf_mesh, psf_out, psf_out_m)
!
      end subroutine cross_section_main
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_psf_field_type
!
!
      allocate(psf_mesh(num_psf))
      allocate(psf_list(num_psf))
      allocate(psf_search(num_psf))
      allocate(psf_param(num_psf))
!
      allocate( psf_out(num_psf) )
      allocate( psf_out_m(num_psf) )
!
      end subroutine alloc_psf_field_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_psf_field_type
!
      use set_psf_iso_control
      use set_fields_for_psf
      use find_node_and_patch_psf
!
      integer(kind = kint) :: i_psf
!
      do i_psf = 1, num_psf
        call disconnect_merged_ucd_mesh                                 &
    &      (psf_out(i_psf), psf_out_m(i_psf))
      end do
!
      call dealloc_psf_node_and_patch(num_psf, psf_list, psf_mesh)
      call dealloc_psf_field_name(num_psf, psf_mesh)
      call dealloc_psf_field_data(num_psf, psf_mesh)
!
      deallocate(psf_mesh, psf_list)
      deallocate(psf_search, psf_out, psf_out_m, psf_param)
!
      end subroutine dealloc_psf_field_type
!
!  ---------------------------------------------------------------------
!
      end module m_cross_section
