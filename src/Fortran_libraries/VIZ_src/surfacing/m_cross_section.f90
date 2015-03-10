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
!!      subroutine cross_section_init(numnod, numele, numsurf, numedge, &
!!     &          nnod_4_ele, nnod_4_edge, ie, ie_edge,                 &
!!     &          isf_4_ele, iedge_4_sf, iedge_4_ele,                   &
!!     &          interior_ele, inod_global, xx,                        &
!!     &          inod_smp_stack, iele_smp_stack,                       &
!!     &          isurf_smp_stack, iedge_smp_stack,                     &
!!     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,  &
!!     &          num_surf, num_surf_bc, surf_name, surf_istack,        &
!!     &          surf_item, ntot_node_sf_grp, inod_stack_sf_grp,       &
!!     &          inod_surf_grp, num_nod_phys, phys_nod_name)
!!
!!      subroutine cross_section_main(istep_psf, numnod, numedge,       &
!!     &          nnod_4_edge, ie_edge, num_nod_phys, num_tot_nod_phys, &
!!     &          istack_nod_component, d_nod)
!!
!!      subroutine dealloc_psf_field_type
!!      subroutine deallocate_num_patch_psf
!!@endverbatim
!
!
      module m_cross_section
!
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
      use t_psf_outputs
      use t_ucd_data
!
      implicit  none
!
!>      Number of sections
      integer(kind = kint) :: num_psf
!
!>      Structure for isosurface mesh
      type(mesh_geometry), allocatable, save :: psf_mesh(:)
!
!>      Structure for sectioned field
      type(phys_data), allocatable, save :: psf_fld(:)
!
!>      Structure for table for sections
      type(sectiong_list), allocatable, save :: psf_list(:)
!
!>      Structure for search table for sections
      type(psf_search_lists), allocatable, save :: psf_search(:)
!
      type(psf_parameters), allocatable, save :: psf_param(:)
!
      type(psf_patch_data), save :: psf_pat
      type(psf_collect_type), save :: psf_col
!
!>      Structure for cross sectioning output (used by master process)
      type(ucd_data), allocatable, save :: psf_out(:)
!
!
      integer(kind = kint), allocatable :: istack_nod_psf(:)
      integer(kind = kint), allocatable :: istack_nod_psf_smp(:)
!
      integer(kind = kint), allocatable :: istack_patch_psf(:)
      integer(kind = kint), allocatable :: istack_patch_psf_smp(:)
!
      private :: alloc_psf_field_type, allocate_num_patch_psf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cross_section_init(numnod, numele, numsurf, numedge,   &
     &          nnod_4_ele, nnod_4_edge, ie, ie_edge,                   &
     &          isf_4_ele, iedge_4_sf, iedge_4_ele,                     &
     &          interior_ele, inod_global, xx,                          &
     &          inod_smp_stack, iele_smp_stack,                         &
     &          isurf_smp_stack, iedge_smp_stack,                       &
     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,    &
     &          num_surf, num_surf_bc, surf_name, surf_istack,          &
     &          surf_item, ntot_node_sf_grp, inod_stack_sf_grp,         &
     &          inod_surf_grp, num_nod_phys, phys_nod_name)
!
!
      use m_geometry_constants
      use m_control_params_4_psf
!
      use set_psf_iso_control
      use search_ele_list_for_psf
      use set_const_4_sections
      use find_node_and_patch_psf
      use collect_psf_data
!
      integer(kind=kint), intent(in) :: numnod, numele
      integer(kind=kint), intent(in) :: numsurf, numedge
      integer(kind=kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind=kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind=kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in)                                  &
     &              :: iedge_4_sf(numsurf,nedge_4_surf)
      integer(kind=kint), intent(in) :: iedge_4_ele(numele,nedge_4_ele)
      integer(kind=kint), intent(in) :: interior_ele(numele)
      integer(kind=kint_gl), intent(in) :: inod_global(numnod)
      real(kind = kreal), intent(in) :: xx(numnod,3)
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
      integer(kind=kint), intent(in) :: num_surf, num_surf_bc
      integer(kind=kint), intent(in) :: surf_istack(0:num_surf)
      integer(kind=kint), intent(in) :: surf_item(2,num_surf_bc)
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      integer(kind=kint), intent(in) :: ntot_node_sf_grp
      integer(kind=kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      integer(kind=kint), intent(in) :: inod_surf_grp(ntot_node_sf_grp)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint) :: i_psf
!
!
      call alloc_psf_field_type(my_rank)
!
      call set_psf_control                                              &
     &   (num_psf, num_mat, mat_name, num_surf, surf_name,              &
     &    num_nod_phys, phys_nod_name, psf_param, psf_fld, psf_pat)
!
      if (iflag_debug.eq.1) write(*,*) 'set_search_mesh_list_4_psf'
      call set_search_mesh_list_4_psf(num_psf,                          &
     &        numnod, numele, numsurf, numedge, nnod_4_edge, ie_edge,   &
     &        isf_4_ele, iedge_4_sf, interior_ele, inod_smp_stack,      &
     &        iele_smp_stack, isurf_smp_stack, iedge_smp_stack,         &
     &        num_mat, num_mat_bc, mat_istack, mat_item,                &
     &        psf_param, psf_search)
!
!
      do i_psf = 1, num_psf
        call alloc_ref_field_4_psf(numnod, psf_list(i_psf))
        call alloc_nnod_psf(np_smp, numnod, numedge, psf_list(i_psf))
      end do
      call allocate_num_patch_psf(np_smp)
!
      if (iflag_debug.eq.1) write(*,*) 'set_const_4_crossections'
      call set_const_4_crossections                                     &
     &   (num_psf, numnod, inod_smp_stack, xx, psf_list)
!
      if (iflag_debug.eq.1) write(*,*) 'set_node_and_patch_psf'
      call set_node_and_patch_psf                                       &
     &   (num_psf, numnod, numele, numedge, nnod_4_ele,                 &
     &    nnod_4_edge, inod_global, xx, ie, ie_edge, iedge_4_ele,       &
     &    num_surf, num_surf_bc, surf_istack, surf_item,                &
     &    ntot_node_sf_grp, inod_stack_sf_grp, inod_surf_grp,           &
     &    istack_nod_psf_smp, istack_patch_psf_smp, psf_search,         &
     &    psf_list, psf_pat)
!
      call alloc_dat_on_patch_psf(psf_pat)
      call alloc_psf_outputs_num(nprocs, num_psf, psf_col)
!
      if (iflag_debug.eq.1) write(*,*) 'collect_numbers_4_psf'
      call collect_numbers_4_psf(num_psf, psf_header, itype_psf_file,   &
     &    istack_nod_psf_smp, istack_patch_psf_smp,                     &
     &    psf_fld, psf_col, psf_out)
!
      call alloc_psf_outputs_data(psf_col)
      call alloc_SR_array_psf(my_rank, psf_pat%max_ncomp_psf,           &
     &    psf_pat%nnod_psf_tot, psf_pat%npatch_tot, psf_col)
!
      if (iflag_debug.eq.1) write(*,*) 'collect_mesh_4_psf'
      call collect_mesh_4_psf(num_psf, psf_pat, psf_col, psf_out)
!
      if (iflag_debug.eq.1) write(*,*) 'output_psf_grids'
      call output_psf_grids(num_psf, psf_out)
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
      use collect_psf_data
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
      if (iflag_debug.eq.1) write(*,*) 'set_field_4_psf'
      call set_field_4_psf(num_psf, numnod, numedge, nnod_4_edge,       &
     &    ie_edge, istack_nod_psf_smp, num_nod_phys, num_tot_nod_phys,  &
     &    istack_nod_component, d_nod, psf_param, psf_fld, psf_list,    &
     &    psf_pat)
!      call end_eleps_time(20)
!
!      call start_eleps_time(21)
      if (iflag_debug.eq.1) write(*,*) 'collect_field_4_psf'
      call collect_field_4_psf(num_psf, psf_pat, psf_col, psf_out)
!      call end_eleps_time(21)
!
!      call start_eleps_time(22)
      if (iflag_debug.eq.1) write(*,*) 'output_psf_fields'
      call output_psf_fields(num_psf, istep_psf, psf_out)
!      call end_eleps_time(22)
!
      end subroutine cross_section_main
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_psf_field_type(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      allocate(psf_mesh(num_psf))
      allocate(psf_fld(num_psf))
      allocate(psf_list(num_psf))
      allocate(psf_search(num_psf))
      allocate(psf_param(num_psf))
!
      if(my_rank .eq. 0) then
        allocate( psf_out(num_psf) )
      else
        allocate( psf_out(0) )
      end if
!
      end subroutine alloc_psf_field_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_psf_field_type
!
!
      deallocate(psf_mesh, psf_fld, psf_list)
      deallocate(psf_search, psf_out, psf_param)
!
      end subroutine dealloc_psf_field_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_num_patch_psf
!
      deallocate(istack_nod_psf, istack_nod_psf_smp)
      deallocate(istack_patch_psf, istack_patch_psf_smp)
!
      end subroutine deallocate_num_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_num_patch_psf(np_smp)
!
      integer(kind= kint), intent(in) :: np_smp
!
      allocate(istack_nod_psf(0:num_psf))
      allocate(istack_patch_psf(0:num_psf))
      allocate(istack_nod_psf_smp(0:np_smp*num_psf))
      allocate(istack_patch_psf_smp(0:np_smp*num_psf))
!
      istack_nod_psf = 0
      istack_patch_psf = 0
      istack_nod_psf_smp = 0
      istack_patch_psf_smp = 0
!
      end subroutine allocate_num_patch_psf
!
!  ---------------------------------------------------------------------
!
      end module m_cross_section
