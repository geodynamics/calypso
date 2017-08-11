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
!!      subroutine SECTIONING_initialize                                &
!!     &         (mesh, group, ele_mesh, nod_fld)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(phys_data), intent(in) :: nod_fld
!!      subroutine SECTIONING_visualize                                 &
!!     &         (istep_psf, time_d, ele_mesh, nod_fld)
!!        type(time_data), intent(in) :: time_d
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(phys_data), intent(in) :: nod_fld
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
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_psf_geometry_list
      use t_psf_patch_data
      use t_ucd_data
!
      use t_surface_group_connect
      use t_file_IO_parameter
!
      implicit  none
!
!>      Number of sections
      integer(kind = kint) :: num_psf
!
!>      Structure for table for sections
      type(sectioning_list), allocatable, save :: psf_list(:)
!>      Structure for table for sections
      type(grp_section_list), allocatable, save :: psf_grp_list(:)
!
!>      Structure for search table for sections
      type(psf_search_lists), allocatable, save :: psf_search(:)
!
      type(psf_parameters), allocatable, save :: psf_param(:)
!
!>      Structure for psf patch data on local domain
      type(psf_local_data), allocatable, save :: psf_mesh(:)
!
      type(time_data), save :: psf_time_IO
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
      subroutine SECTIONING_initialize                                  &
     &         (mesh, group, ele_mesh, nod_fld)
!
      use m_geometry_constants
      use m_control_data_sections
      use m_control_params_4_psf
!
      use calypso_mpi
      use set_psf_iso_control
      use search_ele_list_for_psf
      use set_const_4_sections
      use find_node_and_patch_psf
      use set_fields_for_psf
      use output_4_psf
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint) :: i_psf
!
!
      num_psf = num_psf_ctl
      if(num_psf .le. 0) return
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_control_params_4_psf'
      call allocate_control_params_4_psf(num_psf)
      do i_psf = 1, num_psf
        if (iflag_debug.eq.1) write(*,*) 'read_control_4_psf', i_psf
        call read_control_4_psf(i_psf)
      end do
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_psf_field_type'
      call alloc_psf_field_type
!
      call calypso_mpi_barrier
      if (iflag_debug.eq.1) write(*,*) 'set_psf_control'
      call set_psf_control(num_psf, group%ele_grp, group%surf_grp,      &
     &    nod_fld, psf_param, psf_mesh)
!
      call calypso_mpi_barrier
      if (iflag_debug.eq.1) write(*,*) 'set_search_mesh_list_4_psf'
      call set_search_mesh_list_4_psf                                   &
     &   (num_psf, mesh%node, mesh%ele, ele_mesh%surf, ele_mesh%edge,   &
     &    group%ele_grp, psf_param, psf_search)
!
!
      do i_psf = 1, num_psf
        call allocate_node_param_smp_type(psf_mesh(i_psf)%node)
        call allocate_ele_param_smp_type(psf_mesh(i_psf)%patch)
!
        call alloc_ref_field_4_psf(mesh%node%numnod, psf_list(i_psf))
      end do
!
      if (iflag_debug.eq.1) write(*,*) 'set_const_4_crossections'
      call set_const_4_crossections(num_psf, mesh%node, psf_list)
!
      if (iflag_debug.eq.1) write(*,*) 'set_node_and_patch_psf'
      call set_node_and_patch_psf                                       &
     &   (num_psf, mesh%node, mesh%ele, ele_mesh%edge, mesh%nod_comm,   &
     &    ele_mesh%edge_comm, group%surf_grp, group%surf_nod_grp,       &
     &    psf_search, psf_list, psf_grp_list, psf_mesh)
!
      call alloc_psf_field_data(num_psf, psf_mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'output_section_mesh'
      call output_section_mesh                                          &
     &   (num_psf, psf_file_IO, psf_mesh, psf_out, psf_out_m)
!
!      call calypso_mpi_barrier
!
      end subroutine SECTIONING_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine SECTIONING_visualize                                   &
     &         (istep_psf, time_d, ele_mesh, nod_fld)
!
      use m_control_params_4_psf
      use set_fields_for_psf
      use set_ucd_data_to_type
      use output_4_psf
!
      integer(kind = kint), intent(in) :: istep_psf
!
      type(time_data), intent(in) :: time_d
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
!
!
      if (num_psf.le.0 .or. istep_psf.le.0) return
!
!      call start_elapsed_time(20)
      call set_field_4_psf(num_psf, ele_mesh%edge, nod_fld,             &
     &    psf_param, psf_list, psf_grp_list, psf_mesh)
!      call end_elapsed_time(20)
!
!      call start_elapsed_time(21)
      if (iflag_debug.eq.1) write(*,*) 'output_section_mesh'
      call output_section_data(num_psf, psf_file_IO, istep_psf, time_d, &
     &    psf_mesh, psf_time_IO, psf_out, psf_out_m)
!
      end subroutine SECTIONING_visualize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_psf_field_type
!
!
      allocate(psf_mesh(num_psf))
      allocate(psf_list(num_psf))
      allocate(psf_grp_list(num_psf))
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
!
        call dealloc_inod_grp_psf(psf_grp_list(i_psf))
      end do
!
      call dealloc_psf_node_and_patch(num_psf, psf_list, psf_mesh)
      call dealloc_psf_field_name(num_psf, psf_mesh)
      call dealloc_psf_field_data(num_psf, psf_mesh)
!
      deallocate(psf_mesh, psf_list, psf_grp_list)
      deallocate(psf_search, psf_out, psf_out_m, psf_param)
!
      end subroutine dealloc_psf_field_type
!
!  ---------------------------------------------------------------------
!
      end module m_cross_section
