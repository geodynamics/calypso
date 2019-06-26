!>@file   t_cross_section.f90
!!@brief  module t_cross_section
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!      subroutine SECTIONING_initialize(fem, nod_fld, psf_ctls, psf)
!!        type(mesh_data), intent(in) :: fem
!!        type(phys_data), intent(in) :: nod_fld
!!      subroutine SECTIONING_visualize                                 &
!!     &         (istep_psf, time_d, fem, nod_fld, psf)
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: fem
!!        type(phys_data), intent(in) :: nod_fld
!!      subroutine dealloc_psf_field_type(psf)
!!@endverbatim
!
!
      module t_cross_section
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
      use t_psf_case_table
      use t_surface_group_connect
      use t_file_IO_parameter
      use t_control_params_4_psf
      use t_control_data_sections
!
      implicit  none
!
      type sectioning_module
!>        Number of sections
        integer(kind = kint) :: num_psf = 0
!
!>        Structure of case table for isosurface
        type(psf_cases) :: psf_case_tbls
!
!>        Structure for table for sections
        type(sectioning_list), allocatable :: psf_list(:)
!>        Structure for table for sections
        type(grp_section_list), allocatable :: psf_grp_list(:)
!
!>        Structure for search table for sections
        type(psf_search_lists), allocatable :: psf_search(:)
!
!>        Structure of sectioning module parameter
        type(psf_parameters), allocatable :: psf_param(:)
!>        Structure of cross sectioning parameter
        type(section_define), allocatable  :: psf_def(:)
!
!>        Structure for psf patch data on local domain
        type(psf_local_data), allocatable :: psf_mesh(:)
!
!>        Structure for psf time output
        type(time_data) :: psf_time_IO
!>        Structure for psf data output
        type(field_IO_params), allocatable :: psf_file_IO(:)
!
!>        Structure for cross sectioning output (used by master process)
        type(ucd_data), allocatable :: psf_out(:)
        type(merged_ucd_data), allocatable :: psf_out_m(:)
      end type sectioning_module
!
      private :: alloc_psf_field_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine SECTIONING_initialize(fem, nod_fld, psf_ctls, psf)
!
      use m_geometry_constants
!
      use calypso_mpi
      use set_psf_iso_control
      use search_ele_list_for_psf
      use set_const_4_sections
      use find_node_and_patch_psf
      use set_fields_for_psf
      use output_4_psf
!
      type(mesh_data), intent(in) :: fem
      type(phys_data), intent(in) :: nod_fld
!
      type(section_controls), intent(inout) :: psf_ctls
      type(sectioning_module), intent(inout) :: psf
!
      integer(kind = kint) :: i_psf
!
!
      psf%num_psf = psf_ctls%num_psf_ctl
      if(psf%num_psf .le. 0) return
!
      call init_psf_case_tables(psf%psf_case_tbls)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_psf_field_type'
      call alloc_psf_field_type(psf)
!
      if (iflag_debug.eq.1) write(*,*) 'set_psf_control'
      call set_psf_control(psf%num_psf, fem%group, nod_fld,             &
     &    psf_ctls, psf%psf_param, psf%psf_def,                         &
     &    psf%psf_mesh, psf%psf_file_IO)
!
      if (iflag_debug.eq.1) write(*,*) 'set_search_mesh_list_4_psf'
      call set_search_mesh_list_4_psf(psf%num_psf, fem%mesh, fem%group, &
     &    psf%psf_param, psf%psf_search)
!
!
      do i_psf = 1, psf%num_psf
        call allocate_node_param_smp_type(psf%psf_mesh(i_psf)%node)
        call allocate_ele_param_smp_type(psf%psf_mesh(i_psf)%patch)
!
        call alloc_ref_field_4_psf(fem%mesh%node, psf%psf_list(i_psf))
      end do
!
      if (iflag_debug.eq.1) write(*,*) 'set_const_4_crossections'
      call set_const_4_crossections                                     &
     &   (psf%num_psf, psf%psf_def, fem%mesh%node, psf%psf_list)
!
      if (iflag_debug.eq.1) write(*,*) 'set_node_and_patch_psf'
      call set_node_and_patch_psf(psf%num_psf, fem%mesh, fem%group,     &
     &    psf%psf_case_tbls, psf%psf_def, psf%psf_search, psf%psf_list, &
     &    psf%psf_grp_list, psf%psf_mesh)
!
      call alloc_psf_field_data(psf%num_psf, psf%psf_mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'output_section_mesh'
      call output_section_mesh(psf%num_psf, psf%psf_file_IO,            &
     &    psf%psf_mesh, psf%psf_out, psf%psf_out_m)
!
      end subroutine SECTIONING_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine SECTIONING_visualize                                   &
     &         (istep_psf, time_d, fem, nod_fld, psf)
!
      use set_fields_for_psf
      use set_ucd_data_to_type
      use output_4_psf
!
      integer(kind = kint), intent(in) :: istep_psf
!
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: fem
      type(phys_data), intent(in) :: nod_fld
!
      type(sectioning_module), intent(inout) :: psf
!
!
      if (psf%num_psf.le.0 .or. istep_psf.le.0) return
!
      call set_field_4_psf(psf%num_psf, fem%mesh%edge, nod_fld,         &
     &    psf%psf_def, psf%psf_param, psf%psf_list, psf%psf_grp_list,   &
     &    psf%psf_mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'output_section_data'
      call output_section_data                                          &
     &   (psf%num_psf, psf%psf_file_IO, istep_psf, time_d,              &
     &    psf%psf_time_IO, psf%psf_out, psf%psf_out_m)
!
      end subroutine SECTIONING_visualize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_psf_field_type(psf)
!
      use m_field_file_format
!
      type(sectioning_module), intent(inout) :: psf
      integer(kind = kint) :: i_psf
!
!
      allocate(psf%psf_mesh(psf%num_psf))
      allocate(psf%psf_list(psf%num_psf))
      allocate(psf%psf_grp_list(psf%num_psf))
      allocate(psf%psf_search(psf%num_psf))
      allocate(psf%psf_param(psf%num_psf))
      allocate(psf%psf_def(psf%num_psf))
!
      allocate(psf%psf_file_IO(psf%num_psf))
      allocate(psf%psf_out(psf%num_psf))
      allocate(psf%psf_out_m(psf%num_psf))
!
      do i_psf = 1, psf%num_psf
        call alloc_coefficients_4_psf(psf%psf_def(i_psf))
      end do
!
      psf%psf_file_IO(1:psf%num_psf)%iflag_format = iflag_sgl_udt
!
      end subroutine alloc_psf_field_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_psf_field_type(psf)
!
      use set_psf_iso_control
      use set_fields_for_psf
      use find_node_and_patch_psf
!
      type(sectioning_module), intent(inout) :: psf
      integer(kind = kint) :: i_psf
!
      do i_psf = 1, psf%num_psf
        call disconnect_merged_ucd_mesh                                 &
    &      (psf%psf_out(i_psf), psf%psf_out_m(i_psf))
!
        call dealloc_inod_grp_psf(psf%psf_grp_list(i_psf))
        call dealloc_coefficients_4_psf(psf%psf_def(i_psf))
      end do
!
      call dealloc_psf_node_and_patch                                   &
    &    (psf%num_psf, psf%psf_list, psf%psf_mesh)
      call dealloc_psf_field_name(psf%num_psf, psf%psf_mesh)
      call dealloc_psf_field_data(psf%num_psf, psf%psf_mesh)
      call dealloc_psf_case_table(psf%psf_case_tbls)
!
      deallocate(psf%psf_mesh, psf%psf_list, psf%psf_grp_list)
      deallocate(psf%psf_search, psf%psf_file_IO)
      deallocate(psf%psf_out, psf%psf_out_m)
      deallocate(psf%psf_param)
!
      end subroutine dealloc_psf_field_type
!
!  ---------------------------------------------------------------------
!
      end module t_cross_section
