!>@file   t_isosurface.f90
!!@brief  module t_isosurface
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for isosurfacing
!!
!!@verbatim
!!      subroutine ISOSURF_initialize(increment_iso, geofem,            &
!!     &                              nod_fld, iso_ctls, iso)
!!        type(mesh_data), intent(in) :: geofem
!!        type(isosurf_controls), intent(inout) :: iso_ctls
!!        type(isosurface_module), intent(inout) :: iso
!!        type(phys_data), intent(in) :: nod_fld
!!      subroutine ISOSURF_visualize(istep_iso, time_d,                 &
!!     &          geofem, edge_comm, nod_fld, iso, SR_sig, SR_il)
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: edge_comm
!!        type(phys_data), intent(in) :: nod_fld
!!        type(isosurface_module), intent(inout) :: iso
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!
!!      subroutine ISOSURF_finalize(iso)
!!        type(isosurface_module), intent(inout) :: iso
!!@endverbatim
!
      module t_isosurface
!
      use m_precision
      use t_mesh_data
      use t_comm_table
      use t_phys_data
      use t_psf_geometry_list
      use t_psf_patch_data
      use t_time_data
      use t_psf_case_table
      use t_ucd_data
      use t_file_IO_parameter
      use t_control_params_4_iso
      use t_control_data_isosurfaces
      use t_solver_SR
      use t_solver_SR_int8
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
      type isosurface_module
!>        Number of isosurfaces
        integer(kind = kint) :: num_iso
!
!>        Structure of case table for isosurface
        type(psf_cases) :: iso_case_tbls
!
!>        Structure of table for sections
        type(sectioning_list), allocatable :: iso_list(:)
!
!>        Structure of search table for sections
        type(psf_search_lists), allocatable :: iso_search(:)
!
!>        Structure of isosurface parameter
        type(psf_parameters), allocatable :: iso_param(:)
        type(isosurface_define), allocatable :: iso_def(:)
!
!>        Structure of isosurface patch data on local domain
        type(psf_local_data), allocatable :: iso_mesh(:)
!
!>        Structure of isosurface time output
        type(time_data) :: iso_time_IO
!>        Structure of isosurface data output
        type(field_IO_params), allocatable :: iso_file_IO(:)
!
!>        Structure of isosurface output (used by master process)
        type(ucd_data), allocatable :: iso_out(:)
      end type isosurface_module
!
      private :: alloc_iso_field_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine ISOSURF_initialize(increment_iso, geofem,              &
     &                              nod_fld, iso_ctls, iso)
!
      use m_geometry_constants
!
      use set_iso_control
      use search_ele_list_for_psf
!
      integer(kind = kint), intent(in) :: increment_iso
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
!
      type(isosurf_controls), intent(inout) :: iso_ctls
      type(isosurface_module), intent(inout) :: iso
!
      integer(kind = kint) :: i_iso
!
!
      iso%num_iso = iso_ctls%num_iso_ctl
      if(increment_iso .le. 0) iso%num_iso = 0
      if(iso%num_iso .le. 0) return
!
      call init_psf_case_tables(iso%iso_case_tbls)
!
      call alloc_iso_field_type(iso)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_iso_control'
      call s_set_iso_control                                            &
     &   (iso%num_iso, geofem%group, nod_fld, iso_ctls,                 &
     &    iso%iso_param, iso%iso_def, iso%iso_mesh, iso%iso_file_IO)
!
      if (iflag_debug.eq.1) write(*,*) 'set_search_mesh_list_4_psf'
      call set_search_mesh_list_4_psf                                   &
     &   (iso%num_iso, geofem%mesh, geofem%group,                       &
     &    iso%iso_param, iso%iso_search)
!
      do i_iso = 1, iso%num_iso
        call alloc_node_param_smp(iso%iso_mesh(i_iso)%node)
        call alloc_ele_param_smp(iso%iso_mesh(i_iso)%patch)
!
        call alloc_ref_field_4_psf                                      &
     &     (geofem%mesh%node, iso%iso_list(i_iso))
      end do
!
      end subroutine ISOSURF_initialize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine ISOSURF_visualize(istep_iso, time_d,                   &
     &          geofem, edge_comm, nod_fld, iso, SR_sig, SR_il)
!
      use m_work_time
      use m_elapsed_labels_4_VIZ
      use m_geometry_constants
      use t_time_data
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
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: edge_comm
      type(phys_data), intent(in) :: nod_fld
!
      type(isosurface_module), intent(inout) :: iso
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
!
      if (iso%num_iso.le.0 .or. istep_iso.le.0) return
!
      if(iflag_ISO_time) call start_elapsed_time(ist_elapsed_ISO+1)
      if (iflag_debug.eq.1) write(*,*) 'set_const_4_isosurfaces'
      call set_const_4_isosurfaces(iso%num_iso, geofem%mesh%node,       &
     &    nod_fld, iso%iso_def, iso%iso_list)
!
      if (iflag_debug.eq.1) write(*,*) 'set_node_and_patch_iso'
      call set_node_and_patch_iso                                       &
     &   (iso%num_iso, geofem%mesh, edge_comm, iso%iso_case_tbls,       &
     &    iso%iso_search, iso%iso_list, iso%iso_mesh, SR_sig, SR_il)
      if(iflag_ISO_time) call end_elapsed_time(ist_elapsed_ISO+1)
!
      if (iflag_debug.eq.1) write(*,*) 'set_field_4_iso'
      if(iflag_ISO_time) call start_elapsed_time(ist_elapsed_ISO+2)
      call alloc_psf_field_data(iso%num_iso, iso%iso_mesh)
      call set_field_4_iso(iso%num_iso, geofem%mesh%edge, nod_fld,      &
     &    iso%iso_param, iso%iso_def, iso%iso_list, iso%iso_mesh)
      if(iflag_ISO_time) call end_elapsed_time(ist_elapsed_ISO+2)
!
      if(iflag_ISO_time) call start_elapsed_time(ist_elapsed_ISO+3)
      call output_isosurface                                            &
     &   (iso%num_iso, iso%iso_file_IO, istep_iso, time_d,              &
     &    iso%iso_mesh, iso%iso_time_IO, iso%iso_out)
!
      call dealloc_psf_field_data(iso%num_iso, iso%iso_mesh)
      call dealloc_psf_node_and_patch                                   &
     &   (iso%num_iso, iso%iso_list, iso%iso_mesh)
      if(iflag_ISO_time) call end_elapsed_time(ist_elapsed_ISO+3)
!
      end subroutine ISOSURF_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine ISOSURF_finalize(iso)
!
      use set_psf_control
!
      type(isosurface_module), intent(inout) :: iso
!
      integer(kind = kint) :: i_iso
!
!
      if(iso%num_iso .le. 0) return
!
      do i_iso = 1, iso%num_iso
        call dealloc_node_param_smp(iso%iso_mesh(i_iso)%node)
        call dealloc_ele_param_smp(iso%iso_mesh(i_iso)%patch)
      end do
!
      call dealloc_psf_field_name(iso%num_iso, iso%iso_mesh)
      call dealloc_psf_case_table(iso%iso_case_tbls)
!
      deallocate(iso%iso_mesh, iso%iso_list)
      deallocate(iso%iso_search, iso%iso_param, iso%iso_def)
      deallocate(iso%iso_file_IO, iso%iso_out)
!
      end subroutine ISOSURF_finalize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_iso_field_type(iso)
!
      use m_field_file_format
!
      type(isosurface_module), intent(inout) :: iso
!
!
      allocate(iso%iso_mesh(iso%num_iso))
      allocate(iso%iso_list(iso%num_iso))
      allocate(iso%iso_search(iso%num_iso))
      allocate(iso%iso_param(iso%num_iso))
      allocate(iso%iso_def(iso%num_iso))
!
      allocate(iso%iso_file_IO(iso%num_iso))
      allocate(iso%iso_out(iso%num_iso))
!
      iso%iso_file_IO(1:iso%num_iso)%iflag_format = iflag_sgl_ucd
!
      end subroutine alloc_iso_field_type
!
!  ---------------------------------------------------------------------
!
      end module t_isosurface
