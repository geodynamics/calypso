!>@file   map_projection.f90
!!@brief  module map_projection
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!      subroutine MAP_PROJECTION_initialize                            &
!!     &         (increment_psf, elps_PSF, elps_MAP, geofem, edge_comm, &
!!     &          nod_fld, map_ctls, map, SR_sig, SR_il)
!!        type(elapsed_lables), intent(in) :: elps_PSF, elps_MAP
!!        type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: edge_comm
!!        type(phys_data), intent(in) :: nod_fld
!!        type(map_rendering_controls), intent(inout) :: map_ctls
!!        type(map_rendering_module), intent(inout) :: map
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!      subroutine MAP_PROJECTION_visualize                             &
!!     &         (istep_psf, elps_PSF, elps_MAP, time_d,                &
!!     &          geofem, nod_fld, map, SR_sig)
!!        type(elapsed_lables), intent(in) :: elps_PSF, elps_MAP
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!      subroutine MAP_PROJECTION_finalize(map)
!!        type(map_rendering_module), intent(inout) :: map
!!@endverbatim
      module map_projection
!
      use m_precision
      use m_work_time
!
      use calypso_mpi
      use t_map_projection
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine MAP_PROJECTION_initialize                              &
     &         (increment_psf, elps_PSF, elps_MAP, geofem, edge_comm,   &
     &          nod_fld, map_ctls, map, SR_sig, SR_il)
!
      use m_geometry_constants
!
      use calypso_mpi
      use set_map_control
      use search_ele_list_for_psf
      use set_const_4_sections
      use find_node_and_patch_psf
      use set_fields_for_psf
      use multi_map_projections
!
      integer(kind = kint), intent(in) :: increment_psf
      type(elapsed_lables), intent(in) :: elps_PSF, elps_MAP
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: edge_comm
      type(phys_data), intent(in) :: nod_fld
!
      type(map_rendering_controls), intent(inout) :: map_ctls
      type(map_rendering_module), intent(inout) :: map
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      integer(kind = kint) :: i_psf
!
!
      map%num_map = map_ctls%num_map_ctl
      if(increment_psf .le. 0) map%num_map = 0
      if(map%num_map .le. 0) return
!
      if(elps_PSF%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_PSF%ist_elapsed+1)
      call init_psf_case_tables(map%psf_case_tbls)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_map_rendering_module'
      call alloc_map_rendering_module(map)
!
      call s_set_map_control(map%num_map, geofem%group, nod_fld,        &
     &    map_ctls, map%map_param, map%map_def, map%map_mesh,           &
     &    map%view_param, map%color_param, map%cbar_param,              &
     &    map%map_data, map%map_rgb)
!
      if (iflag_debug.eq.1) write(*,*) 'set_search_mesh_list_4_psf'
      call set_search_mesh_list_4_psf                                   &
     &   (map%num_map, geofem%mesh, geofem%group,                       &
     &    map%map_param, map%psf_search)
!
!
      do i_psf = 1, map%num_map
        call alloc_node_param_smp(map%map_mesh(i_psf)%node)
        call alloc_ele_param_smp(map%map_mesh(i_psf)%patch)
!
        call alloc_ref_field_4_psf                                      &
     &     (geofem%mesh%node, map%map_list(i_psf))
      end do
!
      if (iflag_debug.eq.1) write(*,*) 'set_const_4_crossections'
      call set_const_4_crossections                                     &
     &   (map%num_map, map%map_def, geofem%mesh%node, map%map_list)
!
      if (iflag_debug.eq.1) write(*,*) 'set_node_and_patch_psf'
      call set_node_and_patch_psf                                       &
     &   (map%num_map, geofem%mesh, geofem%group, edge_comm,            &
     &    map%psf_case_tbls, map%map_def, map%psf_search, map%map_list, &
     &    map%map_grp_list, map%map_mesh, SR_sig, SR_il)
!
      call alloc_psf_field_data(map%num_map, map%map_mesh)
      if(elps_PSF%flag_elapsed)                                         &
     &          call end_elapsed_time(elps_PSF%ist_elapsed+1)
!
      if (iflag_debug.eq.1) write(*,*) 'output_section_mesh'
      call init_multi_map_projections                                   &
     &   (elps_MAP, map%num_map, map%view_param, map%map_mesh,          &
     &    map%map_psf_dat, map%map_data, map%map_rgb, SR_sig)
!
      end subroutine MAP_PROJECTION_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine MAP_PROJECTION_visualize                               &
     &         (istep_psf, elps_PSF, elps_MAP, time_d,                  &
     &          geofem, nod_fld, map, SR_sig)
!
      use set_fields_for_psf
      use set_ucd_data_to_type
      use multi_map_projections
!
      integer(kind = kint), intent(in) :: istep_psf
      type(elapsed_lables), intent(in) :: elps_PSF, elps_MAP
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
!
      type(map_rendering_module), intent(inout) :: map
      type(send_recv_status), intent(inout) :: SR_sig
!
!
      if(map%num_map.le.0 .or. istep_psf.le.0) return
!
      if(elps_PSF%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_PSF%ist_elapsed+2)
      call set_field_4_psf(map%num_map, geofem%mesh%edge, nod_fld,      &
     &    map%map_def, map%map_param, map%map_list, map%map_grp_list,   &
     &    map%map_mesh)
      if(elps_PSF%flag_elapsed)                                         &
     &          call end_elapsed_time(elps_PSF%ist_elapsed+2)
!
      if(iflag_debug.eq.1) write(*,*) 'output_section_data'
      call s_multi_map_projections(map%num_map, istep_psf, elps_MAP,    &
     &    time_d, map%map_mesh, map%color_param, map%cbar_param,        &
     &    map%map_psf_dat, map%map_data, map%map_rgb, SR_sig)
!
      end subroutine MAP_PROJECTION_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine MAP_PROJECTION_finalize(map)
!
      type(map_rendering_module), intent(inout) :: map
!
!
      if(map%num_map .le. 0) return
      call dealloc_map_rendering_module(map)
!
      end subroutine MAP_PROJECTION_finalize
!
!  ---------------------------------------------------------------------
!
      end module map_projection
