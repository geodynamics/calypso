!>@file   FEM_analyzer_sph_MHD.f90
!!@brief  module FEM_analyzer_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2010
!
!>@brief Top routines to transfer spherical harmonics grids data
!!       to FEM data for data visualization
!!
!!@verbatim
!!      subroutine FEM_initialize_sph_MHD(MHD_files, MHD_step,          &
!!     &          geofem, ele_mesh, iphys, nod_fld, MHD_IO)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(MHD_step_param), intent(in) :: MHD_step
!!        type(mesh_data), intent(inout) :: geofem
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(phys_address), intent(inout) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!      subroutine FEM_analyze_sph_MHD                                  &
!!     &         (MHD_files, geofem, nod_fld, MHD_step, visval, MHD_IO)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!      subroutine FEM_finalize(MHD_files, MHD_step, MHD_IO)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(MHD_step_param), intent(in) :: MHD_step
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!
!!      subroutine SPH_to_FEM_bridge_MHD(sph, WK, mesh, nod_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(works_4_sph_trans_MHD), intent(in) :: WK
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!      subroutine FEM_to_SPH_bridge
!!@endverbatim
!!
!!@n @param  i_step       Current time step
!!@n @param  visval       Return flag to call visualization routines
!
      module FEM_analyzer_sph_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_work_time
!
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_MHD_IO_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_sph_MHD(MHD_files, MHD_step,            &
     &          geofem, ele_mesh, iphys, nod_fld, MHD_IO)
!
      use m_array_for_send_recv
      use t_cal_max_indices
!
      use init_nodal_field_address
      use nod_phys_send_recv
      use node_monitor_IO
      use parallel_FEM_mesh_init
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
      type(mesh_data), intent(inout) :: geofem
      type(element_geometry), intent(inout) :: ele_mesh
      type(phys_address), intent(inout) :: iphys
      type(phys_data), intent(inout) :: nod_fld
      type(MHD_IO_data), intent(inout) :: MHD_IO
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_local_nod_4_monitor'
      call set_local_nod_4_monitor(geofem%mesh, geofem%group)
!
!  -------------------------------
!
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_init_with_IO                                        &
     &   (MHD_files%FEM_mesh_flags%iflag_output_SURF,                   &
     &    MHD_files%mesh_file_IO, geofem%mesh, geofem%group, ele_mesh)
!
      call deallocate_surface_geom_type(ele_mesh%surf)
      call dealloc_edge_geometory(ele_mesh%edge)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_nod_fld_address'
      call init_nod_fld_address(geofem%mesh%node, nod_fld, iphys)
!
!  connect grid data to volume output
!
      if(MHD_step%ucd_step%increment .gt. 0) then
        call alloc_phys_range(nod_fld%ntot_phys_viz, MHD_IO%range)
      end if
!
      if(iflag_debug .gt. 0) write(*,*) 'output_grd_file_4_snapshot'
      call output_grd_file_4_snapshot(MHD_files%ucd_file_IO,            &
     &    MHD_step%ucd_step, geofem%mesh, nod_fld, MHD_IO%fem_ucd)
!
      end subroutine FEM_initialize_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_sph_MHD                                    &
     &         (MHD_files, geofem, nod_fld, MHD_step, visval, MHD_IO)
!
      use nod_phys_send_recv
      use output_viz_file_control
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(inout) :: nod_fld
!
      integer (kind =kint), intent(inout) :: visval
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_IO_data), intent(inout) :: MHD_IO
!
!
!*  ----------   Count steps for visualization
!*
      visval = iflag_vizs_w_fix_step(MHD_step%time_d%i_time_step,       &
     &                             MHD_step%viz_step)
      call istep_viz_w_fix_dt(MHD_step%time_d%i_time_step,              &
     &                             MHD_step%viz_step)
      if(lead_field_data_flag(MHD_step%time_d%i_time_step,MHD_step)     &
     &  .ne. 0) return
!
!*  ----------- Data communication  --------------
!
      if (iflag_debug.gt.0) write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(geofem%mesh, nod_fld)
!
!*  -----------  Output volume data --------------
!*
      call s_output_ucd_file_control                                    &
     &   (MHD_files%ucd_file_IO, MHD_step%time_d%i_time_step,           &
     &    MHD_step%time_d, MHD_step%ucd_step, MHD_IO%fem_ucd)
!
      end subroutine FEM_analyze_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_MHD(sph, WK, mesh, nod_fld)
!
      use t_spheric_parameter
      use t_sph_trans_arrays_MHD
!
      use set_address_sph_trans_snap
!
      type(sph_grids), intent(in) :: sph
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(mesh_geometry), intent(in) :: mesh
!
      type(phys_data), intent(inout) :: nod_fld
!*
!*  -----------  data transfer to FEM array --------------
!*
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'copy_force_from_transform MHD'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_MHD%forward, mesh, nod_fld)
!
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'copy_field_from_transform SNAP'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_snap%backward, mesh, nod_fld)
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'copy_force_from_transform SNAP'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_snap%forward, mesh, nod_fld)
!
      end subroutine SPH_to_FEM_bridge_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!      subroutine FEM_to_SPH_bridge
!
!
!      end subroutine FEM_to_SPH_bridge
!
!-----------------------------------------------------------------------
!
      subroutine FEM_finalize(MHD_files, MHD_step, MHD_IO)
!
      use t_cal_max_indices
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
!
      type(MHD_IO_data), intent(inout) :: MHD_IO
!
!
     if(MHD_step%ucd_step%increment .gt. 0) then
       call dealloc_phys_range(MHD_IO%range)
       call finalize_output_ucd(MHD_files%ucd_file_IO, MHD_IO%fem_ucd)
     end if
!
      end subroutine FEM_finalize
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_MHD
