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
!!     &          mesh, group, ele_mesh, iphys, nod_fld, range, fem_ucd)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(MHD_step_param), intent(in) :: MHD_step
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(phys_address), intent(inout) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(maximum_informations), intent(inout) :: range
!!        type(ucd_file_data), intent(inout) :: fem_ucd
!!      subroutine FEM_analyze_sph_MHD                                  &
!!     &         (MHD_files, mesh, nod_fld, MHD_step, visval, fem_ucd)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(ucd_file_data), intent(inout) :: fem_ucd
!!      subroutine FEM_finalize(MHD_files, MHD_step, range, fem_ucd)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(MHD_step_param), intent(in) :: MHD_step
!!
!!      subroutine SPH_to_FEM_bridge_MHD                                &
!!     &         (sph_params, sph_rtp, WK, mesh, iphys, nod_fld)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
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
      use t_ucd_file
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_ucd_file
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
     &          mesh, group, ele_mesh, iphys, nod_fld, range, fem_ucd)
!
      use m_array_for_send_recv
      use t_cal_max_indices
!
      use set_field_address
      use nod_phys_send_recv
      use node_monitor_IO
      use parallel_FEM_mesh_init
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
      type(phys_address), intent(inout) :: iphys
      type(phys_data), intent(inout) :: nod_fld
      type(maximum_informations), intent(inout) :: range
      type(ucd_file_data), intent(inout) :: fem_ucd
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_local_node_id_4_monitor'
      call set_local_node_id_4_monitor(mesh%node, group%nod_grp)
!
!  -------------------------------
!
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_init_with_IO(MHD_files%iflag_output_SURF,           &
     &    MHD_files%mesh_file_IO, mesh, group, ele_mesh)
!
      call deallocate_surface_geom_type(ele_mesh%surf)
      call dealloc_edge_geometory(ele_mesh%edge)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_field_address'
      call init_field_address(mesh%node%numnod, nod_fld, iphys)
!
!  connect grid data to volume output
!
      if(MHD_step%ucd_step%increment .gt. 0) then
        call alloc_phys_range(nod_fld%ntot_phys_viz, range)
      end if
!
      if(iflag_debug .gt. 0) write(*,*) 'output_grd_file_4_snapshot'
      call output_grd_file_4_snapshot(MHD_files%ucd_file_IO,            &
     &    MHD_step%ucd_step, mesh, nod_fld, fem_ucd)
!
      end subroutine FEM_initialize_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_sph_MHD                                    &
     &         (MHD_files, mesh, nod_fld, MHD_step, visval, fem_ucd)
!
      use nod_phys_send_recv
      use output_viz_file_control
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(inout) :: nod_fld
!
      integer (kind =kint), intent(inout) :: visval
      type(MHD_step_param), intent(inout) :: MHD_step
      type(ucd_file_data), intent(inout) :: fem_ucd
!
      integer(kind = kint) :: iflag
!
!
!*  ----------   Count steps for visualization
!*
!*
      visval = 1
      visval = viz_file_step_4_fix(MHD_step%time_d%i_time_step,         &
     &                             MHD_step%viz_step)
      iflag = lead_field_data_flag(MHD_step%time_d%i_time_step,         &
     &                             MHD_step)
      if(iflag .ne. 0) return
!
!*  ----------- Data communication  --------------
!
      if (iflag_debug.gt.0) write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(mesh, nod_fld)
!
!*  -----------  Output volume data --------------
!*
      call s_output_ucd_file_control                                    &
     &   (MHD_files%ucd_file_IO, MHD_step%time_d%i_time_step,           &
     &    MHD_step%time_d, MHD_step%ucd_step, fem_ucd)
!
      end subroutine FEM_analyze_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_MHD                                  &
     &         (sph_params, sph_rtp, WK, mesh, iphys, nod_fld)
!
      use t_spheric_parameter
      use t_sph_trans_arrays_MHD
!
      use copy_snap_4_sph_trans
      use copy_MHD_4_sph_trans
      use coordinate_convert_4_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(mesh_geometry), intent(in) :: mesh
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!*
!*  -----------  data transfer to FEM array --------------
!*
      if (iflag_debug.gt.0) write(*,*) 'copy_forces_to_snapshot_rtp'
      call copy_forces_to_snapshot_rtp                                  &
     &   (sph_params%m_folding, sph_rtp, WK%trns_MHD,                   &
     &    mesh%node, iphys, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_snap_vec_fld_from_trans'
      call copy_snap_vec_fld_from_trans                                 &
     &   (sph_params%m_folding, sph_rtp, WK%trns_snap,                  &
     &    mesh%node, iphys, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_snap_vec_force_from_trans'
      call copy_snap_vec_force_from_trans                               &
     &   (sph_params%m_folding, sph_rtp, WK%trns_snap,                  &
     &    mesh%node, iphys, nod_fld)
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
      subroutine FEM_finalize(MHD_files, MHD_step, range, fem_ucd)
!
      use t_cal_max_indices
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
!
      type(maximum_informations), intent(inout) :: range
      type(ucd_file_data), intent(inout) :: fem_ucd
!
!
     if(MHD_step%ucd_step%increment .gt. 0) then
       call dealloc_phys_range(range)
       call finalize_output_ucd(MHD_files%ucd_file_IO, fem_ucd)
     end if
!
      end subroutine FEM_finalize
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_MHD
