!>@file   FEM_analyzer_sph_MHD.f90
!!@brief  module FEM_analyzer_sph_MHD
!!
!!@author H. Matsui (UC Berkeley) and T. Kera (Tohoku University)
!!@date Programmed in Apr., 2010
!>        Modified by T. Kera in Aug., 2021
!
!>@brief Top routines to transfer spherical harmonics grids data
!!       to FEM data for data visualization
!!
!!@verbatim
!!      subroutine FEM_initialize_sph_MHD(MHD_files, MHD_step,          &
!!     &                                  FEM_MHD, MHD_IO, m_SR)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(MHD_step_param), intent(in) :: MHD_step
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine FEM_analyze_sph_MHD(MHD_files, FEM_MHD, MHD_step,    &
!!     &                               MHD_IO, m_SR)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(time_data), intent(in) :: time_d
!!        type(FEM_mesh_field_data), intent(in) :: FEM_MHD
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine FEM_finalize(MHD_files, MHD_step, MHD_IO)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(MHD_step_param), intent(in) :: MHD_step
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!
!!      subroutine SPH_to_FEM_bridge_MHD(sph, WK, geofem, nod_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(works_4_sph_trans_MHD), intent(in) :: WK
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(inout) :: nod_fld
!!      subroutine FEM_to_SPH_bridge
!!@endverbatim
!!
!!@n @param  i_step       Current time step
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
      use t_comm_table
      use t_phys_data
      use t_phys_address
      use t_FEM_mesh_field_data
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_MHD_IO_data
      use t_ucd_file
      use t_mesh_SR
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
     &                                  FEM_MHD, MHD_IO, m_SR)
!
      use m_work_time
      use m_elapsed_labels_4_MHD
      use t_cal_max_indices
      use t_node_monitor_IO
!
      use set_control_field_data
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use const_element_comm_tables
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
!
      type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if (iflag_debug.gt.0) write(*,*) 'init_field_data'
      call init_field_data(FEM_MHD%geofem%mesh%node%numnod,             &
     &                     FEM_MHD%field, FEM_MHD%iphys)
!
      if (iflag_debug.gt.0) write(*,*) 'set_local_nod_4_monitor'
      call set_local_nod_4_monitor                                      &
     &   (FEM_MHD%geofem%mesh, FEM_MHD%geofem%group, FEM_MHD%nod_mntr)
!
!  -------------------------------
!      INIT communication buffer
!  -------------------------------
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_comm_initialization'
      call FEM_comm_initialization(FEM_MHD%geofem%mesh, m_SR)
!
!  -------------------------------
!  connect grid data to volume output
!  -------------------------------
      if(MHD_step%ucd_step%increment .gt. 0) then
        call alloc_phys_range(FEM_MHD%field%ntot_phys_viz,              &
     &                        MHD_IO%range)
      end if
!
!  -------------------------------
!
      if(iflag_debug .gt. 0) write(*,*) 'output_grd_file_4_snapshot'
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+6)
      call output_grd_file_4_snapshot(MHD_files%ucd_file_IO,            &
     &    MHD_step%ucd_step, FEM_MHD%geofem%mesh, FEM_MHD%field,        &
     &    MHD_IO%ucd, m_SR%SR_sig, m_SR%SR_i)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+6)
!
      end subroutine FEM_initialize_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_sph_MHD(MHD_files, FEM_MHD, MHD_step,      &
     &                               MHD_IO, m_SR)
!
      use m_work_time
      use m_elapsed_labels_4_MHD
      use nod_phys_send_recv
      use output_viz_file_control
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(mesh_SR), intent(inout) :: m_SR
!
!*  ----------   Count steps for visualization
!*
      if(lead_field_data_flag(MHD_step%time_d%i_time_step, MHD_step)    &
     &    .eqv. .FALSE.) return
!
!*  ----------- Data communication  --------------
!
      if (iflag_debug.gt.0) write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(FEM_MHD%geofem%mesh, FEM_MHD%field,     &
     &                          m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
!*  -----------  Output volume data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+6)
      call s_output_ucd_file_control                                    &
     &   (MHD_files%ucd_file_IO, MHD_step%time_d%i_time_step,           &
     &    MHD_step%ucd_step, MHD_step%time_d, MHD_IO%ucd)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+6)
!
      end subroutine FEM_analyze_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_MHD(sph, WK, geofem, nod_fld)
!
      use t_spheric_parameter
      use t_sph_trans_arrays_MHD
!
      use set_address_sph_trans_snap
!
      type(sph_grids), intent(in) :: sph
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(mesh_data), intent(in) :: geofem
!
      type(phys_data), intent(inout) :: nod_fld
!*
!*  -----------  data transfer to FEM array --------------
!*
      if (iflag_debug.gt.0) write(*,*) 'copy_force_from_transform MHD'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_MHD%forward, geofem%mesh, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                'copy_field_from_transform base fields'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_snap%backward, geofem%mesh, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_force_from_transform snap'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_snap%forward, geofem%mesh, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                'copy_field_from_transform diff_vector'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_difv%backward, geofem%mesh, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_field_from_transform SNAP'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_eflux%backward, geofem%mesh, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_force_from_transform SNAP'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_eflux%forward, geofem%mesh, nod_fld)
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
       call finalize_output_ucd(MHD_files%ucd_file_IO, MHD_IO%ucd)
     end if
!
      end subroutine FEM_finalize
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_MHD
