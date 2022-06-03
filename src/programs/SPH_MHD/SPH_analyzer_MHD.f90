!>@file   SPH_analyzer_MHD
!!@brief  module SPH_analyzer_MHD
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_initialize_MHD(MHD_files, SPH_model, iphys,      &
!!     &                              MHD_step, sph_fst_IO, SPH_MHD,    &
!!     &                              SPH_WK, SR_sig, SR_r)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(phys_address), intent(in) :: iphys
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!        type(field_IO), intent(inout) :: sph_fst_IO
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!      subroutine SPH_analyze_MHD(i_step, MHD_files, iflag_finish,     &
!!     &                           SPH_model, MHD_step, sph_fst_IO,     &
!!     &                           SPH_MHD, SPH_WK, SR_sig, SR_r)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!        type(field_IO), intent(inout) :: sph_fst_IO
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module SPH_analyzer_MHD
!
      use m_precision
      use m_constants
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_MHD_step_parameter
      use t_phys_address
      use t_control_parameter
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_SPH_mesh_field_data
      use t_boundary_data_sph_MHD
      use t_work_SPH_MHD
      use t_field_data_IO
      use t_solver_SR
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_initialize_MHD(MHD_files, SPH_model, iphys,        &
     &                              MHD_step, sph_fst_IO, SPH_MHD,      &
     &                              SPH_WK, SR_sig, SR_r)
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_sph_boundary_input_data
!
      use set_control_sph_mhd
      use set_initial_sph_dynamo
      use adjust_reference_fields
      use set_bc_sph_mhd
      use adjust_reference_fields
      use material_property
      use init_radial_infos_sph_mhd
      use const_radial_mat_4_sph
      use cal_sol_sph_MHD_crank
      use cal_nonlinear
      use init_sphrical_transform_MHD
      use check_dependency_for_MHD
      use input_control_sph_MHD
!
      use m_work_time
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(phys_address), intent(in) :: iphys
!
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
      type(field_IO), intent(inout) :: sph_fst_IO
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!   Allocate spectr field data
!
      call set_sph_MHD_sprctr_data(SPH_model%MHD_prop, SPH_MHD)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo(SPH_model%bc_IO, SPH_MHD%groups,    &
     &   SPH_model%MHD_BC, SPH_MHD%ipol, SPH_MHD%sph, SPH_WK%r_2nd,     &
     &   SPH_model%omega_sph, SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_MHD'
      call init_sph_transform_MHD(SPH_model, iphys, SPH_WK%trans_p,     &
     &    SPH_WK%trns_WK, SPH_MHD, SR_sig, SR_r)
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' read_sph_initial_data_control'
      call read_sph_initial_data_control                                &
     &   (MHD_files, SPH_model, SPH_MHD%sph, SPH_MHD%ipol, MHD_step,    &
     &    SPH_MHD%fld, sph_fst_IO)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_reference_scalars'
      call init_reference_scalars                                       &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_WK%r_2nd, SPH_model%refs,      &
     &    SPH_MHD%fld, SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' sph_initial_data_control'
      call sph_initial_data_control                                     &
     &   (MHD_files, SPH_model, SPH_MHD%sph, SPH_MHD%ipol, MHD_step,    &
     &    SPH_MHD%fld, sph_fst_IO)
      MHD_step%iflag_initial_step = 0
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph                                    &
     &   (SPH_model%MHD_prop, SPH_model%refs,                           &
     &    SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' const_radial_mat_sph_mhd'
      call const_radial_mat_sph_mhd                                     &
     &   (MHD_step%time_d%dt, SPH_model%MHD_prop, SPH_model%sph_MHD_bc, &
     &    SPH_MHD%sph, SPH_WK%r_2nd, SPH_WK%trans_p%leg,                &
     &    SPH_WK%MHD_mats)
!*
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_MHD_evolved_boundaries'
      call set_MHD_evolved_boundaries(MHD_step%time_d, SPH_MHD%sph,     &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start                                       &
     &   (SPH_MHD%sph%sph_rj, SPH_WK%r_2nd, SPH_model%MHD_prop,         &
     &    SPH_model%sph_MHD_bc, SPH_WK%trans_p%leg,                     &
     &    SPH_MHD%ipol, SPH_MHD%fld)
!
!* obtain nonlinear terms for starting
!
      if(iflag_debug .gt. 0) write(*,*) 'first nonlinear'
      call nonlinear(SPH_WK%r_2nd, SPH_model, SPH_WK%trans_p,           &
     &               SPH_WK%trns_WK, SPH_MHD, SR_sig, SR_r)
!
!* -----  Open Volume integration data files -----------------
!*
      if(iflag_debug .gt. 0) write(*,*) 'open_sph_vol_rms_file_mhd'
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      call open_sph_vol_rms_file_mhd                                    &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld,                       &
     &    SPH_WK%monitor, SR_sig)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
      end subroutine SPH_initialize_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_MHD(i_step, MHD_files, iflag_finish,       &
     &                           SPH_model, MHD_step, sph_fst_IO,       &
     &                           SPH_MHD, SPH_WK, SR_sig, SR_r)
!
      use calypso_mpi_real
      use cal_momentum_eq_explicit
      use cal_sol_sph_MHD_crank
      use cal_nonlinear
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use output_viz_file_control
      use cal_write_sph_monitor_data
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      integer(kind = kint), intent(inout) :: iflag_finish
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
      type(field_IO), intent(inout) :: sph_fst_IO
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!*  ----------  add time evolution -----------------
!*
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+1)
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+2)
      if(iflag_debug.gt.0) write(*,*) 'sel_explicit_sph'
      call sel_explicit_sph(i_step, MHD_step%time_d%dt,                 &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc, SPH_MHD%sph,        &
     &    SPH_MHD%ipol, SPH_MHD%fld)
!*
!*  ----------  time evolution by inplicit method ----------
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_MHD_evolved_boundaries'
      call set_MHD_evolved_boundaries(MHD_step%time_d, SPH_MHD%sph,     &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+3)
      call s_cal_sol_sph_MHD_crank                                      &
     &   (MHD_step%time_d%dt, SPH_MHD%sph%sph_rj, SPH_WK%r_2nd,         &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc, SPH_WK%trans_p%leg, &
     &    SPH_MHD%ipol, SPH_WK%MHD_mats, SPH_MHD%fld)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+3)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+2)
!*
!*  ----------------lead nonlinear term ... ----------
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+4)
      call nonlinear(SPH_WK%r_2nd, SPH_model, SPH_WK%trans_p,           &
     &               SPH_WK%trns_WK, SPH_MHD, SR_sig, SR_r)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+4)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+1)
!
!* ----  Update fields after time evolution ------------------------=
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+5)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph                                   &
     &   (SPH_model%MHD_prop, SPH_model%refs,                           &
     &    SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!*
      if(lead_field_data_flag(i_step, MHD_step)) then
        if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
        call s_lead_fields_4_sph_mhd(SPH_WK%monitor, SPH_WK%r_2nd,      &
     &      SPH_model%MHD_prop, SPH_model%sph_MHD_bc, SPH_WK%trans_p,   &
     &      SPH_WK%MHD_mats, SPH_WK%trns_WK, SPH_MHD, SR_sig, SR_r)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+5)
!
!*  -----------  output restart data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+6)
      if(output_IO_flag(MHD_step%time_d%i_time_step,                    &
     &                  MHD_step%rst_step)) then
        if(iflag_debug.gt.0) write(*,*) 'output_sph_restart_control'
        call output_sph_restart_control(MHD_step%time_d%i_time_step,    &
     &      MHD_files%fst_file_IO, MHD_step%time_d, SPH_MHD%fld,        &
     &      MHD_step%rst_step, sph_fst_IO)
      end if
!
      MHD_step%finish_d%elapsed_local                                   &
     &    = MPI_WTIME() - MHD_step%finish_d%started_time
      call calypso_mpi_allreduce_one_real                               &
     &   (MHD_step%finish_d%elapsed_local,                              &
     &    MHD_step%finish_d%elapsed_max, MPI_MAX)
      if      (MHD_step%finish_d%i_end_step .eq. -1                     &
     &   .and.  MHD_step%finish_d%elapsed_max                           &
     &        .gt. MHD_step%finish_d%elapsed_time) then
        iflag_finish = 1
        call output_sph_restart_control(MHD_step%finish_d%i_end_step,   &
     &      MHD_files%fst_file_IO, MHD_step%time_d, SPH_MHD%fld,        &
     &      MHD_step%rst_step, sph_fst_IO)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+6)
!
!*  -----------  lead energy data --------------
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+7)
      if(output_IO_flag(i_step, MHD_step%rms_step)) then
        if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
        call output_rms_sph_mhd_control(MHD_step%time_d, SPH_MHD,       &
     &      SPH_model%MHD_prop, SPH_model%sph_MHD_bc,                   &
     &      SPH_WK%r_2nd, SPH_WK%trans_p%leg, SPH_WK%monitor, SR_sig)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+7)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph                                    &
     &   (SPH_model%MHD_prop, SPH_model%refs,                           &
     &    SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!
      if(i_step .ge. MHD_step%finish_d%i_end_step                       &
     &    .and. MHD_step%finish_d%i_end_step .gt. 0) then
        iflag_finish = 1
      end if
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
      end subroutine SPH_analyze_MHD
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_MHD
!
!      end subroutine SPH_finalize_MHD
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_MHD
