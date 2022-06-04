!>@file   SPH_analyzer_d_bench.f90
!!        module SPH_analyzer_d_bench
!!
!!@author H. Matsui
!!@date   Programmed in 2012
!!@n      modified in 2013
!
!>@brief spherical harmonics part of 
!!        Initialzation and evolution loop for dynamo benchmark check
!!
!!@verbatim
!!      subroutine SPH_init_sph_dbench(MHD_files, iphys, SPH_model,     &
!!     &          MHD_step, SPH_MHD, SPH_WK, SR_sig, SR_r, cdat)
!!         type(MHD_file_IO_params), intent(in) :: MHD_files
!!         type(phys_address), intent(in) :: iphys
!!         type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!         type(MHD_step_param), intent(inout) :: MHD_step
!!         type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!         type(work_SPH_MHD), intent(inout) :: SPH_WK
!!         type(circle_fld_maker), intent(inout) :: cdat
!!         type(send_recv_status), intent(inout) :: SR_sig
!!         type(send_recv_real_buffer), intent(inout) :: SR_r
!!      subroutine SPH_analyze_dbench(i_step, MHD_files, SPH_model,     &
!!     &          SPH_MHD, SPH_WK, SR_sig, SR_r, cdat, bench)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!         type(SPH_MHD_model_data), intent(in) :: SPH_model
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!        type(phys_data), intent(inout) :: cdat
!!        type(dynamobench_monitor), intent(inout) :: bench
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!      subroutine SPH_finalize_dbench
!!@endverbatim
!
      module SPH_analyzer_d_bench
!
      use m_precision
      use m_MHD_step_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use t_MHD_file_parameter
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_boundary_data_sph_MHD
      use t_field_on_circle
      use t_field_4_dynamobench
      use t_work_SPH_MHD
      use t_solver_SR
!
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_init_sph_dbench(MHD_files, iphys, SPH_model,       &
     &          MHD_step, SPH_MHD, SPH_WK, SR_sig, SR_r, cdat)
!
      use m_constants
      use m_machine_parameter
!
      use t_sph_boundary_input_data
!
      use set_control_sph_mhd
      use adjust_reference_fields
      use set_bc_sph_mhd
      use adjust_reference_fields
      use material_property
      use cal_nonlinear
      use init_sphrical_transform_MHD
      use init_radial_infos_sph_mhd
      use const_radial_mat_4_sph
      use cal_rms_fields_by_sph
      use r_interpolate_sph_data
      use sph_mhd_rst_IO_control
      use field_at_mid_equator
      use check_dependency_for_MHD
      use input_control_sph_MHD
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(phys_address), intent(in) :: iphys
!
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(circle_fld_maker), intent(inout) :: cdat
!
!   Allocate spectr field data
!
      call set_sph_MHD_sprctr_data(SPH_model%MHD_prop, SPH_MHD)
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo(SPH_model%bc_IO, SPH_MHD%groups,    &
     &   SPH_model%MHD_BC, SPH_MHD%ipol, SPH_MHD%sph, SPH_WK%r_2nd,     &
     &   SPH_model%omega_sph, SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_MHD'
      call init_sph_transform_MHD(SPH_model, iphys, SPH_WK%trans_p,     &
     &    SPH_WK%trns_WK, SPH_MHD, SR_sig, SR_r)
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' read_alloc_sph_restart_data'
      call read_alloc_sph_restart_data(MHD_files%fst_file_IO,           &
     &    MHD_step%init_d, SPH_MHD%fld, MHD_step%rst_step)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_reference_scalars'
      call init_reference_scalars                                       &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_WK%r_2nd, SPH_model%refs,      &
     &    SPH_MHD%fld, SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
      if (iflag_debug.eq.1) write(*,*) 'const_radial_mat_sph_snap'
      call const_radial_mat_sph_snap                                    &
     &   (SPH_model%MHD_prop, SPH_model%sph_MHD_bc,                     &
     &    SPH_MHD%sph%sph_rj, SPH_WK%r_2nd, SPH_WK%trans_p%leg,         &
     &    SPH_WK%MHD_mats)
!
!     --------------------- 
!  set original spectr mesh data for extension of B
!
      call init_radial_sph_interpolation(MHD_files%org_rj_file_IO,      &
     &    SPH_MHD%sph%sph_params, SPH_MHD%sph%sph_rj, SPH_WK%rj_itp)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_rms_4_sph_spectr_4_mhd'
      call init_rms_4_sph_spectr_4_mhd                                  &
     &   (SPH_MHD%sph, SPH_MHD%fld, SPH_WK%monitor)
!
!* -----  find mid-equator point -----------------
!*
      call set_mid_equator_point_global(my_rank, SPH_WK%trans_p,        &
     &    SPH_MHD%sph%sph_params, SPH_MHD%sph%sph_rtp,                  &
     &    SPH_MHD%sph%sph_rj, cdat)
!
      end subroutine SPH_init_sph_dbench
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_dbench(i_step, MHD_files, SPH_model,       &
     &          SPH_MHD, SPH_WK, SR_sig, SR_r, cdat, bench)
!
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use const_data_4_dynamobench
      use input_control_sph_MHD
      use output_viz_file_control
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(SPH_MHD_model_data), intent(in) :: SPH_model
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(circle_fld_maker), intent(inout) :: cdat
      type(dynamobench_monitor), intent(inout) :: bench
!
!
      call read_alloc_sph_rst_4_snap(i_step,                            &
     &    MHD_files%org_rj_file_IO, MHD_files%fst_file_IO,              &
     &    MHD_step1%rst_step, SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld,   &
     &    MHD_step1%init_d, SPH_WK%rj_itp)
      call copy_time_data(MHD_step1%init_d, MHD_step1%time_d)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph                                    &
     &   (SPH_model%MHD_prop, SPH_model%refs,                           &
     &    SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start(SPH_MHD%sph%sph_rj, SPH_WK%r_2nd,     &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc, SPH_WK%trans_p%leg, &
     &    SPH_MHD%ipol, SPH_MHD%fld)
!
!* ----  Update fields after time evolution ------------------------=
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+5)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph                                   &
     &   (SPH_model%MHD_prop, SPH_model%refs,                           &
     &    SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!*
      if(lead_field_data_flag(i_step, MHD_step1)) then
        if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
        call s_lead_fields_4_sph_mhd(SPH_WK%monitor, SPH_WK%r_2nd,      &
     &      SPH_model%MHD_prop, SPH_model%sph_MHD_bc, SPH_WK%trans_p,   &
     &      SPH_WK%MHD_mats, SPH_WK%trns_WK, SPH_MHD, SR_sig, SR_r)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+5)
!
!*  -----------  lead mid-equator field --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+7)
      if(iflag_debug.gt.0)  write(*,*) 'const_data_4_dynamobench'
      call s_const_data_4_dynamobench                                   &
     &   (MHD_step1%time_d%time, SPH_MHD%sph%sph_params,                &
     &    SPH_MHD%sph%sph_rj, SPH_model%sph_MHD_bc, SPH_WK%trans_p,     &
     &    SPH_MHD%ipol, SPH_MHD%fld, cdat, SPH_WK%monitor%pwr,          &
     &    bench, SPH_WK%monitor%WK_pwr)
      call output_field_4_dynamobench(i_step, MHD_step1%time_d%time,    &
     &   SPH_model%sph_MHD_bc, SPH_MHD%ipol, bench)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+7)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
      end subroutine SPH_analyze_dbench
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_dbench
!
!      end subroutine SPH_finalize_dbench
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_d_bench
