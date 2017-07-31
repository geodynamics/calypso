!>@file   SPH_analyzer_MHD
!!@brief  module SPH_analyzer_MHD
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_initialize_MHD(MHD_files, bc_IO, iphys, MHD_step)
!!        type(phys_address), intent(in) :: iphys
!!      subroutine SPH_analyze_MHD                                      &
!!     &         (i_step, MHD_files, iflag_finish, MHD_step)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!@endverbatim
!
      module SPH_analyzer_MHD
!
      use m_precision
      use m_constants
      use m_MHD_step_parameter
      use m_physical_property
      use m_radial_matrices_sph
      use t_phys_address
      use t_MHD_step_parameter
      use t_MHD_file_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_initialize_MHD(MHD_files, bc_IO, iphys, MHD_step)
!
      use calypso_mpi
      use m_machine_parameter
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_rms_4_sph_spectr
      use m_sph_trans_arrays_MHD
      use m_boundary_data_sph_MHD
      use m_bc_data_list
!
      use t_sph_boundary_input_data
!
      use set_control_sph_mhd
      use set_sph_phys_address
      use const_fdm_coefs
      use set_initial_sph_dynamo
      use adjust_reference_fields
      use set_bc_sph_mhd
      use adjust_reference_fields
      use material_property
      use init_radial_infos_sph_mhd
      use const_radial_mat_4_sph
      use sph_mhd_rms_IO
      use cal_sol_sph_MHD_crank
      use cal_nonlinear
      use init_sphrical_transform_MHD
      use check_dependency_for_MHD
      use input_control_sph_MHD
!
      use m_work_time
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(boundary_spectra), intent(in) :: bc_IO
      type(phys_address), intent(in) :: iphys
!
      type(MHD_step_param), intent(inout) :: MHD_step
!
!
!   Allocate spectr field data
!
      call set_sph_MHD_sprctr_data                                      &
     &   (sph1%sph_rj, MHD_prop1, ipol, idpdr, itor, rj_fld1)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo                                     &
     &   (bc_IO, sph_grps1, MHD_BC1, ipol, sph1,                        &
     &    omega_sph1, ref_temp1, ref_comp1, MHD_prop1, sph_MHD_bc1,     &
     &    r_2nd, rj_fld1)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_MHD'
      call init_sph_transform_MHD(MHD_prop1, sph_MHD_bc1,               &
     &    ipol, idpdr, itor, iphys, sph1, comms_sph1, omega_sph1,       &
     &    trans_p1, trns_WK1, rj_fld1)
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' sph_initial_data_control'
      call sph_initial_data_control                                     &
     &   (MHD_files, ref_temp1%t_rj, sph1%sph_params, sph1%sph_rj,      &
     &    MHD_prop1%ref_param_T, sph_MHD_bc1%sph_bc_B,                  &
     &    ipol, idpdr, itor, rj_fld1, MHD_step)
      MHD_step%iflag_initial_step = 0
!
      if(iflag_debug.gt.0) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(ref_temp1, ref_comp1, MHD_prop1,   &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' const_radial_mat_sph_mhd'
      call const_radial_mat_sph_mhd(MHD_step%time_d%dt, MHD_prop1,      &
     &    sph_MHD_bc1, sph1%sph_rj, r_2nd, trans_p1%leg, sph_MHD_mat1)
!*
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start(sph1%sph_rj, r_2nd,                   &
     &    MHD_prop1, sph_MHD_bc1, trans_p1%leg, ipol, itor, rj_fld1)
!
!* obtain nonlinear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'first nonlinear'
      call nonlinear(sph1, comms_sph1, omega_sph1, r_2nd,               &
     &    MHD_prop1, sph_MHD_bc1, trans_p1, ref_temp1, ref_comp1,       &
     &    ipol, itor, trns_WK1, rj_fld1)
!
!* -----  Open Volume integration data files -----------------
!*
      if(iflag_debug .gt. 0) write(*,*) 'open_sph_vol_rms_file_mhd'
      call start_elapsed_time(4)
      call open_sph_vol_rms_file_mhd                                    &
     &   (sph1%sph_params, sph1%sph_rj, ipol, rj_fld1, pwr1, WK_pwr)
      call end_elapsed_time(4)
!
      end subroutine SPH_initialize_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_MHD                                        &
     &         (i_step, MHD_files, iflag_finish, MHD_step)
!
      use m_work_time
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_sph_trans_arrays_MHD
      use m_rms_4_sph_spectr
      use m_boundary_data_sph_MHD
!
      use cal_momentum_eq_explicit
      use cal_sol_sph_MHD_crank
      use cal_nonlinear
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
      use output_viz_file_control
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      integer(kind = kint), intent(inout) :: iflag_finish
      type(MHD_step_param), intent(inout) :: MHD_step
!
      integer(kind = kint) :: iflag
      real(kind = kreal) :: total_max
!
!*  ----------  add time evolution -----------------
!*
!
      call start_elapsed_time(5)
      call start_elapsed_time(6)
      if(iflag_debug.gt.0) write(*,*) 'sel_explicit_sph'
      call sel_explicit_sph(i_step, MHD_step%time_d%dt,                 &
     &    MHD_prop1, sph_MHD_bc1, sph1%sph_rj, ipol, itor, rj_fld1)
!*
!*  ----------  time evolution by inplicit method ----------
!*
      call start_elapsed_time(7)
      call s_cal_sol_sph_MHD_crank(MHD_step%time_d%dt, sph1%sph_rj,     &
     &    r_2nd, MHD_prop1, sph_MHD_bc1, trans_p1%leg,                  &
     &    ipol, idpdr, itor, sph_MHD_mat1, rj_fld1)
      call end_elapsed_time(7)
      call end_elapsed_time(6)
!*
!*  ----------------lead nonlinear term ... ----------
!*
      call start_elapsed_time(8)
      call nonlinear(sph1, comms_sph1, omega_sph1, r_2nd,               &
     &    MHD_prop1, sph_MHD_bc1, trans_p1, ref_temp1, ref_comp1,       &
     &    ipol, itor, trns_WK1, rj_fld1)
      call end_elapsed_time(8)
      call end_elapsed_time(5)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_elapsed_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(ref_temp1, ref_comp1, MHD_prop1,  &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!*
      iflag = lead_field_data_flag(i_step, MHD_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
        call s_lead_fields_4_sph_mhd                                    &
     &     (sph1, comms_sph1, r_2nd, MHD_prop1, sph_MHD_bc1, trans_p1,  &
     &      ipol, sph_MHD_mat1, trns_WK1, rj_fld1)
      end if
      call end_elapsed_time(9)
!
!*  -----------  output restart data --------------
!*
      call start_elapsed_time(4)
      call start_elapsed_time(10)
      iflag = set_IO_step_flag(MHD_step%time_d%i_time_step,             &
     &                         MHD_step%rst_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0) write(*,*) 'output_sph_restart_control'
        call output_sph_restart_control(MHD_files%fst_file_IO,          &
     &     MHD_step%time_d, rj_fld1, MHD_step%rst_step)
      end if
!
      total_time = MPI_WTIME() - total_start
      call MPI_allREDUCE (total_time, total_max, ione, CALYPSO_REAL,    &
     &    MPI_MAX, CALYPSO_COMM, ierr_MPI)
      if      (MHD_step%finish_d%i_end_step .eq. -1                     &
     &   .and. total_max .gt. MHD_step%finish_d%elapsed_time) then
        MHD_step%rst_step%istep_file = MHD_step%finish_d%i_end_step
        iflag_finish = 1
        call output_sph_restart_control(MHD_files%fst_file_IO,          &
     &     MHD_step%time_d, rj_fld1, MHD_step%rst_step)
      end if
      call end_elapsed_time(10)
!
!*  -----------  lead energy data --------------
!*
      call start_elapsed_time(11)
      iflag = output_IO_flag(i_step, MHD_step%rms_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
        call output_rms_sph_mhd_control                                 &
     &     (MHD_step%time_d, sph1%sph_params, sph1%sph_rj,              &
     &      sph_MHD_bc1%sph_bc_U, trans_p1%leg, ipol, rj_fld1,          &
     &      pwr1, WK_pwr)
      end if
      call end_elapsed_time(11)
!
      if(iflag_debug.gt.0) write(*,*) 'sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(ref_temp1, ref_comp1, MHD_prop1,   &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!
      if(i_step .ge. MHD_step%finish_d%i_end_step                       &
     &    .and. MHD_step%finish_d%i_end_step .gt. 0) then
        iflag_finish = 1
      end if
      call end_elapsed_time(4)
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
